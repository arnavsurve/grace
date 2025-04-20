package emitter

import (
	"fmt"
	"sort"
	"strings"

	"github.com/arnavsurve/grace/internal/compiler/ast"
	"github.com/arnavsurve/grace/internal/compiler/lib"
	"github.com/arnavsurve/grace/internal/compiler/scope"
	"github.com/arnavsurve/grace/internal/compiler/symbols"
)

// NOTES:
// - String literal continuation logic is basic.
// - Complex expressions within STRING concatenation might require more robust temp var handling.

const (
	areaAIndent     = "       "       // 7 spaces (column 8)
	areaBIndent     = "           "   // 11 spaces (column 12)
	tempIntName     = "GRACE-TMP-INT" // Temporary integer variable
	tempStringName  = "GRACE-TMP-STR" // Temporary string variable
	tempStringWidth = 256             // Max width for temp string var
	eofFlagName     = "GRACE-EOF-FLAG"

	cobolAreaACol        = 8
	cobolAreaBCol        = 12
	cobolLineEndCol      = 72        // Standard line length limit
	cobolContinuationCol = 7         // Hyphen position for continuation
	continuationPrefix   = "      -" // 6 spaces + hyphen
	returnVarPrefix      = "RET-"    // e.g. GRACE-RET-PROCNAME
)

type Emitter struct {
	builder                 strings.Builder
	errors                  []string
	needsTempInt            bool
	needsTempString         bool
	needsEOFflag            bool
	declaredVars            map[string]bool // Tracks COBOL var names declared in WORKING-STORAGE
	globalScope             *scope.Scope
	currentProcInfo         *symbols.SymbolInfo
	currentEmittingProcName string
	currentLocalScope       *scope.Scope // Local scope for lookups

	recordSymbols  map[string]*symbols.SymbolInfo // Map record name -> symbol
	fileSymbols    map[string]*symbols.SymbolInfo // Map file handle name -> symbol
	fileDeclASTs   []*ast.FileDeclarationStatement
	recordDeclASTs []*ast.RecordDeclarationStatement
}

func NewEmitter() *Emitter {
	return &Emitter{
		errors:         []string{},
		declaredVars:   make(map[string]bool),
		recordSymbols:  make(map[string]*symbols.SymbolInfo),
		fileSymbols:    make(map[string]*symbols.SymbolInfo),
		fileDeclASTs:   []*ast.FileDeclarationStatement{},
		recordDeclASTs: []*ast.RecordDeclarationStatement{},
	}
}

func (e *Emitter) addError(format string, args ...any) {
	errMsg := fmt.Sprintf(format, args...)
	e.errors = append(e.errors, errMsg)
}

func (e *Emitter) Errors() []string {
	return e.errors
}

// sanitizeIdentifier ALWAYS prepends "GRACE-" and uppercases.
func sanitizeIdentifier(name string) string {
	upperName := strings.ToUpper(name)
	safeName := strings.ReplaceAll(upperName, "_", "-") // Allow underscores in Grace, map to hyphen
	return "GRACE-" + safeName                          // Always add prefix
}

// --- Emit Helpers ---

func (e *Emitter) emitA(line string) {
	// Area A lines often don't need periods (like DIVISION headers, SECTION headers)
	e.builder.WriteString(areaAIndent + line + "\n")
}

// emitB writes a line to Area B, ensuring it ends with a period.
// Handles string literal continuation for DISPLAY "literal" and MOVE "literal" TO var.
func (e *Emitter) emitB(line string) {
	trimmedLine := strings.TrimSpace(line)
	if trimmedLine == "" {
		return
	}

	// --- Special Handling for DISPLAY String Literals ---
	if strings.HasPrefix(trimmedLine, "DISPLAY ") {
		parts := strings.SplitN(trimmedLine, " ", 2)
		if len(parts) == 2 {
			content := strings.TrimSpace(parts[1])
			originalHadPeriod := strings.HasSuffix(content, ".")
			if originalHadPeriod {
				content = content[:len(content)-1]
			} // Remove period for processing
			isQuoted := len(content) >= 2 && content[0] == '"' && content[len(content)-1] == '"'

			if isQuoted {
				literalContent := content[1 : len(content)-1]

				// Check if the *entire original* statement fits
				fullDisplayStmtLen := len(areaBIndent) + len("DISPLAY ") + len(content) + 1 // +1 for period
				if fullDisplayStmtLen <= cobolLineEndCol {
					e.builder.WriteString(fmt.Sprintf("%sDISPLAY \"%s\".\n", areaBIndent, literalContent))
					return
				}

				// --- Multi-line handling for DISPLAY (if entire stmt doesn't fit) ---
				// Calculate how much of the literal can fit on the *first* line
				maxFirstChunkLen := cobolLineEndCol - (len(areaBIndent) + len("DISPLAY ") + 2) // "literal"
				maxFirstChunkLen = max(maxFirstChunkLen, 0)

				firstChunk := literalContent
				if len(firstChunk) > maxFirstChunkLen {
					firstChunk = firstChunk[:maxFirstChunkLen]
				}
				remaining := literalContent[len(firstChunk):]

				// Emit first line
				e.builder.WriteString(fmt.Sprintf("%sDISPLAY \"%s\"\n", areaBIndent, firstChunk))

				// Max length for subsequent chunks
				maxLenContinue := cobolLineEndCol - len(continuationPrefix) - 2 // -"literal"
				maxLenContinue = max(maxLenContinue, 0)

				for len(remaining) > 0 {
					chunkLen := maxLenContinue
					if chunkLen <= 0 {
						chunkLen = 1
					} // Ensure progress

					currChunk := remaining
					if len(currChunk) > chunkLen {
						currChunk = currChunk[:chunkLen]
					}
					remaining = remaining[len(currChunk):]

					// Emit continuation line
					e.builder.WriteString(fmt.Sprintf("%s\"%s\"", continuationPrefix, currChunk))
					if len(remaining) > 0 {
						e.builder.WriteString("\n") // Newline if more chunks follow
					}
				}
				// Add final period after the last chunk
				e.builder.WriteString(".\n")
				return
			}
		}
		// If not a simple DISPLAY "literal", fall through to standard handling
	}

	// --- Special Handling for MOVE String Literals ---
	if strings.HasPrefix(trimmedLine, "MOVE \"") {
		idxLiteralStart := len("MOVE \"")
		idxLiteralEndQuote := -1
		idxToKeyword := -1

		// Find the last quote potentially before " TO "
		lastQuote := strings.LastIndex(trimmedLine, "\"")
		if lastQuote > idxLiteralStart { // Ensure it's not the opening quote
			// Look for " TO " after this potential closing quote
			tempIdx := strings.Index(trimmedLine[lastQuote:], " TO ")
			if tempIdx != -1 {
				idxLiteralEndQuote = lastQuote
				idxToKeyword = lastQuote + tempIdx
			}
		}

		if idxLiteralEndQuote != -1 && idxToKeyword != -1 {
			literalContent := trimmedLine[idxLiteralStart:idxLiteralEndQuote]
			// Extract the " TO variable" part, keep space before TO
			suffixPart := strings.TrimSpace(trimmedLine[idxToKeyword:])
			suffixPart = strings.TrimSuffix(suffixPart, ".") // Keep "TO variable"

			// --- Check if the entire original statement fits ---
			originalLineLength := len(areaBIndent) + len(trimmedLine)
			if !strings.HasSuffix(trimmedLine, ".") {
				originalLineLength++ // Account for adding the period later
			}

			if originalLineLength <= cobolLineEndCol {
				// Fits on one line, use standard handling (adds period)
				finalLine := trimmedLine
				if !strings.HasSuffix(finalLine, ".") {
					finalLine += "."
				}
				e.builder.WriteString(areaBIndent + finalLine + "\n")
				return
			}

			// --- Multi-line handling for MOVE (triggered because full line doesn't fit) ---

			// Calculate max length for the string literal chunk on the *first* MOVE line
			maxLenFirstLineLiteral := cobolLineEndCol - (len(areaBIndent) + len("MOVE ") + 2) // "literal"
			maxLenFirstLineLiteral = max(maxLenFirstLineLiteral, 0)

			firstChunk := literalContent
			if len(firstChunk) > maxLenFirstLineLiteral {
				firstChunk = firstChunk[:maxLenFirstLineLiteral]
			}
			remainingLiteral := literalContent[len(firstChunk):]

			// Emit first line: MOVE "chunk"
			e.builder.WriteString(fmt.Sprintf("%sMOVE \"%s\"\n", areaBIndent, firstChunk))

			// Max length for subsequent literal chunks on continuation lines
			maxLenContinueLiteral := cobolLineEndCol - len(continuationPrefix) - 2 // -"literal"
			maxLenContinueLiteral = max(maxLenContinueLiteral, 0)

			lastLiteralLineContent := "" // Track the content written on the last line used for literals

			// Loop through remaining literal parts (if any)
			for len(remainingLiteral) > 0 {
				chunkLen := maxLenContinueLiteral
				if chunkLen <= 0 {
					chunkLen = 1
				} // Ensure progress

				currChunk := remainingLiteral
				if len(currChunk) > chunkLen {
					currChunk = currChunk[:chunkLen]
				}
				remainingLiteral = remainingLiteral[len(currChunk):]

				// Emit continuation line: -"chunk"
				lastLiteralLineContent = fmt.Sprintf("%s\"%s\"", continuationPrefix, currChunk)
				e.builder.WriteString(lastLiteralLineContent)

				if len(remainingLiteral) > 0 {
					// More literal chunks follow, end this line
					e.builder.WriteString("\n")
					lastLiteralLineContent = "" // Reset as this line is finished
				}
				// If it was the last chunk, DO NOT add newline yet, suffix might append.
			}

			// --- Now, append the suffix ---
			suffixWithSpaceAndPeriod := fmt.Sprintf(" %s.", suffixPart) // Add leading space and period

			if lastLiteralLineContent != "" {
				// Suffix needs to be appended to the last literal line
				if len(lastLiteralLineContent)+len(suffixWithSpaceAndPeriod) <= cobolLineEndCol {
					// Append suffix to the current line
					e.builder.WriteString(suffixWithSpaceAndPeriod + "\n")
				} else {
					// Suffix doesn't fit on the last literal line, needs its own cont. line
					e.builder.WriteString("\n") // End the last literal line
					e.builder.WriteString(fmt.Sprintf("%s %s.\n", continuationPrefix, suffixPart))
				}
			} else {
				// The entire literal fit on the *first* line (e.g., MOVE "chunk"\n)
				// We already know the suffix didn't fit on that first line (from originalLineLength check)
				// So, the suffix *must* go on a new continuation line.
				e.builder.WriteString(fmt.Sprintf("%s %s.\n", continuationPrefix, suffixPart))
			}
			return // Finished handling multi-line MOVE
		}
		// If parsing MOVE "..." TO ... failed, fall through
	}

	// --- Fallback: Standard line handling (add period if needed) ---
	needsPeriod := true
	noPeriodPrefixes := []string{
		"IDENTIFICATION DIVISION", "PROGRAM-ID", "DATA DIVISION",
		"WORKING-STORAGE SECTION", "PROCEDURE DIVISION", "DECLARATIVES",
		"END DECLARATIVES",
	}
	isSectionHeader := strings.HasSuffix(trimmedLine, " SECTION.")

	for _, prefix := range noPeriodPrefixes {
		if strings.HasPrefix(trimmedLine, prefix) {
			needsPeriod = false
			break
		}
	}
	if isSectionHeader {
		needsPeriod = false
	}
	if strings.HasSuffix(trimmedLine, ".") {
		needsPeriod = false
	}

	finalLine := trimmedLine
	if needsPeriod {
		finalLine += "."
	}

	// Check length before writing (basic safety for non-handled lines)
	if !strings.Contains(finalLine, "\n") && len(areaBIndent+finalLine) > cobolLineEndCol {
		e.addError("Emitter Warning: Generated COBOL line may exceed %d columns: %s", cobolLineEndCol, areaBIndent+finalLine)
	}

	e.builder.WriteString(areaBIndent + finalLine + "\n")
}

func (e *Emitter) emitComment(comment string) {
	line := "      *" + comment
	e.builder.WriteString(line + "\n")
}

// --- Analysis Phase ---

func (e *Emitter) analyzeProgram(program *ast.Program) {
	e.needsTempInt = false
	e.needsTempString = false
	e.needsEOFflag = false // Reset flag

	// Traverse AST to check for temps and READ statements
	for _, stmt := range program.Statements {
		e.analyzeStatement(stmt)
	}
}

func (e *Emitter) analyzeStatement(stmt ast.Statement) {
	switch s := stmt.(type) {
	case *ast.ProcDeclarationStatement:
		if s.Body != nil {
			for _, bodyStmt := range s.Body.Statements {
				e.analyzeStatement(bodyStmt) // Recurse into proc body
			}
		}
	case *ast.PrintStatement:
		e.analyzeExpression(s.Value)
	case *ast.DeclarationStatement:
		if s.Value != nil { // Check assignment part
			e.analyzeExpression(s.Value)
		}
	case *ast.ReassignmentStatement:
		e.analyzeExpression(s.Value)
	case *ast.ReturnStatement:
		if s.ReturnValue != nil {
			e.analyzeExpression(s.ReturnValue)
		}
	case *ast.ExpressionStatement:
		e.analyzeExpression(s.Expression)
	case *ast.BlockStatement:
		for _, blockStmt := range s.Statements {
			e.analyzeStatement(blockStmt)
		}
	case *ast.ReadStatement: // NEW: Check for read statements
		e.needsEOFflag = true // Found a READ, need the EOF flag
	case *ast.WriteStatement, *ast.RecordDeclarationStatement, *ast.FileDeclarationStatement:
		// These don't directly require analysis for temps/EOF flag
	}
}

func (e *Emitter) analyzeExpression(expr ast.Expression) {
	if expr == nil {
		return
	}
	switch ex := expr.(type) {
	case *ast.BinaryExpression:
		resultType := ex.ResultType()
		if resultType == "int" && !isConstantFoldedInt(ex) {
			e.needsTempInt = true
		}
		if resultType == "string" && !isConstantFoldedString(ex) {
			e.needsTempString = true
		}
		e.analyzeExpression(ex.Left)
		e.analyzeExpression(ex.Right)
	case *ast.GroupedExpression:
		e.analyzeExpression(ex.Expression)
	case *ast.ProcCallExpression:
		for _, arg := range ex.Arguments {
			e.analyzeExpression(arg)
		}
	case *ast.InputExpression:
		// Could analyze record type usage here if needed later
	case *ast.OutputExpression:
		// Could analyze record type usage here if needed later
	case *ast.Identifier, *ast.IntegerLiteral, *ast.StringLiteral:
		// Base cases
	}
}

// Helper to check if a binary int expression was folded by the parser
func isConstantFoldedInt(be *ast.BinaryExpression) bool {
	_, leftIsLit := be.Left.(*ast.IntegerLiteral)
	// If Left is literal and Operator is empty, it was folded.
	return leftIsLit && be.Operator == ""
}

// Helper to check if a binary string expression was folded by the parser
func isConstantFoldedString(be *ast.BinaryExpression) bool {
	_, leftIsLit := be.Left.(*ast.StringLiteral)
	// If Left is literal and Operator is empty, it was folded.
	return leftIsLit && be.Operator == ""
}

// Helper to determine the correct COBOL variable name based on current scope context
func (e *Emitter) getScopedCobolName(identifierName string) string {
	if e.currentLocalScope != nil {
		_, existsInLocal := e.currentLocalScope.LookupCurrentScope(identifierName)
		if existsInLocal && e.currentEmittingProcName != "" {
			// Param or local var of the current proc
			prefixedName := e.currentEmittingProcName + "-" + identifierName
			return sanitizeIdentifier(prefixedName) // GRACE-PROCNAME-VARNAME
		}
	}

	// If not in local scope or not currently emitting a proc, assume global
	return sanitizeIdentifier(identifierName)
}

// --- Main Emit Function ---

func (e *Emitter) Emit(program *ast.Program, nameWithoutExt string) string {
	e.builder.Reset()
	e.errors = []string{}
	e.declaredVars = make(map[string]bool)
	e.recordSymbols = make(map[string]*symbols.SymbolInfo)
	e.fileSymbols = make(map[string]*symbols.SymbolInfo) // Clear collected symbols
	e.fileDeclASTs = []*ast.FileDeclarationStatement{}
	e.recordDeclASTs = []*ast.RecordDeclarationStatement{}

	if program == nil {
		e.addError("Internal Error: Received nil program.")
		return ""
	}
	if program.GlobalScope == nil {
		e.addError("Internal Error: Program AST has nil GlobalScope.")
		return ""
	}
	e.globalScope = program.GlobalScope

	// --- Collect necessary declarations FIRST ---
	e.collectDeclarations(program.Statements) // Populates record/fileDeclASTs

	// --- Analyze for temp vars and EOF flag ---
	e.analyzeProgram(program)

	// --- Emit COBOL Structure ---
	e.emitHeader(nameWithoutExt)
	e.emitEnvironmentDivision()            // Uses e.fileDeclASTs
	e.emitDataDivision(program.Statements) // Uses record/file decls and analysis results
	e.emitProcedureDivision(program.Statements)

	return e.builder.String()
}

func (e *Emitter) collectDeclarations(stmts []ast.Statement) {
	for _, stmt := range stmts {
		switch s := stmt.(type) {
		case *ast.RecordDeclarationStatement:
			if s.Symbol != nil {
				e.recordDeclASTs = append(e.recordDeclASTs, s)
				e.recordSymbols[s.Name.Value] = s.Symbol // Store resolved symbol by name
			} else {
				e.addError("Internal Error: Record AST node '%s' missing symbol info.", s.Name.Value)
			}
		case *ast.DeclarationStatement:
			if s.Name != nil && s.Name.Symbol != nil && s.Name.Symbol.Type == "file" {
				e.fileSymbols[s.Name.Value] = s.Name.Symbol
			}
		case *ast.FileDeclarationStatement:
			if s.Symbol != nil {
				e.fileDeclASTs = append(e.fileDeclASTs, s)
				e.fileSymbols[s.Name.Value] = s.Symbol // Store resolved symbol by name
			} else {
				e.addError("Internal Error: File AST node '%s' missing symbol info.", s.Name.Value)
			}
		case *ast.ProcDeclarationStatement: // Recurse into procs for local file/record decls? Not standard COBOL. Assume global.
			if s.Body != nil {
				// Potentially collect local declarations if needed later
			}
		}
	}
}

// --- Emit Structure Divisions ---

func (e *Emitter) emitHeader(nameWithoutExt string) {
	e.builder.WriteString(areaAIndent + "IDENTIFICATION DIVISION.\n")
	programId := fmt.Sprintf("PROGRAM-ID. %s.", strings.ToUpper(nameWithoutExt))
	e.builder.WriteString(areaAIndent + programId + "\n\n")
}

// --- Environment Division ---
func (e *Emitter) emitEnvironmentDivision() {
	if len(e.fileSymbols) == 0 {
		return
	} // Only emit if files are declared

	e.emitA("ENVIRONMENT DIVISION.")
	e.emitA("INPUT-OUTPUT SECTION.")
	e.emitA("FILE-CONTROL.")

	// Sort for deterministic output
	var fileNames []string
	for name := range e.fileSymbols {
		fileNames = append(fileNames, name)
	}
	sort.Strings(fileNames)

	for _, name := range fileNames {
		fileSym := e.fileSymbols[name]
		logicalName := sanitizeIdentifier(name)
		systemName := fileSym.SystemFileName

		// Emit: SELECT logical-name ASSIGN TO "system-name".
		e.emitA(fmt.Sprintf("    SELECT %s ASSIGN TO \"%s\".", logicalName, systemName))
	}
	e.builder.WriteString("\n")
}

// --- Data Division ---
func (e *Emitter) emitDataDivision(programStatements []ast.Statement) {
	e.declaredVars = make(map[string]bool) // Reset tracker

	hasFiles := len(e.fileSymbols) > 0
	hasWorkingStorage := e.needsWorkingStorageContent(programStatements)
	hasLocalStorage := e.needsLocalStorageContent(programStatements)

	if !hasFiles && !hasWorkingStorage && !hasLocalStorage {
		return
	} // Nothing to emit

	e.emitA("DATA DIVISION.")

	// --- FILE SECTION ---
	if hasFiles {
		e.emitFileSection()
	}

	// --- WORKING-STORAGE SECTION ---
	if hasWorkingStorage {
		e.emitWorkingStorageSection(programStatements)
	}

	// --- LOCAL-STORAGE SECTION ---
	if hasLocalStorage {
		e.emitLocalStorageSection(programStatements)
	}
}

// --- File Section ---
func (e *Emitter) emitFileSection() {
	if len(e.fileSymbols) == 0 {
		return
	}

	e.emitA("FILE SECTION.")

	// Sort keys
	var fileNames []string
	for name := range e.fileSymbols {
		fileNames = append(fileNames, name)
	}
	sort.Strings(fileNames)

	for _, name := range fileNames {
		fileSym := e.fileSymbols[name]
		if fileSym.FileRecordTypeSymbol == nil {
			continue
		} // Need resolved record info

		logicalName := sanitizeIdentifier(name)
		recordSym := fileSym.FileRecordTypeSymbol
		// Generate a unique FD record name (e.g., GRACE-SIMPLEREC-FD-INFILE)
		// This is safer than just GRACE-SIMPLEREC-FD if multiple files use SimpleRec.
		recordTypeNameForFD := sanitizeIdentifier(recordSym.Name) + "-FD-" + sanitizeIdentifier(name)

		e.emitA(fmt.Sprintf("FD  %s.", logicalName))
		e.emitA(fmt.Sprintf("01  %s.", recordTypeNameForFD)) // Use unique FD record name
		for _, field := range recordSym.Fields {
			picClause := e.getCobolPicClause(field.Type, field.Width)
			fieldName := sanitizeIdentifier(field.Name)
			e.emitA(fmt.Sprintf("    05 %s %s.", fieldName, picClause))
		}
	}
	e.builder.WriteString("\n")
}

// --- Helper to check if WS is needed ---
func (e *Emitter) needsWorkingStorageContent(programStatements []ast.Statement) bool {
	if e.needsTempInt || e.needsTempString || e.needsEOFflag {
		return true
	}
	// Check for global variables (including records) or procedure return vars
	for _, info := range e.globalScope.Symbols {
		if info.Type != "proc" && info.Type != "record" && info.Type != "file" { // Global simple vars or record vars
			return true
		}
		if info.Type == "proc" && info.ReturnType != "void" { // Proc return vars
			return true
		}
	}
	return false
}

// --- Working Storage Section ---
func (e *Emitter) emitWorkingStorageSection(programStatements []ast.Statement) {
	e.emitA("WORKING-STORAGE SECTION.")
	e.declaredVars = make(map[string]bool) // Ensure clean slate for WS

	var globalVarNames []string
	var procASTNodes []*ast.ProcDeclarationStatement // For return vars

	// Collect global vars and procs needing return vars
	for name, info := range e.globalScope.Symbols {
		if info.Type != "proc" && info.Type != "record" && info.Type != "file" { // Actual global variable
			globalVarNames = append(globalVarNames, name)
		}
	}
	for _, stmt := range programStatements { // Find proc AST nodes
		if procDecl, ok := stmt.(*ast.ProcDeclarationStatement); ok {
			if procDecl.Name != nil && procDecl.Name.Symbol != nil && procDecl.Name.Symbol.ReturnType != "void" {
				procASTNodes = append(procASTNodes, procDecl)
			}
		}
	}
	sort.Strings(globalVarNames) // Sort global vars

	// --- Declare Global Variables (Simple and Record) ---
	for _, varName := range globalVarNames {
		info, _ := e.globalScope.Lookup(varName) // Should exist
		cobolName := sanitizeIdentifier(varName)
		if _, declared := e.declaredVars[cobolName]; declared {
			continue
		}

		if info.RecordTypeSymbol != nil { // It's a global record variable
			e.emitRecordVariableStructure(cobolName, info.RecordTypeSymbol)
			e.declaredVars[cobolName] = true // Mark group item declared
		} else { // It's a simple global variable (int/string)
			width := info.Width
			if width <= 0 {
				width = lib.GetDefaultWidth(info.Type)
			}
			if width <= 0 {
				width = 1
			} // Failsafe
			picClause := e.getCobolPicClause(info.Type, width)
			e.emitA(fmt.Sprintf("01 %s %s.", cobolName, picClause))
			e.declaredVars[cobolName] = true
		}
	}

	// --- Declare Procedure Return Variables ---
	if len(procASTNodes) > 0 {
		e.builder.WriteString("\n")
		e.emitComment("GRACE Procedure Return Variables")
		sort.Slice(procASTNodes, func(i, j int) bool { return procASTNodes[i].Name.Value < procASTNodes[j].Name.Value })

		for _, procNode := range procASTNodes {
			procInfo := procNode.Name.Symbol
			retVarNameRaw := returnVarPrefix + procNode.Name.Value
			cobolRetVarName := sanitizeIdentifier(retVarNameRaw)
			if _, declared := e.declaredVars[cobolRetVarName]; declared {
				continue
			}

			if procInfo.ReturnRecordTypeSymbol != nil { // Proc returns a record
				e.emitRecordVariableStructure(cobolRetVarName, procInfo.ReturnRecordTypeSymbol)
				e.declaredVars[cobolRetVarName] = true
			} else { // Proc returns int/string
				width := procInfo.ReturnWidth
				if width <= 0 {
					width = lib.GetDefaultWidth(procInfo.ReturnType)
				}
				if width <= 0 {
					width = 1
				}
				picClause := e.getCobolPicClause(procInfo.ReturnType, width)
				e.emitA(fmt.Sprintf("01 %s %s.", cobolRetVarName, picClause))
				e.declaredVars[cobolRetVarName] = true
			}
		}
	}

	// --- Declare Helper Variables ---
	if e.needsTempInt || e.needsTempString || e.needsEOFflag {
		e.builder.WriteString("\n")
		e.emitComment("GRACE Compiler Helper Variables")
		if e.needsTempInt {
			cobolName := tempIntName
			if _, declared := e.declaredVars[cobolName]; !declared {
				e.emitA(fmt.Sprintf("01 %s PIC 9(%d).", cobolName, lib.DefaultIntWidth))
				e.declaredVars[cobolName] = true
			}
		}
		if e.needsTempString {
			cobolName := tempStringName
			if _, declared := e.declaredVars[cobolName]; !declared {
				e.emitA(fmt.Sprintf("01 %s PIC X(%d).", cobolName, tempStringWidth))
				e.declaredVars[cobolName] = true
			}
		}
		if e.needsEOFflag {
			cobolName := eofFlagName
			if _, declared := e.declaredVars[cobolName]; !declared {
				e.emitA(fmt.Sprintf("01 %s PIC X VALUE 'N'.", cobolName))
				e.emitA(fmt.Sprintf("   88 %s-REACHED VALUE 'Y'.", cobolName)) // 88 level for condition name
				e.declaredVars[cobolName] = true
			}
		}
	}
	e.builder.WriteString("\n") // Newline after WORKING-STORAGE
}

// --- Helper to check if LS is needed ---
func (e *Emitter) needsLocalStorageContent(programStatements []ast.Statement) bool {
	for _, stmt := range programStatements {
		if procDecl, ok := stmt.(*ast.ProcDeclarationStatement); ok {
			if procDecl.LocalScope != nil && len(procDecl.LocalScope.Symbols) > 0 {
				return true // Found a proc with params or locals
			}
		}
	}
	return false
}

// --- Modified Local Storage Section ---
func (e *Emitter) emitLocalStorageSection(programStatements []ast.Statement) {
	e.emitA("LOCAL-STORAGE SECTION.")
	procASTNodes := []*ast.ProcDeclarationStatement{}
	for _, stmt := range programStatements {
		if procDecl, ok := stmt.(*ast.ProcDeclarationStatement); ok {
			if procDecl.LocalScope != nil && len(procDecl.LocalScope.Symbols) > 0 {
				procASTNodes = append(procASTNodes, procDecl)
			}
		}
	}
	sort.Slice(procASTNodes, func(i, j int) bool { return procASTNodes[i].Name.Value < procASTNodes[j].Name.Value })

	for _, procNode := range procASTNodes {
		procName := procNode.Name.Value
		e.emitComment(fmt.Sprintf("Locals & Params for procedure '%s'", procName))
		localVarsToDeclare := map[string]*symbols.SymbolInfo{}

		// Collect all symbols from the local scope
		for name, info := range procNode.LocalScope.Symbols {
			infoCopy := info // Operate on a copy
			localVarsToDeclare[name] = infoCopy
		}

		// Sort local/param names for deterministic output
		var sortedNames []string
		for name := range localVarsToDeclare {
			sortedNames = append(sortedNames, name)
		}
		sort.Strings(sortedNames)

		for _, varName := range sortedNames {
			info := localVarsToDeclare[varName]
			cobolName := sanitizeIdentifier(procName + "-" + varName)
			// Skip if already somehow declared (shouldn't happen with LS)
			// if _, declared := e.declaredVars[cobolName]; declared { continue } // No need to check declaredVars for LS

			if info.RecordTypeSymbol != nil { // Local record variable
				e.emitRecordVariableStructure(cobolName, info.RecordTypeSymbol)
				// No need to add to declaredVars for LS
			} else { // Local simple variable (int/string) - includes parameters
				width := info.Width
				if width <= 0 {
					width = lib.GetDefaultWidth(info.Type)
				}
				if width <= 0 {
					width = 1
				}
				picClause := e.getCobolPicClause(info.Type, width)
				e.emitA(fmt.Sprintf("01 %s %s.", cobolName, picClause))
			}
		}
		e.builder.WriteString("\n")
	}
}

// --- NEW: Helper to emit record structure for variables ---
func (e *Emitter) emitRecordVariableStructure(varCobolName string, recordSym *symbols.SymbolInfo) {
	if recordSym == nil || recordSym.Type != "record" {
		e.addError("Internal Emitter Error: Invalid record symbol for variable '%s'", varCobolName)
		return
	}
	e.emitA(fmt.Sprintf("01 %s.", varCobolName))
	for _, field := range recordSym.Fields {
		picClause := e.getCobolPicClause(field.Type, field.Width)
		fieldName := sanitizeIdentifier(field.Name) // Use sanitized field name
		e.emitA(fmt.Sprintf("    05 %s %s.", fieldName, picClause))
	}
}

// --- Helper for PIC clauses ---
func (e *Emitter) getCobolPicClause(fieldType string, width int) string {
	if width <= 0 {
		e.addError("Internal Error: Cannot generate PIC clause for non-positive width %d (type %s)", width, fieldType)
		return "PIC X" // Failsafe
	}
	pic := ""
	switch fieldType {
	case "string":
		pic = fmt.Sprintf("PIC X(%d)", width)
	case "int":
		pic = fmt.Sprintf("PIC 9(%d)", width)
	default:
		// Might be a record name used as type - should not reach here
		pic = "PIC X" // Failsafe
		e.addError("Internal Error: Unknown type '%s' for PIC clause generation.", fieldType)
	}
	return pic
}

// --- Modified Procedure Division ---
func (e *Emitter) emitProcedureDivision(programStatements []ast.Statement) {
	e.emitA("PROCEDURE DIVISION.")

	hasProcs := false
	hasFiles := len(e.fileSymbols) > 0

	// Check if any procedures exist
	for _, stmt := range programStatements {
		if _, ok := stmt.(*ast.ProcDeclarationStatement); ok {
			hasProcs = true
			break
		}
	}

	// Emit DECLARATIVES if procedures exist
	if hasProcs {
		e.emitA("DECLARATIVES.")
		for _, stmt := range programStatements {
			if procDecl, ok := stmt.(*ast.ProcDeclarationStatement); ok {
				e.emitStatement(procDecl) // Emits SECTION for the proc
			}
		}
		e.emitA("END DECLARATIVES.\n") // Add newline for spacing
	}

	// --- Emit MAIN SECTION ---
	e.emitA("MAIN SECTION.")

	// Emit OPEN statements if files exist
	if hasFiles {
		e.emitOpenStatements()
	}

	// Emit main logic (non-proc statements)
	for _, stmt := range programStatements {
		// Skip proc definitions and record/file decls in main logic emit
		_, isProc := stmt.(*ast.ProcDeclarationStatement)
		_, isRecord := stmt.(*ast.RecordDeclarationStatement)
		_, isFile := stmt.(*ast.FileDeclarationStatement)
		if !isProc && !isRecord && !isFile {
			e.emitStatement(stmt)
		}
	}

	// Emit CLOSE statements if files exist
	if hasFiles {
		e.emitCloseStatements()
	}

	e.emitB("GOBACK.")
}

// --- OPEN/CLOSE Helpers ---
func (e *Emitter) emitOpenStatements() {
	inputs := []string{}
	outputs := []string{}
	for name, fileSym := range e.fileSymbols {
		cobolName := sanitizeIdentifier(name)
		if fileSym.Mode == "input" {
			inputs = append(inputs, cobolName)
		} else if fileSym.Mode == "output" {
			outputs = append(outputs, cobolName)
		}
	}
	sort.Strings(inputs)
	sort.Strings(outputs)

	openStmt := "OPEN"
	if len(inputs) > 0 {
		openStmt += " INPUT " + strings.Join(inputs, " ")
	}
	if len(outputs) > 0 {
		openStmt += " OUTPUT " + strings.Join(outputs, " ")
	}
	if len(inputs) > 0 || len(outputs) > 0 {
		e.emitB(openStmt)
	}
}

func (e *Emitter) emitCloseStatements() {
	closeFiles := []string{}
	for name := range e.fileSymbols {
		closeFiles = append(closeFiles, sanitizeIdentifier(name))
	}
	sort.Strings(closeFiles)
	if len(closeFiles) > 0 {
		e.emitB("CLOSE " + strings.Join(closeFiles, " "))
	}
}

// --- Emit Statements ---
func (e *Emitter) emitStatement(stmt ast.Statement) {
	switch s := stmt.(type) {
	case *ast.PrintStatement:
		e.emitPrint(s)
	case *ast.DeclarationStatement:
		e.emitDeclaration(s)
	case *ast.ReassignmentStatement:
		e.emitReassignment(s)
	case *ast.BlockStatement:
		e.emitBlockStatement(s)
	case *ast.ReturnStatement:
		e.emitReturnStatement(s)
	case *ast.ProcDeclarationStatement:
		e.emitProcDeclaration(s)
	case *ast.ExpressionStatement:
		e.emitExpressionStatement(s)
	case *ast.ReadStatement:
		e.emitReadStatement(s)
	case *ast.WriteStatement:
		e.emitWriteStatement(s)
	// Record/File declarations are handled in Data Division, skip here
	case *ast.RecordDeclarationStatement, *ast.FileDeclarationStatement:
		// Do nothing in procedure division for these
	default:
		e.addError("Emitter encountered unknown statement type: %T", stmt)
	}
}

func (e *Emitter) emitPrint(stmt *ast.PrintStatement) {
	if stmt == nil || stmt.Value == nil {
		return
	}

	// Check if printing a record variable directly
	if ident, ok := stmt.Value.(*ast.Identifier); ok && ident.Symbol != nil && ident.Symbol.RecordTypeSymbol != nil {
		// Printing a record variable - COBOL usually displays group items
		cobolName := e.getScopedCobolName(ident.Value)
		e.emitB(fmt.Sprintf("DISPLAY %s", cobolName))
		return
	}
	// Check if printing result of function returning record
	if call, ok := stmt.Value.(*ast.ProcCallExpression); ok && call.ResolvedRecordType != nil {
		retVarNameRaw := returnVarPrefix + call.Function.Value
		cobolRetVarName := sanitizeIdentifier(retVarNameRaw)
		// TODO: Need to PERFORM the call first if not already done?
		// Assume call happens elsewhere if used in expression. If direct print(call()), need perform.
		// For now, assume result is in return var
		e.emitB(fmt.Sprintf("DISPLAY %s", cobolRetVarName))
		return
	}

	// --- Existing logic for simple types/expressions ---
	valueStr, isLiteral, err := e.emitExpressionForResult(stmt.Value)
	if err != nil {
		return
	} // Error handled
	if valueStr == "" {
		return
	}

	if isLiteral {
		if strLit, ok := stmt.Value.(*ast.StringLiteral); ok && strLit.Value == "" {
			e.emitB("DISPLAY SPACE")
		} else {
			e.emitB(fmt.Sprintf(`DISPLAY %s`, valueStr)) // Handles quoted strings
		}
	} else {
		e.emitB(fmt.Sprintf(`DISPLAY %s`, valueStr)) // Handles vars/temps
	}
}

func (e *Emitter) emitDeclaration(stmt *ast.DeclarationStatement) {
	if stmt == nil || stmt.Name == nil {
		return
	}

	// If it's just a record variable declaration without assignment (e.g., `myRec: CustomerRecord`),
	// there's no COBOL Procedure Division statement needed. Memory is allocated in Data Div.
	if stmt.HasExplicitType && stmt.ExplicitType != nil && stmt.ExplicitType.IsRecord && stmt.Value == nil {
		return
	}

	// Handle assignments (:= value or : type = value)
	if stmt.Value == nil {
		// Should only happen for record decls now, handled above. Error otherwise?
		// p.addError("Internal Error: Declaration without value for non-record type?")
		return
	}

	targetVarCobolName := e.getScopedCobolName(stmt.Name.Value)
	// Note: We don't check declaredVars here because this might be the first use in PDIV,
	// even though it was declared in DATA DIV. Emitter needs the symbol info from the parser.
	if stmt.Name.Symbol == nil {
		e.addError("Internal Error: Declaration AST node missing symbol for '%s'", stmt.Name.Value)
		return
	}

	err := e.emitAssignment(targetVarCobolName, stmt.Value, stmt.Name.Symbol) // Pass target symbol info
	_ = err                                                                   // Error handled internally
}

func (e *Emitter) emitReassignment(stmt *ast.ReassignmentStatement) {
	if stmt == nil || stmt.Name == nil || stmt.Value == nil {
		return
	}
	targetVarCobolName := e.getScopedCobolName(stmt.Name.Value)
	if stmt.Name.Symbol == nil {
		e.addError("Internal Error: Reassignment AST node missing symbol for '%s'", stmt.Name.Value)
		return
	}

	err := e.emitAssignment(targetVarCobolName, stmt.Value, stmt.Name.Symbol) // Pass target symbol info
	_ = err                                                                   // Error handled internally
}

// --- Core Expression/Assignment Helpers ---

// emitExpressionForResult
func (e *Emitter) emitExpressionForResult(expr ast.Expression) (string, bool, error) {
	if expr == nil {
		return "", false, fmt.Errorf("cannot emit nil expression")
	}

	switch node := expr.(type) {
	case *ast.Identifier:
		if node.Symbol == nil {
			err := fmt.Errorf("Internal Error: Identifier '%s' has no symbol", node.Value)
			e.addError(err.Error())
			return "", false, err
		}
		// Handle different symbol types
		switch node.Symbol.Type {
		case "proc":
			err := fmt.Errorf("procedure '%s' used as value", node.Value)
			e.addError("Emitter Error: %v", err)
			return "", false, err
		case "record":
			err := fmt.Errorf("record type '%s' used as value", node.Value)
			e.addError("Emitter Error: %v", err)
			return "", false, err
		case "file":
			err := fmt.Errorf("file handle '%s' used directly as value", node.Value)
			e.addError("Emitter Error: %v", err)
			return "", false, err
		case "int", "string":
			cobolName := e.getScopedCobolName(node.Value)
			return cobolName, false, nil
		default: // Includes variable holding a record type (e.g., "CustomerRecord")
			if node.Symbol.RecordTypeSymbol != nil {
				// Referencing a record variable
				cobolName := e.getScopedCobolName(node.Value)
				return cobolName, false, nil
			} else {
				err := fmt.Errorf("unknown symbol type '%s' for identifier '%s'", node.Symbol.Type, node.Value)
				e.addError(err.Error())
				return "", false, err
			}
		}

	case *ast.IntegerLiteral:
		return fmt.Sprintf("%d", node.Value), true, nil
	case *ast.StringLiteral:
		escapedValue := strings.ReplaceAll(node.Value, `"`, `""`)
		return fmt.Sprintf(`"%s"`, escapedValue), true, nil

	case *ast.BinaryExpression:
		// ... (logic for int/string binary ops using temps - mostly unchanged) ...
		// Ensure it doesn't allow operations on record/file types (parser should prevent)
		if isConstantFoldedInt(node) { /* ... */
		}
		if isConstantFoldedString(node) { /* ... */
		}
		resultType := node.ResultType()
		// ... handle int/string using temps ...
		if resultType == "int" {
			// ... COMPUTE into tempIntName ...
			return tempIntName, false, nil
		} else if resultType == "string" {
			// ... STRING into tempStringName ...
			return tempStringName, false, nil
		} else {
			err := fmt.Errorf("cannot evaluate binary expression yielding type '%s'", resultType)
			e.addError("Emitter Error: %v", err)
			return "", false, err
		}

	case *ast.GroupedExpression:
		return e.emitExpressionForResult(node.Expression)

	case *ast.ProcCallExpression:
		// Handle call returning basic type or record type
		resultVar, err := e.emitProcCallAndGetResultVar(node) // Emits PERFORM
		if err != nil {
			return "", false, err
		}
		if resultVar == "" { // void call used as value
			err = fmt.Errorf("cannot use result of void procedure '%s'", node.Function.Value)
			e.addError("Emitter Error: %v", err)
			return "", false, err
		}
		return resultVar, false, nil // Return name of WS return variable

	case *ast.InputExpression, *ast.OutputExpression:
		// These expressions create file handles but are typically used with `:=`
		// Using them directly as a value isn't meaningful in COBOL procedure division.
		err := fmt.Errorf("%s() used directly as value", node.TokenLiteral())
		e.addError("Emitter Error: %v", err)
		return "", false, err

	default:
		err := fmt.Errorf("cannot get result for expression type %T", expr)
		e.addError("Emitter Error: %v", err)
		return "", false, err
	}
}

func (e *Emitter) emitAssignment(targetVarCobolName string, rhsExpr ast.Expression, targetSymbol *symbols.SymbolInfo) error {
	if targetSymbol == nil {
		e.addError("Internal Error: Target symbol missing for assignment to %s", targetVarCobolName)
		return fmt.Errorf("missing target symbol")
	}

	expectedTargetType := targetSymbol.Type
	isProcReturnVar := false

	if strings.HasPrefix(targetVarCobolName, sanitizeIdentifier(returnVarPrefix)) && targetSymbol.Type == "proc" {
		isProcReturnVar = true
		expectedTargetType = targetSymbol.ReturnType
		if expectedTargetType == "" || expectedTargetType == "unknown" {
			e.addError("Internal Error: Procedure symbol '%s' has missing or unknown return type info during assignment", targetSymbol.Name)
			return fmt.Errorf("internal error: proc return type missing")
		}
	}

	// --- Handle Record Assignment ---
	// Needs to check against targetSymbol.RecordTypeSymbol (if assigning TO a record variable)
	// OR targetSymbol.ReturnRecordTypeSymbol (if assigning TO a proc return var that is a record)
	if (targetSymbol.RecordTypeSymbol != nil && !isProcReturnVar) || (targetSymbol.ReturnRecordTypeSymbol != nil && isProcReturnVar) {
		targetRecordSymbol := targetSymbol.RecordTypeSymbol
		if isProcReturnVar {
			targetRecordSymbol = targetSymbol.ReturnRecordTypeSymbol
		}

		if targetRecordSymbol == nil {
			// Should not happen if the outer condition was met, but safeguard
			e.addError("Internal Error: Target record symbol link is nil for '%s'", targetVarCobolName)
			return fmt.Errorf("internal error: nil target record symbol")
		}

		rhsType := rhsExpr.ResultType() // This should be the Record Name string

		// Compare record type names for compatibility
		if rhsType != targetRecordSymbol.Name {
			e.addError("Internal Error: Record type mismatch in emitter (%s vs %s)", rhsType, targetRecordSymbol.Name)
			return fmt.Errorf("record type mismatch")
		}

		// Get the COBOL name of the source (might be another var or a proc call return var)
		sourceEntity, isLiteral, err := e.emitExpressionForResult(rhsExpr)
		if err != nil {
			return err
		}
		if isLiteral {
			e.addError("Cannot assign literal value to a record variable '%s'", targetVarCobolName)
			return fmt.Errorf("literal to record assignment")
		}
		if sourceEntity == "" {
			return fmt.Errorf("invalid RHS for record assignment")
		}

		// Emit MOVE CORRESPONDING
		if sourceEntity == targetVarCobolName {
			e.emitComment(fmt.Sprintf("Self-assignment of record %s - skipping MOVE CORR", targetVarCobolName))
			return nil
		}

		if sourceEntity != targetVarCobolName {
			e.emitB(fmt.Sprintf("MOVE CORRESPONDING %s TO %s", sourceEntity, targetVarCobolName))
		}
		return nil
	}

	// --- Handle Basic Type Assignment (int/string/file handles) ---
	if targetSymbol.Type == "file" {
		_, isInputExpr := rhsExpr.(*ast.InputExpression)
		_, isOutputExpr := rhsExpr.(*ast.OutputExpression)

		if isInputExpr || isOutputExpr {
			// Assignment from fh := input(...) or fh := output(...)
			// No Procedure Division statement needed.
			return nil
		} else {
			e.addError("Cannot reassign file handles using '='. Declare new handles with ':='.")
			return fmt.Errorf("file handle reassignment attempt")
		}
	}

	// --- Use COMPUTE for non-folded binary int expressions ---
	if binExpr, ok := rhsExpr.(*ast.BinaryExpression); ok && !isConstantFoldedInt(binExpr) && !isConstantFoldedString(binExpr) {
		resultType := binExpr.ResultType()
		if resultType == "int" {
			// Ensure the target *expects* an int
			if expectedTargetType != "int" {
				targetDesc := getTargetDescription(targetVarCobolName, targetSymbol, isProcReturnVar)
				e.addError("Emitter Error: Type mismatch: Cannot COMPUTE integer result into %s", targetDesc)
				return fmt.Errorf("type mismatch for compute")
			}
			computeStr, err := e.emitExpressionForCompute(binExpr)
			if err != nil {
				return err
			}
			e.emitB(fmt.Sprintf("COMPUTE %s = %s", targetVarCobolName, computeStr))
			return nil
		} else if resultType == "string" {
			// Ensure the target *expects* a string
			if expectedTargetType != "string" {
				targetDesc := getTargetDescription(targetVarCobolName, targetSymbol, isProcReturnVar)
				e.addError("Emitter Error: Type mismatch: Cannot STRING result into %s", targetDesc)
				return fmt.Errorf("type mismatch for string concat")
			}
			// Direct STRING into target var
			err := e.emitStringConcatenation(targetVarCobolName, binExpr)
			return err
		}
		// If binary expression result type is neither int nor string, fall through to MOVE (will likely fail type check)
	}

	// --- Fallback: Evaluate RHS and MOVE ---
	sourceEntity, isLiteral, err := e.emitExpressionForResult(rhsExpr)
	if err != nil {
		return err
	}
	if sourceEntity == "" {
		return fmt.Errorf("invalid RHS expression for assignment")
	}

	// Self-assignment check (basic types)
	if !isLiteral && sourceEntity == targetVarCobolName {
		e.emitComment(fmt.Sprintf("Self-assignment of %s - skipping MOVE", targetVarCobolName))
		return nil
	}

	// Type check (final sanity check using the determined expectedTargetType)
	rhsType := rhsExpr.ResultType()
	if rhsType == "void" {
		err = fmt.Errorf("cannot assign void result to %s", targetVarCobolName)
		e.addError("Emitter Error: %v", err)
		return err
	}
	// Compare RHS type against the *expected* type of the target (could be variable type or proc return type)
	if expectedTargetType != "unknown" && rhsType != "unknown" && expectedTargetType != rhsType {
		targetDesc := getTargetDescription(targetVarCobolName, targetSymbol, isProcReturnVar)
		err = fmt.Errorf("type mismatch assigning %s to %s", rhsType, targetDesc)
		e.addError("Emitter Error: %v", err)
		return err
	}

	// Default: MOVE the result (literal, var, temp-var, return-var)
	e.emitB(fmt.Sprintf(`MOVE %s TO %s`, sourceEntity, targetVarCobolName))
	return nil
}

// Helper function to get a descriptive string for the assignment target
func getTargetDescription(cobolName string, symbol *symbols.SymbolInfo, isReturnVar bool) string {
	if isReturnVar && symbol.Type == "proc" {
		// Use the procedure's return type for the description
		return fmt.Sprintf("return value of procedure '%s' (expecting %s)", symbol.Name, symbol.ReturnType)
	}
	// Otherwise, describe the variable using its own type info
	return fmt.Sprintf("variable %s (type %s)", cobolName, symbol.Type)
}

func (e *Emitter) emitExpressionForCompute(expr ast.Expression) (string, error) {
	if expr == nil {
		err := fmt.Errorf("nil expression")
		e.addError("Internal Emitter Error: %v", err)
		return "", err
	}
	switch node := expr.(type) {
	case *ast.Identifier:
		if node.Symbol == nil {
			// Parser should have caught this, defensive
			err := fmt.Errorf("internal Emitter Error: Undeclared id '%s' in arithmetic expression", node.Value)
			e.addError(err.Error())
			return "", err
		}

		if node.Symbol.Type != "int" {
			err := fmt.Errorf("id '%s' (type %s) used in arithmetic, expected 'int'", node.Value, node.Symbol.Type)
			e.addError("Emitter Error: %v", err)
			return "", err
		}

		cobolName := e.getScopedCobolName(node.Value)
		return cobolName, nil

	case *ast.IntegerLiteral:
		return fmt.Sprintf("%d", node.Value), nil

	case *ast.InputExpression, *ast.OutputExpression:
		err := fmt.Errorf("type %T used in arithmetic expression", node)
		e.addError("Emitter Error: %v", err)
		return "", err

	case *ast.BinaryExpression:
		if node.ResultType() != "int" {
			err := fmt.Errorf("non-int binary expr in arithmetic (%s)", node.ResultType())
			e.addError("Internal Error: %v", err)
			return "", err
		}
		leftStr, errL := e.emitExpressionForCompute(node.Left)
		if errL != nil {
			return "", errL
		}
		rightStr, errR := e.emitExpressionForCompute(node.Right)
		if errR != nil {
			return "", errR
		}
		// Add parens defensively
		if _, ok := node.Left.(*ast.BinaryExpression); ok {
			leftStr = fmt.Sprintf("( %s )", leftStr)
		}
		if _, ok := node.Left.(*ast.GroupedExpression); ok {
			leftStr = fmt.Sprintf("( %s )", leftStr)
		}
		if _, ok := node.Right.(*ast.BinaryExpression); ok {
			rightStr = fmt.Sprintf("( %s )", rightStr)
		}
		if _, ok := node.Right.(*ast.GroupedExpression); ok {
			rightStr = fmt.Sprintf("( %s )", rightStr)
		}
		if node.Operator == "/" {
			if lit, ok := node.Right.(*ast.IntegerLiteral); ok && lit.Value == 0 {
				e.emitComment("WARNING: Potential division by zero")
			}
		}
		return fmt.Sprintf("%s %s %s", leftStr, node.Operator, rightStr), nil

	case *ast.GroupedExpression:
		innerExprStr, err := e.emitExpressionForCompute(node.Expression)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("( %s )", innerExprStr), nil

	case *ast.ProcCallExpression:
		if node.ResolvedReturnType != "int" {
			err := fmt.Errorf("result of proc call '%s' (type %s) used in arithmetic, expected 'int'", node.Function.Value, node.ResolvedReturnType)
			e.addError("Emitter Error: %v", err)
			return "", err
		}
		// Need to emit PERFORM first and use return var
		retVar, err := e.emitProcCallAndGetResultVar(node)
		if err != nil {
			return "", err
		}
		return retVar, nil // Return the name of the var holding the result

	default:
		err := fmt.Errorf("unknown expr type %T in arithmetic", expr)
		e.addError("Emitter Error: %v", err)
		return "", err
	}
}

// emitProcDeclaration emits a COBOL SECTION for a procedure.
func (e *Emitter) emitProcDeclaration(stmt *ast.ProcDeclarationStatement) {
	if stmt == nil || stmt.Name == nil || stmt.Body == nil || stmt.ReturnType == nil || stmt.Name.Symbol == nil {
		e.addError("Internal Error: Invalid proc decl node or missing symbol")
		return
	}
	procName := stmt.Name.Value
	procInfo := stmt.Name.Symbol

	// Set context
	previousProcInfo := e.currentProcInfo
	previousProcName := e.currentEmittingProcName
	previousLocalScope := e.currentLocalScope
	e.currentProcInfo = procInfo
	e.currentEmittingProcName = procName
	e.currentLocalScope = stmt.LocalScope // Use the scope attached to the AST node
	defer func() {                        // Restore context
		e.currentProcInfo = previousProcInfo
		e.currentEmittingProcName = previousProcName
		e.currentLocalScope = previousLocalScope
	}()

	procCobolName := sanitizeIdentifier(procName)
	e.emitA(procCobolName + " SECTION.") // SECTION based emit

	// Emit signature comment
	paramStrings := []string{}
	for _, p := range stmt.Parameters {
		paramStrings = append(paramStrings, p.String())
	}
	returnString := stmt.ReturnType.String()
	sigComment := fmt.Sprintf("proc %s(%s): %s", procName, strings.Join(paramStrings, ", "), returnString)
	e.emitComment(sigComment)

	// Emit Procedure Body - statements inside use local scope via getScopedCobolName
	e.emitStatement(stmt.Body)

	e.emitB("EXIT SECTION")
	e.builder.WriteString("\n")
}

func (e *Emitter) emitBlockStatement(block *ast.BlockStatement) {
	if block == nil {
		return
	}
	for _, stmt := range block.Statements {
		e.emitStatement(stmt)
	}
}

// emitReturnStatement handles return [expression] specifically
func (e *Emitter) emitReturnStatement(stmt *ast.ReturnStatement) {
	if stmt == nil {
		e.addError("Internal Emitter Error: Invalid return statement node")
		return
	}

	// --- Get Procedure Return Info from Emitter State ---
	if e.currentProcInfo == nil {
		e.addError("Internal Emitter Error: emitReturnStatement called with nil currentProcInfo.")
		return
	}

	procInfo := *e.currentProcInfo
	procName := e.currentEmittingProcName
	expectedReturnType := procInfo.ReturnType

	// --- Handle Void Return ---
	if expectedReturnType == "void" {
		if stmt.ReturnValue != nil {
			e.addError("Emitter Warning: Return in void procedure has value; ignoring.")
		}
		return // EXIT SECTION handles control flow.
	}

	// --- Handle Non-Void Return ---
	if stmt.ReturnValue == nil {
		e.addError("Emitter Error: Missing return value for non-void procedure '%s' expecting %s", procName, expectedReturnType)
		return
	}

	// Type check (sanity)
	actualReturnType := stmt.ReturnValue.ResultType()
	if actualReturnType != "unknown" && actualReturnType != "void" && actualReturnType != expectedReturnType {
		e.addError(fmt.Sprintf("Emitter Type Error: Cannot return value of type '%s' from procedure expecting '%s'", actualReturnType, expectedReturnType))
		return
	}
	if actualReturnType == "void" {
		e.addError("Emitter Error: Attempting to return result of void expression/call.")
		return
	}

	// Determine target COBOL variable based on *expected* type
	targetReturnVarNameRaw := returnVarPrefix + procName
	targetReturnVar := sanitizeIdentifier(targetReturnVarNameRaw)

	// Emit Assignment to the specific return var
	err := e.emitAssignment(targetReturnVar, stmt.ReturnValue, &procInfo)
	if err != nil { /* Error already added by emitAssignment */
	}
}

func (e *Emitter) emitExpressionStatement(stmt *ast.ExpressionStatement) {
	if stmt == nil || stmt.Expression == nil {
		e.addError("Emitter Error: Invalid expression statement node")
		return
	}
	if procCall, ok := stmt.Expression.(*ast.ProcCallExpression); ok {
		_, err := e.emitProcCallAndGetResultVar(procCall) // Call, ignore result var name
		_ = err                                           // Error handled internally
	} else {
		e.addError("Emitter Warning: Expression of type %T used as statement has no effect.", stmt.Expression)
	}
}

func (e *Emitter) emitProcCallAndGetResultVar(callExpr *ast.ProcCallExpression) (string, error) {
	if callExpr == nil || callExpr.Function == nil || callExpr.Function.Symbol == nil {
		err := fmt.Errorf("invalid proc call AST")
		e.addError("Emitter Error: %v", err)
		return "", err
	}
	procName := callExpr.Function.Value
	procCobolName := sanitizeIdentifier(procName)
	procSymInfo := *callExpr.Function.Symbol // The symbol *of the procedure itself*

	// --- Emit Argument Assignments ---
	if len(callExpr.Arguments) != len(procSymInfo.ParamNames) {
		err := fmt.Errorf("arg count mismatch for '%s'", procName)
		e.addError("Emitter Error: %v", err)
		return "", err
	}
	for i, argExpr := range callExpr.Arguments {
		if i >= len(procSymInfo.ParamNames) || i >= len(procSymInfo.ParamTypes) {
			break
		}
		paramGraceName := procSymInfo.ParamNames[i]
		paramCobolName := sanitizeIdentifier(procName + "-" + paramGraceName) // LS var name

		// Need the symbol info for the parameter *variable* to pass to emitAssignment
		// Construct temporary param symbol info based on proc signature
		paramInfo := symbols.SymbolInfo{
			Name:    paramGraceName,
			Type:    procSymInfo.ParamTypes[i],
			Width:   procSymInfo.ParamWidths[i],
			IsConst: false,
			// TODO: Need to link RecordTypeSymbol if param is a record type
			// RecordTypeSymbol: procSymInfo.ParamRecordSymbols[i], // Need this linkage
		}

		err := e.emitAssignment(paramCobolName, argExpr, &paramInfo)
		if err != nil {
			e.addError("Emitter Error: Failed assignment for arg %d of '%s': %v", i+1, procName, err)
			return "", err
		}
	}

	// --- Emit PERFORM ---
	e.emitB(fmt.Sprintf("PERFORM %s", procCobolName))

	// --- Determine Result Variable ---
	switch procSymInfo.ReturnType {
	case "void":
		return "", nil // No result var
	case "unknown":
		err := fmt.Errorf("proc '%s' has unresolved return type", procName)
		e.addError("Emitter Error: %v", err)
		return "", err
	default: // Includes "int", "string", and RecordType names
		retVarNameRaw := returnVarPrefix + procName
		cobolRetVarName := sanitizeIdentifier(retVarNameRaw)
		// Verify it was declared (already done in Data Div emit based on procInfo)
		return cobolRetVarName, nil
	}
}

func (e *Emitter) emitStringConcatenation(targetCobolVar string, expr *ast.BinaryExpression) error {
	operands := []string{}
	// tempVarsNeeded := make(map[string]string)

	err := e.collectStringOperands(expr, &operands)
	if err != nil {
		return err
	}
	if len(operands) == 0 {
		e.emitB(fmt.Sprintf(`MOVE "" TO %s`, targetCobolVar))
		return nil
	}

	// --- Emit intermediate steps (if any were collected - currently none expected) ---
	// for _, stmt := range tempVarsNeeded {
	// 	e.emitB(stmt)
	// }

	// --- Emit STRING statement ---
	e.emitB(fmt.Sprintf(`MOVE SPACES TO %s`, targetCobolVar)) // Initialize target

	// First operand line
	firstLine := fmt.Sprintf("%sSTRING %s DELIMITED BY SIZE", areaBIndent, operands[0])
	e.builder.WriteString(firstLine)

	// Subsequent operand lines
	operandIndent := areaBIndent + "       " // Indent for subsequent operands
	for i := 1; i < len(operands); i++ {
		e.builder.WriteString("\n")
		operandLine := fmt.Sprintf("%s%s DELIMITED BY SIZE", operandIndent, operands[i])
		e.builder.WriteString(operandLine)
	}

	// INTO line
	intoIndent := areaBIndent + "    " // Indent for INTO clause
	e.builder.WriteString("\n")
	intoLine := fmt.Sprintf("%sINTO %s", intoIndent, targetCobolVar)
	e.builder.WriteString(intoLine)

	// Add final period and newline
	e.builder.WriteString(".\n")

	return nil
}

// collectStringOperands recursively gathers COBOL entities (literals/vars) for STRING.
func (e *Emitter) collectStringOperands(expr ast.Expression, operands *[]string) error {
	if expr == nil {
		err := fmt.Errorf("nil expression")
		e.addError("Internal Error: %v", err)
		return err
	}
	switch node := expr.(type) {
	case *ast.StringLiteral:
		litStr, _, err := e.emitExpressionForResult(node)
		if err != nil {
			return err
		}
		*operands = append(*operands, litStr)
	case *ast.Identifier:
		if node.Symbol == nil {
			// Parser should have caught this
			err := fmt.Errorf("internal Emitter Error: Undeclared id '%s' in string concatenation", node.Value)
			e.addError(err.Error())
			return err
		}

		if node.Symbol.Type != "string" {
			err := fmt.Errorf("id '%s' (type %s) used in string concat, expected 'string'", node.Value, node.Symbol.Type)
			e.addError("Emitter Error: %v", err)
			return err
		}

		cobolName := e.getScopedCobolName(node.Value)
		*operands = append(*operands, cobolName)

	case *ast.BinaryExpression:
		if node.Operator == "+" && node.ResultType() == "string" {
			if isConstantFoldedString(node) {
				return e.collectStringOperands(node.Left, operands)
			}
			errL := e.collectStringOperands(node.Left, operands)
			if errL != nil {
				return errL
			}
			errR := e.collectStringOperands(node.Right, operands)
			if errR != nil {
				return errR
			}
		} else {
			// Reject non-string binary results
			err := fmt.Errorf("cannot use result of non-string expr (%s, type %s) directly in string concat", node.String(), node.ResultType())
			e.addError("Emitter Limitation: %v. Assign to temp string var first.", err)
			return err
		}
	case *ast.GroupedExpression:
		return e.collectStringOperands(node.Expression, operands)
	case *ast.ProcCallExpression:
		if node.ResolvedReturnType != "string" {
			err := fmt.Errorf("cannot use result of proc call '%s' (type %s) in string concat, expected string", node.Function.Value, node.ResolvedReturnType)
			e.addError("Emitter Limitation: %v", err)
			return err
		}
		// --- Handling proc call operands ---
		// Similar to non-string binary ops, needs temp var. Currently unsupported directly.
		err := fmt.Errorf("cannot use result of proc call '%s' (type %s) directly in string concat", node.Function.Value, node.ResultType())
		e.addError("Emitter Limitation: %v. Assign result to a variable first.", err)
		return err
		// --- End Handling ---
	case *ast.IntegerLiteral, *ast.InputExpression, *ast.OutputExpression:
		err := fmt.Errorf("type %T used in string concat, expected string", node)
		e.addError("Emitter Error: %v", err)
		return err
	default:
		err := fmt.Errorf("unexpected expr type %T in string concat", expr)
		e.addError("Internal Error: %v", err)
		return err
	}
	return nil
}

// --- I/O Statement Emitters ---

func (e *Emitter) emitReadStatement(stmt *ast.ReadStatement) {
	if stmt.FileHandleSymbol == nil || stmt.RecordVarSymbol == nil {
		e.addError("Internal Error: Read statement symbols not resolved.")
		return
	}
	fileCobolName := sanitizeIdentifier(stmt.FileHandle.Value)
	varCobolName := e.getScopedCobolName(stmt.RecordVar.Value) // Get correct name based on scope
	eofConditionName := fmt.Sprintf("%s-REACHED", eofFlagName) // e.g., GRACE-EOF-FLAG-REACHED

	readStmt := fmt.Sprintf("READ %s INTO %s\n%s    AT END SET %s TO TRUE\n%sEND-READ.",
		fileCobolName,
		varCobolName,
		areaAIndent, // Indent continuation lines relative to Area A start (col 8)
		eofConditionName,
		areaAIndent) // Indent END-READ

	e.builder.WriteString(areaAIndent + readStmt + "\n")
}

func (e *Emitter) emitWriteStatement(stmt *ast.WriteStatement) {
	if stmt.FileHandleSymbol == nil || stmt.RecordVarSymbol == nil || stmt.FileHandleSymbol.FileRecordTypeSymbol == nil {
		e.addError("Internal Error: Write statement symbols not resolved.")
		return
	}
	// WRITE uses the FD's record name. Reconstruct the unique FD name.
	fileHandleName := stmt.FileHandle.Value
	recordTypeNameForFD := sanitizeIdentifier(stmt.FileHandleSymbol.RecordTypeName) + "-FD-" + sanitizeIdentifier(fileHandleName)
	varCobolName := e.getScopedCobolName(stmt.RecordVar.Value)

	e.emitB(fmt.Sprintf("WRITE %s FROM %s", recordTypeNameForFD, varCobolName))
}
