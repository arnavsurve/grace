package emitter

import (
	"fmt"
	"sort"
	"strconv"
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
	maxIntTempsNeeded       int
	currentTempVarNum       int
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
		errors:            []string{},
		maxIntTempsNeeded: 0,
		currentTempVarNum: 0,
		declaredVars:      make(map[string]bool),
		recordSymbols:     make(map[string]*symbols.SymbolInfo),
		fileSymbols:       make(map[string]*symbols.SymbolInfo),
		fileDeclASTs:      []*ast.FileDeclarationStatement{},
		recordDeclASTs:    []*ast.RecordDeclarationStatement{},
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

	// --- Check length and attempt COMPUTE break ---
	fullLineWithIndentAndPeriod := areaBIndent + finalLine
	if len(fullLineWithIndentAndPeriod) > cobolLineEndCol {
		// Check if it's a COMPUTE statement we can easily break
		if strings.HasPrefix(trimmedLine, "COMPUTE ") {
			parts := strings.SplitN(finalLine, "=", 2) // Split on first '=' (including period if added)
			if len(parts) == 2 {
				part1 := strings.TrimSpace(parts[0]) + "= " // Add space after = for readability
				part2 := strings.TrimSpace(parts[1])        // This part will include the final period

				// Check if BOTH parts would now fit reasonably (a heuristic)
				// Part 1 needs indent, part 2 needs continuation prefix + space
				if len(areaBIndent+part1) <= cobolLineEndCol && len(continuationPrefix+" "+part2) <= cobolLineEndCol {
					e.builder.WriteString(areaBIndent + part1 + "\n")
					e.builder.WriteString(continuationPrefix + " " + part2 + "\n") // Write part2 with period
					return                                                         // Handled the break
				}
			}
		}
		// If not a COMPUTE or break doesn't help/fit, fall back to warning
		e.addError("Emitter Warning: Generated COBOL line exceeds %d columns (break attempted): %s", cobolLineEndCol, fullLineWithIndentAndPeriod)
		e.builder.WriteString(fullLineWithIndentAndPeriod + "\n") // Write long line anyway
	} else {
		// Line fits, write as usual
		e.builder.WriteString(fullLineWithIndentAndPeriod + "\n")
	}
}

func (e *Emitter) emitComment(comment string) {
	line := "      *" + comment
	e.builder.WriteString(line + "\n")
}

// Helper function for temp var names
func (e *Emitter) getNumberedTempName(num int) string {
	return fmt.Sprintf("%s-%d", tempIntName, num)
}

// Helper to reset counter before processing a complex expression
func (e *Emitter) resetTempVarCounter() {
	e.currentTempVarNum = 0
}

// Helper to get the next available temp var number and name
func (e *Emitter) nextTempVarNum() int {
	e.currentTempVarNum++
	if e.currentTempVarNum > e.maxIntTempsNeeded {
		// This indicates the pre-analysis count was wrong, or logic error
		e.addError("Internal Emitter Error: Exceeded pre-calculated max temporary variables (%d needed, %d calculated)", e.currentTempVarNum, e.maxIntTempsNeeded)
		// Return the potentially problematic number anyway to maybe allow completion
	}
	return e.currentTempVarNum
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

// analyzeAndSetMaxTemps calculates the maximum number of temporary variables
// needed to evaluate any single complex integer expression in the program,
// based on the total number of binary operations.

// We need a recursive function to walk the AST and find complex expressions
// For now, just set a placeholder based on detecting *any* non-trivial compute
// This is INSUFFICIENT for correctness but shows the structure.
// A real implementation needs a recursive walk.
func (e *Emitter) analyzeAndSetMaxTemps(program *ast.Program) {
	maxNeededForAnyStmt := 0 // Track the max needed for *any single* expression eval
	for _, stmt := range program.Statements {
		// Use the helper to find the max ops needed within this statement
		countForThisStmt := e.countOpsInStatement(stmt)
		if countForThisStmt > maxNeededForAnyStmt {
			maxNeededForAnyStmt = countForThisStmt
		}
	}

	// If basic analysis needed a temp but complex analysis didn't find any ops, still allocate 1
	if e.needsTempInt && maxNeededForAnyStmt == 0 {
		maxNeededForAnyStmt = 1
	}
	e.maxIntTempsNeeded = maxNeededForAnyStmt
}

// countOpsInStatement recursively finds the maximum number of binary integer
// operations within any single expression in a given statement.
func (e *Emitter) countOpsInStatement(stmt ast.Statement) int {
	maxOps := 0

	// Recursive helper function to count non-folded integer binary operations
	var countOps func(expr ast.Expression) int
	countOps = func(expr ast.Expression) int {
		if expr == nil {
			return 0
		}
		count := 0
		switch node := expr.(type) {
		case *ast.BinaryExpression:
			if node.ResultType() == "int" && !isConstantFoldedInt(node) {
				// Count this operation + operations in children
				count = 1 + countOps(node.Left) + countOps(node.Right)
			} else {
				// Not an int operation we need to break down, just count children
				count = countOps(node.Left) + countOps(node.Right)
			}
		case *ast.GroupedExpression:
			count = countOps(node.Expression) // Parens don't add operations
		case *ast.ProcCallExpression:
			// Count operations within arguments, take the max needed for any single arg
			argMax := 0
			for _, arg := range node.Arguments {
				argMax = max(argMax, countOps(arg))
			}
			count = argMax // The call itself doesn't count as an arithmetic op here
		case *ast.Identifier, *ast.IntegerLiteral, *ast.StringLiteral, *ast.InputExpression, *ast.OutputExpression:
			// These nodes don't contain arithmetic operations themselves
			count = 0
			// Add other AST expression node types if necessary
		default:
			// Unknown type, assume 0 ops
			count = 0
		}
		return count
	}

	// Check expressions within different statement types that might need computation
	switch s := stmt.(type) {
	case *ast.DeclarationStatement:
		if s.Value != nil {
			maxOps = max(maxOps, countOps(s.Value))
		}
	case *ast.ReassignmentStatement:
		maxOps = max(maxOps, countOps(s.Value))
	case *ast.PrintStatement: // If print uses complex expression, it might use temps
		maxOps = max(maxOps, countOps(s.Value))
	case *ast.ReturnStatement: // If return uses complex expression
		if s.ReturnValue != nil {
			maxOps = max(maxOps, countOps(s.ReturnValue))
		}
	case *ast.ExpressionStatement: // e.g., a proc call with complex args
		maxOps = max(maxOps, countOps(s.Expression))
	// Recurse into blocks/procs
	case *ast.ProcDeclarationStatement:
		if s.Body != nil {
			for _, bodyStmt := range s.Body.Statements {
				maxOps = max(maxOps, e.countOpsInStatement(bodyStmt))
			}
		}
	case *ast.BlockStatement:
		for _, blockStmt := range s.Statements {
			maxOps = max(maxOps, e.countOpsInStatement(blockStmt))
		}
		// Add other statement types containing expressions if necessary
	}
	return maxOps
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

	// --- Analyze for basic flags and max temps needed ---
	e.analyzeProgram(program)
	e.analyzeAndSetMaxTemps(program)

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
	hasHelpers := e.needsTempInt || e.needsTempString || e.needsEOFflag || e.maxIntTempsNeeded > 0
	if hasHelpers {
		e.builder.WriteString("\n")
		e.emitComment("GRACE Compiler Helper Variables")

		// Declare the main TMP-INT if needed by simple analysis OR if complex compute needs it
		// Declare Numbered Integer Temps (Calculation)
		if e.maxIntTempsNeeded > 0 {
			// Use S9(18) for intermediate calculation safety
			picClause := "PIC S9(18)"
			for i := 1; i <= e.maxIntTempsNeeded; i++ {
				numTempName := e.getNumberedTempName(i)
				if _, declared := e.declaredVars[numTempName]; !declared {
					e.emitA(fmt.Sprintf("01 %s %s.", numTempName, picClause))
					e.declaredVars[numTempName] = true
				}
			}
		} else if e.needsTempInt {
			// If only basic analysis needed a temp, maybe declare TMP-INT-1 still?
			// Or rely on the analysis finding at least 1 op count?
			// Let's ensure at least one is declared if needsTempInt is true
			numTempName := e.getNumberedTempName(1)
			if _, declared := e.declaredVars[numTempName]; !declared {
				e.emitA(fmt.Sprintf("01 %s PIC S9(18).", numTempName))
				e.declaredVars[numTempName] = true
			}
		}

		if e.needsTempInt {
			dispTempName := "GRACE-TMP-DISPLAY"
			picClause := "PIC Z(17)9-"
			if _, declared := e.declaredVars[dispTempName]; !declared {
				e.emitA(fmt.Sprintf("01 %s %s.", dispTempName, picClause))
				e.declaredVars[dispTempName] = true
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
				e.emitA(fmt.Sprintf("   88 %s-REACHED VALUE 'Y'.", cobolName))
				e.declaredVars[cobolName] = true
			}
		}
	}
	e.builder.WriteString("\n") // Newline after WORKING-STORAGE
}

// evaluateSubExpression recursively evaluates an integer expression,
// emits intermediate COMPUTE steps, and returns the final COBOL entity (literal, var, temp var) holding the result.
func (e *Emitter) evaluateSubExpression(expr ast.Expression) (string, error) {
	if expr == nil {
		err := fmt.Errorf("nil expression in evaluateSubExpression")
		e.addError("Internal Emitter Error: %v", err)
		return "", err
	}
	switch node := expr.(type) {
	case *ast.Identifier:
		if node.Symbol == nil || node.Symbol.Type != "int" {
			err := fmt.Errorf("invalid or non-int identifier '%s' in arithmetic", node.Value)
			e.addError("Emitter Error: %v", err)
			return "", err
		}
		return e.getScopedCobolName(node.Value), nil

	case *ast.FieldAccessExpression:
		if node.ResolvedField == nil {
			err := fmt.Errorf("unresolved field in arithmetic")
			e.addError("Internal Error: %v", err)
			return "", err
		}
		if node.ResolvedField.Type != "int" { // Check if field type is valid for arithmetic
			err := fmt.Errorf("field '%s' (type %s) used in arithmetic, expected 'int'", node.Field.Value, node.ResolvedField.Type)
			e.addError("Emitter Error: %v", err)
			return "", err
		}
		recordIdent, ok := node.Record.(*ast.Identifier)
		if !ok {
			err := fmt.Errorf("complex field access base in arithmetic")
			e.addError("Emitter Error: %v", err)
			return "", err
		}
		recordVarCobolName := e.getScopedCobolName(recordIdent.Value)
		fieldCobolName := sanitizeIdentifier(node.Field.Value)
		qualifiedName := fmt.Sprintf("%s OF %s", fieldCobolName, recordVarCobolName)
		return qualifiedName, nil

	case *ast.IntegerLiteral:
		return fmt.Sprintf("%d", node.Value), nil

	case *ast.GroupedExpression:
		// Parentheses only affect evaluation order, recurse on inner
		// The structure ensures precedence, no specific COBOL needed for the parens themselves here
		return e.evaluateSubExpression(node.Expression)

	case *ast.ProcCallExpression:
		if node.ResolvedReturnType != "int" {
			err := fmt.Errorf("non-int proc call '%s' used in arithmetic", node.Function.Value)
			e.addError("Emitter Error: %v", err)
			return "", err
		}
		// Emit PERFORM if needed and return the dedicated return variable
		// emitProcCallAndGetResultVar handles PERFORM emission
		retVar, err := e.emitProcCallAndGetResultVar(node)
		if err != nil {
			return "", err
		}
		if retVar == "" { // Should not happen if ResolvedReturnType is "int"
			err = fmt.Errorf("internal error: proc call '%s' returned no variable despite non-void type", node.Function.Value)
			e.addError("Emitter Error: %v", err)
			return "", err
		}
		return retVar, nil

	case *ast.BinaryExpression:
		if node.ResultType() != "int" {
			err := fmt.Errorf("non-int binary expr type '%s' in complex compute", node.ResultType())
			e.addError("Internal Emitter Error: %v", err)
			return "", err
		}
		// Constant folding should have happened in the parser if possible.
		// If we reach here with literals, emit directly.
		if folded := e.tryEmitFoldedBinary(node); folded != "" {
			return folded, nil
		}

		// Recursively evaluate left and right operands
		leftOperand, errL := e.evaluateSubExpression(node.Left)
		if errL != nil {
			return "", errL
		}
		rightOperand, errR := e.evaluateSubExpression(node.Right)
		if errR != nil {
			return "", errR
		}

		// Allocate a new temporary variable for this operation's result
		tempVarNum := e.nextTempVarNum()
		tempVarName := e.getNumberedTempName(tempVarNum)

		// Division by zero check (runtime might still occur if vars are zero)
		if node.Operator == "/" {
			if lit, ok := node.Right.(*ast.IntegerLiteral); ok && lit.Value == 0 {
				e.emitComment(fmt.Sprintf("WARNING: Division by literal zero in %s / %s", leftOperand, rightOperand))
				// Parser should have already added a semantic error for literal zero
			}
			// Add ON SIZE ERROR? Maybe not for initial version.
		}

		// Emit the intermediate COMPUTE statement
		computeStmt := fmt.Sprintf("COMPUTE %s = %s %s %s", tempVarName, leftOperand, node.Operator, rightOperand)
		e.emitB(computeStmt)

		// Return the name of the temp var holding the result
		return tempVarName, nil

	default:
		err := fmt.Errorf("unsupported expression type %T in complex compute", expr)
		e.addError("Emitter Error: %v", err)
		return "", err
	}
}

// Helper to handle constant folding at emission time if parser didn't
// Returns the literal string if folded, or empty string otherwise.
func (e *Emitter) tryEmitFoldedBinary(node *ast.BinaryExpression) string {
	leftLit, lok := node.Left.(*ast.IntegerLiteral)
	rightLit, rok := node.Right.(*ast.IntegerLiteral)
	if !(lok && rok) {
		return ""
	} // Not two literals

	leftVal := leftLit.Value
	rightVal := rightLit.Value
	var resultVal int

	switch node.Operator {
	case "+":
		resultVal = leftVal + rightVal
	case "-":
		resultVal = leftVal - rightVal
	case "*":
		resultVal = leftVal * rightVal
	case "/":
		if rightVal == 0 {
			return ""
		} // Avoid folding division by zero
		resultVal = leftVal / rightVal
	default:
		return "" // Unknown operator
	}
	return fmt.Sprintf("%d", resultVal)
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

// --- Helper to emit record structure for variables ---
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

// emitComplexCompute handles emitting a multi-step compute operation.
func (e *Emitter) emitComplexCompute(targetVarCobolName string, expr ast.Expression) error {
	e.resetTempVarCounter() // Reset counter for this expression
	e.emitComment(fmt.Sprintf("Begin multi-step COMPUTE for %s", targetVarCobolName))

	// Evaluate the expression, emitting intermediate steps via evaluateSubExpression
	finalResultEntity, err := e.evaluateSubExpression(expr)
	if err != nil {
		e.emitComment(fmt.Sprintf("Error during multi-step COMPUTE for %s", targetVarCobolName))
		return err // Error already added by evaluateSubExpression
	}

	// Emit the final COMPUTE statement storing the result in the target variable
	finalComputeStmt := fmt.Sprintf("COMPUTE %s = %s", targetVarCobolName, finalResultEntity)
	e.emitB(finalComputeStmt)
	e.emitComment(fmt.Sprintf("End multi-step COMPUTE for %s", targetVarCobolName))
	return nil
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

	// --- Handle Records First ---
	if ident, ok := stmt.Value.(*ast.Identifier); ok && ident.Symbol != nil && ident.Symbol.RecordTypeSymbol != nil {
		cobolName := e.getScopedCobolName(ident.Value)
		e.emitB(fmt.Sprintf("DISPLAY %s", cobolName))
		return
	}
	if call, ok := stmt.Value.(*ast.ProcCallExpression); ok && call.ResolvedRecordType != nil {
		retVarNameRaw := returnVarPrefix + call.Function.Value
		cobolRetVarName := sanitizeIdentifier(retVarNameRaw)
		e.emitB(fmt.Sprintf("DISPLAY %s", cobolRetVarName))
		return
	}

	// --- Get Value Representation ---
	valueStr, isLiteral, err := e.emitExpressionForResult(stmt.Value)
	if err != nil {
		return
	}
	if valueStr == "" {
		return
	} // Handle potential empty results?

	// --- Emit DISPLAY based on type ---
	if isLiteral {
		// Display literals directly (handle empty string)
		if strLit, ok := stmt.Value.(*ast.StringLiteral); ok && strLit.Value == "" {
			e.emitB("DISPLAY SPACE")
		} else {
			e.emitB(fmt.Sprintf(`DISPLAY %s`, valueStr))
		}
	} else {
		// It's a variable (user var, temp var, return var)
		// Check variable type for special handling
		isTempInt := isNumberedTempIntVarName(valueStr)
		isTempStr := (valueStr == tempStringName)
		dispTempName := "GRACE-TMP-DISPLAY"
		_, hasDisplayField := e.declaredVars[dispTempName]

		if isTempInt { // Format temporary integer vars using DISPLAY FUNCTION TRIM
			if hasDisplayField {
				e.emitB(fmt.Sprintf("MOVE %s TO %s", valueStr, dispTempName))

				e.emitB(fmt.Sprintf("DISPLAY FUNCTION TRIM(%s)", dispTempName))
			} else {
				e.addError("Internal Emitter Error: TMP-DISPLAY required for %s but not declared.", valueStr)

				e.emitB(fmt.Sprintf(`DISPLAY %s`, valueStr)) // Fallback to direct display
			}
		} else if isTempStr { // Format temporary string vars using FUNCTION TRIM

			e.emitB(fmt.Sprintf("DISPLAY FUNCTION TRIM(%s)", tempStringName))
		} else { // Standard display for other variables

			e.emitB(fmt.Sprintf(`DISPLAY %s`, valueStr))
		}
	}
}

// Helper function to check if a name matches the numbered temporary integer pattern
func isNumberedTempIntVarName(name string) bool {
	// Check if the name has the correct prefix
	prefix := tempIntName + "-" // e.g., "GRACE-TMP-INT-"
	if !strings.HasPrefix(name, prefix) {
		return false
	}

	// Extract and check number part
	numPart := name[len(prefix):]

	// If the remaining part can be parsed as an integer, it's a valid temp int var name
	_, err := strconv.Atoi(numPart)
	return err == nil
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
	if stmt == nil || stmt.Target == nil || stmt.Value == nil {
		return
	}

	// --- Emit LHS target to get COBOL name (simple or qualified) ---
	targetNameStr, isLit, err := e.emitExpressionForResult(stmt.Target)
	if err != nil || isLit || targetNameStr == "" {
		e.addError("Internal Error: Invalid target in reassignment emitter for %s", stmt.Target.String())
		return
	}

	// --- Get Symbol Info for the specific target (field or variable) ---
	// This is needed for emitAssignment's checks/logic
	var targetFieldOrVarSymbol *symbols.SymbolInfo
	if ident, ok := stmt.Target.(*ast.Identifier); ok {
		targetFieldOrVarSymbol = ident.Symbol // Symbol of the variable itself
	} else if fa, ok := stmt.Target.(*ast.FieldAccessExpression); ok {
		if fa.ResolvedField != nil {
			// Use field's type/width for assignment checks
			targetFieldOrVarSymbol = &symbols.SymbolInfo{
				Type:    fa.ResolvedField.Type,
				Width:   fa.ResolvedField.Width,
				IsConst: false, // Fields are assignable
			}
		}
	}

	if targetFieldOrVarSymbol == nil {
		e.addError("Internal Error: Could not determine target symbol info in reassignment emitter for %s", stmt.Target.String())
		return
	}

	// Call emitAssignment with the COBOL target name string and the symbol info
	// representing the thing being assigned TO (field or variable).
	err = e.emitAssignment(targetNameStr, stmt.Value, targetFieldOrVarSymbol)
	_ = err // error handled internally
}

// --- Core Expression/Assignment Helpers ---

// emitExpressionForResult evaluates an expression, potentially emitting intermediate
// steps (like COMPUTE or STRING into a temp var), and returns the
// final COBOL entity (literal, var, temp var) holding the result,
// plus a boolean indicating if it's a literal.
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
		resultType := node.ResultType()

		// Handle constant folding first
		if foldedIntStr := e.tryEmitFoldedBinary(node); foldedIntStr != "" && resultType == "int" {
			return foldedIntStr, true, nil // Return folded integer literal string
		}
		// TODO: Folded strings if needed

		// Handle non-folded INT expression
		if resultType == "int" {
			computeStr, err := e.emitExpressionForCompute(node)
			if err != nil {
				return "", false, err
			}

			if e.maxIntTempsNeeded < 1 {
				e.addError("Internal Emitter Error: Need temp var for '%s' but none allocated (maxIntTempsNeeded=%d)", computeStr, e.maxIntTempsNeeded)
				return "", false, fmt.Errorf("temp variable allocation mismatch")
			}
			tempVarToUse := e.getNumberedTempName(1) // Use GRACE-TMP-INT-1

			// Emit the COMPUTE statement to calculate the result into the temp var
			e.emitB(fmt.Sprintf("COMPUTE %s = %s", tempVarToUse, computeStr))

			return tempVarToUse, false, nil // Return numbered temp name, mark as non-literal
		}

		// Handle non-folded STRING expression
		if resultType == "string" {
			if !e.needsTempString {
				e.addError("Internal Emitter Error: Need string temp var for concatenation but not flagged by analysis")
				return "", false, fmt.Errorf("string temp variable mismatch")
			}

			err := e.emitStringConcatenation(tempStringName, node)
			if err != nil {
				return "", false, err
			}

			return tempStringName, false, nil
		}

		err := fmt.Errorf("cannot evaluate binary expression yielding type '%s'", resultType)
		e.addError("Emitter Error: %v", err)
		return "", false, err

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

	case *ast.FieldAccessExpression:
		if node.ResolvedField == nil {
			e.addError("Internal Error: Field access lacks resolved info: %s", node.String())
			return "", false, fmt.Errorf("unresolved field")
		}
		// Assume node.Record is *ast.Identifier for now
		recordIdent, ok := node.Record.(*ast.Identifier)
		if !ok {
			e.addError("Emitter Limitation: Field access only supported on direct variable identifiers currently.")
			return "", false, fmt.Errorf("complex field access base")
		}
		recordVarCobolName := e.getScopedCobolName(recordIdent.Value)
		fieldCobolName := sanitizeIdentifier(node.Field.Value)
		qualifiedName := fmt.Sprintf("%s OF %s", fieldCobolName, recordVarCobolName)
		return qualifiedName, false, nil // It's a variable reference

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

// emitAssignment handles generating COBOL for := and = statements.
// It now checks line length for integer COMPUTE and uses multi-step if necessary.
func (e *Emitter) emitAssignment(targetVarCobolName string, rhsExpr ast.Expression, targetSymbol *symbols.SymbolInfo) error {
	if targetSymbol == nil {
		e.addError("Internal Error: Target symbol missing for assignment to %s", targetVarCobolName)
		return fmt.Errorf("missing target symbol")
	}

	// --- Determine the actual expected type for the target ---
	expectedTargetType := targetSymbol.Type
	targetRecordSymbol := targetSymbol.RecordTypeSymbol // Default for non-return vars
	isProcReturnVar := false

	// Override expected type and record symbol if it's a procedure return variable
	if strings.HasPrefix(targetVarCobolName, sanitizeIdentifier(returnVarPrefix)) && targetSymbol.Type == "proc" {
		isProcReturnVar = true
		expectedTargetType = targetSymbol.ReturnType
		targetRecordSymbol = targetSymbol.ReturnRecordTypeSymbol // Use the specific return record symbol

		if expectedTargetType == "" || expectedTargetType == "unknown" {
			e.addError("Internal Error: Procedure symbol '%s' has missing or unknown return type info during assignment", targetSymbol.Name)
			return fmt.Errorf("internal error: proc return type missing")
		}
	}

	// --- Handle Record Assignment (target expects a record) ---
	if expectedTargetType != "int" && expectedTargetType != "string" && expectedTargetType != "file" && targetRecordSymbol != nil {
		// This should be the Record Name string
		rhsType := rhsExpr.ResultType()

		// Compare record type names for compatibility
		if rhsType != targetRecordSymbol.Name {
			targetDesc := getTargetDescription(targetVarCobolName, targetSymbol, isProcReturnVar)
			e.addError("Emitter Error: Type mismatch assigning %s to %s", rhsType, targetDesc)
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

		// Emit MOVE CORRESPONDING, skipping self-assignment
		if sourceEntity != targetVarCobolName {
			e.emitB(fmt.Sprintf("MOVE CORRESPONDING %s TO %s", sourceEntity, targetVarCobolName))
		} else {
			e.emitComment(fmt.Sprintf("Self-assignment of record %s - skipping MOVE CORR", targetVarCobolName))
		}
		return nil
	}

	// --- Handle File Handle Assignment (target is a file handle) ---
	// Note: Use targetSymbol.Type here, not expectedTargetType, as it's about the var itself
	if targetSymbol.Type == "file" {
		_, isInputExpr := rhsExpr.(*ast.InputExpression)
		_, isOutputExpr := rhsExpr.(*ast.OutputExpression)

		if isInputExpr || isOutputExpr {
			// Assignment from fh := input(...) or fh := output(...). No PDIV statement needed.
			return nil
		} else {
			e.addError("Cannot reassign file handles using '='. Declare new handles with ':='.")
			return fmt.Errorf("file handle reassignment attempt")
		}
	}

	// --- Preliminary Type Check for Basic Types ---
	// Check if RHS is compatible *before* deciding between COMPUTE/MOVE/STRING etc.
	rhsType := rhsExpr.ResultType()
	if rhsType == "void" {
		err := fmt.Errorf("cannot assign void result to %s", targetVarCobolName)
		e.addError("Emitter Error: %v", err)
		return err
	}
	// Compare RHS type against the *expected* type of the target (variable type or proc return type)
	if expectedTargetType != "unknown" && rhsType != "unknown" && expectedTargetType != rhsType {
		// This catches assigning string to int, int to string, record to basic, basic to record (if missed above) etc.
		targetDesc := getTargetDescription(targetVarCobolName, targetSymbol, isProcReturnVar)
		err := fmt.Errorf("type mismatch assigning %s to %s", rhsType, targetDesc)
		e.addError("Emitter Error: %v", err)
		return err
	}

	// --- Handle INT Assignment ---
	if expectedTargetType == "int" {
		// Generate the full expression string for a potential single-line COMPUTE
		computeExprStr, err := e.emitExpressionForCompute(rhsExpr)
		if err != nil {
			// Error likely means non-int type used, should have been caught by parser or above check
			e.addError("Internal Emitter Error: Failed to generate compute string for int target: %v", err)
			return err
		}

		// Check if the single-line COMPUTE fits
		singleLineCompute := fmt.Sprintf("COMPUTE %s = %s", targetVarCobolName, computeExprStr)
		// Check length including indent and period
		if len(areaBIndent+singleLineCompute+".") <= cobolLineEndCol {
			// It fits! Emit the single line.
			e.emitB(singleLineCompute)
		} else {
			// Too long, use multi-step compute
			e.emitComment(fmt.Sprintf("Expression too long for single COMPUTE ..."))
			err := e.emitComplexCompute(targetVarCobolName, rhsExpr) // Calls evaluateSubExpression
			if err != nil {
				// Error already added by emitComplexCompute or its callees
				return err
			}
		}
		return nil // Integer assignment handled
	}

	// --- Handle STRING Assignment ---
	if expectedTargetType == "string" {
		// Check if RHS is a non-folded binary '+' expression (requires STRING verb)
		if binExpr, ok := rhsExpr.(*ast.BinaryExpression); ok && !isConstantFoldedString(binExpr) && binExpr.Operator == "+" {
			err := e.emitStringConcatenation(targetVarCobolName, binExpr)
			if err != nil {
				return err
			}
		} else {
			// Otherwise (literal, variable, folded string, function call), use MOVE
			sourceEntity, isLiteral, err := e.emitExpressionForResult(rhsExpr)
			if err != nil {
				return err
			}
			if sourceEntity == "" {
				return fmt.Errorf("invalid RHS expression for string assignment")
			}

			// Self-assignment check
			if !isLiteral && sourceEntity == targetVarCobolName {
				e.emitComment(fmt.Sprintf("Self-assignment of %s - skipping MOVE", targetVarCobolName))
			} else {
				e.emitB(fmt.Sprintf(`MOVE %s TO %s`, sourceEntity, targetVarCobolName))
			}
		}
		return nil // String assignment handled
	}

	// --- Fallback / Error case ---
	// If we reach here, the expectedTargetType wasn't record, file, int, or string.
	e.addError("Internal Emitter Error: Unhandled assignment case for target %s (type %s) from RHS (type %s)", targetVarCobolName, expectedTargetType, rhsType)
	return fmt.Errorf("unhandled assignment type: %s", expectedTargetType)
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

	case *ast.FieldAccessExpression:
		// Similar logic to emitExpressionForResult, generate qualified name
		if node.ResolvedField == nil {
			err := fmt.Errorf("unresolved field in arithmetic")
			e.addError("Internal Error: %v", err)
			return "", err
		}
		if node.ResolvedField.Type != "int" { // Check if field type is valid for arithmetic
			err := fmt.Errorf("field '%s' (type %s) used in arithmetic, expected 'int'", node.Field.Value, node.ResolvedField.Type)
			e.addError("Emitter Error: %v", err)
			return "", err
		}
		recordIdent, ok := node.Record.(*ast.Identifier)
		if !ok {
			err := fmt.Errorf("complex field access base in arithmetic")
			e.addError("Emitter Error: %v", err)
			return "", err
		}
		recordVarCobolName := e.getScopedCobolName(recordIdent.Value)
		fieldCobolName := sanitizeIdentifier(node.Field.Value)
		qualifiedName := fmt.Sprintf("%s OF %s", fieldCobolName, recordVarCobolName)
		return qualifiedName, nil

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
		return fmt.Sprintf("(%s)", innerExprStr), nil

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
		return retVar, nil

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
