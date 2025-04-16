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

	// Define area constants for clarity and reuse
	cobolAreaACol        = 8
	cobolAreaBCol        = 12
	cobolLineEndCol      = 72        // Standard line length limit
	cobolContinuationCol = 7         // Hyphen position for continuation
	continuationPrefix   = "      -" // 6 spaces + hyphen

	// Prefix for generated return variables
	returnVarPrefix = "RET-" // e.g. GRACE-RET-PROCNAME
)

type Emitter struct {
	builder                 strings.Builder
	errors                  []string
	needsTempInt            bool
	needsTempString         bool
	declaredVars            map[string]bool // Tracks COBOL var names declared in WORKING-STORAGE
	globalScope             *scope.Scope
	currentProcInfo         *symbols.SymbolInfo
	currentEmittingProcName string
	currentLocalScope       *scope.Scope // Local scope for lookups
}

func NewEmitter() *Emitter {
	return &Emitter{
		errors:       []string{},
		declaredVars: make(map[string]bool),
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
// Also replaces internal hyphens just in case (optional).
func sanitizeIdentifier(name string) string {
	upperName := strings.ToUpper(name)
	// Optional: Replace hyphens if Grace allows them but COBOL target might not like them everywhere
	// safeName := strings.ReplaceAll(upperName, "-", "_")
	safeName := upperName      // Assume hyphens are okay for now in GnuCOBOL var names
	return "GRACE-" + safeName // Always add prefix
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

	if e.globalScope == nil {
		return
	}

	// Traverse AST to check for complex operations needing temps
	for _, stmt := range program.Statements {
		e.analyzeStatementForTemps(stmt)
	}
}

func (e *Emitter) analyzeStatementForTemps(stmt ast.Statement) {
	switch s := stmt.(type) {
	case *ast.ProcDeclarationStatement:
		if s.Body != nil {
			for _, bodyStmt := range s.Body.Statements {
				e.analyzeStatementForTemps(bodyStmt)
			}
		}
	case *ast.PrintStatement:
		e.analyzeExpression(s.Value)
	case *ast.DeclarationStatement:
		e.analyzeExpression(s.Value)
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
			e.analyzeStatementForTemps(blockStmt)
		}
	}
}

func (e *Emitter) analyzeExpression(expr ast.Expression) {
	if expr == nil {
		return
	}
	switch ex := expr.(type) {
	case *ast.BinaryExpression:
		resultType := ex.ResultType()
		// Need temp if not folded AND type is int/string
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
	// Base cases (Identifier, Literals) don't trigger needs flags
	case *ast.Identifier, *ast.IntegerLiteral, *ast.StringLiteral:
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

	if program == nil {
		e.addError("Internal Emitter Error: Received nil program from parser.")
		return ""
	}

	e.globalScope = program.GlobalScope
	if e.globalScope == nil {
		e.addError("Internal Emitter Error: Program AST has nil GlobalSymbols map.")
	}

	e.analyzeProgram(program) // Pass program for AST walk and global symbols map

	e.emitHeader(nameWithoutExt)
	e.emitDataDivision(program.Statements)

	e.builder.WriteString(areaAIndent + "PROCEDURE DIVISION.\n")

	hasProcs := false
	for _, stmt := range program.Statements {
		if _, ok := stmt.(*ast.ProcDeclarationStatement); ok {
			hasProcs = true
			break
		}
	}

	if hasProcs {
		e.builder.WriteString(areaAIndent + "DECLARATIVES.\n\n")
		for _, stmt := range program.Statements {
			if procDecl, ok := stmt.(*ast.ProcDeclarationStatement); ok {
				e.emitStatement(procDecl)
			}
		}
		e.builder.WriteString(areaAIndent + "END DECLARATIVES.\n\n")
	}

	e.builder.WriteString(areaAIndent + "MAIN SECTION.\n")
	for _, stmt := range program.Statements {
		if _, ok := stmt.(*ast.ProcDeclarationStatement); !ok {
			e.emitStatement(stmt)
		}
	}
	e.emitB("GOBACK.") // emitB adds the period

	return e.builder.String()
}

// --- Emit Structure Divisions ---

func (e *Emitter) emitHeader(nameWithoutExt string) {
	e.builder.WriteString(areaAIndent + "IDENTIFICATION DIVISION.\n")
	programId := fmt.Sprintf("PROGRAM-ID. %s.", strings.ToUpper(nameWithoutExt))
	e.builder.WriteString(areaAIndent + programId + "\n\n")
}

func (e *Emitter) emitDataDivision(programStatements []ast.Statement) {
	e.declaredVars = make(map[string]bool) // Reset global tracker for this emission

	if e.globalScope == nil {
		e.addError("Internal Emitter Error: GlobalSymbols map is nil during Data Division emission.")
		return
	}

	var globalVarNames []string
	procASTNodes := []*ast.ProcDeclarationStatement{}
	hasGlobals := false
	hasParams := false
	hasLocals := false
	hasProcReturns := false

	for _, stmt := range programStatements {
		switch node := stmt.(type) {
		case *ast.DeclarationStatement:
			if node.Name != nil {
				_, isGlobal := e.globalScope.Symbols[node.Name.Value]
				if isGlobal {
					globalVarNames = append(globalVarNames, node.Name.Value)
					hasGlobals = true
				}
			}
		case *ast.ProcDeclarationStatement:
			if node.Name == nil {
				continue
			}
			procASTNodes = append(procASTNodes, node)
			procName := node.Name.Value
			procInfo, exists := e.globalScope.Lookup(procName)
			if exists && procInfo.Type == "proc" {
				if len(procInfo.ParamNames) > 0 {
					hasParams = true
				}
				// Check AST node's scope for locals vs params
				if node.LocalScope != nil && len(node.LocalScope.Symbols) > len(procInfo.ParamNames) {
					hasLocals = true
				}
				if procInfo.ReturnType != "void" {
					hasProcReturns = true
				}
			} else {
				e.addError("Internal Emitter Error: Proc AST node '%s' found but no matching 'proc' symbol in global scope.", procName)
			}
			// default to ignoring other valid top-level statements like Print, ExpressionStatement
		}
	}

	// Check if helper variables are needed (flags should be set by analyzeProgram)
	needsHelpers := e.needsTempInt || e.needsTempString
	needsWorkingStorage := hasGlobals || needsHelpers || hasProcReturns
	needsLocalStorage := hasParams || hasLocals

	if !needsWorkingStorage && !needsLocalStorage {
		return // Nothing for DATA DIVISION
	}

	e.builder.WriteString(areaAIndent + "DATA DIVISION.\n")

	// --- WORKING-STORAGE SECTION ---
	if needsWorkingStorage {
		e.builder.WriteString(areaAIndent + "WORKING-STORAGE SECTION.\n")
		sort.Strings(globalVarNames)
		// Declare Global Variables
		for _, varName := range globalVarNames {
			info, infoExists := e.globalScope.Symbols[varName]
			if !infoExists {
				// Shouldn't happen if globalVarNames is built correctly
				continue
			}

			cobolName := sanitizeIdentifier(varName)
			if _, declared := e.declaredVars[cobolName]; declared {
				continue
			}

			width := info.Width
			if width <= 0 && info.Type != "unknown" { /* Default width logic */
				resolvedWidth := lib.GetDefaultWidth(info.Type)
				if resolvedWidth <= 0 {
					resolvedWidth = 1
				}
				// Use dummy token for location as we don't have original token here
				e.addError(fmt.Sprintf("Emitter Warning (using dummy token): Invalid width (%d) for global var '%s', using %d.", info.Width, varName, resolvedWidth))
				width = resolvedWidth
			}

			picClause := ""
			switch info.Type {
			case "string":
				if width > 0 {
					picClause = fmt.Sprintf("01 %s PIC X(%d)", cobolName, width)
				}
			case "int":
				if width > 0 {
					picClause = fmt.Sprintf("01 %s PIC 9(%d)", cobolName, width)
				}
			default:
				continue
			}

			if picClause != "" {
				e.builder.WriteString(areaAIndent + picClause + ".\n")
				e.declaredVars[cobolName] = true
			}
		}

		if hasProcReturns {
			e.builder.WriteString("\n")
			e.emitComment("GRACE Procedure Return Variables")
			// Sort procs for deterministic output
			sort.Slice(procASTNodes, func(i, j int) bool {
				if procASTNodes[i].Name == nil || procASTNodes[j].Name == nil {
					return false
				}
				return procASTNodes[i].Name.Value < procASTNodes[j].Name.Value
			})

			for _, procNode := range procASTNodes {
				if procNode.Name == nil {
					continue
				}
				procName := procNode.Name.Value

				procInfo, exists := e.globalScope.Lookup(procName)
				if !exists || procInfo.Type != "proc" || procInfo.ReturnType == "void" {
					continue // Skip void procedures or those missing symbols
				}

				retVarName := returnVarPrefix + procName // RET-PROCNAME
				cobolRetVarName := sanitizeIdentifier(retVarName)

				if _, declared := e.declaredVars[cobolRetVarName]; declared {
					continue
				}

				width := procInfo.ReturnWidth
				if width <= 0 {
					e.addError("Internal Emitter Error: Proc '%s' has invalid return width %d.", procName, width)
					continue
				}

				picClause := ""
				switch procInfo.ReturnType {
				case "string":
					picClause = fmt.Sprintf("01 %s PIC X(%d)", cobolRetVarName, width)
				case "int":
					picClause = fmt.Sprintf("01 %s PIC 9(%d)", cobolRetVarName, width)
				default:
					continue // Shouldn't happen
				}

				e.builder.WriteString(areaAIndent + picClause + ".\n")
				e.declaredVars[cobolRetVarName] = true
			}
		}

		// Declare Helper Variables (temps only)
		if needsHelpers {
			e.builder.WriteString("\n")
			e.emitComment("GRACE Compiler Helper Variables")
			if e.needsTempInt {
				cobolName := tempIntName
				if _, declared := e.declaredVars[cobolName]; !declared {
					e.builder.WriteString(fmt.Sprintf("%s01 %s PIC 9(%d).\n", areaAIndent, cobolName, lib.DefaultIntWidth))
					e.declaredVars[cobolName] = true
				}
			}
			if e.needsTempString {
				cobolName := tempStringName
				if _, declared := e.declaredVars[cobolName]; !declared {
					e.builder.WriteString(fmt.Sprintf("%s01 %s PIC X(%d).\n", areaAIndent, cobolName, tempStringWidth))
					e.declaredVars[cobolName] = true
				}
			}
		}
		e.builder.WriteString("\n") // Newline after WORKING-STORAGE
	}

	// --- LOCAL-STORAGE SECTION ---
	if needsLocalStorage {
		e.builder.WriteString(areaAIndent + "LOCAL-STORAGE SECTION." + "\n")
		sort.Slice(procASTNodes, func(i, j int) bool {
			if procASTNodes[i].Name == nil || procASTNodes[j].Name == nil {
				return false
			}
			return procASTNodes[i].Name.Value < procASTNodes[j].Name.Value
		})
		for _, procNode := range procASTNodes {
			if procNode.Name == nil {
				continue
			}
			procName := procNode.Name.Value
			procGlobalInfo, exists := e.globalScope.Lookup(procName)
			if !exists || procGlobalInfo.Type != "proc" {
				continue
			}
			paramNameSet := make(map[string]bool)
			for _, pName := range procGlobalInfo.ParamNames {
				paramNameSet[pName] = true
			}
			hasContentForProc := false
			// Declare parameters
			if len(procGlobalInfo.ParamNames) > 0 {
				if !hasContentForProc {
					e.emitComment(fmt.Sprintf("Parameters & Locals for procedure '%s'", procName))
					hasContentForProc = true
				}
				for i, paramName := range procGlobalInfo.ParamNames {
					cobolName := sanitizeIdentifier(procName + "-" + paramName)
					if _, declared := e.declaredVars[cobolName]; declared {
						continue
					}
					paramType := procGlobalInfo.ParamTypes[i]
					paramWidth := procGlobalInfo.ParamWidths[i]
					if paramWidth <= 0 {
						continue
					}
					picClause := ""
					switch paramType {
					case "string":
						picClause = fmt.Sprintf("01 %s PIC X(%d)", cobolName, paramWidth)
					case "int":
						picClause = fmt.Sprintf("01 %s PIC 9(%d)", cobolName, paramWidth)
					default:
						continue
					}
					e.builder.WriteString(areaAIndent + picClause + ".\n")
					e.declaredVars[cobolName] = true
				}
			}
			// Declare local variables
			if procNode.LocalScope != nil {
				localSymbolsToDeclare := map[string]symbols.SymbolInfo{}
				for symbolName, symbolInfo := range procNode.LocalScope.Symbols {
					if _, isParam := paramNameSet[symbolName]; !isParam {
						localSymbolsToDeclare[symbolName] = symbolInfo
					}
				}
				if len(localSymbolsToDeclare) > 0 {
					if !hasContentForProc {
						e.emitComment(fmt.Sprintf("Parameters & Locals for procedure '%s'", procName))
						hasContentForProc = true
					}
					localVarNames := make([]string, 0, len(localSymbolsToDeclare))
					for name := range localSymbolsToDeclare {
						localVarNames = append(localVarNames, name)
					}
					sort.Strings(localVarNames)
					for _, localVarName := range localVarNames {
						localInfo := localSymbolsToDeclare[localVarName]
						cobolName := sanitizeIdentifier(procName + "-" + localVarName)
						if _, declared := e.declaredVars[cobolName]; declared {
							continue
						}
						width := localInfo.Width
						if width <= 0 {
							continue
						}
						picClause := ""
						switch localInfo.Type {
						case "string":
							picClause = fmt.Sprintf("01 %s PIC X(%d)", cobolName, width)
						case "int":
							picClause = fmt.Sprintf("01 %s PIC 9(%d)", cobolName, width)
						default:
							continue
						}
						e.builder.WriteString(areaAIndent + picClause + ".\n")
						e.declaredVars[cobolName] = true
					}
				}
			}
			if hasContentForProc {
				e.builder.WriteString("\n")
			}
		}
	}
}

// --- Emit Statements ---

func (e *Emitter) emitStatement(stmt ast.Statement) {
	switch s := stmt.(type) {
	case *ast.ReturnStatement:
		e.emitReturnStatement(s)
	case *ast.ProcDeclarationStatement:
		e.emitProcDeclaration(s)
	case *ast.ExpressionStatement:
		e.emitExpressionStatement(s)
	case *ast.BlockStatement:
		e.emitBlockStatement(s)
	case *ast.PrintStatement:
		e.emitPrint(s)
	case *ast.DeclarationStatement:
		e.emitDeclaration(s)
	case *ast.ReassignmentStatement:
		e.emitReassignment(s)
	default:
		e.addError("Emitter encountered unknown statement type: %T", stmt)
	}
}

func (e *Emitter) emitPrint(stmt *ast.PrintStatement) {
	if stmt == nil || stmt.Value == nil {
		e.addError("Emitter Error: Invalid print statement (nil)")
		return
	}

	callExpr, isProcCall := stmt.Value.(*ast.ProcCallExpression)
	if isProcCall && callExpr.ResultType() != "void" {
		// Print direct proc call result
		procName := callExpr.Function.Value
		procSymInfo := callExpr.Function.Symbol
		if procSymInfo == nil || procSymInfo.Type != "proc" {
			e.addError("Internal Error: ")
		}

		retVarName := returnVarPrefix + procName
		cobolRetVarName := sanitizeIdentifier(retVarName)

		if _, declared := e.declaredVars[cobolRetVarName]; !declared {
			e.addError("Internal Emitter Error: Return variable %s for procedure '%s' not declared.", cobolRetVarName, procName)
			return
		}

		// Emit argument assignments & PERFORM
		if len(callExpr.Arguments) != len(procSymInfo.ParamNames) {
			e.addError("")
			return
		}

		for i, argExpr := range callExpr.Arguments {
			paramGraceName := procSymInfo.ParamNames[i]
			paramCobolName := sanitizeIdentifier(procName + "-" + paramGraceName)
			err := e.emitAssignment(paramCobolName, argExpr)
			if err != nil {
				e.addError("Internal Emitter Error: ")
				return
			}
		}

		e.emitB(fmt.Sprintf("PERFORM %s", sanitizeIdentifier(procName)))
		e.emitB(fmt.Sprintf(`DISPLAY %s`, cobolRetVarName))

	} else {
		valueStr, isLiteral, err := e.emitExpressionForResult(stmt.Value)
		if err != nil {
			return
		} // Error handled
		if valueStr == "" { /* Error handled */
			return
		}

		if isLiteral {
			if strLit, ok := stmt.Value.(*ast.StringLiteral); ok && strLit.Value == "" {
				e.emitB("DISPLAY SPACE")
			} else {
				e.emitB(fmt.Sprintf(`DISPLAY %s`, valueStr))
			}
		} else {
			e.emitB(fmt.Sprintf(`DISPLAY %s`, valueStr))
		}
	}
}

func (e *Emitter) emitDeclaration(stmt *ast.DeclarationStatement) {
	if stmt == nil || stmt.Name == nil || stmt.Value == nil {
		e.addError("Emitter Error: Invalid declaration statement (nil)")
		return
	}
	targetVarCobolName := e.getScopedCobolName(stmt.Name.Value)
	if _, declared := e.declaredVars[targetVarCobolName]; !declared {
		// This implies a parser error allowed use before declaration/scope issue,
		// or the declaration itself failed earlier in emitDataDivision.
		e.addError("Emitter Warning: Skipping assignment for '%s' as it was not declared.", targetVarCobolName)
		return
	}
	err := e.emitAssignment(targetVarCobolName, stmt.Value)
	_ = err // Error is added within emitAssignment or its callees
}

func (e *Emitter) emitReassignment(stmt *ast.ReassignmentStatement) {
	if stmt == nil || stmt.Name == nil || stmt.Value == nil {
		e.addError("Emitter Error: Invalid reassignment statement (nil)")
		return
	}
	targetVarCobolName := e.getScopedCobolName(stmt.Name.Value)
	if _, declared := e.declaredVars[targetVarCobolName]; !declared {
		e.addError("Emitter Warning: Skipping reassignment for '%s' as it was not declared.", targetVarCobolName)
		return
	}
	err := e.emitAssignment(targetVarCobolName, stmt.Value)
	_ = err // Error handled internally
}

// --- Core Expression/Assignment Helpers ---

func (e *Emitter) emitExpressionForResult(expr ast.Expression) (string, bool, error) {
	if expr == nil {
		return "", false, fmt.Errorf("cannot emit nil expression")
	}

	switch node := expr.(type) {
	case *ast.Identifier:
		if node.Symbol == nil {
			err := fmt.Errorf("Internal Emitter Error: Identifier '%s' used but has no symbol info", node.Value)
			e.addError(err.Error())
			return "", false, err
		}
		if node.Symbol.Type == "proc" {
			err := fmt.Errorf("procedure name '%s' used as a value", node.Value)
			e.addError("Emitter Error: %v", err)
			return "", false, err
		}
		// The COBOL compiler will resolve whether it is WORKING-STORAGE or LOCAL-STORAGE based on context
		cobolName := e.getScopedCobolName(node.Value)
		return cobolName, false, nil

	case *ast.IntegerLiteral:
		return fmt.Sprintf("%d", node.Value), true, nil

	case *ast.StringLiteral:
		escapedValue := strings.ReplaceAll(node.Value, `"`, `""`)
		return fmt.Sprintf(`"%s"`, escapedValue), true, nil

	case *ast.BinaryExpression:
		if isConstantFoldedInt(node) {
			return fmt.Sprintf("%d", node.Left.(*ast.IntegerLiteral).Value), true, nil
		}
		if isConstantFoldedString(node) {
			escapedValue := strings.ReplaceAll(node.Left.(*ast.StringLiteral).Value, `"`, `""`)
			return fmt.Sprintf(`"%s"`, escapedValue), true, nil
		}

		resultType := node.ResultType()
		if resultType == "int" {
			if !e.needsTempInt {
				err := fmt.Errorf("temp int var needed but not flagged")
				e.addError("Internal Emitter Error: %v", err)
				return "", false, err
			}
			computeExprStr, err := e.emitExpressionForCompute(node)
			if err != nil {
				return "", false, err
			}
			e.emitB(fmt.Sprintf("COMPUTE %s = %s", tempIntName, computeExprStr))
			return tempIntName, false, nil
		} else if resultType == "string" {
			if !e.needsTempString {
				err := fmt.Errorf("temp string var needed but not flagged")
				e.addError("Internal Emitter Error: %v", err)
				return "", false, err
			}
			err := e.emitStringConcatenation(tempStringName, node) // Emits STRING into temp
			if err != nil {
				return "", false, err
			}
			return tempStringName, false, nil
		} else {
			err := fmt.Errorf("cannot evaluate binary expression yielding type '%s'", resultType)
			e.addError("Emitter Error: %v", err)
			return "", false, err
		}

	case *ast.GroupedExpression:
		return e.emitExpressionForResult(node.Expression)

	case *ast.ProcCallExpression:
		if node.Function.Symbol == nil || node.Function.Symbol.Type != "proc" {
			err := fmt.Errorf("Internal Emitter Error: Invalid procedure call target: '%s'", node.Function.Value)
			e.addError(err.Error())
			return "", false, err
		}

		resultVar, err := e.emitProcCallAndGetResultVar(node)
		if err != nil {
			return "", false, err
		}

		// Should only happen for void calls used as values
		// Indicates a parser error
		if resultVar == "" {
			err = fmt.Errorf("cannot use result of void procedure '%s'", node.Function.Value)
			e.addError("Emitter Error: %v", err)
			return "", false, err
		}
		return resultVar, false, nil

	default:
		err := fmt.Errorf("cannot get result for expression of type %T", expr)
		e.addError("Emitter Error: %v", err)
		return "", false, err
	}
}

// internal/compiler/emitter/emitter.go

func (e *Emitter) emitAssignment(targetVarCobolName string, rhsExpr ast.Expression) error {
	// Check if RHS is a binary expression that wasn't folded
	actualRhsExpr := rhsExpr
	if grp, ok := rhsExpr.(*ast.GroupedExpression); ok {
		actualRhsExpr = grp.Expression
	}

	if binExpr, ok := actualRhsExpr.(*ast.BinaryExpression); ok {
		// Check if it's NOT constant folded (i.e., needs runtime compute/string)
		if !isConstantFoldedInt(binExpr) && !isConstantFoldedString(binExpr) {
			resultType := binExpr.ResultType()

			if resultType == "int" {
				computeStr, err := e.emitExpressionForCompute(binExpr)
				if err != nil {
					return err
				}
				e.emitB(fmt.Sprintf("COMPUTE %s = %s", targetVarCobolName, computeStr))
				return nil
			} else if resultType == "string" {
				err := e.emitStringConcatenation(targetVarCobolName, binExpr) // Directly into target
				return err                                                    // Return result of emitStringConcatenation (which might be nil error)
			}
			// If type is unknown, fall through (error should exist from parser/type check)
		} else {
		}
	}

	// --- Fallback: Evaluate RHS and MOVE ---
	sourceEntity, isLiteral, err := e.emitExpressionForResult(rhsExpr)
	if err != nil {
		return err
	}
	if sourceEntity == "" {
		return fmt.Errorf("internal emitter error: invalid RHS expression for assignment to %s", targetVarCobolName)
	}

	// Self-assignment check
	if !isLiteral && sourceEntity == targetVarCobolName {
		e.emitComment(fmt.Sprintf("Assignment of %s to itself - no MOVE generated", targetVarCobolName))
		return nil
	}

	// Type check (already done in parser mostly, but good safeguard)
	rhsType := rhsExpr.ResultType()
	if rhsType == "void" {
		err = fmt.Errorf("cannot assign void result to %s", targetVarCobolName)
		e.addError("Emitter Error: %v", err)
		return err
	}

	// Default: MOVE the result (literal, var, temp-var, return-var)
	e.emitB(fmt.Sprintf(`MOVE %s TO %s`, sourceEntity, targetVarCobolName))
	return nil
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

	case *ast.StringLiteral:
		err := fmt.Errorf("string literal '%s' in arithmetic", node.Value)
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
		err := fmt.Errorf("proc call '%s' in arithmetic", node.Function.Value)
		e.addError("Emitter Error: %v", err)
		return "", err

	default:
		err := fmt.Errorf("unknown expr type %T in arithmetic", expr)
		e.addError("Emitter Error: %v", err)
		return "", err
	}
}

// emitProcDeclaration emits a COBOL SECTION for a procedure.
func (e *Emitter) emitProcDeclaration(stmt *ast.ProcDeclarationStatement) {
	if stmt == nil || stmt.Name == nil || stmt.Body == nil || stmt.ReturnType == nil {
		e.addError("Emitter Error: Invalid proc decl node")
		return
	}

	if stmt.Name.Symbol == nil {
		e.addError("Internal Emitter Error: Proc declaration AST node '%s' missing symbol info.", stmt.Name.Value)
		return
	}
	if stmt.Name.Symbol.Type != "proc" {
		e.addError("Internal Emitter Error: Proc declaration AST node symbol for '%s' is not type proc.", stmt.Name.Value)
		return
	}

	procName := stmt.Name.Value

	// Set context
	previousProcInfo := e.currentProcInfo
	previousProcName := e.currentEmittingProcName
	previousLocalScope := e.currentLocalScope
	e.currentProcInfo = stmt.Name.Symbol
	e.currentEmittingProcName = procName
	e.currentLocalScope = stmt.LocalScope
	defer func() {
		e.currentProcInfo = previousProcInfo         // Restore context when function exits
		e.currentEmittingProcName = previousProcName // Restore previous name
		e.currentLocalScope = previousLocalScope
	}()

	procCobolName := sanitizeIdentifier(stmt.Name.Value)
	e.builder.WriteString(areaAIndent + procCobolName + " SECTION." + "\n")

	paramStrings := []string{}
	for _, p := range stmt.Parameters {
		paramStrings = append(paramStrings, p.String())
	}
	returnString := stmt.ReturnType.String()
	sigComment := fmt.Sprintf("proc %s(%s): %s ", stmt.Name.Value, strings.Join(paramStrings, ", "), returnString)
	e.emitComment(sigComment)

	// --- Emit Procedure Body ---
	// The statements inside the body will refer to parameters using their
	// sanitized names (e.g., GRACE-A), which COBOL resolves to LOCAL-STORAGE here.
	e.emitStatement(stmt.Body)

	e.emitB("EXIT SECTION") // emitB adds the period
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
	exprToReturn := stmt.ReturnValue

	// --- Handle Void Return ---
	if expectedReturnType == "void" {
		if exprToReturn != nil {
			e.addError("Emitter Warning: Return in void procedure has value; ignoring.")
		}
		return // EXIT SECTION handles control flow.
	}

	// --- Handle Non-Void Return ---
	if exprToReturn == nil {
		e.addError("Emitter Error: Missing return value for non-void procedure expecting %s", expectedReturnType)
		return
	}

	// Type check (sanity)
	actualReturnType := exprToReturn.ResultType()
	if actualReturnType != "unknown" && actualReturnType != "void" && actualReturnType != expectedReturnType {
		e.addError(fmt.Sprintf("Emitter Type Error: Cannot return value of type '%s' from procedure expecting '%s'", actualReturnType, expectedReturnType))
		return
	}
	if actualReturnType == "unknown" {
		e.addError("Emitter Warning: Cannot verify return type for expression yielding 'unknown'.")
	}
	if actualReturnType == "void" {
		e.addError("Emitter Error: Attempting to return result of void expression/call.")
		return
	}

	// Determine target COBOL variable based on *expected* type
	targetReturnVarNameRaw := returnVarPrefix + procName
	targetReturnVar := sanitizeIdentifier(targetReturnVarNameRaw)

	// Verify the target variable was declared (sanity check)
	if _, declared := e.declaredVars[targetReturnVar]; !declared {
		e.addError("Internal Emitter Error: Target return variable %s for procedure '%s' not found in declaredVars.", targetReturnVar, procName)
		return
	}

	// Emit Assignment to the specific return var
	err := e.emitAssignment(targetReturnVar, exprToReturn)
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
	procSymInfo := *callExpr.Function.Symbol

	// --- 1. Emit Argument Assignments ---
	if len(callExpr.Arguments) != len(procSymInfo.ParamNames) {
		err := fmt.Errorf("arg count mismatch for proc '%s'", procName)
		e.addError("Emitter Error: %v", err)
		return "", err
	}

	for i, argExpr := range callExpr.Arguments {
		if i >= len(procSymInfo.ParamNames) {
			break
		}
		paramGraceName := procSymInfo.ParamNames[i]
		paramCobolName := sanitizeIdentifier(procName + "-" + paramGraceName)

		// Assign argument value to parameter variable
		err := e.emitAssignment(paramCobolName, argExpr)
		if err != nil {
			e.addError("Emitter Error: Failed assignment for arg %d of '%s': %v", i+1, procName, err)
			return "", err
		}
	}

	// --- 2. Emit PERFORM ---
	e.emitB(fmt.Sprintf("PERFORM %s", procCobolName)) // emitB adds period

	// --- 3. Determine Result Variable ---
	switch procSymInfo.ReturnType {
	case "int", "string":
		// Construct the specific return variable name
		retVarNameRaw := returnVarPrefix + procName
		cobolRetVarName := sanitizeIdentifier(retVarNameRaw)
		// Verify it was declared (essential check)
		if _, declared := e.declaredVars[cobolRetVarName]; !declared {
			err := fmt.Errorf("internal Emitter Error: Return variable %s for procedure '%s' was not declared", cobolRetVarName, procName)
			e.addError(err.Error())
			return "", err
		}
		return cobolRetVarName, nil
	case "void":
		return "", nil // No result var
	case "unknown":
		err := fmt.Errorf("proc '%s' has unresolved return type", procName)
		e.addError("Emitter Error: %v", err)
		return "", err
	default:
		err := fmt.Errorf("unsupported return type '%s' for proc '%s'", procSymInfo.ReturnType, procName)
		e.addError("Emitter Error: %v", err)
		return "", err
	}
}

func (e *Emitter) emitStringConcatenation(targetCobolVar string, expr *ast.BinaryExpression) error {
	operands := []string{}
	tempVarsNeeded := make(map[string]string)

	err := e.collectStringOperands(expr, &operands, tempVarsNeeded)
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
func (e *Emitter) collectStringOperands(expr ast.Expression, operands *[]string, tempVarsNeeded map[string]string) error {
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
				return e.collectStringOperands(node.Left, operands, tempVarsNeeded)
			}
			errL := e.collectStringOperands(node.Left, operands, tempVarsNeeded)
			if errL != nil {
				return errL
			}
			errR := e.collectStringOperands(node.Right, operands, tempVarsNeeded)
			if errR != nil {
				return errR
			}
		} else {
			// --- Handling non-string operands ---
			// This is where temp vars would be needed if supported.
			// Currently, report error as unsupported.
			err := fmt.Errorf("cannot use result of non-string expr (%s, type %s) directly in string concat", node.String(), node.ResultType())
			e.addError("Emitter Limitation: %v. Assign to temp string var first.", err)
			return err
			// --- End Handling ---
		}
	case *ast.GroupedExpression:
		return e.collectStringOperands(node.Expression, operands, tempVarsNeeded)
	case *ast.ProcCallExpression:
		// --- Handling proc call operands ---
		// Similar to non-string binary ops, needs temp var. Currently unsupported directly.
		err := fmt.Errorf("cannot use result of proc call '%s' (type %s) directly in string concat", node.Function.Value, node.ResultType())
		e.addError("Emitter Limitation: %v. Assign result to a variable first.", err)
		return err
		// --- End Handling ---
	default:
		err := fmt.Errorf("unexpected expr type %T in string concat", expr)
		e.addError("Internal Error: %v", err)
		return err
	}
	return nil
}
