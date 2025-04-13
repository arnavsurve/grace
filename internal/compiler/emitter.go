package compiler

import (
	"fmt"
	"sort"
	"strings"

	"github.com/arnavsurve/grace/internal/compiler/lib"
)

// NOTES:
// - Parameter scoping currently uses global WORKING-STORAGE (needs LOCAL-STORAGE or name mangling).
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

	// Names for dedicated return value vars
	returnIntVarName    = "GRACE-RETURN-INT"
	returnStringVarName = "GRACE-RETURN-STR"
	// Widths for return vars
	returnDefaultIntWidth    = 18
	returnDefaultStringWidth = 256
)

type Emitter struct {
	builder         strings.Builder
	errors          []string
	needsTempInt    bool
	needsTempString bool
	needsReturnInt  bool
	needsReturnStr  bool
	declaredVars    map[string]bool       // Tracks COBOL var names declared in WORKING-STORAGE
	procInfo        map[string]SymbolInfo // Cache of all symbol info from parser
}

func NewEmitter() *Emitter {
	return &Emitter{
		errors:       []string{},
		declaredVars: make(map[string]bool),
		procInfo:     make(map[string]SymbolInfo),
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
				if maxFirstChunkLen < 0 {
					maxFirstChunkLen = 0
				} // Safety

				firstChunk := literalContent
				if len(firstChunk) > maxFirstChunkLen {
					firstChunk = firstChunk[:maxFirstChunkLen]
				}
				remaining := literalContent[len(firstChunk):]

				// Emit first line
				e.builder.WriteString(fmt.Sprintf("%sDISPLAY \"%s\"\n", areaBIndent, firstChunk))

				// Max length for subsequent chunks
				maxLenContinue := cobolLineEndCol - len(continuationPrefix) - 2 // -"literal"
				if maxLenContinue < 0 {
					maxLenContinue = 0
				} // Safety

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
			if maxLenFirstLineLiteral < 0 {
				maxLenFirstLineLiteral = 0
			}

			firstChunk := literalContent
			if len(firstChunk) > maxLenFirstLineLiteral {
				firstChunk = firstChunk[:maxLenFirstLineLiteral]
			}
			remainingLiteral := literalContent[len(firstChunk):]

			// Emit first line: MOVE "chunk"
			e.builder.WriteString(fmt.Sprintf("%sMOVE \"%s\"\n", areaBIndent, firstChunk))

			// Max length for subsequent literal chunks on continuation lines
			maxLenContinueLiteral := cobolLineEndCol - len(continuationPrefix) - 2 // -"literal"
			if maxLenContinueLiteral < 0 {
				maxLenContinueLiteral = 0
			}

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
	if len(areaBIndent+finalLine) > cobolLineEndCol {
		e.addError("Emitter Warning: Generated COBOL line may exceed %d columns: %s", cobolLineEndCol, areaBIndent+finalLine)
	}

	e.builder.WriteString(areaBIndent + finalLine + "\n")
}

func (e *Emitter) emitComment(comment string) {
	line := "      *" + comment
	e.builder.WriteString(line + "\n")
}

// --- Analysis Phase ---

func (e *Emitter) analyzeProgram(program *Program) {
	e.needsTempInt = false
	e.needsTempString = false
	e.needsReturnInt = false
	e.needsReturnStr = false
	e.procInfo = make(map[string]SymbolInfo) // Use this as the main cache

	if program.SymbolTable == nil {
		e.addError("Internal Emitter Error: Program Symbol Table is nil during analysis phase.")
		return
	}

	// Populate e.procInfo with ALL symbols and check global needs
	for name, info := range program.SymbolTable {
		e.procInfo[name] = info // Copy all symbols
		if info.Type == "proc" {
			if info.ReturnType == "int" {
				e.needsReturnInt = true
			}
			if info.ReturnType == "string" {
				e.needsReturnStr = true
			}
		}
	}

	// Traverse AST to check for complex operations needing temps
	for _, stmt := range program.Statements {
		e.analyzeStatement(stmt)
	}
}

func (e *Emitter) analyzeStatement(stmt Statement) {
	switch s := stmt.(type) {
	case *ProcDeclarationStatement:
		if s.Body != nil {
			for _, bodyStmt := range s.Body.Statements {
				e.analyzeStatement(bodyStmt)
			}
		}
	case *PrintStatement:
		e.analyzeExpression(s.Value)
	case *DeclarationStatement:
		e.analyzeExpression(s.Value)
	case *ReassignmentStatement:
		e.analyzeExpression(s.Value)
	case *ReturnStatement:
		if s.ReturnValue != nil {
			e.analyzeExpression(s.ReturnValue)
		}
	case *ExpressionStatement:
		e.analyzeExpression(s.Expression)
	case *BlockStatement:
		for _, blockStmt := range s.Statements {
			e.analyzeStatement(blockStmt)
		}
	}
}

func (e *Emitter) analyzeExpression(expr Expression) {
	if expr == nil {
		return
	}
	switch ex := expr.(type) {
	case *BinaryExpression:
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
	case *GroupedExpression:
		e.analyzeExpression(ex.Expression)
	case *ProcCallExpression:
		for _, arg := range ex.Arguments {
			e.analyzeExpression(arg)
		}
	// Base cases (Identifier, Literals) don't trigger needs flags
	case *Identifier, *IntegerLiteral, *StringLiteral:
	}
}

// Helper to check if a binary int expression was folded by the parser
func isConstantFoldedInt(be *BinaryExpression) bool {
	_, leftIsLit := be.Left.(*IntegerLiteral)
	// If Left is literal and Operator is empty, it was folded.
	return leftIsLit && be.Operator == ""
}

// Helper to check if a binary string expression was folded by the parser
func isConstantFoldedString(be *BinaryExpression) bool {
	_, leftIsLit := be.Left.(*StringLiteral)
	// If Left is literal and Operator is empty, it was folded.
	return leftIsLit && be.Operator == ""
}

// --- Main Emit Function ---

func (e *Emitter) Emit(program *Program, nameWithoutExt string) string {
	e.builder.Reset()
	e.errors = []string{}
	e.declaredVars = make(map[string]bool)
	e.procInfo = make(map[string]SymbolInfo)

	// Analyze first to set flags and cache procInfo
	e.analyzeProgram(program)

	// Emit Divisions
	e.emitHeader(nameWithoutExt)
	// Pass the full symbol table from the program (parser output)
	e.emitDataDivision(program.SymbolTable)

	e.builder.WriteString(areaAIndent + "PROCEDURE DIVISION.\n")

	hasProcs := false
	for _, stmt := range program.Statements {
		if _, ok := stmt.(*ProcDeclarationStatement); ok {
			hasProcs = true
			break
		}
	}

	if hasProcs {
		e.builder.WriteString(areaAIndent + "DECLARATIVES.\n\n")
		for _, stmt := range program.Statements {
			if procDecl, ok := stmt.(*ProcDeclarationStatement); ok {
				e.emitStatement(procDecl)
			}
		}
		e.builder.WriteString("\n" + areaAIndent + "END DECLARATIVES.\n\n")
	} else {
		e.builder.WriteString("\n")
	}

	e.builder.WriteString(areaAIndent + "MAIN SECTION.\n")
	for _, stmt := range program.Statements {
		if _, ok := stmt.(*ProcDeclarationStatement); !ok {
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

// emitDataDivision declares global variables, parameters (as globals currently),
// and necessary helper variables (temps, returns).
func (e *Emitter) emitDataDivision(symbolTable map[string]SymbolInfo) {
	// fmt.Println("DEBUG: >>> Entering emitDataDivision <<<")
	e.declaredVars = make(map[string]bool) // Reset for this emission

	// --- Determine Need for WORKING-STORAGE ---
	needsWorkingStorage := false
	hasGlobals := false
	hasParams := false

	if symbolTable == nil {
		e.addError("Internal Emitter Error: Symbol Table is nil during Data Division emission.")
		symbolTable = make(map[string]SymbolInfo) // Avoid nil panic
	}

	// Use the cached e.procInfo which contains ALL symbols from analyzeProgram
	for _, info := range e.procInfo {
		if info.Type == "proc" {
			if len(info.ParamNames) > 0 {
				hasParams = true
			}
		} else {
			hasGlobals = true
		}
		if hasGlobals && hasParams {
			break
		}
	}
	needsWorkingStorage = hasGlobals || hasParams || e.needsTempInt || e.needsTempString || e.needsReturnInt || e.needsReturnStr

	if !needsWorkingStorage {
		// fmt.Println("DEBUG: Skipping DATA DIVISION (nothing to declare).")
		return
	}

	e.builder.WriteString(areaAIndent + "DATA DIVISION.\n")
	e.builder.WriteString(areaAIndent + "WORKING-STORAGE SECTION.\n")

	// --- Declare Global Variables (using e.procInfo which has all symbols) ---
	// fmt.Println("DEBUG: Declaring Global Variables...")
	var globalVarNames []string
	for name, info := range e.procInfo {
		if info.Type != "proc" { // Identify globals (non-procs)
			globalVarNames = append(globalVarNames, name)
		}
	}
	sort.Strings(globalVarNames)
	// fmt.Printf("DEBUG: Global symbols to declare: %v\n", globalVarNames)

	for _, varName := range globalVarNames {
		info := e.procInfo[varName] // Get info from cache
		cobolName := sanitizeIdentifier(varName)
		// fmt.Printf("DEBUG: Processing Global var: '%s'\n", varName)

		if _, declared := e.declaredVars[cobolName]; declared {
			e.emitComment(fmt.Sprintf("Skipping duplicate global declaration for '%s'", cobolName))
			// fmt.Printf("  -> DEBUG: SKIPPED global '%s' (already declared)\n", cobolName)
			continue
		}

		width := info.Width
		if width <= 0 && info.Type != "unknown" && info.Type != "undeclared" {
			resolvedWidth := lib.GetDefaultWidth(info.Type)
			if resolvedWidth <= 0 {
				resolvedWidth = 1
			}
			e.addError("Emitter Warning: Invalid width (%d) for global variable '%s' (type %s), using %d.", info.Width, varName, info.Type, resolvedWidth)
			width = resolvedWidth
		}

		var picClause string = ""
		switch info.Type {
		case "string":
			if width > 0 {
				picClause = fmt.Sprintf("01 %s PIC X(%d)", cobolName, width)
			} else { /* error */
			}
		case "int":
			if width > 0 {
				picClause = fmt.Sprintf("01 %s PIC 9(%d)", cobolName, width)
			} else { /* error */
			}
		case "unknown", "undeclared":
			continue // Already warned by parser or handled as error
		case "void":
			continue // Cannot declare void vars
		default:
			e.addError("Unsupported global type '%s' for '%s'", info.Type, varName)
			continue
		}

		if picClause != "" {
			e.builder.WriteString(areaAIndent + picClause + ".\n")
			e.declaredVars[cobolName] = true
			// fmt.Printf("  -> DEBUG: Added Global '%s' to declaredVars\n", cobolName)
		} else {
			// fmt.Printf("  -> DEBUG: SKIPPED adding Global '%s' (picClause empty/invalid)\n", cobolName)
		}
	}

	// --- Declare Parameters ---
	if hasParams {
		// fmt.Println("DEBUG: Declaring Parameters...")
		procNames := make([]string, 0, len(e.procInfo))
		for name, info := range e.procInfo {
			if info.Type == "proc" {
				procNames = append(procNames, name)
			}
		}
		sort.Strings(procNames)

		for _, procName := range procNames {
			procInfo := e.procInfo[procName]
			if len(procInfo.ParamNames) == 0 {
				continue
			}
			// fmt.Printf("DEBUG: Processing params for proc '%s'\n", procName)

			for i, paramName := range procInfo.ParamNames {
				cobolName := sanitizeIdentifier(paramName)
				// fmt.Printf("DEBUG:   Processing parameter '%s' (COBOL: %s)\n", paramName, cobolName)

				if _, declared := e.declaredVars[cobolName]; declared {
					e.emitComment(fmt.Sprintf("Skipping duplicate parameter declaration for '%s'", cobolName))
					// fmt.Printf("  -> DEBUG: SKIPPED param '%s' (already declared)\n", cobolName)
					continue
				}

				paramType := procInfo.ParamTypes[i]
				paramWidth := procInfo.ParamWidths[i]
				if paramWidth <= 0 {
					paramWidth = 1 /* Error/Warning */
				}

				var picClause string = ""
				switch paramType {
				case "string":
					picClause = fmt.Sprintf("01 %s PIC X(%d)", cobolName, paramWidth)
				case "int":
					picClause = fmt.Sprintf("01 %s PIC 9(%d)", cobolName, paramWidth)
				default:
					continue // Should not happen
				}

				e.builder.WriteString(areaAIndent + picClause + ".\n")
				e.emitComment(fmt.Sprintf("Parameter '%s' for procedure %s", paramName, procName))
				e.declaredVars[cobolName] = true
				// fmt.Printf("  -> DEBUG: Added Param '%s' to declaredVars\n", cobolName)
			}
		}
	} // end if hasParams

	// fmt.Println("DEBUG: Finished declaring user vars/params.")

	// --- Declare Helper Variables ---
	needsHelpers := e.needsTempInt || e.needsTempString || e.needsReturnInt || e.needsReturnStr
	if needsHelpers {
		e.builder.WriteString("\n")
		e.emitComment("GRACE Compiler Helper Variables")
		// fmt.Println("DEBUG: Declaring Helper Variables...")
	}
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
	if e.needsReturnInt {
		cobolName := returnIntVarName
		if _, declared := e.declaredVars[cobolName]; !declared {
			e.builder.WriteString(fmt.Sprintf("%s01 %s PIC 9(%d).\n", areaAIndent, cobolName, returnDefaultIntWidth))
			e.declaredVars[cobolName] = true
		}
	}
	if e.needsReturnStr {
		cobolName := returnStringVarName
		if _, declared := e.declaredVars[cobolName]; !declared {
			e.builder.WriteString(fmt.Sprintf("%s01 %s PIC X(%d).\n", areaAIndent, cobolName, returnDefaultStringWidth))
			e.declaredVars[cobolName] = true
		}
	}

	if needsWorkingStorage {
		e.builder.WriteString("\n")
	}
	// fmt.Println("DEBUG: <<< Exiting emitDataDivision >>>")
}

// --- Emit Statements ---

func (e *Emitter) emitStatement(stmt Statement) {
	switch s := stmt.(type) {
	case *ReturnStatement:
		e.emitReturnStatement(s)
	case *ProcDeclarationStatement:
		e.emitProcDeclaration(s)
	case *ExpressionStatement:
		e.emitExpressionStatement(s)
	case *BlockStatement:
		e.emitBlockStatement(s)
	case *PrintStatement:
		e.emitPrint(s)
	case *DeclarationStatement:
		e.emitDeclaration(s)
	case *ReassignmentStatement:
		e.emitReassignment(s)
	default:
		e.addError("Emitter encountered unknown statement type: %T", stmt)
	}
}

func (e *Emitter) emitPrint(stmt *PrintStatement) {
	if stmt == nil || stmt.Value == nil {
		e.addError("Emitter Error: Invalid print statement (nil)")
		return
	}
	valueStr, isLiteral, err := e.emitExpressionForResult(stmt.Value)
	if err != nil {
		return
	} // Error handled
	if valueStr == "" { /* Error handled */
		return
	}

	if isLiteral {
		if strLit, ok := stmt.Value.(*StringLiteral); ok && strLit.Value == "" {
			e.emitB("DISPLAY SPACE")
		} else {
			e.emitB(fmt.Sprintf(`DISPLAY %s`, valueStr))
		}
	} else {
		e.emitB(fmt.Sprintf(`DISPLAY %s`, valueStr))
	}
}

func (e *Emitter) emitDeclaration(stmt *DeclarationStatement) {
	if stmt == nil || stmt.Name == nil || stmt.Value == nil {
		e.addError("Emitter Error: Invalid declaration statement (nil)")
		return
	}
	targetVarCobolName := sanitizeIdentifier(stmt.Name.Value)
	if _, declared := e.declaredVars[targetVarCobolName]; !declared {
		// This implies a parser error allowed use before declaration/scope issue,
		// or the declaration itself failed earlier in emitDataDivision.
		e.addError("Emitter Warning: Skipping assignment for '%s' as it was not declared.", targetVarCobolName)
		return
	}
	err := e.emitAssignment(targetVarCobolName, stmt.Value)
	_ = err // Error is added within emitAssignment or its callees
}

func (e *Emitter) emitReassignment(stmt *ReassignmentStatement) {
	if stmt == nil || stmt.Name == nil || stmt.Value == nil {
		e.addError("Emitter Error: Invalid reassignment statement (nil)")
		return
	}
	targetVarCobolName := sanitizeIdentifier(stmt.Name.Value)
	if _, declared := e.declaredVars[targetVarCobolName]; !declared {
		e.addError("Emitter Warning: Skipping reassignment for '%s' as it was not declared.", targetVarCobolName)
		return
	}
	// TODO: Add check here or in parser: Cannot reassign const variable
	err := e.emitAssignment(targetVarCobolName, stmt.Value)
	_ = err // Error handled internally
}

// --- Core Expression/Assignment Helpers ---

func (e *Emitter) emitExpressionForResult(expr Expression) (string, bool, error) {
	if expr == nil {
		return "", false, fmt.Errorf("cannot emit nil expression")
	}

	switch node := expr.(type) {
	case *Identifier:
		cobolName := sanitizeIdentifier(node.Value)
		// Check if it's a known variable/parameter OR a procedure name
		info, declared := e.procInfo[node.Value] // Check combined cache
		if !declared {
			err := fmt.Errorf("reference to undeclared identifier '%s' used in expression", node.Value)
			e.addError("Emitter Error: %v", err)
			return "", false, err
		}
		if info.Type == "proc" {
			// This happens if proc name is used without (), parser should error ideally
			err := fmt.Errorf("procedure name '%s' used as a value", node.Value)
			e.addError("Emitter Error: %v", err)
			return "", false, err
		}
		// It's a declared variable/parameter
		return cobolName, false, nil

	case *IntegerLiteral:
		return fmt.Sprintf("%d", node.Value), true, nil

	case *StringLiteral:
		escapedValue := strings.ReplaceAll(node.Value, `"`, `""`)
		return fmt.Sprintf(`"%s"`, escapedValue), true, nil

	case *BinaryExpression:
		if isConstantFoldedInt(node) {
			return fmt.Sprintf("%d", node.Left.(*IntegerLiteral).Value), true, nil
		}
		if isConstantFoldedString(node) {
			escapedValue := strings.ReplaceAll(node.Left.(*StringLiteral).Value, `"`, `""`)
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

	case *GroupedExpression:
		return e.emitExpressionForResult(node.Expression)

	case *ProcCallExpression:
		resultVar, err := e.emitProcCallAndGetResultVar(node)
		if err != nil {
			return "", false, err
		}
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

func (e *Emitter) emitAssignment(targetVarCobolName string, rhsExpr Expression) error {
	sourceEntity, isLiteral, err := e.emitExpressionForResult(rhsExpr)
	if err != nil {
		return err
	}
	if sourceEntity == "" {
		return fmt.Errorf("invalid RHS expression for assignment")
	}

	if !isLiteral && sourceEntity == targetVarCobolName { // Self-assignment x = x
		e.emitComment(fmt.Sprintf("Assignment of %s to itself - no MOVE generated", targetVarCobolName))
		return nil
	}

	// Check if RHS was originally a binary expression that wasn't folded
	// If so, emit COMPUTE/STRING directly into target instead of via temp var.
	actualRhsExpr := rhsExpr
	if grp, ok := rhsExpr.(*GroupedExpression); ok {
		actualRhsExpr = grp.Expression
	}

	if binExpr, ok := actualRhsExpr.(*BinaryExpression); ok {
		if !isConstantFoldedInt(binExpr) && !isConstantFoldedString(binExpr) {
			if binExpr.ResultType() == "int" {
				computeStr, err := e.emitExpressionForCompute(binExpr)
				if err != nil {
					return err
				}
				e.emitB(fmt.Sprintf("COMPUTE %s = %s", targetVarCobolName, computeStr))
				return nil
			} else if binExpr.ResultType() == "string" {
				err := e.emitStringConcatenation(targetVarCobolName, binExpr) // Directly into target
				return err
			}
			// If type is unknown, fall through (error should exist)
		}
	}

	// Default: MOVE the result (literal, var, temp-var, return-var)
	rhsType := rhsExpr.ResultType()
	if rhsType == "void" {
		err = fmt.Errorf("cannot assign void result to %s", targetVarCobolName)
		e.addError("Emitter Error: %v", err)
		return err
	}
	if rhsType == "unknown" { /* Allow if parser allowed, error should exist */
	}
	// Assume MOVE works for int/string/unknown that passed parser
	e.emitB(fmt.Sprintf(`MOVE %s TO %s`, sourceEntity, targetVarCobolName))
	return nil
}

func (e *Emitter) emitExpressionForCompute(expr Expression) (string, error) {
	if expr == nil {
		err := fmt.Errorf("nil expression")
		e.addError("Internal Emitter Error: %v", err)
		return "", err
	}
	switch node := expr.(type) {
	case *Identifier:
		cobolName := sanitizeIdentifier(node.Value)
		info, ok := e.procInfo[node.Value]
		if !ok {
			err := fmt.Errorf("undeclared id '%s' in arithmetic", node.Value)
			e.addError("Emitter Error: %v", err)
			return "", err
		}
		if info.Type != "int" {
			err := fmt.Errorf("id '%s' (type %s) in arithmetic", node.Value, info.Type)
			e.addError("Emitter Error: %v", err)
			return "", err
		}
		return cobolName, nil
	case *IntegerLiteral:
		return fmt.Sprintf("%d", node.Value), nil
	case *StringLiteral:
		err := fmt.Errorf("string literal '%s' in arithmetic", node.Value)
		e.addError("Emitter Error: %v", err)
		return "", err
	case *BinaryExpression:
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
		if _, ok := node.Left.(*BinaryExpression); ok {
			leftStr = fmt.Sprintf("( %s )", leftStr)
		}
		if _, ok := node.Left.(*GroupedExpression); ok {
			leftStr = fmt.Sprintf("( %s )", leftStr)
		}
		if _, ok := node.Right.(*BinaryExpression); ok {
			rightStr = fmt.Sprintf("( %s )", rightStr)
		}
		if _, ok := node.Right.(*GroupedExpression); ok {
			rightStr = fmt.Sprintf("( %s )", rightStr)
		}
		if node.Operator == "/" {
			if lit, ok := node.Right.(*IntegerLiteral); ok && lit.Value == 0 {
				e.emitComment("WARNING: Potential division by zero")
			}
		}
		return fmt.Sprintf("%s %s %s", leftStr, node.Operator, rightStr), nil
	case *GroupedExpression:
		innerExprStr, err := e.emitExpressionForCompute(node.Expression)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("( %s )", innerExprStr), nil
	case *ProcCallExpression:
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
func (e *Emitter) emitProcDeclaration(stmt *ProcDeclarationStatement) {
	if stmt == nil || stmt.Name == nil || stmt.Body == nil || stmt.ReturnType == nil {
		e.addError("Emitter Error: Invalid proc decl node")
		return
	}
	procCobolName := sanitizeIdentifier(stmt.Name.Value)
	_, ok := e.procInfo[stmt.Name.Value]
	if !ok {
		e.addError("Internal Emitter Error: Symbol info missing for proc '%s'", stmt.Name.Value)
	}

	e.builder.WriteString(areaAIndent + procCobolName + " SECTION." + "\n") // Use raw write to control period

	paramStrings := []string{}
	for _, p := range stmt.Parameters {
		paramStrings = append(paramStrings, p.String())
	}
	returnString := stmt.ReturnType.String()
	sigComment := fmt.Sprintf(" Grace Proc: proc %s(%s): %s ", stmt.Name.Value, strings.Join(paramStrings, ", "), returnString)
	e.emitComment(sigComment)

	e.emitStatement(stmt.Body) // Emits body statements

	e.emitB("EXIT SECTION")     // emitB adds the period
	e.builder.WriteString("\n") // Add blank line after section for readability
}

func (e *Emitter) emitBlockStatement(block *BlockStatement) {
	if block == nil {
		return
	}
	for _, stmt := range block.Statements {
		e.emitStatement(stmt)
	}
}

func (e *Emitter) emitReturnStatement(stmt *ReturnStatement) {
	if stmt == nil {
		e.addError("Emitter Error: Invalid return statement node")
		return
	}

	// --- Determine Target Return Var ---
	// !!! This still uses the flawed inference based on return value type.
	// !!! Needs context tracking for proper implementation.
	expectedReturnType := "void"
	exprToReturn := stmt.ReturnValue
	targetReturnVar := ""

	if exprToReturn != nil {
		exprType := exprToReturn.ResultType()
		switch exprType {
		case "int":
			expectedReturnType = "int"
		case "string":
			expectedReturnType = "string"
		case "void":
			e.addError("Emitter Error: Attempting to return void expr")
			return
		case "unknown":
			e.addError("Emitter Error: Cannot return value of unknown type")
			return
		default:
			e.addError("Emitter Error: Cannot return value of type '%s'", exprType)
			return
		}
	}

	switch expectedReturnType {
	case "int":
		targetReturnVar = returnIntVarName
		if !e.needsReturnInt {
			e.addError("Internal Emitter Error: Return int needed but %s not declared", targetReturnVar)
			return
		}
		if exprToReturn == nil {
			e.addError("Emitter Error: Missing return value for non-void (int) proc")
			return
		}
	case "string":
		targetReturnVar = returnStringVarName
		if !e.needsReturnStr {
			e.addError("Internal Emitter Error: Return string needed but %s not declared", targetReturnVar)
			return
		}
		if exprToReturn == nil {
			e.addError("Emitter Error: Missing return value for non-void (string) proc")
			return
		}
	case "void":
		if exprToReturn != nil {
			e.addError("Emitter Warning: Return in void context has value; ignoring.")
		}
		return // Nothing to emit for void return itself
	default:
		e.addError("Internal Emitter Error: Unexpected return type '%s'", expectedReturnType)
		return
	}

	// --- Emit Assignment to Return Var ---
	err := e.emitAssignment(targetReturnVar, exprToReturn) // Uses emitB internally
	_ = err                                                // Error handled within emitAssignment
}

func (e *Emitter) emitExpressionStatement(stmt *ExpressionStatement) {
	if stmt == nil || stmt.Expression == nil {
		e.addError("Emitter Error: Invalid expression statement node")
		return
	}
	if procCall, ok := stmt.Expression.(*ProcCallExpression); ok {
		_, err := e.emitProcCallAndGetResultVar(procCall) // Call, ignore result var name
		_ = err                                           // Error handled internally
	} else {
		e.addError("Emitter Warning: Expression of type %T used as statement has no effect.", stmt.Expression)
	}
}

func (e *Emitter) emitProcCallAndGetResultVar(callExpr *ProcCallExpression) (string, error) {
	if callExpr == nil || callExpr.Function == nil {
		err := fmt.Errorf("invalid proc call AST")
		e.addError("Emitter Error: %v", err)
		return "", err
	}
	procName := callExpr.Function.Value
	procCobolName := sanitizeIdentifier(procName)
	procSymInfo, ok := e.procInfo[procName]
	if !ok {
		err := fmt.Errorf("symbol info missing for proc '%s'", procName)
		e.addError("Internal Error: %v", err)
		return "", err
	}

	// --- 1. Emit Argument Assignments ---
	if len(callExpr.Arguments) != len(procSymInfo.ParamNames) {
		err := fmt.Errorf("arg count mismatch for proc '%s'", procName)
		e.addError("Emitter Error: %v", err)
		return "", err
	}
	for i, argExpr := range callExpr.Arguments {
		if i >= len(procSymInfo.ParamNames) {
			err := fmt.Errorf("arg index %d out of bounds for '%s'", i, procName)
			e.addError("Internal Error: %v", err)
			return "", err
		}
		paramGraceName := procSymInfo.ParamNames[i]
		paramCobolName := sanitizeIdentifier(paramGraceName)
		if _, declared := e.declaredVars[paramCobolName]; !declared {
			err := fmt.Errorf("param '%s' for proc '%s' not declared", paramCobolName, procCobolName)
			e.addError("Internal Error: %v", err)
			return "", err
		}
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
	case "int":
		if !e.needsReturnInt {
			err := fmt.Errorf("proc '%s' returns int, but %s not declared", procName, returnIntVarName)
			e.addError("Internal Error: %v", err)
			return "", err
		}
		return returnIntVarName, nil
	case "string":
		if !e.needsReturnStr {
			err := fmt.Errorf("proc '%s' returns string, but %s not declared", procName, returnStringVarName)
			e.addError("Internal Error: %v", err)
			return "", err
		}
		return returnStringVarName, nil
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

func (e *Emitter) emitStringConcatenation(targetCobolVar string, expr *BinaryExpression) error {
	operands := []string{}
	tempVarsNeeded := make(map[string]string) // Should not be needed with current collectStringOperands

	err := e.collectStringOperands(expr, &operands, tempVarsNeeded) // Pass map, though likely unused
	if err != nil {
		return err
	}
	if len(operands) == 0 {
		e.emitB(fmt.Sprintf(`MOVE "" TO %s`, targetCobolVar))
		return nil
	}

	// --- Emit intermediate steps (if any were collected - currently none expected) ---
	for _, stmt := range tempVarsNeeded {
		e.emitB(stmt)
	}

	// --- Emit STRING statement ---
	e.emitB(fmt.Sprintf(`MOVE SPACES TO %s`, targetCobolVar)) // Initialize target

	var stringStmt strings.Builder
	stringStmt.WriteString(fmt.Sprintf("STRING %s DELIMITED BY SIZE", operands[0]))
	for i := 1; i < len(operands); i++ {
		stringStmt.WriteString("\n")
		stringStmt.WriteString(fmt.Sprintf("%s       %s DELIMITED BY SIZE", areaBIndent, operands[i]))
	}
	stringStmt.WriteString("\n")
	stringStmt.WriteString(fmt.Sprintf("%s    INTO %s", areaBIndent, targetCobolVar))

	e.emitB(stringStmt.String()) // Pass whole block to emitB for final period

	return nil
}

// collectStringOperands recursively gathers COBOL entities (literals/vars) for STRING.
// CURRENTLY DOES NOT SUPPORT non-string/non-concat expressions requiring temp vars.
func (e *Emitter) collectStringOperands(expr Expression, operands *[]string, tempVarsNeeded map[string]string) error {
	if expr == nil {
		err := fmt.Errorf("nil expression")
		e.addError("Internal Error: %v", err)
		return err
	}
	switch node := expr.(type) {
	case *StringLiteral:
		litStr, _, err := e.emitExpressionForResult(node)
		if err != nil {
			return err
		}
		*operands = append(*operands, litStr)
	case *Identifier:
		cobolName := sanitizeIdentifier(node.Value)
		info, ok := e.procInfo[node.Value] // Check cache (includes globals/params)
		if !ok {
			err := fmt.Errorf("undeclared id '%s' in string concat", node.Value)
			e.addError("Emitter Error: %v", err)
			return err
		}
		if info.Type != "string" {
			err := fmt.Errorf("id '%s' (type %s) in string concat", node.Value, info.Type)
			e.addError("Emitter Error: %v", err)
			return err
		}
		*operands = append(*operands, cobolName)
	case *BinaryExpression:
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
	case *GroupedExpression:
		return e.collectStringOperands(node.Expression, operands, tempVarsNeeded)
	case *ProcCallExpression:
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
