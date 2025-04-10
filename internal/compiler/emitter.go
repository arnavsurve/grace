package compiler

import (
	"fmt"
	"sort"
	"strings"
)

const (
	areaAIndent = "       "     // 7 spaces (column 8)
	areaBIndent = "           " // 11 spaces (column 12)
)

type Emitter struct {
	builder strings.Builder
	errors  []string
	// TODO: Add mechanism for temporary variables
	// tempVarCounter int
}

func NewEmitter() *Emitter {
	return &Emitter{
		errors: []string{},
	}
}

func (e *Emitter) addError(format string, args ...any) {
	errMsg := fmt.Sprintf(format, args...)
	e.errors = append(e.errors, errMsg)
}

func (e *Emitter) Errors() []string {
	return e.errors
}

func (e *Emitter) Emit(program *Program, nameWithoutExt string) string {
	e.builder.Reset() // Ensure clean state for multiple calls if needed

	e.emitHeader(nameWithoutExt)
	e.emitDataDivision(program.SymbolTable)

	e.emitA("PROCEDURE DIVISION.")

	for _, stmt := range program.Statements {
		switch stmt := stmt.(type) {
		case *PrintStatement:
			e.emitPrint(stmt)
		case *DeclarationStatement:
			e.emitDeclaration(stmt)
		case *ReassignmentStatement:
			e.emitReassignment(stmt)
		default:
			// This should ideally not happen if the parser produces known types
			// If we land here, check parsing logic for bugs
			e.addError("Emitter encountered unknown statement type: %T", stmt)
		}
	}

	e.emitFooter()

	if len(e.errors) > 0 {
		// TODO: Return errors instead of potentially broken COBOL?
		// Or return COBOL + errors? For now, return COBOL, errors checked separately.
	}

	return e.builder.String()
}

// --- Emit Helpers ---

func (e *Emitter) emitA(line string) {
	e.builder.WriteString(areaAIndent + line + "\n")
}

func (e *Emitter) emitB(line string) {
	e.builder.WriteString(areaBIndent + line + "\n")
}

// --- Emit Structure ---

func (e *Emitter) emitHeader(nameWithoutExt string) {
	e.emitA("IDENTIFICATION DIVISION.")
	programId := fmt.Sprintf("PROGRAM-ID. %s.", strings.ToUpper(nameWithoutExt))
	e.emitA(programId)
	e.builder.WriteString("\n")
}

func (e *Emitter) emitDataDivision(symbolTable map[string]SymbolInfo) {
	if len(symbolTable) > 0 {
		e.emitA("DATA DIVISION.")
		e.emitA("WORKING-STORAGE SECTION.")

		var names []string
		for varName := range symbolTable {
			names = append(names, varName)
		}
		sort.Strings(names)

		// Declare variables
		for _, varName := range names {
			info := symbolTable[varName]
			cobolName := strings.ToUpper(varName)

			switch info.Type {
			case "string":
				e.emitA(fmt.Sprintf("01 %s PIC X(30).", cobolName))
			case "int":
				e.emitA(fmt.Sprintf("01 %s PIC 9(6).", cobolName))
			case "unknown", "undeclared":
				// Do not emit declaration for invalid types found during parsing
				continue
			default:
				e.addError("Unsupported variable type '$s' for '%s' in Data Division", info.Type, varName)
			}
		}
		e.builder.WriteString("\n") // Ensure PROCEDURE DIVISION starts on new line
	}
}

func (e *Emitter) emitFooter() {
	e.emitB("STOP RUN.")
}

// --- Emit Statements ---

func (e *Emitter) emitPrint(stmt *PrintStatement) {
	if stmt == nil || stmt.Value == nil {
		e.addError("Invalid print statement or value is nil")
		return
	}

	// Use emitExpression to get the value representation
	valueStr := e.emitExpression(stmt.Value)
	if valueStr == "" {
		// Error already added by emitExpression or value is inherently unprintable type
		if len(e.errors) == 0 {
			e.addError("Emitter Error: Cannot generate printable value for print statement")
		}
		return
	}

	switch stmt.Value.(type) {
	case *StringLiteral:
		// Identifiers are assumed (by parser checks) to resolve to printable types (string/int for now)
		e.emitB(fmt.Sprintf(`DISPLAY "%s".`, valueStr))
	case *Identifier, *IntegerLiteral:
		e.emitB(fmt.Sprintf(`DISPLAY %s.`, valueStr))
	case *BinaryExpression:
		// TODO: Requires a temporary variable
		// To implement later:
		// 1. Declare a temporary variable (e.g., GRACE-TMP-INT-1 PIC 9(6)).
		// 2. Emit: COMPUTE GRACE-TMP-INT-1 = expression.
		// 3. Emit: DISPLAY GRACE-TMP-INT-1.
		e.addError("Emitter Limitation: Cannot print the result of an arithmetic expression directly. Assign to a variable first.")
	default:
		// Should not happen if parser validates printable types
		e.addError("Print statement cannot display value of type %T", stmt.Value)
	}
}

func (e *Emitter) emitDeclaration(stmt *DeclarationStatement) {
	if stmt == nil || stmt.Name == nil || stmt.Value == nil {
		e.addError("Emitter received invalid assignment statement")
		return
	}

	varName := strings.ToUpper(stmt.Name.Value)
	valueStr := e.emitExpression(stmt.Value)

	if valueStr == "" {
		e.addError("Emitter Error: Could not generate code for value assigned to '%s'", varName)
		return
	}

	// Decide between MOVE and COMPUTE based on the Value's type
	switch v := stmt.Value.(type) {
	// Simple values of variable references use MOVE
	case *IntegerLiteral, *Identifier:
		e.emitB(fmt.Sprintf(`MOVE %s TO %s.`, valueStr, varName))
	case *StringLiteral:
		e.emitB(fmt.Sprintf(`MOVE "%s" TO %s.`, valueStr, varName))
	case *BinaryExpression:
		// Arithmetic expressions use COMPUTE
		e.emitB(fmt.Sprintf("COMPUTE %s = %s.", varName, valueStr))
	default:
		// Should not hapen if parser validates expression types
		e.addError("Declaration statement cannot assign value of type %T to %s", v, varName)
	}
}

func (e *Emitter) emitReassignment(stmt *ReassignmentStatement) {
	if stmt == nil || stmt.Name == nil || stmt.Value == nil {
		e.addError("Emitter received invalid reassignment statement")
		return
	}

	varName := strings.ToUpper(stmt.Name.Value)
	valueStr := e.emitExpression(stmt.Value)

	if valueStr == "" {
		e.addError("Emitter Error: Could not generate code for value reassigned to '%s'", varName)
		return
	}

	// Semantic checks (like const assignment, type mismatch) are done by the parser.
	// The emitter assumes the AST is valid at this point.
	// Decide between MOVE and COMPUTE based on the Value's type.
	switch stmt.Value.(type) {
	case *IntegerLiteral, *Identifier:
		e.emitB(fmt.Sprintf(`MOVE %s TO %s.`, valueStr, varName))
	case *StringLiteral:
		e.emitB(fmt.Sprintf(`MOVE "%s" TO %s.`, valueStr, varName))
	case *BinaryExpression:
		e.emitB(fmt.Sprintf("COMPUTE %s = %s.", varName, valueStr))
	default:
		// Should not hapen if parser validates expression types
		e.addError("Reassignment statement cannot assign values of type %T to %s", stmt.Value, varName)
	}
}

// emitExpression converts an Expression AST node into its COBOL string representation
func (e *Emitter) emitExpression(expr Expression) string {
	switch node := expr.(type) {
	case *Identifier:
		// Assumes parser checked declaration and resolved type. Return uppercase name.
		return strings.ToUpper(node.Value)
	case *IntegerLiteral:
		// Return the integer value as a string
		return fmt.Sprintf("%d", node.Value)
	case *StringLiteral:
		// Return the string literal enclosed in quotes
		// TODO: Handle potential quotes within the string itself
		return fmt.Sprintf("%s", node.Value)
	case *BinaryExpression:
		// Recursively emit left and right operands and combine with the operator
		leftStr := e.emitExpression(node.Left)
		rightStr := e.emitExpression(node.Right)
		if leftStr == "" || rightStr == "" {
			// Error occurred in recursive call
			e.addError("Emitter Error: Failed to emit operands for binary expression")
			return ""
		}
		if _, ok := node.Left.(*BinaryExpression); ok {
			leftStr = fmt.Sprintf("(%s)", leftStr)
		}
		if _, ok := node.Right.(*BinaryExpression); ok {
			rightStr = fmt.Sprintf("(%s)", rightStr)
		}

		return fmt.Sprintf("%s %s %s", leftStr, node.Operator, rightStr)
	// case *GroupedExpression:
	// 	// For COBOL emission, we often don't need explicit parens if COMPUTE handles precedence.
	// 	// However, to be safe/for clarity, we can add them.
	// 	innerStr := e.emitExpression(node.Expression)
	// 	if innerStr == "" {
	// 		return ""
	// 	}
	// 	return fmt.Sprintf("(%s)", innerStr)
	default:
		// This case should ideally not be reached if the parser only produces known expression types
		e.addError("Emitter Error: Cannot emit code for unknown expression type %T", expr)
		return ""
	}
}
