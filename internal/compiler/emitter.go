package compiler

import (
	"fmt"
	"sort"
	"strings"

	"github.com/arnavsurve/grace/internal/compiler/lib"
)

// NOTES:
// Temp reuse is safe for single expression statements
// Revisit this when we need to support multiple nested/composite expressions
// Scoped temp management or a flattened IR when this comes up

const (
	areaAIndent     = "       "       // 7 spaces (column 8)
	areaBIndent     = "           "   // 11 spaces (column 12)
	tempIntName     = "GRACE-TMP-INT" // Temporary integer variable
	tempStringName  = "GRACE-TMP-STR" // Temporary string variable
	tempStringWidth = 256             // Max width for temp string var
)

type Emitter struct {
	builder         strings.Builder
	errors          []string
	needsTempInt    bool
	needsTempString bool
}

func NewEmitter() *Emitter {
	return &Emitter{
		errors:          []string{},
		needsTempInt:    false,
		needsTempString: false,
	}
}

func (e *Emitter) addError(format string, args ...any) {
	errMsg := fmt.Sprintf(format, args...)
	e.errors = append(e.errors, errMsg)
}

func (e *Emitter) Errors() []string {
	return e.errors
}

// Pre-pass to check for expression printing
// This will be called before emitting the main divisions
func (e *Emitter) analyzeProgram(program *Program) {
	e.needsTempInt = false // Reset for analysis
	e.needsTempString = false

	for _, stmt := range program.Statements {
		if ps, ok := stmt.(*PrintStatement); ok {
			if binExpr, isBinary := ps.Value.(*BinaryExpression); isBinary {
				if binExpr.ResultType() == "int" {
					e.needsTempInt = true
				} else if binExpr.ResultType() == "string" {
					e.needsTempString = true
				}
				// TODO: check for other complex expressions needing temps later
			}
		}
		// TODO: Check assignments (future: complex RHS might need temps)
		// For now, STRING ... INTO handles direct assignment of concat
	}
}

func (e *Emitter) Emit(program *Program, nameWithoutExt string) string {
	e.builder.Reset() // Ensure clean state for multiple calls if needed

	e.analyzeProgram(program)

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
	hasSymbols := symbolTable != nil && len(symbolTable) > 0
	needsWorkingStorage := hasSymbols || e.needsTempInt || e.needsTempString

	if !needsWorkingStorage {
		return // Skip DATA DIVISION entirely if nothing to declare
	}

	e.emitA("DATA DIVISION.")
	e.emitA("WORKING-STORAGE SECTION.")

	if hasSymbols {
		var names []string
		for varName := range symbolTable {
			names = append(names, varName)
		}
		sort.Strings(names) // Ensure deterministic order

		// Declare user-defined variables
		for _, varName := range names {
			info := symbolTable[varName]
			cobolName := strings.ToUpper(varName)

			if info.Width <= 0 {
				e.addError("Emitter Warning: Invalid width (%d) for variable '%s', using 1.", info.Width, varName)
				info.Width = 1
			}

			switch info.Type {
			case "string":
				e.emitA(fmt.Sprintf("01 %s PIC X(%d).", cobolName, info.Width))
			case "int":
				e.emitA(fmt.Sprintf("01 %s PIC 9(%d).", cobolName, info.Width))
			case "unknown", "undeclared":
				// Parser should prevent this, but warn if it slips through somehow
				// Defensive programming and whatnot
				e.addError("Emitter Warning: Skipping declaration for '%s' with unresolved type '%s'", varName, info.Type)
				continue
			default:
				e.addError("Unsupported variable type '%s' for '%s' in Data Division", info.Type, varName)
			}
		}
	}

	// Add a newline if symbols were declared AND temp vars are needed
	if hasSymbols && (e.needsTempInt || e.needsTempString) {
		e.builder.WriteString("\n")
	}

	// Declare temporary variables if needed
	if e.needsTempInt {
		e.emitA(fmt.Sprintf("01 %s PIC 9(%d).", tempIntName, lib.DefaultIntWidth))
	}
	if e.needsTempString {
		e.emitA(fmt.Sprintf("01 %s PIC X(%d).", tempStringName, tempStringWidth))
	}

	e.builder.WriteString("\n") // Ensure PROCEDURE DIVISION starts on new line
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

	switch exprNode := stmt.Value.(type) {
	case *StringLiteral:
		if exprNode.Value == "" {
			e.emitB("DISPLAY SPACE.") // Use COBOL figurative constant for blank lines rather than a 0 length alphanumeric
		} else {
			// Identifiers are assumed (by parser checks) to resolve to printable types (string/int for now)
			escapedValue := strings.ReplaceAll(exprNode.Value, `"`, `""`)
			e.emitB(fmt.Sprintf(`DISPLAY "%s".`, escapedValue))
		}
	case *IntegerLiteral:
		e.emitB(fmt.Sprintf(`DISPLAY %d.`, exprNode.Value))
	case *Identifier:
		cobolName := strings.ToUpper(exprNode.Value)
		e.emitB(fmt.Sprintf(`DISPLAY %s.`, cobolName))
	case *BinaryExpression:
		// Handle printing expressions (using temp vars)
		if exprNode.ResultType() == "int" {
			if !e.needsTempInt {
				e.addError("Emitter Internal Error: Attempting to print int expression but temp var flag not set.")
				return
			}
			exprStr := e.emitExpression(exprNode)
			if exprStr == "" {
				return
			} // Error handled by emitExpression
			// Check for simple literal case folded by parser
			if _, isLit := exprNode.Left.(*IntegerLiteral); isLit && exprNode.Operator == "" { // Check if it became a literal after folding
				e.emitB(fmt.Sprintf("DISPLAY %s.", exprStr)) // Display folded literal directly
			} else {
				e.emitB(fmt.Sprintf("COMPUTE %s = %s.", tempIntName, exprStr))
				e.emitB(fmt.Sprintf("DISPLAY %s.", tempIntName))
			}
		} else if exprNode.ResultType() == "string" {
			if !e.needsTempString {
				e.addError("Emitter Internal Error: Printing string expression requires temp var, but flag not set.")
				return
			}
			// Check for simple literal case folded by parser
			if lit, isLit := exprNode.Left.(*StringLiteral); isLit && exprNode.Operator == "" {
				if lit.Value == "" {
					e.emitB("DISPLAY SPACE.")
				} else {
					escapedValue := strings.ReplaceAll(lit.Value, `"`, `""`)
					e.emitB(fmt.Sprintf(`DISPLAY "%s".`, escapedValue))
				}
			} else {
				err := e.emitStringConcatenation(tempStringName, exprNode)
				if err != nil {
					return
				} // Error handled by emitStringConcatenation
				e.emitB(fmt.Sprintf("DISPLAY %s.", tempStringName))
			}
		} else {
			e.addError("Emitter Limitation: Cannot print results of expression with unknown or unsupported type '%s'", exprNode.ResultType())
		}
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

	switch v := stmt.Value.(type) {
	case *IntegerLiteral:
		e.emitB(fmt.Sprintf(`MOVE %d TO %s.`, v.Value, varName))
	case *StringLiteral:
		escapedValue := strings.ReplaceAll(v.Value, `"`, `""`)
		e.emitB(fmt.Sprintf(`MOVE "%s" TO %s.`, escapedValue, varName))
	case *Identifier:
		// Assigning one variable to another
		sourceVarName := strings.ToUpper(v.Value)
		e.emitB(fmt.Sprintf(`MOVE %s TO %s.`, sourceVarName, varName))
	case *BinaryExpression:
		resultType := v.ResultType()
		if resultType == "int" {
			// Arithmetic expressions use COMPUTE
			exprStr := e.emitExpression(v)
			if exprStr == "" {
				return
			}
			e.emitB(fmt.Sprintf("COMPUTE %s = %s.", varName, exprStr))
		} else if resultType == "string" {
			// String concatenation uses STRING ... INTO
			err := e.emitStringConcatenation(varName, v)
			if err != nil {
				return
			}
		} else {
			e.addError("Declaration cannot assign result of expression with type '%s' to '%s'", resultType, varName)
		}
	default:
		e.addError("Declaration statement cannot assign value of type %T to '%s'", v, varName)
	}
}

func (e *Emitter) emitReassignment(stmt *ReassignmentStatement) {
	if stmt == nil || stmt.Name == nil || stmt.Value == nil {
		e.addError("Emitter received invalid reassignment statement")
		return
	}

	varName := strings.ToUpper(stmt.Name.Value)

	// Semantic checks (const, type mismatch) are handled by parser. Emitter assumes validity.
	switch v := stmt.Value.(type) {
	case *IntegerLiteral:
		e.emitB(fmt.Sprintf(`MOVE %d TO %s.`, v.Value, varName))
	case *StringLiteral:
		escapedValue := strings.ReplaceAll(v.Value, `"`, `""`)
		e.emitB(fmt.Sprintf(`MOVE "%s" TO %s.`, escapedValue, varName))
	case *Identifier:
		// Reassigning one variable to another
		sourceVarName := strings.ToUpper(v.Value)
		e.emitB(fmt.Sprintf(`MOVE %s TO %s.`, sourceVarName, varName))
	case *BinaryExpression:
		resultType := v.ResultType()
		if resultType == "int" {
			exprStr := e.emitExpression(v)
			if exprStr == "" {
				if len(e.errors) == 0 {
					e.addError("Emitter Error: Could not generate arithmetic expression for reassignment to '%s'", varName)
				}
				return
			}
			e.emitB(fmt.Sprintf("COMPUTE %s = %s.", varName, exprStr))
		} else if resultType == "string" {
			err := e.emitStringConcatenation(varName, v)
			if err != nil {
				return
			}
		} else {
			e.addError("Reassignment cannot assign result of expression with type '%s' to '%s'", resultType, varName)
		}
	default:
		// Should not hapen if parser validates expression types
		e.addError("Reassignment statement cannot assign values of type %T to '%s'", v, varName)
	}
}

// emitExpression converts an Expression AST node into its COBOL string representation
// suitable for use withing COMPUTE or as part of STRING.
func (e *Emitter) emitExpression(expr Expression) string {
	switch node := expr.(type) {
	case *Identifier:
		return strings.ToUpper(node.Value)
	case *IntegerLiteral:
		return fmt.Sprintf("%d", node.Value)
	case *StringLiteral:
		// Return the string literal enclosed in quotes
		// TODO: Handle potential quotes within the string itself
		// NOTE: GnuCOBOL generally handles single quotes inside double quotes. We can assume simple strings for now.
		escapedValue := strings.ReplaceAll(node.Value, `"`, `""`)
		return fmt.Sprintf("%s", escapedValue)
	case *BinaryExpression:
		// Recursively emit left and right operands and combine with the operator
		// This is primarily for COMPUTE. STRING needs different handling.
		leftStr := e.emitExpression(node.Left)
		rightStr := e.emitExpression(node.Right)
		if leftStr == "" || rightStr == "" {
			// Error occurred in recursive call
			if len(e.errors) == 0 {
				e.addError("Emitter Error: Failed to emit operands for binary expression")
			}
			return ""
		}
		if _, ok := node.Left.(*BinaryExpression); ok {
			leftStr = fmt.Sprintf("(%s)", leftStr)
		} else if _, ok := node.Left.(*GroupedExpression); ok {
			leftStr = fmt.Sprintf("(%s)", leftStr)
		}
		if _, ok := node.Right.(*BinaryExpression); ok {
			rightStr = fmt.Sprintf("(%s)", rightStr)
		} else if _, ok := node.Right.(*GroupedExpression); ok {
			rightStr = fmt.Sprintf("(%s)", rightStr)
		}

		if node.ResultType() == "int" {
			return fmt.Sprintf("%s %s %s", leftStr, node.Operator, rightStr)
		} else if node.ResultType() == "string" {
			// We don't return a single string for concat here.
			// The caller (emitDeclaration/Reassignment/Print) handles it via emitStringConcatenation.
			return ""
		} else {
			e.addError("Emitter Error: Cannot emit code for BinaryExpression with unknown result type")
			return ""
		}
	case *GroupedExpression:
		// Return the string for the inner expression. Outer emitExpression calls will add parens if needed
		if node.Expression == nil {
			e.addError("Internal Emitter Error: GroupedExpression has nil inner expression.")
			return ""
		}
		return e.emitExpression(node.Expression) // Just return inner
	default:
		// This case should ideally not be reached if the parser only produces known expression types
		e.addError("Emitter Error: Cannot emit code for unknown expression type %T", expr)
		return ""
	}
}

// emitStringConcatenation generates a COBOL STRING statement.
// It flattens the potentially nested BinaryExpression tree for concat.
func (e *Emitter) emitStringConcatenation(targetCobolVar string, expr *BinaryExpression) error {
	operands := []string{}
	err := e.collectStringOperands(expr, &operands)
	if err != nil {
		return err
	}

	if len(operands) == 0 {
		e.addError("Internal Emitter Error: No operands found for string concatenation")
		return fmt.Errorf("no operands for string concatenation")
	}

	// Emit the start of the STRING statement
	firstOperand := operands[0]
	e.emitB(fmt.Sprintf("STRING %s DELIMITED BY SIZE", firstOperand)) // First operand starts the statement

	// Emit subsequent operands on new lines, indented
	for i := 1; i < len(operands); i++ {
		operand := operands[i]
		e.emitB(fmt.Sprintf("%s DELIMITED BY SIZE", operand))
	}

	e.emitB(fmt.Sprintf("INTO %s.", targetCobolVar))

	return nil
}

func (e *Emitter) collectStringOperands(expr Expression, operands *[]string) error {
	switch node := expr.(type) {
	case *StringLiteral:
		litStr := e.emitExpression(node)
		escapedValue := strings.ReplaceAll(litStr, `"`, `""`)
		quotedLitStr := fmt.Sprintf(`"%s"`, escapedValue)
		if litStr == "" && escapedValue != "" {
			return fmt.Errorf("failed to emit string literal")
		}
		*operands = append(*operands, quotedLitStr)
	case *Identifier:
		// Get the uppercase variable name
		identStr := e.emitExpression(node)
		if identStr == "" {
			return fmt.Errorf("failed to emit indentifier")
		}
		*operands = append(*operands, identStr)
	case *BinaryExpression:
		if node.Operator == "+" && node.ResultType() == "string" {
			// Recursively collect form left, then right
			err := e.collectStringOperands(node.Left, operands)
			if err != nil {
				return err
			}
			err = e.collectStringOperands(node.Right, operands)
			if err != nil {
				return err
			}
		} else {
			// Should be caught by parser, defensive check
			e.addError("Internal Emitter Error: Unexpected operator '%s' or type in string concatenation traversal", node.Operator)
			return fmt.Errorf("invalid node in string concatenation")
		}
	case *GroupedExpression:
		// Recurse into the grouped expression
		return e.collectStringOperands(node.Expression, operands)
	default:
		e.addError("Internal Emitter Error: Unexpected expression type %T encountered during string concatenation", expr)
		return fmt.Errorf("unexpected node type %T", expr)
	}
	return nil
}
