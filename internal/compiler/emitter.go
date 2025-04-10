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
			e.addError("Emitter encountered unknown statement type: %T", stmt)
		}
	}

	e.emitFooter()
	return e.builder.String()
}

// Emit Helpers

func (e *Emitter) emitA(line string) {
	e.builder.WriteString(areaAIndent + line + "\n")
}

func (e *Emitter) emitB(line string) {
	e.builder.WriteString(areaBIndent + line + "\n")
}

// Emit Structure

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

		e.builder.WriteString("\n")
	}
}

func (e *Emitter) emitFooter() {
	e.emitB("STOP RUN.")
}

func (e *Emitter) emitPrint(stmt *PrintStatement) {
	if stmt == nil || stmt.Value == nil {
		e.addError("Invalid print statement or value is nil")
		return
	}

	switch v := stmt.Value.(type) {
	case *StringLiteral:
		// TODO: Ensure quotes within the string are handled if necessary
		e.emitB(fmt.Sprintf(`DISPLAY "%s".`, v.Value))
	case *IntegerLiteral:
		e.emitB(fmt.Sprintf(`DISPLAY %d.`, v.Value))
	case *Identifier:
		// Assume parser validated declaration and emit DISPLAY for the variable
		e.emitB(fmt.Sprintf(`DISPLAY %s.`, strings.ToUpper(v.Value)))
	default:
		e.addError("Print statement cannot display value of type %T", v)
	}
}

func (e *Emitter) emitDeclaration(stmt *DeclarationStatement) {
	if stmt == nil || stmt.Name == nil || stmt.Value == nil {
		e.addError("Emitter received invalid assignment statement")
		return
	}

	varName := strings.ToUpper(stmt.Name.Value)

	switch v := stmt.Value.(type) {
	case *StringLiteral:
		e.emitB(fmt.Sprintf(`MOVE "%s" TO %s.`, v.Value, varName))
	case *IntegerLiteral:
		e.emitB(fmt.Sprintf(`MOVE %d TO %s.`, v.Value, varName))
	case *Identifier:
		e.emitB(fmt.Sprintf(`MOVE %s TO %s.`, strings.ToUpper(v.Value), varName))
	default:
		e.addError("Declaration statement cannot assign value of type %T to %s", v, varName)
	}
}

func (e *Emitter) emitReassignment(stmt *ReassignmentStatement) {
	if stmt == nil || stmt.Name == nil || stmt.Value == nil {
		e.addError("Emitter received invalid reassignment statement")
		return
	}

	varName := strings.ToUpper(stmt.Name.Value)

	// Semantic checks (like const assignment, type mismatch) are done by the parser.
	// The emitter assumes the AST is valid at this point and just generates the MOVE.
	switch v := stmt.Value.(type) {
	case *StringLiteral:
		e.emitB(fmt.Sprintf(`MOVE "%s" TO %s.`, v.Value, varName))
	case *IntegerLiteral:
		e.emitB(fmt.Sprintf(`MOVE %d TO %s.`, v.Value, varName))
	case *Identifier:
		e.emitB(fmt.Sprintf(`MOVE %s TO %s.`, strings.ToUpper(v.Value), varName))
	default:
		e.addError("Reassignment statement cannot assign values of type %T to %s", v, varName)
	}
}
