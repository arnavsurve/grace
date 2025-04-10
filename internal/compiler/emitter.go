package compiler

import (
	"fmt"
	"strings"
)

const (
	areaAIndent = "       "     // 7 spaces (column 8)
	areaBIndent = "           " // 11 spaces (column 12)
)

type Emitter struct {
	builder   strings.Builder
	variables map[string]string
	errors    []string
}

func NewEmitter() *Emitter {
	return &Emitter{
		variables: make(map[string]string),
		errors:    []string{},
	}
}

func (e *Emitter) addError(format string, args ...interface{}) {
	errMsg := fmt.Sprintf(format, args...)
	e.errors = append(e.errors, errMsg)
}

func (e *Emitter) Errors() []string {
	return e.errors
}

func (e *Emitter) Emit(program *Program, nameWithoutExt string) string {
	// Use the symbol table from the parser to declare variables
	e.variables = program.SymbolTable

	e.emitHeader(nameWithoutExt)
	e.emitDataDivision()

	for _, stmt := range program.Statements {
		switch stmt := stmt.(type) {
		case *PrintStatement:
			e.emitPrint(stmt)
		case *AssignmentStatement:
			e.emitAssignment(stmt)
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
}

func (e *Emitter) emitDataDivision() {
	if len(e.variables) > 0 {
		e.emitA("DATA DIVISION.")
		e.emitA("WORKING-STORAGE SECTION.")

		// Declare variables
		for varName, typ := range e.variables {
			cobolName := strings.ToUpper(varName)

			switch typ {
			case "string":
				e.emitA(fmt.Sprintf("01 %s PIC X(30).", cobolName))
			case "int":
				e.emitA(fmt.Sprintf("01 %s PIC 9(6).", cobolName))
			}
		}

		e.builder.WriteString("\n")
	}

	e.emitA("PROCEDURE DIVISION.")
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
		e.emitB(fmt.Sprintf(`DISPLAY "%s".`, v.Value))
	case *Identifier:
		e.emitB(fmt.Sprintf(`DISPLAY %s.`, strings.ToUpper(v.Value)))
	default:
		e.addError("Statement value %v is not a string literal or identifier", v)
	}
}

func (e *Emitter) emitAssignment(stmt *AssignmentStatement) {
	if stmt == nil || stmt.Value == nil {
		e.addError("Invalid assignment statement")
		return
	}

	switch v := stmt.Value.(type) {
	case *StringLiteral:
		e.emitB(fmt.Sprintf(`MOVE "%s" TO %s.`, v.Value, strings.ToUpper(stmt.Name.Value)))
	case *IntegerLiteral:
		e.emitB(fmt.Sprintf(`MOVE %d TO %s.`, v.Value, strings.ToUpper(stmt.Name.Value)))
	default:
		e.addError("Assignment value %v is not a supported type", v)
	}
}
