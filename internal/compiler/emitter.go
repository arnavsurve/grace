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
	variables map[string]bool // Track declared variables
}

func NewEmitter() *Emitter {
	return &Emitter{
		variables: make(map[string]bool),
	}
}

func (e *Emitter) Emit(program *Program) string {
	// First pass to collect variables
	for _, stmt := range program.Statements {
		if assignment, ok := stmt.(*AssignmentStatement); ok {
			e.variables[assignment.Name.Value] = true
		}
	}

	e.emitHeader()
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

func (e *Emitter) emitHeader() {
	e.emitA("IDENTIFICATION DIVISION.")
	e.emitA("PROGRAM-ID. MAIN.")
}

func (e *Emitter) emitDataDivision() {
	if len(e.variables) > 0 {
		e.emitA("DATA DIVISION.")
		e.emitA("WORKING-STORAGE SECTION.")

		// Declare variables
		for varName := range e.variables {
			e.emitA(fmt.Sprintf("01 %s PIC X(255).", strings.ToUpper(varName)))
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
		fmt.Println("Error: Invalid print statement or value is nil")
		return
	}

	switch v := stmt.Value.(type) {
	case *StringLiteral:
		e.emitB(fmt.Sprintf(`DISPLAY "%s".`, v.Value))
	case *Identifier:
		e.emitB(fmt.Sprintf(`DISPLAY %s.`, strings.ToUpper(v.Value)))
	default:
		fmt.Printf(`Statement value %v is not a string literal or identifier`, v)
	}
}

func (e *Emitter) emitAssignment(stmt *AssignmentStatement) {
	if stmt == nil || stmt.Value == nil {
		fmt.Println("Error: Invalid assignment statement")
		return
	}

	switch v := stmt.Value.(type) {
	case *StringLiteral:
		e.emitB(fmt.Sprintf(`MOVE "%s" TO %s.`, v.Value, strings.ToUpper(stmt.Name.Value)))
	default:
		fmt.Printf(`Assignment value %v is not a supported type`, v)
	}
}
