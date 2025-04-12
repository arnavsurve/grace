package compiler

import (
	"testing"
)

// --- Test Helper Functions ---

// checkParserErrors is a common helper function for parser tests.
func checkParserErrors(t *testing.T, p *Parser) {
	t.Helper() // Marks this function as a test helper
	errors := p.Errors()
	if len(errors) == 0 {
		return
	}

	t.Errorf("Parser has %d errors:", len(errors))
	for i, msg := range errors {
		t.Errorf("   Error %d: %q", i+1, msg)
	}
	t.FailNow() // Stop the test if there are parsing errors
}

// --- The Test Case ---

func TestProcSimple(t *testing.T) {
	input := `
proc sayHi() {
  print("hi from proc!")
}

print("calling proc...")
sayHi()
print("...proc called")
`

	l := NewLexer(input)
	p := NewParser(l)
	program := p.ParseProgram()

	// 1. Check for Parser Errors
	checkParserErrors(t, p)

	// 2. Check Program Structure
	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}
	// Expected statements: proc definition, print, call, print
	if len(program.Statements) != 4 {
		t.Fatalf("program.Statements expected=4 statements, got=%d", len(program.Statements))
	}

	// 3. Check Statement 1: Procedure Declaration
	stmt1 := program.Statements[0]
	procDecl, ok := stmt1.(*ProcDeclarationStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *ProcDeclarationStatement. got=%T", stmt1)
	}

	// Check procedure name
	if procDecl.Name == nil {
		t.Fatalf("procDecl.Name is nil")
	}
	if procDecl.Name.Value != "sayHi" {
		t.Errorf("procDecl.Name.Value expected='sayHi', got=%q", procDecl.Name.Value)
	}
	if procDecl.Name.TokenLiteral() != "sayHi" {
		t.Errorf("procDecl.Name.TokenLiteral() expected='sayHi', got=%q", procDecl.Name.TokenLiteral())
	}
	if procDecl.TokenLiteral() != "proc" {
		t.Errorf("procDecl.TokenLiteral() expected='proc', got=%q", procDecl.TokenLiteral())
	}

	// Check procedure body
	if procDecl.Body == nil {
		t.Fatalf("procDecl.Body is nil")
	}
	if len(procDecl.Body.Statements) != 1 {
		t.Fatalf("procDecl.Body.Statements expected=1 statement, got=%d", len(procDecl.Body.Statements))
	}

	// Check statement inside the procedure body
	bodyStmt1 := procDecl.Body.Statements[0]
	printStmtInBody, ok := bodyStmt1.(*PrintStatement)
	if !ok {
		t.Fatalf("procDecl.Body.Statements[0] is not *PrintStatement. got=%T", bodyStmt1)
	}
	if printStmtInBody.Value == nil {
		t.Fatalf("printStmtInBody.Value is nil")
	}
	strLiteralInBody, ok := printStmtInBody.Value.(*StringLiteral)
	if !ok {
		t.Fatalf("printStmtInBody.Value is not *StringLiteral. got=%T", printStmtInBody.Value)
	}
	if strLiteralInBody.Value != "hi from proc!" {
		t.Errorf("strLiteralInBody.Value expected='hi from proc!', got=%q", strLiteralInBody.Value)
	}

	// 4. Check Statement 2: First top-level Print Statement
	stmt2 := program.Statements[1]
	printStmt1, ok := stmt2.(*PrintStatement) // Print is a direct statement
	if !ok {
		t.Fatalf("program.Statements[1] is not *PrintStatement. got=%T", stmt2)
	}
	if printStmt1.Value == nil {
		t.Fatalf("printStmt1.Value is nil")
	}
	strLiteral1, ok := printStmt1.Value.(*StringLiteral)
	if !ok {
		t.Fatalf("printStmt1.Value is not *StringLiteral. got=%T", printStmt1.Value)
	}
	if strLiteral1.Value != "calling proc..." {
		t.Errorf("strLiteral1.Value expected='calling proc...', got=%q", strLiteral1.Value)
	}

	// 5. Check Statement 3: Procedure Call Statement (ExpressionStatement wrapping ProcCallExpression)
	stmt3 := program.Statements[2]
	exprStmt, ok := stmt3.(*ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[2] is not *ExpressionStatement. got=%T", stmt3)
	}
	procCall, ok := exprStmt.Expression.(*ProcCallExpression)
	if !ok {
		t.Fatalf("exprStmt.Expression is not *ProcCallExpression. got=%T", exprStmt.Expression)
	}
	if procCall.Function == nil {
		t.Fatalf("procCall.Function is nil")
	}
	if procCall.Function.Value != "sayHi" {
		t.Errorf("procCall.Function.Value expected='sayHi', got=%q", procCall.Function.Value)
	}
	if len(procCall.Arguments) != 0 {
		t.Errorf("procCall.Arguments expected=0, got=%d", len(procCall.Arguments))
	}

	// 6. Check Statement 4: Second top-level Print Statement
	stmt4 := program.Statements[3]
	printStmt2, ok := stmt4.(*PrintStatement) // Print is a direct statement
	if !ok {
		t.Fatalf("program.Statements[3] is not *PrintStatement. got=%T", stmt4)
	}
	if printStmt2.Value == nil {
		t.Fatalf("printStmt2.Value is nil")
	}
	strLiteral2, ok := printStmt2.Value.(*StringLiteral)
	if !ok {
		t.Fatalf("printStmt2.Value is not *StringLiteral. got=%T", printStmt2.Value)
	}
	if strLiteral2.Value != "...proc called" {
		t.Errorf("strLiteral2.Value expected='...proc called', got=%q", strLiteral2.Value)
	}

	// Optional: Use the String() method for a high-level check (uncomment if String methods are solid)
	// expectedString := `proc sayHi() {
	//     print("hi from proc!")
	// }
	// print("calling proc...")
	// sayHi()
	// print("...proc called")
	// ` // Be careful with exact formatting (newlines, tabs) from String() methods
	// if program.String() != expectedString {
	// 	t.Errorf("program.String() wrong.\nexpected=\n%s\ngot=\n%s", expectedString, program.String())
	// }
}
