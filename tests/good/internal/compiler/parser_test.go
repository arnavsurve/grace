package compiler

import (
	"testing"

	"github.com/arnavsurve/grace/internal/compiler/ast"
	"github.com/arnavsurve/grace/internal/compiler/lexer"
	"github.com/arnavsurve/grace/internal/compiler/lib"
	"github.com/arnavsurve/grace/internal/compiler/parser"
)

// --- Test Helper Functions ---

// checkParserErrors checks for fatal errors. Logs warnings but doesn't fail for them.
func checkParserErrors(t *testing.T, p *parser.Parser) {
	t.Helper() // Marks this function as a test helper
	errors := p.Errors()
	warnings := p.Warnings()

	// Print warnings if any occurred
	if len(warnings) > 0 {
		t.Logf("Parser produced %d warnings:", len(warnings))
		for i, msg := range warnings {
			t.Logf("   Warning %d: %q", i+1, msg)
		}
	}

	// Check for fatal errors
	if len(errors) == 0 {
		return // No fatal errors
	}

	t.Errorf("Parser has %d fatal errors:", len(errors))
	for i, msg := range errors {
		t.Errorf("   Error %d: %q", i+1, msg)
	}
	t.FailNow() // Stop the test if there are fatal parsing errors
}

// --- Original Test Case (Updated for new AST structure if needed) ---

func TestProcSimple(t *testing.T) {
	// This test now needs the explicit void return type
	input := `
proc sayHi(): void {
  print("hi from proc!")
  // implicit return ok in void
}

print("calling proc...")
sayHi()
print("...proc called")
`

	l := lexer.NewLexer(input)
	p := parser.NewParser(l)
	program := p.ParseProgram()

	checkParserErrors(t, p) // Check for fatal errors

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}
	if len(program.Statements) != 4 {
		t.Fatalf("program.Statements expected=4 statements, got=%d", len(program.Statements))
	}

	// Check Proc 'sayHi'
	stmt1 := program.Statements[0]
	procDecl, ok := stmt1.(*ast.ProcDeclarationStatement)
	if !ok {
		t.Fatalf("program.Statements[0] not *ProcDeclarationStatement, got=%T", stmt1)
	}
	if procDecl.Name.Value != "sayHi" {
		t.Errorf("procDecl.Name expected='sayHi', got=%q", procDecl.Name.Value)
	}

	// Check Parameters (should be empty)
	if len(procDecl.Parameters) != 0 {
		t.Fatalf("sayHi Proc expected 0 parameters, got=%d", len(procDecl.Parameters))
	}

	// Check Return Type (should be void)
	if procDecl.ReturnType == nil {
		t.Fatalf("sayHi procDecl.ReturnType is nil")
	}
	if !procDecl.ReturnType.IsVoid {
		t.Errorf("sayHi procDecl return type should be void")
	}
	if procDecl.ReturnType.Name != "void" {
		t.Errorf("sayHi procDecl return type name expected 'void', got %q", procDecl.ReturnType.Name)
	}
	if procDecl.ReturnType.Width != 0 {
		t.Errorf("sayHi procDecl return type width expected 0, got %d", procDecl.ReturnType.Width)
	}

	// Check Body
	if len(procDecl.Body.Statements) != 1 {
		t.Fatalf("sayHi body expected 1 statement, got %d", len(procDecl.Body.Statements))
	}
	if _, ok := procDecl.Body.Statements[0].(*ast.PrintStatement); !ok {
		t.Errorf("sayHi body stmt 0 not PrintStatement")
	}

	// Check Call (Statement 3)
	stmt3 := program.Statements[2]
	exprStmt, ok := stmt3.(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[2] not *ExpressionStatement")
	}
	callExpr, ok := exprStmt.Expression.(*ast.ProcCallExpression)
	if !ok {
		t.Fatalf("Stmt 3 expression not ProcCallExpression")
	}
	if callExpr.Function.Value != "sayHi" {
		t.Errorf("Call expr function name mismatch")
	}
	if len(callExpr.Arguments) != 0 {
		t.Errorf("Call expr arg count mismatch")
	}
	if callExpr.ResolvedReturnType != "void" {
		t.Errorf("Call expr resolved return type mismatch, expected void got %s", callExpr.ResolvedReturnType)
	}

	// Check Symbol Table Entry
	st := program.SymbolTable
	sayHiInfo, ok := st["sayHi"]
	if !ok {
		t.Fatalf("Symbol 'sayHi' not found in symbol table")
	}
	if sayHiInfo.Type != "proc" {
		t.Errorf("Symbol 'sayHi' type not proc")
	}
	if len(sayHiInfo.ParamNames) != 0 {
		t.Errorf("Symbol 'sayHi' param count mismatch")
	}
	if sayHiInfo.ReturnType != "void" {
		t.Errorf("Symbol 'sayHi' return type mismatch")
	}
	if sayHiInfo.ReturnWidth != 0 {
		t.Errorf("Symbol 'sayHi' return width mismatch")
	}
}

// --- New Test Case for Parameters and Return ---

func TestProcParamsReturn(t *testing.T) {
	input := `
// Procedure returning int
proc add(a: int(6), b: int(6)): int(7) {
  sum := a + b
  return sum // Return variable
}

// Procedure returning string, using default width for return type
proc greet(name: string(20)): string {
   prefix := "Hello, "
   // Return expression (folded constant + variable)
   return prefix + name
}

// Procedure returning void
proc doNothing(val: int(5)): void {
    print("Value:")
	print(val)
	return // Optional return in void
}

// Main logic calling the procedures
res := add(15, 27)
print(res)

msg := greet("World")
print(msg)

doNothing(99)

// Call returning string, ignored result (should warn)
greet("Again")

print("Done.")
`
	l := lexer.NewLexer(input)
	p := parser.NewParser(l)
	program := p.ParseProgram()

	// 1. Check for Parser Errors (warnings are ok, checkParserErrors handles this)
	checkParserErrors(t, p)

	// 2. Check Program Structure (3 procs + 6 main statements)
	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}
	if len(program.Statements) != 9 { // 3 procs + 6 main stmts
		t.Fatalf("program.Statements expected=9 statements, got=%d", len(program.Statements))
	}

	// --- Check Proc 'add' ---
	stmt1 := program.Statements[0]
	addProc, ok := stmt1.(*ast.ProcDeclarationStatement)
	if !ok {
		t.Fatalf("program.Statements[0] not *ProcDeclarationStatement, got=%T", stmt1)
	}
	if addProc.Name.Value != "add" {
		t.Errorf("addProc.Name expected='add', got=%q", addProc.Name.Value)
	}

	// Check Parameters for 'add'
	if len(addProc.Parameters) != 2 {
		t.Fatalf("addProc expected 2 parameters, got=%d", len(addProc.Parameters))
	}
	// Param 0: a: int(6)
	if addProc.Parameters[0].Name.Value != "a" {
		t.Errorf("addProc param 0 name expected 'a', got %q", addProc.Parameters[0].Name.Value)
	}
	if addProc.Parameters[0].TypeNode == nil {
		t.Fatalf("addProc param 0 TypeNode is nil")
	}
	if addProc.Parameters[0].TypeNode.Name != "int" {
		t.Errorf("addProc param 0 type expected 'int', got %q", addProc.Parameters[0].TypeNode.Name)
	}
	if addProc.Parameters[0].TypeNode.Width != 6 {
		t.Errorf("addProc param 0 width expected 6, got %d", addProc.Parameters[0].TypeNode.Width)
	}
	// Param 1: b: int(6)
	if addProc.Parameters[1].Name.Value != "b" {
		t.Errorf("addProc param 1 name expected 'b', got %q", addProc.Parameters[1].Name.Value)
	}
	if addProc.Parameters[1].TypeNode == nil {
		t.Fatalf("addProc param 1 TypeNode is nil")
	}
	if addProc.Parameters[1].TypeNode.Name != "int" {
		t.Errorf("addProc param 1 type expected 'int', got %q", addProc.Parameters[1].TypeNode.Name)
	}
	if addProc.Parameters[1].TypeNode.Width != 6 {
		t.Errorf("addProc param 1 width expected 6, got %d", addProc.Parameters[1].TypeNode.Width)
	}

	// Check Return Type for 'add'
	if addProc.ReturnType == nil {
		t.Fatalf("addProc.ReturnType is nil")
	}
	if addProc.ReturnType.Name != "int" {
		t.Errorf("addProc return type name expected 'int', got %q", addProc.ReturnType.Name)
	}
	if addProc.ReturnType.Width != 7 {
		t.Errorf("addProc return type width expected 7, got %d", addProc.ReturnType.Width)
	}
	if addProc.ReturnType.IsVoid {
		t.Errorf("addProc return type should not be void")
	}

	// Check Body for 'add' (declaration + return)
	if len(addProc.Body.Statements) != 2 {
		t.Fatalf("addProc body expected 2 statements, got %d", len(addProc.Body.Statements))
	}
	if _, ok := addProc.Body.Statements[0].(*ast.DeclarationStatement); !ok {
		t.Errorf("addProc body stmt 0 not DeclarationStatement")
	}
	retStmtAdd, ok := addProc.Body.Statements[1].(*ast.ReturnStatement)
	if !ok {
		t.Errorf("addProc body stmt 1 not ReturnStatement")
	}
	if retStmtAdd.ReturnValue == nil {
		t.Errorf("addProc return statement has nil value")
	}

	// --- Check Proc 'greet' ---
	stmt2 := program.Statements[1]
	greetProc, ok := stmt2.(*ast.ProcDeclarationStatement)
	if !ok {
		t.Fatalf("program.Statements[1] not *ProcDeclarationStatement, got=%T", stmt2)
	}
	if greetProc.Name.Value != "greet" {
		t.Errorf("greetProc.Name expected='greet', got=%q", greetProc.Name.Value)
	}
	// Check Parameters for 'greet'
	if len(greetProc.Parameters) != 1 {
		t.Fatalf("greetProc expected 1 parameter, got=%d", len(greetProc.Parameters))
	}
	if greetProc.Parameters[0].Name.Value != "name" {
		t.Errorf("greetProc param 0 name expected 'name', got %q", greetProc.Parameters[0].Name.Value)
	}
	if greetProc.Parameters[0].TypeNode.Name != "string" {
		t.Errorf("greetProc param 0 type expected 'string', got %q", greetProc.Parameters[0].TypeNode.Name)
	}
	if greetProc.Parameters[0].TypeNode.Width != 20 {
		t.Errorf("greetProc param 0 width expected 20, got %d", greetProc.Parameters[0].TypeNode.Width)
	}
	// Check Return Type for 'greet' (default width)
	if greetProc.ReturnType == nil {
		t.Fatalf("greetProc.ReturnType is nil")
	}
	if greetProc.ReturnType.Name != "string" {
		t.Errorf("greetProc return type name expected 'string', got %q", greetProc.ReturnType.Name)
	}
	if greetProc.ReturnType.Width != lib.DefaultStringWidth {
		t.Errorf("greetProc return type width expected default %d, got %d", lib.DefaultStringWidth, greetProc.ReturnType.Width)
	}
	if greetProc.ReturnType.IsVoid {
		t.Errorf("greetProc return type should not be void")
	}
	// Check Body for 'greet' (declaration + return statement)
	if len(greetProc.Body.Statements) != 2 {
		t.Fatalf("greetProc body expected 2 statements, got %d", len(greetProc.Body.Statements))
	}
	if _, ok := greetProc.Body.Statements[0].(*ast.DeclarationStatement); !ok {
		t.Errorf("greetProc body stmt 0 not DeclarationStatement")
	}
	retStmtGreet, ok := greetProc.Body.Statements[1].(*ast.ReturnStatement)
	if !ok {
		t.Errorf("greetProc body stmt 1 not ReturnStatement")
	}
	if retStmtGreet.ReturnValue == nil {
		t.Errorf("greetProc return statement has nil value")
	}

	// --- Check Proc 'doNothing' ---
	stmt3 := program.Statements[2]
	voidProc, ok := stmt3.(*ast.ProcDeclarationStatement)
	if !ok {
		t.Fatalf("program.Statements[2] not *ProcDeclarationStatement, got=%T", stmt3)
	}
	if voidProc.Name.Value != "doNothing" {
		t.Errorf("voidProc.Name expected='doNothing', got=%q", voidProc.Name.Value)
	}
	// Check Parameters for 'doNothing'
	if len(voidProc.Parameters) != 1 {
		t.Fatalf("voidProc expected 1 parameter, got=%d", len(voidProc.Parameters))
	}
	if voidProc.Parameters[0].TypeNode.Name != "int" {
		t.Errorf("voidProc param 0 type expected 'int', got %q", voidProc.Parameters[0].TypeNode.Name)
	}
	if voidProc.Parameters[0].TypeNode.Width != 5 {
		t.Errorf("voidProc param 0 width expected 5, got %d", voidProc.Parameters[0].TypeNode.Width)
	}
	// Check Return Type for 'doNothing'
	if voidProc.ReturnType == nil {
		t.Fatalf("voidProc.ReturnType is nil")
	}
	if !voidProc.ReturnType.IsVoid {
		t.Errorf("voidProc return type should be void")
	}
	if voidProc.ReturnType.Name != "void" {
		t.Errorf("voidProc return type name expected 'void', got %q", voidProc.ReturnType.Name)
	}
	// Check Body for 'doNothing' (print, print, return)
	if len(voidProc.Body.Statements) != 3 {
		t.Fatalf("voidProc body expected 3 statements, got %d", len(voidProc.Body.Statements))
	}
	if _, ok := voidProc.Body.Statements[0].(*ast.PrintStatement); !ok {
		t.Errorf("voidProc body stmt 0 not PrintStatement")
	}
	if _, ok := voidProc.Body.Statements[1].(*ast.PrintStatement); !ok {
		t.Errorf("voidProc body stmt 1 not PrintStatement")
	}
	retStmtVoid, ok := voidProc.Body.Statements[2].(*ast.ReturnStatement)
	if !ok {
		t.Errorf("voidProc body stmt 2 not ReturnStatement")
	}
	if retStmtVoid.ReturnValue != nil {
		t.Errorf("voidProc return statement should have nil value")
	}

	// --- Check Main Logic Statements ---
	// Stmt 4: Declaration `res := add(...)`
	declStmt1, ok := program.Statements[3].(*ast.DeclarationStatement)
	if !ok {
		t.Fatalf("program.Statements[3] not DeclarationStatement")
	}
	if declStmt1.Name.Value != "res" {
		t.Errorf("Decl stmt 1 name mismatch")
	}
	callExpr1, ok := declStmt1.Value.(*ast.ProcCallExpression)
	if !ok {
		t.Fatalf("Decl stmt 1 value not ProcCallExpression")
	}
	if callExpr1.Function.Value != "add" {
		t.Errorf("Call expr 1 function name mismatch")
	}
	if len(callExpr1.Arguments) != 2 {
		t.Errorf("Call expr 1 arg count mismatch")
	}
	if callExpr1.ResolvedReturnType != "int" {
		t.Errorf("Call expr 1 resolved return type mismatch")
	}
	if callExpr1.ResolvedReturnWidth != 7 {
		t.Errorf("Call expr 1 resolved return width mismatch")
	}

	// Stmt 5: Print `print(res)`
	if _, ok := program.Statements[4].(*ast.PrintStatement); !ok {
		t.Fatalf("Stmt 5 not print")
	}

	// Stmt 6: Declaration `msg := greet(...)`
	declStmt2, ok := program.Statements[5].(*ast.DeclarationStatement)
	if !ok {
		t.Fatalf("program.Statements[5] not DeclarationStatement")
	}
	if declStmt2.Name.Value != "msg" {
		t.Errorf("Decl stmt 2 name mismatch")
	}
	callExpr2, ok := declStmt2.Value.(*ast.ProcCallExpression)
	if !ok {
		t.Fatalf("Decl stmt 2 value not ProcCallExpression")
	}
	if callExpr2.Function.Value != "greet" {
		t.Errorf("Call expr 2 function name mismatch")
	}
	if callExpr2.ResolvedReturnType != "string" {
		t.Errorf("Call expr 2 resolved return type mismatch")
	}
	if callExpr2.ResolvedReturnWidth != lib.DefaultStringWidth {
		t.Errorf("Call expr 2 resolved return width mismatch")
	}

	// Stmt 7: Print `print(msg)`
	if _, ok := program.Statements[6].(*ast.PrintStatement); !ok {
		t.Fatalf("Stmt 7 not print")
	}

	// Stmt 8: Call `doNothing(...)` (ExpressionStatement)
	exprStmt1, ok := program.Statements[7].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[7] not ExpressionStatement")
	}
	callExpr3, ok := exprStmt1.Expression.(*ast.ProcCallExpression)
	if !ok {
		t.Fatalf("Stmt 8 expression not ProcCallExpression")
	}
	if callExpr3.Function.Value != "doNothing" {
		t.Errorf("Call expr 3 function name mismatch")
	}
	if len(callExpr3.Arguments) != 1 {
		t.Errorf("Call expr 3 arg count mismatch")
	}
	if callExpr3.ResolvedReturnType != "void" {
		t.Errorf("Call expr 3 resolved return type mismatch")
	}

	// Stmt 9: Call `greet(...)` ignored result (ExpressionStatement)
	exprStmt2, ok := program.Statements[8].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[8] not ExpressionStatement")
	}
	callExpr4, ok := exprStmt2.Expression.(*ast.ProcCallExpression)
	if !ok {
		t.Fatalf("Stmt 9 expression not ProcCallExpression")
	}
	if callExpr4.Function.Value != "greet" {
		t.Errorf("Call expr 4 function name mismatch")
	}
	if callExpr4.ResolvedReturnType != "string" {
		t.Errorf("Call expr 4 resolved return type mismatch")
	}

	// --- Check Symbol Table ---
	st := program.SymbolTable
	// Check 'add' proc info
	addInfo, ok := st["add"]
	if !ok {
		t.Fatalf("Symbol 'add' not found in symbol table")
	}
	if addInfo.Type != "proc" {
		t.Errorf("Symbol 'add' type not proc")
	}
	if len(addInfo.ParamNames) != 2 {
		t.Errorf("Symbol 'add' param count mismatch")
	}
	if addInfo.ParamNames[0] != "a" {
		t.Errorf("Symbol 'add' param 0 name mismatch")
	}
	if addInfo.ParamTypes[0] != "int" {
		t.Errorf("Symbol 'add' param 0 type mismatch")
	}
	if addInfo.ParamWidths[0] != 6 {
		t.Errorf("Symbol 'add' param 0 width mismatch")
	}
	if addInfo.ReturnType != "int" {
		t.Errorf("Symbol 'add' return type mismatch")
	}
	if addInfo.ReturnWidth != 7 {
		t.Errorf("Symbol 'add' return width mismatch")
	}

	// Check 'greet' proc info
	greetInfo, ok := st["greet"]
	if !ok {
		t.Fatalf("Symbol 'greet' not found")
	}
	if greetInfo.Type != "proc" {
		t.Errorf("Symbol 'greet' type not proc")
	}
	if len(greetInfo.ParamNames) != 1 {
		t.Errorf("Symbol 'greet' param count mismatch")
	}
	if greetInfo.ReturnType != "string" {
		t.Errorf("Symbol 'greet' return type mismatch")
	}
	if greetInfo.ReturnWidth != lib.DefaultStringWidth {
		t.Errorf("Symbol 'greet' return width mismatch (expected default %d, got %d)", lib.DefaultStringWidth, greetInfo.ReturnWidth)
	}

	// Check 'doNothing' proc info
	dnInfo, ok := st["doNothing"]
	if !ok {
		t.Fatalf("Symbol 'doNothing' not found")
	}
	if dnInfo.Type != "proc" {
		t.Errorf("Symbol 'doNothing' type not proc")
	}
	if dnInfo.ReturnType != "void" {
		t.Errorf("Symbol 'doNothing' return type mismatch")
	}
	if dnInfo.ReturnWidth != 0 {
		t.Errorf("Symbol 'doNothing' return width mismatch")
	}

	// Check global variables 'res' and 'msg'
	resInfo, ok := st["res"]
	if !ok {
		t.Fatalf("Symbol 'res' not found")
	}
	if resInfo.Type != "int" {
		t.Errorf("Symbol 'res' type mismatch")
	}
	// Width of 'res' is inferred from 'add' return width (7)
	if resInfo.Width != 7 {
		t.Errorf("Symbol 'res' width mismatch, expected 7 got %d", resInfo.Width)
	}

	msgInfo, ok := st["msg"]
	if !ok {
		t.Fatalf("Symbol 'msg' not found")
	}
	if msgInfo.Type != "string" {
		t.Errorf("Symbol 'msg' type mismatch")
	}
	// Width of 'msg' inferred from 'greet' return width (default 30)
	if msgInfo.Width != lib.DefaultStringWidth {
		t.Errorf("Symbol 'msg' width mismatch, expected %d got %d", lib.DefaultStringWidth, msgInfo.Width)
	}
}

// TODO: Add more specific failure tests:
// - TestProcMissingReturnType (bad) -> Syntax Error: Expected ':' after parameter list...
// - TestProcMissingParamWidth (bad) -> Syntax Error: Explicit width specification...required for parameter...
// - TestProcVoidReturnWithValue (bad) -> Semantic Error: Cannot return a value from procedure...declared as void
// - TestProcReturnMismatchType (bad) -> Semantic Error: Type mismatch - cannot return value of type...
// - TestProcArgCountMismatch (bad) -> Semantic Error: Procedure...expects N arguments, but got M
// - TestProcArgTypeMismatch (bad) -> Semantic Error: Type mismatch for argument N...Expected X, got Y
// - TestProcReturnWidthWarning (good, check logs) -> Semantic Warning: Width of returned value (X) may exceed declared return width (Y)...
// - TestProcArgWidthWarning (good, check logs) -> Semantic Warning: Width of argument N (X) might exceed parameter width (Y)...
// - TestProcArgLiteralWidthError (bad) -> Semantic Error: Argument N width X exceeds parameter width Y...
