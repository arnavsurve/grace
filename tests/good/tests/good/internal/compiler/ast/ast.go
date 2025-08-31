package ast

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/arnavsurve/grace/internal/compiler/lib"
	"github.com/arnavsurve/grace/internal/compiler/scope"
	"github.com/arnavsurve/grace/internal/compiler/symbols"
	"github.com/arnavsurve/grace/internal/compiler/token"
)

// --- Interfaces ---
type Node interface {
	TokenLiteral() string
	String() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
	ResultType() string
	ResultWidth() int
	GetToken() token.Token
	GetResolvedSymbol() *symbols.SymbolInfo // Returns symbol info if expr is Ident, or record/file info if expr is read()/input() etc.
}

// --- Program ---
type Program struct {
	Statements  []Statement
	GlobalScope *scope.Scope
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	}
	return ""
}

// String for Program concatenates the string representations of its statements
func (p *Program) String() string {
	var out bytes.Buffer
	for _, s := range p.Statements {
		out.WriteString(s.String())
		// Add a newline between top-level statements for readability
		out.WriteString("\n")
	}
	return out.String()
}

// --- Statements ---

// PrintStatement -> print("hello") or print(var)
type PrintStatement struct {
	Token token.Token // print
	Value Expression
}

func (ps *PrintStatement) statementNode()       {}
func (ps *PrintStatement) TokenLiteral() string { return ps.Token.Literal }
func (ps *PrintStatement) String() string {
	var out bytes.Buffer
	out.WriteString(ps.TokenLiteral() + "(") // "print("
	if ps.Value != nil {
		out.WriteString(ps.Value.String())
	}
	out.WriteString(")") // ")"
	return out.String()
}

// TypeNode represents a type specification like int(10), string, void, MyRecordType
type TypeNode struct {
	Token      token.Token // The type token (e.g., 'int', 'string', 'void')
	Name       string      // "int", "string", "void"
	WidthToken token.Token // Optional width token (e.g., '10')
	Width      int         // Parsed width (explicit or default), 0 for void
	IsVoid     bool
	IsRecord   bool                // If this type name resolves to a record
	RecordInfo *symbols.SymbolInfo // Pointer to the record's symbol info if IsRecord is true
}

func (tn *TypeNode) TokenLiteral() string { return tn.Token.Literal }
func (tn *TypeNode) String() string {
	if tn.IsVoid {
		return "void"
	}
	s := tn.Name
	// Only show width only for base types where it was explicitly provided
	if !tn.IsRecord && tn.WidthToken.Type == token.TokenInt {
		s += fmt.Sprintf("(%s)", tn.WidthToken.Literal)
	}
	return s
}

// DeclarationStatement -> x := 123 or const y : string = "hi"
type DeclarationStatement struct {
	Token              token.Token // := or = (depending on explicit type)
	IsConst            bool
	Name               *Identifier
	HasExplicitType    bool
	ExplicitType       *TypeNode
	Value              Expression
	ResolvedRecordType *symbols.SymbolInfo // If declared with an explicit record type
}

func (ds *DeclarationStatement) statementNode()       {}
func (ds *DeclarationStatement) TokenLiteral() string { return ds.Token.Literal } // Literal is ":=" or "="
func (ds *DeclarationStatement) String() string {
	var out bytes.Buffer
	if ds.IsConst {
		out.WriteString("const ")
	}
	out.WriteString(ds.Name.String())

	if ds.HasExplicitType {
		out.WriteString(": ")
		out.WriteString(ds.ExplicitType.String()) // Use TypeNode's string representation
		if ds.Value != nil {                      // Check if there's an assignment part
			out.WriteString(" ")
			out.WriteString(ds.TokenLiteral()) // Should be '=' if HasExplicitType and Value != nil
			out.WriteString(" ")
			out.WriteString(ds.Value.String())
		}
	} else { // Inferred type: name := value
		out.WriteString(" ")
		out.WriteString(ds.TokenLiteral()) // Should be ':='
		out.WriteString(" ")
		if ds.Value != nil { // Should always be non-nil for :=
			out.WriteString(ds.Value.String())
		}
	}
	return out.String()
}

// ReassignmentStatement -> x = 456 or person.name = "Alice"
type ReassignmentStatement struct {
	Token  token.Token // =
	Target Expression  // Can be *Identifier or *FieldAccessExpression
	Value  Expression
}

func (rs *ReassignmentStatement) statementNode()       {}
func (rs *ReassignmentStatement) TokenLiteral() string { return rs.Token.Literal } // Literal is "="
func (rs *ReassignmentStatement) String() string {
	var out bytes.Buffer
	if rs.Target != nil {
		out.WriteString(rs.Target.String())
	}
	out.WriteString(" " + rs.TokenLiteral() + " ") // " = "
	if rs.Value != nil {
		out.WriteString(rs.Value.String())
	}
	return out.String()
}

// BlockStatement -> { statement1 \n statement2 }
type BlockStatement struct {
	Token      token.Token // {
	Statements []Statement
}

func (bs *BlockStatement) statementNode()       {}
func (bs *BlockStatement) TokenLiteral() string { return bs.Token.Literal }
func (bs *BlockStatement) String() string {
	var out bytes.Buffer
	out.WriteString("{\n") // Opening brace and newline
	for _, s := range bs.Statements {
		out.WriteString("\t" + s.String() + "\n") // Add tab and newline
	}
	out.WriteString("}")
	return out.String()
}

// ReturnStatement -> return expression or return
type ReturnStatement struct {
	Token       token.Token
	ReturnValue Expression // nil if returning from a void procedure
}

func (rs *ReturnStatement) statementNode()       {}
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }
func (rs *ReturnStatement) String() string {
	var out bytes.Buffer
	out.WriteString(rs.TokenLiteral()) // "return"
	if rs.ReturnValue != nil {
		out.WriteString(" ")
		out.WriteString(rs.ReturnValue.String())
	}
	return out.String()
}

type Parameter struct {
	Name     *Identifier
	TypeNode *TypeNode // Includes resolved record info if applicable
}

func (p *Parameter) String() string {
	return p.Name.String() + ": " + p.TypeNode.String()
}

// ProcDeclarationStatement -> proc name() { body }
type ProcDeclarationStatement struct {
	Token      token.Token // The 'proc' token
	Name       *Identifier
	Parameters []*Parameter
	ReturnType *TypeNode // Includes resolved record info if applicable
	Body       *BlockStatement
	LocalScope *scope.Scope
}

func (pds *ProcDeclarationStatement) statementNode()       {}
func (pds *ProcDeclarationStatement) TokenLiteral() string { return pds.Token.Literal }
func (pds *ProcDeclarationStatement) String() string {
	var out bytes.Buffer
	out.WriteString(pds.TokenLiteral() + " ") // "proc "
	if pds.Name != nil {
		out.WriteString(pds.Name.String())
	}
	out.WriteString("(")
	params := []string{}
	for _, p := range pds.Parameters {
		params = append(params, p.String()) // Use Parameter's String()
	}
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(")")

	out.WriteString(": ") // Return type is mandatory
	if pds.ReturnType != nil {
		out.WriteString(pds.ReturnType.String()) // Use TypeNode's String()
	} else {
		// Should not happen if parser enforces mandatory return type
		out.WriteString("?")
	}

	out.WriteString(" ")
	if pds.Body != nil {
		out.WriteString(pds.Body.String()) // Uses BlockStatement's String()
	}
	return out.String()
}

// ExpressionStatement wraps an expression to be used as a statement (e.g., a function call)
type ExpressionStatement struct {
	Token      token.Token // the first token of the expression
	Expression Expression
}

func (es *ExpressionStatement) statementNode()       {}
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Literal }
func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

// --- Expressions ---

// RecordField represents 'name: type(width)' inside a record definition
type RecordField struct {
	Name     *Identifier
	TypeNode *TypeNode // Must resolve to int or string with explicit width
}

func (rf *RecordField) String() string {
	return rf.Name.String() + ": " + rf.TypeNode.String()
}

// RecordDeclarationStatement -> record Name { field ... }
type RecordDeclarationStatement struct {
	Token  token.Token // 'record' token
	Name   *Identifier // Name of the record (e.g. Customer, Employee)
	Fields []*RecordField
	Symbol *symbols.SymbolInfo // Resolved symbol into for this record type (set by parser)
}

func (rds *RecordDeclarationStatement) statementNode()       {}
func (rds *RecordDeclarationStatement) TokenLiteral() string { return rds.Token.Literal }
func (rds *RecordDeclarationStatement) String() string {
	var out bytes.Buffer
	out.WriteString("record " + rds.Name.String() + " {\n")
	for _, f := range rds.Fields {
		out.WriteString("\t" + f.String() + "\n")
	}
	out.WriteString("}")
	return out.String()
}

// FileDeclarationStatement -> handle := input("sys", Rec) or handle: file = input("sys", Rec)
// NOTE: The parser creates this from either syntax (`:=` or `: file =`)
type FileDeclarationStatement struct {
	Token             token.Token         // ':=' or '=' token
	Name              *Identifier         // Logical file handle name
	IsExplicitlyTyped bool                // True is ': file =' was used
	Value             Expression          // input(...) or output(...) expression node
	Symbol            *symbols.SymbolInfo // Resolved symbol info for this file handle (set by parser)
}

func (fds *FileDeclarationStatement) statementNode()       {}
func (fds *FileDeclarationStatement) TokenLiteral() string { return fds.Token.Literal }
func (fds *FileDeclarationStatement) String() string {
	// Reconstruct a possible source representation
	var out bytes.Buffer
	out.WriteString(fds.Name.String())
	if fds.IsExplicitlyTyped {
		out.WriteString(": file") // Assuming 'file' is the type keyword
	}
	out.WriteString(" " + fds.TokenLiteral() + " ") // := or =
	out.WriteString(fds.Value.String())
	return out.String()
}

// ReadStatement -> read fileHandle into recordVar
type ReadStatement struct {
	Token            token.Token // 'read' token
	FileHandle       *Identifier
	RecordVar        *Identifier // Variable to read data into
	FileHandleSymbol *symbols.SymbolInfo
	RecordVarSymbol  *symbols.SymbolInfo
}

func (rs *ReadStatement) statementNode()       {}
func (rs *ReadStatement) TokenLiteral() string { return rs.Token.Literal }
func (rs *ReadStatement) String() string {
	return fmt.Sprintf("read %s into %s", rs.FileHandle.String(), rs.RecordVar.String())
}

// WriteStatement -> write fileHandle from recordVar
type WriteStatement struct {
	Token            token.Token // 'write' token
	FileHandle       *Identifier
	RecordVar        *Identifier
	FileHandleSymbol *symbols.SymbolInfo
	RecordVarSymbol  *symbols.SymbolInfo
}

func (ws *WriteStatement) statementNode()       {}
func (ws *WriteStatement) TokenLiteral() string { return ws.Token.Literal }
func (ws *WriteStatement) String() string {
	return fmt.Sprintf("write %s from %s", ws.FileHandle.String(), ws.RecordVar.String())
}

// --- Expressions ---

// Identifier -> varName or TypeName or FunctionName
type Identifier struct {
	Token token.Token // IDENT
	Value string
	// Resolved symbol info (variable, const, proc, record type)
	Symbol *symbols.SymbolInfo
}

func (i *Identifier) expressionNode()                        {}
func (i *Identifier) TokenLiteral() string                   { return i.Token.Literal }
func (i *Identifier) String() string                         { return i.Value }
func (i *Identifier) GetToken() token.Token                  { return i.Token }
func (i *Identifier) GetResolvedSymbol() *symbols.SymbolInfo { return i.Symbol }

func (i *Identifier) ResultType() string {
	if i.Symbol != nil {
		if i.Symbol.RecordTypeSymbol != nil { // Variable holding a record
			return i.Symbol.RecordTypeSymbol.Name // e.g. CustomerRecord
		}
		return i.Symbol.Type // int, string, proc, record, file
	}
	return "unknown" // Shouldn't happen if semantic analysis runs correctly
}

func (i *Identifier) ResultWidth() int {
	if i.Symbol != nil {
		// If it's a record variable, width is the record's total width
		if i.Symbol.RecordTypeSymbol != nil {
			return i.Symbol.RecordTypeSymbol.TotalWidth
		}
		return i.Symbol.Width // Width for int/string, TotalWidth for record type itself, 0 for proc/file
	}
	return 0
}

// StringLiteral -> "hello"
type StringLiteral struct {
	Token token.Token
	Value string
	Width int
}

func (sl *StringLiteral) expressionNode()      {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }
func (sl *StringLiteral) ResultType() string   { return "string" }
func (sl *StringLiteral) ResultWidth() int     { return sl.Width }
func (sl *StringLiteral) String() string {
	return fmt.Sprintf("%q", sl.Value)
}
func (sl *StringLiteral) GetToken() token.Token                  { return sl.Token }
func (sl *StringLiteral) GetResolvedSymbol() *symbols.SymbolInfo { return nil }

// IntegerLiteral -> 123
type IntegerLiteral struct {
	Token token.Token
	Value int
	Width int
}

func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) ResultType() string   { return "int" }
func (il *IntegerLiteral) ResultWidth() int     { return il.Width }
func (il *IntegerLiteral) String() string {
	return il.Token.Literal // Return the original literal string
}
func (il *IntegerLiteral) GetToken() token.Token                  { return il.Token }
func (il *IntegerLiteral) GetResolvedSymbol() *symbols.SymbolInfo { return nil }

// BooleanLiteral -> (true/false)
type BooleanLiteral struct {
	Token token.Token
	Value bool
	Width int
}

func (bl *BooleanLiteral) expressionNode()                        {}
func (bl *BooleanLiteral) TokenLiteral() string                   { return bl.Token.Literal }
func (bl *BooleanLiteral) String() string                         { return bl.Token.Literal }
func (bl *BooleanLiteral) ResultType() string                     { return "bool" }
func (bl *BooleanLiteral) ResultWidth() int                       { return 1 }
func (bl *BooleanLiteral) GetToken() token.Token                  { return bl.Token }
func (bl *BooleanLiteral) GetResolvedSymbol() *symbols.SymbolInfo { return nil }

// BinaryExpression -> (left + right)
type BinaryExpression struct {
	Token    token.Token // +, -, *, /
	Left     Expression
	Operator string // +, -, *, /
	Right    Expression

	// Cached results to avoid recalculation
	cachedResultType  string
	cachedResultWidth int
	resultsCalculated bool
}

func (be *BinaryExpression) expressionNode()      {}
func (be *BinaryExpression) TokenLiteral() string { return be.Token.Literal }
func (be *BinaryExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(be.Left.String())
	out.WriteString(" " + be.Operator + " ")
	out.WriteString(be.Right.String())
	out.WriteString(")")
	return out.String()
}
func (be *BinaryExpression) GetToken() token.Token                  { return be.Token }
func (be *BinaryExpression) GetResolvedSymbol() *symbols.SymbolInfo { return nil }

func (be *BinaryExpression) calculateResults() {
	if be.resultsCalculated {
		return
	}

	leftType := be.Left.ResultType()
	rightType := be.Right.ResultType()
	be.cachedResultType = "unknown" // Default
	be.cachedResultWidth = 0        // Default

	// String Concatenation (+)
	if leftType == "string" && rightType == "string" && be.Operator == "+" {
		be.cachedResultType = "string"
		leftLit, leftIsLit := be.Left.(*StringLiteral)
		rightLit, rightIsLit := be.Right.(*StringLiteral)
		if leftIsLit && rightIsLit { // Constant folding width
			be.cachedResultWidth = len(leftLit.Value) + len(rightLit.Value)
		} else { // Heuristic width
			leftWidth := be.Left.ResultWidth()
			rightWidth := be.Right.ResultWidth()
			if leftWidth <= 0 {
				leftWidth = lib.DefaultStringWidth
			}
			if rightWidth <= 0 {
				rightWidth = lib.DefaultStringWidth
			}
			be.cachedResultWidth = leftWidth + rightWidth
		}
	}

	// Integer Arithmetic (+, -, *, /)
	if leftType == "int" && rightType == "int" {
		// Check if operator is valid for int
		isValidIntOp := false
		switch be.Operator {
		case "+", "-", "*", "/":
			isValidIntOp = true
		}

		if isValidIntOp {
			be.cachedResultType = "int"
			leftLit, leftIsLit := be.Left.(*IntegerLiteral)
			rightLit, rightIsLit := be.Right.(*IntegerLiteral)
			if leftIsLit && rightIsLit { // Constant folding width
				leftVal := leftLit.Value
				rightVal := rightLit.Value
				var resultVal int
				switch be.Operator {
				case "+":
					resultVal = leftVal + rightVal
				case "-":
					resultVal = leftVal - rightVal
				case "*":
					resultVal = leftVal * rightVal
				case "/":
					if rightVal == 0 {
						// Parser should add error. Width remains 0 or default.
						// Use default width to avoid subsequent errors assuming 0 width.
						be.cachedResultWidth = lib.DefaultIntWidth
					} else {
						resultVal = leftVal / rightVal
					}
				}
				// Calculate width only if division was valid or op wasn't division
				if !(be.Operator == "/" && rightVal == 0) {
					be.cachedResultWidth = lib.CalculateWidthForValue(resultVal)
				}

			} else { // Heuristic width
				leftWidth := be.Left.ResultWidth()
				rightWidth := be.Right.ResultWidth()
				if leftWidth <= 0 {
					leftWidth = lib.DefaultIntWidth
				}
				if rightWidth <= 0 {
					rightWidth = lib.DefaultIntWidth
				}

				switch be.Operator {
				case "+", "-":
					maxWidth := max(leftWidth, rightWidth)
					be.cachedResultWidth = maxWidth + 1
				case "*":
					// Result width can be sum of operand widths
					be.cachedResultWidth = leftWidth + rightWidth
				case "/":
					// Result width is typically <= dividend width
					be.cachedResultWidth = leftWidth
				}
			}
		}
	}

	// Ensure width is at least default if type is known but width calculation failed/resulted in 0
	if be.cachedResultType == "int" && be.cachedResultWidth <= 0 {
		be.cachedResultWidth = lib.DefaultIntWidth
	}
	if be.cachedResultType == "string" && be.cachedResultWidth <= 0 {
		be.cachedResultWidth = lib.DefaultStringWidth
	}

	be.resultsCalculated = true
}

func (be *BinaryExpression) ResultWidth() int {
	be.calculateResults()
	return be.cachedResultWidth
}

func (be *BinaryExpression) ResultType() string {
	be.calculateResults()
	return be.cachedResultType
}

// GroupedExpression -> (expression)
type GroupedExpression struct {
	Token      token.Token // '('
	Expression Expression
}

func (ge *GroupedExpression) expressionNode()      {}
func (ge *GroupedExpression) TokenLiteral() string { return ge.Token.Literal }
func (ge *GroupedExpression) String() string {
	return "(" + ge.Expression.String() + ")" // Wrap inner expression in parens
}

func (ge *GroupedExpression) ResultType() string {
	return ge.Expression.ResultType()
}

func (ge *GroupedExpression) ResultWidth() int {
	return ge.Expression.ResultWidth()
}
func (ge *GroupedExpression) GetToken() token.Token { return ge.Token }
func (ge *GroupedExpression) GetResolvedSymbol() *symbols.SymbolInfo {
	return ge.Expression.GetResolvedSymbol()
}

// ProcCallExpression represents 'funcName(arg1, arg2)'
type ProcCallExpression struct {
	Token     token.Token  // The function name token
	Function  *Identifier  // The identifier for the function
	Arguments []Expression // List of argument expressions

	// These are set by parser using Function.symbol
	ResolvedReturnType  string
	ResolvedReturnWidth int
	ResolvedRecordType  *symbols.SymbolInfo
}

func (pce *ProcCallExpression) expressionNode()      {}
func (pce *ProcCallExpression) TokenLiteral() string { return pce.Token.Literal }
func (pce *ProcCallExpression) ResultType() string   { return pce.ResolvedReturnType }
func (pce *ProcCallExpression) ResultWidth() int     { return pce.ResolvedReturnWidth }
func (pce *ProcCallExpression) String() string {
	var out bytes.Buffer
	args := []string{}
	if pce.Arguments != nil {
		for _, a := range pce.Arguments {
			args = append(args, a.String())
		}
	}
	out.WriteString(pce.Function.String())
	out.WriteString("(")
	out.WriteString(strings.Join(args, ", "))
	out.WriteString(")")
	return out.String()
}
func (pce *ProcCallExpression) GetToken() token.Token                  { return pce.Token }
func (pce *ProcCallExpression) GetResolvedSymbol() *symbols.SymbolInfo { return pce.ResolvedRecordType }

// InputExpression represents `input("filename", RecordType)`
type InputExpression struct {
	Token              token.Token // 'input' token
	SystemFileName     *StringLiteral
	RecordTypeName     *Identifier
	ResolvedFileInfo   *symbols.SymbolInfo
	ResolvedRecordInfo *symbols.SymbolInfo
}

func (ie *InputExpression) expressionNode()      {}
func (ie *InputExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *InputExpression) String() string {
	return fmt.Sprintf("input(%s, %s)", ie.SystemFileName.String(), ie.RecordTypeName.String())
}
func (ie *InputExpression) ResultType() string                     { return "file" } // The expression returns a file handle
func (ie *InputExpression) ResultWidth() int                       { return 0 }      // Files don't have a width
func (ie *InputExpression) GetToken() token.Token                  { return ie.Token }
func (ie *InputExpression) GetResolvedSymbol() *symbols.SymbolInfo { return ie.ResolvedFileInfo }

// OutputExpression represents `output("filename", RecordType)`
type OutputExpression struct {
	Token              token.Token // 'output' token
	SystemFileName     *StringLiteral
	RecordTypeName     *Identifier
	ResolvedFileInfo   *symbols.SymbolInfo
	ResolvedRecordInfo *symbols.SymbolInfo
}

func (oe *OutputExpression) expressionNode()      {}
func (oe *OutputExpression) TokenLiteral() string { return oe.Token.Literal }
func (oe *OutputExpression) String() string {
	return fmt.Sprintf("output(%s, %s)", oe.SystemFileName.String(), oe.RecordTypeName.String())
}
func (oe *OutputExpression) ResultType() string                     { return "file" } // The expression returns a file handle
func (oe *OutputExpression) ResultWidth() int                       { return 0 }      // Files don't have a width
func (oe *OutputExpression) GetToken() token.Token                  { return oe.Token }
func (oe *OutputExpression) GetResolvedSymbol() *symbols.SymbolInfo { return oe.ResolvedFileInfo }

// FieldAccessExpression represents a record field access like `record.field`
type FieldAccessExpression struct {
	Token         token.Token        // The '.' token
	Record        Expression         // Expression evaluating to the record variable (e.g., *ast.Identifier)
	Field         *Identifier        // The field being accessed
	ResolvedField *symbols.FieldInfo // Resolved field info (set by parser)
}

func (fe *FieldAccessExpression) expressionNode()      {}
func (fe *FieldAccessExpression) TokenLiteral() string { return fe.Token.Literal }
func (fe *FieldAccessExpression) String() string {
	return fmt.Sprintf("(%s.%s)", fe.Record.String(), fe.Field.String())
}
func (fe *FieldAccessExpression) GetToken() token.Token { return fe.Token }
func (fe *FieldAccessExpression) ResultType() string {
	if fe.ResolvedField != nil {
		return fe.ResolvedField.Type
	}
	return "unknown"
}

func (fe *FieldAccessExpression) ResultWidth() int {
	if fe.ResolvedField != nil {
		return fe.ResolvedField.Width
	}
	return 0
}

func (fe *FieldAccessExpression) GetResolvedSymbol() *symbols.SymbolInfo {
	// Accessing a field doesn't yield a top-level symbol directly
	// We *could* return the symbol of the field's type if needed, but nil seems appropriate
	return nil
}

// Helper for pretty printing AST to bless it with my chud eyes
func PrintAST(node Node, indent string) {
	if node == nil {
		fmt.Println(indent + "<nil>")
		return
	}

	switch n := node.(type) {
	case *Program:
		fmt.Println(indent + "Program")
		if n.GlobalScope != nil {
			// Optionally print global scope symbols here for debugging
		}
		for _, stmt := range n.Statements {
			PrintAST(stmt, indent+"  ")
		}

	case *DeclarationStatement:
		typeName := "inferred"
		widthStr := ""
		if n.HasExplicitType {
			typeName = n.ExplicitType.Name
			if !n.ExplicitType.IsRecord && n.ExplicitType.Width > 0 {
				widthStr = fmt.Sprintf("(%d)", n.ExplicitType.Width)
			}
		}
		assignOp := ":="
		if n.Value == nil { // Declaration without assignment (e.g., record var)
			assignOp = ""
		} else if n.HasExplicitType {
			assignOp = "="
		}
		fmt.Printf("%sDeclarationStatement Name: %s, Const: %t, Type: %s%s, Op: %s\n", indent, n.Name.String(), n.IsConst, typeName, widthStr, assignOp)
		if n.Value != nil {
			fmt.Println(indent + "  Value:")
			PrintAST(n.Value, indent+"    ")
		}

	case *ReassignmentStatement:
		fmt.Println(indent + "ReassignmentStatement")
		fmt.Println(indent + "  Target:")
		PrintAST(n.Target, indent+"    ")
		fmt.Println(indent + "  Value:")
		PrintAST(n.Value, indent+"    ")

	case *PrintStatement:
		fmt.Println(indent + "PrintStatement")
		PrintAST(n.Value, indent+"  ")

	case *ReturnStatement:
		fmt.Println(indent + "ReturnStatement")
		if n.ReturnValue != nil {
			PrintAST(n.ReturnValue, indent+"  ")
		} else {
			fmt.Println(indent + "  <void>")
		}

	case *ExpressionStatement:
		fmt.Println(indent + "ExpressionStatement")
		PrintAST(n.Expression, indent+"  ")

	case *ProcDeclarationStatement:
		fmt.Println(indent + "ProcDeclarationStatement")
		fmt.Println(indent+"  Name:", n.Name.String())
		fmt.Println(indent + "  Params:")
		for _, param := range n.Parameters {
			fmt.Println(indent+"    "+param.Name.String()+":", param.TypeNode.String())
		}
		fmt.Println(indent+"  ReturnType:", n.ReturnType.String())
		PrintAST(n.Body, indent+"  ")

	case *BlockStatement:
		fmt.Println(indent + "BlockStatement")
		for _, stmt := range n.Statements {
			PrintAST(stmt, indent+"  ")
		}

	case *RecordDeclarationStatement:
		fmt.Println(indent+"RecordDeclarationStatement Name:", n.Name.String())
		for i, field := range n.Fields {
			fmt.Printf("%s  Field[%d]: %s: %s\n", indent, i, field.Name.String(), field.TypeNode.String())
		}
	case *FileDeclarationStatement:
		typeDecl := "inferred"
		if n.IsExplicitlyTyped {
			typeDecl = ": file"
		}
		fmt.Printf("%sFileDeclarationStatement Name: %s %s %s\n", indent, n.Name.String(), typeDecl, n.TokenLiteral())
		fmt.Println(indent + "  Value:")
		PrintAST(n.Value, indent+"    ")

	case *ReadStatement:
		fmt.Printf("%sReadStatement File: %s, Into: %s\n", indent, n.FileHandle.String(), n.RecordVar.String())

	case *WriteStatement:
		fmt.Printf("%sWriteStatement File: %s, From: %s\n", indent, n.FileHandle.String(), n.RecordVar.String())

	// --- EXPRESSIONS ---
	case *Identifier:
		symbolType := "nil"
		symbolWidth := 0
		if n.Symbol != nil {
			symbolType = n.Symbol.Type
			symbolWidth = n.Symbol.Width
			if n.Symbol.RecordTypeSymbol != nil { // It's a record variable
				symbolType = n.Symbol.RecordTypeSymbol.Name
				symbolWidth = n.Symbol.RecordTypeSymbol.TotalWidth
			}
		}
		fmt.Printf("%sIdentifier: %s (Symbol Type: %s, Width: %d)\n", indent, n.Value, symbolType, symbolWidth)

	case *StringLiteral:
		fmt.Printf("%sStringLiteral: %q (Width: %d)\n", indent, n.Value, n.Width)

	case *IntegerLiteral:
		fmt.Printf("%sIntegerLiteral: %d (Width: %d)\n", indent, n.Value, n.Width)

	case *BooleanLiteral:
		fmt.Printf("%sBooleanLiteral: %s (Width: 1)\n", indent, n.Value)

	case *BinaryExpression:
		fmt.Printf("%sBinaryExpression Op: %s (ResultType: %s, ResultWidth: %d)\n", indent, n.Operator, n.ResultType(), n.ResultWidth())
		fmt.Println(indent + "  Left:")
		PrintAST(n.Left, indent+"    ")
		fmt.Println(indent + "  Right:")
		PrintAST(n.Right, indent+"    ")

	case *GroupedExpression:
		fmt.Println(indent + "GroupedExpression")
		PrintAST(n.Expression, indent+"  ")

	case *ProcCallExpression:
		fmt.Printf("%sProcCallExpression Func: %s (Returns: %s, Width: %d)\n", indent, n.Function.String(), n.ResultType(), n.ResultWidth())
		for i, arg := range n.Arguments {
			fmt.Printf(indent+"  Arg[%d]:\n", i)
			PrintAST(arg, indent+"    ")
		}
	case *InputExpression:
		fmt.Printf("%sInputExpression File: %s, Record: %s\n", indent, n.SystemFileName.String(), n.RecordTypeName.String())
	case *OutputExpression:
		fmt.Printf("%sOutputExpression File: %s, Record: %s\n", indent, n.SystemFileName.String(), n.RecordTypeName.String())

	case *FieldAccessExpression:
		fieldType := "unknown"
		fieldWidth := 0
		if n.ResolvedField != nil {
			fieldType = n.ResolvedField.Type
			fieldWidth = n.ResolvedField.Width
		}
		fmt.Printf("%sFieldAccessExpression Field: %s (Type: %s, Width: %d)\n", indent, n.Field.String(), fieldType, fieldWidth)
		fmt.Println(indent + "  Record:")
		PrintAST(n.Record, indent+"    ")

	default:
		fmt.Printf("%s<unknown node type: %T>\n", indent, n)
	}
}
