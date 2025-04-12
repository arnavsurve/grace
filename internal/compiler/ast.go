package compiler

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/arnavsurve/grace/internal/compiler/lib"
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
}

// --- Symbol Info ---
type SymbolInfo struct {
	Type    string // string, int, bool, proc, unknown, undeclared etc
	Width   int
	IsConst bool
	// TODO: Add Proc specific info later (params, return type)
}

// --- Program ---
type Program struct {
	Statements  []Statement
	SymbolTable map[string]SymbolInfo
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
	Token Token // print
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

// DeclarationStatement -> x := 123 or const y : string = "hi"
type DeclarationStatement struct {
	Token              Token // := or = (depending on explicit type)
	IsConst            bool
	Name               *Identifier
	HasExplicitType    bool
	ExplicitTypeToken  Token
	ExplicitWidthToken Token
	Value              Expression
}

func (ds *DeclarationStatement) statementNode()       {}
func (ds *DeclarationStatement) TokenLiteral() string { return ds.Token.Literal } // Literal is ":=" or "="
func (ds *DeclarationStatement) String() string {
	var out bytes.Buffer
	if ds.IsConst {
		out.WriteString("const ")
	}
	if ds.Name != nil {
		out.WriteString(ds.Name.String()) // Use Identifier's String()
	}

	if ds.HasExplicitType {
		// name: type = value
		out.WriteString(": ")
		out.WriteString(ds.ExplicitTypeToken.Literal)
		out.WriteString("(" + ds.ExplicitTypeToken.Literal + ")")
		out.WriteString(" ")
		out.WriteString(ds.TokenLiteral()) // =
		out.WriteString(" ")
	} else {
		// Inferred type - name := value
		out.WriteString(" ")
		out.WriteString(ds.TokenLiteral())
		out.WriteString(" ")
	}

	if ds.Value != nil {
		out.WriteString(ds.Value.String())
	}
	return out.String()
}

// ReassignmentStatement -> x = 456
type ReassignmentStatement struct {
	Token Token // =
	Name  *Identifier
	Value Expression
}

func (rs *ReassignmentStatement) statementNode()       {}
func (rs *ReassignmentStatement) TokenLiteral() string { return rs.Token.Literal } // Literal is "="
func (rs *ReassignmentStatement) String() string {
	var out bytes.Buffer
	if rs.Name != nil {
		out.WriteString(rs.Name.String())
	}
	out.WriteString(" " + rs.TokenLiteral() + " ") // " = "
	if rs.Value != nil {
		out.WriteString(rs.Value.String())
	}
	return out.String()
}

// BlockStatement -> { statement1 \n statement2 }
type BlockStatement struct {
	Token      Token // {
	Statements []Statement
}

func (bs *BlockStatement) statementNode()       {}
func (bs *BlockStatement) TokenLiteral() string { return bs.Token.Literal }
func (bs *BlockStatement) String() string {
	var out bytes.Buffer
	out.WriteString("{\n") // Opening brace and newline
	for _, s := range bs.Statements {
		// Add indentation for statements within the block
		out.WriteString("\t" + s.String() + "\n") // Add tab and newline
	}
	out.WriteString("}") // Closing brace (might want a newline after depending on context)
	return out.String()
}

// ProcDeclarationStatement -> proc name() { body }
type ProcDeclarationStatement struct {
	Token Token // The 'proc' token
	Name  *Identifier
	Body  *BlockStatement
	// TODO: Parameters []Identifier
	// TODO: ReturnType Node (Type Node)
}

func (pds *ProcDeclarationStatement) statementNode()       {}
func (pds *ProcDeclarationStatement) TokenLiteral() string { return pds.Token.Literal }

func (pds *ProcDeclarationStatement) String() string {
	var out bytes.Buffer
	out.WriteString(pds.TokenLiteral() + " ") // "proc "
	if pds.Name != nil {
		out.WriteString(pds.Name.String())
	}
	out.WriteString("()") // Parentheses for parameters (empty for now)
	// TODO: Add parameter list string representation later
	// TODO: Add return type string representation later
	out.WriteString(" ")
	if pds.Body != nil {
		out.WriteString(pds.Body.String()) // Uses BlockStatement's String()
	}
	return out.String()
}

// ExpressionStatement wraps an expression to be used as a statement (e.g., a function call)
type ExpressionStatement struct {
	Token      Token // the first token of the expression
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

// Identifier -> varName
type Identifier struct {
	Token        Token // IDENT
	Value        string
	Width        int
	ResolvedType string // Store the resolved type during parsing/semantic analysis
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
func (i *Identifier) ResultType() string   { return i.ResolvedType }
func (i *Identifier) ResultWidth() int     { return i.Width }
func (i *Identifier) String() string       { return i.Value }

// StringLiteral -> "hello"
type StringLiteral struct {
	Token Token
	Value string
	Width int
}

func (sl *StringLiteral) expressionNode()      {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }
func (sl *StringLiteral) ResultType() string   { return "string" }
func (sl *StringLiteral) ResultWidth() int     { return sl.Width }
func (sl *StringLiteral) String() string {
	return `"` + sl.Value + `"`
}

// IntegerLiteral -> 123
type IntegerLiteral struct {
	Token Token
	Value int
	Width int
}

func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) ResultType() string   { return "int" }
func (il *IntegerLiteral) ResultWidth() int     { return il.Width }
func (il *IntegerLiteral) String() string { // Format the integer as a string
	return fmt.Sprintf("%d", il.Value)
}

// BinaryExpression -> (left + right)
type BinaryExpression struct {
	Token    Token // +, -, *, /
	Left     Expression
	Operator string // +, -, *, /
	Right    Expression
}

func (be *BinaryExpression) expressionNode()      {}
func (be *BinaryExpression) TokenLiteral() string { return be.Token.Literal }
func (be *BinaryExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(") // Parentheses for clarity/precedence
	if be.Left != nil {
		out.WriteString(be.Left.String())
	}
	out.WriteString(" " + be.Operator + " ")
	if be.Right != nil {
		out.WriteString(be.Right.String())
	}
	out.WriteString(")")
	return out.String()
}

func (be *BinaryExpression) ResultWidth() int {
	// Calculate string concatenation width
	if be.ResultType() == "string" {
		leftLit, leftIsLit := be.Left.(*StringLiteral)
		rightLit, rightIsLit := be.Right.(*StringLiteral)
		if leftIsLit && rightIsLit {
			return len(leftLit.Value) + len(rightLit.Value)
		}
		leftWidth := be.Left.ResultWidth()
		rightWidth := be.Right.ResultWidth()
		if leftWidth <= 0 {
			leftWidth = lib.DefaultStringWidth
		}
		if rightWidth <= 0 {
			rightWidth = lib.DefaultStringWidth
		}
		return leftWidth + rightWidth
	}
	// Integer width calculation
	leftLit, leftIsLit := be.Left.(*IntegerLiteral)
	rightLit, rightIsLit := be.Right.(*IntegerLiteral)
	if leftIsLit && rightIsLit {
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
				return lib.DefaultIntWidth
			}
			resultVal = leftVal / rightVal
		default:
			return lib.DefaultIntWidth
		}
		return lib.CalculateWidthForValue(resultVal)
	}
	// Fallback heuristics
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
		return max(leftWidth, rightWidth) + 1
	case "*":
		return leftWidth + rightWidth
	case "/":
		return leftWidth
	default:
		return lib.DefaultIntWidth
	}
}

func (be *BinaryExpression) ResultType() string {
	leftType := be.Left.ResultType()
	rightType := be.Right.ResultType()
	if leftType == "int" && rightType == "int" {
		return "int"
	}
	if leftType == "string" && rightType == "string" && be.Operator == "+" {
		return "string"
	}
	return "unknown"
}

// GroupedExpression -> (expression)
type GroupedExpression struct {
	Token      Token // '('
	Expression Expression
}

func (ge *GroupedExpression) expressionNode()      {}
func (ge *GroupedExpression) TokenLiteral() string { return ge.Token.Literal }
func (ge *GroupedExpression) String() string {
	if ge.Expression != nil {
		return "(" + ge.Expression.String() + ")" // Wrap inner expression in parens
	}
	return "()" // Should not happen with valid parsing
}

func (ge *GroupedExpression) ResultType() string {
	if ge.Expression != nil {
		return ge.Expression.ResultType()
	}
	return "unknown"
}

func (ge *GroupedExpression) ResultWidth() int {
	if ge.Expression != nil {
		return ge.Expression.ResultWidth()
	}
	return 0
}

// ProcCallExpression represents 'funcName(arg1, arg2)'
type ProcCallExpression struct {
	Token     Token        // The function name token
	Function  *Identifier  // The identifier for the function
	Arguments []Expression // List of argument expressions
}

func (pce *ProcCallExpression) expressionNode()      {}
func (pce *ProcCallExpression) TokenLiteral() string { return pce.Token.Literal }
func (pce *ProcCallExpression) ResultType() string   { return "void" } // TODO: Update when functions return values
func (pce *ProcCallExpression) ResultWidth() int     { return 0 }      // TODO: Update when functions return values
func (pce *ProcCallExpression) String() string {
	var out bytes.Buffer
	args := []string{}
	if pce.Arguments != nil {
		for _, a := range pce.Arguments {
			args = append(args, a.String())
		}
	}
	if pce.Function != nil {
		out.WriteString(pce.Function.String())
	}
	out.WriteString("(")
	out.WriteString(strings.Join(args, ", "))
	out.WriteString(")")
	return out.String()
}
