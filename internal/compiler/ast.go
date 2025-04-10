package compiler

type Node interface {
	TokenLiteral() string
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

type SymbolInfo struct {
	Type    string // string, int, etc
	Width   int    // e.g. 6 for int(6) = max value of 999999, 15 for string(15), etc
	IsConst bool
}

// Program is the root of every parsed file
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

// PrintStatement -> print("hello") or print(var)
type PrintStatement struct {
	Token Token // print
	Value Expression
}

func (ps *PrintStatement) statementNode()       {}
func (ps *PrintStatement) TokenLiteral() string { return ps.Token.Literal }

type DeclarationStatement struct {
	Token   Token // :=
	IsConst bool
	Name    *Identifier
	Value   Expression
}

func (ds *DeclarationStatement) statementNode()       {}
func (ds *DeclarationStatement) TokenLiteral() string { return ds.Token.Literal } // Literal is ":="

type ReassignmentStatement struct {
	Token Token // =
	Name  *Identifier
	Value Expression
}

func (rs *ReassignmentStatement) statementNode()       {}
func (rs *ReassignmentStatement) TokenLiteral() string { return rs.Token.Literal } // Literal is "="

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

// IntegerLiteral -> 21
type IntegerLiteral struct {
	Token Token
	Value int
	Width int
}

func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) ResultType() string   { return "int" }
func (il *IntegerLiteral) ResultWidth() int     { return il.Width }

type BinaryExpression struct {
	Token    Token // +, -, *, /
	Left     Expression
	Operator string // +, -, *, /
	Right    Expression
}

func (be *BinaryExpression) expressionNode()      {}
func (be *BinaryExpression) TokenLiteral() string { return be.Token.Literal }

func (be *BinaryExpression) ResultWidth() int {
	leftWidth := be.Left.ResultWidth()
	rightWidth := be.Right.ResultWidth()

	switch be.Operator {
	case "+", "-":
		return max(leftWidth, rightWidth) + 1
	case "*":
		return leftWidth + rightWidth
	case "/":
		// Conservative for now.
		// TODO: this does not reflect the potential size needed, especially for decimals
		// Placeholder for now
		return leftWidth
	default:
		return 0 // Unknown
	}
}

// ResultType for BinaryExpression depends on operands and operator
// For now, assume int + int -> int, etc. String ops can be added later
func (be *BinaryExpression) ResultType() string {
	leftType := be.Left.ResultType()
	rightType := be.Right.ResultType()

	if leftType == "int" && rightType == "int" {
		return "int"
	}

	return "unknown"
}

type GroupedExpression struct {
	Token      Token // '('
	Expression Expression
}

func (ge *GroupedExpression) expressionNode()      {}
func (ge *GroupedExpression) TokenLiteral() string { return ge.Token.Literal }

func (ge *GroupedExpression) ResultType() string {
	return ge.Expression.ResultType()
}
