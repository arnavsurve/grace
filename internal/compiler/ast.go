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
}

// Program is the root of every parsed file
type Program struct {
	Statements  []Statement
	SymbolTable map[string]string
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	}
	return ""
}

// PrintStatement -> print("hello")
type PrintStatement struct {
	Token Token // print
	Value Expression
}

func (ps *PrintStatement) statementNode()       {}
func (ps *PrintStatement) TokenLiteral() string { return ps.Token.Literal }

// StringLiteral -> "hello"
type StringLiteral struct {
	Token Token
	Value string
}

func (sl *StringLiteral) expressionNode()      {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }

// IntegerLiteral -> 21
type IntegerLiteral struct {
	Token Token
	Value int
}

func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }

type AssignmentStatement struct {
	Token Token // :=
	Name  *Identifier
	Value Expression
}

func (as *AssignmentStatement) statementNode()       {}
func (as *AssignmentStatement) TokenLiteral() string { return as.Token.Literal }

type Identifier struct {
	Token Token // IDENT
	Value string
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
