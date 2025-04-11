package compiler

import (
	"github.com/arnavsurve/grace/internal/compiler/lib"
)

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
	// Calculate string concatenation width
	if be.ResultType() == "string" {
		// --- Constant Folding Check ---
		leftLit, leftIsLit := be.Left.(*StringLiteral)
		rightLit, rightIsLit := be.Right.(*StringLiteral)

		if leftIsLit && rightIsLit {
			// Both operants are string literals
			return len(leftLit.Value) + len(rightLit.Value)
		}

		// --- Mixed types or variables ---
		// Use the inferred/declared widths of the operands
		leftWidth := be.Left.ResultWidth()
		rightWidth := be.Right.ResultWidth()

		if leftWidth <= 0 || rightWidth <= 0 {
			return lib.DefaultStringWidth
		}

		return leftWidth + rightWidth
	}

	// Integer width calculation
	// --- Constant Folding Check ---
	leftLit, leftIsLit := be.Left.(*IntegerLiteral)
	rightLit, rightIsLit := be.Right.(*IntegerLiteral)

	if leftIsLit && rightIsLit {
		// Both operands are literals, calculate the exact result width
		leftVal := leftLit.Value
		rightVal := rightLit.Value
		var resultVal int

		switch be.Operator {
		case "+":
			resultVal = leftVal + rightVal
		case "-":
			resultVal = leftVal - rightVal // Result could be negative? PIC 9 handles unsigned
		case "*":
			resultVal = leftVal * rightVal
		case "/":
			if rightVal == 0 {
				// Parser already adds error, return a default/error width
				return lib.DefaultIntWidth
			}
			resultVal = leftVal / rightVal
		default:
			return lib.DefaultIntWidth // Unknown operator
		}

		return max(lib.DefaultIntWidth, lib.CalculateWidthForValue(resultVal))
	}

	// Fallback to heuristics (for integers) if not all literals

	leftWidth := be.Left.ResultWidth()
	rightWidth := be.Right.ResultWidth()

	if leftWidth <= 0 || rightWidth <= 0 {
		// This might happen if an operand's width couldn't be determined (e.g. undeclared var)
		return lib.DefaultIntWidth
	}

	switch be.Operator {
	case "+", "-":
		return max(leftWidth, rightWidth) + 1
	case "*":
		return leftWidth + rightWidth
	case "/":
		// Conservative for now.
		// TODO: this does not reflect the potential size needed, especially for decimals
		// Placeholder for now
		// NOTE: result width is generally <= dividend width
		return leftWidth
	default:
		return lib.DefaultIntWidth // Unknown operator
	}
}

// ResultType for BinaryExpression depends on operands and operator
// For now, assume int + int -> int, etc. String ops can be added later
func (be *BinaryExpression) ResultType() string {
	leftType := be.Left.ResultType()
	rightType := be.Right.ResultType()

	if leftType == "int" && rightType == "int" && be.Operator != "+" {
		// Arithmetic operators that are not '+' only work on ints
		// TODO: floats as well
		return "int"
	} else if leftType == "int" && rightType == "int" && be.Operator == "+" {
		return "int"
	} else if leftType == "string" && rightType == "string" && be.Operator == "+" {
		return "string"
	}

	// Mismatched types or unsupported operator for types will be caught by parser
	// Return unknown here, parser will add the specific error
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

func (ge *GroupedExpression) ResultWidth() int {
	if ge.Expression == nil {
		// Parser bug
		return 0 // TODO: consider default or error signaling
	}
	return ge.Expression.ResultWidth()
}
