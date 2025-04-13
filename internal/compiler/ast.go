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
	GetToken() Token
}

// --- Symbol Info ---
type SymbolInfo struct {
	Type    string // string, int, bool, proc, unknown, undeclared etc
	Width   int
	IsConst bool

	// --- Proc specific info ---
	ParamNames  []string
	ParamTypes  []string
	ParamWidths []int
	ReturnType  string
	ReturnWidth int
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

// ReturnStatement -> return expression or return
type ReturnStatement struct {
	Token       Token
	ReturnValue Expression
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

type TypeNode struct {
	Token      Token  // The type token (e.g., 'int', 'string', 'void')
	Name       string // "int", "string", "void"
	WidthToken Token  // Optional width token (e.g., '10')
	Width      int    // Parsed width (explicit or default), 0 for void
	IsVoid     bool
}

func (tn *TypeNode) TokenLiteral() string { return tn.Token.Literal }
func (tn *TypeNode) String() string {
	if tn.IsVoid {
		return "void"
	}
	s := tn.Name
	// Only show width if it was *explicitly* provided via WidthToken
	if tn.WidthToken.Type == TokenInt {
		s += fmt.Sprintf("(%s)", tn.WidthToken.Literal)
	}
	return s
}

type Parameter struct {
	Name     *Identifier
	TypeNode *TypeNode
}

func (p *Parameter) String() string {
	typeStr := p.TypeNode.Name
	// Check if the width came from an explicit token
	if p.TypeNode.WidthToken.Type == TokenInt {
		typeStr += fmt.Sprintf("(%d)", p.TypeNode.Width) // Show explicit width
	} else if p.TypeNode.Width > 0 && !p.TypeNode.IsVoid {
		// If width > 0 but no explicit token, it was inferred (default)
		typeStr += fmt.Sprintf("(default:%d)", p.TypeNode.Width) // Optional: Indicate default
	} else if !p.TypeNode.IsVoid {
		// Should not happen if parser enforces width correctly
		typeStr += "(?)" // Indicate error if width somehow missing/invalid
	}
	return p.Name.String() + ": " + typeStr
}

// ProcDeclarationStatement -> proc name() { body }
type ProcDeclarationStatement struct {
	Token      Token // The 'proc' token
	Name       *Identifier
	Parameters []*Parameter
	ReturnType *TypeNode
	Body       *BlockStatement
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
func (i *Identifier) GetToken() Token      { return i.Token }

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
	return fmt.Sprintf("%q", sl.Value)
}
func (sl *StringLiteral) GetToken() Token { return sl.Token }

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
func (il *IntegerLiteral) String() string {
	return il.Token.Literal // Return the original literal string
}
func (il *IntegerLiteral) GetToken() Token { return il.Token }

// BinaryExpression -> (left + right)
type BinaryExpression struct {
	Token    Token // +, -, *, /
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
func (be *BinaryExpression) GetToken() Token { return be.Token }

// calculateResults computes and caches type/width if not already done.
// Called internally by ResultType and ResultWidth.
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
func (ge *GroupedExpression) GetToken() Token { return ge.Token }

// ProcCallExpression represents 'funcName(arg1, arg2)'
type ProcCallExpression struct {
	Token               Token        // The function name token
	Function            *Identifier  // The identifier for the function
	Arguments           []Expression // List of argument expressions
	ResolvedReturnType  string
	ResolvedReturnWidth int
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
	if pce.Function != nil {
		out.WriteString(pce.Function.String())
	}
	out.WriteString("(")
	out.WriteString(strings.Join(args, ", "))
	out.WriteString(")")
	return out.String()
}
func (pce *ProcCallExpression) GetToken() Token { return pce.Token }
