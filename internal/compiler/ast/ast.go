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
}

// --- Symbol Info ---

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

// DeclarationStatement -> x := 123 or const y : string = "hi"
type DeclarationStatement struct {
	Token              token.Token // := or = (depending on explicit type)
	IsConst            bool
	Name               *Identifier
	HasExplicitType    bool
	ExplicitTypeToken  token.Token
	ExplicitWidthToken token.Token
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
	Token token.Token // =
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
	Token      token.Token // {
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
	Token       token.Token
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
	Token      token.Token // The type token (e.g., 'int', 'string', 'void')
	Name       string      // "int", "string", "void"
	WidthToken token.Token // Optional width token (e.g., '10')
	Width      int         // Parsed width (explicit or default), 0 for void
	IsVoid     bool
}

func (tn *TypeNode) TokenLiteral() string { return tn.Token.Literal }
func (tn *TypeNode) String() string {
	if tn.IsVoid {
		return "void"
	}
	s := tn.Name
	// Only show width if it was *explicitly* provided via WidthToken
	if tn.WidthToken.Type == token.TokenInt {
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
	if p.TypeNode.WidthToken.Type == token.TokenInt {
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
	Token      token.Token // The 'proc' token
	Name       *Identifier
	Parameters []*Parameter
	ReturnType *TypeNode
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

// Identifier -> varName
type Identifier struct {
	Token token.Token // IDENT
	Value string
	// Annotate identifier with symbol info during parsing/semantic analysis so the
	// emitter doesn't have to traverse that shit and I don't have to implement that shit.
	Symbol *symbols.SymbolInfo
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }

func (i *Identifier) ResultType() string {
	if i.Symbol != nil {
		return i.Symbol.Type
	}
	return "unknown" // Should not happen if parser catches undeclared
}

func (i *Identifier) ResultWidth() int {
	if i.Symbol != nil {
		return i.Symbol.Width
	}
	return 0
}

func (i *Identifier) String() string        { return i.Value }
func (i *Identifier) GetToken() token.Token { return i.Token }

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
func (sl *StringLiteral) GetToken() token.Token { return sl.Token }

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
func (il *IntegerLiteral) GetToken() token.Token { return il.Token }

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
func (be *BinaryExpression) GetToken() token.Token { return be.Token }

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
	Token      token.Token // '('
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
func (ge *GroupedExpression) GetToken() token.Token { return ge.Token }

// ProcCallExpression represents 'funcName(arg1, arg2)'
type ProcCallExpression struct {
	Token     token.Token  // The function name token
	Function  *Identifier  // The identifier for the function
	Arguments []Expression // List of argument expressions

	// These are set by parser using Function.symbol
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
func (pce *ProcCallExpression) GetToken() token.Token { return pce.Token }

// Helper for pretty printing AST to bless it with my chud eyes
func PrintAST(node Node, indent string) {
	switch n := node.(type) {
	case *Program:
		fmt.Println(indent + "Program")
		for _, stmt := range n.Statements {
			PrintAST(stmt, indent+"  ")
		}

	case *DeclarationStatement:
		fmt.Println(indent + "DeclarationStatement")
		fmt.Println(indent+"  Name:", n.Name.String())
		fmt.Println(indent+"  IsConst:", n.IsConst)
		if n.HasExplicitType {
			fmt.Println(indent+"  Type:", n.ExplicitTypeToken.Literal)
			fmt.Println(indent+"  Width:", n.ExplicitWidthToken.Literal)
		} else {
			fmt.Println(indent + "  Type: inferred")
		}
		fmt.Println(indent + "  Value:")
		PrintAST(n.Value, indent+"    ")

	case *ReassignmentStatement:
		fmt.Println(indent + "ReassignmentStatement")
		fmt.Println(indent+"  Name:", n.Name.String())
		fmt.Println(indent + "  Value:")
		PrintAST(n.Value, indent+"    ")

	case *PrintStatement:
		fmt.Println(indent + "PrintStatement")
		PrintAST(n.Value, indent+"  ")

	case *ReturnStatement:
		fmt.Println(indent + "ReturnStatement")
		PrintAST(n.ReturnValue, indent+"  ")

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

	case *Identifier:
		symbolType := "nil"
		if n.Symbol != nil {
			symbolType = n.Symbol.Type
		}
		fmt.Printf("%sIdentifier: %s (Symbol Type: %s)\n", indent, n.Value, symbolType)

	case *StringLiteral:
		fmt.Println(indent+"StringLiteral:", fmt.Sprintf("%q", n.Value))

	case *IntegerLiteral:
		fmt.Println(indent+"IntegerLiteral:", n.Value)

	case *BinaryExpression:
		fmt.Println(indent + "BinaryExpression")
		fmt.Println(indent+"  Operator:", n.Operator)
		fmt.Println(indent + "  Left:")
		PrintAST(n.Left, indent+"    ")
		fmt.Println(indent + "  Right:")
		PrintAST(n.Right, indent+"    ")

	case *GroupedExpression:
		fmt.Println(indent + "GroupedExpression")
		PrintAST(n.Expression, indent+"  ")

	case *ProcCallExpression:
		fmt.Println(indent + "ProcCallExpression")
		fmt.Println(indent+"  Function:", n.Function.String())
		for i, arg := range n.Arguments {
			fmt.Printf(indent+"  Arg[%d]:\n", i)
			PrintAST(arg, indent+"    ")
		}

	default:
		fmt.Printf("%s<unknown node type: %T>\n", indent, n)
	}
}
