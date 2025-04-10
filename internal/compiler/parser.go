package compiler

import (
	"fmt"
	"strconv"
)

// Constants for Precedence
const (
	_ int = iota // Give values starting from 0, but assign to blank identifier
	LOWEST
	SUM     // +, -
	PRODUCT // *, /
	// PREFIX // -X or !X (We don't have these yet)
	// CALL   // myFunction(X) (We don't have these yet)
	// INDEX  // array[index] (We don't have these yet)
)

// TODO: Add precedence map for tokens later if using Pratt parsing or more complex logic
// var precedences = map[TokenType]int{...}

type Parser struct {
	l           *Lexer
	curTok      Token
	peekTok     Token
	errors      []string
	symbolTable map[string]SymbolInfo // Track declared variables, types, const status
}

func NewParser(l *Lexer) *Parser {
	p := &Parser{
		l:           l,
		symbolTable: make(map[string]SymbolInfo),
	}
	p.nextToken() // Initialize curTok
	p.nextToken() // Initialize peekTok
	return p
}

func (p *Parser) nextToken() {
	p.curTok = p.peekTok
	p.peekTok = p.l.NextToken()
	// Debugging token stream:
	// fmt.Printf("curTok: %v, peekTok: %v\n", p.curTok, p.peekTok)
}

func (p *Parser) addError(format string, args ...any) {
	// TODO: Add line/column numbers later
	errMsg := fmt.Sprintf(format, args...)
	p.errors = append(p.errors, errMsg)
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) getSymbolInfo(name string) (SymbolInfo, bool) {
	info, exists := p.symbolTable[name]
	return info, exists
}

func (p *Parser) ParseProgram() *Program {
	program := &Program{
		Statements:  []Statement{},
		SymbolTable: p.symbolTable, // Share the final symbol table
	}

	for p.curTok.Type != TokenEOF {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		} else if len(p.errors) > 0 && p.curTok.Type != TokenEOF {
			// Basic error recovery: If parseStatement failed and returned nil,
			// and we are not at EOF, advance token to prevent infinite loop
			// on the problematic token. More sophisticated recovery could try
			// to sync to the next statement start.
			// fmt.Printf("Recovery: Skipping token %v\n", p.curTok) // Debugging
			// p.nextToken() // Let's comment this out for now, rely on statement parsers advancing
		}
		// Ensure we always advance if parseStatement didn't consume anything
		// This might be needed if an error occurs very early in parseStatement
		// NO - parseStatement and its callees *must* consume tokens to make progress or report EOF.
	}

	return program
}

func (p *Parser) parseStatement() Statement {
	switch p.curTok.Type {
	case TokenPrint:
		return p.parsePrintStatement()
	case TokenConst:
		return p.parseDeclarationStatement()
	case TokenIdent:
		// Look ahead to decide if declaration or reassignment
		switch p.peekTok.Type {
		case TokenAssignDefine: // :=
			return p.parseDeclarationStatement()
		case TokenAssign: // =
			return p.parseReassignmentStatement()
		default:
			p.addError("Syntax Error: Expected ':=' or '=' after identifier '%s', got '%s'", p.curTok.Literal, p.peekTok.Literal)
			// Attempt recovery: Skip the identifier and the unexpected token
			p.nextToken()                  // Consume identifier
			if p.curTok.Type != TokenEOF { // Avoid skipping EOF
				p.nextToken() // Consume the unexpected peek token
			}
			return nil
		}
	case TokenEOF:
		return nil
	default:
		p.addError("Syntax Error: Unexpected token at start of statement: %s ('%s')", p.curTok.Type, p.curTok.Literal)
		// Attempt recovery: consume the unexpected token
		if p.curTok.Type != TokenEOF { // Avoid skipping EOF
			p.nextToken()
		}
		return nil
	}
}

func (p *Parser) parseDeclarationStatement() Statement {
	stmt := &DeclarationStatement{}
	startTok := p.curTok // Keep track for context

	// Handle optional 'const'
	if p.curTok.Type == TokenConst {
		stmt.IsConst = true
		p.nextToken() // Consume 'const'
		if p.curTok.Type != TokenIdent {
			p.addError("Syntax Error: Expected identifier after 'const', got %s", p.curTok.Type)
			return nil
		}
	}

	// Current token must be IDENT now
	if p.curTok.Type != TokenIdent {
		p.addError("Internal Parser Error: Expected identifier for declaration start (or after const), got %s", p.curTok.Type)
		return nil
	}
	stmt.Name = &Identifier{Token: p.curTok, Value: p.curTok.Literal}
	varName := stmt.Name.Value

	// Check for redeclaration *before* parsing the value
	_, declared := p.getSymbolInfo(varName)
	if declared {
		p.addError("Semantic Error: Variable '%s' already declared", varName)
		// Continue parsing, but won't add to symbol table
	}

	// Expect :=
	if p.peekTok.Type != TokenAssignDefine {
		expected := "':='"
		// More specific error if const was used without :=
		if startTok.Type == TokenConst {
			p.addError("Syntax Error: Constants must be declared using ':=', found '%s' after 'const %s'", p.peekTok.Literal, varName)
		} else {
			p.addError("Syntax Error: Expected %s after identifier '%s' in declaration, got '%s'", expected, varName, p.peekTok.Literal)
		}
		return nil
	}
	p.nextToken()         // Consume IDENT
	stmt.Token = p.curTok // Store := token
	p.nextToken()         // Consume :=, curTok is now start of expression

	// Call the expression parser
	expr := p.parseExpression(LOWEST) // Start with lowest precedence
	if expr == nil {
		// Error should have been added by parseExpression or its children
		// Check if it was just missing
		if p.curTok.Type != TokenEOF && p.curTok != stmt.Token { // Avoid error if we just consumed :=
			p.addError("Syntax Error: Invalid or missing expression after ':=' for variable '%s'", varName)
		}
		return nil // Cannot proceed without a valid expression
	}
	stmt.Value = expr

	// Semantic Analysis: Determine type and update symbol table
	valueType := expr.ResultType()

	if valueType == "unknown" {
		// Error reported during expression parsing
	}

	// Add to symbol table only if not previously declared and type is known
	if !declared && valueType != "unknown" {
		p.symbolTable[varName] = SymbolInfo{Type: valueType, IsConst: stmt.IsConst}
		stmt.Name.ResolvedType = valueType
	}

	// parseExpression consumed all tokens for the expression.
	// curTok should now be the token *after* the expression (e.g., EOF, or start of next statement).
	return stmt
}

func (p *Parser) parseReassignmentStatement() Statement {
	stmt := &ReassignmentStatement{}

	// Current token is IDENT
	if p.curTok.Type != TokenIdent {
		p.addError("Internal Parser Error: Expected identifier for reassignment, got %s", p.curTok.Type)
		return nil
	}
	stmt.Name = &Identifier{Token: p.curTok, Value: p.curTok.Literal}
	varName := stmt.Name.Value

	// Semantic Check 1 & 2: Is declared? Is const?
	symbolInfo, declared := p.getSymbolInfo(varName)
	isConst := false
	if !declared {
		p.addError("Semantic Error: Cannot assign to undeclared variable '%s'", varName)
		symbolInfo = SymbolInfo{Type: "undeclared"} // Placeholder
	} else {
		if symbolInfo.IsConst {
			p.addError("Semantic Error: Cannot assign to constant variable '%s'", varName)
			isConst = true
		}
		stmt.Name.ResolvedType = symbolInfo.Type // Store original type even if const error
	}

	// Expect =
	if p.peekTok.Type != TokenAssign {
		p.addError("Syntax Error: Expected '=' after identifier '%s' in assignment, got '%s'", varName, p.peekTok.Literal)
		return nil
	}
	p.nextToken()         // Consume IDENT
	stmt.Token = p.curTok // Store = token
	p.nextToken()         // Consume =, curTok is now expression start

	// Call the expression parser
	expr := p.parseExpression(LOWEST) // Start with lowest precedence
	if expr == nil {
		if p.curTok.Type != TokenEOF && p.curTok != stmt.Token {
			p.addError("Syntax Error: Invalid or missing expression after '=' for variable '%s'", varName)
		}
		return nil
	}
	stmt.Value = expr

	// Semantic Check 3: Type compatibility
	// Only perform if variable was declared and not const
	if declared && !isConst {
		valueType := expr.ResultType()
		if valueType == "unknown" {
			// Error already reported
		} else if symbolInfo.Type != valueType {
			p.addError("Semantic Error: Type mismatch - cannot assign value of type %s to variable '%s' (type %s)", valueType, varName, symbolInfo.Type)
		}
	}

	// parseExpression consumed expression tokens
	return stmt
}

func (p *Parser) parsePrintStatement() Statement {
	stmt := &PrintStatement{Token: p.curTok} // Store PRINT token

	// Expect (
	if p.peekTok.Type != TokenLParen {
		p.addError("Syntax Error: Expected '(' after 'print', got %s", p.peekTok.Literal)
		return nil
	}
	p.nextToken() // Consume PRINT
	p.nextToken() // Consume (, curTok is now expression start

	// Call the expression parser
	expr := p.parseExpression(LOWEST) // Start with lowest precedence
	if expr == nil {
		// Error should have been added by parseExpression
		if p.curTok.Type == TokenRParen { // Maybe just an empty print()?
			p.addError("Syntax Error: Expected expression inside print(), found ')'")
		} else if p.curTok.Type != TokenEOF {
			// Error added by parseExpression if token unexpected
		}
		return nil
	}
	stmt.Value = expr

	// Semantic check: Ensure expression type is printable
	valueType := expr.ResultType()
	if valueType == "unknown" {
		// Error already reported
	} else if valueType != "int" && valueType != "string" {
		p.addError("Semantic Error: Cannot print value of type %s", valueType)
	}

	// --- Expect and Consume ')' ---
	// After parseExpression returns, curTok should be the token *after* the expression.
	// We expect this token to be ')'.
	if p.curTok.Type != TokenRParen {
		p.addError("Syntax Error: Expected ')' after print expression, got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		return nil // Structure is broken
	}
	p.nextToken() // Consume the ')'

	return stmt
}

// Expression Parsing (Recursive Descent / Pratt Style Mix)

// NOTE: This implementation is closer to Pratt parsing's idea of precedence
// levels passed down, even though we don't use explicit prefix/infix parse functions yet.
// The precedence argument helps control operator binding.

// parseExpression: Entry point and handles lowest precedence operators recursively.
func (p *Parser) parseExpression(precedence int) Expression {
	// "prefix" part: Get the initial part of the expression (a primary or unary op)
	leftExpr := p.parsePrefix()
	if leftExpr == nil {
		return nil // Error parsing the start of the expression
	}

	// "infix" part: Loop while the *next* operator has higher precedence
	// than the current 'precedence' level we're allowed to handle.
	for p.peekTok.Type != TokenEOF && precedence < p.peekPrecedence() {
		// Check if the peek token is an infix operator we handle (+, -, *, /)
		peekType := p.peekTok.Type
		if peekType != TokenPlus && peekType != TokenMinus &&
			peekType != TokenAsterisk && peekType != TokenSlash {
			return leftExpr // Not an infix operator we handle at this level
		}

		p.nextToken()                               // Consume the left expression's last token. curTok is now the operator.
		leftExpr = p.parseInfixExpression(leftExpr) // Build BinaryExpression
		if leftExpr == nil {
			return nil // Error during infix parsing
		}
	}

	return leftExpr
}

// parsePrefix: Parses primary expressions (literals, identifiers, grouped)
// and potentially prefix operators later (like '-' for negation).
func (p *Parser) parsePrefix() Expression {
	switch p.curTok.Type {
	case TokenInt:
		return p.parseIntegerLiteral()
	case TokenString:
		return p.parseStringLiteral()
	case TokenIdent:
		return p.parseIdentifier()
	case TokenLParen:
		return p.parseGroupedExpression()
	// TODO: Add prefix operators like TokenMinus for negation later
	// case TokenMinus:
	//  return p.parsePrefixExpression()
	default:
		p.addError("Syntax Error: Unexpected token '%s' (%s) when expecting the start of an expression", p.curTok.Literal, p.curTok.Type)
		return nil
	}
}

// parseInfixExpression: Creates a BinaryExpression node.
// Assumes curTok is the operator, leftExpr is the already-parsed left side.
func (p *Parser) parseInfixExpression(left Expression) Expression {
	opToken := p.curTok // Store the operator token
	operator := p.curTok.Literal
	currentPrecedence := p.curPrecedence() // Get precedence of the current operator

	p.nextToken() // Consume the operator, curTok is now start of right operand

	// Parse the right operand, ensuring it binds operators with higher precedence first
	right := p.parseExpression(currentPrecedence)
	if right == nil {
		p.addError("Syntax Error: Expected expression after operator '%s'", operator)
		return nil
	}

	// Semantic Check: Type check for arithmetic
	leftType := left.ResultType()
	rightType := right.ResultType()

	// Basic check: only int arithmetic for now
	if leftType != "int" || rightType != "int" {
		if !(leftType == "string" && rightType == "string" && operator == "+") { // Allow string + string later?
			p.addError("Semantic Error: Operator '%s' requires integer operands, got %s and %s", operator, leftType, rightType)
		} else if operator == "+" {
			p.addError("Semantic Error: String concatenation (+) not yet supported")
		}
		// Continue creating node for syntax checking purposes, ResultType will be "unknown"
	}

	// TODO: Check for division by zero?
	// if operator == "/" { ... }

	return &BinaryExpression{
		Token:    opToken,
		Left:     left,
		Operator: operator,
		Right:    right,
	}
}

// --- Prefix/Primary Parsers ---

func (p *Parser) parseIntegerLiteral() Expression {
	lit := &IntegerLiteral{Token: p.curTok}
	val, err := strconv.ParseInt(p.curTok.Literal, 10, 64)
	if err != nil {
		p.addError("Syntax Error: Could not parse integer literal '%s': %v", p.curTok.Literal, err)
		p.nextToken() // Consume bad token
		return nil
	}
	lit.Value = int(val)
	p.nextToken() // Consume the integer token
	return lit
}

func (p *Parser) parseStringLiteral() Expression {
	expr := &StringLiteral{Token: p.curTok, Value: p.curTok.Literal}
	p.nextToken() // Consume the string token
	return expr
}

func (p *Parser) parseIdentifier() Expression {
	varName := p.curTok.Literal
	symbolInfo, declared := p.getSymbolInfo(varName)
	if !declared {
		p.addError("Semantic Error: Identifier '%s' used before declaration", varName)
		expr := &Identifier{Token: p.curTok, Value: varName, ResolvedType: "unknown"}
		p.nextToken() // Consume identifier
		return expr
	}
	expr := &Identifier{Token: p.curTok, Value: varName, ResolvedType: symbolInfo.Type}
	p.nextToken() // Consume identifier
	return expr
}

func (p *Parser) parseGroupedExpression() Expression {
	p.nextToken() // Consume '('

	// Recursively call parseExpression with lowest precedence to handle the inner part
	expr := p.parseExpression(LOWEST)
	if expr == nil {
		// Error already reported by the inner call
		return nil
	}

	// After parsing the inner expression, expect ')'
	if p.curTok.Type != TokenRParen { // Check CURRENT token
		p.addError("Syntax Error: Expected ')' after expression in parentheses, got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		// If not ')', structure is broken, don't consume
		return nil
	}
	p.nextToken() // Consume ')'

	// No need for GroupedExpression node, just return inner expression's AST
	return expr
}

// --- Precedence Helpers ---

// Define precedences for operators
var precedences = map[TokenType]int{
	TokenAssign:   LOWEST, // Or a separate ASSIGNMENT precedence level
	TokenPlus:     SUM,
	TokenMinus:    SUM,
	TokenSlash:    PRODUCT,
	TokenAsterisk: PRODUCT,
	// Add others like TokenLParen for call expressions later
}

// peekPrecedence returns the precedence of the next token (peekTok)
func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekTok.Type]; ok {
		return p
	}
	return LOWEST // Default lowest precedence for non-operators
}

// curPrecedence returns the precedence of the current token (curTok)
func (p *Parser) curPrecedence() int {
	if p, ok := precedences[p.curTok.Type]; ok {
		return p
	}
	return LOWEST
}
