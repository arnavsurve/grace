package compiler

import (
	"fmt"
	"strconv"

	"github.com/arnavsurve/grace/internal/compiler/lib"
)

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

// --- Program Parsing ---

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
			// Basic error recovery attempt: If parseStatement failed,
			// skip the token that caused the error to avoid infinite loop.
			// A better recovery would try to find the start of the next statement.
			// fmt.Printf("Recovery: Skipping token %v\n", p.curTok) // Debugging
			// p.nextToken() // Let's rely on statement parsers consuming on error for now
		}
		// The statement parsing functions MUST consume tokens to make progress
		// or return nil only at EOF or after consuming the problematic token.
	}

	return program
}

// --- Statement Parsing ---

func (p *Parser) parseStatement() Statement {
	switch p.curTok.Type {
	case TokenPrint:
		return p.parsePrintStatement()
	case TokenConst:
		return p.parseDeclarationStatement()
	case TokenIdent:
		switch p.peekTok.Type {
		case TokenAssignDefine:
			return p.parseDeclarationStatement()
		case TokenAssign:
			return p.parseReassignmentStatement()
		default:
			p.addError("Syntax Error: Expected ':=' or '=' after identifier '%s', got '%s'", p.curTok.Literal, p.peekTok.Literal)
			// Attempt recovery: Skip the identifier and the unexpected token and consume tokens that caused the error.
			p.nextToken() // Consume identifier
			if p.curTok.Type != TokenEOF {
				p.nextToken()
			} // Consume the bad peek token
			return nil
		}
	case TokenEOF:
		return nil
	default:
		p.addError("Syntax Error: Unexpected token at start of statement: %s ('%s')", p.curTok.Type, p.curTok.Literal)
		// Attempt recovery: consume the unexpected token
		if p.curTok.Type != TokenEOF {
			p.nextToken()
		}
		return nil
	}
}

func (p *Parser) parseDeclarationStatement() Statement {
	stmt := &DeclarationStatement{}

	// Handle optional 'const'
	if p.curTok.Type == TokenConst {
		stmt.IsConst = true
		p.nextToken() // Consume 'const'
		if p.curTok.Type != TokenIdent {
			p.addError("Syntax Error: Expected identifier after 'const', got %s", p.curTok.Type)
			if p.curTok.Type != TokenEOF {
				p.nextToken()
			}
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
	}

	// Expect :=
	if p.peekTok.Type != TokenAssignDefine {
		// TODO: Handle specific error for const x = ... later if needed
		p.addError("Syntax Error: Expected ':=' after identifier '%s' in declaration, got '%s'", varName, p.peekTok.Literal)
		p.nextToken() // Consume IDENT
		if p.curTok.Type != TokenEOF {
			p.nextToken()
		} // Consume bad peek token
		return nil
	}
	p.nextToken()         // Consume IDENT
	stmt.Token = p.curTok // Store := token
	p.nextToken()         // Consume :=, curTok is now start of expression

	expr := p.parseExpression()
	if expr == nil {
		// Error added by expression parser
		return nil
	}
	stmt.Value = expr

	// --- Semantic Analysis ---
	valueType := expr.ResultType()
	if valueType == "unknown" && len(p.errors) > 0 { // Check if errors already exist
		// Error likely reported during expression parsing
	} else if valueType == "unknown" {
		// If no previous errors, the type resolution failed silently?
		p.addError("Semantic Error: Could not determine type for expression assigned to '%s'", varName)
	}

	var varWidth int

	// Include variable width in symbol table
	// TODO: implement explicit width definition in explicit typing, for now we just implement the fallback default widths
	switch val := stmt.Value.(type) {
	case *IntegerLiteral:
		literalWidth := lib.CalculateWidthForValue(val.Value)
		varWidth = max(lib.DefaultIntWidth, literalWidth)
	case *StringLiteral:
		// TODO: similar to integer, take the max of the default string width versus the actual len of the declared str
		// For now we just use the default
		varWidth = lib.DefaultStringWidth
	case *Identifier:
		// Look up identifier in symbol table
		if info, ok := p.symbolTable[val.Value]; ok {
			varWidth = info.Width
		} else {
			p.addError("Internal Error: Could not determine width of identifier '%s'", val.Value)
			varWidth = 0 // fallback or skip insertion
		}
	case *BinaryExpression:
		varWidth = val.ResultWidth()
	default:
		p.addError("Internal Error: Could not determine width of unknown identifier or literal.")
	}

	// Add to symbol table only if not previously declared and type is known
	if !declared && valueType != "unknown" {
		// Ensure width is at least 1 if calculation somehow resulted in 0 (except for error cases -> !"unknown")
		if varWidth <= 0 && valueType != "unknown" {
			p.addError("Internal Warning: Calculated width was <= 0 for '%s', using 1.", varName)
			varWidth = 1 // By doing this, we ensure a valid PIC clause
		}
		p.symbolTable[varName] = SymbolInfo{Type: valueType, IsConst: stmt.IsConst, Width: varWidth}

		if stmt.Name != nil {
			stmt.Name.ResolvedType = valueType
		}
		stmt.Name.Width = varWidth
	}

	// IMPORTANT: parseExpression consumes all its tokens.
	// curTok should be the token after the expression. No nextToken() needed here.
	return stmt
}

func (p *Parser) parseReassignmentStatement() Statement {
	stmt := &ReassignmentStatement{}

	// Current token is IDENT
	if p.curTok.Type != TokenIdent { // Error
		return nil
	}
	stmt.Name = &Identifier{Token: p.curTok, Value: p.curTok.Literal}
	varName := stmt.Name.Value

	// Semantic Check 1 & 2: Is declared? Is const?
	symbolInfo, declared := p.getSymbolInfo(varName)
	isConst := false
	if !declared {
		p.addError("Semantic Error: Cannot assign to undeclared variable '%s'", varName)
		symbolInfo = SymbolInfo{Type: "undeclared"}
	} else {
		if symbolInfo.IsConst {
			p.addError("Semantic Error: Cannot assign to constant variable '%s'", varName)
			isConst = true
		}
		stmt.Name.ResolvedType = symbolInfo.Type
	}

	// Expect =
	if p.peekTok.Type != TokenAssign {
		p.addError("Syntax Error: Expected '=' after identifier '%s' in assignment, got '%s'", varName, p.peekTok.Literal)
		p.nextToken() // Consume IDENT
		if p.curTok.Type != TokenEOF {
			p.nextToken()
		} // Consume bad peek token
		return nil
	}
	p.nextToken()         // Consume IDENT
	stmt.Token = p.curTok // Store = token
	p.nextToken()         // Consume =, curTok is now expression start

	expr := p.parseExpression()
	if expr == nil {
		return nil
	}
	stmt.Value = expr

	// --- Semantic Check 3: Type compatibility ---
	if declared && !isConst {
		valueType := expr.ResultType()
		if valueType != "unknown" && symbolInfo.Type != valueType {
			p.addError("Semantic Error: Type mismatch - cannot assign value '%s' (type %s) to variable %s (type %s)", expr.TokenLiteral(), valueType, varName, symbolInfo.Type)
		}
	}

	// IMPORTANT: parseExpression consumes all its tokens. No nextToken() needed.
	return stmt
}

func (p *Parser) parsePrintStatement() Statement {
	stmt := &PrintStatement{Token: p.curTok} // Store PRINT token

	// Expect (
	if p.peekTok.Type != TokenLParen {
		p.addError("Syntax Error: Expected '(' after 'print', got %s", p.peekTok.Literal)
		p.nextToken() // Consume PRINT
		if p.curTok.Type != TokenEOF {
			p.nextToken()
		} // Consume bad peek token
		return nil
	}
	p.nextToken() // Consume PRINT
	p.nextToken() // Consume (, curTok is now expression start

	// --- Call the expression parser ---
	expr := p.parseExpression()
	if expr == nil {
		return nil
	}
	stmt.Value = expr

	// Semantic check: Ensure expression type is printable
	valueType := expr.ResultType()
	if valueType == "unknown" { /* Error reported */
	} else if valueType != "int" && valueType != "string" {
		p.addError("Semantic Error: Cannot print value of type %s", valueType)
	}

	// --- Expect and Consume ')' ---
	// After parseExpression returns, curTok should be the token *after* the expression.
	if p.curTok.Type != TokenRParen {
		p.addError("Syntax Error: Expected ')' after print expression, got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		// Don't consume if it's not ')', the structure is broken.
		return nil
	}
	p.nextToken() // Consume the ')'

	return stmt
}

// --- Expression Parsing (Recursive Descent) ---

// parseExpression: Handles lowest precedence (addition, subtraction)
func (p *Parser) parseExpression() Expression {
	leftExpr := p.parseTerm() // Parse the left operand (must be a term)
	if leftExpr == nil {
		return nil
	}

	// Loop while the current token is + or -
	for p.curTok.Type == TokenPlus || p.curTok.Type == TokenMinus {
		opToken := p.curTok
		operator := p.curTok.Literal

		p.nextToken() // Consume the operator (+ or -)

		rightExpr := p.parseTerm() // Parse the right operand (must be a term)
		if rightExpr == nil {
			p.addError("Syntax Error: Expected expression term after operator '%s'", operator)
			return nil // Can't build node without right side
		}

		// --- Semantic Check ---
		leftType := leftExpr.ResultType()
		rightType := rightExpr.ResultType()
		if leftType != "int" || rightType != "int" {
			p.addError("Semantic Error: Operator '%s' requires integer operands, got %s and %s", operator, leftType, rightType)
			// Don't return nil here, allow AST build for further syntax checks,
			// but ResultType will be "unknown"
		}

		// --- CONSTANT FOLDING DURING PARSING ---
		leftLit, leftIsLit := leftExpr.(*IntegerLiteral)
		rightLit, rightIsLit := rightExpr.(*IntegerLiteral)

		if leftIsLit && rightIsLit {
			// Both operands are literals, calculate the exact result width
			leftVal := leftLit.Value
			rightVal := rightLit.Value
			var resultVal int
			calculated := true // Flag to indicate successful calculation

			switch operator {
			case "+":
				resultVal = leftVal + rightVal
			case "-":
				resultVal = leftVal - rightVal // Result could be negative? PIC 9 handles unsigned
				// NOTE: multiplication/division folding happens in parseTerm()
			default:
				calculated = false
			}

			if calculated {
				resultWidth := lib.CalculateWidthForValue(resultVal)
				leftExpr = &IntegerLiteral{
					Token: opToken,
					Value: resultVal,
					Width: resultWidth,
				}
				// Continue loop with the folded literal as the new left expression
				continue
			}
		}

		// Build the BinaryExpression node
		leftExpr = &BinaryExpression{
			Token:    opToken,
			Left:     leftExpr,
			Operator: operator,
			Right:    rightExpr,
		}
		// Loop continues, checking the new curTok
	}
	// When loop finishes, curTok is the token after the last term.
	return leftExpr
}

// parseTerm: Handles multiplication and division.
func (p *Parser) parseTerm() Expression {
	leftExpr := p.parsePrimary() // Parse the left operand (must be a primary)
	if leftExpr == nil {
		return nil
	}

	// Loop while the *current* token is * or /
	for p.curTok.Type == TokenAsterisk || p.curTok.Type == TokenSlash {
		opToken := p.curTok
		operator := p.curTok.Literal

		p.nextToken() // Consume the operator (* or /)

		rightExpr := p.parsePrimary() // Parse the right operand (must be a primary)
		if rightExpr == nil {
			p.addError("Syntax Error: Expected expression primary after operator '%s'", operator)
			return nil
		}

		// --- Semantic Check: Type ---
		leftType := leftExpr.ResultType()
		rightType := rightExpr.ResultType()
		if leftType != "int" || rightType != "int" {
			p.addError("Semantic Error: Operator '%s' requires integer operands, got %s and %s", operator, leftType, rightType)
			// Continue building node for further syntax checks
		}

		// --- Semantic Check: Division by Literal Zero ---
		if operator == "/" {
			if lit, ok := rightExpr.(*IntegerLiteral); ok {
				if lit.Value == 0 {
					p.addError("Semantic Error: Division by literal zero")
				}
			}
		}

		// --- CONSTANT FOLDING DURING PARSING ---
		leftLit, leftIsLit := leftExpr.(*IntegerLiteral)
		rightLit, rightIsLit := rightExpr.(*IntegerLiteral)

		if leftIsLit && rightIsLit {
			// Both operands are literals, calculate the exact result width
			leftVal := leftLit.Value
			rightVal := rightLit.Value
			var resultVal int
			calculated := true // Flag to indicate successful calculation
			isDivZero := false

			switch operator {
			case "*":
				resultVal = leftVal * rightVal
			case "/":
				if rightVal == 0 {
					// Error already added by prior divide by zero check
					calculated = false
					isDivZero = true
				} else {
					resultVal = leftVal / rightVal
				}
			default:
				calculated = false
			}

			if calculated {
				resultWidth := lib.CalculateWidthForValue(resultVal)
				// Need to synthesize a token or use operator token? Use operator tok for now
				leftExpr = &IntegerLiteral{
					Token: opToken,
					Value: resultVal,
					Width: resultWidth,
				}
				// Continue loop with the folded literal as the new left expression
				continue
			} else if isDivZero {
				// TODO:
				// If div by zero, return nil or an error node?
				// For now, we just fall through
			}
		}

		// Build the BinaryExpression node
		leftExpr = &BinaryExpression{
			Token:    opToken,
			Left:     leftExpr,
			Operator: operator,
			Right:    rightExpr,
		}
		// Loop continues, checking the new curTok
	}
	// When loop finishes, curTok is the token *after* the last primary.
	return leftExpr
}

// parsePrimary: Handles highest precedence items.
func (p *Parser) parsePrimary() Expression {
	switch p.curTok.Type {
	case TokenInt:
		return p.parseIntegerLiteral() // Consumes token
	case TokenString:
		return p.parseStringLiteral() // Consumes token
	case TokenIdent:
		return p.parseIdentifier() // Consumes token
	case TokenLParen:
		return p.parseGroupedExpression() // Consumes tokens including ')'
	default:
		p.addError("Syntax Error: Unexpected token '%s' (%s) when expecting an expression component (number, variable, string, or '(')", p.curTok.Literal, p.curTok.Type)
		// Do NOT consume token here, let the caller decide recovery.
		return nil
	}
}

// --- Primary/Helper Parsers (Consume their own tokens) ---

func (p *Parser) parseIntegerLiteral() Expression {
	// Same as before, ensure p.nextToken() is called
	// We initialize default width here, TODO implement explicit width
	lit := &IntegerLiteral{Token: p.curTok, Width: lib.DefaultIntWidth}
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
	// Same as before, ensure p.nextToken() is called
	// We initialize default width here, TODO implement explicit width
	expr := &StringLiteral{Token: p.curTok, Value: p.curTok.Literal, Width: lib.DefaultStringWidth}
	p.nextToken() // Consume the string token
	return expr
}

func (p *Parser) parseIdentifier() Expression {
	// Same as before, ensure p.nextToken() is called
	varName := p.curTok.Literal
	symbolInfo, declared := p.getSymbolInfo(varName)
	expr := &Identifier{Token: p.curTok, Value: varName}
	if !declared {
		p.addError("Semantic Error: Identifier '%s' used before declaration", varName)
		expr.ResolvedType = "unknown"
		expr.Width = 0
	} else {
		// Populate both type and width from symbol table
		expr.ResolvedType = symbolInfo.Type
		expr.Width = symbolInfo.Width
	}
	p.nextToken() // Consume identifier
	return expr
}

func (p *Parser) parseGroupedExpression() Expression {
	// Uses the new top-level parseExpression
	p.nextToken()               // Consume '('
	expr := p.parseExpression() // Call the new base expression parser
	if expr == nil {
		// Error should have been reported.
		// Need to decide if we try to consume the ')' here for recovery.
		// If curTok is already ')', consume it?
		if p.curTok.Type == TokenRParen {
			p.nextToken()
		}
		return nil
	}
	// Expect ')' as the *current* token now
	if p.curTok.Type != TokenRParen {
		p.addError("Syntax Error: Expected ')' after expression in parentheses, got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		// Don't consume if not ')'
		return nil
	}
	p.nextToken() // Consume ')'
	return expr   // Return inner expression's AST
}
