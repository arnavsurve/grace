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
		case TokenAssignDefine, TokenColon:
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

	// 1. Handle optional 'const'
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

	// 2. Expect IDENT
	if p.curTok.Type != TokenIdent {
		if !stmt.IsConst {
			p.addError("Internal Parser Error: Expected identifier for declaration start, got %s", p.curTok.Type)
		}
		if p.curTok.Type != TokenEOF {
			p.nextToken()
		}
		return nil
	}

	identToken := p.curTok
	varName := identToken.Literal
	stmt.Name = &Identifier{Token: identToken, Value: varName}

	// 3. Check for Redeclaration (early check)
	_, declared := p.getSymbolInfo(varName)
	if declared {
		p.addError("Semantic Error: Variable '%s' already declared", varName)
		// Continue parsing to find more errors, but don't add to symbol table later
	}

	// 4. Check for ':=' (Inferred) or ':' (Explicit) and consume tokens up to expression start
	var explicitType string = ""
	explicitWidth := 0
	var useExplicitType bool = false

	// We look at peekTok to decide the path
	switch p.peekTok.Type {
	case TokenColon:
		// --- Explicit Type Path ---
		useExplicitType = true
		p.nextToken() // Consume IDENT
		p.nextToken() // Consume ':'

		// Expect Type Literal
		if p.curTok.Type != TokenTypeLiteral {
			p.addError("Syntax Error: Expected type (e.g., 'int', 'string') after ':', got %s ('%s')", p.curTok.Type, p.curTok.Literal)
			if p.curTok.Type != TokenEOF {
				p.nextToken()
			} // Consume bad token
			return nil
		}
		explicitType = p.curTok.Literal
		// Validate if type is known
		if explicitType != "int" && explicitType != "string" {
			p.addError("Syntax Error: Unknown type '%s'", explicitType)
			// Allow parsing to continue to find more errors, but type will be marked unknown later
		}
		p.nextToken() // Consume Type Literal

		// Optionally check for width: '(' INT ')'
		if p.curTok.Type == TokenLParen {
			p.nextToken() // Consume '('
			if p.curTok.Type != TokenInt {
				p.addError("Syntax Error: Expected integer width inside parentheses after type '%s', got %s ('%s')", explicitType, p.curTok.Type, p.curTok.Literal)
				if p.curTok.Type != TokenEOF {
					p.nextToken()
				} // Consume bad token
				// Try to recover if possible? Maybe look for ')'? For now, return nil.
				return nil
			}
			widthVal, err := strconv.Atoi(p.curTok.Literal)
			if err != nil || widthVal <= 0 {
				p.addError("Syntax Error: Invalid width specification '%s'. Width must be a positive integer.", p.curTok.Literal)
				widthVal = 0 // Mark as invalid, parser will use defaults later if needed
			}
			explicitWidth = widthVal
			p.nextToken() // Consume INT

			if p.curTok.Type != TokenRParen {
				p.addError("Syntax Error: Expected ')' after width specification, got %s ('%s')", p.curTok.Type, p.curTok.Literal)
				if p.curTok.Type != TokenEOF {
					p.nextToken()
				} // Consume bad token
				return nil
			}
			p.nextToken() // Consume ')'
		} // End optional width parsing

		// Expect '=' for assignment
		if p.curTok.Type != TokenAssign {
			p.addError("Syntax Error: Expected '=' after type specification for '%s', got %s ('%s')", varName, p.curTok.Type, p.curTok.Literal)
			if p.curTok.Type != TokenEOF {
				p.nextToken()
			} // Consume bad token
			return nil
		}
		stmt.Token = p.curTok // Store '=' token
		p.nextToken()         // Consume '=', curTok is now start of expression

	case TokenAssignDefine:
		// --- Inferred Type Path ---
		useExplicitType = false
		p.nextToken()         // Consume IDENT. curTok is now ':='
		stmt.Token = p.curTok // Store ':=' token
		p.nextToken()         // Consume ':='. curTok is now start of expression

	default:
		// Neither ':' nor ':=' found after IDENT
		p.addError("Syntax Error: Expected ':=' or ':' after identifier '%s' in declaration, got '%s' ('%s')", varName, p.peekTok.Type, p.peekTok.Literal)
		p.nextToken() // Consume IDENT
		if p.curTok.Type != TokenEOF {
			p.nextToken()
		} // Consume the unexpected peek token
		return nil
	}

	// 5. Parse the Expression (curTok is now the start of the expression)
	expr := p.parseExpression()
	if expr == nil {
		// Error added by expression parser. Don't proceed.
		return nil
	}
	stmt.Value = expr
	// After parseExpression, curTok is the token *after* the expression

	// --- 6. Semantic Analysis & Symbol Table ---
	valueType := expr.ResultType() // Get type from RHS expression
	var finalWidth int = 0
	var finalType string = "unknown" // Start assuming unknown

	if valueType == "unknown" && len(p.errors) == 0 {
		// If RHS expression parsing failed internally without adding an error
		p.addError("Internal Error: Expression resulted in 'unknown' type without specific error for '%s'", varName)
	}

	// Determine finalType and finalWidth based on explicit vs inferred declaration
	if useExplicitType {
		// --- Explicit Declaration Logic ---
		finalType = explicitType // Start with the explicitly declared type

		// Check if declared type is valid (might have errored earlier but we check again)
		if finalType != "int" && finalType != "string" {
			// Error already added for unknown type. Mark as unknown.
			finalType = "unknown"
		}

		// If the declared type is valid, check against the expression's type
		if finalType != "unknown" && valueType != "unknown" && finalType != valueType {
			// Allow int literal assign to float var later? For now, strict.
			p.addError("Semantic Error: Type mismatch - cannot assign value of type %s to variable '%s' declared as %s", valueType, varName, finalType)
			finalType = "unknown" // Mark as error state
		}

		// Determine Width
		if finalType != "unknown" { // Only proceed if type is valid
			if explicitWidth > 0 {
				// Explicit width was provided
				finalWidth = explicitWidth
				// Check if LITERAL value fits (only for literals)
				if litInt, ok := expr.(*IntegerLiteral); ok && finalType == "int" {
					reqWidth := lib.CalculateWidthForValue(litInt.Value)
					if reqWidth > finalWidth {
						p.addError("Semantic Error: Integer literal %d requires width %d, but variable '%s' declared with width %d", litInt.Value, reqWidth, varName, finalWidth)
					}
				} else if litStr, ok := expr.(*StringLiteral); ok && finalType == "string" {
					reqWidth := len(litStr.Value)
					if reqWidth > finalWidth {
						p.addError("Semantic Error: String literal with length %d exceeds declared width %d for variable '%s'", reqWidth, finalWidth, varName)
					}
				}
			} else {
				// Explicit type, but no explicit width - use default/inferred from expr
				reqWidth := expr.ResultWidth() // Width needed by the expression
				if finalType == "int" {
					finalWidth = max(lib.DefaultIntWidth, reqWidth)
				} else if finalType == "string" {
					finalWidth = max(lib.DefaultStringWidth, reqWidth)
				}
				// No else needed here, finalType is known int/string
			}
		} else {
			// finalType became unknown due to mismatch or bad explicit type
			finalWidth = 0
		}

	} else { // Not useExplicitType
		// --- Inferred Declaration Logic (`:=`) ---
		if valueType != "unknown" {
			finalType = valueType // Type is inferred from expression

			// Determine Width (Inferred)
			reqWidth := expr.ResultWidth()
			if finalType == "int" {
				finalWidth = max(lib.DefaultIntWidth, reqWidth)
			} else if finalType == "string" {
				finalWidth = max(lib.DefaultStringWidth, reqWidth)
			} else {
				// Should not happen if valueType != "unknown"
				p.addError("Internal Error: Cannot determine default width for inferred unknown type '%s'", finalType)
				finalType = "unknown" // Mark as error
				finalWidth = 0
			}
		} else {
			// valueType was unknown from expression parsing
			finalType = "unknown"
			finalWidth = 0
		}
	} // End of explicit vs inferred logic

	// --- 7. Final Validation and Symbol Table Update ---

	// Ensure width is valid before adding to symbol table
	if finalType != "unknown" && finalWidth <= 0 {
		p.addError("Internal Warning: Final width calculated as <= 0 for '%s' (type %s), setting to 1.", varName, finalType)
		finalWidth = 1
	}

	// Add to symbol table ONLY if it's not a redeclaration AND type is known
	if !declared && finalType != "unknown" {
		p.symbolTable[varName] = SymbolInfo{
			Type:    finalType,
			IsConst: stmt.IsConst,
			Width:   finalWidth,
		}
		// Also update the identifier node in the AST
		stmt.Name.ResolvedType = finalType
		stmt.Name.Width = finalWidth
	} else {
		// Handle cases where symbol wasn't added:
		// - Redeclaration (error added earlier)
		// - Error occurred determining type/width (error added earlier)
		// Update AST node to reflect the (potentially invalid) outcome
		stmt.Name.ResolvedType = finalType
		stmt.Name.Width = finalWidth
	}

	// parseExpression consumed tokens including the last part of the expression.
	// The current token (p.curTok) should now be whatever follows the expression
	// (e.g., EOF, or the start of the next statement). No nextToken() needed here.
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
	leftExpr := p.parseTerm() // Parse the left operand (must be a term - multiplication, division)
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

		// Check for valid operations
		isValidOp := false
		if operator == "+" {
			// '+' supports int+int OR string+string
			if (leftType == "int" && rightType == "int") || (leftType == "string" && rightType == "string") {
				isValidOp = true
			}
		} else if operator == "-" {
			// '-' only supports
			if leftType == "int" && rightType == "int" {
				isValidOp = true
			}
		} // Other operators (*, /) are handled in parseTerm

		if !isValidOp {
			// Construct error message
			errMsg := fmt.Sprintf("Semantic Error: Operator '%s' cannot be applied to types %s and %s", operator, leftType, rightType)
			if (leftType == "string" || rightType == "string") && operator != "+" {
				errMsg += fmt.Sprintf(" (Operator '%s' not defined for strings)", operator)
			} else if leftType != rightType {
				errMsg += " (Type mismatch)"
			} else if leftType != "int" && leftType != "string" {
				errMsg += " (Unsupported types for operation)"
			}
			p.addError(errMsg)
			// Don't return nil, let ResultType become "unknown"
		}

		// --- CONSTANT FOLDING DURING PARSING ---

		// --- Integer constant folding ---
		if leftType == "int" && rightType == "int" {
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
					// Skip BinaryExpression creation and
					// continue loop with the folded literal as the new left expression
					continue
				}
			}
		}

		// --- String constant folding ---
		if leftType == "string" && rightType == "string" {
			leftLit, leftIsLit := leftExpr.(*StringLiteral)
			rightLit, rightIsLit := rightExpr.(*StringLiteral)

			if leftIsLit && rightIsLit {
				resultVal := leftLit.Value + rightLit.Value
				resultWidth := max(lib.DefaultStringWidth, len(resultVal))

				leftExpr = &StringLiteral{
					Token: opToken,
					Value: resultVal,
					Width: resultWidth,
				}
				// Skip BinaryExpression creation and
				// continue loop with the folded literal as the new left expression
				continue
			}
		}

		// Build the BinaryExpression node if no folding occured
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
	// Store the initial token for potential use in the AST node
	token := p.curTok

	val, err := strconv.ParseInt(p.curTok.Literal, 10, 64)
	if err != nil {
		p.addError("Syntax Error: Could not parse integer literal '%s': %v", p.curTok.Literal, err)
		p.nextToken() // Consume bad token
		return nil
	}

	actualWidth := lib.CalculateWidthForValue(int(val))

	lit := &IntegerLiteral{
		Token: token,
		Value: int(val),
		Width: actualWidth,
	}

	p.nextToken() // Consume the integer token
	return lit
}

func (p *Parser) parseStringLiteral() Expression {
	// Store the token
	token := p.curTok
	value := p.curTok.Literal

	actualWidth := len(value)

	// Create the literal node
	expr := &StringLiteral{
		Token: token,
		Value: value,
		Width: actualWidth,
	}
	p.nextToken() // Consume the string token
	return expr
}

func (p *Parser) parseIdentifier() Expression {
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
