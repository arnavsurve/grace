package compiler

import (
	"fmt"
	"strconv"
)

type Parser struct {
	l           *Lexer
	curTok      Token
	peekTok     Token
	errors      []string
	symbolTable map[string]SymbolInfo
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
}

func (p *Parser) addError(format string, args ...any) {
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
		SymbolTable: p.symbolTable,
	}

	for p.curTok.Type != TokenEOF {
		stmt := p.parseStatement()
		if stmt == nil {
			// If parsing failed, advance token to avoid infinite loop on bad input
			// But only if we didn't hit EOF trying to parse
			if p.curTok.Type != TokenEOF {
				p.nextToken()
			}
			continue // Skip adding nil statement
		}
		program.Statements = append(program.Statements, stmt)
		// NOTE: Do not call nextToken() here. The parsing functions
		// should consume exactly the tokens for their statement.
		// The next loop iteration will start with curTok being the
		// first token *after* the successfully parsed statement.
	}

	return program
}

// Expects curTok to be the start of the statement (PRINT, CONST, IDENT)
// Should consume tokens leaving curTok as the first token *after* the statement
func (p *Parser) parseStatement() Statement {
	switch p.curTok.Type {
	case TokenPrint:
		return p.parsePrintStatement()
	case TokenConst:
		return p.parseDeclarationStatement() // Let it handle the const logic internally
	case TokenIdent:
		// Look ahead to decide if declaration or reassignment
		switch p.peekTok.Type {
		case TokenAssignDefine: // :=
			return p.parseDeclarationStatement()
		case TokenAssign: // =
			return p.parseReassignmentStatement()
		default:
			p.addError("Expected ':=' or '=' after identifier '%s', got '%s'", p.curTok.Literal, p.peekTok.Literal)
			return nil // Error, don't advance token here, let main loop handle advance
		}
	default:
		if p.curTok.Type != TokenEOF {
			p.addError("Unexpected token at start of statement: %s (%s)", p.curTok.Type, p.curTok.Literal)
		}
		return nil // Error or EOF, don't advance
	}
}

// Expects curTok == CONST or IDENT (if not const)
// Consumes all tokens for the declaration statement.
func (p *Parser) parseDeclarationStatement() Statement { // Return Statement interface
	stmt := &DeclarationStatement{}

	// Handle optional 'const'
	if p.curTok.Type == TokenConst {
		stmt.IsConst = true
		p.nextToken() // Consume 'const'
		if p.curTok.Type != TokenIdent {
			p.addError("Expected identifier after 'const', got %s", p.curTok.Type)
			return nil
		}
	}

	// Now curTok must be IDENT
	if p.curTok.Type != TokenIdent {
		// This should only happen if 'const' wasn't followed by IDENT
		p.addError("Internal error: Expected identifier for declaration, got %s", p.curTok.Type)
		return nil
	}
	stmt.Name = &Identifier{Token: p.curTok, Value: p.curTok.Literal}
	varName := stmt.Name.Value

	// Check for redeclaration
	if _, exists := p.getSymbolInfo(varName); exists {
		p.addError("Variable '%s' already declared", varName)
		// Don't return nil yet, try to parse rest to find more errors potentially
	}

	// Expect :=
	if p.peekTok.Type != TokenAssignDefine {
		p.addError("Expected ':=' after identifier '%s' in declaration, got '%s'", varName, p.peekTok.Literal)
		return nil // Cannot proceed usefully
	}
	p.nextToken()         // Consume IDENT
	stmt.Token = p.curTok // Store := token
	p.nextToken()         // Consume :=, curTok is now expression start

	// Parse expression
	expr := p.parseExpression() // parseExpression should NOT consume the token
	if expr == nil {
		p.addError("Expected expression after ':=' for variable '%s', got %s", varName, p.curTok.Literal)
		return nil // Cannot proceed
	}
	stmt.Value = expr

	// Determine type from expression
	var valueType string
	switch expr.(type) {
	case *StringLiteral:
		valueType = "string"
	case *IntegerLiteral:
		valueType = "int"
	case *Identifier: // Handle assignment from variable
		identValue := expr.(*Identifier).Value
		info, declared := p.getSymbolInfo(identValue)
		if !declared {
			p.addError("Variable '%s' used in assignment before declaration", identValue)
			valueType = "unknown"
		} else {
			valueType = info.Type // Infer type from source variable
		}
	default:
		p.addError("Unsupported expression type in declaration for variable '%s'", varName)
		valueType = "unknown"
	}

	// Add to symbol table if not redeclared and type is known
	if _, exists := p.getSymbolInfo(varName); !exists && valueType != "unknown" {
		p.symbolTable[varName] = SymbolInfo{Type: valueType, IsConst: stmt.IsConst}
		// Type check if assigning from another variable
		if srcIdent, ok := expr.(*Identifier); ok {
			srcInfo, _ := p.getSymbolInfo(srcIdent.Value) // Already checked declaration
			if srcInfo.Type != valueType && valueType != "unknown" {
				p.addError("Type mismatch in declaration: Cannot assign %s to variable '%s' of type %s", srcInfo.Type, varName, valueType)
				// Potentially invalidate symbol table entry? For now, just error.
			}
		}
	}

	// Consume the expression token now that we're done with it
	p.nextToken()

	return stmt
}

// Expects curTok == IDENT
// Consumes all tokens for the reassignment statement.
func (p *Parser) parseReassignmentStatement() Statement { // Return Statement interface
	stmt := &ReassignmentStatement{}

	// Current token is IDENT
	if p.curTok.Type != TokenIdent {
		p.addError("Internal error: Expected identifier for reassignment, got %s", p.curTok.Type)
		return nil
	}
	stmt.Name = &Identifier{Token: p.curTok, Value: p.curTok.Literal}
	varName := stmt.Name.Value

	// Semantic Check 1: Is declared?
	symbolInfo, declared := p.getSymbolInfo(varName)
	if !declared {
		p.addError("Cannot assign to undeclared variable '%s'", varName)
		symbolInfo = SymbolInfo{Type: "undeclared"} // Mark to skip further checks
	} else {
		// Semantic Check 2: Is const?
		if symbolInfo.IsConst {
			p.addError("Cannot assign to constant variable '%s'", varName)
		}
	}

	// Expect =
	if p.peekTok.Type != TokenAssign {
		p.addError("Expected '=' after identifier '%s' in assignment, got '%s'", varName, p.peekTok.Literal)
		return nil // Cannot proceed
	}
	p.nextToken()         // Consume IDENT
	stmt.Token = p.curTok // Store = token
	p.nextToken()         // Consume =, curTok is now expression start

	// Parse Expression
	expr := p.parseExpression() // parseExpression should NOT consume token
	if expr == nil {
		p.addError("Expected expression after '=' for variable '%s', got %s", varName, p.curTok.Literal)
		return nil
	}
	stmt.Value = expr

	// Semantic Check 3: Type compatibility (only if declared and not const)
	if declared && !symbolInfo.IsConst {
		var valueType string
		switch expr.(type) {
		case *StringLiteral:
			valueType = "string"
		case *IntegerLiteral:
			valueType = "int"
		case *Identifier: // Handle assignment from variable
			identValue := expr.(*Identifier).Value
			info, srcDeclared := p.getSymbolInfo(identValue)
			if !srcDeclared {
				p.addError("Variable '%s' used in assignment before declaration", identValue)
				valueType = "unknown"
			} else {
				valueType = info.Type
			}
		default:
			p.addError("Unsupported expression type in assignment for variable '%s'", varName)
			valueType = "unknown"
		}

		if valueType != "unknown" && symbolInfo.Type != valueType {
			p.addError("Type mismatch: Cannot assign value of type %s to variable '%s' (type %s)", valueType, varName, symbolInfo.Type)
		}
	}

	// Consume the expression token
	p.nextToken()

	return stmt
}

// Expects curTok == PRINT
// Consumes all tokens for the print statement.
func (p *Parser) parsePrintStatement() Statement {
	stmt := &PrintStatement{Token: p.curTok}

	// Expect (
	if p.peekTok.Type != TokenLParen {
		p.addError("Expected '(' after 'print', got %s", p.peekTok.Literal)
		return nil
	}
	p.nextToken() // Consume PRINT
	p.nextToken() // Consume ( , curTok is now expression start

	// Parse expression
	expr := p.parseExpression() // parseExpression should NOT consume token
	if expr == nil {
		p.addError("Expected expression inside print(), got %s", p.curTok.Type)
		return nil
	}
	stmt.Value = expr

	// Check if identifier is declared
	if ident, ok := expr.(*Identifier); ok {
		if _, declared := p.getSymbolInfo(ident.Value); !declared {
			p.addError("Variable '%s' used before declaration in print", ident.Value)
			// Don't return nil, just log error
		}
	}

	p.nextToken() // Consume expression token

	// Expect )
	if p.curTok.Type != TokenRParen {
		p.addError("Expected ')' after print expression, got %s (%s)", p.curTok.Type, p.curTok.Literal)
		return nil
	}
	p.nextToken() // Consume )

	return stmt
}

// Parses an expression based on the *current* token.
// Does NOT advance the token stream (nextToken is not called).
// The caller is responsible for consuming the expression token.
func (p *Parser) parseExpression() Expression {
	switch p.curTok.Type {
	case TokenString:
		return &StringLiteral{Token: p.curTok, Value: p.curTok.Literal}
	case TokenInt:
		val, err := strconv.ParseInt(p.curTok.Literal, 10, 64)
		if err != nil {
			p.addError("Could not parse integer literal: %s", p.curTok.Literal)
			return nil
		}
		return &IntegerLiteral{Token: p.curTok, Value: int(val)}
	case TokenIdent:
		// Just return the identifier node. Declaration/type checks happen in the calling context (assignment/print).
		return &Identifier{Token: p.curTok, Value: p.curTok.Literal}
	default:
		// Don't add error here, caller will report based on context
		return nil
	}
}
