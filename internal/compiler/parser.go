package compiler

import (
	"fmt"
	"strconv"
)

type Parser struct {
	l       *Lexer
	curTok  Token
	peekTok Token
	errors  []string

	symbolTable map[string]string // varName -> type ("string" or "int")
}

func NewParser(l *Lexer) *Parser {
	p := &Parser{
		l:           l,
		symbolTable: make(map[string]string),
	}
	p.nextToken()
	p.nextToken()
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

func (p *Parser) SymbolTable() map[string]string {
	return p.symbolTable
}

func (p *Parser) isDeclared(varName string) bool {
	_, exists := p.symbolTable[varName]
	return exists
}

func (p *Parser) ParseProgram() *Program {
	program := &Program{
		Statements:  []Statement{},
		SymbolTable: p.symbolTable,
	}

	for p.curTok.Type != TokenEOF {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}

	return program
}

func (p *Parser) parseStatement() Statement {
	switch p.curTok.Type {
	case TokenPrint:
		return p.parsePrintStatement()
	case TokenIdent:
		if p.peekTok.Type == TokenAssignDefine {
			return p.parseAssignmentStatement()
		}
	default:
		return nil
	}
	return nil
}

func (p *Parser) parsePrintStatement() *PrintStatement {
	stmt := &PrintStatement{Token: p.curTok} // TokenPrint

	// Expect (
	p.nextToken()
	if p.curTok.Type != TokenLParen {
		return nil
	}

	// Expect expression (string literal, int, or identifier)
	p.nextToken()
	switch p.curTok.Type {
	case TokenString:
		stmt.Value = &StringLiteral{Token: p.curTok, Value: p.curTok.Literal}

	case TokenIdent:
		varName := p.curTok.Literal
		if !p.isDeclared(varName) {
			p.addError("Variable '%s' used before declaration", varName)
			return nil
		}
		stmt.Value = &Identifier{Token: p.curTok, Value: p.curTok.Literal}

	case TokenInt:
		val, err := strconv.ParseInt(p.curTok.Literal, 10, 64)
		if err != nil {
			p.addError("Unexpected token type: %s", p.curTok.Type)
			return nil
		}
		stmt.Value = &IntegerLiteral{Token: p.curTok, Value: int(val)}

	default:
		p.addError("Unexpected token type: %s", p.curTok.Type)
		return nil
	}

	// Expect )
	p.nextToken()
	if p.curTok.Type != TokenRParen {
		return nil
	}

	return stmt
}

func (p *Parser) parseAssignmentStatement() *AssignmentStatement {
	stmt := &AssignmentStatement{}

	// Identifier name
	stmt.Name = &Identifier{Token: p.curTok, Value: p.curTok.Literal}

	varName := stmt.Name.Value

	// Expect :=
	p.nextToken()
	if p.curTok.Type != TokenAssignDefine {
		return nil
	}

	stmt.Token = p.curTok // :=

	// Expect expression (currently only string literals supported)
	p.nextToken()

	switch p.curTok.Type {
	case TokenString:
		stmt.Value = &StringLiteral{Token: p.curTok, Value: p.curTok.Literal}

		if prevType, exists := p.symbolTable[varName]; exists && prevType != "string" {
			p.addError("Type error: Cannot assign string to variable '%s' (already declared as %s)", varName, prevType)
			return nil
		}
		p.symbolTable[varName] = "string"

	case TokenInt:
		val, err := strconv.ParseInt(p.curTok.Literal, 10, 64)
		if err != nil {
			return nil
		}
		stmt.Value = &IntegerLiteral{Token: p.curTok, Value: int(val)}

		if prevType, exists := p.symbolTable[varName]; exists && prevType != "int" {
			p.addError("Type error: Cannot assign string to variable '%s' (already declared as %s)", varName, prevType)
			return nil
		}
		p.symbolTable[varName] = "int"
	}

	return stmt
}
