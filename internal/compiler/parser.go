package compiler

type Parser struct {
	l       *Lexer
	curTok  Token
	peekTok Token
}

func NewParser(l *Lexer) *Parser {
	p := &Parser{l: l}
	p.nextToken()
	p.nextToken()
	return p
}

func (p *Parser) nextToken() {
	p.curTok = p.peekTok
	p.peekTok = p.l.NextToken()
}

func (p *Parser) ParseProgram() *Program {
	program := &Program{}
	program.Statements = []Statement{}

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

	// Expect expression (string literal or identifier)
	p.nextToken()
	if p.curTok.Type == TokenString {
		stmt.Value = &StringLiteral{Token: p.curTok, Value: p.curTok.Literal}
	} else if p.curTok.Type == TokenIdent {
		stmt.Value = &Identifier{Token: p.curTok, Value: p.curTok.Literal}
	} else {
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

	// Expect :=
	p.nextToken()
	if p.curTok.Type != TokenAssignDefine {
		return nil
	}

	stmt.Token = p.curTok // :=

	// Expect expression (currently only string literals supported)
	p.nextToken()
	if p.curTok.Type != TokenString {
		return nil
	}

	stmt.Value = &StringLiteral{Token: p.curTok, Value: p.curTok.Literal}

	return stmt
}
