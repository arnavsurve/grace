package compiler

type Lexer struct {
	input        string
	position     int  // current char index
	readPosition int  // next char index
	ch           byte // current char

	line   int // current line number (1-indexed)
	column int // current column number (1-indexed)
}

func NewLexer(input string) *Lexer {
	l := &Lexer{input: input, line: 1, column: 1}
	l.readChar()
	return l
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0 // ASCII NULL (EOF)
	} else {
		l.ch = l.input[l.readPosition]
	}

	if l.ch == '\n' {
		l.line++
		l.column = 0
	} else {
		l.column++
	}

	l.position = l.readPosition
	l.readPosition++
}

// Returns the next character without consuming it
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}

func (l *Lexer) NextToken() Token {
	l.skipWhitespace()

	startLine := l.line
	startCol := l.column

	var tok Token

	switch l.ch {
	case '/':
		if l.peekChar() == '/' {
			// Single line comment
			l.readChar()
			l.readComment()
			return l.NextToken()
		} else {
			// Division
			tok = Token{Type: TokenSlash, Literal: string(l.ch), Line: startLine, Column: startCol}
			l.readChar()
			return tok
		}
	case '=':
		tok = Token{Type: TokenAssign, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case '(':
		tok = Token{Type: TokenLParen, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case ')':
		tok = Token{Type: TokenRParen, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case '{':
		tok = Token{Type: TokenLBrace, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case '}':
		tok = Token{Type: TokenRBrace, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case '+':
		tok = Token{Type: TokenPlus, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case '-':
		tok = Token{Type: TokenMinus, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case '*':
		tok = Token{Type: TokenAsterisk, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case '"':
		// readString consumes necessary chars and returns the token
		return l.readString(startLine, startCol)
	case ':':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = Token{Type: TokenAssignDefine, Literal: literal, Line: startLine, Column: startCol}
			l.readChar()
			return tok
		} else {
			tok = Token{Type: TokenColon, Literal: string(l.ch), Line: startLine, Column: startCol}
			l.readChar()
			return tok
		}
	case 0:
		// EOF
		tok = Token{Type: TokenEOF, Literal: "", Line: startLine, Column: startCol}
		// Do NOT call l.readChar() here
		return tok
	default:
		if isLetter(l.ch) {
			ident := l.readIdentifier()
			tokenType := TokenIdent

			switch ident {
			case "print":
				tokenType = TokenPrint
			case "const":
				tokenType = TokenConst
			case "proc":
				tokenType = TokenProc
			case "string", "int", "bool":
				tokenType = TokenTypeLiteral
			}

			return Token{Type: tokenType, Literal: ident, Line: startLine, Column: startCol}

		} else if isDigit(l.ch) {
			return l.readInteger(startLine, startCol)
		} else {
			tok = Token{Type: TokenIllegal, Literal: string(l.ch), Line: startLine, Column: startCol}
			l.readChar()
			return tok
		}
	}
}

func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\n' || l.ch == '\t' || l.ch == '\r' {
		l.readChar()
	}
}

func (l *Lexer) readComment() {
	for l.ch != '\n' && l.ch != 0 {
		l.readChar()
	}
}

func (l *Lexer) readIdentifier() string {
	start := l.position
	for isLetter(l.ch) || isDigit(l.ch) {
		l.readChar()
	}
	return l.input[start:l.position]
}

func (l *Lexer) readString(startLine, startCol int) Token {
	start := l.position + 1 // Skip opening "
	l.readChar()            // Consume opening "

	for l.ch != '"' && l.ch != 0 {
		// TODO: Add handling for escape sequences if needed
		l.readChar()
	}

	if l.ch == 0 {
		// TODO: Unterminated string - handle error?
		// For now, return what was read as illegal or handle in parser
		lit := l.input[start:l.position]
		return Token{Type: TokenString, Literal: lit, Line: startLine, Column: startCol}
	}

	lit := l.input[start:l.position]
	l.readChar() // Consume closing "
	return Token{Type: TokenString, Literal: lit, Line: startLine, Column: startCol}
}

func (l *Lexer) readInteger(startLine, startCol int) Token {
	start := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	literal := l.input[start:l.position]
	return Token{Type: TokenInt, Literal: literal, Line: startLine, Column: startCol}
}

func isLetter(ch byte) bool {
	return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}
