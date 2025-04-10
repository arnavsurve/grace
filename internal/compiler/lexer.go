package compiler

type Lexer struct {
	input        string
	position     int  // current char index
	readPosition int  // next char index
	ch           byte // current char
}

func NewLexer(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0 // ASCII NULL (EOF)
	} else {
		l.ch = l.input[l.readPosition]
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

	var tok Token

	switch l.ch {
	case '=':
		tok = Token{Type: TokenConst, Literal: string(l.ch)}
		l.readChar()
		return tok
	case '(':
		tok = Token{Type: TokenLParen, Literal: string(l.ch)}
		l.readChar()
		return tok
	case ')':
		tok = Token{Type: TokenRParen, Literal: string(l.ch)}
		l.readChar()
		return tok
	case '"':
		return l.readString()
	case ':':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = Token{Type: TokenAssignDefine, Literal: literal}
			l.readChar()
			return tok
		}
		return Token{Type: TokenIllegal, Literal: string(l.ch)}
	case 0:
		tok = Token{Type: TokenEOF, Literal: ""}
		return tok
	default:
		if isLetter(l.ch) {
			ident := l.readIdentifier()

			switch ident {
			case "print":
				return Token{Type: TokenPrint, Literal: ident}
			case "const":
				return Token{Type: TokenConst, Literal: ident}
			case "string", "int", "bool":
				return Token{Type: TokenTypeLiteral, Literal: ident}
			default:
				return Token{Type: TokenIdent, Literal: ident}
			}
		} else if isDigit(l.ch) {
			return l.readInteger()
		} else {
			tok = Token{Type: TokenIllegal, Literal: string(l.ch)}
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

func (l *Lexer) readIdentifier() string {
	start := l.position
	for isLetter(l.ch) || isDigit(l.ch) {
		l.readChar()
	}
	return l.input[start:l.position]
}

func (l *Lexer) readString() Token {
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
		return Token{Type: TokenString, Literal: lit}
	}

	lit := l.input[start:l.position]
	l.readChar() // Consume closing "
	return Token{Type: TokenString, Literal: lit}
}

func (l *Lexer) readInteger() Token {
	start := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	literal := l.input[start:l.position]
	return Token{Type: TokenInt, Literal: literal}
}

func isLetter(ch byte) bool {
	return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}
