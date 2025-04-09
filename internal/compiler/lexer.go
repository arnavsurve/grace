package compiler

import "unicode"

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

func (l *Lexer) NextToken() Token {
	l.skipWhitespace()

	switch l.ch {
	case '(':
		tok := Token{Type: TokenLParen, Literal: string(l.ch)}
		l.readChar()
		return tok
	case ')':
		tok := Token{Type: TokenRParen, Literal: string(l.ch)}
		l.readChar()
		return tok
	case '"':
		return l.readString()
	case ':':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok := Token{Type: TokenAssignDefine, Literal: literal}
			l.readChar()
			return tok
		}
		return Token{Type: TokenIllegal, Literal: string(l.ch)}
	case 0:
		return Token{Type: TokenEOF, Literal: ""}
	default:
		if isLetter(l.ch) {
			ident := l.readIdentifier()

			switch ident {
			case "print":
				return Token{Type: TokenPrint, Literal: ident}
			case "string", "int", "bool":
				return Token{Type: TokenTypeLiteral, Literal: ident}
			default:
				return Token{Type: TokenIdent, Literal: ident}
			}
		} else if isDigit(l.ch) {
			return l.readInteger()
		}
		return Token{Type: TokenIllegal, Literal: string(l.ch)}
	}
}

func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\n' || l.ch == '\t' || l.ch == '\r' {
		l.readChar()
	}
}

func (l *Lexer) readIdentifier() string {
	start := l.position
	for isLetter(l.ch) {
		l.readChar()
	}
	return l.input[start:l.position]
}

func (l *Lexer) readString() Token {
	l.readChar() // skip opening "
	start := l.position
	for l.ch != '"' && l.ch != 0 {
		l.readChar()
	}
	lit := l.input[start:l.position]
	l.readChar() // skip closing "
	return Token{Type: TokenString, Literal: lit}
}

func (l *Lexer) readInteger() Token {
	start := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	return Token{Type: TokenInt, Literal: l.input[start:l.position]}
}

func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}

func isLetter(ch byte) bool {
	return unicode.IsLetter(rune(ch))
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}
