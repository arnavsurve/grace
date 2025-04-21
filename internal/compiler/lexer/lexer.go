package lexer

import "github.com/arnavsurve/grace/internal/compiler/token"

type Lexer struct {
	input        string
	position     int  // current char index
	readPosition int  // next char index
	ch           byte // current char

	line   int // current line number (1-indexed)
	column int // current column number (1-indexed)
}

func NewLexer(input string) *Lexer {
	l := &Lexer{input: input, line: 1, column: 0}
	l.readChar()
	return l
}

func (l *Lexer) ResetPosition() {
	l.position = 0
	l.readPosition = 0
	l.line = 1
	l.column = 0 // readChar increments this to 1, so start at 0
	l.ch = 0
	l.readChar() // Read the first character
}

// readChar advances the lexer's position and updates the current character
// It handles EOF and tracks line/column numbers correctly
func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0 // ASCII NULL (EOF)
	} else {
		l.ch = l.input[l.readPosition]
	}

	l.position = l.readPosition
	l.readPosition++

	if l.ch == '\n' {
		l.line++
		l.column = 1
	} else if l.ch != 0 {
		l.column++
	}
}

// Returns the next character without consuming it
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}

func (l *Lexer) NextToken() token.Token {
	l.skipWhitespace()

	startLine := l.line
	startCol := l.column

	var tok token.Token

	switch l.ch {
	case '/':
		if l.peekChar() == '/' {
			// Single line comment
			l.readChar()
			l.readComment()
			return l.NextToken()
		} else if l.peekChar() == '*' {
			// Block comment
			l.readChar()
			l.readBlockComment(startLine, startCol)
			return l.NextToken()
		} else {
			// Division
			tok = token.Token{Type: token.TokenSlash, Literal: string(l.ch), Line: startLine, Column: startCol}
			l.readChar()
			return tok
		}
	case '=':
		tok = token.Token{Type: token.TokenAssign, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case '(':
		tok = token.Token{Type: token.TokenLParen, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case ')':
		tok = token.Token{Type: token.TokenRParen, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case '{':
		tok = token.Token{Type: token.TokenLBrace, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case '}':
		tok = token.Token{Type: token.TokenRBrace, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case '+':
		tok = token.Token{Type: token.TokenPlus, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case '-':
		tok = token.Token{Type: token.TokenMinus, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case '*':
		tok = token.Token{Type: token.TokenAsterisk, Literal: string(l.ch), Line: startLine, Column: startCol}
		l.readChar()
		return tok
	case '"':
		// readString consumes necessary chars and returns the token
		return l.readString(startLine, startCol)
	case ',':
		tok = l.newToken(token.TokenComma, string(l.ch), startLine, startCol)
		l.readChar() // Consume the comma
		return tok   // Return immediately
	case '.': // NEW
		tok = l.newToken(token.TokenDot, string(l.ch), startLine, startCol)
		l.readChar() // Consume '.'
		return tok
	case ':':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.TokenAssignDefine, Literal: literal, Line: startLine, Column: startCol}
			l.readChar()
			return tok
		} else {
			tok = token.Token{Type: token.TokenColon, Literal: string(l.ch), Line: startLine, Column: startCol}
			l.readChar()
			return tok
		}
	case 0:
		// EOF
		tok = token.Token{Type: token.TokenEOF, Literal: "", Line: startLine, Column: startCol}
		// Do NOT call l.readChar() here
		return tok
	default:
		if isLetter(l.ch) {
			ident := l.readIdentifier()
			tokenType := lookupIdent(ident)

			return l.newToken(tokenType, ident, startLine, startCol)
		} else if isDigit(l.ch) {
			return l.readInteger(startLine, startCol)
		} else {
			tok = l.newToken(token.TokenIllegal, string(l.ch), startLine, startCol)
			l.readChar()
			return tok
		}
	}
}

// newToken is a helper to create a token.Token struct
func (l *Lexer) newToken(tokenType token.TokenType, literal string, line, col int) token.Token {
	return token.Token{Type: tokenType, Literal: literal, Line: line, Column: col}
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

func (l *Lexer) readBlockComment(startLine, startCol int) {
	l.readChar() // Consume the opening '*'

	for {
		if l.ch == 0 {
			// Reached EOF before closing block comment
			// TODO: Add error reporting if desired
			return
		}
		if l.ch == '*' && l.peekChar() == '/' {
			l.readChar() // Consume '*'
			l.readChar() // Consume '/'
			return       // End of block comment
		}
		l.readChar() // Consume character inside comment
	}
}

func (l *Lexer) readIdentifier() string {
	start := l.position
	for isLetter(l.ch) || isDigit(l.ch) {
		l.readChar()
	}
	return l.input[start:l.position]
}

func (l *Lexer) readString(startLine, startCol int) token.Token {
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
		return token.Token{Type: token.TokenString, Literal: lit, Line: startLine, Column: startCol}
	}

	lit := l.input[start:l.position]
	l.readChar() // Consume closing "
	return token.Token{Type: token.TokenString, Literal: lit, Line: startLine, Column: startCol}
}

func (l *Lexer) readInteger(startLine, startCol int) token.Token {
	start := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	literal := l.input[start:l.position]
	return token.Token{Type: token.TokenInt, Literal: literal, Line: startLine, Column: startCol}
}

func isLetter(ch byte) bool {
	return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

// keywords maps identifier strings to their corresponding token types.
var keywords = map[string]token.TokenType{
	"print":  token.TokenPrint,
	"const":  token.TokenConst,
	"proc":   token.TokenProc,
	"return": token.TokenReturn,
	"void":   token.TokenVoid,
	"int":    token.TokenTypeLiteral,
	"string": token.TokenTypeLiteral,
	"record": token.TokenRecord,
	"file":   token.TokenFileKeyword,
	"input":  token.TokenInput,
	"output": token.TokenOutput,
	"read":   token.TokenRead,
	"write":  token.TokenWrite,
	"into":   token.TokenInto,
	"from":   token.TokenFrom,
	// Add other type keywords like "bool" here if needed
}

// lookupIdent checks if an identifier is a keyword, returning the keyword's
// token type or token.TokenIdent if it's not a keyword.
func lookupIdent(ident string) token.TokenType {
	// Use case-sensitive lookup
	if tokType, ok := keywords[ident]; ok {
		return tokType
	}
	// Not a keyword, it's a user-defined identifier
	return token.TokenIdent
}
