package compiler

type TokenType string

const (
	// Single character tokens
	TokenLParen TokenType = "LPAREN"
	TokenRParen TokenType = "RPAREN"

	// Keywords
	TokenPrint TokenType = "PRINT"

	// Literals
	TokenString TokenType = "STRING"

	// Special
	TokenEOF     TokenType = "EOF"
	TokenIllegal TokenType = "ILLEGAL"

	// Assignment
	TokenAssignDefine TokenType = "DEFINE" // :=
	TokenIdent        TokenType = "IDENT"  // Identifier (e.g. variable name)

	// Types
	TokenTypeLiteral TokenType = "TYPE"
)

type Token struct {
	Type    TokenType
	Literal string
}
