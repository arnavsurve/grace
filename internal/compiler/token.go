package compiler

type TokenType string

const (
	// Single character tokens
	TokenLParen TokenType = "LPAREN" // (
	TokenRParen TokenType = "RPAREN" // )
	TokenAssign TokenType = "ASSIGN" // = (reassignment)

	// Keywords
	TokenPrint TokenType = "PRINT" // print
	TokenConst TokenType = "CONST" // const

	// Literals
	TokenString TokenType = "STRING" // "..."
	TokenInt    TokenType = "INT"    // 43

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
