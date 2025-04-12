package compiler

type TokenType string

const (
	// Single character tokens
	TokenLParen   TokenType = "LPAREN"   // (
	TokenRParen   TokenType = "RPAREN"   // )
	TokenLBrace   TokenType = "LBRACE"   // {
	TokenRBrace   TokenType = "RBRACE"   // }
	TokenAssign   TokenType = "ASSIGN"   // = (reassignment)
	TokenPlus     TokenType = "PLUS"     // +
	TokenMinus    TokenType = "MINUS"    // -
	TokenAsterisk TokenType = "ASTERISK" // *
	TokenSlash    TokenType = "SLASH"    // / (division)
	TokenColon    TokenType = "COLON"    // :

	// Keywords
	TokenPrint TokenType = "PRINT" // print
	TokenConst TokenType = "CONST" // const
	TokenProc  TokenType = "PROC"

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
	Line    int
	Column  int
}
