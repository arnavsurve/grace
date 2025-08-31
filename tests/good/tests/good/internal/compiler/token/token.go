package token

type TokenType string

const (
	// Single character tokens
	TokenLParen   TokenType = "LPAREN"
	TokenRParen   TokenType = "RPAREN"
	TokenLBrace   TokenType = "LBRACE"
	TokenRBrace   TokenType = "RBRACE"
	TokenAssign   TokenType = "ASSIGN"
	TokenPlus     TokenType = "PLUS"
	TokenMinus    TokenType = "MINUS"
	TokenAsterisk TokenType = "ASTERISK"
	TokenSlash    TokenType = "SLASH"
	TokenColon    TokenType = "COLON"
	TokenComma    TokenType = "COMMA"
	TokenDot      TokenType = "DOT"

	// Keywords
	TokenPrint  TokenType = "PRINT"
	TokenConst  TokenType = "CONST"
	TokenProc   TokenType = "PROC"
	TokenReturn TokenType = "RETURN"
	TokenVoid   TokenType = "VOID" // used as a type literal
	// NOTE: TokenVoid is also lexed and can be checked specifically,
	// but is often treated like a TokenTypeLiteral in parsing logic for type nodes
	TokenRecord      TokenType = "RECORD"
	TokenFileKeyword TokenType = "FILE"
	TokenInput       TokenType = "INPUT"
	TokenOutput      TokenType = "OUTPUT"
	TokenRead        TokenType = "READ"
	TokenWrite       TokenType = "WRITE"
	TokenInto        TokenType = "INTO"
	TokenFrom        TokenType = "FROM"

	// Literals & Identifiers
	TokenString  TokenType = "STRING"  // "..."
	TokenInt     TokenType = "INT"     // 43
	TokenIdent   TokenType = "IDENT"   // Identifier (e.g. variable name)
	TokenBoolLit TokenType = "BOOLEAN" // true, false

	// Special
	TokenEOF     TokenType = "EOF"
	TokenIllegal TokenType = "ILLEGAL"

	// Assignment
	TokenAssignDefine TokenType = "DEFINE" // :=

	// Types (used by parser to categorize type names)
	// NOTE: int, string are lexed as TokenTypeLiteral, void as TokenVoid, file as TokenFileKeyword
	TokenTypeLiteral TokenType = "TYPE_LITERAL"
)

type Token struct {
	Type    TokenType
	Literal string
	Line    int
	Column  int
}

func (t Token) IsTypeKeyword() bool {
	return t.Type == TokenTypeLiteral || t.Type == TokenVoid || t.Type == TokenFileKeyword
}
