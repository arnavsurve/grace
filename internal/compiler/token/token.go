package token

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
	TokenComma    TokenType = "COMMA"    // ,

	// Keywords
	TokenPrint  TokenType = "PRINT"  // print
	TokenConst  TokenType = "CONST"  // const
	TokenProc   TokenType = "PROC"   // proc
	TokenReturn TokenType = "RETURN" // return
	TokenVoid   TokenType = "VOID"   // void (used as a type literal)
	// NOTE: TokenVoid is also lexed and can be checked specifically,
	// but is often treated like a TokenTypeLiteral in parsing logic for type nodes
	TokenRecord      TokenType = "RECORD" // record
	TokenFileKeyword TokenType = "FILE"   // file keyword (use as type)
	TokenInput       TokenType = "INPUT"  // input keyword
	TokenOutput      TokenType = "OUTPUT" // output keyword
	TokenRead        TokenType = "READ"   // read keyword
	TokenWrite       TokenType = "WRITE"  // write keyword
	TokenInto        TokenType = "INTO"   // into keyword
	TokenFrom        TokenType = "FROM"   // from keyword

	// Literals & Identifiers
	TokenString TokenType = "STRING" // "..."
	TokenInt    TokenType = "INT"    // 43
	TokenIdent  TokenType = "IDENT"  // Identifier (e.g. variable name)

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
