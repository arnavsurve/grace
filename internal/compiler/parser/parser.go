package parser

import (
	"fmt"
	"slices"
	"strconv"

	"github.com/arnavsurve/grace/internal/compiler/ast"
	"github.com/arnavsurve/grace/internal/compiler/lexer"
	"github.com/arnavsurve/grace/internal/compiler/lib"
	"github.com/arnavsurve/grace/internal/compiler/scope"
	"github.com/arnavsurve/grace/internal/compiler/symbols"
	"github.com/arnavsurve/grace/internal/compiler/token"
)

// Precedence levels for Pratt parsing
const (
	_ int = iota
	PrecLowest
	PrecSum     // +, -
	PrecProduct // *, /
	PrecCall    // function(...)
	PrecPrimary // Literals, identifiers, (...), input(...), output(...)
)

// Map tokens to precedence levels
var precedences = map[token.TokenType]int{
	token.TokenPlus:     PrecSum,
	token.TokenMinus:    PrecSum,
	token.TokenAsterisk: PrecProduct,
	token.TokenSlash:    PrecProduct,
	token.TokenLParen:   PrecCall, // For function calls like identifier(...)
}

func tokenPrecedence(tok token.Token) int {
	if p, ok := precedences[tok.Type]; ok {
		return p
	}
	return PrecLowest
}

type Parser struct {
	l               *lexer.Lexer
	curTok          token.Token
	peekTok         token.Token
	errors          []string
	warnings        []string                      // Separate warnings
	symbolTable     map[string]symbols.SymbolInfo // Global symbol table (includes procs and globals)
	currentProcName string                        // Track the current procedure being parsed for return checks

	// Scope management
	currentScope *scope.Scope

	// Pratt parser fucntions (maps are initialized in initializePratt)
	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn

	// Temporary storage for Pass 1 results
	// Maps identifier name to basic info needed for Pass 2 resolution
	// Using full SymbolInfo temporarily, but only specific fields are populated in Pass 1
	pass1Symbols map[string]*symbols.SymbolInfo
}

func NewParser(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:               l,
		errors:          []string{},
		warnings:        []string{},
		symbolTable:     make(map[string]symbols.SymbolInfo),
		currentProcName: "",  // Not inside a proc initially
		currentScope:    nil, // Will be initialized before parsing passes
		pass1Symbols:    make(map[string]*symbols.SymbolInfo),
	}
	return p
}

// --- Token Handling ---
func (p *Parser) nextToken() {
	p.curTok = p.peekTok
	p.peekTok = p.l.NextToken()
}

// --- Error/Warning Handling ---
func (p *Parser) addError(tok token.Token, format string, args ...any) {
	msg := fmt.Sprintf(format, args...)
	errMsg := fmt.Sprintf("%d:%d: Syntax Error: %s", tok.Line, tok.Column, msg)
	p.errors = append(p.errors, errMsg)
}

func (p *Parser) addSemanticError(tok token.Token, format string, args ...any) {
	msg := fmt.Sprintf(format, args...)
	errMsg := fmt.Sprintf("%d:%d: Semantic Error: %s", tok.Line, tok.Column, msg)
	p.errors = append(p.errors, errMsg)
}

func (p *Parser) addWarning(tok token.Token, format string, args ...any) {
	msg := fmt.Sprintf(format, args...)
	// Prefix warnings for clarity
	p.warnings = append(p.warnings, fmt.Sprintf("%d:%d: Semantic Warning: %s", tok.Line, tok.Column, msg))
}

// Errors returns only fatal errors
func (p *Parser) Errors() []string {
	return p.errors
}

// Warnings returns non-fatal warnings
func (p *Parser) Warnings() []string {
	return p.warnings
}

// AllMessages returns both errors and warnings
func (p *Parser) AllMessages() []string {
	all := make([]string, 0, len(p.errors)+len(p.warnings))
	all = append(all, p.errors...)
	all = append(all, p.warnings...)
	return all
}

// getSymbolInfo checks the current scope (global only for now)
func (p *Parser) getSymbolInfo(name string) (symbols.SymbolInfo, bool) {
	// Check current scope first
	if sym, ok := p.lookupSymbol(name); ok {
		return *sym, true
	}

	// Fallback to global symbol table (e.g. for emitter or testing)
	info, exists := p.symbolTable[name]
	return info, exists
}

// -- Scope Management Wrappers ---
func (p *Parser) pushScope() {
	p.currentScope = scope.NewScope(p.currentScope, p.currentScope.Name)
}

func (p *Parser) popScope() {
	if p.currentScope != nil {
		p.currentScope = p.currentScope.Outer
	}
	// Add check if popScope is called on nil scope (shouldn't happen)
	if p.currentScope == nil && len(p.errors) == 0 { // Only error if no other errors exist
		// This indicates a bug in scope push/pop pairing
		p.addError(p.curTok, "Internal Parser Error: Popped global scope")
	}
}

// Defines a symbol in the *current* scope.
func (p *Parser) defineSymbol(name string, info symbols.SymbolInfo) error {
	if p.currentScope == nil {
		// This should not happen if initialized correctly
		return fmt.Errorf("internal error: currentScope is nil before defining '%s'", name)
	}
	return p.currentScope.Define(name, info)
}

// Looks up a symbol starting from the current scope outwards.
func (p *Parser) lookupSymbol(name string) (*symbols.SymbolInfo, bool) {
	if p.currentScope == nil {
		// This might happen if lookup occurs before initialization
		// Or after an error caused premature scope popping
		return nil, false
	}
	infoPtr, found := p.currentScope.Lookup(name)
	if found {
		return infoPtr, true
	}

	return nil, false
}

// Looks up a symbol *only* in the current scope level.
func (p *Parser) lookupCurrentScopeSymbol(name string) (*symbols.SymbolInfo, bool) {
	if p.currentScope == nil {
		return nil, false
	}
	return p.currentScope.LookupCurrentScope(name)
}

// --- Program Parsing (Two-Pass Orchestration) ---

func (p *Parser) ParseProgram() *ast.Program {
	// --- Pass 1: Collect Global Declarations (Procs, Records, Files) ---
	pass1Errs := p.collectGlobalDeclarations()
	p.errors = append(p.errors, pass1Errs...) // Add any errors from pass 1
	if len(p.errors) > 0 {
		// Filter out only fatal errors if needed, or just stop if any error.
		// Let's stop if Pass 1 had errors preventing symbol resolution.
		fmt.Println("Errors during Pass 1, stopping.") // Debugging
		return nil
	}

	// --- Pass 2: Full Parse ---
	p.l.ResetPosition()                            // Reset lexer for Pass 2
	p.currentScope = scope.NewScope(nil, "global") // Initialize global scope for Pass 2
	p.errors = []string{}                          // Reset errors for Pass 2
	p.warnings = []string{}                        // Reset warnings for Pass 2

	// Initialize tokens for Pass 2
	p.nextToken()
	p.nextToken()

	// Populate global scope with declarations from Pass 1
	for name, infoPtr := range p.pass1Symbols {
		err := p.defineSymbol(name, *infoPtr) // Define in Pass 2's global scope
		if err != nil {
			// Should not happen if Pass 1 checked duplicates correctly
			p.addSemanticError(token.Token{}, "Internal Error: Failed to define pre-declared symbol '%s' in Pass 2: %v", name, err)
		}
	}

	// Initialize Pratt functions if not already done
	if p.prefixParseFns == nil {
		p.initializePratt()
	}

	program := &ast.Program{
		Statements:  []ast.Statement{},
		GlobalScope: p.currentScope, // Assign the fully populated global scope
	}

	// Main parsing loop (Pass 2)
	for p.curTok.Type != token.TokenEOF {
		startTokForNilCheck := p.curTok // Store token before calling parseStatement

		stmt := p.parseStatement()

		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		} else {
			// parseStatement returned nil. Check WHY.
			if len(p.errors) > 0 {
				// A real error occurred during parsing the statement.
				// Basic recovery: Skip the token that likely caused the error
				// We might need smarter recovery later (e.g., skip until newline or ';')
				if p.curTok == startTokForNilCheck && p.curTok.Type != token.TokenEOF {
					// If the token didn't advance during the failed parse, force advance
					p.nextToken()
				} // Otherwise assume parseStatement consumed the problematic token
			} else if p.curTok != startTokForNilCheck {
				// Statement was skipped successfully (like 'record' def in Pass 2)
				// parseStatement returned nil, no errors, AND curTok advanced.
				// This is OK, just continue the loop.
			} else if p.curTok.Type != token.TokenEOF {
				// Statement is nil, no errors, AND token DID NOT advance.
				// This indicates an internal parser logic error where a token
				// wasn't handled or consumed correctly.
				p.addError(p.curTok, "Internal Parser Error: Unhandled token %s ('%s')", p.curTok.Type, p.curTok.Literal)
				p.nextToken() // Force advance to prevent infinite loop
			}
		}
	}

	// Final scope check
	if p.currentScope == nil || p.currentScope.Outer != nil {
		p.addError(token.Token{}, "Internal Parser Error: Scope mismatch at end of parsing.")
		return nil
	}

	return program
}

// --- Pass 1 Implementation ---

// collectGlobalDeclarations performs the first pass to gather signatures/info
// for top-level procs, records, and potentially files (if declared explicitly at top level).
// Populates p.pass1Symbols.
func (p *Parser) collectGlobalDeclarations() []string {
	pass1Errs := []string{}
	tempParser := NewParser(p.l) // Use a temporary parser state
	tempParser.nextToken()
	tempParser.nextToken()

	processedRecords := make(map[string]bool) // Track records processed in this pass

	// Need to potentially loop until stable if records depend on others (not supported yet)
	// For now, assume records are defined before use in files within Pass 1 scan.

	for tempParser.curTok.Type != token.TokenEOF {
		switch tempParser.curTok.Type {
		case token.TokenRecord:
			// --- Parse Record Signature ---
			recordNameTok := tempParser.peekTok
			if recordNameTok.Type != token.TokenIdent {
				pass1Errs = append(pass1Errs, fmt.Sprintf("%d:%d: Syntax Error: Expected record name after 'record'", tempParser.peekTok.Line, tempParser.peekTok.Column))
				tempParser.skipUntilDeclarationStart()
				continue
			}
			recordName := recordNameTok.Literal

			// Check for duplicate definitions found *during this pass*
			if _, exists := p.pass1Symbols[recordName]; exists {
				pass1Errs = append(pass1Errs, fmt.Sprintf("%d:%d: Semantic Error: Symbol '%s' already declared", recordNameTok.Line, recordNameTok.Column, recordName))
				tempParser.skipRecordOrProcBody()
				continue
			}

			// Basic parsing just to get fields and skip body
			recordInfo, err := tempParser.parseRecordSignatureForPass1()
			if err != nil {
				pass1Errs = append(pass1Errs, err.Error())
				// skipRecordOrProcBody() should have been called internally
			} else {
				recordInfo.Name = recordName // Store original name
				infoCopy := recordInfo
				p.pass1Symbols[recordName] = &infoCopy
				processedRecords[recordName] = true
			}
			// parseRecordSignatureForPass1 consumes the body

		case token.TokenProc:
			// --- Parse Proc Signature ---
			procNameTok := tempParser.peekTok
			if procNameTok.Type != token.TokenIdent {
				pass1Errs = append(pass1Errs, fmt.Sprintf("%d:%d: Syntax Error: Expected procedure name after 'proc'", tempParser.peekTok.Line, tempParser.peekTok.Column))
				tempParser.skipUntilDeclarationStart()
				continue
			}
			procName := procNameTok.Literal

			if _, exists := p.pass1Symbols[procName]; exists {
				pass1Errs = append(pass1Errs, fmt.Sprintf("%d:%d: Semantic Error: Symbol '%s' already declared", procNameTok.Line, procNameTok.Column, procName))
				tempParser.skipRecordOrProcBody()
				continue
			}

			procInfo, err := tempParser.parseProcSignatureForPass1()
			if err != nil {
				pass1Errs = append(pass1Errs, err.Error())
				// skipRecordOrProcBody() should have been called internally
			} else {
				procInfo.Name = procName // Store original name
				infoCopy := procInfo
				p.pass1Symbols[procName] = &infoCopy
			}
			// parseProcSignatureForPass1 consumes the body

			// Note: File declarations involving `:= input(...)` happen in Pass 2.
			// Only handle `: file = input(...)` if we decide to support that globally,
			// but it's complex due to needing the record type.
			// Let's assume file declarations happen inside MAIN or procs for now via `:=`.

		default:
			// Skip other tokens quickly in Pass 1
			tempParser.nextToken()
		}
	}

	// TODO: Add validation pass here? Check if records used by files/procs exist?
	// For now, Pass 2 handles unresolved symbols.

	return pass1Errs
}

// parseRecordSignatureForPass1 parses just enough to get field names/types/widths
// and consumes the record body. Used only in Pass 1.
func (p *Parser) parseRecordSignatureForPass1() (symbols.SymbolInfo, error) {
	recordInfo := symbols.SymbolInfo{Type: "record", Fields: []symbols.FieldInfo{}}

	if !p.expectPeek(token.TokenIdent) { // Consume 'record', check for name
		p.skipUntilDeclarationStart()
		return recordInfo, fmt.Errorf("%d:%d: Syntax Error: Expected record name", p.curTok.Line, p.curTok.Column)
	}
	// recordName := p.curTok.Literal // Name already checked by caller
	recordNameTok := p.curTok

	if !p.expectPeek(token.TokenLBrace) { // Consume name, check for '{'
		p.skipUntilDeclarationStart()
		return recordInfo, fmt.Errorf("%d:%d: Syntax Error: Expected '{' for record '%s'", p.curTok.Line, p.curTok.Column, recordNameTok.Literal)
	}
	// curTok is now '{'
	p.nextToken() // Consume '{'

	fieldOffset := 0
	fieldNames := make(map[string]bool)

	for p.curTok.Type != token.TokenRBrace && p.curTok.Type != token.TokenEOF {
		if p.curTok.Type != token.TokenIdent {
			p.skipRecordOrProcBody() // Skip rest of malformed body
			return recordInfo, fmt.Errorf("%d:%d: Syntax Error: Expected field name (identifier) in record '%s'", p.curTok.Line, p.curTok.Column, recordNameTok.Literal)
		}
		fieldName := p.curTok.Literal
		fieldNameTok := p.curTok

		if _, exists := fieldNames[fieldName]; exists {
			p.skipRecordOrProcBody()
			return recordInfo, fmt.Errorf("%d:%d: Semantic Error: Duplicate field name '%s' in record '%s'", fieldNameTok.Line, fieldNameTok.Column, fieldName, recordNameTok.Literal)
		}
		fieldNames[fieldName] = true

		if !p.expectPeek(token.TokenColon) {
			p.skipRecordOrProcBody()
			return recordInfo, fmt.Errorf("%d:%d: Syntax Error: Expected ':' after field name '%s'", p.curTok.Line, p.curTok.Column, fieldName)
		}
		// curTok is ':'
		p.nextToken() // Consume ':'
		// curTok is start of type node

		// Parse type node (must be int or string with explicit width)
		typeNode := p.parseTypeNode() // Use the main type parser
		if typeNode == nil {
			p.skipRecordOrProcBody()
			return recordInfo, fmt.Errorf("%d:%d: Syntax Error: Invalid type specification for field '%s'", p.curTok.Line, p.curTok.Column, fieldName) // Error should be in p.errors
		}
		if typeNode.IsVoid || typeNode.IsRecord {
			p.skipRecordOrProcBody()
			return recordInfo, fmt.Errorf("%d:%d: Semantic Error: Record field '%s' type cannot be '%s'", fieldNameTok.Line, fieldNameTok.Column, fieldName, typeNode.Name)
		}
		if typeNode.Width <= 0 {
			p.skipRecordOrProcBody()
			return recordInfo, fmt.Errorf("%d:%d: Semantic Error: Record field '%s' requires an explicit positive width (e.g., int(5), string(10))", fieldNameTok.Line, fieldNameTok.Column, fieldName)
		}

		field := symbols.FieldInfo{
			Name:   fieldName,
			Type:   typeNode.Name,
			Width:  typeNode.Width,
			Offset: fieldOffset,
		}
		recordInfo.Fields = append(recordInfo.Fields, field)
		fieldOffset += field.Width
		recordInfo.TotalWidth = fieldOffset // Update total width

		// curTok is now after the type spec (e.g., after ')')
	}

	if p.curTok.Type != token.TokenRBrace {
		// Unterminated record definition
		return recordInfo, fmt.Errorf("%d:%d: Syntax Error: Expected '}' to close record definition for '%s'", p.curTok.Line, p.curTok.Column, recordNameTok.Literal)
	}
	p.nextToken() // Consume '}'

	return recordInfo, nil
}

// parseProcSignatureForPass1 parses just enough to get params/return type info
// and consumes the procedure body. Used only in Pass 1.
func (p *Parser) parseProcSignatureForPass1() (symbols.SymbolInfo, error) {
	procInfo := symbols.SymbolInfo{Type: "proc", ParamNames: []string{}, ParamTypes: []string{}, ParamWidths: []int{}}

	if !p.expectPeek(token.TokenIdent) { // Consume 'proc', check name
		p.skipUntilDeclarationStart()
		return procInfo, fmt.Errorf("%d:%d: Syntax Error: Expected procedure name", p.curTok.Line, p.curTok.Column)
	}
	// procName := p.curTok.Literal // Checked by caller
	procNameTok := p.curTok

	if !p.expectPeek(token.TokenLParen) { // Consume name, check '('
		p.skipUntilDeclarationStart()
		return procInfo, fmt.Errorf("%d:%d: Syntax Error: Expected '(' after procedure name '%s'", p.curTok.Line, p.curTok.Column, procNameTok.Literal)
	}
	// curTok is '('

	// Parse parameter list (reusing main parser logic carefully)
	// This is tricky as parseParameterList might rely on full scope lookup if types could be records.
	// Let's assume params are simple types for now in Pass 1.
	params, paramNames, err := p.parseParameterList() // This consumes up to ')'
	if err != nil {
		// Error added by parseParameterList
		p.skipSignatureAndBody() // Try to skip rest
		return procInfo, fmt.Errorf("error parsing parameters for '%s': %w", procNameTok.Literal, err)
	}
	procInfo.ParamNames = paramNames
	// Populate types/widths - requires param types to be base types for Pass 1
	for _, param := range params {
		if param.TypeNode == nil {
			continue
		} // Skip if parsing failed earlier
		if param.TypeNode.IsRecord {
			// TODO: Handle record type parameters in Pass 1? Complex. Defer for now.
			p.skipSignatureAndBody()
			return procInfo, fmt.Errorf("%d:%d: Semantic Error: Record types as parameters not fully supported in Pass 1 yet (param '%s')", param.Name.Token.Line, param.Name.Token.Column, param.Name.Value)
		}
		procInfo.ParamTypes = append(procInfo.ParamTypes, param.TypeNode.Name)
		procInfo.ParamWidths = append(procInfo.ParamWidths, param.TypeNode.Width)
	}
	// curTok is now ')'

	if p.curTok.Type != token.TokenRParen {
		// This indicates an internal logic error if parseParameterList succeeded
		p.addError(p.curTok, "Internal parser error: parseParameterList finished on wrong token: %+v", p.curTok)
		p.skipSignatureAndBody() // Attempt recovery
		return procInfo, fmt.Errorf("internal error after params")
	}
	// Consume the ')' explicitly
	p.nextToken()

	// Step 2: Check that the current token is now ':'
	if p.curTok.Type != token.TokenColon {
		p.addError(p.curTok, "Expected ':' after '()', got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		p.skipSignatureAndBody() // Attempt recovery
		return procInfo, fmt.Errorf("expected ':' after '()'")
	}
	// curTok is ':'
	p.nextToken() // Consume ':'
	// curTok is start of return type

	// Parse return type node
	returnTypeNode := p.parseTypeNode()
	if returnTypeNode == nil {
		p.skipSignatureAndBody()
		return procInfo, fmt.Errorf("error parsing return type for '%s'", procNameTok.Literal) // Error should be in p.errors
	}
	if returnTypeNode.IsRecord {
		// TODO: Handle record return types in Pass 1? Need to look up record.
		p.skipSignatureAndBody()
		return procInfo, fmt.Errorf("%d:%d: Semantic Error: Record return types not fully supported in Pass 1 yet (type '%s')", returnTypeNode.Token.Line, returnTypeNode.Token.Column, returnTypeNode.Name)
	}
	procInfo.ReturnType = returnTypeNode.Name
	procInfo.ReturnWidth = returnTypeNode.Width // Width stored even if void (will be 0)
	// curTok is now after return type (should be '{')

	// Skip body
	if p.curTok.Type != token.TokenLBrace {
		p.skipUntilDeclarationStart()
		return procInfo, fmt.Errorf("%d:%d: Syntax Error: Expected '{' to start body for procedure '%s'", p.curTok.Line, p.curTok.Column, procNameTok.Literal)
	}
	p.skipBlock() // Consumes '{' to past matching '}'

	return procInfo, nil
}

// skipUntilDeclarationStart attempts to find the next 'proc' or 'record' or EOF.
func (p *Parser) skipUntilDeclarationStart() {
	p.skipUntil(token.TokenProc, token.TokenRecord, token.TokenEOF)
}

// skipRecordOrProcBody finds the opening '{' and skips to its matching '}'.
func (p *Parser) skipRecordOrProcBody() {
	// 1. Ensure we start on 'record' or 'proc'
	if p.curTok.Type != token.TokenRecord && p.curTok.Type != token.TokenProc {
		// Don't consume, let the caller handle the unexpected token
		return
	}

	// 2. Consume 'record'/'proc' keyword
	p.nextToken()

	// 3. Consume the name IDENTIFIER
	if p.curTok.Type == token.TokenIdent {
		p.nextToken()
	} else {
		// If no identifier follows, it's a syntax error already,
		// but we still need to try and find the block to skip it.
		p.skipUntil(token.TokenLBrace, token.TokenProc, token.TokenRecord, token.TokenEOF)
		// If we didn't find '{', exit; the outer loop will handle EOF or next decl.
		if p.curTok.Type != token.TokenLBrace {
			return
		}
		// If we found '{' after skipping, proceed to skipBlock
	}

	// 4. Now, curTok *should* be the LBrace '{'. Find and skip the block.
	if p.curTok.Type == token.TokenLBrace {
		p.skipBlock() // skipBlock consumes { through }
	} else {
		// If we consumed 'record' + 'ident' but didn't find '{'
		// Attempt recovery by skipping to next declaration or EOF
		p.skipUntil(token.TokenProc, token.TokenRecord, token.TokenEOF)
	}
}

// skipSignatureAndBody attempts to advance the parser past a potentially malformed signature and its body
func (p *Parser) skipSignatureAndBody() {
	// Try to find the opening brace '{'
	for p.curTok.Type != token.TokenLBrace && p.curTok.Type != token.TokenEOF && p.curTok.Type != token.TokenProc {
		p.nextToken()
	}
	// If found '{', skip the block
	if p.curTok.Type == token.TokenLBrace {
		p.skipBlock()
	}
	// If we hit PROC or EOF, the outer loop will handle it.
}

// skipUntil advances tokens until one of the target types or EOF is found.
// Does NOT consume the target token.
func (p *Parser) skipUntil(targets ...token.TokenType) {
	for p.curTok.Type != token.TokenEOF {
		if slices.Contains(targets, p.curTok.Type) {
			return // Found target, stop skipping
		}
		p.nextToken()
	}
}

// --- Statement Parsing (Pass 2) ---

func (p *Parser) parseStatement() ast.Statement {
	switch p.curTok.Type {
	case token.TokenRecord:
		// Records only defined at top level, skip in pass 2 (already in symbol table)
		p.skipRecordOrProcBody() // Need to skip the definition
		return nil               // No AST node needed from Pass 2 for definition itself
	case token.TokenProc:
		return p.parseProcDeclarationStatement() // Parse fully in Pass 2 for AST/local scope
	case token.TokenReturn:
		return p.parseReturnStatement()
	case token.TokenPrint:
		return p.parsePrintStatement()
	case token.TokenConst:
		return p.parseDeclarationStatement() // Handles const modifier
	case token.TokenRead:
		return p.parseReadStatement()
	case token.TokenWrite:
		return p.parseWriteStatement()
	case token.TokenIdent:
		switch p.peekTok.Type {
		case token.TokenAssignDefine, token.TokenColon:
			return p.parseDeclarationStatement() // Handles `:=` and `: type`
		case token.TokenAssign:
			return p.parseReassignmentStatement()
		case token.TokenLParen:
			return p.parseExpressionStatement() // Proc call as statement
		default:
			// If it's just an identifier, treat as expression statement (likely warning)
			return p.parseExpressionStatement()
		}
	case token.TokenLBrace:
		p.addError(p.curTok, "Unexpected '{' at start of statement (blocks only allowed inside procedures)")
		p.skipBlock()
		return nil
	case token.TokenEOF:
		return nil
	default:
		// Try parsing as expression statement (e.g., for literals, non-call expressions)
		exprStmt := p.parseExpressionStatement()
		if exprStmt != nil {
			// Add warnings for pointless statements (literals, non-call expressions)
			// Check done within parseExpressionStatement now
			return exprStmt
		}
		// If parseExpressionStatement failed, an error was likely added.
		if len(p.errors) == 0 { // Add generic error if not specific one exists
			p.addError(p.curTok, "Unexpected token at start of statement: %s ('%s')", p.curTok.Type, p.curTok.Literal)
		}
		p.nextToken() // Consume unexpected token to attempt recovery
		return nil
	}
}

// parseDeclarationStatement parses `[const] name := value` or `[const] name: type = value` or `name : RecordType`
func (p *Parser) parseDeclarationStatement() ast.Statement {
	stmt := &ast.DeclarationStatement{}
	isConst := false

	if p.curTok.Type == token.TokenConst {
		isConst = true
		stmt.IsConst = true
		p.nextToken() // Consume 'const'
		if p.curTok.Type != token.TokenIdent {
			p.addError(p.curTok, "Expected identifier after 'const'")
			return nil
		}
	}

	if p.curTok.Type != token.TokenIdent {
		p.addError(p.curTok, "Internal Parser Error: Expected identifier for declaration start")
		return nil
	}
	identToken := p.curTok
	varName := identToken.Literal
	stmt.Name = &ast.Identifier{Token: identToken, Value: varName}

	// Check for redeclaration *in the current scope*
	_, declaredInCurrent := p.lookupCurrentScopeSymbol(varName)
	if declaredInCurrent {
		p.addSemanticError(identToken, "Variable '%s' already declared in this scope", varName)
		// Allow parsing to continue to find other errors, but don't define symbol again.
	}

	var finalType string
	var finalWidth int
	var finalSymbolInfo symbols.SymbolInfo
	var recordTypeSymbol *symbols.SymbolInfo // To link variable to its record type

	// Determine declaration type (:=, :, : file =)
	switch p.peekTok.Type {
	case token.TokenAssignDefine: // name := value
		if isConst {
			p.addError(p.peekTok, "Cannot use ':=' with 'const'. Use 'const name : type = value'.")
			return nil // TODO: try to recover
		}
		stmt.Token = p.peekTok // Store ':='
		p.nextToken()          // Consume IDENT
		p.nextToken()          // Consume ':='

		valueExpr := p.parseExpression(PrecLowest)
		if valueExpr == nil {
			return nil
		}
		stmt.Value = valueExpr

		// Infer type and width
		valueType := valueExpr.ResultType()
		valueWidth := valueExpr.ResultWidth()

		if valueType == "void" {
			p.addSemanticError(valueExpr.GetToken(), "Cannot assign result of void expression to variable '%s'", varName)
			finalType = "unknown"
		} else if valueType == "unknown" {
			// Error should have been reported by expression parser
			finalType = "unknown"
		} else if valueType == "file" {
			// Handle file handle assignment `fh := input(...)`
			finalType = "file"
			finalWidth = 0 // Files don't have intrinsic width
			// Get the symbol info created by input/output expression parser
			if fileExprSym := valueExpr.GetResolvedSymbol(); fileExprSym != nil {
				// Copy relevant info from the temporary file symbol to the final symbol
				finalSymbolInfo = symbols.SymbolInfo{
					Name:                 varName,
					Type:                 "file",
					IsConst:              false, // File handles aren't const
					Mode:                 fileExprSym.Mode,
					SystemFileName:       fileExprSym.SystemFileName,
					RecordTypeName:       fileExprSym.RecordTypeName,
					FileRecordTypeSymbol: fileExprSym.FileRecordTypeSymbol, // Link to the record type symbol
				}
			} else {
				p.addError(valueExpr.GetToken(), "Internal error: input/output expression did not resolve file info")
				finalType = "unknown"
			}
		} else { // int, string, or record type name from function call
			// If valueExpr is a proc call returning a record, valueType will be the record name.
			recordSym, isRecordType := p.lookupSymbol(valueType)
			if isRecordType && recordSym.Type == "record" {
				// Handle assignment from func returning record: `recVar := myFuncReturningRec()`
				finalType = valueType // Type is the record name
				finalWidth = recordSym.TotalWidth
				recordTypeSymbol = recordSym // Link variable to record type
			} else if valueType == "int" || valueType == "string" {
				// Standard type inference
				finalType = valueType
				finalWidth = valueWidth
				if finalWidth <= 0 { // Apply default width if needed
					finalWidth = lib.GetDefaultWidth(finalType)
				}
			} else {
				p.addSemanticError(valueExpr.GetToken(), "Cannot infer type from expression for '%s'", varName)
				finalType = "unknown"
			}
		}

		// Populate standard fields only if it's not a file type (file type populated above)
		if finalType != "file" {
			finalSymbolInfo = symbols.SymbolInfo{
				Name:             varName,
				Type:             finalType,
				Width:            finalWidth,
				IsConst:          false,            // := is never const
				RecordTypeSymbol: recordTypeSymbol, // Link to record type if applicable
			}
		}

	case token.TokenColon: // --- name : type [= value] ---
		p.nextToken() // Consume IDENT
		p.nextToken() // Consume ':'
		stmt.HasExplicitType = true

		explicitTypeNode := p.parseTypeNode() // Parses int(w), string(w), void, file, RecordName
		if explicitTypeNode == nil {
			return nil
		}
		stmt.ExplicitType = explicitTypeNode
		finalType = explicitTypeNode.Name

		// Check if it's a record type
		if explicitTypeNode.IsRecord {
			if isConst {
				p.addError(identToken, "Cannot declare 'const' variable '%s' of record type '%s'", varName, finalType)
				// Continue parsing but mark invalid
			}
			finalWidth = explicitTypeNode.RecordInfo.TotalWidth
			recordTypeSymbol = explicitTypeNode.RecordInfo // Link variable to record type
			finalSymbolInfo = symbols.SymbolInfo{
				Name:             varName,
				Type:             finalType, // Store record name as type
				Width:            finalWidth,
				IsConst:          isConst,
				RecordTypeSymbol: recordTypeSymbol,
			}
			// --- Allow declaration without assignment for records ---
			if p.curTok.Type == token.TokenAssign {
				p.addError(p.curTok, "Direct assignment during declaration is not supported for record type '%s'. Declare first, then use READ or assign fields.", finalType)
				// Try to skip assignment part for recovery
				p.nextToken()                 // Consume '='
				p.parseExpression(PrecLowest) // Parse and discard expression
				// Fall through, symbol is defined, but assignment was error
			}
			// No '=' is expected/allowed here for records currently
		} else if finalType == "file" {
			// Explicit file declaration: `fh : file = input(...)`
			finalWidth = 0 // Files have no width
			finalSymbolInfo = symbols.SymbolInfo{
				Name:    varName,
				Type:    "file",
				IsConst: isConst, // Const file handle? Maybe disallow.
			}
			if isConst {
				p.addWarning(identToken, "Declaring 'const' file handle '%s' has no effect.", varName)
			}

			if p.curTok.Type != token.TokenAssign {
				p.addError(p.curTok, "Expected '=' after ': file' declaration for '%s'", varName)
				return nil
			}
			stmt.Token = p.curTok // Store '='
			p.nextToken()         // Consume '='

			valueExpr := p.parseExpression(PrecLowest)
			if valueExpr == nil {
				return nil
			}
			stmt.Value = valueExpr

			// Check RHS type
			if valueExpr.ResultType() != "file" {
				p.addSemanticError(valueExpr.GetToken(), "Expected input() or output() call after ': file =', got %s", valueExpr.ResultType())
				finalType = "unknown" // Mark as error
			} else {
				// Copy details from the resolved input/output expression symbol
				if fileExprSym := valueExpr.GetResolvedSymbol(); fileExprSym != nil {
					finalSymbolInfo.Mode = fileExprSym.Mode
					finalSymbolInfo.SystemFileName = fileExprSym.SystemFileName
					finalSymbolInfo.RecordTypeName = fileExprSym.RecordTypeName
					finalSymbolInfo.FileRecordTypeSymbol = fileExprSym.FileRecordTypeSymbol
				} else {
					p.addError(valueExpr.GetToken(), "Internal error: input/output expression did not resolve file info")
					finalType = "unknown"
				}
			}

		} else if finalType == "int" || finalType == "string" {
			// Standard explicit type: int(W) or string(W)
			if explicitTypeNode.Width <= 0 {
				p.addError(explicitTypeNode.Token, "Explicit width required for type '%s' is missing or invalid", finalType)
				return nil // Already errored in parseTypeNode probably
			}
			finalWidth = explicitTypeNode.Width
			finalSymbolInfo = symbols.SymbolInfo{
				Name:    varName,
				Type:    finalType,
				Width:   finalWidth,
				IsConst: isConst,
			}

			// Expect '=' assignment for non-record explicit types
			if p.curTok.Type != token.TokenAssign {
				p.addError(p.curTok, "Expected '=' after type specification '%s' for variable '%s'", explicitTypeNode.String(), varName)
				return nil
			}
			stmt.Token = p.curTok // Store '='
			p.nextToken()         // Consume '='

			valueExpr := p.parseExpression(PrecLowest)
			if valueExpr == nil {
				return nil
			}
			stmt.Value = valueExpr

			// Type check assignment
			valueType := valueExpr.ResultType()
			if valueType != "unknown" && valueType != finalType {
				p.addSemanticError(valueExpr.GetToken(), "Type mismatch: cannot assign %s to variable '%s' of type %s", valueType, varName, finalType)
				finalType = "unknown" // Mark as error
			}
			// Width check assignment (literal value vs declared width)
			valueWidth := valueExpr.ResultWidth()
			if litInt, ok := valueExpr.(*ast.IntegerLiteral); ok && finalType == "int" {
				reqWidth := lib.CalculateWidthForValue(litInt.Value)
				if reqWidth > finalWidth {
					p.addSemanticError(valueExpr.GetToken(), "Integer literal %s requires width %d, but variable '%s' declared with width %d", litInt.Token.Literal, reqWidth, varName, finalWidth)
				}
			} else if litStr, ok := valueExpr.(*ast.StringLiteral); ok && finalType == "string" {
				reqWidth := len(litStr.Value)
				if reqWidth > finalWidth {
					p.addSemanticError(valueExpr.GetToken(), "String literal length %d exceeds declared width %d for variable '%s'", reqWidth, finalWidth, varName)
				}
			} else if finalType != "unknown" && valueWidth > 0 && valueWidth > finalWidth {
				// Warning for non-literal expressions exceeding width
				p.addWarning(valueExpr.GetToken(), "Width of assigned expression (%d) exceeds variable '%s' declared width (%d). Potential truncation.", valueWidth, varName, finalWidth)
			}

		} else if finalType == "void" {
			p.addError(explicitTypeNode.Token, "Cannot declare variable '%s' with type 'void'", varName)
			finalType = "unknown"
		} else { // Unknown type name used after ':'
			p.addError(explicitTypeNode.Token, "Unknown type '%s'", finalType)
			finalType = "unknown"
			// Consume potential = value part
			if p.curTok.Type == token.TokenAssign {
				p.nextToken()
				p.parseExpression(PrecLowest) // Parse and discard
			}
		}

	default:
		p.addError(p.peekTok, "Expected ':=' or ':' after identifier '%s' in declaration, got '%s'", varName, p.peekTok.Type)
		return nil
	}

	// --- Define Symbol ---
	if !declaredInCurrent {

		// Use finalSymbolInfo which is populated based on the declaration type
		err := p.defineSymbol(varName, finalSymbolInfo)
		if err != nil {
			p.addSemanticError(stmt.Name.Token, "Internal Error: Failed to define variable '%s': %v", varName, err)
		} else {
			// Link the resolved symbol back to the AST identifier node
			resolvedSymPtr, _ := p.lookupSymbol(varName) // Look up immediately after defining
			if resolvedSymPtr != nil {
				stmt.Name.Symbol = resolvedSymPtr
				// Also link the resolved record type symbol if it was a record variable declaration
				if resolvedSymPtr.RecordTypeSymbol != nil {
					stmt.ResolvedRecordType = resolvedSymPtr.RecordTypeSymbol
				}
			} else {
				p.addError(stmt.Name.Token, "Internal Error: Failed to lookup symbol '%s' immediately after definition", varName)
			}
		}
	} else if !declaredInCurrent && (finalType == "unknown" || finalType == "void") {
		// Don't define symbol if type resolution failed
	}

	return stmt
}

// parseProcDeclarationStatement parses `proc name(params...) : retType { body }`
func (p *Parser) parseProcDeclarationStatement() *ast.ProcDeclarationStatement {
	startTok := p.curTok // Store 'proc' token
	stmt := &ast.ProcDeclarationStatement{Token: startTok}

	// Assume signature already collected in Pass 1 and exists in p.pass1Symbols

	if !p.expectPeek(token.TokenIdent) {
		return nil
	}
	procName := p.curTok.Literal
	stmt.Name = &ast.Identifier{Token: p.curTok, Value: procName}

	// Retrieve full symbol info defined from Pass 1
	procInfoPtr, exists := p.lookupSymbol(procName) // Look up in current (global) scope
	if !exists || procInfoPtr == nil || procInfoPtr.Type != "proc" {
		p.addSemanticError(stmt.Name.Token, "Internal Error: Procedure '%s' signature not found in scope during Pass 2", procName)
		// Attempt to skip body for recovery
		p.skipSignatureAndBody()
		return nil
	}
	stmt.Name.Symbol = procInfoPtr

	// --- Enter Proc Scope ---
	previousProcName := p.currentProcName
	p.currentProcName = procName
	p.pushScope() // Create new scope for params and locals
	stmt.LocalScope = p.currentScope
	defer func() {
		p.popScope() // Ensure scope is popped even on error return
		p.currentProcName = previousProcName
	}()

	// Parse Parameter List `(...)`
	if !p.expectPeek(token.TokenLParen) {
		return nil
	}
	// curTok is now '('

	// Pass 2: Parse params fully for AST, resolving types (including records)
	params, _, err := p.parseParameterList() // Pass 2 version needs full type resolution
	if err != nil {
		p.skipSignatureAndBody() // Attempt recovery
		return nil
	}
	stmt.Parameters = params
	// curTok is now ')'

	// Define parameters in the *local* scope
	for i, param := range stmt.Parameters {
		if param.Name != nil && param.TypeNode != nil {
			// Get param info from the globally stored proc symbol for consistency
			if i < len(procInfoPtr.ParamTypes) {
				paramInfo := symbols.SymbolInfo{
					Name:             param.Name.Value,
					Type:             procInfoPtr.ParamTypes[i], // Use type name from global info
					Width:            procInfoPtr.ParamWidths[i],
					IsConst:          false,                     // Params are not const
					RecordTypeSymbol: param.TypeNode.RecordInfo, // Link if param type node resolved to a record
				}
				defErr := p.defineSymbol(param.Name.Value, paramInfo)
				if defErr != nil {
					// Should only happen if parseParameterList allowed duplicates
					p.addSemanticError(param.Name.Token, "Internal Error: %v", defErr)
				} else {
					// Link param identifier in AST to its symbol
					paramSymPtr, _ := p.lookupCurrentScopeSymbol(param.Name.Value)
					param.Name.Symbol = paramSymPtr
				}
			} else {
				p.addError(param.Name.Token, "Internal Error: Parameter count mismatch between AST and symbol info for '%s'", procName)
			}
		}
	}

	// --- Parse Return Type `: type` ---
	if p.curTok.Type != token.TokenRParen {
		// This check is mainly defensive, parseParameterList should ensure this
		p.addError(p.curTok, "Internal parser error: Expected ')' after parameter parsing")
		p.skipSignatureAndBody()
		return nil
	}
	p.nextToken() // Consume the ')' explicitly.

	// Now curTok should be ':'
	if p.curTok.Type != token.TokenColon {
		p.addError(p.curTok, "Expected ':' for return type after '()', got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		p.skipSignatureAndBody()
		return nil
	}
	p.nextToken() // Consume ':'
	// Now curTok is the start of the type node ('void')

	// Pass 2: Parse return type fully, resolving records
	returnTypeNode := p.parseTypeNode()
	if returnTypeNode == nil {
		return nil
	}
	stmt.ReturnType = returnTypeNode

	// --- Semantic Check: Return type consistency with Pass 1 ---
	if returnTypeNode.Name != procInfoPtr.ReturnType {
		p.addError(returnTypeNode.Token, "Internal Error: Return type mismatch between Pass 1 ('%s') and Pass 2 ('%s') for '%s'", procInfoPtr.ReturnType, returnTypeNode.Name, procName)
		// Decide how critical this is. Maybe proceed but log error.
	}
	// Link return type symbol if it's a record
	if returnTypeNode.IsRecord {
		procInfoPtr.ReturnRecordTypeSymbol = returnTypeNode.RecordInfo
	}

	// --- Parse Procedure Body `{...}` ---
	if p.curTok.Type != token.TokenLBrace {
		p.addError(p.curTok, "Expected '{'")
		return nil
	}
	stmt.Body = p.parseBlockStatement() // Parses statements within the proc's scope
	if stmt.Body == nil {
		return nil
	}

	// TODO: Enhanced check for missing return statement in non-void procs

	return stmt
}

// parseTypeNode parses a type specification: `type(width)` or `void` or `RecordName` or `file`
// Returns the TypeNode or nil on error. Sets IsRecord flag and RecordInfo if applicable.
func (p *Parser) parseTypeNode() *ast.TypeNode {
	typeTok := p.curTok
	typeName := typeTok.Literal

	node := &ast.TypeNode{Token: typeTok, Name: typeName}

	switch typeTok.Type {
	case token.TokenVoid:
		node.IsVoid = true
		node.Width = 0
		p.nextToken() // Consume 'void'

	case token.TokenFileKeyword: // `file` type keyword
		node.Name = "file" // Canonical type name
		node.Width = 0
		p.nextToken() // Consume 'file'
		// No width allowed for file type
		if p.curTok.Type == token.TokenLParen {
			p.addError(p.curTok, "Width specification not allowed for 'file' type")
			p.skipParentheses() // Attempt recovery
			return nil
		}

	case token.TokenTypeLiteral: // int, string
		if typeName != "int" && typeName != "string" {
			p.addError(typeTok, "Internal Error: Unexpected TYPE_LITERAL '%s'", typeName)
			p.nextToken()
			return nil
		}
		p.nextToken() // Consume 'int' or 'string'
		// Check for explicit width `(width)`
		if p.curTok.Type == token.TokenLParen {
			p.nextToken() // Consume '('
			if p.curTok.Type != token.TokenInt {
				p.addError(p.curTok, "Expected integer width inside parentheses for type '%s'", typeName)
				if p.curTok.Type != token.TokenRParen {
					p.nextToken()
				} // Consume bad token
				if p.curTok.Type == token.TokenRParen {
					p.nextToken()
				} // Consume ')'
				return nil
			}
			node.WidthToken = p.curTok
			widthVal, err := strconv.Atoi(p.curTok.Literal)
			if err != nil || widthVal <= 0 {
				p.addError(p.curTok, "Invalid width '%s'. Must be positive integer.", p.curTok.Literal)
				// Keep width 0 to indicate error
			} else {
				node.Width = widthVal
			}
			p.nextToken() // Consume INT
			if p.curTok.Type != token.TokenRParen {
				p.addError(p.curTok, "Expected ')' after width specification, got %s ('%s')", p.curTok.Type, p.curTok.Literal)
				// Attempt to recover slightly by skipping until ) if possible? Or just return nil. Let's return nil.
				return nil
			}
			p.nextToken() // Consume the ')' <<<--- IMPORTANT
		} else {
			// No width provided - error? Or default? PARAMS/FIELDS require width.
			// Let the caller (parseParameter, parseRecordField, etc.) decide if width is mandatory.
			// Store 0 to indicate no explicit width was parsed.
			node.Width = 0
		}

	case token.TokenIdent: // Could be a record type name
		recordSymbol, exists := p.lookupSymbol(typeName) // Look up in current scope outwards
		if exists && recordSymbol.Type == "record" {
			node.IsRecord = true
			node.RecordInfo = recordSymbol
			node.Width = recordSymbol.TotalWidth // Store total width
			p.nextToken()                        // Consume record type IDENT
			// No width spec allowed for record types
			if p.curTok.Type == token.TokenLParen {
				p.addError(p.curTok, "Width specification not allowed for record type '%s'", typeName)
				p.skipParentheses() // Attempt recovery
				return nil
			}
		} else {
			p.addError(typeTok, "Unknown type name '%s'", typeName)
			p.nextToken() // Consume unknown type identifier
			return nil
		}

	default:
		p.addError(typeTok, "Expected type name (int, string, void, file, record name), got %s", typeTok.Type)
		p.nextToken() // Consume the unexpected token
		return nil
	}

	return node
}

// parseParameterList (Pass 2) - Parses params fully, resolving record types.
func (p *Parser) parseParameterList() ([]*ast.Parameter, []string, error) {
	params := []*ast.Parameter{}
	paramNames := []string{}
	paramNameSet := make(map[string]bool)

	if p.curTok.Type != token.TokenLParen {
		p.addError(p.curTok, "Internal Parser Error: Expected '('")
		return nil, nil, fmt.Errorf("internal error")
	}
	if p.peekTok.Type == token.TokenRParen {
		p.nextToken() // Consume '('
		return params, paramNames, nil
	}
	p.nextToken() // Consume '('

	// First param
	param, err := p.parseSingleParameter()
	if err != nil {
		return nil, nil, err
	}
	if _, duplicate := paramNameSet[param.Name.Value]; duplicate {
		p.addSemanticError(param.Name.Token, "Duplicate parameter name '%s'", param.Name.Value)
		return nil, nil, fmt.Errorf("duplicate parameter")
	}
	params = append(params, param)
	paramNames = append(paramNames, param.Name.Value)
	paramNameSet[param.Name.Value] = true

	// Subsequent params
	for p.curTok.Type == token.TokenComma {
		p.nextToken()                           // Consume ','
		if p.curTok.Type == token.TokenRParen { // Trailing comma
			p.addError(p.curTok, "Unexpected ')' after comma")
			return nil, nil, fmt.Errorf("trailing comma")
		}
		param, err := p.parseSingleParameter()
		if err != nil {
			return nil, nil, err
		}
		if _, duplicate := paramNameSet[param.Name.Value]; duplicate {
			p.addSemanticError(param.Name.Token, "Duplicate parameter name '%s'", param.Name.Value)
			return nil, nil, fmt.Errorf("duplicate parameter")
		}
		params = append(params, param)
		paramNames = append(paramNames, param.Name.Value)
		paramNameSet[param.Name.Value] = true
	}

	// Expect ')' - DO NOT CONSUME
	if p.curTok.Type != token.TokenRParen {
		p.addError(p.curTok, "Expected ',' or ')' after parameter, got %s", p.curTok.Type)
		return nil, nil, fmt.Errorf("syntax error in parameter list")
	}

	return params, paramNames, nil
}

// parseSingleParameter (Pass 2) - Uses full type resolution. Width mandatory for base types.
func (p *Parser) parseSingleParameter() (*ast.Parameter, error) {
	if p.curTok.Type != token.TokenIdent {
		p.addError(p.curTok, "Expected parameter name")
		return nil, fmt.Errorf("expected name")
	}
	ident := &ast.Identifier{Token: p.curTok, Value: p.curTok.Literal}

	if !p.expectPeek(token.TokenColon) {
		p.addError(p.peekTok, "Expected ':' after parameter name '%s'", ident.Value)
		return nil, fmt.Errorf("expected ':'")
	}
	p.nextToken() // Consume ':'

	typeNode := p.parseTypeNode() // Resolves int(w), string(w), RecordType
	if typeNode == nil {
		return nil, fmt.Errorf("invalid type for parameter '%s'", ident.Value)
	}
	if typeNode.IsVoid {
		p.addError(typeNode.Token, "Parameter '%s' cannot have type 'void'", ident.Value)
		return nil, fmt.Errorf("void parameter")
	}
	// Width check: Mandatory for int/string, irrelevant for records
	if !typeNode.IsRecord && typeNode.Width <= 0 {
		// parseTypeNode sets Width>0 only if explicitly provided and valid
		p.addError(typeNode.Token, "Explicit positive width required for parameter '%s' of type '%s'", ident.Value, typeNode.Name)
		return nil, fmt.Errorf("missing/invalid width for parameter")
	}

	return &ast.Parameter{Name: ident, TypeNode: typeNode}, nil
}

// parseBlockStatement parses `{ statements... }`
func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.curTok} // Store '{' token
	block.Statements = []ast.Statement{}

	// Expect curTok to be LBrace
	if p.curTok.Type != token.TokenLBrace {
		p.addError(p.curTok, "Internal Parser Error: Expected '{' to start block, got %s", p.curTok.Type)
		return nil
	}
	p.nextToken() // Consume '{'

	for p.curTok.Type != token.TokenRBrace && p.curTok.Type != token.TokenEOF {
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		} else {
			// TODO: If parseStatement returned nil due to an error, it should have consumed
			// the problematic token. If it was EOF, the loop condition handles it.
			// If it was some other reason, we might loop infinitely.
			// Safeguard: if errors occurred and we haven't reached the end or }, advance.
			if len(p.errors) > 0 && p.curTok.Type != token.TokenRBrace && p.curTok.Type != token.TokenEOF {
				p.nextToken()
			}
		}
	}

	if p.curTok.Type != token.TokenRBrace {
		// If EOF was reached before '}', add error
		p.addError(p.curTok, "Expected '}' to close block, found %s ('%s')", p.curTok.Type, p.curTok.Literal)
		return nil
	}

	p.nextToken() // Consume '}' - curTok is now after '}'
	return block
}

// skipBlock attempts recovery by consuming tokens until a matching '}'
func (p *Parser) skipBlock() {
	openCount := 1
	if p.curTok.Type != token.TokenLBrace {
		return
	}
	p.nextToken() // Consume initial '{'

	for openCount > 0 && p.curTok.Type != token.TokenEOF {
		switch p.curTok.Type {
		case token.TokenLBrace:
			openCount++
		case token.TokenRBrace:
			openCount--
		}
		// IMPORTANT: Check count *before* consuming the final brace
		if openCount == 0 {
			break
		}
		p.nextToken()
	}
	// Consume the final '}' AFTER the loop condition ensures openCount is 0
	if p.curTok.Type == token.TokenRBrace && openCount == 0 {
		p.nextToken()
	}
}

// parseReturnStatement (Pass 2) parses `return [expression]`
func (p *Parser) parseReturnStatement() ast.Statement {
	stmt := &ast.ReturnStatement{Token: p.curTok}
	returnTok := p.curTok
	p.nextToken() // Consume 'return'

	if p.currentProcName == "" {
		p.addError(returnTok, "'return' statement outside of procedure")
		return nil
	}
	procInfo, ok := p.lookupSymbol(p.currentProcName) // Look up in current scope outwards
	if !ok || procInfo.Type != "proc" {
		p.addError(returnTok, "Internal Error: Cannot find procedure info for '%s'", p.currentProcName)
		return nil
	}

	hasPotentialValue := p.isExpressionStart(p.curTok)

	if procInfo.ReturnType == "void" {
		if hasPotentialValue {
			p.addSemanticError(p.curTok, "Cannot return value from void procedure '%s'", p.currentProcName)
			p.skipExpressionTokens()
		}
		stmt.ReturnValue = nil
	} else { // Expecting non-void return
		if !hasPotentialValue {
			p.addError(returnTok, "Expected expression after 'return' for non-void procedure '%s'", p.currentProcName)
			return nil
		}
		returnValue := p.parseExpression(PrecLowest)
		if returnValue == nil {
			return nil
		}
		stmt.ReturnValue = returnValue

		// --- Semantic Check: Return type match ---
		valueType := returnValue.ResultType() // Can be "int", "string", "RecordName"
		expectedType := procInfo.ReturnType   // Can also be "int", "string", "RecordName"

		if valueType == "void" {
			p.addSemanticError(returnValue.GetToken(), "Cannot return result of void expression/call from '%s'", p.currentProcName)
		} else if valueType != "unknown" && valueType != expectedType {
			p.addSemanticError(returnValue.GetToken(), "Type mismatch: cannot return %s from procedure '%s' expecting %s", valueType, p.currentProcName, expectedType)
		}

		// --- Semantic Check: Return width compatibility (Warning/Error) ---
		// Only applicable if NOT returning a record type
		valueWidth := returnValue.ResultWidth()
		declaredWidth := procInfo.ReturnWidth // This is 0 if proc returns a record

		if procInfo.ReturnRecordTypeSymbol == nil && valueType == expectedType { // Check only non-record compatible types
			if valueWidth > 0 && declaredWidth > 0 && valueWidth > declaredWidth {
				isLiteralReturn := false // Check if returnValue is IntLiteral or StringLiteral
				if _, ok := returnValue.(*ast.IntegerLiteral); ok {
					isLiteralReturn = true
				}
				if _, ok := returnValue.(*ast.StringLiteral); ok {
					isLiteralReturn = true
				}

				if isLiteralReturn {
					p.addSemanticError(returnValue.GetToken(), "Literal return value width (%d) exceeds declared width (%d) for '%s'", valueWidth, declaredWidth, p.currentProcName)
				} else {
					p.addWarning(returnValue.GetToken(), "Width of returned expression (%d) may exceed declared width (%d) for '%s'.", valueWidth, declaredWidth, p.currentProcName)
				}
			}
		}
	}
	return stmt
}

// isExpressionStart checks if a token can potentially start an expression.
func (p *Parser) isExpressionStart(tok token.Token) bool {
	switch tok.Type {
	case token.TokenInt, token.TokenString, token.TokenIdent, token.TokenLParen, token.TokenMinus, token.TokenPlus: // Add others if needed (unary ops?)
		return true
	default:
		return false
	}
}

// skipExpressionTokens tries to consume tokens that likely form an expression.
// Basic recovery for syntax errors. Stops at potential statement terminators.
func (p *Parser) skipExpressionTokens() {
	for !p.isStatementEnd(p.curTok) && p.curTok.Type != token.TokenEOF {
		// TODO: Handle nested parentheses/braces correctly if needed
		p.nextToken()
	}
}

// isStatementEnd checks for tokens that typically end a simple statement (in Grace's case, context-dependent)
func (p *Parser) isStatementEnd(tok token.Token) bool {
	// Newlines are handled implicitly by the main parsing loop.
	// RBrace usually ends a block/statement within it.
	// EOF ends the program.
	// Specific keywords might end expressions (e.g., 'proc', 'const' starting next line).
	switch tok.Type {
	case token.TokenRBrace, token.TokenEOF, token.TokenProc, token.TokenConst, token.TokenReturn, token.TokenPrint:
		return true
	default:
		return false
	}
}

// parseExpressionStatement handles statements that are just expressions (like proc calls)
func (p *Parser) parseExpressionStatement() ast.Statement {
	startTok := p.curTok
	stmt := &ast.ExpressionStatement{Token: startTok}

	expr := p.parseExpression(PrecLowest)
	if expr == nil {
		return nil
	}
	stmt.Expression = expr

	// --- Semantic Check: Pointless or invalid statements ---
	switch node := stmt.Expression.(type) {
	case *ast.ProcCallExpression:
		// Check if the *result* of a non-void call is ignored (Warning)
		if node.ResultType() != "void" {
			p.addWarning(stmt.Token, "Result of non-void procedure call '%s' is ignored.", node.Function.Value)
		}
	case *ast.IntegerLiteral:
		p.addWarning(stmt.Token, "Integer literal used as statement has no effect.")
	case *ast.StringLiteral:
		p.addWarning(stmt.Token, "String literal used as statement has no effect.")
	case *ast.Identifier:
		// Check if it's a variable identifier OR a procedure identifier *used incorrectly*
		if node.Symbol.Type == "proc" {
			p.addSemanticError(stmt.Token, "Procedure '%s' used as statement without calling it.", node.Value)
		} else if node.Symbol.Type != "unknown" { // Don't warn about unknown types again
			p.addWarning(stmt.Token, "Variable '%s' used as statement has no effect.", node.Value)
		}
	case *ast.BinaryExpression:
		p.addWarning(stmt.Token, "Binary expression used as statement has no effect.")
	}

	return stmt // Return the statement node
}

// parseReassignmentStatement parses `name = value`
func (p *Parser) parseReassignmentStatement() ast.Statement {
	stmt := &ast.ReassignmentStatement{}
	if p.curTok.Type != token.TokenIdent {
		p.addError(p.curTok, "Internal Error: Expected identifier")
		return nil
	}
	identToken := p.curTok
	stmt.Name = &ast.Identifier{Token: identToken, Value: p.curTok.Literal}
	varName := stmt.Name.Value

	// --- Semantic Check: Declaration, Constness, Type ---
	targetSymbolPtr, declared := p.lookupSymbol(varName)
	isValidTarget := false
	var targetType string
	var targetWidth int
	var targetRecordSymbol *symbols.SymbolInfo

	if !declared {
		p.addSemanticError(identToken, "Cannot assign to undeclared variable '%s'", varName)
	} else {
		stmt.Name.Symbol = targetSymbolPtr // Link identifier in AST
		targetType = targetSymbolPtr.Type
		targetWidth = targetSymbolPtr.Width
		targetRecordSymbol = targetSymbolPtr.RecordTypeSymbol // Will be non-nil if it's a record variable

		if targetSymbolPtr.Type == "proc" {
			p.addSemanticError(identToken, "Cannot assign to procedure '%s'", varName)
		} else if targetSymbolPtr.Type == "file" {
			p.addSemanticError(identToken, "Cannot reassign file handle '%s'. Declare new handles with ':='.", varName)
		} else if targetSymbolPtr.IsConst {
			p.addSemanticError(identToken, "Cannot assign to constant '%s'", varName)
		} else {
			isValidTarget = true
		}
	}

	// --- Parse RHS ---
	if !p.expectPeek(token.TokenAssign) {
		return nil
	}
	stmt.Token = p.curTok
	p.nextToken() // Consume '='
	valueExpr := p.parseExpression(PrecLowest)
	if valueExpr == nil {
		return nil
	}
	stmt.Value = valueExpr

	// --- Semantic Checks: Type and Width ---
	if isValidTarget {
		valueType := valueExpr.ResultType()
		valueWidth := valueExpr.ResultWidth()
		valueRecordSymbol := valueExpr.GetResolvedSymbol() // Get record symbol if RHS resolves to one

		// Check void assignment
		if valueType == "void" {
			p.addSemanticError(valueExpr.GetToken(), "Cannot assign result of void expression to '%s'", varName)
		} else if valueType != "unknown" && targetType != "unknown" {
			// --- Type Compatibility ---
			// Check 1: Basic types (int, string) must match exactly
			isBasicTarget := (targetType == "int" || targetType == "string")
			isBasicValue := (valueType == "int" || valueType == "string")

			if isBasicTarget && isBasicValue && targetType != valueType {
				p.addSemanticError(valueExpr.GetToken(), "Type mismatch: cannot assign %s to variable '%s' of type %s", valueType, varName, targetType)
			} else if targetRecordSymbol != nil && valueRecordSymbol != nil {
				// Check 2: Record types must match exactly (by name/symbol pointer)
				if targetRecordSymbol.Name != valueRecordSymbol.Name { // Compare by original name for error message
					p.addSemanticError(valueExpr.GetToken(), "Type mismatch: cannot assign record of type %s to variable '%s' of type %s", valueRecordSymbol.Name, varName, targetRecordSymbol.Name)
				} else {
					// TODO: Emit MOVE CORRESPONDING for record assignment later
				}
			} else if targetRecordSymbol != nil && valueRecordSymbol == nil {
				// Check 3: Cannot assign basic type to record var
				p.addSemanticError(valueExpr.GetToken(), "Type mismatch: cannot assign %s to record variable '%s' of type %s", valueType, varName, targetRecordSymbol.Name)
			} else if targetRecordSymbol == nil && valueRecordSymbol != nil {
				// Check 4: Cannot assign record type to basic var
				p.addSemanticError(valueExpr.GetToken(), "Type mismatch: cannot assign record of type %s to variable '%s' of type %s", valueRecordSymbol.Name, varName, targetType)
			}

			// --- Width Compatibility (only for basic types) ---
			if isBasicTarget && isBasicValue && targetType == valueType {
				if valueWidth > 0 && targetWidth > 0 && valueWidth > targetWidth {
					isLiteralRHS := false
					if _, ok := valueExpr.(*ast.IntegerLiteral); ok {
						isLiteralRHS = true
					}
					if _, ok := valueExpr.(*ast.StringLiteral); ok {
						isLiteralRHS = true
					}

					if isLiteralRHS {
						p.addSemanticError(valueExpr.GetToken(), "Value width %d exceeds variable '%s' width %d", valueWidth, varName, targetWidth)
					} else {
						p.addWarning(valueExpr.GetToken(), "Width of assigned value (%d) might exceed variable '%s' width (%d).", valueWidth, varName, targetWidth)
					}
				}
			}
		}
	}

	return stmt
}

// parsePrintStatement parses `print(expression)`
func (p *Parser) parsePrintStatement() ast.Statement {
	stmt := &ast.PrintStatement{Token: p.curTok} // Store PRINT token

	// Expect '('
	if !p.expectPeek(token.TokenLParen) {
		// Error added by expectPeek
		return nil
	}
	// curTok is now '('

	p.nextToken() // Consume '(', curTok is now start of expression

	// Parse the expression to print
	valueExpr := p.parseExpression(PrecLowest)
	if valueExpr == nil {
		// Error handled in parseExpression. Check if we need ')'.
		if p.curTok.Type == token.TokenRParen {
			p.nextToken()
		} // Consume ')' if found for recovery
		return nil
	}
	stmt.Value = valueExpr
	// curTok is now after the parsed expression

	// Semantic check: Ensure expression type is printable (int, string)
	valueType := valueExpr.ResultType()
	if valueType == "void" {
		p.addSemanticError(valueExpr.GetToken(), "Cannot print result of void procedure call")
	} else if valueType == "unknown" {
		// Error likely already reported by expression parser or variable resolution
		// No additional error here unless debugging internal issues.
	} else if valueType != "int" && valueType != "string" { // Extend if more printable types added
		p.addSemanticError(valueExpr.GetToken(), "Cannot print value of type '%s'", valueType)
	}

	// Expect ')'
	if p.curTok.Type != token.TokenRParen {
		p.addError(p.curTok, "Expected ')' after print expression, got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		// Attempt recovery? If we see a token that could start next statement, maybe return stmt? Risky.
		// Don't consume if not ')', let the outer loop handle the unexpected token.
		return nil
	}
	p.nextToken() // Consume the ')'

	return stmt
}

// --- I/O Statement Parsers (Pass 2) ---

func (p *Parser) parseReadStatement() ast.Statement {
	stmt := &ast.ReadStatement{Token: p.curTok}
	if !p.expectPeek(token.TokenIdent) { // Expect file handle name
		p.addError(p.peekTok, "Expected file handle identifier after 'read'")
		return nil
	}
	fileIdent := &ast.Identifier{Token: p.curTok, Value: p.curTok.Literal}
	stmt.FileHandle = fileIdent

	// Lookup file handle symbol
	fileSym, ok := p.lookupSymbol(fileIdent.Value)
	if !ok {
		p.addSemanticError(fileIdent.Token, "Undeclared file handle '%s'", fileIdent.Value)
		// Continue parsing to find INTO error maybe
	} else if fileSym.Type != "file" {
		p.addSemanticError(fileIdent.Token, "'%s' is not a file handle (it's a %s)", fileIdent.Value, fileSym.Type)
	} else if fileSym.Mode != "input" {
		p.addSemanticError(fileIdent.Token, "File handle '%s' was not opened for input", fileIdent.Value)
	} else {
		stmt.FileHandle.Symbol = fileSym // Link AST node to symbol
		stmt.FileHandleSymbol = fileSym  // Store for emitter convenience
	}

	if !p.expectPeek(token.TokenInto) { // Expect 'into'
		p.addError(p.peekTok, "Expected 'into' after file handle '%s' in read statement", fileIdent.Value)
		return nil
	}

	if !p.expectPeek(token.TokenIdent) { // Expect record variable name
		p.addError(p.peekTok, "Expected record variable identifier after 'into'")
		return nil
	}
	recordVarIdent := &ast.Identifier{Token: p.curTok, Value: p.curTok.Literal}
	stmt.RecordVar = recordVarIdent

	// Lookup record variable symbol
	recVarSym, ok := p.lookupSymbol(recordVarIdent.Value)
	if !ok {
		p.addSemanticError(recordVarIdent.Token, "Undeclared record variable '%s'", recordVarIdent.Value)
	} else if recVarSym.IsConst {
		p.addSemanticError(recordVarIdent.Token, "Cannot read into constant '%s'", recordVarIdent.Value)
	} else if recVarSym.RecordTypeSymbol == nil { // Check if it's actually a record variable
		p.addSemanticError(recordVarIdent.Token, "'%s' is not a record variable (it's a %s)", recordVarIdent.Value, recVarSym.Type)
	} else {
		stmt.RecordVar.Symbol = recVarSym // Link AST node
		stmt.RecordVarSymbol = recVarSym  // Store for emitter

		// Check if record variable type matches file's expected record type
		if fileSym != nil && fileSym.Type == "file" && fileSym.FileRecordTypeSymbol != nil {
			if fileSym.FileRecordTypeSymbol.Name != recVarSym.RecordTypeSymbol.Name { // Compare record type names
				p.addSemanticError(recordVarIdent.Token, "Type mismatch: file '%s' expects record type '%s', but variable '%s' is type '%s'",
					fileIdent.Value, fileSym.RecordTypeName, recordVarIdent.Value, recVarSym.RecordTypeSymbol.Name)
			}
		} else if fileSym != nil && fileSym.Type == "file" {
			p.addError(fileIdent.Token, "Internal Error: File handle '%s' has no associated record type information", fileIdent.Value)
		}
	}

	p.nextToken()

	return stmt
}

func (p *Parser) parseWriteStatement() ast.Statement {
	stmt := &ast.WriteStatement{Token: p.curTok}
	if !p.expectPeek(token.TokenIdent) { // Expect file handle name
		p.addError(p.peekTok, "Expected file handle identifier after 'write'")
		return nil
	}
	fileIdent := &ast.Identifier{Token: p.curTok, Value: p.curTok.Literal}
	stmt.FileHandle = fileIdent

	// Lookup file handle symbol
	fileSym, ok := p.lookupSymbol(fileIdent.Value)
	if !ok {
		p.addSemanticError(fileIdent.Token, "Undeclared file handle '%s'", fileIdent.Value)
	} else if fileSym.Type != "file" {
		p.addSemanticError(fileIdent.Token, "'%s' is not a file handle (it's a %s)", fileIdent.Value, fileSym.Type)
	} else if fileSym.Mode != "output" {
		p.addSemanticError(fileIdent.Token, "File handle '%s' was not opened for output", fileIdent.Value)
	} else {
		stmt.FileHandle.Symbol = fileSym
		stmt.FileHandleSymbol = fileSym
	}

	if !p.expectPeek(token.TokenFrom) { // Expect 'from'
		p.addError(p.peekTok, "Expected 'from' after file handle '%s' in write statement", fileIdent.Value)
		return nil
	}

	if !p.expectPeek(token.TokenIdent) { // Expect record variable name
		p.addError(p.peekTok, "Expected record variable identifier after 'from'")
		return nil
	}
	recordVarIdent := &ast.Identifier{Token: p.curTok, Value: p.curTok.Literal}
	stmt.RecordVar = recordVarIdent

	// Lookup record variable symbol
	recVarSym, ok := p.lookupSymbol(recordVarIdent.Value)
	if !ok {
		p.addSemanticError(recordVarIdent.Token, "Undeclared record variable '%s'", recordVarIdent.Value)
	} else if recVarSym.RecordTypeSymbol == nil { // Check if it's a record variable
		p.addSemanticError(recordVarIdent.Token, "'%s' is not a record variable (it's a %s)", recordVarIdent.Value, recVarSym.Type)
	} else {
		stmt.RecordVar.Symbol = recVarSym
		stmt.RecordVarSymbol = recVarSym

		// Check if record variable type matches file's expected record type
		if fileSym != nil && fileSym.Type == "file" && fileSym.FileRecordTypeSymbol != nil {
			if fileSym.FileRecordTypeSymbol.Name != recVarSym.RecordTypeSymbol.Name {
				p.addSemanticError(recordVarIdent.Token, "Type mismatch: file '%s' expects record type '%s', but variable '%s' is type '%s'",
					fileIdent.Value, fileSym.RecordTypeName, recordVarIdent.Value, recVarSym.RecordTypeSymbol.Name)
			}
		} else if fileSym != nil && fileSym.Type == "file" {
			p.addError(fileIdent.Token, "Internal Error: File handle '%s' has no associated record type information", fileIdent.Value)
		}
	}

	p.nextToken()

	return stmt
}

// --- Expression Parsing (Pratt Parser is Pass 2) ---

// Type definitions for Pratt parsing functions
type (
	prefixParseFn func() ast.Expression               // NUD (Null Denotation)
	infixParseFn  func(ast.Expression) ast.Expression // LED (Left Denotation)
)

var (
	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
)

// registerPrefix associates a token type with its prefix parsing function.
func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	if prefixParseFns == nil {
		prefixParseFns = make(map[token.TokenType]prefixParseFn)
	}
	prefixParseFns[tokenType] = fn
}

// registerInfix associates a token type with its infix parsing function.
func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	if infixParseFns == nil {
		infixParseFns = make(map[token.TokenType]infixParseFn)
	}
	infixParseFns[tokenType] = fn
}

// Initialize Pratt parser functions (call this in NewParser or lazily)
func (p *Parser) initializePratt() {
	// Prefixes (NUDs)
	p.registerPrefix(token.TokenIdent, p.parseIdentifier)
	p.registerPrefix(token.TokenInt, p.parseIntegerLiteral)
	p.registerPrefix(token.TokenString, p.parseStringLiteral)
	p.registerPrefix(token.TokenLParen, p.parseGroupedExpression)
	p.registerPrefix(token.TokenInput, p.parseInputOutputExpression)
	p.registerPrefix(token.TokenOutput, p.parseInputOutputExpression)
	// p.registerPrefix(TokenMinus, p.parsePrefixExpression) // Example for unary minus
	// p.registerPrefix(TokenPlus, p.parsePrefixExpression) // Example for unary plus

	// Infixes (LEDs)
	p.registerInfix(token.TokenPlus, p.parseInfixExpression)
	p.registerInfix(token.TokenMinus, p.parseInfixExpression)
	p.registerInfix(token.TokenAsterisk, p.parseInfixExpression)
	p.registerInfix(token.TokenSlash, p.parseInfixExpression)
	p.registerInfix(token.TokenLParen, p.parseProcCallExpression) // For func(...) style calls
}

// parseExpression is the main entry point for Pratt parsing.
func (p *Parser) parseExpression(precedence int) ast.Expression {
	// Ensure Pratt functions are registered
	if prefixParseFns == nil {
		p.initializePratt()
	}

	// NUD (Prefix) lookup
	prefix := prefixParseFns[p.curTok.Type]
	if prefix == nil {
		p.addError(p.curTok, "No prefix parsing function found for token type %s ('%s')", p.curTok.Type, p.curTok.Literal)
		return nil
	}
	leftExpr := prefix() // Call the prefix function

	// LED (Infix) loop
	for precedence < tokenPrecedence(p.curTok) {
		infix := infixParseFns[p.curTok.Type]
		if infix == nil {
			// If no infix function, we're done with this precedence level
			return leftExpr
		}
		// We have an infix operator or call
		leftExpr = infix(leftExpr) // Pass the left expression to the infix function
		if leftExpr == nil {       // Check if infix parsing failed
			return nil
		}
	}

	return leftExpr
}

// --- Pratt NUD/Prefix Functions ---

func (p *Parser) parseIntegerLiteral() ast.Expression {
	token := p.curTok
	val, err := strconv.ParseInt(token.Literal, 10, 64) // Use 64-bit intermediate parsing
	if err != nil {
		p.addError(token, "Could not parse integer literal '%s': %v", token.Literal, err)
		p.nextToken() // Consume bad token
		return nil
	}
	// TODO: Check if val exceeds COBOL limits if necessary (e.g., 18 digits)

	lit := &ast.IntegerLiteral{
		Token: token,
		Value: int(val), // Store as standard Go int
		Width: lib.CalculateWidthForValue(int(val)),
	}
	p.nextToken() // Consume the integer token
	return lit
}

func (p *Parser) parseStringLiteral() ast.Expression {
	token := p.curTok
	expr := &ast.StringLiteral{
		Token: token,
		Value: token.Literal, // Literal value already extracted by lexer
		Width: len(token.Literal),
	}
	p.nextToken() // Consume the string token
	return expr
}

// parseIdentifier (Pratt Prefix/NUD Function for Pass 2)
func (p *Parser) parseIdentifier() ast.Expression {
	token := p.curTok
	varName := token.Literal

	expr := &ast.Identifier{Token: token, Value: varName, Symbol: nil}

	// Use lookupSymbol to find in current or outer scopes
	symbolInfoPtr, declared := p.lookupSymbol(varName)

	if !declared {
		p.addSemanticError(token, "Identifier '%s' used before declaration", varName)
	} else {
		expr.Symbol = symbolInfoPtr
	}
	p.nextToken() // Consume identifier
	return expr
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	startToken := p.curTok // Store '('
	p.nextToken()          // Consume '('

	// Parse the inner expression using the lowest precedence
	expr := p.parseExpression(PrecLowest)
	if expr == nil {
		// Error already reported by parseExpression. Attempt recovery.
		p.skipUntil(token.TokenRParen) // Skip until ')'
		if p.curTok.Type == token.TokenRParen {
			p.nextToken()
		} // Consume ')' if found
		return nil
	}

	// Expect ')'
	if p.curTok.Type != token.TokenRParen {
		p.addError(p.curTok, "Expected ')' after expression in parentheses, got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		// Don't consume if not ')', let outer parsers handle recovery
		return nil
	}
	p.nextToken() // Consume ')'

	// Wrap in GroupedExpression node OR return inner expr directly.
	// Wrapping helps if we need to distinguish (a+b) from a+b later.
	// return expr // Simpler if grouping is only for precedence
	return &ast.GroupedExpression{Token: startToken, Expression: expr} // Keep structure
}

// parseInputOutputExpression
func (p *Parser) parseInputOutputExpression() ast.Expression {
	modeToken := p.curTok
	mode := modeToken.Literal // "input" or "output"

	if !p.expectPeek(token.TokenLParen) {
		return nil
	}
	p.nextToken() // Consume '('

	// Expect system file name (string literal)
	if p.curTok.Type != token.TokenString {
		p.addError(p.curTok, "Expected system file name (string literal) as first argument to %s()", mode)
		p.skipUntilCommaOrEnd(token.TokenRParen)
		if p.curTok.Type == token.TokenRParen {
			p.nextToken()
		}
		return nil
	}
	sysFileNameLit := p.parseStringLiteral().(*ast.StringLiteral) // Use existing parser

	// Expect comma
	if p.curTok.Type != token.TokenComma {
		p.addError(p.curTok, "Expected ',' after file name in %s()", mode)
		p.skipUntil(token.TokenRParen)
		if p.curTok.Type == token.TokenRParen {
			p.nextToken()
		}
		return nil
	}
	p.nextToken() // Consume ','

	// Expect record type name (identifier)
	if p.curTok.Type != token.TokenIdent {
		p.addError(p.curTok, "Expected record type name (identifier) as second argument to %s()", mode)
		p.skipUntil(token.TokenRParen)
		if p.curTok.Type == token.TokenRParen {
			p.nextToken()
		}
		return nil
	}
	recordTypeIdent := p.parseIdentifier().(*ast.Identifier) // Use existing parser

	// Semantic check: Lookup record type name globally
	recordSym, ok := p.lookupSymbol(recordTypeIdent.Value) // Should be global
	if !ok {
		p.addSemanticError(recordTypeIdent.Token, "Undeclared record type '%s' used in %s()", recordTypeIdent.Value, mode)
		recordSym = &symbols.SymbolInfo{Type: "unknown"} // Avoid nil pointer checks later, mark as error state
	} else if recordSym.Type != "record" {
		p.addSemanticError(recordTypeIdent.Token, "'%s' is not a record type (it's a %s)", recordTypeIdent.Value, recordSym.Type)
		recordSym.Type = "unknown" // Mark as error state
	}

	// Expect closing parenthesis
	if p.curTok.Type != token.TokenRParen {
		p.addError(p.curTok, "Expected ')' after arguments in %s()", mode)
		// Don't consume, let outer parser handle if possible
		return nil
	}
	p.nextToken() // Consume ')'

	// Create a temporary symbol info for the file handle this expression represents.
	// This info will be copied when assigned via `fh := input(...)`
	fileInfo := &symbols.SymbolInfo{
		Type:                 "file", // Indicates the result type
		Mode:                 mode,
		SystemFileName:       sysFileNameLit.Value,
		RecordTypeName:       recordTypeIdent.Value,
		FileRecordTypeSymbol: nil, // Initialize
	}
	// Link to the actual record symbol if it was found correctly
	if recordSym.Type == "record" {
		fileInfo.FileRecordTypeSymbol = recordSym
	}

	// Create appropriate AST node
	if mode == "input" {
		return &ast.InputExpression{
			Token:              modeToken,
			SystemFileName:     sysFileNameLit,
			RecordTypeName:     recordTypeIdent,
			ResolvedFileInfo:   fileInfo,  // Store the temp info
			ResolvedRecordInfo: recordSym, // Store the looked-up record info
		}
	} else { // mode == "output"
		return &ast.OutputExpression{
			Token:              modeToken,
			SystemFileName:     sysFileNameLit,
			RecordTypeName:     recordTypeIdent,
			ResolvedFileInfo:   fileInfo,  // Store the temp info
			ResolvedRecordInfo: recordSym, // Store the looked-up record info
		}
	}
}

// --- Pratt LED/Infix Functions ---

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	opToken := p.curTok
	operator := opToken.Literal
	precedence := tokenPrecedence(opToken)
	p.nextToken() // Consume operator
	right := p.parseExpression(precedence)
	if right == nil {
		return nil
	}

	// --- Semantic Checks ---
	leftType := left.ResultType()
	rightType := right.ResultType()
	isValidOp := false

	// Disallow operations on record or file types directly
	if leftType == "record" || rightType == "record" || leftType == "file" || rightType == "file" {
		p.addSemanticError(opToken, "Operator '%s' cannot be applied to record or file types ('%s', '%s')", operator, leftType, rightType)
		// Don't attempt folding, result type is unknown/error
		return &ast.BinaryExpression{Token: opToken, Left: left, Operator: operator, Right: right} // Return node for structure, but type is error
	}

	// --- Existing Checks for int/string ---
	if operator == "+" {
		if leftType == "int" && rightType == "int" {
			isValidOp = true
		}
		if leftType == "string" && rightType == "string" {
			isValidOp = true
		}
	} else if operator == "-" || operator == "*" || operator == "/" {
		if leftType == "int" && rightType == "int" {
			isValidOp = true
		}
	}

	if leftType == "void" || rightType == "void" {
		p.addSemanticError(opToken, "Cannot use void value in binary operation '%s'", operator)
		isValidOp = false
	} else if !isValidOp {
		if leftType != "unknown" && rightType != "unknown" { // Avoid redundant errors
			p.addSemanticError(opToken, "Operator '%s' cannot be applied to types '%s' and '%s'", operator, leftType, rightType)
		}
	} else if operator == "/" {
		if lit, ok := right.(*ast.IntegerLiteral); ok && lit.Value == 0 {
			p.addSemanticError(opToken, "Division by literal zero")
		}
	}

	// --- Constant Folding ---
	if isValidOp { // Only fold if operation is valid for the types
		foldedExpr := p.tryConstantFolding(left, right, opToken)
		if foldedExpr != nil {
			return foldedExpr
		}
	}

	// Build the BinaryExpression node
	expr := &ast.BinaryExpression{Token: opToken, Left: left, Operator: operator, Right: right}
	// ResultType/Width calculated lazily by the node
	return expr
}

// parseProcCallExpression (Pratt Infix/LED Function for Pass 2)
func (p *Parser) parseProcCallExpression(function ast.Expression) ast.Expression {
	ident, ok := function.(*ast.Identifier)
	if !ok {
		p.addError(p.curTok, "Expected identifier before '(' for procedure call")
		return nil
	}

	procName := ident.Value
	if ident.Symbol == nil {
		p.skipParentheses()
		return nil
	} // Undeclared error already added
	if ident.Symbol.Type != "proc" {
		p.addSemanticError(ident.Token, "'%s' is not a procedure", procName)
		p.skipParentheses()
		return nil
	}
	symbolInfo := *ident.Symbol

	expr := &ast.ProcCallExpression{
		Token:               ident.Token,
		Function:            ident,
		ResolvedReturnType:  symbolInfo.ReturnType,             // Name of return type (e.g., "int", "MyRecord")
		ResolvedReturnWidth: symbolInfo.ReturnWidth,            // Width if int/string, 0 otherwise
		ResolvedRecordType:  symbolInfo.ReturnRecordTypeSymbol, // Link if returns record
	}

	// Parse Arguments `(...)`
	if p.curTok.Type != token.TokenLParen { // Should be '(' as this is infix call
		p.addError(p.curTok, "Internal Error: Expected '(' for procedure call infix")
		return nil
	}
	p.nextToken() // Consume '('
	var err error
	expr.Arguments, err = p.parseExpressionList(token.TokenRParen) // Parses until ')'
	if err != nil {
		return nil
	}
	// curTok is now after ')'

	// --- Semantic Checks: Argument Count & Types ---
	if len(expr.Arguments) != len(symbolInfo.ParamNames) {
		p.addSemanticError(ident.Token, "Procedure '%s' expects %d args, got %d", procName, len(symbolInfo.ParamNames), len(expr.Arguments))
	} else {
		for i, argExpr := range expr.Arguments {
			argType := argExpr.ResultType() // e.g., "int", "MyRecord"
			argWidth := argExpr.ResultWidth()
			// argRecordSymbol := argExpr.GetResolvedSymbol() // Record symbol if arg is record var/expr

			if i >= len(symbolInfo.ParamTypes) || i >= len(symbolInfo.ParamWidths) {
				p.addError(ident.Token, "Internal Error: Arg count mismatch for '%s'", procName)
				break
			}
			expectedType := symbolInfo.ParamTypes[i] // e.g., "int", "MyRecord"
			expectedWidth := symbolInfo.ParamWidths[i]
			// TODO: Need expected record type symbol from procInfo for comparison
			// expectedParamRecordSymbol := symbolInfo.ParamRecordTypeSymbols[i] // Need to add this to SymbolInfo

			argToken := argExpr.GetToken()

			// Basic Type Check
			if argType == "void" {
				p.addSemanticError(argToken, "Cannot pass void value as argument %d to '%s'", i+1, procName)
			} else if argType != "unknown" && expectedType != "unknown" {
				// Check if types match by name (covers int, string, and record names)
				if argType != expectedType {
					p.addSemanticError(argToken, "Type mismatch for arg %d of '%s'. Expected '%s', got '%s'", i+1, procName, expectedType, argType)
				} else {
					// --- Width Check (only if types match and are NOT records) ---
					_, isRecordArg := p.lookupSymbol(argType) // Quick check if argType is a known record name
					if !isRecordArg {                         // Only check width for non-record types like int/string
						if argWidth > 0 && expectedWidth > 0 && argWidth > expectedWidth {
							isLiteralArg := false // Check if argExpr is IntLiteral or StringLiteral
							if _, ok := argExpr.(*ast.IntegerLiteral); ok {
								isLiteralArg = true
							}
							if _, ok := argExpr.(*ast.StringLiteral); ok {
								isLiteralArg = true
							}

							if isLiteralArg {
								p.addSemanticError(argToken, "Arg %d width %d exceeds parameter width %d for '%s'", i+1, argWidth, expectedWidth, procName)
							} else {
								p.addWarning(argToken, "Width of arg %d (%d) might exceed param width (%d) for '%s'.", i+1, argWidth, expectedWidth, procName)
							}
						}
					}
				}
			}
		}
	}
	return expr
}

// --- Helper Functions ---

// tryConstantFolding attempts to fold binary operations on literals.
// Returns the folded literal expression (IntegerLiteral or StringLiteral) or nil if folding is not possible.
func (p *Parser) tryConstantFolding(left, right ast.Expression, opToken token.Token) ast.Expression {
	leftLitInt, leftIsInt := left.(*ast.IntegerLiteral)
	rightLitInt, rightIsInt := right.(*ast.IntegerLiteral)
	leftLitStr, leftIsStr := left.(*ast.StringLiteral)
	rightLitStr, rightIsStr := right.(*ast.StringLiteral)
	operator := opToken.Literal

	// Integer Folding (+, -, *, /)
	if leftIsInt && rightIsInt {
		leftVal := leftLitInt.Value
		rightVal := rightLitInt.Value
		var resultVal int
		calculated := true
		switch operator {
		case "+":
			resultVal = leftVal + rightVal
		case "-":
			resultVal = leftVal - rightVal
		case "*":
			resultVal = leftVal * rightVal
		case "/":
			if rightVal == 0 {
				// Error already added by semantic check. Don't fold.
				return nil
			}
			resultVal = leftVal / rightVal
		default:
			calculated = false // Unknown operator for int folding
		}
		if calculated {
			return &ast.IntegerLiteral{
				Token: opToken, // Use operator token for location info of the folded result
				Value: resultVal,
				Width: lib.CalculateWidthForValue(resultVal), // Calculate exact width
			}
		}
	}

	// String Folding (only for '+')
	if leftIsStr && rightIsStr && operator == "+" {
		resultVal := leftLitStr.Value + rightLitStr.Value
		return &ast.StringLiteral{
			Token: opToken, // Use operator token for location
			Value: resultVal,
			Width: len(resultVal), // Exact width
		}
	}

	return nil // Folding not applicable
}

// skipParentheses attempts to consume tokens until a matching ')' is found.
// Basic recovery utility. Assumes curTok is the token *after* the opening '('.
func (p *Parser) skipParentheses() {
	openCount := 1 // We are inside the parentheses

	for openCount > 0 && p.curTok.Type != token.TokenEOF {
		switch p.curTok.Type {
		case token.TokenLParen:
			openCount++
		case token.TokenRParen:
			openCount--
		}
		if openCount == 0 { // Found matching brace
			p.nextToken() // Consume the final ')'
			break
		}
		p.nextToken()
	}
	// If loop finished due to EOF, error state persists.
}

// parseExpressionList parses a comma-separated list of expressions until endToken is found.
// Expects curTok to be the token *after* the opening delimiter (e.g., after '(').
// Consumes tokens up to and including the endToken. Used for arguments.
func (p *Parser) parseExpressionList(endToken token.TokenType) ([]ast.Expression, error) {
	list := []ast.Expression{}

	// Check for empty list (e.g., func())
	if p.curTok.Type == endToken {
		p.nextToken() // Consume endToken
		return list, nil
	}

	// Parse first expression
	expr := p.parseExpression(PrecLowest) // Use main expression parser
	if expr == nil {
		// Error already reported by parseExpression or means syntax error here
		if len(p.errors) == 0 { // Add error if parseExpression didn't
			p.addError(p.curTok, "Expected expression in list")
		}
		// Attempt recovery? Skip until comma or end token?
		p.skipUntilCommaOrEnd(endToken)
		if p.curTok.Type == endToken {
			p.nextToken()
		} // Consume end if found
		return nil, fmt.Errorf("failed to parse expression in list")
	}
	list = append(list, expr)

	// Parse subsequent expressions separated by commas
	for p.curTok.Type == token.TokenComma {
		p.nextToken() // Consume ','

		// Handle trailing comma case: `func(a, )` - invalid
		if p.curTok.Type == endToken {
			p.addError(p.curTok, "Unexpected '%s' after comma in list", endToken)
			p.nextToken() // Consume endToken
			return nil, fmt.Errorf("unexpected end token after comma")
		}

		expr := p.parseExpression(PrecLowest)
		if expr == nil {
			if len(p.errors) == 0 {
				p.addError(p.curTok, "Expected expression after comma in list")
			}
			p.skipUntilCommaOrEnd(endToken)
			if p.curTok.Type == endToken {
				p.nextToken()
			} // Consume end if found
			return nil, fmt.Errorf("failed to parse expression after comma")
		}
		list = append(list, expr)
	}

	// Expect the end token
	if p.curTok.Type != endToken {
		p.addError(p.curTok, "Expected '%s' or ',' after expression in list, got %s ('%s')", endToken, p.curTok.Type, p.curTok.Literal)
		// Recovery: skip until end token?
		p.skipUntil(endToken)
		// Return nil because the list is likely incomplete/incorrect
		return nil, fmt.Errorf("syntax error in expression list")
	}

	p.nextToken() // Consume endToken

	return list, nil
}

// skipUntilCommaOrEnd advances tokens until comma, endToken, or EOF.
func (p *Parser) skipUntilCommaOrEnd(endToken token.TokenType) {
	for p.curTok.Type != token.TokenComma && p.curTok.Type != endToken && p.curTok.Type != token.TokenEOF {
		// TODO: Need smarter skipping for nested structures if expressions get complex
		p.nextToken()
	}
}

// --- Utility Functions ---

// expectPeek checks if the next token is of the expected type. If so, it consumes
// the current token (advances p.curTok) and returns true. Otherwise, adds an error
// and returns false.
func (p *Parser) expectPeek(expectedType token.TokenType) bool {
	if p.peekTok.Type == expectedType {
		p.nextToken() // Consume current, advance peek to current
		return true
	}
	p.peekError(expectedType)
	return false
}

// peekError adds an error message indicating the expected vs actual peek token.
func (p *Parser) peekError(expectedType token.TokenType) {
	msg := fmt.Sprintf("Expected next token to be %s, got %s ('%s') instead",
		expectedType, p.peekTok.Type, p.peekTok.Literal)
	// Report error at the location of the *peek* token, as that's where the expectation failed
	p.addError(p.peekTok, msg)
}
