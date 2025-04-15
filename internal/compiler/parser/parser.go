package parser

import (
	"fmt"
	"slices"
	"strconv"
	"strings"

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
	PrecPrimary // Literals, identifiers, (...)
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
}

func NewParser(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:               l,
		errors:          []string{},
		warnings:        []string{},
		symbolTable:     make(map[string]symbols.SymbolInfo),
		currentProcName: "",  // Not inside a proc initially
		currentScope:    nil, // Will be initialized before parsing passes
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
	p.errors = append(p.errors, fmt.Sprintf("%d:%d: Syntax Error: %s", tok.Line, tok.Column, msg))
}

func (p *Parser) addSemanticError(tok token.Token, format string, args ...any) {
	msg := fmt.Sprintf(format, args...)
	p.errors = append(p.errors, fmt.Sprintf("%d:%d: Semantic Error: %s", tok.Line, tok.Column, msg))
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
	fmt.Printf(">>> Pushing scope (outer: %p)\n", p.currentScope) // Debugging
	p.currentScope = scope.NewScope(p.currentScope, p.currentScope.Name)
}

func (p *Parser) popScope() {
	if p.currentScope != nil {
		fmt.Printf("<<< Popping scope (back to: %p)\n", p.currentScope.Outer) // Debugging
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
	// --- Pass 1: Collect Procedure Signatures ---
	procSignatures, pass1Errs := p.collectProcedureSignatures()
	p.errors = append(p.errors, pass1Errs...) // Add any errors from pass 1
	if len(p.errors) > 0 {
		// If signature collection failed significantly, don't proceed
		return nil
	}

	// --- Pass 2: Full Parse ---
	// Initialize/Reset parser state for Pass 2
	p.l.ResetPosition()
	p.currentScope = scope.NewScope(nil, "global") // Initialize global scope
	p.errors = []string{}                          // Reset errors for Pass 2
	p.warnings = []string{}                        // Reset warnings for Pass 2

	// Initialize tokens for Pass 2
	p.nextToken()
	p.nextToken()

	// Populate global scope with procedure signatures from Pass 1
	for name, info := range procSignatures {
		err := p.defineSymbol(name, info)
		if err != nil {
			// This would be an internal error (duplicate definition in pass 1?)
			// Or potentially a keyword collision if not handled earlier
			p.addSemanticError(token.Token{}, "Internal Error: Failed to define pre-declared procedure '%s': %v", name, err)
		}
	}

	// Check if Pratt functions are initialized (do this once)
	if p.prefixParseFns == nil {
		p.initializePratt()
	}

	// Prepare the final Program AST
	program := &ast.Program{
		Statements:  []ast.Statement{},
		GlobalScope: p.currentScope,
	}

	// Main parsing loop (Pass 2)
	for p.curTok.Type != token.TokenEOF {
		stmt := p.parseStatement() // This now uses scoping correctly
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		} else if len(p.errors) > 0 {
			// Attempt recovery by skipping to next potential statement start?
			// For now, rely on individual parsers consuming tokens or the loop eventually hitting EOF
			fmt.Printf("DEBUG: Skipping token after failed statement parse: %v\n", p.curTok)
			p.nextToken() // Use cautiously for recovery
		} else if p.curTok.Type != token.TokenEOF {
			// If parseStatement returned nil but no errors, it's unexpected.
			p.addError(p.curTok, "Internal Parser Error: parseStatement returned nil without errors")
			p.nextToken() // Advance to prevent infinite loop
		}
	}

	// Sanity check scope
	if p.currentScope == nil || p.currentScope.Outer != nil {
		p.addError(token.Token{}, "Internal Parser Error: Scope mismatch at end of parsing.")
		return nil
	}

	return program
}

// --- Pass 1 Implementation ---

// collectProcedureSignatures performs the first pass to gather proc signatures.
// It needs its own token handling to avoid interfering with the main parser state.
func (p *Parser) collectProcedureSignatures() (map[string]symbols.SymbolInfo, []string) {
	signatures := make(map[string]symbols.SymbolInfo)
	pass1Errs := []string{}
	// Use a temporary parser state for this pass to avoid side effects
	tempParser := NewParser(p.l) // Creates a new parser with its own lexer state
	tempParser.nextToken()       // Initialize tokens
	tempParser.nextToken()

	for tempParser.curTok.Type != token.TokenEOF {
		if tempParser.curTok.Type == token.TokenProc {
			// --- Parse Signature ---
			tempParser.nextToken() // Consume 'proc'
			if tempParser.curTok.Type != token.TokenIdent {
				pass1Errs = append(pass1Errs, fmt.Sprintf("%d:%d: Syntax Error: Expected procedure name after 'proc'", tempParser.curTok.Line, tempParser.curTok.Column))
				tempParser.skipUntil(token.TokenProc, token.TokenEOF) // Try to find next proc
				continue
			}
			procName := tempParser.curTok.Literal
			procNameToken := tempParser.curTok

			// Check for duplicate procedure names found *during this pass*
			if _, exists := signatures[procName]; exists {
				pass1Errs = append(pass1Errs, fmt.Sprintf("%d:%d: Semantic Error: Procedure '%s' already declared", procNameToken.Line, procNameToken.Column, procName))
				// Skip rest of this signature and body
				tempParser.skipSignatureAndBody()
				continue
			}

			tempParser.nextToken() // Consume proc name IDENT

			// Parse Parameters `(...)`
			if tempParser.curTok.Type != token.TokenLParen {
				pass1Errs = append(pass1Errs, fmt.Sprintf("%d:%d: Syntax Error: Expected '(' after procedure name '%s'", tempParser.curTok.Line, tempParser.curTok.Column, procName))
				tempParser.skipSignatureAndBody()
				continue
			}
			// Use the main parser logic for parameter list (carefully adapting state if needed, or making it standalone)
			// For simplicity here, let's assume parseParameterList can work with the tempParser state
			params, paramNames, err := tempParser.parseParameterList() // Needs to handle errors and token consumption correctly
			if err != nil || tempParser.curTok.Type != token.TokenRParen {
				// Error already added by parseParameterList (or should be)
				pass1Errs = append(pass1Errs, tempParser.errors...) // Collect errors
				tempParser.errors = []string{}                      // Clear errors for next attempt
				tempParser.skipSignatureAndBody()
				continue
			}
			// At this point, curTok should be ')'

			// Parse Return Type `: type`
			if tempParser.curTok.Type != token.TokenRParen {
				pass1Errs = append(pass1Errs, fmt.Sprintf("%d:%d: Syntax Error: Expected ')' after parameter list", tempParser.curTok.Line, tempParser.curTok.Column))
				tempParser.skipSignatureAndBody()
				continue
			}
			if tempParser.peekTok.Type != token.TokenColon {
				pass1Errs = append(pass1Errs, fmt.Sprintf("%d:%d: Syntax Error: Expected ':' for return type after '()'", tempParser.peekTok.Line, tempParser.peekTok.Column))
				tempParser.skipSignatureAndBody()
				continue
			}
			tempParser.nextToken() // Consume ')'
			tempParser.nextToken() // Consume ':'

			// Parse the return type node
			returnTypeNode := tempParser.parseTypeNode()
			if returnTypeNode == nil {
				pass1Errs = append(pass1Errs, tempParser.errors...)
				tempParser.errors = []string{}
				tempParser.skipSignatureAndBody()
				continue
			}
			// curTok is now the token after the return type (should be '{')

			// --- Store Signature ---
			info := symbols.SymbolInfo{
				Type:        "proc",
				IsConst:     true,
				Width:       0, // N/A for proc itself
				ParamNames:  paramNames,
				ParamTypes:  make([]string, len(params)),
				ParamWidths: make([]int, len(params)),
				ReturnType:  returnTypeNode.Name,
				ReturnWidth: returnTypeNode.Width,
			}
			// Populate param details
			validSignature := true
			for i, p := range params {
				if p.TypeNode == nil || p.TypeNode.IsVoid || p.TypeNode.Width <= 0 {
					// Param parsing error occurred earlier, mark signature invalid
					validSignature = false
					break
				}
				info.ParamTypes[i] = p.TypeNode.Name
				info.ParamWidths[i] = p.TypeNode.Width
			}

			if validSignature {
				signatures[procName] = info
			} else {
				// Add error if not already present from param/type parsing
				foundSigErr := false
				for _, e := range pass1Errs {
					if strings.Contains(e, procName) {
						foundSigErr = true
						break
					}
				}
				if !foundSigErr {
					pass1Errs = append(pass1Errs, fmt.Sprintf("%d:%d: Semantic Error: Invalid signature for procedure '%s'", procNameToken.Line, procNameToken.Column, procName))
				}
			}

			// --- Skip Body ---
			if tempParser.curTok.Type == token.TokenLBrace {
				tempParser.skipBlock() // Skips from '{' to past matching '}'
			} else {
				// If '{' wasn't next, something is wrong, but signature might be stored
				pass1Errs = append(pass1Errs, fmt.Sprintf("%d:%d: Syntax Error: Expected '{' to start body for procedure '%s', got %s", tempParser.curTok.Line, tempParser.curTok.Column, procName, tempParser.curTok.Type))
				// Attempt to find next proc or EOF
				tempParser.skipUntil(token.TokenProc, token.TokenEOF)
			}
			// After skipping, curTok is after '}' or at PROC/EOF
			continue // Continue to next top-level item

		}
		// If not 'proc', advance to the next token for Pass 1 scanning
		tempParser.nextToken()
	}

	return signatures, pass1Errs
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

// --- Statement Parsing ---

func (p *Parser) parseStatement() ast.Statement {
	switch p.curTok.Type {
	case token.TokenProc:
		return p.parseProcDeclarationStatement()
	case token.TokenReturn:
		return p.parseReturnStatement()
	case token.TokenPrint:
		return p.parsePrintStatement()
	case token.TokenConst:
		return p.parseDeclarationStatement() // Handles const modifier
	case token.TokenIdent:
		// Look ahead to determine declaration, reassignment, or proc call
		switch p.peekTok.Type {
		case token.TokenAssignDefine, token.TokenColon: // x := ... or x : type = ...
			return p.parseDeclarationStatement()
		case token.TokenAssign: // x = ...
			return p.parseReassignmentStatement()
		case token.TokenLParen: // x(...)
			// Parse as an expression statement, which handles proc calls
			return p.parseExpressionStatement()
		default:
			p.addError(p.peekTok, "Expected ':=', ':', '=', or '(' after identifier '%s', got '%s'", p.curTok.Literal, p.peekTok.Type)
			p.nextToken() // Consume IDENT
			// Consume the unexpected token to attempt recovery
			if p.curTok.Type != token.TokenEOF && p.curTok.Type != token.TokenRBrace { // Avoid consuming closing brace
				p.nextToken()
			}
			return nil
		}
	case token.TokenLBrace: // Block statement cannot occur outside procedures
		p.addError(p.curTok, "Unexpected '{' at the start of a top-level statement")
		p.skipBlock() // Attempt to skip the block
		return nil
	case token.TokenEOF:
		return nil // End of input
	default:
		// Could be an expression statement starting with a literal or '('.
		// Try parsing as expression statement. If that fails, it's a syntax error.
		parsedStmt := p.parseExpressionStatement() // Get the statement (interface type)

		if parsedStmt != nil {
			// Type assertion to get the concrete *ast.ExpressionStatement
			exprStmt, ok := parsedStmt.(*ast.ExpressionStatement)
			if !ok {
				// This should not happen if parseExpressionStatement only returns *ast.ExpressionStatement
				// But good defensive check.
				p.addError(p.curTok, "Internal Parser Error: Expected ExpressionStatement, got %T", parsedStmt)
				return nil // Or return parsedStmt without checks? nil seems safer.
			}

			// Now use exprStmt (which has type *ast.ExpressionStatement) to access fields
			switch node := exprStmt.Expression.(type) {
			case *ast.ProcCallExpression:
				if node.ResultType() != "void" {
					p.addWarning(exprStmt.Token, "Result of non-void procedure call '%s' is ignored.", node.Function.Value)
				}
			case *ast.IntegerLiteral:
				p.addWarning(exprStmt.Token, "Integer literal used as statement has no effect.")
			case *ast.StringLiteral:
				p.addWarning(exprStmt.Token, "String literal used as statement has no effect.")
			case *ast.Identifier:
				if node.Symbol.Type == "proc" {
					p.addSemanticError(exprStmt.Token, "Procedure '%s' used as statement without calling it.", node.Value)
				} else if node.Symbol.Type != "unknown" {
					p.addWarning(exprStmt.Token, "Variable '%s' used as statement has no effect.", node.Value)
				}
			case *ast.BinaryExpression:
				p.addWarning(exprStmt.Token, "Binary expression used as statement has no effect.")
			}
			return exprStmt // Return the concrete statement
		}

		// If parseExpressionStatement returned nil, it means parseExpression failed.
		if len(p.errors) == 0 { // If no specific error was added, add a generic one
			p.addError(p.curTok, "Unexpected token at start of statement: %s ('%s')", p.curTok.Type, p.curTok.Literal)
		}
		// Consume unexpected token to attempt recovery if it wasn't consumed already
		if p.curTok.Type != token.TokenEOF && p.curTok.Type != token.TokenRBrace {
			p.nextToken()
		}
		return nil
	}
}

// parseDeclarationStatement parses `[const] name := value` or `[const] name: type[(width)] = value`
func (p *Parser) parseDeclarationStatement() ast.Statement {
	stmt := &ast.DeclarationStatement{Token: p.curTok}

	// 1. Handle 'const'
	if p.curTok.Type == token.TokenConst {
		stmt.IsConst = true
		p.nextToken()
		if p.curTok.Type != token.TokenIdent {
			p.addError(p.curTok, "Expected identifier after 'const'")
			return nil
		}
	}

	// 2. Expect IDENT
	if p.curTok.Type != token.TokenIdent {
		p.addError(p.curTok, "Internal Parser Error: Expected identifier for declaration")
		return nil
	}
	identToken := p.curTok
	varName := identToken.Literal
	stmt.Name = &ast.Identifier{Token: identToken, Value: varName}

	// 3. Check for Redeclaration *in the current scope*
	_, declaredInCurrent := p.lookupCurrentScopeSymbol(varName)
	if declaredInCurrent {
		p.addSemanticError(identToken, "Variable '%s' already declared in this scope", varName)
		// Don't define it again, but continue parsing
	}

	// --- Rest of parsing (Explicit/Inferred type, RHS expression) ---
	// ... (This part remains largely the same as your previous version) ...
	var explicitTypeNode *ast.TypeNode = nil // Store parsed explicit type info

	// 4. Check for ':=' (Inferred) or ':' (Explicit) and consume tokens up to '=' or expression start
	switch p.peekTok.Type {
	case token.TokenColon:
		// --- Explicit Type Path: `name : type[(width)] = value` ---
		stmt.HasExplicitType = true
		p.nextToken() // Consume IDENT
		p.nextToken() // Consume ':'

		// Parse the TypeNode (e.g., "int", "string(10)")
		typeNode := p.parseTypeNode()
		if typeNode == nil {
			return nil
		} // Error handled in parseTypeNode
		if typeNode.IsVoid {
			p.addError(typeNode.Token, "Cannot declare variable '%s' with type 'void'", varName)
			return nil
		}
		explicitTypeNode = typeNode
		stmt.ExplicitTypeToken = typeNode.Token       // Store 'int' or 'string' token
		stmt.ExplicitWidthToken = typeNode.WidthToken // Store '10' token if present

		// Expect '=' for assignment after explicit type
		if p.curTok.Type != token.TokenAssign {
			p.addError(p.curTok, "Expected '=' after type specification for '%s', got %s ('%s')", varName, p.curTok.Type, p.curTok.Literal)
			return nil
		}
		stmt.Token = p.curTok // Store '=' token
		p.nextToken()         // Consume '=', curTok is now start of expression

	case token.TokenAssignDefine:
		// --- Inferred Type Path: `name := value` ---
		stmt.HasExplicitType = false
		p.nextToken()         // Consume IDENT. curTok is now ':='
		stmt.Token = p.curTok // Store ':=' token
		p.nextToken()         // Consume ':='. curTok is now start of expression

	default:
		p.addError(p.peekTok, "Expected ':=' or ':' after identifier '%s' in declaration, got '%s'", varName, p.peekTok.Type)
		return nil
	}

	// 5. Parse the Value Expression
	valueExpr := p.parseExpression(PrecLowest)
	if valueExpr == nil {
		return nil
	}
	stmt.Value = valueExpr

	// --- 6. Semantic Analysis (Largely the same, uses finalType/finalWidth) ---
	// ... (Type checking, width checking logic as before) ...
	valueType := valueExpr.ResultType()
	valueWidth := valueExpr.ResultWidth()
	var finalType string = "unknown" // Start assuming unknown
	var finalWidth int = 0

	if valueType == "void" {
		p.addSemanticError(valueExpr.GetToken(), "Cannot assign result of void procedure call to variable '%s'", varName)
		valueType = "unknown" // Mark as error state to prevent symbol table entry
	}
	if valueType == "unknown" && len(p.errors) == 0 {
		p.addError(stmt.Token, "Internal Error: Expression resulted in 'unknown' type without specific error for '%s' assignment", varName)
	}

	if stmt.HasExplicitType { // Explicit Type (`name : type = value`)
		finalType = explicitTypeNode.Name
		finalWidth = explicitTypeNode.Width

		if valueType != "unknown" && finalType != "unknown" && finalType != valueType {
			p.addSemanticError(valueExpr.GetToken(), "Type mismatch - cannot assign value of type '%s' to variable '%s' declared as '%s'", valueType, varName, finalType)
			finalType = "unknown" // Mark as error state
		}
		// Width check for literals
		if finalType != "unknown" {
			if litInt, ok := valueExpr.(*ast.IntegerLiteral); ok && finalType == "int" {
				reqWidth := lib.CalculateWidthForValue(litInt.Value)
				if reqWidth > finalWidth {
					p.addSemanticError(valueExpr.GetToken(), "Integer literal %s requires width %d, but variable '%s' declared with width %d", litInt.Token.Literal, reqWidth, varName, finalWidth)
				}
			} else if litStr, ok := valueExpr.(*ast.StringLiteral); ok && finalType == "string" {
				reqWidth := len(litStr.Value)
				if reqWidth > finalWidth {
					p.addSemanticError(valueExpr.GetToken(), "String literal with length %d exceeds declared width %d for variable '%s'", reqWidth, finalWidth, varName)
				}
			}
		}
	} else { // Inferred Type (`name := value`)
		if valueType != "unknown" {
			finalType = valueType
			finalWidth = valueWidth
			if finalWidth <= 0 {
				finalWidth = lib.GetDefaultWidth(finalType)
				if finalWidth <= 0 && finalType != "unknown" {
					p.addWarning(stmt.Name.Token, "Could not determine valid width for inferred type '%s'. Using width 1.", finalType)
					finalWidth = 1
				}
			}
		} else {
			finalType = "unknown"
			finalWidth = 0
		}
	}

	// --- 7. Final Validation and Symbol Table Update ---
	if finalType != "unknown" && finalWidth <= 0 {
		p.addWarning(stmt.Name.Token, "Final width for '%s' (type %s) resolved to %d, using 1.", varName, finalType, finalWidth)
		finalWidth = 1
	}

	// Define in current scope ONLY if not a redeclaration *at this level* AND type is valid
	if !declaredInCurrent && finalType != "unknown" && finalType != "void" {
		symbolInfo := symbols.SymbolInfo{
			Type:    finalType,
			IsConst: stmt.IsConst,
			Width:   finalWidth,
		}
		err := p.defineSymbol(varName, symbolInfo)
		if err != nil {
			// Should not happen if declaredInCurrent check was correct
			p.addSemanticError(stmt.Name.Token, "Internal Error: Failed to define variable '%s': %v", varName, err)
		}
	} else {
		p.addSemanticError(stmt.Name.Token, "Internal Error: Failed to lookup symbol '%s' immediately after definition", varName)
	}

	return stmt
}

// parseProcDeclarationStatement parses `proc name(params...) : retType { body }`
func (p *Parser) parseProcDeclarationStatement() *ast.ProcDeclarationStatement {
	startTok := p.curTok // Store 'proc' token
	stmt := &ast.ProcDeclarationStatement{Token: startTok}

	// Proc signature was already collected in Pass 1 and added to global scope.
	// We parse it again here for the AST structure and to set up the local scope.

	if p.curTok.Type != token.TokenProc {
		p.addError(p.curTok, "Internal Parser Error: Expected 'proc'")
		return nil
	}
	if !p.expectPeek(token.TokenIdent) {
		return nil
	}

	// Create Identifier node, Symbol is initially nil
	stmt.Name = &ast.Identifier{Token: p.curTok, Value: p.curTok.Literal}
	procName := stmt.Name.Value
	previousProcName := p.currentProcName
	p.currentProcName = procName

	// Check if it exists in the global scope (sanity check for Pass 1)
	procInfoPtr, exists := p.lookupSymbol(procName) // Should find it in global scope

	if !exists || procInfoPtr.Type != "proc" {
		// This implies Pass 1 failed or logic error
		p.addSemanticError(stmt.Name.Token, "Internal Error: Procedure '%s' signature not found during Pass 2", procName)
	} else {
		stmt.Name.Symbol = procInfoPtr // Mark valid proc
	}

	// Enter new scope
	p.pushScope()
	stmt.LocalScope = p.currentScope
	defer p.popScope()

	// Parse Parameter List `(...)` again for AST and local scope definition
	if !p.expectPeek(token.TokenLParen) {
		p.currentProcName = previousProcName
		return nil
	}
	params, _, err := p.parseParameterList() // Don't need paramNames list here
	if err != nil {
		p.currentProcName = previousProcName
		return nil // Error handled in parseParameterList
	}
	stmt.Parameters = params
	// curTok should be ')'

	// Define parameters in the *current* (new) scope
	for _, param := range stmt.Parameters {
		// Pass 1 already validated param signature, define it here
		if param.Name != nil && param.TypeNode != nil {
			paramInfo := symbols.SymbolInfo{
				Type:    param.TypeNode.Name,
				IsConst: false, // Parameters are mutable by default
				Width:   param.TypeNode.Width,
			}
			defErr := p.defineSymbol(param.Name.Value, paramInfo)
			if defErr != nil {
				// This should only happen if parseParameterList allowed duplicates somehow
				p.addSemanticError(param.Name.Token, "Internal Error: %v", defErr)
			}
		}
	}

	// Parse Return Type `: type` again for AST
	if p.curTok.Type != token.TokenRParen {
		p.addError(p.curTok, "Internal Parser Error: Expected ')'")
		p.currentProcName = previousProcName
		return nil
	}
	if !p.expectPeek(token.TokenColon) {
		p.addError(p.peekTok, "Expected ':' for return type after '()', got '%s'", p.curTok.Type)
		p.currentProcName = previousProcName
		return nil
	}
	p.nextToken() // Consume ':'

	returnTypeNode := p.parseTypeNode()
	if returnTypeNode == nil {
		p.currentProcName = previousProcName
		return nil
	}
	if procInfoPtr != nil && procInfoPtr.ReturnType == returnTypeNode.Name && procInfoPtr.ReturnWidth != returnTypeNode.Width {
		p.addError(returnTypeNode.Token, "Internal Parser Error: Return width mismatch between Pass 1 (%d) and Pass 2 (%d) for '%s'", procInfoPtr.ReturnWidth, returnTypeNode.Width, procName)
		p.currentProcName = previousProcName
		return nil
	}

	stmt.ReturnType = returnTypeNode
	// curTok should be '{'

	// Parse Procedure Body `{...}`
	if p.curTok.Type != token.TokenLBrace {
		p.addError(p.curTok, "Expected '{' to start procedure body, got %s", p.curTok.Type)
		p.currentProcName = previousProcName
		return nil
	}
	stmt.Body = p.parseBlockStatement() // Will parse statements within the new scope
	if stmt.Body == nil {
		p.currentProcName = previousProcName
		return nil // Error in body parsing
	}
	// parseBlockStatement consumed '}'

	// TODO: Check if there's a return statement.
	// This is basic, just check if the last line in the proc body is a return stmt
	// Need to revisit this for more complex control flow
	if !stmt.ReturnType.IsVoid {
		hasReturn := false
		if len(stmt.Body.Statements) > 0 {
			if _, ok := stmt.Body.Statements[len(stmt.Body.Statements)-1].(*ast.ReturnStatement); ok {
				hasReturn = true
			}
		}
		if !hasReturn {
			p.addSemanticError(stmt.Name.Token, "Missing return statement in non-void procedure '%s'", procName)
		}
	}

	// Scope is popped by defer

	p.currentProcName = previousProcName // Restore outer context
	return stmt
}

// parseTypeNode parses a type specification: `type` or `type(width)` or `void`
// Used for explicit variable types, parameters, and return types.
// Expects curTok to be the type name (IDENT or VOID). Consumes tokens through type spec.
// Returns the TypeNode or nil on error.
// IMPORTANT: If type is 'int' or 'string' and width is not specified,
// it assigns the default width to the TypeNode.
func (p *Parser) parseTypeNode() *ast.TypeNode {
	// Expect 'int', 'string', or 'void'
	isKnownTypeLiteral := p.curTok.Type == token.TokenTypeLiteral && (p.curTok.Literal == "int" || p.curTok.Literal == "string")
	isVoidType := p.curTok.Type == token.TokenVoid

	if !isKnownTypeLiteral && !isVoidType {
		p.addError(p.curTok, "Expected type name (int, string, void), got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		// Attempt recovery by consuming the bad token? Let caller handle.
		// If we consume here, caller might misinterpret next token. Better to return nil.
		// p.nextToken()
		return nil
	}

	typeTok := p.curTok
	typeName := typeTok.Literal
	node := &ast.TypeNode{
		Token:  typeTok,
		Name:   typeName,
		IsVoid: isVoidType,
		Width:  0,
	}
	p.nextToken() // Consume type name token ('int', 'string', 'void')

	// Parse optional width `(int)` only if not void
	if !node.IsVoid && p.curTok.Type == token.TokenLParen {
		p.nextToken() // Consume '('
		if p.curTok.Type != token.TokenInt {
			p.addError(p.curTok, "Expected integer width inside parentheses for type '%s', got %s ('%s')", typeName, p.curTok.Type, p.curTok.Literal)
			// Consume the bad token inside parens if it's not ')'
			if p.curTok.Type != token.TokenRParen && p.curTok.Type != token.TokenEOF {
				p.nextToken()
			}
			// If we now see ')', consume it.
			if p.curTok.Type == token.TokenRParen {
				p.nextToken()
			}
			return nil
		}
		node.WidthToken = p.curTok // Store the INT token
		widthVal, err := strconv.Atoi(p.curTok.Literal)
		if err != nil || widthVal <= 0 {
			p.addError(p.curTok, "Invalid width specification '%s'. Width must be a positive integer.", p.curTok.Literal)
			node.Width = 0 // Mark as invalid width for now
		} else {
			node.Width = widthVal
		}
		p.nextToken() // Consume INT

		if p.curTok.Type != token.TokenRParen {
			p.addError(p.curTok, "Expected ')' after width specification, got %s ('%s')", p.curTok.Type, p.curTok.Literal)
			// Don't consume if not ')', let caller handle
			return nil // Error, stop parsing type node
		}
		p.nextToken() // Consume ')'
	} else if !node.IsVoid && node.Width == 0 {
		// If not void and width wasn't explicitly set (or was set to 0 which indicates invalid), assign default width
		defaultWidth := lib.GetDefaultWidth(typeName)
		if defaultWidth == 0 {
			// This should only happen for unknown types, which already errored. Safety check.
			p.addError(typeTok, "Internal Error: Could not determine default width for known type '%s'", typeName)
			node.Width = 0
		} else {
			node.Width = defaultWidth
		}
	}
	// If void, width remains 0.
	// If width was validly parsed, node.Width has the explicit value.

	// curTok is now the token *after* the type specification (e.g., '=', '{', ',', ')')
	return node
}

// parseParameterList parses `ident: type(width), ident: type(width), ...`
// Expects curTok to be '('. Consumes tokens inside the list.
// Leaves curTok on the closing ')'.
// Returns list of Parameter nodes, list of param names, and error.
func (p *Parser) parseParameterList() ([]*ast.Parameter, []string, error) {
	params := []*ast.Parameter{}
	paramNames := []string{}
	paramNameSet := make(map[string]bool) // For checking duplicate names

	if p.curTok.Type != token.TokenLParen { // Should be called only when curTok is '('
		p.addError(p.curTok, "Internal Parser Error: Expected '(' to start parameter list, got %s", p.curTok.Type)
		return nil, nil, fmt.Errorf("internal error: missing '('")
	}

	// Check for empty list: ()
	if p.peekTok.Type == token.TokenRParen {
		p.nextToken()                  // Consume '('
		return params, paramNames, nil // Return empty lists, success
	}

	p.nextToken() // Consume '('

	// Parse first parameter
	param, err := p.parseSingleParameter()
	if err != nil {
		return nil, nil, err
	} // Error handled in parseSingleParameter
	if _, duplicate := paramNameSet[param.Name.Value]; duplicate {
		p.addSemanticError(param.Name.Token, "Duplicate parameter name '%s'", param.Name.Value)
		// Continue parsing other params to find more errors, but mark this call as failed?
		// For now, let's stop on first duplicate by returning error.
		return nil, nil, fmt.Errorf("duplicate parameter name")
	}
	params = append(params, param)
	paramNames = append(paramNames, param.Name.Value)
	paramNameSet[param.Name.Value] = true
	// parseSingleParameter consumes tokens up to token after type spec

	// Parse subsequent parameters (separated by comma)
	for p.curTok.Type == token.TokenComma {

		p.nextToken() // Consume ','

		// Handle trailing comma case: `(a: int(1), )`
		if p.curTok.Type == token.TokenRParen {
			p.addError(p.curTok, "Unexpected ')' after comma in parameter list")
			return nil, nil, fmt.Errorf("unexpected ')' after comma")
		}

		param, err := p.parseSingleParameter()
		if err != nil {
			return nil, nil, err
		}
		if _, duplicate := paramNameSet[param.Name.Value]; duplicate {
			p.addSemanticError(param.Name.Token, "Duplicate parameter name '%s'", param.Name.Value)
			return nil, nil, fmt.Errorf("duplicate parameter name")
		}
		params = append(params, param)
		paramNames = append(paramNames, param.Name.Value)
		paramNameSet[param.Name.Value] = true
	}

	// Expect ')' - DO NOT CONSUME IT HERE
	if p.curTok.Type != token.TokenRParen {
		p.addError(p.curTok, "Expected ',' or ')' after parameter, got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		// Recovery? Skip until ')'? For now, return error.
		return nil, nil, fmt.Errorf("syntax error in parameter list")
	}

	return params, paramNames, nil
}

// parseSingleParameter parses `ident: type(width)` - width is MANDATORY here.
// Expects curTok to be the identifier. Consumes up to token after type spec.
func (p *Parser) parseSingleParameter() (*ast.Parameter, error) {
	if p.curTok.Type != token.TokenIdent {
		p.addError(p.curTok, "Expected parameter name (identifier), got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		if p.curTok.Type != token.TokenEOF && p.curTok.Type != token.TokenColon && p.curTok.Type != token.TokenRParen {
			p.nextToken()
		} // Consume bad token
		return nil, fmt.Errorf("expected parameter name")
	}
	ident := &ast.Identifier{Token: p.curTok, Value: p.curTok.Literal}

	if !p.expectPeek(token.TokenColon) {
		p.addError(p.peekTok, "Expected ':' after parameter name '%s'", ident.Value)
		return nil, fmt.Errorf("expected ':' after parameter name")
	}
	// curTok is now ':'
	p.nextToken() // Consume ':'
	// curTok is now the start of the type node

	// Parse the type node (e.g., "int(10)", "string(30)")
	typeNode := p.parseTypeNode()
	if typeNode == nil {
		return nil, fmt.Errorf("invalid type specification for parameter '%s'", ident.Value)
	}
	if typeNode.IsVoid {
		p.addError(typeNode.Token, "Parameter '%s' cannot have type 'void'", ident.Value)
		return nil, fmt.Errorf("parameter cannot be void")
	}
	// Check if width was explicitly provided (it's mandatory for params)
	// parseTypeNode sets Width > 0 if explicit width was given, or 0 if not.
	// It does NOT assign default width for parameters.
	if typeNode.Width <= 0 { // Check if a valid positive width was parsed
		// WidthToken check is more precise if parseTypeNode preserves it correctly
		widthMissing := typeNode.WidthToken.Type != token.TokenInt
		if widthMissing {
			p.addError(typeNode.Token, "Explicit width specification (e.g., 'int(10)', 'string(30)') is required for parameter '%s'", ident.Value)
		} else {
			// Width was provided but was invalid (e.g., "(0)", "(-5)") - parseTypeNode should have errored
			// Add a safety error here if needed.
			if len(p.errors) == 0 || !strings.Contains(p.errors[len(p.errors)-1], "Invalid width specification") {
				p.addError(typeNode.WidthToken, "Invalid width '%s' for parameter '%s'", typeNode.WidthToken.Literal, ident.Value)
			}
		}
		// Enforce the rule: no valid width means error.
		return nil, fmt.Errorf("missing or invalid mandatory width for parameter")
	}

	// TODO: Add parameter to current scope's symbol table when scopes are implemented

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
			// If parseStatement returned nil due to an error, it should have consumed
			// the problematic token. If it was EOF, the loop condition handles it.
			// If it was some other reason, we might loop infinitely.
			// Safeguard: if errors occurred and we haven't reached the end or }, advance.
			if len(p.errors) > 0 && p.curTok.Type != token.TokenRBrace && p.curTok.Type != token.TokenEOF {
				// fmt.Printf("DEBUG: Advancing token after failed statement parse in block: %v\n", p.curTok)
				// p.nextToken() // Use cautiously
			}
		}
	}

	if p.curTok.Type != token.TokenRBrace {
		// If EOF was reached before '}', add error
		p.addError(p.curTok, "Expected '}' to close block, found %s ('%s')", p.curTok.Type, p.curTok.Literal)
		// Attempt to find '}' for recovery? Difficult. Return nil for now.
		return nil
	}

	p.nextToken() // Consume '}' - curTok is now after '}'

	return block
}

// skipBlock attempts recovery by consuming tokens until a matching '}'
func (p *Parser) skipBlock() {
	openCount := 1 // Assume '{' was the current token
	if p.curTok.Type != token.TokenLBrace {
		return
	} // Should not happen if called correctly
	p.nextToken() // Consume the initial '{'

	for openCount > 0 && p.curTok.Type != token.TokenEOF {
		switch p.curTok.Type {
		case token.TokenLBrace:
			openCount++
		case token.TokenRBrace:
			openCount--
		}
		if openCount == 0 { // Found matching brace
			p.nextToken() // Consume the final '}'
			break
		}
		p.nextToken()
	}
	// If loop finished due to EOF, error state persists.
}

// parseReturnStatement parses `return [expression]`
func (p *Parser) parseReturnStatement() ast.Statement {
	stmt := &ast.ReturnStatement{Token: p.curTok}
	returnTok := p.curTok // Keep the 'return' token for error reporting location
	p.nextToken()         // Consume 'return'

	// --- Semantic Check: Inside a procedure? ---
	if p.currentProcName == "" {
		p.addError(returnTok, "'return' statement outside of procedure")
		// Skip parsing expression, assume end of statement
		// Try to advance until a likely statement boundary? Let main loop handle for now.
		// Consume any potential value that might follow invalidly
		if p.isExpressionStart(p.curTok) {
			p.skipExpressionTokens() // Basic attempt to consume the invalid expression
		}
		return nil
	}

	// --- Get Procedure Return Info ---
	procInfo, ok := p.getSymbolInfo(p.currentProcName)
	if !ok { // Should not happen if currentProcName is set correctly
		p.addError(returnTok, "Internal Parser Error: Could not find symbol info for current procedure '%s'", p.currentProcName)
		procInfo = symbols.SymbolInfo{ReturnType: "unknown"} // Fallback for checks, treat as error state
	}

	// --- Parse Return Value (or check for none if void) ---
	// Check if something follows 'return' that could be an expression start
	hasPotentialValue := p.isExpressionStart(p.curTok)

	if procInfo.ReturnType == "void" {
		// Void function: should not have a return value
		if hasPotentialValue {
			p.addSemanticError(p.curTok, "Cannot return a value from procedure '%s' declared as void", p.currentProcName)
			// Consume the start of the invalid expression to allow parsing to continue
			p.skipExpressionTokens() // Attempt to consume the invalid expression
		}
		// Whether error or not, void return has nil value node
		stmt.ReturnValue = nil
		// No nextToken needed here, curTok is already after 'return' or after skipped value
	} else if procInfo.ReturnType == "unknown" {
		// Error case from missing proc info
		p.addError(returnTok, "Cannot determine validity of return statement for procedure '%s' due to previous errors", p.currentProcName)
		if hasPotentialValue {
			p.skipExpressionTokens()
		}
		return nil
	} else {
		// Non-void function: MUST have a return value
		if !hasPotentialValue {
			// Check if it's immediately followed by '}' or EOF
			if p.curTok.Type == token.TokenRBrace || p.curTok.Type == token.TokenEOF {
				p.addError(returnTok, "Expected expression after 'return' for non-void procedure '%s'", p.currentProcName)
			} else {
				// Some other token followed return, but not an expression start
				p.addError(p.curTok, "Expected expression after 'return', got %s ('%s')", p.curTok.Type, p.curTok.Literal)
			}
			return nil
		}

		returnValue := p.parseExpression(PrecLowest)
		if returnValue == nil {
			// Error added by parseExpression or syntax error after return
			if len(p.errors) == 0 || !strings.Contains(p.errors[len(p.errors)-1], "Expected expression") { // Avoid duplicate generic errors
				// This condition might be tricky, parseExpression should report its own errors.
				// IfreturnValue is nil and no error exists, it's an unexpected state.
				p.addError(returnTok, "Internal Parser Error: Failed to parse return expression for non-void procedure '%s'", p.currentProcName)
			}
			return nil
		}
		stmt.ReturnValue = returnValue

		// --- Semantic Check: Return type match ---
		valueType := returnValue.ResultType()
		if valueType != "unknown" && valueType != procInfo.ReturnType {
			p.addSemanticError(returnValue.GetToken(), "Type mismatch - cannot return value of type %s from procedure '%s' declared to return %s", valueType, p.currentProcName, procInfo.ReturnType)
		}

		// --- Semantic Check: Return width compatibility (Warning) ---
		valueWidth := returnValue.ResultWidth()
		declaredWidth := procInfo.ReturnWidth
		// Check only if types are compatible/known, widths are positive, and value > declared
		if valueType != "unknown" && valueType != "void" && valueType == procInfo.ReturnType && valueWidth > 0 && declaredWidth > 0 && valueWidth > declaredWidth {

			// Check if the returned value is a literal
			isLiteralReturn := false
			if _, ok := returnValue.(*ast.IntegerLiteral); ok {
				isLiteralReturn = true
			}
			if _, ok := returnValue.(*ast.StringLiteral); ok {
				isLiteralReturn = true
			}

			if isLiteralReturn {
				// ERROR for literals exceeding declared width
				p.addSemanticError(returnValue.GetToken(), "Literal return value width (%d) exceeds declared return width (%d) for procedure '%s'", valueWidth, declaredWidth, p.currentProcName)
			} else {
				// WARNING for non-literals (variables, expressions) - Keep this as a warning
				p.addWarning(returnValue.GetToken(), "Width of returned expression (%d) may exceed declared return width (%d) for procedure '%s'. Potential truncation.", valueWidth, declaredWidth, p.currentProcName)
			}

		} else if valueType != "unknown" && valueType != "void" && declaredWidth <= 0 && procInfo.ReturnType != "void" {
			// Existing warning about invalid declared width in proc signature
			p.addWarning(returnTok, "Could not verify return width for procedure '%s'; declared width is invalid (%d).", p.currentProcName, declaredWidth)
		}
	}

	// parseExpression consumed tokens. curTok is now after the returned value (or where it should be).
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
		// Error reported by parseExpression or sub-parsers
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
		p.addError(p.curTok, "Internal Parser Error: Expected identifier for reassignment")
		return nil
	}
	identToken := p.curTok
	stmt.Name = &ast.Identifier{Token: identToken, Value: p.curTok.Literal}
	varName := stmt.Name.Value

	// Semantic Check: Is declared (in any scope)? Is const?
	symbolInfoPtr, declared := p.lookupSymbol(varName) // Use lookupSymbol
	isConst := false
	isValidTarget := false

	if !declared {
		p.addSemanticError(identToken, "Cannot assign to undeclared variable '%s'", varName)
	} else if symbolInfoPtr.Type == "proc" {
		p.addSemanticError(identToken, "Cannot assign to procedure '%s'", varName)
	} else {
		symbolInfo := *symbolInfoPtr // Dereference after checks
		if symbolInfo.IsConst {
			p.addSemanticError(identToken, "Cannot assign to constant variable '%s'", varName)
			isConst = true
		}
		isValidTarget = !isConst
	}

	// Expect '='
	if !p.expectPeek(token.TokenAssign) {
		return nil
	}
	stmt.Token = p.curTok

	// Parse RHS expression
	p.nextToken()
	valueExpr := p.parseExpression(PrecLowest)
	if valueExpr == nil {
		return nil
	}
	stmt.Value = valueExpr

	// --- Semantic Check 3 & 4: Type and Width compatibility ---
	if isValidTarget && declared { // Only check if target is valid and info exists
		symbolInfo := *symbolInfoPtr // Use the info found
		valueType := valueExpr.ResultType()
		targetType := symbolInfo.Type

		// Type Check
		if valueType == "void" {
			p.addSemanticError(valueExpr.GetToken(), "Cannot assign result of void procedure call to variable '%s'", varName)
		} else if valueType != "unknown" && targetType != "unknown" && targetType != valueType {
			p.addSemanticError(valueExpr.GetToken(), "Type mismatch - cannot assign value of type '%s' to variable '%s' (type '%s')", valueType, varName, targetType)
		}

		// Width Check (Error for literals, Warning for expressions)
		valueWidth := valueExpr.ResultWidth()
		declaredWidth := symbolInfo.Width
		if valueType != "unknown" && targetType != "unknown" && valueType == targetType && valueWidth > 0 && declaredWidth > 0 && valueWidth > declaredWidth {
			isLiteralRHS := false
			if _, ok := valueExpr.(*ast.IntegerLiteral); ok {
				isLiteralRHS = true
			}
			if _, ok := valueExpr.(*ast.StringLiteral); ok {
				isLiteralRHS = true
			}

			if isLiteralRHS {
				p.addSemanticError(valueExpr.GetToken(), "Value width %d exceeds variable '%s' width %d", valueWidth, varName, declaredWidth)
			} else {
				p.addWarning(valueExpr.GetToken(), "Width of assigned value (%d) might exceed variable '%s' width (%d). Potential truncation.", valueWidth, varName, declaredWidth)
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

// --- Expression Parsing (Pratt Parser) ---

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

// --- Pratt LED/Infix Functions ---

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	// This function handles binary operators like +, -, *, /
	opToken := p.curTok // The binary operator token
	operator := opToken.Literal
	precedence := tokenPrecedence(opToken)

	p.nextToken() // Consume the operator

	// Parse the right operand with the same or higher precedence
	right := p.parseExpression(precedence)
	if right == nil {
		p.addError(opToken, "Expected expression after operator '%s'", operator)
		return nil // Can't build node without right side
	}

	// --- Semantic Checks for Binary Operation ---
	leftType := left.ResultType()
	rightType := right.ResultType()
	isValidOp := false

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
	} // Add other operators here (e.g., comparison)

	// Handle errors: void operands, type mismatch, invalid operator
	if leftType == "void" || rightType == "void" {
		p.addSemanticError(opToken, "Cannot use void value in binary operation '%s'", operator)
		isValidOp = false // Ensure it's marked invalid
	} else if !isValidOp {
		// Only add error if types are known but incompatible, or operator invalid
		if leftType != "unknown" && rightType != "unknown" {
			errMsg := fmt.Sprintf("Operator '%s' cannot be applied to types '%s' and '%s'", operator, leftType, rightType)
			p.addSemanticError(opToken, errMsg)
		} else {
			// If unknown types involved, previous errors likely exist. Avoid redundant messages.
		}
		// Result type remains unknown
	} else if operator == "/" { // Specific check for division by zero literal
		if lit, ok := right.(*ast.IntegerLiteral); ok && lit.Value == 0 {
			p.addSemanticError(opToken, "Division by literal zero")
			// Result type remains int, but operation is problematic. Width calculation might default.
		}
	}

	// --- Constant Folding ---
	// Check if folding is possible (both operands literals, valid op)
	foldedExpr := p.tryConstantFolding(left, right, opToken)
	if foldedExpr != nil {
		return foldedExpr // Return the folded result directly
	}

	// No folding, build the BinaryExpression node
	expr := &ast.BinaryExpression{
		Token:    opToken,
		Left:     left,
		Operator: operator,
		Right:    right,
	}
	// ResultType/Width will be calculated lazily by the node itself
	return expr
}

// parseProcCallExpression (Pratt Infix/LED Function for Pass 2)
func (p *Parser) parseProcCallExpression(function ast.Expression) ast.Expression {
	ident, ok := function.(*ast.Identifier)
	if !ok {
		p.addError(p.curTok, "Expected identifier before '(' for procedure call, got %T", function)
		p.skipParentheses()
		return nil
	}
	// IDENT already has its Symbol field set by parseIdentifier

	procName := ident.Value

	if ident.Symbol == nil {
		// Error added by parseIdentifier (undeclared)
		p.skipParentheses()
		return nil
	}
	if ident.Symbol.Type != "proc" {
		p.addSemanticError(ident.Token, "'%s' is not a procedure, it's a %s", procName, ident.Symbol.Type)
		p.skipParentheses()
		return nil
	}
	symbolInfo := *ident.Symbol

	expr := &ast.ProcCallExpression{
		Token:               ident.Token, // Use identifier token for location
		Function:            ident,
		ResolvedReturnType:  symbolInfo.ReturnType,
		ResolvedReturnWidth: symbolInfo.ReturnWidth,
	}

	// Parse Arguments
	p.nextToken() // Consume '('
	var err error
	expr.Arguments, err = p.parseExpressionList(token.TokenRParen) // Parses until ')'
	if err != nil {
		return nil // Error handled in parseExpressionList
	}
	// curTok is now after ')'

	// --- Semantic Checks: Argument Count & Types (mostly same as before) ---
	if len(expr.Arguments) != len(symbolInfo.ParamNames) {
		p.addSemanticError(ident.Token, "Procedure '%s' expects %d arguments, but got %d", procName, len(symbolInfo.ParamNames), len(expr.Arguments))
	} else {
		// Check types and widths
		for i, argExpr := range expr.Arguments {
			argType := argExpr.ResultType()
			argWidth := argExpr.ResultWidth()
			// Ensure ParamTypes/Widths exist before indexing
			if i >= len(symbolInfo.ParamTypes) || i >= len(symbolInfo.ParamWidths) {
				p.addError(ident.Token, "Internal Error: Mismatch between ParamNames and ParamTypes/Widths for '%s'", procName)
				break // Stop checking args for this call
			}
			expectedType := symbolInfo.ParamTypes[i]
			expectedWidth := symbolInfo.ParamWidths[i]

			argToken := argExpr.GetToken()

			// Type Check
			if argType == "void" {
				p.addSemanticError(argToken, "Cannot pass result of void procedure call as argument %d to '%s'", i+1, procName)
			} else if argType != "unknown" && argType != expectedType {
				p.addSemanticError(argToken, "Type mismatch for argument %d of '%s'. Expected '%s', got '%s'", i+1, procName, expectedType, argType)
			}

			// Width Check
			if argType != "unknown" && argType != "void" && argType == expectedType && argWidth > 0 && expectedWidth > 0 && argWidth > expectedWidth {
				isLiteralArg := false
				if _, ok := argExpr.(*ast.IntegerLiteral); ok {
					isLiteralArg = true
				}
				if _, ok := argExpr.(*ast.StringLiteral); ok {
					isLiteralArg = true
				}

				if isLiteralArg {
					p.addSemanticError(argToken, "Argument %d width %d exceeds parameter width %d for '%s'", i+1, argWidth, expectedWidth, procName)
				} else {
					p.addWarning(argToken, "Width of argument %d (%d) might exceed parameter width (%d) for '%s'. Potential truncation.", i+1, argWidth, expectedWidth, procName)
				}
			} else if argType != "unknown" && argType != "void" && argType == expectedType && expectedWidth <= 0 {
				p.addWarning(ident.Token, "Could not verify width for argument %d of '%s'; parameter declared width is invalid (%d).", i+1, procName, expectedWidth)
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
