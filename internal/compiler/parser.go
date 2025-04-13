package compiler

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/arnavsurve/grace/internal/compiler/lib"
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
var precedences = map[TokenType]int{
	TokenPlus:     PrecSum,
	TokenMinus:    PrecSum,
	TokenAsterisk: PrecProduct,
	TokenSlash:    PrecProduct,
	TokenLParen:   PrecCall, // For function calls like identifier(...)
}

func tokenPrecedence(tok Token) int {
	if p, ok := precedences[tok.Type]; ok {
		return p
	}
	return PrecLowest
}

type Parser struct {
	l               *Lexer
	curTok          Token
	peekTok         Token
	errors          []string
	warnings        []string              // Separate warnings
	symbolTable     map[string]SymbolInfo // Global symbol table (includes procs and globals)
	currentProcName string                // Track the current procedure being parsed for return checks
	// TODO: Implement proper scoping (e.g., nested symbol tables)
}

func NewParser(l *Lexer) *Parser {
	p := &Parser{
		l:               l,
		errors:          []string{},
		warnings:        []string{},
		symbolTable:     make(map[string]SymbolInfo),
		currentProcName: "", // Not inside a proc initially
	}
	p.nextToken() // Initialize curTok
	p.nextToken() // Initialize peekTok
	return p
}

func (p *Parser) nextToken() {
	p.curTok = p.peekTok
	p.peekTok = p.l.NextToken()
}

func (p *Parser) addError(tok Token, format string, args ...any) {
	msg := fmt.Sprintf(format, args...)
	p.errors = append(p.errors, fmt.Sprintf("Line %d, Col %d: Syntax Error: %s", tok.Line, tok.Column, msg))
}

func (p *Parser) addSemanticError(tok Token, format string, args ...any) {
	msg := fmt.Sprintf(format, args...)
	p.errors = append(p.errors, fmt.Sprintf("Line %d, Col %d: Semantic Error: %s", tok.Line, tok.Column, msg))
}

func (p *Parser) addWarning(tok Token, format string, args ...any) {
	msg := fmt.Sprintf(format, args...)
	// Prefix warnings for clarity
	p.warnings = append(p.warnings, fmt.Sprintf("Line %d, Col %d: Semantic Warning: %s", tok.Line, tok.Column, msg))
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
func (p *Parser) getSymbolInfo(name string) (SymbolInfo, bool) {
	// TODO: Implement scope lookup when nesting is added
	info, exists := p.symbolTable[name]
	return info, exists
}

// --- Program Parsing ---

func (p *Parser) ParseProgram() *Program {
	program := &Program{
		Statements:  []Statement{},
		SymbolTable: p.symbolTable, // Share the final symbol table
	}

	for p.curTok.Type != TokenEOF {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		} else {
			// If parseStatement returned nil, it implies an error occurred
			// and the responsible parsing function should have consumed
			// the problematic token(s) to allow recovery.
			// We simply continue to the next token if EOF hasn't been reached.
			// Avoids infinite loops if a parser returns nil without consuming.
			if p.curTok.Type != TokenEOF {
				// As a safeguard, if we are here and didn't parse a statement,
				// advance the token to prevent potential infinite loops,
				// though individual parsers *should* handle this.
				// fmt.Printf("DEBUG: Advancing token after failed statement parse: %v\n", p.curTok)
				// p.nextToken() // Use cautiously if needed
			}
		}
	}

	return program
}

// --- Statement Parsing ---

func (p *Parser) parseStatement() Statement {
	switch p.curTok.Type {
	case TokenProc:
		return p.parseProcDeclarationStatement()
	case TokenReturn:
		return p.parseReturnStatement()
	case TokenPrint:
		return p.parsePrintStatement()
	case TokenConst:
		return p.parseDeclarationStatement() // Handles const modifier
	case TokenIdent:
		// Look ahead to determine declaration, reassignment, or proc call
		switch p.peekTok.Type {
		case TokenAssignDefine, TokenColon: // x := ... or x : type = ...
			return p.parseDeclarationStatement()
		case TokenAssign: // x = ...
			return p.parseReassignmentStatement()
		case TokenLParen: // x(...)
			// Parse as an expression statement, which handles proc calls
			return p.parseExpressionStatement()
		default:
			p.addError(p.peekTok, "Expected ':=', ':', '=', or '(' after identifier '%s', got '%s'", p.curTok.Literal, p.peekTok.Type)
			p.nextToken() // Consume IDENT
			// Consume the unexpected token to attempt recovery
			if p.curTok.Type != TokenEOF && p.curTok.Type != TokenRBrace { // Avoid consuming closing brace
				p.nextToken()
			}
			return nil
		}
	case TokenLBrace: // Block statement cannot occur outside procedures
		p.addError(p.curTok, "Unexpected '{' at the start of a top-level statement")
		p.skipBlock() // Attempt to skip the block
		return nil
	case TokenEOF:
		return nil // End of input
	default:
		// Could be an expression statement starting with a literal or '('.
		// Try parsing as expression statement. If that fails, it's a syntax error.
		stmt := p.parseExpressionStatement()
		if stmt != nil {
			// Check if it was just a literal/identifier without side effect (optional warning)
			// Example: "hello" or 5 or myVar as a statement
			// These have no effect in COBOL unless it's a proc call.
			if _, isLit := stmt.Expression.(*StringLiteral); isLit {
				p.addWarning(stmt.Token, "String literal used as statement has no effect.")
			} else if _, isLit := stmt.Expression.(*IntegerLiteral); isLit {
				p.addWarning(stmt.Token, "Integer literal used as statement has no effect.")
			} else if _, isIdent := stmt.Expression.(*Identifier); isIdent {
				// Check if it's a variable identifier, not a proc call handled elsewhere
				p.addWarning(stmt.Token, "Identifier '%s' used as statement has no effect.", stmt.Expression.(*Identifier).Value)
			}
			return stmt
		}
		// If parseExpressionStatement returned nil, it means parseExpression failed.
		// The error should have been added there. We don't add another one here.
		// Ensure the problematic token was consumed by parseExpression or its callers.
		if len(p.errors) == 0 { // If no specific error was added, add a generic one
			p.addError(p.curTok, "Unexpected token at start of statement: %s ('%s')", p.curTok.Type, p.curTok.Literal)
		}
		// Consume unexpected token to attempt recovery if it wasn't consumed already
		if p.curTok.Type != TokenEOF && p.curTok.Type != TokenRBrace {
			p.nextToken()
		}
		return nil
	}
}

// parseDeclarationStatement parses `[const] name := value` or `[const] name: type[(width)] = value`
func (p *Parser) parseDeclarationStatement() Statement {
	stmt := &DeclarationStatement{Token: p.curTok} // Store first token (const or ident)

	// 1. Handle optional 'const'
	if p.curTok.Type == TokenConst {
		stmt.IsConst = true
		p.nextToken() // Consume 'const'
		if p.curTok.Type != TokenIdent {
			p.addError(p.curTok, "Expected identifier after 'const', got %s", p.curTok.Type)
			if p.curTok.Type != TokenEOF && p.curTok.Type != TokenRBrace {
				p.nextToken()
			} // Consume bad token
			return nil
		}
	}

	// 2. Expect IDENT
	if p.curTok.Type != TokenIdent {
		// This case should only be reached if 'const' wasn't present,
		// as the 'const' case checks for IDENT above.
		// However, the entry point to this function also checks for IDENT or CONST.
		// This path indicates an internal logic error if reached.
		p.addError(p.curTok, "Internal Parser Error: Expected identifier for declaration start, got %s", p.curTok.Type)
		if p.curTok.Type != TokenEOF && p.curTok.Type != TokenRBrace {
			p.nextToken()
		} // Consume bad token
		return nil
	}
	identToken := p.curTok
	varName := identToken.Literal
	stmt.Name = &Identifier{Token: identToken, Value: varName}

	// 3. Check for Redeclaration (in current scope - global only for now)
	// TODO: Scope awareness needed for parameters/locals vs globals
	_, declared := p.getSymbolInfo(varName)
	if declared {
		// Use Semantic Error for redeclarations
		p.addSemanticError(identToken, "Variable '%s' already declared in this scope", varName)
		// Continue parsing to find more errors, but don't add to symbol table later
	}

	var explicitTypeNode *TypeNode = nil // Store parsed explicit type info

	// 4. Check for ':=' (Inferred) or ':' (Explicit) and consume tokens up to '=' or expression start
	switch p.peekTok.Type {
	case TokenColon:
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
			// Attempt to continue parsing if possible, but the declaration is invalid
			// Skip to where assignment value might start? Difficult. Return nil.
			return nil
		}
		explicitTypeNode = typeNode
		stmt.ExplicitTypeToken = typeNode.Token       // Store 'int' or 'string' token
		stmt.ExplicitWidthToken = typeNode.WidthToken // Store '10' token if present

		// Expect '=' for assignment after explicit type
		if p.curTok.Type != TokenAssign {
			p.addError(p.curTok, "Expected '=' after type specification for '%s', got %s ('%s')", varName, p.curTok.Type, p.curTok.Literal)
			if p.curTok.Type != TokenEOF && p.curTok.Type != TokenRBrace {
				p.nextToken()
			} // Consume bad token
			return nil
		}
		stmt.Token = p.curTok // Store '=' token
		p.nextToken()         // Consume '=', curTok is now start of expression

	case TokenAssignDefine:
		// --- Inferred Type Path: `name := value` ---
		stmt.HasExplicitType = false
		p.nextToken()         // Consume IDENT. curTok is now ':='
		stmt.Token = p.curTok // Store ':=' token
		p.nextToken()         // Consume ':='. curTok is now start of expression

	default:
		p.addError(p.peekTok, "Expected ':=' or ':' after identifier '%s' in declaration, got '%s'", varName, p.peekTok.Type)
		p.nextToken() // Consume IDENT
		if p.curTok.Type != TokenEOF && p.curTok.Type != TokenRBrace {
			p.nextToken()
		} // Consume unexpected peek token
		return nil
	}

	// 5. Parse the Value Expression
	// Use the main expression parser which handles precedence
	valueExpr := p.parseExpression(PrecLowest)
	if valueExpr == nil {
		// If parsing the expression failed, an error should already be recorded.
		// Check if we need to add a generic error if parseExpression didn't.
		if len(p.errors) == 0 {
			p.addError(p.curTok, "Expected expression after '%s'", stmt.Token.Literal)
		}
		return nil
	}
	stmt.Value = valueExpr
	// After parseExpression, curTok is the token *after* the expression

	// --- 6. Semantic Analysis & Symbol Table Update ---
	valueType := valueExpr.ResultType()
	valueWidth := valueExpr.ResultWidth()
	var finalType string = "unknown" // Start assuming unknown
	var finalWidth int = 0

	if valueType == "void" {
		p.addSemanticError(valueExpr.GetToken(), "Cannot assign result of void procedure call to variable '%s'", varName)
		valueType = "unknown" // Mark as error state to prevent symbol table entry
	}
	if valueType == "unknown" && len(p.errors) == 0 {
		// If RHS expression parsing resulted in 'unknown' without adding a specific error
		p.addError(stmt.Token, "Internal Error: Expression resulted in 'unknown' type without specific error for '%s' assignment", varName)
	}

	if stmt.HasExplicitType { // Explicit Type (`name : type = value`)
		finalType = explicitTypeNode.Name   // Use explicitly declared type name
		finalWidth = explicitTypeNode.Width // Use explicitly declared width (which includes default if not specified)

		// Check type compatibility (allow unknown types to pass here, focus on known mismatches)
		if valueType != "unknown" && finalType != "unknown" && finalType != valueType {
			p.addSemanticError(valueExpr.GetToken(), "Type mismatch - cannot assign value of type '%s' to variable '%s' declared as '%s'", valueType, varName, finalType)
			finalType = "unknown" // Mark as error state
		}

		// Check if literal value fits explicit width (Error, not warning, for literals)
		if finalType != "unknown" {
			if litInt, ok := valueExpr.(*IntegerLiteral); ok && finalType == "int" {
				reqWidth := lib.CalculateWidthForValue(litInt.Value) // Width needed for the literal itself
				if reqWidth > finalWidth {
					p.addSemanticError(valueExpr.GetToken(), "Integer literal %s requires width %d, but variable '%s' declared with width %d", litInt.Token.Literal, reqWidth, varName, finalWidth)
					// Don't change finalWidth, just report error. Emitter might truncate.
				}
			} else if litStr, ok := valueExpr.(*StringLiteral); ok && finalType == "string" {
				reqWidth := len(litStr.Value) // String literal length
				if reqWidth > finalWidth {
					p.addSemanticError(valueExpr.GetToken(), "String literal with length %d exceeds declared width %d for variable '%s'", reqWidth, finalWidth, varName)
				}
			}
			// Note: Width check for non-literal expressions (vars, function calls, binary ops)
			// is harder at parse time without full evaluation. The emitter relies on the
			// declared width `finalWidth` being the target size. We only issue warnings
			// for potential truncation during *reassignment*, not declaration.
		}

	} else { // Inferred Type (`name := value`)
		if valueType != "unknown" {
			finalType = valueType // Type is inferred from expression
			// Width is also inferred from expression (or default if expression width is unknown)
			finalWidth = valueWidth
			if finalWidth <= 0 { // If expression had unknown/zero width, use default
				finalWidth = lib.GetDefaultWidth(finalType)
				if finalWidth <= 0 && finalType != "unknown" { // Fallback if default is also zero (e.g., for a new base type)
					p.addWarning(stmt.Name.Token, "Could not determine valid width for inferred type '%s' for variable '%s'. Using width 1.", finalType, varName)
					finalWidth = 1
				}
			}
		} else {
			// valueType was unknown from expression parsing
			finalType = "unknown"
			finalWidth = 0
		}
	}

	// --- 7. Final Validation and Symbol Table Update ---
	// Ensure width is valid before adding to symbol table
	if finalType != "unknown" && finalWidth <= 0 {
		// This might happen if inferred type had issues or explicit width was invalid (e.g., 0)
		p.addWarning(stmt.Name.Token, "Final width for '%s' (type %s) resolved to %d, using 1.", varName, finalType, finalWidth)
		finalWidth = 1
	}

	// Add to symbol table ONLY if not a redeclaration AND type is known/valid
	if !declared && finalType != "unknown" && finalType != "void" {
		p.symbolTable[varName] = SymbolInfo{
			Type:        finalType,
			IsConst:     stmt.IsConst,
			Width:       finalWidth,
			ParamNames:  nil, // Ensure proc fields are clear for non-procs
			ParamTypes:  nil,
			ParamWidths: nil,
			ReturnType:  "",
			ReturnWidth: 0,
		}
		// Update the identifier node in the AST with resolved info
		stmt.Name.ResolvedType = finalType
		stmt.Name.Width = finalWidth
	} else {
		// Handle cases where symbol wasn't added or is invalid:
		// - Redeclaration (error added earlier)
		// - Error occurred determining type/width (error added earlier)
		// Still mark the AST node, even if invalid, to potentially aid later analysis/error reporting
		stmt.Name.ResolvedType = finalType
		stmt.Name.Width = finalWidth
	}

	// parseExpression consumed tokens including the last part of the expression. No nextToken() needed.
	return stmt
}

// parseProcDeclarationStatement parses `proc name(params...) : retType { body }`
func (p *Parser) parseProcDeclarationStatement() *ProcDeclarationStatement {
	startTok := p.curTok // Store 'proc' token
	stmt := &ProcDeclarationStatement{Token: startTok}

	// 1. Parse 'proc' IDENTIFIER
	if p.curTok.Type != TokenProc { // Should be called when curTok is 'proc'
		p.addError(p.curTok, "Internal Parser Error: Expected 'proc' to start procedure declaration.")
		return nil
	}
	if !p.expectPeek(TokenIdent) { // Consumes 'proc', checks peek is IDENT. If yes, consumes IDENT.
		p.addError(p.peekTok, "Expected procedure name (identifier) after 'proc', got %s", p.peekTok.Type)
		return nil
	}
	// curTok is now the procedure name IDENT
	stmt.Name = &Identifier{Token: p.curTok, Value: p.curTok.Literal}
	procName := stmt.Name.Value
	previousProcName := p.currentProcName // Save context for potential nesting
	p.currentProcName = procName          // Set context for return statement checks

	// 2. Check for Redeclaration (using the name just parsed)
	_, exists := p.getSymbolInfo(procName) // Use getSymbolInfo consistently
	if exists {
		p.addSemanticError(stmt.Name.Token, "Redeclaration of '%s'", procName)
		// Continue parsing signature/body to find more errors, but don't add to symbol table later
	}

	// 3. Parse Parameter List `(...)`
	if !p.expectPeek(TokenLParen) { // Consumes IDENT, checks peek is '('. If yes, consumes '('.
		p.addError(p.peekTok, "Expected '(' after procedure name '%s', got %s", procName, p.peekTok.Type)
		p.currentProcName = previousProcName
		return nil
	}
	// curTok is now '('
	params, paramNames, err := p.parseParameterList() // Expects curTok = '(', leaves curTok = ')'
	if err != nil {
		p.currentProcName = previousProcName
		return nil // Error handled in parseParameterList
	}
	stmt.Parameters = params
	// After parseParameterList returns successfully, curTok MUST be ')'

	// 4. Parse Return Type `: type`
	if p.curTok.Type != TokenRParen { // Sanity check
		p.addError(p.curTok, "Internal Parser Error: Expected ')' after parameter list parsing, got %s", p.curTok.Type)
		p.currentProcName = previousProcName
		return nil
	}
	if p.peekTok.Type != TokenColon { // Check token AFTER ')'
		p.addError(p.peekTok, "Expected ':' after '()' for procedure '%s' to specify return type, got %s", procName, p.peekTok.Type)
		p.currentProcName = previousProcName
		return nil
	}
	p.nextToken() // Consume ')'
	p.nextToken() // Consume ':' - curTok is now the start of the type node

	returnTypeNode := p.parseTypeNode() // Parses the type node
	if returnTypeNode == nil {
		p.currentProcName = previousProcName
		return nil // Error during type parsing
	}
	stmt.ReturnType = returnTypeNode
	// curTok is now the token after the type spec (should be '{')

	// 5. --- PRE-ADD Symbol Info (before body parsing) ---
	// Create SymbolInfo based on the parsed signature
	info := SymbolInfo{
		Type:        "proc",
		IsConst:     true,
		Width:       0, // N/A
		ParamNames:  paramNames,
		ParamTypes:  make([]string, len(params)),
		ParamWidths: make([]int, len(params)),
		ReturnType:  returnTypeNode.Name,  // Default to parsed type name
		ReturnWidth: returnTypeNode.Width, // Use parsed width (includes default)
	}
	validSignature := true // Assume valid initially
	if returnTypeNode.Name == "unknown" {
		validSignature = false // Return type parsing failed
	}
	for i, param := range params { // Validate parameters again before adding to symbol table
		if param.TypeNode == nil || param.TypeNode.Name == "unknown" || param.TypeNode.IsVoid || param.TypeNode.Width <= 0 {
			validSignature = false // Param definition had errors
			break
		}
		info.ParamTypes[i] = param.TypeNode.Name
		info.ParamWidths[i] = param.TypeNode.Width
	}

	// Add to symbol table ONLY if not a redeclaration AND the signature was valid
	if !exists && validSignature {
		p.symbolTable[procName] = info
		stmt.Name.ResolvedType = "proc" // Mark AST node as valid proc
	} else {
		// Mark AST node as unknown if signature invalid or redeclared
		stmt.Name.ResolvedType = "unknown"
		// Ensure subsequent lookups inside the body fail cleanly if signature was bad
		if !exists { // If it wasn't a redeclaration but signature was bad, add dummy entry
			p.symbolTable[procName] = SymbolInfo{Type: "unknown"}
		}
		// No need to set validSignature = false here, it's already implied or was set above
	}
	// --- End Symbol Info Pre-Add ---

	// 6. Parse Procedure Body `{...}`
	if p.curTok.Type != TokenLBrace { // Expect '{' after type parsing
		p.addError(p.curTok, "Expected '{' to start procedure body after return type, got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		p.currentProcName = previousProcName
		return nil
	}

	// TODO: Enter new scope here, add parameters to local scope

	// Parse the block statement (calls parseStatement recursively, which can call parseReturnStatement)
	stmt.Body = p.parseBlockStatement()
	if stmt.Body == nil {
		// Error handled in parseBlockStatement
		p.currentProcName = previousProcName
		// TODO: Exit scope here on error too
		return nil
	}
	// parseBlockStatement consumed '}'

	// TODO: Exit scope here after successful body parse

	// 7. Cleanup and Return
	p.currentProcName = previousProcName // Restore outer context
	return stmt
}

// parseTypeNode parses a type specification: `type` or `type(width)` or `void`
// Used for explicit variable types, parameters, and return types.
// Expects curTok to be the type name (IDENT or VOID). Consumes tokens through type spec.
// Returns the TypeNode or nil on error.
// IMPORTANT: If type is 'int' or 'string' and width is not specified,
// it assigns the default width to the TypeNode.
func (p *Parser) parseTypeNode() *TypeNode {
	// Expect 'int', 'string', or 'void'
	isKnownTypeLiteral := p.curTok.Type == TokenTypeLiteral && (p.curTok.Literal == "int" || p.curTok.Literal == "string")
	isVoidType := p.curTok.Type == TokenVoid

	if !isKnownTypeLiteral && !isVoidType {
		p.addError(p.curTok, "Expected type name (int, string, void), got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		// Attempt recovery by consuming the bad token? Let caller handle.
		// If we consume here, caller might misinterpret next token. Better to return nil.
		// p.nextToken()
		return nil
	}

	typeTok := p.curTok
	typeName := typeTok.Literal
	node := &TypeNode{
		Token:  typeTok,
		Name:   typeName,
		IsVoid: isVoidType,
		Width:  0, // Default, will be updated below
	}
	p.nextToken() // Consume type name token ('int', 'string', 'void')

	// Parse optional width `(INT)` only if not void
	if !node.IsVoid && p.curTok.Type == TokenLParen {
		p.nextToken() // Consume '('
		if p.curTok.Type != TokenInt {
			p.addError(p.curTok, "Expected integer width inside parentheses for type '%s', got %s ('%s')", typeName, p.curTok.Type, p.curTok.Literal)
			// Recovery? Skip until ')'? Difficult. Return nil.
			// Consume the bad token inside parens if it's not ')'
			if p.curTok.Type != TokenRParen && p.curTok.Type != TokenEOF {
				p.nextToken()
			}
			// If we now see ')', consume it.
			if p.curTok.Type == TokenRParen {
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

		if p.curTok.Type != TokenRParen {
			p.addError(p.curTok, "Expected ')' after width specification, got %s ('%s')", p.curTok.Type, p.curTok.Literal)
			// Don't consume if not ')', let caller handle
			return nil // Error, stop parsing type node
		}
		p.nextToken() // Consume ')'
	} else if !node.IsVoid && node.Width == 0 {
		// If not void and width wasn't explicitly set (or was invalidly set to 0), assign default width
		defaultWidth := lib.GetDefaultWidth(typeName)
		if defaultWidth == 0 {
			// This should only happen for unknown types, which already errored. Safety check.
			p.addError(typeTok, "Internal Error: Could not determine default width for known type '%s'", typeName)
			// Mark width as 0 or 1? Let's use 0 to indicate error.
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
func (p *Parser) parseParameterList() ([]*Parameter, []string, error) {
	params := []*Parameter{}
	paramNames := []string{}
	paramNameSet := make(map[string]bool) // For checking duplicate names

	if p.curTok.Type != TokenLParen { // Should be called only when curTok is '('
		p.addError(p.curTok, "Internal Parser Error: Expected '(' to start parameter list, got %s", p.curTok.Type)
		return nil, nil, fmt.Errorf("internal error: missing '('")
	}

	// Check for empty list: ()
	if p.peekTok.Type == TokenRParen {
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
	for p.curTok.Type == TokenComma {

		p.nextToken() // Consume ','

		// Handle trailing comma case: `(a: int(1), )`
		if p.curTok.Type == TokenRParen {
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
	if p.curTok.Type != TokenRParen {
		p.addError(p.curTok, "Expected ',' or ')' after parameter, got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		// Recovery? Skip until ')'? For now, return error.
		return nil, nil, fmt.Errorf("syntax error in parameter list")
	}

	return params, paramNames, nil
}

// parseSingleParameter parses `ident: type(width)` - width is MANDATORY here.
// Expects curTok to be the identifier. Consumes up to token after type spec.
func (p *Parser) parseSingleParameter() (*Parameter, error) {
	if p.curTok.Type != TokenIdent {
		p.addError(p.curTok, "Expected parameter name (identifier), got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		if p.curTok.Type != TokenEOF && p.curTok.Type != TokenColon && p.curTok.Type != TokenRParen {
			p.nextToken()
		} // Consume bad token
		return nil, fmt.Errorf("expected parameter name")
	}
	ident := &Identifier{Token: p.curTok, Value: p.curTok.Literal}

	if !p.expectPeek(TokenColon) {
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
		// Mark as error but return node for potential partial AST usefulness? Or nil? Let's return nil.
		return nil, fmt.Errorf("parameter cannot be void")
	}
	// Check if width was explicitly provided (it's mandatory for params)
	// parseTypeNode sets Width > 0 if explicit width was given, or 0 if not.
	// It does NOT assign default width for parameters.
	if typeNode.Width <= 0 { // Check if a valid positive width was parsed
		// WidthToken check is more precise if parseTypeNode preserves it correctly
		widthMissing := typeNode.WidthToken.Type != TokenInt
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

	// Parameter definition is valid, update identifier node info
	ident.ResolvedType = typeNode.Name
	ident.Width = typeNode.Width // Use the explicitly parsed width

	// TODO: Add parameter to current scope's symbol table when scopes are implemented

	return &Parameter{Name: ident, TypeNode: typeNode}, nil
}

// parseBlockStatement parses `{ statements... }`
func (p *Parser) parseBlockStatement() *BlockStatement {
	block := &BlockStatement{Token: p.curTok} // Store '{' token
	block.Statements = []Statement{}

	// Expect curTok to be LBrace
	if p.curTok.Type != TokenLBrace {
		p.addError(p.curTok, "Internal Parser Error: Expected '{' to start block, got %s", p.curTok.Type)
		return nil
	}
	p.nextToken() // Consume '{'

	for p.curTok.Type != TokenRBrace && p.curTok.Type != TokenEOF {
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		} else {
			// If parseStatement returned nil due to an error, it should have consumed
			// the problematic token. If it was EOF, the loop condition handles it.
			// If it was some other reason, we might loop infinitely.
			// Safeguard: if errors occurred and we haven't reached the end or }, advance.
			if len(p.errors) > 0 && p.curTok.Type != TokenRBrace && p.curTok.Type != TokenEOF {
				// fmt.Printf("DEBUG: Advancing token after failed statement parse in block: %v\n", p.curTok)
				// p.nextToken() // Use cautiously
			}
		}
	}

	if p.curTok.Type != TokenRBrace {
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
	if p.curTok.Type != TokenLBrace {
		return
	} // Should not happen if called correctly
	p.nextToken() // Consume the initial '{'

	for openCount > 0 && p.curTok.Type != TokenEOF {
		switch p.curTok.Type {
		case TokenLBrace:
			openCount++
		case TokenRBrace:
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
func (p *Parser) parseReturnStatement() Statement {
	stmt := &ReturnStatement{Token: p.curTok}
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
		procInfo = SymbolInfo{ReturnType: "unknown"} // Fallback for checks, treat as error state
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
			if p.curTok.Type == TokenRBrace || p.curTok.Type == TokenEOF {
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
			if _, ok := returnValue.(*IntegerLiteral); ok {
				isLiteralReturn = true
			}
			if _, ok := returnValue.(*StringLiteral); ok {
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
func (p *Parser) isExpressionStart(tok Token) bool {
	switch tok.Type {
	case TokenInt, TokenString, TokenIdent, TokenLParen, TokenMinus, TokenPlus: // Add others if needed (unary ops?)
		return true
	default:
		return false
	}
}

// skipExpressionTokens tries to consume tokens that likely form an expression.
// Basic recovery for syntax errors. Stops at potential statement terminators.
func (p *Parser) skipExpressionTokens() {
	for !p.isStatementEnd(p.curTok) && p.curTok.Type != TokenEOF {
		// TODO: Handle nested parentheses/braces correctly if needed
		p.nextToken()
	}
}

// isStatementEnd checks for tokens that typically end a simple statement (in Grace's case, context-dependent)
func (p *Parser) isStatementEnd(tok Token) bool {
	// Newlines are handled implicitly by the main parsing loop.
	// RBrace usually ends a block/statement within it.
	// EOF ends the program.
	// Specific keywords might end expressions (e.g., 'proc', 'const' starting next line).
	switch tok.Type {
	case TokenRBrace, TokenEOF, TokenProc, TokenConst, TokenReturn, TokenPrint:
		return true
	default:
		return false
	}
}

// parseExpressionStatement handles statements that are just expressions (like proc calls)
func (p *Parser) parseExpressionStatement() *ExpressionStatement {
	startTok := p.curTok
	stmt := &ExpressionStatement{Token: startTok}

	expr := p.parseExpression(PrecLowest)
	if expr == nil {
		// Error reported by parseExpression or sub-parsers
		return nil
	}
	stmt.Expression = expr

	// --- Semantic Check: Allow ignoring non-void results? ---
	// Current decision: Yes, allow ignoring results (like C, Go). Add optional warning.
	if call, ok := stmt.Expression.(*ProcCallExpression); ok {
		// Check the *resolved* return type from the call expression node
		if call.ResultType() != "void" {
			p.addWarning(stmt.Token, "Result of non-void procedure call '%s' is ignored.", call.Function.Value)
		}
	} else if _, ok := stmt.Expression.(*IntegerLiteral); ok {
		p.addWarning(stmt.Token, "Integer literal used as statement has no effect.")
	} else if _, ok := stmt.Expression.(*StringLiteral); ok {
		p.addWarning(stmt.Token, "String literal used as statement has no effect.")
	} else if ident, isIdent := stmt.Expression.(*Identifier); isIdent {
		// Check if it's a variable identifier (proc identifiers shouldn't reach here standalone)
		if ident.ResolvedType != "proc" && ident.ResolvedType != "unknown" {
			p.addWarning(stmt.Token, "Variable '%s' used as statement has no effect.", ident.Value)
		}
	} // Could add more checks for pointless statements (e.g., binary op without assignment).

	// If parseExpression succeeded, curTok is already advanced past the expression.
	return stmt
}

// parseReassignmentStatement parses `name = value`
func (p *Parser) parseReassignmentStatement() Statement {
	stmt := &ReassignmentStatement{}

	// Current token is IDENT
	if p.curTok.Type != TokenIdent {
		// Should not happen based on call site logic
		p.addError(p.curTok, "Internal Parser Error: Expected identifier for reassignment start, got %s", p.curTok.Type)
		if p.curTok.Type != TokenEOF && p.curTok.Type != TokenRBrace {
			p.nextToken()
		}
		return nil
	}
	identToken := p.curTok
	stmt.Name = &Identifier{Token: identToken, Value: p.curTok.Literal}
	varName := stmt.Name.Value

	// Semantic Check 1 & 2: Is declared? Is const?
	// TODO: Scope awareness
	symbolInfo, declared := p.getSymbolInfo(varName)
	isConst := false
	isValidTarget := false // Track if assignment is potentially valid

	if !declared {
		p.addSemanticError(identToken, "Cannot assign to undeclared variable '%s'", varName)
		// Mark identifier as unknown in AST for potential later use in error reporting
		stmt.Name.ResolvedType = "unknown"
		stmt.Name.Width = 0
	} else if symbolInfo.Type == "proc" {
		p.addSemanticError(identToken, "Cannot assign to procedure '%s'", varName)
		stmt.Name.ResolvedType = "proc" // Keep type info
		stmt.Name.Width = 0
	} else {
		// Variable exists and is not a proc
		if symbolInfo.IsConst {
			p.addSemanticError(identToken, "Cannot assign to constant variable '%s'", varName)
			isConst = true
		}
		// Set identifier info from symbol table
		stmt.Name.ResolvedType = symbolInfo.Type
		stmt.Name.Width = symbolInfo.Width
		isValidTarget = !isConst // Valid target if declared and not const
	}

	// Expect '='
	// Use expectPeek for clarity and correct token consumption
	if !p.expectPeek(TokenAssign) {
		// Error added by expectPeek
		return nil
	}
	// curTok is now '='
	stmt.Token = p.curTok // Store '=' token

	// Parse RHS expression
	p.nextToken() // Consume '=', curTok is now expression start
	valueExpr := p.parseExpression(PrecLowest)
	if valueExpr == nil {
		return nil
	} // Error handled in parseExpression
	stmt.Value = valueExpr

	// --- Semantic Check 3: Type compatibility ---
	if isValidTarget { // Only check types if the target variable itself is valid
		valueType := valueExpr.ResultType()
		targetType := symbolInfo.Type // Use resolved type from symbol table

		if valueType == "void" {
			p.addSemanticError(valueExpr.GetToken(), "Cannot assign result of void procedure call to variable '%s'", varName)
		} else if valueType != "unknown" && targetType != "unknown" && targetType != valueType {
			p.addSemanticError(valueExpr.GetToken(), "Type mismatch - cannot assign value of type '%s' to variable '%s' (type '%s')", valueType, varName, targetType)
		}

		// --- Semantic Check 4: Width compatibility (Warning/Error) ---
		valueWidth := valueExpr.ResultWidth()
		declaredWidth := symbolInfo.Width
		if valueType != "unknown" && targetType != "unknown" && valueType == targetType && valueWidth > 0 && declaredWidth > 0 && valueWidth > declaredWidth {
			// Check if the RHS is a literal - if so, this is an error, not a warning
			isLiteralRHS := false
			if _, ok := valueExpr.(*IntegerLiteral); ok {
				isLiteralRHS = true
			}
			if _, ok := valueExpr.(*StringLiteral); ok {
				isLiteralRHS = true
			}

			if isLiteralRHS {
				// Use SemanticError for literals exceeding width
				p.addSemanticError(valueExpr.GetToken(), "Value width %d exceeds variable '%s' width %d", valueWidth, varName, declaredWidth)
			} else {
				// For non-literals (vars, expressions), issue a warning
				p.addWarning(valueExpr.GetToken(), "Width of assigned value (%d) might exceed variable '%s' width (%d). Potential truncation.", valueWidth, varName, declaredWidth)
			}
		}
	}

	// parseExpression consumed all its tokens. No nextToken() needed.
	return stmt
}

// parsePrintStatement parses `print(expression)`
func (p *Parser) parsePrintStatement() Statement {
	stmt := &PrintStatement{Token: p.curTok} // Store PRINT token

	// Expect '('
	if !p.expectPeek(TokenLParen) {
		// Error added by expectPeek
		return nil
	}
	// curTok is now '('

	p.nextToken() // Consume '(', curTok is now start of expression

	// Parse the expression to print
	valueExpr := p.parseExpression(PrecLowest)
	if valueExpr == nil {
		// Error handled in parseExpression. Check if we need ')'.
		if p.curTok.Type == TokenRParen {
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
	if p.curTok.Type != TokenRParen {
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
	prefixParseFn func() Expression           // NUD (Null Denotation)
	infixParseFn  func(Expression) Expression // LED (Left Denotation)
)

var (
	prefixParseFns map[TokenType]prefixParseFn
	infixParseFns  map[TokenType]infixParseFn
)

// registerPrefix associates a token type with its prefix parsing function.
func (p *Parser) registerPrefix(tokenType TokenType, fn prefixParseFn) {
	if prefixParseFns == nil {
		prefixParseFns = make(map[TokenType]prefixParseFn)
	}
	prefixParseFns[tokenType] = fn
}

// registerInfix associates a token type with its infix parsing function.
func (p *Parser) registerInfix(tokenType TokenType, fn infixParseFn) {
	if infixParseFns == nil {
		infixParseFns = make(map[TokenType]infixParseFn)
	}
	infixParseFns[tokenType] = fn
}

// Initialize Pratt parser functions (call this in NewParser or lazily)
func (p *Parser) initializePratt() {
	// Prefixes (NUDs)
	p.registerPrefix(TokenIdent, p.parseIdentifier)
	p.registerPrefix(TokenInt, p.parseIntegerLiteral)
	p.registerPrefix(TokenString, p.parseStringLiteral)
	p.registerPrefix(TokenLParen, p.parseGroupedExpression)
	// p.registerPrefix(TokenMinus, p.parsePrefixExpression) // Example for unary minus
	// p.registerPrefix(TokenPlus, p.parsePrefixExpression) // Example for unary plus

	// Infixes (LEDs)
	p.registerInfix(TokenPlus, p.parseInfixExpression)
	p.registerInfix(TokenMinus, p.parseInfixExpression)
	p.registerInfix(TokenAsterisk, p.parseInfixExpression)
	p.registerInfix(TokenSlash, p.parseInfixExpression)
	p.registerInfix(TokenLParen, p.parseProcCallExpression) // For func(...) style calls
}

// parseExpression is the main entry point for Pratt parsing.
func (p *Parser) parseExpression(precedence int) Expression {
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

func (p *Parser) parseIntegerLiteral() Expression {
	token := p.curTok
	val, err := strconv.ParseInt(token.Literal, 10, 64) // Use 64-bit intermediate parsing
	if err != nil {
		p.addError(token, "Could not parse integer literal '%s': %v", token.Literal, err)
		p.nextToken() // Consume bad token
		return nil
	}
	// TODO: Check if val exceeds COBOL limits if necessary (e.g., 18 digits)

	lit := &IntegerLiteral{
		Token: token,
		Value: int(val), // Store as standard Go int
		Width: lib.CalculateWidthForValue(int(val)),
	}
	p.nextToken() // Consume the integer token
	return lit
}

func (p *Parser) parseStringLiteral() Expression {
	token := p.curTok
	expr := &StringLiteral{
		Token: token,
		Value: token.Literal, // Literal value already extracted by lexer
		Width: len(token.Literal),
	}
	p.nextToken() // Consume the string token
	return expr
}

func (p *Parser) parseIdentifier() Expression {
	// This is called when an identifier appears where a value is expected (prefix context)
	token := p.curTok
	varName := token.Literal
	// TODO: Scope lookup
	symbolInfo, declared := p.getSymbolInfo(varName)
	expr := &Identifier{Token: token, Value: varName}

	if !declared {
		p.addSemanticError(token, "Identifier '%s' used before declaration", varName)
		expr.ResolvedType = "unknown"
		expr.Width = 0
	} else {
		// Populate type and width from symbol table for variables
		expr.ResolvedType = symbolInfo.Type
		expr.Width = symbolInfo.Width
	}
	p.nextToken() // Consume identifier
	return expr
}

func (p *Parser) parseGroupedExpression() Expression {
	startToken := p.curTok // Store '('
	p.nextToken()          // Consume '('

	// Parse the inner expression using the lowest precedence
	expr := p.parseExpression(PrecLowest)
	if expr == nil {
		// Error already reported by parseExpression. Attempt recovery.
		p.skipUntil(TokenRParen) // Skip until ')'
		if p.curTok.Type == TokenRParen {
			p.nextToken()
		} // Consume ')' if found
		return nil
	}

	// Expect ')'
	if p.curTok.Type != TokenRParen {
		p.addError(p.curTok, "Expected ')' after expression in parentheses, got %s ('%s')", p.curTok.Type, p.curTok.Literal)
		// Don't consume if not ')', let outer parsers handle recovery
		return nil
	}
	p.nextToken() // Consume ')'

	// Wrap in GroupedExpression node OR return inner expr directly.
	// Wrapping helps if we need to distinguish (a+b) from a+b later.
	// return expr // Simpler if grouping is only for precedence
	return &GroupedExpression{Token: startToken, Expression: expr} // Keep structure
}

// --- Pratt LED/Infix Functions ---

func (p *Parser) parseInfixExpression(left Expression) Expression {
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
		if lit, ok := right.(*IntegerLiteral); ok && lit.Value == 0 {
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
	expr := &BinaryExpression{
		Token:    opToken,
		Left:     left,
		Operator: operator,
		Right:    right,
	}
	// ResultType/Width will be calculated lazily by the node itself
	return expr
}

// parseProcCallExpression (as LED/Infix for Pratt) handles `identifier(...)`
func (p *Parser) parseProcCallExpression(function Expression) Expression {
	// 'function' is the expression parsed just before the '('. It should be an Identifier.
	ident, ok := function.(*Identifier)
	if !ok {
		p.addError(p.curTok, "Expected identifier before '(' for procedure call, got %T", function)
		p.skipParentheses() // Attempt recovery
		return nil
	}

	procName := ident.Value
	callToken := p.curTok // Store '(' token

	// Check if identifier is declared and is a procedure
	// TODO: Scope lookup
	symbolInfo, exists := p.getSymbolInfo(procName)
	if !exists {
		p.addSemanticError(ident.Token, "Call to undefined procedure '%s'", procName)
		p.skipParentheses() // Attempt recovery
		return nil
	}
	if symbolInfo.Type != "proc" {
		p.addSemanticError(ident.Token, "'%s' is not a procedure, it's a %s", procName, symbolInfo.Type)
		p.skipParentheses() // Attempt recovery
		return nil
	}

	expr := &ProcCallExpression{
		Token:    callToken, // Use '(' as the call token? Or ident? Let's use ident.
		Function: ident,     // Use the identifier node parsed as the left expression
		// Set resolved return type/width from symbol table info
		ResolvedReturnType:  symbolInfo.ReturnType,
		ResolvedReturnWidth: symbolInfo.ReturnWidth,
	}
	// Function identifier already consumed by prefix parser. Current token is '('.

	// Parse Arguments using helper
	var err error
	// parseExpressionList expects curTok *after* '(', so advance first
	p.nextToken()                                            // Consume '('
	expr.Arguments, err = p.parseExpressionList(TokenRParen) // Parses until ')'
	if err != nil {
		return nil // Error handled in parseExpressionList
	}
	// parseExpressionList consumes the final ')'. curTok is now after ')'.

	// --- Semantic Checks: Argument Count & Types ---
	if len(expr.Arguments) != len(symbolInfo.ParamNames) {
		// Use the identifier token for the error location
		p.addSemanticError(ident.Token, "Procedure '%s' expects %d arguments, but got %d", procName, len(symbolInfo.ParamNames), len(expr.Arguments))
		// Continue type checking if possible? Only if counts match.
	} else {
		// Check types and widths
		for i, argExpr := range expr.Arguments {
			argType := argExpr.ResultType()
			argWidth := argExpr.ResultWidth()
			expectedType := symbolInfo.ParamTypes[i]
			expectedWidth := symbolInfo.ParamWidths[i] // Mandatory width from proc decl

			argToken := argExpr.GetToken() // Get token for error reporting

			// Type Check
			if argType == "void" {
				p.addSemanticError(argToken, "Cannot pass result of void procedure call as argument %d to '%s'", i+1, procName)
			} else if argType != "unknown" && argType != expectedType {
				p.addSemanticError(argToken, "Type mismatch for argument %d of '%s'. Expected '%s', got '%s'", i+1, procName, expectedType, argType)
			}

			// Width Check (Warning/Error) - Only if types match and widths are known positive
			if argType != "unknown" && argType != "void" && argType == expectedType && argWidth > 0 && expectedWidth > 0 && argWidth > expectedWidth {
				// Is argument a literal? If so, it's likely an error.
				isLiteralArg := false
				if _, ok := argExpr.(*IntegerLiteral); ok {
					isLiteralArg = true
				}
				if _, ok := argExpr.(*StringLiteral); ok {
					isLiteralArg = true
				}

				if isLiteralArg {
					p.addSemanticError(argToken, "Argument %d width %d exceeds parameter width %d for '%s'", i+1, argWidth, expectedWidth, procName)
				} else {
					p.addWarning(argToken, "Width of argument %d (%d) might exceed parameter width (%d) for '%s'. Potential truncation.", i+1, argWidth, expectedWidth, procName)
				}
			} else if argType != "unknown" && argType != "void" && argType == expectedType && expectedWidth <= 0 {
				// This indicates an issue with param declaration parsing in proc definition
				p.addWarning(ident.Token, "Could not verify width for argument %d of '%s'; parameter declared width is invalid (%d).", i+1, procName, expectedWidth)
			}
		}
	}

	// curTok is now the token *after* the closing ')' of the call
	return expr
}

// --- Helper Functions ---

// tryConstantFolding attempts to fold binary operations on literals.
// Returns the folded literal expression (IntegerLiteral or StringLiteral) or nil if folding is not possible.
func (p *Parser) tryConstantFolding(left, right Expression, opToken Token) Expression {
	leftLitInt, leftIsInt := left.(*IntegerLiteral)
	rightLitInt, rightIsInt := right.(*IntegerLiteral)
	leftLitStr, leftIsStr := left.(*StringLiteral)
	rightLitStr, rightIsStr := right.(*StringLiteral)
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
			return &IntegerLiteral{
				Token: opToken, // Use operator token for location info of the folded result
				Value: resultVal,
				Width: lib.CalculateWidthForValue(resultVal), // Calculate exact width
			}
		}
	}

	// String Folding (only for '+')
	if leftIsStr && rightIsStr && operator == "+" {
		resultVal := leftLitStr.Value + rightLitStr.Value
		return &StringLiteral{
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

	for openCount > 0 && p.curTok.Type != TokenEOF {
		switch p.curTok.Type {
		case TokenLParen:
			openCount++
		case TokenRParen:
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

// skipUntil advances tokens until a specific token type is found or EOF. Consumes the target token.
func (p *Parser) skipUntil(target TokenType) {
	for p.curTok.Type != target && p.curTok.Type != TokenEOF {
		p.nextToken()
	}
	// Consume the target token if found
	if p.curTok.Type == target {
		p.nextToken()
	}
}

// parseExpressionList parses a comma-separated list of expressions until endToken is found.
// Expects curTok to be the token *after* the opening delimiter (e.g., after '(').
// Consumes tokens up to and including the endToken. Used for arguments.
func (p *Parser) parseExpressionList(endToken TokenType) ([]Expression, error) {
	list := []Expression{}

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
	for p.curTok.Type == TokenComma {
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
func (p *Parser) skipUntilCommaOrEnd(endToken TokenType) {
	for p.curTok.Type != TokenComma && p.curTok.Type != endToken && p.curTok.Type != TokenEOF {
		// TODO: Need smarter skipping for nested structures if expressions get complex
		p.nextToken()
	}
}

// --- Utility Functions ---

// expectPeek checks if the next token is of the expected type. If so, it consumes
// the current token (advances p.curTok) and returns true. Otherwise, adds an error
// and returns false.
func (p *Parser) expectPeek(expectedType TokenType) bool {
	if p.peekTok.Type == expectedType {
		p.nextToken() // Consume current, advance peek to current
		return true
	}
	p.peekError(expectedType)
	return false
}

// peekError adds an error message indicating the expected vs actual peek token.
func (p *Parser) peekError(expectedType TokenType) {
	msg := fmt.Sprintf("Expected next token to be %s, got %s ('%s') instead",
		expectedType, p.peekTok.Type, p.peekTok.Literal)
	// Report error at the location of the *peek* token, as that's where the expectation failed
	p.addError(p.peekTok, msg)
}
