package scope

import (
	"fmt"

	"github.com/arnavsurve/grace/internal/compiler/symbols"
)

// --- Scope ---
type Scope struct {
	Symbols map[string]*symbols.SymbolInfo
	Outer   *Scope
	Name    string
}

func NewScope(outer *Scope, name string) *Scope {
	return &Scope{
		Symbols: make(map[string]*symbols.SymbolInfo),
		Outer:   outer,
		Name:    name,
	}
}

// Define adds a symbol ONLY to the current scope level.
// It returns an error if the symbol already exists at this level.
func (s *Scope) Define(name string, info symbols.SymbolInfo) error {
	if _, exists := s.Symbols[name]; exists {
		// TODO: Distinguish between redeclaration in the same scope vs shadowing
		// For now, simple error for redeclaration in the *same* scope.
		return fmt.Errorf("symbol '%s' already declared in this scope", name)
	}
	infoToStore := info
	s.Symbols[name] = &infoToStore
	return nil
}

// Lookup searches for a symbol starting from the current scope and traversing outwards.
func (s *Scope) Lookup(name string) (*symbols.SymbolInfo, bool) {
	for scope := s; scope != nil; scope = scope.Outer {
		if infoPtr, ok := scope.Symbols[name]; ok {
			return infoPtr, true
		}
	}
	return nil, false
}

// LookupCurrentScope checks ONLY the current scope level.
func (s *Scope) LookupCurrentScope(name string) (*symbols.SymbolInfo, bool) {
	infoPtr, ok := s.Symbols[name]
	if ok {
		return infoPtr, true
	}
	return nil, false
}
