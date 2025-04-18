package scope

import (
	"fmt"

	"github.com/arnavsurve/grace/internal/compiler/symbols"
)

// --- Scope ---
type Scope struct {
	Symbols map[string]symbols.SymbolInfo
	Outer   *Scope
	Name    string
}

func NewScope(outer *Scope, name string) *Scope {
	return &Scope{
		Symbols: make(map[string]symbols.SymbolInfo),
		Outer:   outer,
		Name:    name,
	}
}

// Define adds a symbol ONLY to the current scope level.
// It returns an error if the symbol already exists at this level.
func (s *Scope) Define(name string, info symbols.SymbolInfo) error {
	if _, exists := s.Symbols[name]; exists {
		// Distinguish between redeclaration in the same scope vs shadowing
		// For now, simple error for redeclaration in the *same* scope.
		return fmt.Errorf("symbol '%s' already declared in this scope", name)
	}
	s.Symbols[name] = info
	fmt.Printf("Defined '%s' in scope %s\n", name, s.Name) // Debugging
	return nil
}

// Lookup searches for a symbol starting from the current scope and traversing outwards.
func (s *Scope) Lookup(name string) (*symbols.SymbolInfo, bool) {
	fmt.Printf("Looking up '%s' starting in scope %s\n", name, s.Name) // Debugging
	for scope := s; scope != nil; scope = scope.Outer {
		fmt.Printf("  Checking scope %s...\n", scope.Name) // Debugging
		if info, ok := scope.Symbols[name]; ok {
			fmt.Printf("  Found '%s' in scope %s\n", name, scope.Name) // Debugging
			// Return a pointer to a copy to prevent modification of the original map entry via the pointer
			infoCopy := info
			return &infoCopy, true
		}
	}
	fmt.Printf("  Did not find '%s'\n", name) // Debugging
	return nil, false
}

// LookupCurrentScope checks ONLY the current scope level.
func (s *Scope) LookupCurrentScope(name string) (*symbols.SymbolInfo, bool) {
	info, ok := s.Symbols[name]
	if ok {
		infoCopy := info
		return &infoCopy, true
	}
	return nil, false
}
