package symbols

type SymbolInfo struct {
	Type    string // string, int, bool, proc, unknown, undeclared etc
	Width   int
	IsConst bool

	// --- Proc specific info ---
	ParamNames  []string
	ParamTypes  []string
	ParamWidths []int
	ReturnType  string
	ReturnWidth int
}
