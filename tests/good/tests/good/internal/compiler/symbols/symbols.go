package symbols

// FieldInfo describes a field within a record structure
type FieldInfo struct {
	Name   string
	Type   string // int, string
	Width  int
	Offset int
}

// SymbolInfo holds information about a declared identifier (variable, const, proc, record, file)
type SymbolInfo struct {
	Name    string
	Type    string // string, int, bool, proc, unknown, undeclared etc
	IsConst bool

	// --- Variable/Constant Info ---
	Width int // Width for int/string; 0 for proc/file/record type

	// --- Procedure info ---
	ParamNames             []string
	ParamTypes             []string
	ParamWidths            []int
	ReturnType             string
	ReturnWidth            int
	ReturnRecordTypeSymbol *SymbolInfo

	// --- Record Type Info ---
	Fields     []FieldInfo
	TotalWidth int

	// --- File Handle Info ---
	Mode                 string
	SystemFileName       string
	RecordTypeName       string
	FileRecordTypeSymbol *SymbolInfo

	// --- Variable Holding a Record Info ---
	RecordTypeSymbol *SymbolInfo
}
