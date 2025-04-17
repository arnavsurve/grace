package compiler

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/arnavsurve/grace/internal/compiler/ast"
	"github.com/arnavsurve/grace/internal/compiler/emitter"
	"github.com/arnavsurve/grace/internal/compiler/lexer"
	"github.com/arnavsurve/grace/internal/compiler/parser"
)

func CompileAndWrite(srcPath, outDir string) (string, error) {
	if err := validateExtension(srcPath); err != nil {
		return "", err
	}

	content, err := readSource(srcPath)
	if err != nil {
		return "", err
	}

	prog, err := parseProgram(content)
	if err != nil {
		return "", err
	}

	cobol, err := emitCobol(prog, srcPath)
	if err != nil {
		return "", err
	}

	outFile, err := writeOutput(cobol, srcPath, outDir)
	if err != nil {
		return "", err
	}

	return outFile, err
}

func validateExtension(path string) error {
	if filepath.Ext(path) != ".grc" {
		return fmt.Errorf("source must have .grc extension")
	}
	return nil
}

func readSource(path string) (string, error) {
	b, err := os.ReadFile(path)
	return string(b), err
}

func parseProgram(src string) (*ast.Program, error) {
	lex := lexer.NewLexer(src)
	p := parser.NewParser(lex)
	prog := p.ParseProgram()
	if errs := p.Errors(); len(errs) > 0 {
		return nil, fmt.Errorf("parser errors: %v", errs)
	}
	return prog, nil
}

func emitCobol(prog *ast.Program, srcPath string) (string, error) {
	name := strings.TrimSuffix(filepath.Base(srcPath), ".grc")
	em := emitter.NewEmitter()
	cobol := em.Emit(prog, name)
	if errs := em.Errors(); len(errs) > 0 {
		return "", fmt.Errorf("emitter errors: %v", errs)
	}
	return cobol, nil
}

func writeOutput(cobol, srcPath, outDir string) (string, error) {
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", err
	}
	outFile := filepath.Join(outDir, strings.TrimSuffix(filepath.Base(srcPath), ".grc")+".cbl")
	return outFile, os.WriteFile(outFile, []byte(cobol), 0o644)
}
