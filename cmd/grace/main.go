package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/arnavsurve/grace/internal/compiler"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: grace <filename>")
		os.Exit(1)
	}

	filename := os.Args[1]
	baseName := filepath.Base(filename)
	nameWithoutExt := strings.TrimSuffix(baseName, filepath.Ext(baseName))

	// Validate extension
	if filepath.Ext(filename) != ".grc" {
		fmt.Println("Error: File must have a .grc extension")
		os.Exit(1)
	}

	// Read file
	content, err := os.ReadFile(filename)
	if err != nil {
		fmt.Printf("Error reading file: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Source:")
	fmt.Println(string(content))

	lexer := compiler.NewLexer(string(content))
	parser := compiler.NewParser(lexer)
	program := parser.ParseProgram()

	// Check for parser errors
	errors := parser.Errors()
	if len(errors) > 0 {
		fmt.Println("Parser errors:")
		for _, err := range errors {
			fmt.Printf("\t%s\n", err)
		}
		os.Exit(1)
	}

	fmt.Println("AST:")
	for _, stmt := range program.Statements {
		compiler.PrintAST(stmt, "")
	}
	fmt.Println()

	emitter := compiler.NewEmitter()
	cobolOutput := emitter.Emit(program, nameWithoutExt)

	// Check for emitter errors
	emitterErrors := emitter.Errors()
	if len(emitterErrors) > 0 {
		fmt.Println("Emitter errors:")
		for _, err := range emitterErrors {
			fmt.Printf("\t%s\n", err)
		}
		os.Exit(1)
	}

	fmt.Println("Generated COBOL:")
	fmt.Println(cobolOutput)

	outDir := "out"

	if err := os.MkdirAll(outDir, 0755); err != nil {
		fmt.Printf("Failed to create output directory %s: %v\n", outDir, err)
		os.Exit(1)
	}

	outfileName := filepath.Join(outDir, nameWithoutExt+".cbl")

	err = os.WriteFile(outfileName, []byte(cobolOutput), 0644)
	if err != nil {
		fmt.Printf("Failed to write COBOL output to %s: %v\n", outfileName, err)
	} else {
		fmt.Printf("Successfully wrote COBOL output to %s\n", outfileName)
	}
}
