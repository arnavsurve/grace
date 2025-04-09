package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/arnavsurve/loveugrace/internal/compiler"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: grace <filename>")
		os.Exit(1)
	}

	filename := os.Args[1]

	// Validate extension
	if filepath.Ext(filename) != ".grace" {
		fmt.Println("Error: File must have a .grace extension")
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

	fmt.Println("AST:")
	for _, stmt := range program.Statements {
		fmt.Printf("%#v\n", stmt)
	}
	fmt.Println()

	emitter := compiler.NewEmitter()
	cobolOutput := emitter.Emit(program)

	fmt.Println("Generated COBOL:")
	fmt.Println(cobolOutput)

	err = os.WriteFile("out/hello.cob", []byte(cobolOutput), 0644)
	if err != nil {
		fmt.Printf("Failed to write COBOL output: %v\n", err)
	}
}
