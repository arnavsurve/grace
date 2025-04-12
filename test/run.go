package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
)

const graceCmd = "go run ./cmd/grace"

func main() {
	fmt.Println("🔍 Running good tests:")
	runGoodTests("tests/good")

	fmt.Println("\n💥 Running bad tests:")
	runBadTests("tests/bad")
}

func runGoodTests(dir string) {
	var wg sync.WaitGroup
	files, _ := filepath.Glob(filepath.Join(dir, "*.grc"))

	passed := 0
	failed := 0

	for _, file := range files {
		wg.Add(1)

		go func(file string) {
			defer wg.Done()

			var buf bytes.Buffer
			name := filepath.Base(file)
			buf.WriteString(fmt.Sprintf("→ %s... ", name))

			// Run compiler
			cmd := exec.Command("sh", "-c", fmt.Sprintf("%s %s", graceCmd, file))
			output, err := cmd.CombinedOutput()
			if err != nil {
				buf.WriteString(fmt.Sprintf("❌ Compile failed: %v\n%s", err, string(output)))
				fmt.Print(buf.String())
				failed++
				return
			}

			nameWithoutExt := strings.TrimSuffix(name, filepath.Ext(name))

			expectedPath := filepath.Join(dir, "expected", nameWithoutExt+".cbl")
			expected, err := os.ReadFile(expectedPath)
			if err != nil {
				buf.WriteString(fmt.Sprintf("❌ Missing expected output: %s\n", expectedPath))
				fmt.Print(buf.String())
				failed++
				return
			}

			outfilePath := filepath.Join("out", nameWithoutExt+".cbl")
			actual, err := os.ReadFile(outfilePath)
			if err != nil {
				buf.WriteString(fmt.Sprintf("❌ Missing actual output: %s\n", outfilePath))
				fmt.Print(buf.String())
				failed++
				return
			}

			if bytes.Equal(expected, actual) {
				buf.WriteString("✅\n")
				passed++
			} else {
				buf.WriteString("❌ Mismatch\n")
				failed++
			}

			fmt.Print(buf.String())
		}(file)
	}
	wg.Wait()
	fmt.Printf("\nSummary: ✅ %d | ❌ %d\n", passed, failed)
}

func runBadTests(dir string) {
	var wg sync.WaitGroup
	files, _ := filepath.Glob(filepath.Join(dir, "*.grc"))

	passed := 0
	failed := 0

	for _, file := range files {
		wg.Add(1)

		go func(file string) {
			defer wg.Done()

			var buf bytes.Buffer
			name := filepath.Base(file)
			buf.WriteString(fmt.Sprintf("→ %s... ", name))

			cmd := exec.Command("sh", "-c", fmt.Sprintf("%s %s", graceCmd, file))
			output, err := cmd.CombinedOutput()
			if err == nil {
				buf.WriteString(fmt.Sprintf("❌ Expected failure but got success\n"))
				failed++
			} else if bytes.Contains(output, []byte("error")) || bytes.Contains(output, []byte("Error")) {
				buf.WriteString(fmt.Sprintf("✅\n"))
				passed++
			} else {
				buf.WriteString(fmt.Sprintf("⚠️ Failed, but no error message detected\n"))
				failed++
			}

			fmt.Print(buf.String())
		}(file)
	}
	wg.Wait()
	fmt.Printf("\nSummary: ✅ %d | ❌ %d\n", passed, failed)
}
