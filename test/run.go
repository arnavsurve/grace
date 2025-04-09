package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

const graceCmd = "go run ./cmd/grace"

func main() {
	fmt.Println("🔍 Running good tests:")
	runGoodTests("tests/good")

	fmt.Println("\n💥 Running bad tests:")
	runBadTests("tests/bad")
}

func runGoodTests(dir string) {
	files, _ := filepath.Glob(filepath.Join(dir, "*.grace"))

	for _, file := range files {
		name := filepath.Base(file)
		fmt.Printf("→ %s... ", name)

		// Run compiler
		cmd := exec.Command("sh", "-c", fmt.Sprintf("%s %s", graceCmd, file))
		output, err := cmd.CombinedOutput()
		if err != nil {
			fmt.Println("❌ Compile failed:", err)
			fmt.Println(string(output))
			continue
		}

		// Read expected output
		expectedPath := filepath.Join(dir, "expected", name[:len(name)-6]+".cob")
		expected, err := os.ReadFile(expectedPath)
		if err != nil {
			fmt.Println("❌ Missing expected output:", expectedPath)
			continue
		}

		// Read actual output
		actual, err := os.ReadFile("out/hello.cob")
		if err != nil {
			fmt.Println("❌ Missing actual output")
			continue
		}

		if bytes.Equal(expected, actual) {
			fmt.Println("✅")
		} else {
			fmt.Println("❌ Mismatch")
		}
	}
}

func runBadTests(dir string) {
	files, _ := filepath.Glob(filepath.Join(dir, "*.grace"))

	for _, file := range files {
		name := filepath.Base(file)
		fmt.Printf("→ %s... ", name)

		cmd := exec.Command("sh", "-c", fmt.Sprintf("%s %s", graceCmd, file))
		output, err := cmd.CombinedOutput()
		if err == nil {
			fmt.Println("❌ Expected failure but got success")
			continue
		}

		if bytes.Contains(output, []byte("error")) || bytes.Contains(output, []byte("Error")) {
			fmt.Println("✅")
		} else {
			fmt.Println("⚠️ Failed, but no error message detected")
		}
	}
}
