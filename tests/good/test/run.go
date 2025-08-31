package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	// "sync" // No longer needed for sequential execution
	"time"
)

const (
	graceCmd       = "go run main.go build"
	compileTimeout = 10 * time.Second // Timeout for the grace compiler itself
	cobcTimeout    = 10 * time.Second // Timeout for the COBOL compiler
	runTimeout     = 5 * time.Second  // Timeout for running the compiled binary
)

type testResult struct {
	fileName string
	passed   bool
	output   string // Contains detailed error/mismatch info on failure
	isGood   bool   // True for good tests, false for bad tests
}

func main() {
	// No channel or WaitGroup needed for sequential execution
	// resultsChan := make(chan testResult)
	// var wg sync.WaitGroup

	fmt.Println("🧹 Cleaning output directory...")
	_ = os.RemoveAll("out") // Ignore error if dir doesn't exist
	_ = os.Mkdir("out", 0755)

	// --- Run Good Tests Sequentially ---
	fmt.Println("\n🔍 Running good tests:")
	goodFiles, _ := filepath.Glob(filepath.Join("tests/good", "*.grc"))
	fmt.Printf("Found %d good test files...\n", len(goodFiles))

	goodPassed, goodFailed := 0, 0
	badPassed, badFailed := 0, 0 // Keep separate counters
	failedTests := []testResult{}

	for _, file := range goodFiles {
		fmt.Printf("→ Running good test: %s\n", filepath.Base(file)) // More verbose progress
		res := runGoodTestSequential(file)                           // Call sequential version
		if res.passed {
			fmt.Printf("  ✅ %s\n", res.fileName)
			goodPassed++
		} else {
			fmt.Printf("  ❌ %s\n", res.fileName)
			goodFailed++
			failedTests = append(failedTests, res)
		}
	}

	// --- Run Bad Tests Sequentially ---
	fmt.Println("\n💥 Running bad tests:")
	badFiles, _ := filepath.Glob(filepath.Join("tests/bad", "*.grc"))
	fmt.Printf("Found %d bad test files...\n", len(badFiles))

	for _, file := range badFiles {
		fmt.Printf("→ Running bad test: %s\n", filepath.Base(file)) // More verbose progress
		res := runBadTestSequential(file)                           // Call sequential version
		if res.passed {
			fmt.Printf("  ✅ %s (Failed as expected)\n", res.fileName)
			badPassed++
		} else {
			fmt.Printf("  ❌ %s (Unexpected Result)\n", res.fileName)
			badFailed++
			failedTests = append(failedTests, res)
		}
	}

	// Goroutine for WaitGroup/close removed

	// Result collection loop removed - results processed inline

	// --- Reporting ---
	// Print detailed failures
	if len(failedTests) > 0 {
		fmt.Println("\n--- Detailed Failures ---")
		for _, failure := range failedTests {
			fmt.Printf("\n❌ Test: %s (%s)\n", failure.fileName, map[bool]string{true: "Good Test", false: "Bad Test"}[failure.isGood])
			fmt.Println("Reason:")
			fmt.Println(failure.output) // Output contains the failure reason
			fmt.Println("---")
		}
	}

	// Print Summary
	fmt.Println("\n--------------------")
	fmt.Printf("Good Tests Summary: ✅ Passed: %d | ❌ Failed: %d\n", goodPassed, goodFailed)
	fmt.Printf("Bad Tests Summary:  ✅ Passed: %d | ❌ Failed: %d\n", badPassed, badFailed) // Passed = Failed as expected
	fmt.Println("--------------------")

	if goodFailed > 0 || badFailed > 0 {
		fmt.Println("\n🚨 Some tests failed!")
		os.Exit(1) // Indicate failure
	} else {
		fmt.Println("\n🎉 All tests passed!")
	}
}

// runGoodTestSequential runs a single 'good' test sequentially and returns the result.
func runGoodTestSequential(file string) testResult {
	// No WaitGroup or channel needed
	var buf bytes.Buffer // For capturing reasons for failure
	fileName := filepath.Base(file)
	nameWithoutExt := strings.TrimSuffix(fileName, filepath.Ext(fileName))
	res := testResult{fileName: fileName, isGood: true, passed: false} // Default to failed

	// --- 1. Compile Grace -> COBOL ---
	cmdCompile := exec.Command("sh", "-c", fmt.Sprintf("%s %s", graceCmd, file))
	compileOutputBytes, compileErr := runCommandWithTimeout(cmdCompile, compileTimeout)
	compileOutput := string(compileOutputBytes)

	if compileErr != nil {
		buf.WriteString(fmt.Sprintf("Grace Compile failed: %v\nOutput:\n%s", compileErr, compileOutput))
		res.output = buf.String()
		return res // Return failure result
	}
	// Check for UNEXPECTED "Error:", "error:", "Syntax Error:", "Semantic Error:" in Grace output
	// Allow "Warning:" and "Semantic Warning:"
	if strings.Contains(compileOutput, "Syntax Error:") || strings.Contains(compileOutput, "Semantic Error:") ||
		(strings.Contains(compileOutput, "Error:") && !strings.Contains(compileOutput, "Semantic Error:")) ||
		(strings.Contains(compileOutput, "error:") && !strings.Contains(compileOutput, "Semantic Error:")) { // Basic check, might need refinement
		buf.WriteString(fmt.Sprintf("Grace Compile produced unexpected fatal errors:\nOutput:\n%s", compileOutput))
		res.output = buf.String()
		return res // Return failure result
	}

	// --- 2. Compare generated COBOL with expected ---
	expectedPath := filepath.Join("tests/good/expected", nameWithoutExt+".cbl")
	expectedCobolBytes, err := os.ReadFile(expectedPath)
	if err != nil {
		buf.WriteString(fmt.Sprintf("Missing expected COBOL output: %s", expectedPath))
		res.output = buf.String()
		return res // Return failure result
	}

	outfilePath := filepath.Join("out", nameWithoutExt+".cbl")
	actualCobolBytes, err := os.ReadFile(outfilePath)
	if err != nil {
		buf.WriteString(fmt.Sprintf("Missing actual generated COBOL: %s\nCompiler Output:\n%s", outfilePath, compileOutput))
		res.output = buf.String()
		return res // Return failure result
	}

	// Normalize line endings before comparison
	expectedCobolNorm := bytes.ReplaceAll(expectedCobolBytes, []byte("\r\n"), []byte("\n"))
	actualCobolNorm := bytes.ReplaceAll(actualCobolBytes, []byte("\r\n"), []byte("\n"))

	if !bytes.Equal(expectedCobolNorm, actualCobolNorm) {
		buf.WriteString(fmt.Sprintf("COBOL Mismatch\nExpected (%s):\n%s\nActual (%s):\n%s", expectedPath, string(expectedCobolNorm), outfilePath, string(actualCobolNorm)))
		res.output = buf.String()
		return res // Return failure result
	}

	// --- 3. Compile COBOL -> Binary ---
	cobolFile := outfilePath
	binFile := filepath.Join("out", nameWithoutExt) // No extension needed for executable usually
	_ = os.Remove(binFile)                          // Remove previous binary if exists

	cmdCobc := exec.Command("cobc", "-x", "-o", binFile, cobolFile)
	cobcOutputBytes, cobcErr := runCommandWithTimeout(cmdCobc, cobcTimeout)
	if cobcErr != nil {
		buf.WriteString(fmt.Sprintf("COBOL Compile failed for %s: %v\nOutput:\n%s", cobolFile, cobcErr, string(cobcOutputBytes)))
		res.output = buf.String()
		return res // Return failure result
	}

	// --- 4. Run Binary (Optional) ---
	// ... (Add runtime checks here if needed) ...

	// If all steps pass
	res.passed = true
	return res // Return success result
}

// runBadTestSequential runs a single 'bad' test sequentially and returns the result.
func runBadTestSequential(file string) testResult {
	// No WaitGroup or channel needed
	fileName := filepath.Base(file)
	res := testResult{fileName: fileName, isGood: false, passed: false} // Default to failed (unexpected success/wrong error)

	cmd := exec.Command("sh", "-c", fmt.Sprintf("%s %s", graceCmd, file))
	outputBytes, err := runCommandWithTimeout(cmd, compileTimeout) // Use timeout
	output := string(outputBytes)

	// Expected outcome: command fails (err != nil) AND output contains specific error patterns
	// Focus on Syntax/Semantic errors first, then general Error/error.
	expectedErrorPatterns := []string{"Syntax Error:", "Semantic Error:", "Error:", "error:"}
	hasExpectedErrorMsg := false
	for _, pattern := range expectedErrorPatterns {
		if strings.Contains(output, pattern) {
			hasExpectedErrorMsg = true
			break
		}
	}

	if err != nil && hasExpectedErrorMsg {
		// Passed: Failed as expected with a recognized error message pattern
		res.passed = true
	} else if err != nil && !hasExpectedErrorMsg {
		// Failed: Command failed, but the output didn't contain expected error messages.
		res.output = fmt.Sprintf("Failed, but no expected error message pattern detected.\nExit Err: %v\nOutput:\n%s", err, output)
	} else { // err == nil
		// Failed: Command succeeded unexpectedly.
		res.output = fmt.Sprintf("Expected failure but got success.\nOutput:\n%s", output)
	}
	return res // Return result
}

// runCommandWithTimeout remains the same as before
func runCommandWithTimeout(cmd *exec.Cmd, timeout time.Duration) ([]byte, error) {
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out // Capture both stdout and stderr

	err := cmd.Start()
	if err != nil {
		return out.Bytes(), fmt.Errorf("failed to start command '%s': %w", cmd.String(), err)
	}

	done := make(chan error, 1)
	go func() {
		done <- cmd.Wait()
	}()

	select {
	case <-time.After(timeout):
		// Timeout occurred
		if killErr := cmd.Process.Kill(); killErr != nil {
			return out.Bytes(), fmt.Errorf("command '%s' timed out after %v and failed to kill: %w", cmd.String(), timeout, killErr)
		}
		return out.Bytes(), fmt.Errorf("command '%s' timed out after %v", cmd.String(), timeout)
	case err := <-done:
		// Command finished
		return out.Bytes(), err
	}
}
