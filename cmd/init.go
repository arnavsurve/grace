package cmd

import (
	"embed"
	"fmt"
	"os"
	"path/filepath"
	"text/template"

	"github.com/spf13/cobra"
)

//go:embed templates/*
var tplFS embed.FS

// init: scaffold a new job
var InitCmd = &cobra.Command{
	Use:   "init [job-name]",
	Short: "Scaffold a new Grace job",
	Args:  cobra.MaximumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		var (
			targetDir string
			jobName   string
		)

		// Determine where to write files
		// targetDir is where files go, jobName is for templating
		if len(args) == 1 {
			targetDir = args[0]
			jobName = args[0]
		} else {
			targetDir = "."        // scatter in cwd
			cwd, err := os.Getwd() // use base name for templating
			cobra.CheckErr(err)
			jobName = filepath.Base(cwd)
		}

		// If we are making a new subdirectory, ensure it doesn't already exist
		if targetDir != "." {
			if _, err := os.Stat(targetDir); err == nil {
				cobra.CheckErr(fmt.Errorf("directory %q already exists", targetDir))
			}

			err := os.MkdirAll(targetDir, 0755)
			cobra.CheckErr(err)
		}

		fmt.Printf("↪ scaffolding new job %q ...\n", jobName)

		err := os.MkdirAll(filepath.Join(targetDir, "src"), 0755)
		cobra.CheckErr(err)
		err = os.MkdirAll(filepath.Join(targetDir, "out"), 0755)
		cobra.CheckErr(err)

		// Copy each template to destination with template data
		data := map[string]string{"JobName": jobName}

		files := map[string]string{
			"templates/hello.grc.tpl": "src/hello.grc",
			"templates/grace.yml.tpl": "grace.yml",
			"templates/gitignore.tpl": ".gitignore",
		}

		for tplPath, outName := range files {
			outPath := filepath.Join(targetDir, outName)
			writeTpl(tplPath, outPath, data)
		}

		fmt.Printf("✓ job %q initialized!\n", jobName)
	},
}

// writeTpl loads tplName from tplFS, executes it with data, and writes to outPath
func writeTpl(tplName, outPath string, data any) {
	t, err := template.ParseFS(tplFS, tplName)
	cobra.CheckErr(err)

	f, err := os.Create(outPath)
	cobra.CheckErr(err)
	defer f.Close()

	err = t.Execute(f, data)
	cobra.CheckErr(err)
}
