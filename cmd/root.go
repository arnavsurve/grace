package cmd

import (
	"github.com/spf13/cobra"
)

var outDir string

var rootCmd = &cobra.Command{
	Use:   "grace",
	Short: "Grace CLI — compiler, job runner, and logger",
	Long: `Grace is the compiler and job manager for your COBOL workflows.

Commands:
  init   Scaffold a new Grace job 
  build  Compile a (.grc) Grace source file into (.cbl) COBOL
  run    Submit a job to Hercules/Hopper
  logs   Tail output logs for a Grace job
`,
}

func Execute() error {
	if err := rootCmd.Execute(); err != nil {
		return err
	}
	return nil
}

func init() {
	rootCmd.PersistentFlags().StringVarP(&outDir, "out", "o", "out", "output directory for build artifacts")

	rootCmd.AddCommand(InitCmd, BuildCmd, RunCmd, LogsCmd)
}
