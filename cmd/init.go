package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
)

// init: scaffold a new job
var InitCmd = &cobra.Command{
	Use:   "init [job-name]",
	Short: "Scaffold a new Grace job",
	Args:  cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		jobName := args[0]
		fmt.Printf("↪ scaffolding new job %q ...\n", jobName)
		// TODO: create directory, copy template files, etc
	},
}
