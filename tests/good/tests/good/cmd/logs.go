package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
)

// logs: tail output from a running job
var LogsCmd = &cobra.Command{
	Use:   "logs <job-id>",
	Short: "Tail output logs for a Grace job",
	Args:  cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		jobID := args[0]
		fmt.Printf("↪ streaming logs for job %q …\n", jobID)
		// TODO: connect to Hopper, tail stdout/stderr
	},
}
