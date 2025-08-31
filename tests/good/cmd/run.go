package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
)

// run: submit job to emulator/daemon
var RunCmd = &cobra.Command{
	Use:   "run [options] <job-dir>",
	Short: "Submit a job to Hercules/Hopper",
	Args:  cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		jobDir := args[0]
		fmt.Printf("↪ running job in %q ...\n", jobDir)
		// TODO: package COBOL, send to Hopper, watch for completion
	},
}
