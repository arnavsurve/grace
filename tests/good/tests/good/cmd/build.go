package cmd

import (
	"fmt"

	"github.com/arnavsurve/grace/internal/compiler"
	"github.com/spf13/cobra"
)

// build: combile .grc -> .cbl
var BuildCmd = &cobra.Command{
	Use:   "build <source.grc>",
	Short: "Compile a Grace source file into COBOL",
	Args:  cobra.ExactArgs(1),
	RunE:  buildRun,
}

func buildRun(cmd *cobra.Command, args []string) error {
	src := args[0]
	out, _ := cmd.Flags().GetString("out")

	fmt.Printf("↪ building %q → %q ...\n", src, outDir+"/")

	outFile, err := compiler.CompileAndWrite(src, out)
	if err != nil {
		return err
	}

	fmt.Printf("✔︎ wrote COBOL to %s\n", outFile)
	return nil
}
