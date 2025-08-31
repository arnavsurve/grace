package main

import (
	"fmt"
	"os"

	"github.com/arnavsurve/grace/cmd"
)

var outDir string

func main() {
	if err := cmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
