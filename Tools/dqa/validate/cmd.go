package validate

import (
	"os"
	"strings"

	"../results"
	"github.com/spf13/cobra"
)

var Cmd = &cobra.Command{
	Use: "validate <directory>...",

	Short: "Validates files in a Secondary Report.",

	Example: `
  pedsnet-dqa validate SecondaryReports/CHOP/ETLv4`,

	Run: func(cmd *cobra.Command, args []string) {
		if len(args) == 0 {
			cmd.Println("At least one path is required.")
			os.Exit(1)
		}

		var errs map[int][]string

		for _, dir := range args {
			stat, err := os.Stat(dir)
			if err != nil {
				cmd.Printf("Error inspecting file '%s': %s\n", dir, err)
				continue
			}

			// A directory is expected.
			if !stat.IsDir() {
				continue
			}

			cmd.Printf("Inspecting directory '%s'\n", dir)

			files, err := results.ReadFromDir(dir)

			if err != nil {
				cmd.Printf("Error reading files: %s\n", err)
				continue
			}

			if len(files) == 0 {
				cmd.Printf("No files to validate.")
				continue
			}

			cmd.Println("Validating files...")

			hasErrors := false

			for name, file := range files {
				errs = file.Validate()

				if len(errs) > 0 {
					hasErrors = true

					cmd.Printf("* Errors found in '%s':\n", name)

					for line, msgs := range errs {
						cmd.Printf("    Line %d: %s\n", line, strings.Join(msgs, ", "))
					}

					cmd.Println("")
				}
			}

			if !hasErrors {
				cmd.Println("* Everything looks good!")
			}
		}
	},
}
