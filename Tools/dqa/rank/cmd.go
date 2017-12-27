package rank

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"

	"../results"
	"../rules"
	dms "github.com/chop-dbhi/data-models-service/client"
	"github.com/fatih/color"
	"github.com/olekukonko/tablewriter"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var Cmd = &cobra.Command{
	Use: "assign-rank-to-issues <path>",

	Short: "Assigns ranks to detected issues in DQA analysis results.",

	Example: `
  pedsnet-dqa assign-rank-to-issues SecondaryReports/CHOP/ETLv4`,

	Run: func(cmd *cobra.Command, args []string) {
		if len(args) != 1 {
			cmd.Usage()
			return
		}

		dryRun := viper.GetBool("rankissues.dryrun")
		token := viper.GetString("rankissues.token")
		url := viper.GetString("rankissues.url")

		if token == "" {
			cmd.Printf("Token required. Use the --token option.")
			os.Exit(1)
		}

		// Read secondary reports from directory.
		files, err := results.ReadFromDir(args[0])
		if err != nil {
			cmd.Println(err)
			os.Exit(1)
		}

		// Get the data model name and version to validate against.
		// TODO: this assumes all files being ranked are using the
		// same data model.
		var modelName, modelVersion string
		for _, f := range files {
			for _, r := range f.Results {
				modelName = r.Model
				modelVersion = r.ModelVersion
				break
			}
		}

		cmd.Printf("Ranking against model '%s/%s'\n", modelName, modelVersion)

		// Fetch the model for validating the rules.
		// TODO: this is a different workflow?
		client, err := dms.New(url)
		if err != nil {
			cmd.Printf("Could not connect to service %s: %s\n", url, err)
			os.Exit(1)
		}

		model, err := client.ModelRevision(modelName, modelVersion)
		if err != nil {
			cmd.Printf("Error fetching model: %s\n", err)
			os.Exit(1)
		}

		rules, err := rules.Fetch(token, model)
		if err != nil {
			cmd.Println("There was a problem with the rules.")
			cmd.Println(err)
			os.Exit(1)
		}

		bold := color.New(color.Bold, color.FgGreen).SprintFunc()

		var matches rankMatches

		for name, file := range files {
			fileChanged := false

			for _, r := range file.Results {
				changedText := "No"
				persistentText := "No"

				if r.IsPersistent() {
					persistentText = "Yes"
				}

				if rule, ok := rules.Run(r); ok {
					oldRankText := r.Rank.String()
					newRankText := rule.Rank.String()

					if r.Rank != rule.Rank {
						changedText = bold("Yes")
						r.Rank = rule.Rank
						fileChanged = true
					}

					matches = append(matches, []string{
						rule.Type,
						r.Table,
						r.Field,
						r.Goal,
						r.CheckCode,
						r.Prevalence,
						newRankText,
						oldRankText,
						changedText,
						persistentText,
					})
				}
			}

			if fileChanged && !dryRun {
				path := filepath.Join(args[0], name)

				// Open the save file for writing.
				f, err := os.Create(path)
				if err != nil {
					cmd.Printf("Error opening file: %s", err)
					os.Exit(1)
				}
				defer f.Close()

				rw := results.NewWriter(f)

				if err := rw.WriteAll(file.Results); err != nil {
					cmd.Printf("Error writing to file: %s", err)
					os.Exit(1)
				}

				if err := rw.Flush(); err != nil {
					cmd.Printf("error flushing file: %s", err)
					os.Exit(1)
				}
			}
		}

		outputSummary(os.Stdout, matches)
	},
}

func outputSummary(w io.Writer, matches rankMatches) {
	// If there are matches, print them out in a table.
	if len(matches) > 0 {
		tw := tablewriter.NewWriter(w)

		tw.SetHeader([]string{
			"type",
			"table",
			"field",
			"goal",
			"check code",
			"prevalence",
			"new rank",
			"old rank",
			"changed",
			"persistent",
		})

		sort.Sort(matches)

		tw.AppendBulk([][]string(matches))

		tw.Render()
	} else {
		fmt.Fprintln(w, "All ranks are up-to-date.")
	}
}

type rankMatches [][]string

func (r rankMatches) Len() int {
	return len(r)
}

func (r rankMatches) Less(i, j int) bool {
	a := r[i]
	b := r[j]

	// Type
	if a[0] < b[0] {
		return true
	} else if a[0] > b[0] {
		return false
	}

	// Table
	if a[1] < b[1] {
		return true
	} else if a[1] > b[1] {
		return false
	}

	// Field
	return a[2] < b[2]
}

func (r rankMatches) Swap(i, j int) {
	r[i], r[j] = r[j], r[i]
}

func init() {
	flags := Cmd.Flags()

	flags.Bool("dryrun", false, "Outputs a summary of what rank matches without saving the files.")
	flags.String("token", "", "GitHub token to fetch the rules.")
	flags.String("url", dms.DefaultServiceURL, "Data models service URL.")

	viper.BindPFlag("rankissues.dryrun", flags.Lookup("dryrun"))
	viper.BindPFlag("rankissues.token", flags.Lookup("token"))
	viper.BindPFlag("rankissues.url", flags.Lookup("url"))
}
