package issues

import (
	"bytes"
	"encoding/csv"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"sort"
	"strings"

	"github.com/PEDSnet/tools/cmd/dqa/results"
	"github.com/PEDSnet/tools/cmd/dqa/uni"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var Cmd = &cobra.Command{
	Use: "merge-issues <reportdir> <logfile>...",

	Short: "Merge issues into a secondary report.",

	Long: ``,

	Example: `Merge issues into a secondary report.
  pedsnet-dqa merge-issues --token=... SecondaryReports/CHOP/ETLv5 person_issue.csv

Multiple log files can be applied:
  pedsnet-dqa merge-issues --token=... SecondaryReports/CHOP/ETLv5 *.csv`,

	Run: func(cmd *cobra.Command, args []string) {
		if len(args) < 2 {
			cmd.Help()
			os.Exit(1)
		}

		dir := args[0]

		// Map of results files by filename.
		// Each filename corresponds to a table name.
		files, err := results.ReadFromDir(dir)
		if err != nil {
			cmd.Printf("Error reading files in '%s'\n", err)
			os.Exit(1)
		}

		token := viper.GetString("issues.token")
		if token == "" {
			cmd.Println("Token required.")
			os.Exit(1)
		}

		// Count of issues merged by file name.
		merged := make(map[string]int)
		appendMerge := make(map[string][]*results.Result)

		var conflicts []*conflict

		// Process all files
		for _, fn := range args[1:] {
			issues, err := readIssues(fn)
			if err != nil {
				log.Printf("error reading issues: %s", err)
				continue
			}

			for _, issue := range issues {
				lookup := fmt.Sprintf("%s.csv", issue.Table)
				report, ok := files[lookup]
				if !ok {
					log.Fatalf("no report file for table: %s", issue.Table)
				}

				var found bool

				// Scan results for a match. If none is found, add it.
				for i, r := range report.Results {
					// Ensure we are comparing the correct result.
					if r.Model != issue.Model || r.ModelVersion != issue.ModelVersion || r.DataVersion != issue.DataVersion || r.Table != issue.Table {
						cmd.Println("comparing different versions")
						os.Exit(1)
					}

					if r.Field == issue.Field && r.CheckCode == issue.CheckCode {
						if r.IsUnresolved() || r.IsPersistent() {
							conflicts = append(conflicts, &conflict{
								Index:     i,
								CheckCode: r.CheckCode,
								Table:     r.Table,
								Field:     r.Field,
								Lookup:    lookup,
								Log:       issue,
								Secondary: r,
							})
						}

						found = true
						break
					}
				}

				// If the issue was not found, append it to the results to be written.
				if !found {
					merged[lookup] += 1
					appendMerge[lookup] = append(appendMerge[lookup], issue)
				}
			}
		}

		// Call Python process to resolve conflicts...
		if len(conflicts) > 0 {
			// Fetch the catalog of issue conflict thresholds.
			catalog, err := GetCatalog(token)
			if err != nil {
				cmd.Print(err)
				os.Exit(1)
			}

			var queued []*conflict

			for _, c := range conflicts {
				checks, ok := catalog[c.CheckCode]
				if !ok {
					cmd.Printf("* Unresolved conflict: %s/%s for issue code %s\n", c.Table, c.Field, c.CheckCode)
					continue
				}

				// Set the thresholds.
				if thres, ok := checks[[2]string{c.Table, c.Field}]; ok {
					c.UpperThreshold = thres.Upper
					c.LowerThreshold = thres.Lower
				}

				queued = append(queued, c)
			}

			if len(queued) > 0 {
				// Map output by position.
				resolvedConflicts, err := runResolve(queued)
				if err != nil {
					cmd.Println(err)
				} else {
					// Lookup of all resolved issues to compare with updated results
					// in the existing files.
					resolvedIndex := make(map[*results.Result]struct{})

					for i, c := range queued {
						resolved := resolvedConflicts[i]

						if resolved.Error != "" {
							cmd.Printf("* Error resolving conflict %s/%s for issue code %s\n", c.Table, c.Field, c.CheckCode)
							cmd.Printf("\n\t%s\n", strings.Replace(resolved.Error, "\n", "\n\t", -1))
							continue
						}

						// No change.
						if len(resolved.Issues) == 0 {
							continue
						}

						// Update the existing issue.
						file := files[c.Lookup]

						// Replace existing issue if it has not already been replaced
						// otherwise append to the set. This can occur if there are two
						// issues record for the same field which would be associated with
						// the same index of the original file.
						if _, ok := resolvedIndex[file.Results[c.Index]]; !ok {
							file.Results[c.Index] = resolved.Issues[0]
							resolvedIndex[resolved.Issues[0]] = struct{}{}
						} else {
							file.Results = append(file.Results, resolved.Issues[0])
						}

						cmd.Printf("* Resolved conflict %s/%s for issue code %s\n", c.Table, c.Field, c.CheckCode)

						// Append new ones and update the merged count.
						if len(resolved.Issues) > 1 {
							file.Results = append(file.Results, resolved.Issues[1:]...)
							cmd.Printf("- Appended %d additional issue(s)\n", len(resolved.Issues)-1)
						}

						merged[c.Lookup] += len(resolved.Issues)
					}
				}
			}
		}

		// Append new issues.
		for lookup, issues := range appendMerge {
			file := files[lookup]
			file.Results = append(file.Results, issues...)
		}

		if len(merged) == 0 {
			cmd.Println("No new issues found.")
			return
		}

		for name, count := range merged {
			file := files[name]
			sort.Sort(file.Results)

			// File opened successfully.
			f, err := os.Create(filepath.Join(dir, name))
			if err != nil {
				cmd.Printf("Error opening file to write new issues: %s\n", err)
				continue
			}
			defer f.Close()
			w := results.NewWriter(f)

			if err := w.WriteAll(file.Results); err != nil {
				cmd.Printf("Error writing results to file.")
				continue
			}

			if err := w.Flush(); err != nil {
				cmd.Printf("Error flushing results to file.")
				continue
			}

			cmd.Printf("Merged %d issues into %s\n", count, name)
		}
	},
}

type resolveResult struct {
	Issues []*results.Result
	Error  string
}

func runResolve(conflicts []*conflict) ([]*resolveResult, error) {
	var stdin bytes.Buffer
	if err := json.NewEncoder(&stdin).Encode(conflicts); err != nil {
		panic(err)
	}

	program := viper.GetString("issues.program")
	var args []string

	resolvers := viper.GetString("issues.resolvers")
	if resolvers != "" {
		args = append(args, fmt.Sprintf("--resolvers=%s", resolvers))
	}

	cmd := exec.Command(program, args...)
	cmd.Stdin = &stdin

	out, err := cmd.Output()
	if err != nil {
		if xerr, ok := err.(*exec.ExitError); ok {
			return nil, fmt.Errorf("Error executing resolve command: %s\n%s\n", xerr, string(xerr.Stderr))
		}

		return nil, fmt.Errorf("Error executing resolve command: %s\n", err)
	}

	var results []*resolveResult
	if err := json.Unmarshal(out, &results); err != nil {
		return nil, fmt.Errorf("Error decoding output from resolve command: %s", err)
	}

	for i, c := range conflicts {
		for _, r := range results[i].Issues {
			r.SetFileVersion(c.Log.FileVersion())
		}
	}

	return results, nil
}

func readIssues(fn string) ([]*results.Result, error) {
	fi, err := os.Stat(fn)
	if err != nil {
		return nil, err
	}

	// Directory. Read files in directory, but not recursively.
	if fi.IsDir() {
		var allissues []*results.Result

		files, err := ioutil.ReadDir(fn)
		if err != nil {
			return nil, fmt.Errorf("error reading directory: %s", err)
		}

		for _, fi := range files {
			// No recursion.
			if fi.IsDir() {
				continue
			}

			p := path.Join(fn, fi.Name())
			issues, err := readIssues(p)
			if err != nil {
				return nil, fmt.Errorf("error reading issues from %s: %s", p, err)
			}

			allissues = append(allissues, issues...)
		}

		return allissues, nil
	}

	f, err := os.Open(fn)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	cr := csv.NewReader(uni.New(f))
	fields, err := cr.Read()

	head, err := checkFields(fields)
	if err != nil {
		return nil, err
	}

	var issues []*results.Result

	for {
		row, err := cr.Read()
		if err != nil {
			if err == io.EOF {
				break
			}

			return nil, err
		}

		res := &results.Result{
			DataVersion: row[head.DataVersion],
			DQAVersion:  "0",
			Table:       row[head.Table],
			Field:       row[head.Field],
			CheckCode:   row[head.CheckCode],
			CheckAlias:  row[head.CheckAlias],
			CheckType:   row[head.CheckType],
			Finding:     row[head.Finding],
			Prevalence:  row[head.Prevalence],
			Status:      "new",
			Method:      "auto",
		}

		res.SetFileVersion(4)

		toks := strings.Split(res.DataVersion, "-")
		res.Model = toks[0]
		res.ModelVersion = toks[1]

		issues = append(issues, res)
	}

	return issues, nil
}

type conflict struct {
	Index          int             `json:"-"`
	Lookup         string          `json:"-"`
	CheckCode      string          `json:"-"`
	Table          string          `json:"-"`
	Field          string          `json:"-"`
	Log            *results.Result `json:"log"`
	Secondary      *results.Result `json:"secondary"`
	LowerThreshold int             `json:"threshold_low"`
	UpperThreshold int             `json:"threshold_high"`
}

type issueFields struct {
	DataVersion int
	Table       int
	Field       int
	CheckCode   int
	CheckAlias  int
	CheckType   int
	Finding     int
	Prevalence  int
}

const numFields = 8

func checkFields(fields []string) (*issueFields, error) {
	var head issueFields
	var seen int

	for i, field := range fields {
		switch field {
		case "data_version", "g_data_version":
			head.DataVersion = i

		case "table":
			head.Table = i

		case "field":
			head.Field = i

		case "check_code", "issue_code":
			head.CheckCode = i

		case "check_type", "issue_description":
			head.CheckType = i

		case "check_alias", "alias":
			head.CheckAlias = i

		case "finding":
			head.Finding = i

		case "prevalence":
			head.Prevalence = i
		}

		seen++
	}

	if seen != numFields {
		return nil, errors.New("missing fields")
	}

	return &head, nil
}

func init() {
	flags := Cmd.Flags()
	flags.String("token", "", "Token used to authenticate with GitHub.")
	flags.String("program", "resolve.py", "Path to resolve program.")
	flags.String("resolvers", "", "Path to resolver modules.")

	viper.BindPFlag("issues.token", flags.Lookup("token"))
	viper.BindPFlag("issues.program", flags.Lookup("program"))
	viper.BindPFlag("issues.resolvers", flags.Lookup("resolvers"))
}
