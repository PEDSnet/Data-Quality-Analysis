package migrate

import (
	"bufio"
	"encoding/csv"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"github.com/PEDSnet/tools/cmd/dqa/uni"
	"github.com/spf13/cobra"
)

var codeMapping = map[string][]string{
	"G1-002": {"CA-014", "CA-015"},
	"G1-003": {"AA-003"},
	"G2-002": {"CA-010"},
	"G2-003": {"CA-003", "CA-004"},
	"G2-004": {"AA-001"},
	"G2-005": {"CA-007"},
	"G2-006": {"AA-002"},
	"G2-007": {"CA-011"},
	"G2-008": {"CA-008", "CA-009"},
	"G2-009": {"CA-001"},
	"G2-010": {"CA-002"},
	"G2-011": {"AA-004"},
	"G2-012": {"CA-012"},
	"G2-013": {"CA-005", "CA-006"},
	"G2-014": {"CA-013"},
	"G3-002": {"CA-012"},
	"G3-003": {"CB-001"},
	"G3-005": {"CA-013"},
	"G4-002": {"BA-001"},
}

var newCodesCatalog = map[string]string{
	"AA-001": "Value set violation",
	"AA-002": "Illegal concept identifier",
	"AA-003": "Inconsistency between PK and source value",
	"AA-004": "Unexpected fact",
	"AA-005": "Incorrect concept id vocabulary",
	"AA-006": "Violate inclusion criteria",
	"BA-001": "Missing Data",
	"BA-002": "No matching concepts",
	"BA-003": "Missing Expected Concept",
	"BA-004": "Insufficient facts for visits",
	"CA-001": "Future event",
	"CA-002": "Past event",
	"CA-003": "Pre-birth fact",
	"CA-004": "Post-death fact",
	"CA-005": "Unexpected change in number of records between data cycles",
	"CA-006": "Unexpected change in missingness of a field between data cycles",
	"CA-007": "Identification of entity outliers",
	"CA-008": "Identifcation of specific dates with high number of facts",
	"CA-009": "Identifcation of sudden change in distribution of facts",
	"CA-010": "Total number of records is too low",
	"CA-011": "Implausible Numerical Values",
	"CA-012": "Unexpected field distribution",
	"CA-013": "Unexpected most frequent values",
	"CA-014": "Inconsistency of null values between source values and concept ids",
	"CA-015": "Inconsistency of most frequent values between source values and concept ids",
	"CB-001": "Unexpected fact to patient ratio",
}

var allNewCodes []string

func init() {
	for code, _ := range newCodesCatalog {
		allNewCodes = append(allNewCodes, code)
	}

	// Sort for better presentation.
	sort.Strings(allNewCodes)
	for _, a := range codeMapping {
		sort.Strings(a)
	}
}

func indexOf(s string, a []string) int {
	for i, x := range a {
		if x == s {
			return i
		}
	}
	return -1
}

func readInput(prompt string) string {
	r := bufio.NewReader(os.Stdin)
	fmt.Print(prompt)
	text, _ := r.ReadString('\n')
	return text
}

var Cmd = &cobra.Command{
	Use: "migrate-codes <dir>...",

	Short: "Migrate issue codes in files.",

	Example: `Migrate issue codes in a secondary report.
  pedsnet-dqa migrate-codes SecondaryReports/CHOP/ETLv5
	
Migrate issue codes in the ranking rules.
  pedsnet-dqa migrate-codes SecondaryReports/Ranking`,

	Run: func(cmd *cobra.Command, args []string) {
		if len(args) == 0 {
			cmd.Help()
			os.Exit(1)
		}

		dirs := args

		for _, dir := range dirs {
			filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
				if !strings.HasSuffix(path, ".csv") {
					return nil
				}

				return migrateCodes(path)
			})
		}
	},
}

func migrateCodes(path string) error {
	f, err := os.Open(path)
	if err != nil {
		log.Println("error opening file")
		return nil
	}

	fmt.Printf("# Migrating '%s'\n", path)
	fmt.Println("#")

	// Read all rows.
	r := csv.NewReader(uni.New(f))
	rows, err := r.ReadAll()
	f.Close()
	if err != nil {
		log.Println("error reading file")
		return nil
	}

	// Ensure the there is an issue code column.
	head := rows[0]
	codepos := -1
	descpos := -1

	for i, col := range head {
		switch strings.Replace(strings.ToLower(col), " ", "_", -1) {
		case "check_code", "issue_code":
			codepos = i
		case "check_type", "issue_description":
			descpos = i
		}
	}

	if codepos == -1 {
		log.Println("no check code column. skipping")
		return nil
	}

	var (
		oldCode string
		oldDesc string
	)

	// Replace codes.
	for _, row := range rows[1:] {
		oldCode = row[codepos]

		if descpos >= 0 {
			oldDesc = row[descpos]
		} else {
			oldDesc = ""
		}

		// Not an issue.
		if oldCode == "" {
			continue
		}

		// Already mapped, ignore.
		if indexOf(oldCode, allNewCodes) >= 0 {
			continue
		}

		// Suggested new codes.
		newCodes := codeMapping[oldCode]
		if newCodes == nil {
			newCodes = allNewCodes
		}

		// 1:1 mapping
		if len(newCodes) == 1 {
			row[codepos] = newCodes[0]
			if descpos >= 0 {
				row[descpos] = newCodesCatalog[newCodes[0]]
			}
			continue
		}

		var (
			newCode string
			newDesc string
		)

		fmt.Printf("# Code '%s' needs to be mapped.\n", oldCode)
		fmt.Println("#")
		fmt.Println("# Row contents:")
		for i, c := range head {
			fmt.Printf("#    %s: %s\n", c, row[i])
		}
		fmt.Println("#")

		// Unknown mapping, prompt.
		fmt.Println("# The choices are:")
		for i, code := range newCodes {
			fmt.Printf("#    %d) %s    %s\n", i, code, newCodesCatalog[code])
		}
		fmt.Println("#")

		fmt.Println("# Enter the number for the code and hit enter/return.")
		fmt.Println("# Alternately manually enter a code to override the choices.")
		fmt.Println("# You can skip a code by typing 's' or 'skip'.")
		fmt.Println("# Finally you can type 'save' to save the current file.")

		// Loop until a valid input is received.
		for {
			newCode = strings.ToUpper(strings.TrimSpace(readInput("> ")))

			if newCode == "SAVE" {
				if err := saveFile(path, rows); err != nil {
					fmt.Println("! There was a problem saving to the file. If the issue is fixable, you can try saving again.")
					fmt.Printf("! error: %s\n", err)
					continue
				}

				fmt.Println("Saved current file!\n")
				return nil
			}

			// Skip it.
			if newCode == "S" || newCode == "SKIP" {
				newCode = oldCode
				newDesc = oldDesc
				break
			}

			// Check for text code.
			if indexOf(newCode, allNewCodes) >= 0 {
				newDesc = newCodesCatalog[newCode]
				break
			}

			// Use number to select choice.
			i, err := strconv.Atoi(newCode)
			if err == nil && i < len(newCodes) {
				newCode = newCodes[i]
				newDesc = newCodesCatalog[newCode]
				break
			}

			fmt.Println("Invalid choice. Try again.")
		}

		row[codepos] = newCode
		if descpos >= 0 {
			row[descpos] = newDesc
		}

		fmt.Println("")
	}

	if err := saveFile(path, rows); err != nil {
		fmt.Println("! Error saving file.")
		fmt.Println("! error: %s", err)
	}

	return nil
}

func saveFile(path string, rows [][]string) error {
	// Open file for writing.
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer f.Close()

	w := csv.NewWriter(f)
	w.WriteAll(rows)
	w.Flush()
	return w.Error()
}
