package query

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"

	"github.com/PEDSnet/tools/cmd/dqa/results"
	"github.com/spf13/cobra"
)

var Cmd = &cobra.Command{
	Use: "query ( - | <sql> ) <path>...",

	Short: "Executes a SQL query against one or more sets of results.",

	Example: `
Inline:

  $ pedsnet-dqa query "select * from results limit 10" SecondaryReports/CHOP/ETLv4

Use - to read from stdin:

  $ pedsnet-dqa query - ./ETLv1 ./ETLv2 ./ETLv3 ./ETLv4
  select data_version, "table", field check_code, rank, site_response
  from results
  where status = 'persistent'
  order by data_version, "table", field
  ^D

Read from a file:

  $ pedsnet-dqa query - ./ETLv1 ./ETLv2 ./ETLv3 ./ETLv4 < query.sql
`,

	Run: func(cmd *cobra.Command, args []string) {
		if len(args) < 2 {
			cmd.Usage()
			return
		}

		stmt := args[0]

		// Read the SQL from stdin
		if stmt == "-" {
			printHeader(cmd.OutOrStdout())

			b, err := ioutil.ReadAll(os.Stdin)

			if err != nil {
				fmt.Println(err)
				os.Exit(1)
			}

			stmt = string(b)
		}

		db, err := Open()
		if err != nil {
			cmd.Printf("Error initializing database: %s\n", err)
			os.Exit(1)
		}

		for _, dir := range args[1:] {
			reports, err := results.ReadFromDir(dir)
			if err != nil {
				cmd.Printf("Error reading results from directory: %s\n", err)
				os.Exit(1)
			}

			for _, r := range reports {
				if err = db.Load(r.Header(), r.Results); err != nil {
					cmd.Printf("Error loading results into the database: %s\n", err)
					os.Exit(1)
				}
			}
		}

		w := NewPrettyWriter(os.Stdout)

		err = db.Query(w, stmt)
		if err != nil {
			cmd.Printf("Query error: %s\n", err)
			os.Exit(1)
		}
	},
}

func printHeader(w io.Writer) {
	cols := make([]string, len(columnNames))

	for i, c := range columnNames {
		cols[i] = fmt.Sprintf(`"%s"`, c)
	}

	fmt.Fprintf(w, "The table is called `%s`\n", TableName)
	fmt.Fprintf(w, "The available columns are: %s\n", strings.Join(cols, ", "))
	fmt.Fprintln(w, "---\n")
}
