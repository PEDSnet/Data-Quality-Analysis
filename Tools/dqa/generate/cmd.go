package generate

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/PEDSnet/tools/cmd/dqa/results"
	dms "github.com/chop-dbhi/data-models-service/client"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var Cmd = &cobra.Command{
	Use: "generate-templates <site> <cycle>",

	Short: "Generates a Secondary Report template for a site and data cycle.",

	Long: `The Secondary Report is a set of files that contain placeholders for
putting results of a data quality assessment. Reports will vary over time based
the data cycle, data model version, and ranking rules that used during the
assessment.

The typical process is to generate a new template per site and derive subsequent
reports from the previous. This can be done using the --copy-persistent
flag which ensures all persistent issues are copied to the new template.
`,

	Example: `Generate a new Secondary Report template:
  pedsnet-dqa generate-templates --root=SecondaryReports/CHOP/ETLv5 CHOP ETLv5`,

	Run: func(cmd *cobra.Command, args []string) {
		if len(args) < 2 {
			cmd.Usage()
			return
		}

		// Positional.
		siteName := args[0]
		dataCycle := args[1]

		// Options.
		modelName := viper.GetString("generate.model")
		modelVersion := viper.GetString("generate.version")
		dqaVersion := viper.GetString("generate.dqa-version")
		outDir := viper.GetString("generate.root")
		serviceUrl := viper.GetString("generate.url")
		copyPersistent := viper.GetString("generate.copy-persistent")

		if modelVersion == "" {
			cmd.Println("Model version required. Specify using the --version option.")
			os.Exit(1)
		}

		// Derived value.
		dataVersion := fmt.Sprintf("%s-%s-%s-%s", modelName, modelVersion, siteName, dataCycle)

		var pfiles map[string]*results.File

		// Load the previous set of results.
		if copyPersistent != "" {
			var err error
			pfiles, err = results.ReadFromDir(copyPersistent)

			if err != nil {
				cmd.Println(err)
				os.Exit(1)
			}
		}

		// Create the necessary directories to write the files to.
		if err := os.MkdirAll(outDir, os.ModeDir|0775); err != nil {
			cmd.Printf("Error creating output directory '%s': %s", outDir, err)
			os.Exit(1)
		}

		client, err := dms.New(serviceUrl)
		if err != nil {
			cmd.Printf("Bad service URL: %s", err)
			os.Exit(1)
		}

		model, err := client.ModelRevision(modelName, modelVersion)
		if err != nil {
			cmd.Printf("Error fetching model revision '%s/%s': %s", modelName, modelVersion, err)
		}

		// Copied issue count.
		count := 0

		// Create a file per table.
		for _, table := range model.Tables.List() {
			// Ignore certain tables from the template file.
			if _, ok := results.ExcludedTables[table.Name]; ok {
				continue
			}

			// Path to output file.
			path := filepath.Join(outDir, fmt.Sprintf("%s.csv", table.Name))

			file, err := os.Create(path)
			if err != nil {
				cmd.Printf("Error creating output file: %s", err)
				os.Exit(1)
			}

			// New writer for issues. At a minimum this will add the header.
			w := results.NewWriter(file)

			// Copy persistent issues.
			if copyPersistent != "" {
				if pfile, ok := pfiles[fmt.Sprintf("%s.csv", table.Name)]; ok {
					for _, r := range pfile.Results {
						if r.IsPersistent() || r.IsUnresolved() {

							res := r.Migrate()

							// Update for this version.
							res.Model = modelName
							res.ModelVersion = modelVersion
							res.DataVersion = dataVersion
							res.DQAVersion = dqaVersion

							count++
							w.Write(res)
						}
					}
				}
			}

			w.Flush()
			file.Close()
		}

		cmd.Printf("Wrote files to '%s' for model '%s/%s'\n", outDir, modelName, modelVersion)

		if copyPersistent != "" {
			cmd.Printf("Copied %d issues from '%s'\n", count, copyPersistent)
		}
	},
}

func init() {
	flags := Cmd.Flags()

	flags.String("root", ".", "Directory to the write the files to.")
	flags.String("model", "pedsnet", "The model the DQA files are generated for.")
	flags.String("version", "", "The version of the model the DQA files are generated for.")
	flags.String("dqa-version", "0", "The DQA version.")
	flags.String("url", dms.DefaultServiceURL, "Data models service URL.")
	flags.String("copy-persistent", "", "Copies issues in the specified path with a status of 'persistent' from an existing Secondary Report.")

	viper.BindPFlag("generate.root", flags.Lookup("root"))
	viper.BindPFlag("generate.model", flags.Lookup("model"))
	viper.BindPFlag("generate.version", flags.Lookup("version"))
	viper.BindPFlag("generate.dqa-version", flags.Lookup("dqa-version"))
	viper.BindPFlag("generate.url", flags.Lookup("url"))
	viper.BindPFlag("generate.copy-persistent", flags.Lookup("copy-persistent"))
}
