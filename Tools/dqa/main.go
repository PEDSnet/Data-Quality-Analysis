package main

import (
	"os"

	"github.com/PEDSnet/tools/cmd/dqa/feedback"
	"github.com/PEDSnet/tools/cmd/dqa/generate"
	"github.com/PEDSnet/tools/cmd/dqa/issues"
	"github.com/PEDSnet/tools/cmd/dqa/migrate"
	"github.com/PEDSnet/tools/cmd/dqa/query"
	"github.com/PEDSnet/tools/cmd/dqa/rank"
	"github.com/PEDSnet/tools/cmd/dqa/validate"
	"github.com/blang/semver"
	"github.com/spf13/cobra"
)

var (
	progVersion = semver.Version{
		Major: 0,
		Minor: 3,
		Patch: 0,
		Pre: []semver.PRVersion{
			{VersionStr: "beta"},
			{VersionNum: 1},
		},
	}

	buildVersion string
)

func init() {
	progVersion.Build = []string{
		buildVersion,
	}
}

var mainCmd = &cobra.Command{
	Use: "pedsnet-dqa",

	Short: "Commands for the data quality analysis of PEDSnet data.",

	Run: func(cmd *cobra.Command, args []string) {
		cmd.Help()
	},
}

var versionCmd = &cobra.Command{
	Use: "version",

	Short: "Prints the version of the program.",

	Run: func(cmd *cobra.Command, args []string) {
		cmd.SetOutput(os.Stdout)
		cmd.Printf("%s\n", progVersion)
	},
}

func main() {
	mainCmd.AddCommand(versionCmd)
	mainCmd.AddCommand(generate.Cmd)
	mainCmd.AddCommand(validate.Cmd)
	mainCmd.AddCommand(feedback.Cmd)
	mainCmd.AddCommand(rank.Cmd)
	mainCmd.AddCommand(query.Cmd)
	mainCmd.AddCommand(issues.Cmd)
	mainCmd.AddCommand(migrate.Cmd)

	mainCmd.Execute()
}
