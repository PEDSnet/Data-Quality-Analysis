package issues

import (
	"bytes"
	"context"
	"encoding/csv"
	"fmt"
	"io"
	"regexp"
	"strconv"
	"strings"

	"../uni"
	"github.com/google/go-github/github"
	"golang.org/x/oauth2"
)

const (
	owner = "PEDSnet"

	catalogRepo = "Data-Quality-Analysis"
	catalogPath = "/Data/DQACatalog/"

	conflictRepo             = "Data-Quality-Results"
	conflictAssociationsPath = "SecondaryReports/ConflictResolution/conflict_associations.csv"
)

var checkCodeRe = regexp.MustCompile(`^([A-C][A-C]-\d{3})_`)

type Threshold struct {
	Lower int
	Upper int
}

type Catalog map[string]map[[2]string]*Threshold

// NewGitHubReport initializes a new report for posting to GitHub.
func GetCatalog(token string) (Catalog, error) {
	tk := &oauth2.Token{
		AccessToken: token,
	}

	ctx := context.Background()
	ts := oauth2.StaticTokenSource(tk)
	tc := oauth2.NewClient(oauth2.NoContext, ts)

	client := github.NewClient(tc)

	// Get conflict associations
	fileContent, _, _, err := client.Repositories.GetContents(ctx, owner, conflictRepo, conflictAssociationsPath, nil)
	if err != nil {
		return nil, err
	}
	content, err := fileContent.GetContent()
	if err != nil {
		return nil, err
	}

	buf := uni.New(bytes.NewBufferString(content))
	cr := csv.NewReader(buf)
	if _, err := cr.Read(); err != nil {
		return nil, err
	}

	checkIssuesCodes := make(map[string]string)

	for {
		row, err := cr.Read()
		if err == io.EOF {
			break
		} else if err != nil {
			return nil, err
		}

		checkIssuesCodes[row[1]] = row[0]
	}

	// Fetch thresholds from conflict check mappings.
	_, dirContent, _, err := client.Repositories.GetContents(ctx, owner, catalogRepo, catalogPath, nil)
	if err != nil {
		return nil, err
	}

	catalog := make(Catalog)

	for _, file := range dirContent {
		if *file.Type == "dir" {
			continue
		}

		if !checkCodeRe.MatchString(*file.Name) {
			continue
		}

		// Only process codes that are relevant.
		checkCode := checkCodeRe.FindStringSubmatch(*file.Name)[1]

		var targetCode string
		if _, ok := checkIssuesCodes[checkCode]; ok {
			targetCode = checkIssuesCodes[checkCode]
		} else {
			targetCode = checkCode
		}

		// Fetch to get contents.
		file, _, _, err = client.Repositories.GetContents(ctx, owner, catalogRepo, *file.Path, nil)
		if err != nil {
			return nil, err
		}
		content, err := file.GetContent()
		if err != nil {
			return nil, err
		}

		buf := uni.New(bytes.NewBufferString(content))
		cr := csv.NewReader(buf)
		head, err := cr.Read()
		if err != nil {
			continue
		}

		hlen := len(head)
		if hlen != 5 && hlen != 6 && hlen != 7 {
			return nil, fmt.Errorf("[%s] expected 5, 6, or 7 columns, got %d", targetCode, hlen)
		}

		if _, ok := catalog[targetCode]; !ok {
			catalog[targetCode] = make(map[[2]string]*Threshold)
		}

		var (
			lower int
			upper int
			field string
		)

		for {
			row, err := cr.Read()
			if err == io.EOF {
				break
			} else if err != nil {
				return nil, err
			}

			table := strings.ToLower(row[1])

			lower = 0
			upper = 0

			if hlen == 5 {
				field = strings.ToLower(row[2])
				lower, _ = strconv.Atoi(row[3])
				upper, _ = strconv.Atoi(row[4])
			} else {
				field = strings.ToLower(strings.Join([]string{row[2], row[3]}, ","))
				lower, _ = strconv.Atoi(row[4])
				upper, _ = strconv.Atoi(row[5])
			}

			catalog[targetCode][[2]string{table, field}] = &Threshold{
				Lower: lower,
				Upper: upper,
			}
		}
	}

	return catalog, nil
}
