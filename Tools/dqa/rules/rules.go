package rules

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"strings"

	"../results"
	dms "github.com/chop-dbhi/data-models-service/client"
)

const repoURL = "https://api.github.com/repos/PEDSnet/Data-Quality-Results/contents/%s"

var rulePaths = map[string]string{
	"Admin":       "SecondaryReports/Ranking/RuleSet1_Admin.csv",
	"Demographic": "SecondaryReports/Ranking/RuleSet2_Demographic.csv",
	"Fact":        "SecondaryReports/Ranking/RuleSet3_Fact.csv",
}

// fetchRules fetches the raw contents of a rules file through the GitHub API.
func fetch(kind, path string, token string, model *dms.Model) (Rules, error) {
	// TODO: replace with github client library.
	url := fmt.Sprintf(repoURL, path)
	req, err := http.NewRequest("GET", url, nil)

	if err != nil {
		return nil, err
	}

	req.Header.Set("Accept", "application/vnd.github.v3.raw")
	req.Header.Set("Authorization", "token "+token)

	resp, err := http.DefaultClient.Do(req)

	if err != nil {
		return nil, err
	}

	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		var msg string

		if body, err := ioutil.ReadAll(resp.Body); err != nil {
			msg = "Problem decoding error messages."
		} else {
			msg = string(body)
		}

		return nil, fmt.Errorf("Error fetching `%s` rule file\n%s: %s", kind, resp.Status, msg)
	}

	parser, err := NewParser(resp.Body, model, kind)
	if err != nil {
		return nil, err
	}

	rules, err := parser.Parse()
	if err != nil {
		return nil, err
	}

	return rules, nil
}

// Fetch retrieves all rule files that are hosted on GitHub.
func Fetch(token string, model *dms.Model) (Rules, error) {
	var allrules Rules

	for kind, path := range rulePaths {
		rules, err := fetch(kind, path, token, model)
		if err != nil {
			return nil, err
		}

		allrules = append(allrules, rules...)
	}

	return allrules, nil
}

// Rule defines a mapping from a table, field condition, issue code, and
// prevalence to a specific rank.
type Rule struct {
	Type       string
	Table      string
	Condition  *Condition
	CheckCode  string
	Prevalence string
	Rank       results.Rank
}

// Matches takes a result and determines if the result matches the rule.
func (r *Rule) Matches(s *results.Result) bool {
	if strings.ToLower(s.Table) != r.Table {
		return false
	}

	if !r.Condition.Test(s) {
		return false
	}

	if strings.ToLower(s.CheckCode) != r.CheckCode {
		return false
	}

	if strings.ToLower(s.Prevalence) != r.Prevalence {
		return false
	}

	return true
}

type Rules []*Rule

// Run iterates through all rules for the result until a match is found.
func (s Rules) Run(r *results.Result) (*Rule, bool) {
	for _, rule := range s {
		if match := rule.Matches(r); match {
			return rule, true
		}
	}

	return nil, false
}
