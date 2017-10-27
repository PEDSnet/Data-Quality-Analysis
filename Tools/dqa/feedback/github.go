// The scope of this module is to create an issue on GitHub for each
// issue found in a Secondary Report analysis.
package feedback

import (
	"bytes"
	"context"
	"fmt"
	"strings"

	"golang.org/x/oauth2"

	"github.com/PEDSnet/tools/cmd/dqa/results"
	"github.com/google/go-github/github"
)

const (
	repoOwner = "PEDSnet"

	dataQualityLabel        = "Data Quality"
	dataQualitySummaryLabel = "Data Quality Summary"
)

var (
	// Labels that require a string.
	dataCycleLabel = Labeler("Data Cycle")
	tableLabel     = Labeler("Table")
	rankLabel      = Labeler("Rank")
	causeLabel     = Labeler("Cause")
	statusLabel    = Labeler("Status")
)

// Eventually we will make this more dynamic, at least for line numbers
var githubCheckURL = "https://github.com/PEDSnet/Data-Quality-Analysis/blob/master/Level1/library/%s.R#L16"

func Labeler(p string) func(interface{}) string {
	return func(v interface{}) string {
		return fmt.Sprintf("%s: %s", p, v)
	}
}

func ParseLabel(l string) (string, string, error) {
	toks := strings.SplitN(l, ": ", 2)
	if len(toks) != 2 {
		return "", "", fmt.Errorf("Could not parse label `%s`", l)
	}

	return toks[0], toks[1], nil
}

type GithubReport struct {
	Site       string
	ETLVersion string
	DataCycle  string

	// Keep track of all results that were included in this report
	// for the summary.
	results results.Results

	client *github.Client
	ctx    context.Context
}

func (gr *GithubReport) Len() int {
	return len(gr.results)
}

// FetchSummaryIssues fetches the DQA summary issue.
func (gr *GithubReport) FetchSummaryIssue(ir *github.IssueRequest) (*github.Issue, error) {
	opts := &github.IssueListByRepoOptions{
		State:  "all",
		Labels: *ir.Labels,
	}

	issues, _, err := gr.client.Issues.ListByRepo(gr.ctx, repoOwner, gr.Site, opts)
	if err != nil {
		return nil, err
	}

	if len(issues) == 1 {
		return issues[0], nil
	}

	if len(issues) > 1 {
		// List of URLs to inspect.
		urls := make([]string, len(issues))

		for i, issue := range issues {
			urls[i] = fmt.Sprintf("- %s", issue.HTMLURL)
		}

		return nil, fmt.Errorf("Multiple issues match:\n%s", strings.Join(urls, "\n"))
	}

	return nil, nil
}

// FetchIssues fetches all issues for this site and data cyle.
func (gr *GithubReport) FetchIssues() ([]*github.Issue, error) {
	labels := []string{
		dataQualityLabel,
		dataCycleLabel(gr.DataCycle),
	}

	opts := &github.IssueListByRepoOptions{
		State:  "all",
		Labels: labels,
		ListOptions: github.ListOptions{
			PerPage: 100,
		},
	}

	var issues []*github.Issue

	for {
		page, resp, err := gr.client.Issues.ListByRepo(gr.ctx, repoOwner, gr.Site, opts)
		if err != nil {
			return nil, err
		}

		issues = append(issues, page...)

		if resp.NextPage == 0 {
			break
		}

		opts.Page = resp.NextPage
	}

	return issues, nil
}

func (gr *GithubReport) CreateComment(id int, body string) error {
	c := github.IssueComment{Body: &body}
	_, _, err := gr.client.Issues.CreateComment(gr.ctx, repoOwner, gr.Site, id, &c)
	return err
}

// FetchIssue fetches an issue by id.
func (gr *GithubReport) FetchIssue(id int) (*github.Issue, error) {
	issue, _, err := gr.client.Issues.Get(gr.ctx, repoOwner, gr.Site, id)
	if err != nil {
		return nil, err
	}
	return issue, nil
}

func (gr *GithubReport) OpenIssue(id int) error {
	state := "open"

	ir := &github.IssueRequest{
		State: &state,
	}

	_, _, err := gr.client.Issues.Edit(gr.ctx, repoOwner, gr.Site, id, ir)
	if err != nil {
		return err
	}

	return nil
}

// BuildSummaryIssue builds a new issue requeset for the summary issue for this data cycle.
func (gr *GithubReport) BuildSummaryIssue() (*github.IssueRequest, error) {
	f := &results.File{
		Results: gr.results,
	}

	r := results.NewMarkdownReport(f)
	buf := bytes.NewBuffer(nil)

	if err := r.Render(buf); err != nil {
		return nil, err
	}

	res := f.Results[0]

	title := fmt.Sprintf("DQA Summary: %s (%s) for PEDSnet CDM v%s", gr.DataCycle, gr.ETLVersion, res.ModelVersion)
	body := buf.String()
	labels := []string{
		dataQualityLabel,
		dataQualitySummaryLabel,
		dataCycleLabel(gr.DataCycle),
	}

	ir := github.IssueRequest{
		Title:  &title,
		Body:   &body,
		Labels: &labels,
	}

	return &ir, nil
}

// BuildIssue builds a new issue request based on the result issue.
func (gr *GithubReport) BuildIssue(r *results.Result) (*github.IssueRequest, error) {
	if r.SiteName() != gr.Site || r.ETLVersion() != gr.ETLVersion {
		return nil, fmt.Errorf("Result site or ETL version does not match reports")
	}

	var title string
	if r.Field == "" {
		title = fmt.Sprintf("DQA: %s (%s): %s", gr.DataCycle, gr.ETLVersion, r.Table)
	} else {
		title = fmt.Sprintf("DQA: %s (%s): %s/{%s}", gr.DataCycle, gr.ETLVersion, r.Table, r.Field)
	}

	var body string

	if r.CheckAlias == "" {
		body = fmt.Sprintf("**Description**: %s\n**Finding**: %s", r.CheckType, r.Finding)
	} else {
		url := fmt.Sprintf(githubCheckURL, strings.TrimSpace(r.CheckAlias))
		body = fmt.Sprintf("**Description**: [%s](%s)\n**Finding**: %s", strings.Title(r.CheckType), url, r.Finding)
	}

	labels := []string{
		dataQualityLabel,
		dataCycleLabel(gr.DataCycle),
		tableLabel(r.Table),
	}

	if r.Rank > 0 {
		labels = append(labels, rankLabel(r.Rank))
	}

	if r.Cause != "" {
		labels = append(labels, causeLabel(r.Cause))
	}

	if r.Status != "" {
		labels = append(labels, statusLabel(r.Status))
	}

	// All fields are pointers.
	ir := github.IssueRequest{
		Title:  &title,
		Body:   &body,
		Labels: &labels,
	}

	gr.results = append(gr.results, r)

	return &ir, nil
}

// AddLabels the minimum labels are set on the issue.
func (gr *GithubReport) AddLabels(num int, labels []string) ([]*github.Label, error) {
	allLabels, _, err := gr.client.Issues.AddLabelsToIssue(gr.ctx, repoOwner, gr.Site, num, labels)

	if err != nil {
		return nil, err
	}

	return allLabels, nil
}

// PostIssue sends a request to the GitHub API to create an issue.
// Upon success, a concrete issue is returned with the ID.
func (gr *GithubReport) PostIssue(ir *github.IssueRequest) (*github.Issue, error) {
	issue, _, err := gr.client.Issues.Create(gr.ctx, repoOwner, gr.Site, ir)

	if err != nil {
		return nil, err
	}

	return issue, nil
}

// NewGitHubReport initializes a new report for posting to GitHub.
func NewGitHubReport(site, etl, cycle, token string) *GithubReport {
	tk := &oauth2.Token{
		AccessToken: token,
	}

	ctx := context.Background()
	ts := oauth2.StaticTokenSource(tk)
	tc := oauth2.NewClient(oauth2.NoContext, ts)

	client := github.NewClient(tc)

	return &GithubReport{
		Site:       site,
		ETLVersion: etl,
		DataCycle:  cycle,
		client:     client,
		ctx:        ctx,
	}
}
