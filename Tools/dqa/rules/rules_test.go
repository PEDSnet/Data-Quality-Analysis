package rules

import (
	"fmt"
	"log"
	"os"
	"strings"
	"testing"

	"github.com/PEDSnet/tools/cmd/dqa/results"
	dms "github.com/chop-dbhi/data-models-service/client"
)

var model *dms.Model

func init() {
	url := dms.DefaultServiceURL

	client, err := dms.New(url)
	if err != nil {
		log.Fatalf("Could not connect to service %s: %s", url, err)
	}

	model, err = client.ModelRevision("pedsnet", "2.2.0")
	if err != nil {
		log.Fatalf("Error fetching model: %s", err)
	}
}

var (
	testRules = `
table,field,issue code,prevalence,rank
"in (condition_occurrence, visit_payer)",is primary key,G4-001,full,High
visit_payer,is source value,G2-013,"in (medium, high, low)",High
"visit_payer",is date/year,G2-002,unknown,High
"visit_payer","in (plan_type, plan_class)",G2-013,"in (high, low)",Medium
"visit_payer","is concept id",G3-002,-,Medium
"visit_payer","is other",G3-002,-,Medium
"visit_payer","is date/year/time",G2-002,-,Low
`
	parsedRules = []struct {
		Rule      Rule
		TestField string
	}{
		// Line 1
		{
			Rule{
				Table:      "condition_occurrence",
				Condition:  isPrimaryKey,
				CheckCode:  "g4-001",
				Prevalence: "full",
				Rank:       results.HighRank,
			},
			"condition_occurrence_id",
		},
		{
			Rule{
				Table:      "visit_payer",
				Condition:  isPrimaryKey,
				CheckCode:  "g4-001",
				Prevalence: "full",
				Rank:       results.HighRank,
			},
			"visit_payer_id",
		},

		// Line 2
		{
			Rule{
				Table:      "visit_payer",
				Condition:  isSourceValue,
				CheckCode:  "g2-013",
				Prevalence: "medium",
				Rank:       results.HighRank,
			},
			"visit_payer_source_value",
		},
		{
			Rule{
				Table:      "visit_payer",
				Condition:  isSourceValue,
				CheckCode:  "g2-013",
				Prevalence: "high",
				Rank:       results.HighRank,
			},
			"visit_payer_source_value",
		},
		{
			Rule{
				Table:      "visit_payer",
				Condition:  isSourceValue,
				CheckCode:  "g2-013",
				Prevalence: "low",
				Rank:       results.HighRank,
			},
			"visit_payer_source_value",
		},

		// Line 3
		{
			Rule{
				Table:      "visit_payer",
				Condition:  isDateYear,
				CheckCode:  "g2-002",
				Prevalence: "unknown",
				Rank:       results.HighRank,
			},
			"visit_payer_date",
		},

		// Line 4
		{
			Rule{
				Table: "visit_payer",
				Condition: &Condition{
					Test: func(r *results.Result) bool {
						switch r.Field {
						case "plan_type", "plan_class":
							return true
						}
						return false
					},
				},
				CheckCode:  "g2-013",
				Prevalence: "high",
				Rank:       results.MediumRank,
			},
			"plan_type",
		},
		{
			Rule{
				Table: "visit_payer",
				Condition: &Condition{
					Test: func(r *results.Result) bool {
						switch r.Field {
						case "plan_type", "plan_class":
							return true
						}
						return false
					},
				},
				CheckCode:  "g2-013",
				Prevalence: "low",
				Rank:       results.MediumRank,
			},
			"plan_class",
		},

		// Line 5
		{
			Rule{
				Table:      "visit_payer",
				Condition:  isConceptId,
				CheckCode:  "g3-002",
				Prevalence: "unknown",
				Rank:       results.MediumRank,
			},
			"visit_payer_concept_id",
		},

		// Line 6
		{
			Rule{
				Table:      "visit_payer",
				Condition:  isOther,
				CheckCode:  "g3-002",
				Prevalence: "unknown",
				Rank:       results.MediumRank,
			},
			"some_field",
		},

		{
			Rule{
				Table:      "visit_payer",
				Condition:  isDateYearTime,
				CheckCode:  "g2-002",
				Prevalence: "unknown",
				Rank:       results.LowRank,
			},
			"visit_payer_time",
		},
	}
)

func TestRulesParser(t *testing.T) {
	r := strings.NewReader(testRules)

	p, err := NewParser(r, model, "")

	if err != nil {
		t.Fatal(err)
	}

	rules, err := p.Parse()

	if err != nil {
		t.Error(err)
	}

	if len(rules) != len(parsedRules) {
		t.Errorf("expected %d rules, got %d", len(parsedRules), len(rules))
		t.Fatal(rules)
	}

	for i, act := range rules {
		exp := parsedRules[i]

		if act.Table != exp.Rule.Table {
			t.Errorf("[%d] expected %s, got %s", i, act.Table, exp.Rule.Table)
		}

		if act.CheckCode != exp.Rule.CheckCode {
			t.Errorf("[%d] expected %s, got %s", i, act.CheckCode, exp.Rule.CheckCode)
		}

		if act.Prevalence != exp.Rule.Prevalence {
			t.Errorf("[%d] expected %s, got %s", i, act.Prevalence, exp.Rule.Prevalence)
		}

		if act.Rank != exp.Rule.Rank {
			t.Errorf("[%d] expected %s, got %s", i, act.Rank, exp.Rule.Rank)
		}

		res := &results.Result{
			Table:      act.Table,
			Field:      exp.TestField,
			CheckCode:  act.CheckCode,
			Prevalence: act.Prevalence,
		}

		if !exp.Rule.Condition.Test(res) {
			panic(fmt.Sprintf("[%d] expected condition failed", i))
		}

		if !act.Condition.Test(res) {
			t.Errorf("[%d] condition function doesn't match", i)
		}

		if !act.Matches(res) {
			t.Errorf("[%d] rule does not match result", i)
		}
	}
}

func TestFetch(t *testing.T) {
	token := os.Getenv("GITHUB_AUTH_TOKEN")

	if token == "" {
		t.Skip()
	}

	rules, err := Fetch(token, model)
	if err != nil {
		t.Fatal(err)
	}

	if len(rules) == 0 {
		t.Errorf("No rules parsed")
	}
}
