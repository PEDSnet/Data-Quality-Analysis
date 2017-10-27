package results

import (
	"bytes"
	"testing"
)

func TestReport(t *testing.T) {
	f := NewFile("")
	f.Results = []*Result{
		{
			Table:     "person",
			Field:     "ethnicity_source_value",
			Goal:      "Feasability",
			CheckCode: "G4-002",
			Rank:      LowRank,
		},
		{
			Table:     "person",
			Field:     "location_id",
			Goal:      "Feasability",
			CheckCode: "G4-002",
			Rank:      LowRank,
		},
		{
			Table:     "observation",
			Field:     "location_id",
			Goal:      "Accuracy",
			CheckCode: "G3-003",
			Rank:      MediumRank,
		},
	}

	r := NewMarkdownReport(f)

	buf := bytes.NewBuffer(nil)

	if err := r.Render(buf); err != nil {
		t.Fatalf("Error rendering report: %s", err)
	}
}
