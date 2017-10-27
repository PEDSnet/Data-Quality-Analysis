package results

import "encoding/json"

// Rank is an ordered enumeration of result issue rankings.
type Rank int

func (r Rank) String() string {
	switch r {
	case HighRank:
		return "High"
	case MediumRank:
		return "Medium"
	case LowRank:
		return "Low"
	}

	return ""
}

func (r *Rank) MarshalJSON() ([]byte, error) {
	return json.Marshal(r.String())
}

func (r *Rank) UnmarshalJSON(b []byte) error {
	var s string
	if err := json.Unmarshal(b, &s); err != nil {
		return err
	}

	switch s {
	case "High":
		*r = HighRank
	case "Medium":
		*r = MediumRank
	case "Low":
		*r = LowRank
	}

	return nil
}

const (
	_ Rank = iota
	HighRank
	MediumRank
	LowRank
)

// Goals are a set of goals that each field try to achieve. For template generation,
// a result line is created for each goal.
var Goals = []string{
	"Fidelity",
	"Consistency",
	"Accuracy",
	"Feasibility",
}

// Prevalences are the set valid prevalence values.
var Prevalences = []string{
	"full",
	"high",
	"medium",
	"low",
	"unknown",
}

// Statuses are the set of valid issue statuses.
var Statuses = []string{
	"new",
	"withdrawn",
	"persistent",
	"under review",
	"solution proposed",
}

// Causes are teh set of valid issue causes.
var Causes = []string{
	"ETL: programming error",
	"ETL: unclear conventions",
	"ETL: administrative",
	"Provenance: missing in source",
	"Provenance: entry error or convention",
	"Provenance: site-specific ETL convention",
	"Provenance: new clinical workflow",
	"Provenance: true anomaly",
	"Provenance: EHR configuration",
	"Provenance: administrative workflow",
	"i2b2 transform",
	"Non-issue",
}

// ExcludedTables are a set of tables excluded from analysis.
var ExcludedTables = map[string]struct{}{
	"concept":               struct{}{},
	"concept_ancestor":      struct{}{},
	"concept_class":         struct{}{},
	"concept_relationship":  struct{}{},
	"concept_synonym":       struct{}{},
	"domain":                struct{}{},
	"drug_strength":         struct{}{},
	"source_to_concept_map": struct{}{},
	"relationship":          struct{}{},
	"vocabulary":            struct{}{},
}
