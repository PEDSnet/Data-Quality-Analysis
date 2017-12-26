package rules

import (
	"fmt"
	"strings"

	"../results"
)

type Condition struct {
	Name string
	Test func(r *results.Result) bool
}

// Field conditionals.
var isPersistent = &Condition{
	Test: func(r *results.Result) bool {
		return strings.ToLower(r.Status) == "persistent"
	},
}

var isPrimaryKey = &Condition{
	Test: func(r *results.Result) bool {
		for _, f := range r.Fields() {
			if f == fmt.Sprintf("%s_id", r.Table) {
				return true
			}
		}

		return false
	},
}

var isSourceValue = &Condition{
	Test: func(r *results.Result) bool {
		for _, f := range r.Fields() {
			if strings.HasSuffix(f, "_source_value") {
				return true
			}
		}
		return false
	},
}

var isConceptId = &Condition{
	Test: func(r *results.Result) bool {
		for _, f := range r.Fields() {
			if strings.HasSuffix(f, "_concept_id") {
				return true
			}
		}

		return false
	},
}

var isForeignKey = &Condition{
	Test: func(r *results.Result) bool {
		if isPrimaryKey.Test(r) || isConceptId.Test(r) {
			return false
		}

		for _, f := range r.Fields() {
			if strings.HasSuffix(f, "_id") {
				return true
			}
		}

		return false
	},
}

var isDateYear = &Condition{
	Test: func(r *results.Result) bool {
		for _, f := range r.Fields() {
			if strings.Contains(f, "date") || strings.Contains(f, "year") {
				return true
			}
		}

		return false

	},
}

var isDateYearTime = &Condition{
	Test: func(r *results.Result) bool {

		for _, f := range r.Fields() {
			if strings.HasSuffix(f, "_date") || strings.HasSuffix(f, "_year") || strings.HasSuffix(f, "_time") {
				return true
			}
		}

		return false
	},
}

var isOther = &Condition{
	Test: func(r *results.Result) bool {
		return r.Field != "" && !isPrimaryKey.Test(r) && !isForeignKey.Test(r) && !isSourceValue.Test(r) && !isConceptId.Test(r) && !isDateYear.Test(r)
	},
}
