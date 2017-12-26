package query

import (
	"bytes"
	"fmt"
	"strings"
	"testing"

	"../results"
)

func TestDB(t *testing.T) {
	db, err := Open()
	if err != nil {
		t.Fatal(err)
	}

	results := []*results.Result{
		{
			Model:        "pedsnet",
			ModelVersion: "2.2.0",
			DataVersion:  "pedsnet-2.2.0-site-v1",
			DQAVersion:   "0",
			Table:        "person",
			Field:        "person_id",
			Goal:         "Feasibility",
			CheckCode:    "",
			CheckType:    "",
			Finding:      "",
			Prevalence:   "",
			Rank:         0,
			SiteResponse: "",
			Cause:        "",
			Status:       "",
			Reviewer:     "",
		},
	}

	if err := db.Load(results); err != nil {
		t.Error(err)
	}

	buf := bytes.NewBuffer(nil)
	w := NewCSVWriter(buf)
	if err := db.Query(w, "select * from results"); err != nil {
		t.Error(err)
	}

	exp := fmt.Sprintf("%s\n%s", strings.Join(ColumnNames, ","), strings.Join(results[0].Row(), ","))
	act := buf.String()

	if strings.TrimSpace(act) != strings.TrimSpace(exp) {
		t.Errorf("Expected output %s, got ", exp, act)
	}
}
