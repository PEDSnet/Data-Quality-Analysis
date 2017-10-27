package results

import (
	"bytes"
	"strings"
	"testing"
)

var fileSample = `Model,Model Version,Data Version,DQA Version,Table,Field,Goal,Issue Code,Issue Description,Finding,Prevalence,Rank,Site Response,Cause,Status,Reviewer,Github ID
pedsnet,2.1.0,pedsnet-2.1.0-SITE-ETLv1,0,care_site,care_site_id,Fidelity,,,,,,,,,,
pedsnet,2.1.0,pedsnet-2.1.0-SITE-ETLv1,0,care_site,care_site_id,Consistency,,,,,,,,,,
pedsnet,2.1.0,pedsnet-2.1.0-SITE-ETLv1,0,care_site,care_site_id,Accuracy,,,,,,,,,,
pedsnet,2.1.0,pedsnet-2.1.0-SITE-ETLv1,0,care_site,care_site_id,Feasability,,,,,,,,,,
`

func TestReader(t *testing.T) {
	buf := bytes.NewBufferString(fileSample)
	r, err := NewReader(buf)
	if err != nil {
		t.Fatalf("Error initializing reader: %s", err)
	}

	results, err := r.ReadAll()
	if err != nil {
		t.Fatalf("Error reading results: %s", err)
	}

	if len(results) != 4 {
		t.Errorf("Expected 4 results, got %d", len(results))
	}
}

func TestWriter(t *testing.T) {
	buf := bytes.NewBufferString(fileSample)

	r, err := NewReader(buf)
	if err != nil {
		t.Fatalf("Error initializing reader: %s", err)
	}

	results, err := r.ReadAll()
	if err != nil {
		t.Fatalf("Error reading results: %s", err)
	}

	wbuf := bytes.NewBuffer(nil)
	w := NewWriter(wbuf)

	if err := w.WriteAll(results); err != nil {
		t.Fatalf("Error writing results: %s", err)
	}

	// Flush to buffer.
	w.Flush()

	output := strings.TrimSpace(wbuf.String())
	if output != strings.TrimSpace(fileSample) {
		t.Errorf("Input does not match output:\n%s", output)
	}
}
