package query

import (
	"database/sql"
	"encoding/csv"
	"fmt"
	"io"
	"strings"

	"../results"
	"github.com/olekukonko/tablewriter"

	_ "github.com/mattn/go-sqlite3"
)

const TableName = "results"

var columnNames = []string{
	"model",
	"model_version",
	"data_version",
	"dqa_version",
	"table",
	"field",
	"goal",
	"check_code",
	"check_alias",
	"check_type",
	"finding",
	"prevalence",
	"rank",
	"site_response",
	"cause",
	"status",
	"reviewer",
	"github_id",
	"method",
}

type DB struct {
	db *sql.DB
}

func Open() (*DB, error) {
	db, err := sql.Open("sqlite3", ":memory:")
	if err != nil {
		return nil, err
	}

	cols := make([]string, len(columnNames))

	for i, col := range columnNames {
		cols[i] = fmt.Sprintf("\"%s\" TEXT", col)
	}

	stmt := fmt.Sprintf("CREATE TABLE %s (%s)", TableName, strings.Join(cols, ",\n"))

	if _, err = db.Exec(stmt); err != nil {
		return nil, err
	}

	return &DB{db}, nil
}

func (db *DB) Load(header []string, results []*results.Result) error {
	if len(results) == 0 {
		return nil
	}

	for i, c := range header {
		c = strings.Replace(strings.ToLower(c), " ", "_", -1)

		// Rename.
		switch c {
		case "issue_code":
			c = "check_code"
		case "issue_description":
			c = "check_type"
		}

		header[i] = fmt.Sprintf("`%s`", c)
	}

	params := make([]string, len(header))

	for i, _ := range params {
		params[i] = "?"
	}

	stmt := fmt.Sprintf("INSERT INTO %s (%s) VALUES (%s)", TableName, strings.Join(header, ","), strings.Join(params, ","))

	for _, r := range results {
		row := make([]interface{}, len(params))

		for i, c := range r.Row() {
			// Use null values for empty strings
			if c == "" {
				row[i] = nil
			} else {
				row[i] = c
			}
		}

		_, err := db.db.Exec(stmt, row...)
		if err != nil {
			return err
		}
	}

	return nil
}

func (db *DB) Query(w Writer, stmt string, args ...interface{}) error {
	rows, err := db.db.Query(stmt, args...)
	if err != nil {
		return err
	}

	defer rows.Close()

	cols, err := rows.Columns()
	if err != nil {
		return err
	}

	w.WriteHeader(cols)

	row := make([]interface{}, len(cols))
	out := make([]string, len(row))

	for i, _ := range row {
		row[i] = new(sql.NullString)
	}

	for rows.Next() {
		if err = rows.Scan(row...); err != nil {
			return err
		}

		for i, v := range row {
			x := v.(*sql.NullString)

			if x.Valid {
				out[i] = x.String
			} else {
				out[i] = ""
			}
		}

		w.WriteRow(out)
	}

	w.Flush()

	return rows.Err()
}

type Writer interface {
	WriteHeader([]string) error
	WriteRow([]string) error
	Flush() error
}

type PrettyWriter struct {
	tw *tablewriter.Table
}

func (w *PrettyWriter) WriteHeader(cols []string) error {
	w.tw.SetHeader(cols)
	return nil
}

func (w *PrettyWriter) WriteRow(row []string) error {
	w.tw.Append(row)
	return nil
}

func (w *PrettyWriter) Flush() error {
	w.tw.Render()
	return nil
}

func NewPrettyWriter(w io.Writer) *PrettyWriter {
	tw := tablewriter.NewWriter(w)
	return &PrettyWriter{tw}
}

type CSVWriter struct {
	csv *csv.Writer
}

func (w *CSVWriter) WriteHeader(cols []string) error {
	return w.csv.Write(cols)
}

func (w *CSVWriter) WriteRow(row []string) error {
	return w.csv.Write(row)
}

func (w *CSVWriter) Flush() error {
	w.csv.Flush()
	return w.csv.Error()
}

func NewCSVWriter(w io.Writer) *CSVWriter {
	return &CSVWriter{csv.NewWriter(w)}
}
