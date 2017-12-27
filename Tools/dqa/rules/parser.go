package rules

import (
	"encoding/csv"
	"fmt"
	"io"
	"log"
	"regexp"
	"strings"

	"../results"
	"../uni"
	dms "github.com/chop-dbhi/data-models-service/client"
)

// Header of a valid rules file.
var (
	// Matches the contents of `in (string1, string2, ...)`
	inStmtRe = regexp.MustCompile(`^in\s*\(([^\)]+)\)$`)

	// Matches a standard identifier, including field and table names
	// and prevalence. This is used to validate the value.
	identRe = regexp.MustCompile(`^(?i:[a-z0-9_]+)$`)

	rulesHeader = []string{
		"Table",
		"Field",
		"Issue Code",
		"Prevalence",
		"Rank",
	}

	ruleFieldTypes = []string{
		"is primary key",
		"is source value",
		"is date/year",
		"is date/year/time",
		"is concept id",
		"is other",
	}
)

func inSlice(s string, a []string) bool {
	for _, x := range a {
		if x == s {
			return true
		}
	}

	return false
}

type Errors []error

func (e Errors) Error() string {
	errs := make([]string, len(e))

	for i, err := range e {
		errs[i] = err.Error()
	}

	return strings.Join(errs, "\n")
}

type ParseError struct {
	kind string
	line int
	err  error
}

func (e *ParseError) Error() string {
	return fmt.Sprintf("[%s] Line %d: %s", e.kind, e.line, e.err)
}

func NewParseError(kind string, line int, err error) error {
	return &ParseError{
		kind: kind,
		line: line,
		err:  err,
	}
}

type Parser struct {
	kind  string
	model *dms.Model
	cr    *csv.Reader
	line  int
	// Set of validation errors found as rules are parsed.
	verrs Errors
}

func (*Parser) isIdent(v string) bool {
	return identRe.MatchString(v)
}

func (p *Parser) parseInSet(v string) ([]string, error) {
	var l []string
	m := inStmtRe.FindAllStringSubmatch(v, 1)

	if len(m) > 0 {
		// Split tokens in submatch and trim the space.
		l = strings.Split(m[0][1], ",")
	} else {
		l = []string{v}
	}

	for i, x := range l {
		x = strings.TrimSpace(x)

		if !p.isIdent(x) {
			return nil, fmt.Errorf("'%s' is not a valid identifier", x)
		}

		l[i] = x
	}

	return l, nil
}

func (p *Parser) parseTable(v string) ([]string, error) {
	tables, err := p.parseInSet(v)
	if err != nil {
		return nil, err
	}

	// Validate tables.
	for _, t := range tables {
		if tbl := p.model.Tables.Get(t); tbl == nil {
			err = NewParseError(p.kind, p.line, fmt.Errorf("Table '%s' is not defined", t))
			p.verrs = append(p.verrs, err)
		}
	}

	return tables, nil
}

func (p *Parser) parseField(v string, tables []string) (*Condition, error) {
	// Check for type.
	switch strings.ToLower(strings.TrimSpace(v)) {
	case "is primary key":
		return isPrimaryKey, nil

	case "is source value":
		return isSourceValue, nil

	case "is date/year":
		return isDateYear, nil

	case "is date/year/time":
		return isDateYearTime, nil

	case "is foreign key":
		return isForeignKey, nil

	case "is concept id":
		return isConceptId, nil

	case "is other":
		return isOther, nil
	}

	// Assume in(..) or single value.
	fields, err := p.parseInSet(v)

	if err != nil {
		return nil, err
	}

	// Validate all fields are defined in all tables for this rule.
	for _, f := range fields {
		for _, t := range tables {
			tbl := p.model.Tables.Get(t)
			if tbl == nil {
				continue
			}

			if fld := tbl.Fields.Get(f); fld == nil {
				err := NewParseError(p.kind, p.line, fmt.Errorf("Field '%s' is not defined for table '%s'", f, t))
				p.verrs = append(p.verrs, err)
			}
		}
	}

	return &Condition{
		Test: func(r *results.Result) bool {
			for _, f := range r.Fields() {
				if inSlice(f, fields) {
					return true
				}
			}

			return false
		},
	}, nil
}

func (*Parser) parseCheckCode(v string) (string, error) {
	return strings.ToLower(v), nil
}

func (p *Parser) parsePrevalence(v string) ([]string, error) {
	if v == "-" {
		return []string{"unknown"}, nil
	}

	if v == "in (*)" {
		return results.Prevalences, nil
	}

	return p.parseInSet(v)
}

func (p *Parser) parseRank(v string) (results.Rank, error) {
	switch strings.ToLower(v) {
	case "high":
		return results.HighRank, nil

	case "medium":
		return results.MediumRank, nil

	case "low":
		return results.LowRank, nil
	}

	err := NewParseError(p.kind, p.line, fmt.Errorf("'%s' is not a valid rank", v))
	p.verrs = append(p.verrs, err)

	return 0, nil
}

// parse parses a single line in the rules file which produces one
// or more rules.
func (p *Parser) parse() (Rules, error) {
	row, err := p.cr.Read()

	if err == io.EOF {
		return nil, io.EOF
	}

	if err != nil {
		return nil, err
	}

	p.line++

	var (
		tables      []string
		condition   *Condition
		checkCode   string
		prevalences []string
		rank        results.Rank
	)

	if tables, err = p.parseTable(row[0]); err != nil {
		return nil, NewParseError(p.kind, p.line, err)
	}

	if condition, err = p.parseField(row[1], tables); err != nil {
		if condition == isDateYear {
			log.Printf("[warn] Deprecated type `is date/year` was on line %d. Change the rule type to `is date/year/time`.", p.line)
		}

		return nil, NewParseError(p.kind, p.line, err)
	}

	if checkCode, err = p.parseCheckCode(row[2]); err != nil {
		return nil, NewParseError(p.kind, p.line, err)
	}

	if prevalences, err = p.parsePrevalence(row[3]); err != nil {
		return nil, NewParseError(p.kind, p.line, err)
	}

	if rank, err = p.parseRank(row[4]); err != nil {
		return nil, NewParseError(p.kind, p.line, err)
	}

	var rules Rules

	for _, t := range tables {
		for _, pr := range prevalences {
			rules = append(rules, &Rule{
				Type:       p.kind,
				Table:      t,
				Condition:  condition,
				Prevalence: pr,
				CheckCode:  checkCode,
				Rank:       rank,
			})
		}
	}

	return rules, nil
}

// Parse parses all rules in the underlying reader.
func (p *Parser) Parse() (Rules, error) {
	var (
		err         error
		line, rules Rules
	)

	for {
		line, err = p.parse()

		if err == io.EOF {
			break
		}

		if err != nil {
			return nil, err
		}

		rules = append(rules, line...)
	}

	if len(p.verrs) > 0 {
		return nil, p.verrs
	}

	return rules, nil
}

// NewParser initializes a new parser for a rules file.
func NewParser(r io.Reader, m *dms.Model, kind string) (*Parser, error) {
	cr := csv.NewReader(uni.New(r))

	cr.FieldsPerRecord = len(rulesHeader)
	cr.TrimLeadingSpace = true
	cr.Comment = '#'
	cr.LazyQuotes = true
	cr.TrimLeadingSpace = true

	_, err := cr.Read()

	if err != nil {
		return nil, NewParseError(kind, 1, fmt.Errorf("Invalid header: %s", err))
	}

	return &Parser{
		kind:  kind,
		model: m,
		line:  1,
		cr:    cr,
	}, nil
}
