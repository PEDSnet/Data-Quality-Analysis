# PEDSnet DQA CLI

The command-line interface for various DQA tools.

To get the full set of options for any command, use the `help` command.

```
pedsnet-dqa help <command>
```

## Generate Template

The `generate-templates` command generates a new set of files to be filled out. The `--copy-persistent` option can be used to copy persistent issues from the previous version of results.

Create the v4 result set for CHOP in the `ETLv4/` with the persistent issues in v3 copied over.

```
$ pedsnet-dqa generate-templates --copy-persistent=ETLv3 --root=ETLv4 CHOP ETLv4
Wrote files to 'ETLv4' for model 'pedsnet/2.0.0'
Copied persistent issues from 'ETLv3'
```

## Merge Issues

The `merge-issues` command reads issues from a separate CSV file produced some external process (e.g. R scripts) and written to the corresponding secondary report file. The input file must be a valid CSV file with the following fields in the header:

- `data_version`
- `table`
- `field`
- `issue_code`
- `issue_description`
- `finding`
- `prevalence`

Each row is added to existing secondary report unless the secondary report already contains an record for the same `table`, `field`, and `issue_code`. In this case a conflict warning is printed to the console for the user to handle manually.

To run, use the `merge-issues` subcommand. A GitHub token must be supplied since it fetches conflict threshold data from files on GitHub. The first argument is the secondary report directory. The remaining arguments are the files containing the issues to be merged or a directory containing those files.

```
$ pedsnet-dqa merge-issues --token=... ./ETLv9 care_site_issues.csv measurement_issues.csv
```

## Rank Issues

The `assign-rank-to-issues` command assigns a rank to issues based on a set of pre-determined rules. The set of rules are listed maintained [here](https://github.com/PEDSnet/Data-Quality/tree/master/SecondaryReports/Ranking). The rules are fetched dynamically which requires authorization against the repository (since it is private). This is done by supplying a [GitHub access token](https://help.github.com/articles/creating-an-access-token-for-command-line-use/) with the `--token` option.

Do a *dry run* on a set of results:

```
$ pedsnet-dqa assign-rank-to-issues --dryrun --token=abc123 ./ETLv4
+----------------+-------------------+-------------------------------+------------+------------+----------+----------+---------+
|      TYPE      |       TABLE       |             FIELD             | ISSUE CODE | PREVALENCE | NEW RANK | OLD RANK | CHANGED |
+----------------+-------------------+-------------------------------+------------+------------+----------+----------+---------+
| Administrative | care_site         | care_site_name                | G4-002     | full       | Medium   | Low      | Yes     |
| Administrative | care_site         | place_of_service_source_value | G4-002     | full       | Medium   | Low      | Yes     |
| Administrative | care_site         | specialty_source_value        | G4-002     | full       | Medium   | High     | Yes     |
| Administrative | location          | location_id                   | G2-013     | medium     | Low      | Low      | No      |
| Administrative | provider          | care_site_id                  | G2-013     | high       | Medium   | Low      | Yes     |
| Administrative | provider          | provider_id                   | G2-013     | high       | Medium   | Medium   | No      |
| Demographic    | death             | cause_source_value            | G4-002     | full       | Medium   | Medium   | No      |
| Demographic    | death             | person_id                     | G2-013     | low        | Medium   | Medium   | No      |
| Demographic    | person            | day_of_birth                  | G4-002     | full       | High     | High     | No      |
| Demographic    | person            | person_id                     | G2-013     | medium     | High     | High     | No      |
| Demographic    | person            | provider_id                   | G2-005     | high       | Low      | Low      | No      |
| Fact           | drug_exposure     | drug_exposure_start_date      | G2-009     | low        | Medium   | Medium   | No      |
| Fact           | drug_exposure     | drug_source_concept_id        | G4-002     | full       | High     | High     | No      |
| Fact           | drug_exposure     | person_id                     | G2-005     | high       | Medium   | High     | Yes     |
| Fact           | drug_exposure     | visit_occurrence_id           | G2-005     | high       | Medium   | Medium   | No      |
| Fact           | fact_relationship | relationship_concept_id       | G4-001     | unknown    | High     | High     | No      |
| Fact           | measurement       | measurement_date              | G2-010     | low        | Low      | Low      | No      |
| Fact           | measurement       | measurement_date              | G2-009     | low        | Medium   | Medium   | No      |
| Fact           | measurement       | person_id                     | G2-005     | high       | Medium   | Medium   | No      |
| Fact           | measurement       | visit_occurrence_id           | G2-005     | high       | Medium   | Medium   | No      |
| Fact           | observation       | observation_concept_id        | G2-013     | high       | High     | High     | No      |
| Fact           | observation       | person_id                     | G2-005     | medium     | Medium   | Medium   | No      |
| Fact           | visit_occurrence  | provider_id                   | G4-002     | low        | Low      | Low      | No      |
+----------------+-------------------+-------------------------------+------------+------------+----------+----------+---------+
```

## Site Feedback

The `feedback` command contains two subcommands for generating new feedback and synchronizing it from GitHub issues.

### Generate

The `generate` subcommand creates GitHub issues for each field-level issue and a general issue containing a summary of all issues for the cycle.

Running the following command will validate the issues and print a summary of those that will be created:

```
$ pedsnet-dqa feedback generate --cycle="April 2016" ./CHOP/ETLv8
7 issues found in 'observation.csv'
1 issues found in 'person.csv'
9 issues found in 'procedure_occurrence.csv'
1 issues found in 'visit_payer.csv'
15 issues found in 'drug_exposure.csv'
8 issues found in 'condition_occurrence.csv'
1 issues found in 'location.csv'
14 issues found in 'measurement.csv'
8 issues found in 'visit_occurrence.csv'
1 issues found in 'care_site.csv'
1 issues found in 'fact_relationship.csv'
2 issues found in 'measurement_organism.csv'
No new issues for 'observation_period.csv'
```

To post the feedback to GitHub, the GitHub `--token` option must be supplied and the flag `--post` to explicitly post to GitHub. This will create issues in GitHub for fields that do not already have a GitHub ID associated with it (for example from a previous data cycle) and update the CSV files with the GitHub IDs.

*Note: Since this changes the CSV files, they should be saved prior to running the command and ideally committed to git.*

```
$ pedsnet-dqa feedback generate --cycle="April 2016" --token=abc123 --post ./CHOP/ETLv8
```

### Sync

As issues are addressed on GitHub, the `Cause` and `Status` labels may be adjusted. To keep parity between the CSV files and GitHub issues, the `sync` command can be used to pull labels and update the CSV files.

```
$ pedsnet-dqa feedback sync --cycle="April 2016" --token=abc123 ./CHOP/ETLv8
```

## Query Issues

The `query` subcommand enables querying across the DQA results using SQL.

Given a file with the query named `persistent_fields.sql`:

```sql
select "table", field, count(*)
from results
where status = 'persistent'
group by "table", field
having count(*) > 1
order by count(*) desc, "table", field
```

The query can be read from stdin against multiple result sets.

```
$ pedsnet-dqa query - ./ETLv1 ./ETLv2 ./ETLv3 ./ETLv4 < persistent_fields.sql
+----------------------+------------------------+----------+
|        TABLE         |         FIELD          | COUNT(*) |
+----------------------+------------------------+----------+
| visit_occurrence     | visit_end_date         | 5        |
| observation          | value_as_number        | 4        |
| person               | pn_gestational_age     | 4        |
| person               | provider_id            | 4        |
| person               | year_of_birth          | 4        |
| visit_occurrence     | provider_id            | 4        |
| condition_occurrence | stop_reason            | 3        |
| measurement          | value_as_number        | 3        |
| visit_occurrence     | person_id              | 3        |
| condition_occurrence | condition_end_time     | 2        |
| condition_occurrence | person_id              | 2        |
| observation          | qualifier_source_value | 2        |
| observation          | unit_source_value      | 2        |
| person               | gender_source_value    | 2        |
| person               | time_of_birth          | 2        |
| procedure_occurrence | modifier_concept_id    | 2        |
| procedure_occurrence | modifier_source_value  | 2        |
| provider             | specialty_concept_id   | 2        |
| provider             | specialty_source_value | 2        |
| visit_occurrence     | visit_start_date       | 2        |
+----------------------+------------------------+----------+
```

## Validate Results

Checks the values in the DQA result files to be consistent. The validator checks the:

- `model version` is a valid semantic version.
- `goal` is one of the pre-defined choices.
- `prevalence` is one of the pre-defined choices.
- `rank` is one of the pre-defined choices.
- `cause` is one of the pre-defined choices.
- `status` is one of the pre-defined choices.

The set of *pre-defined choices* for each field are defined in the [SecondaryReports](https://github.com/PEDSnet/Data-Quality/tree/master/SecondaryReports#format-for-secondary-reports) repository.

The command can take one or more directories that contain the report files. For example, the following will evaluate all ETL versions for the CHOP site.

```
$ pedsnet-dqa validate ./CHOP/*
```

A recommended workflow is to produce a report for all sites and pipe it to `more` so the results can be paged through incrementally as the issues are fixed.

```
$ pedsnet-dqa validate \
    Boston/* \
    CCHMC/* \
    CHOP/* \
    Colorado/* \
    Nationwide/* \
    Nemours/* \
    Seattle/* \
    StLouis/* | more
```
