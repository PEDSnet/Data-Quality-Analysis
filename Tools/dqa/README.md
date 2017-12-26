# Generate Feedback (GitHub)

## Required Downloads

### Python 
Python version 2.7

Note: Tested with Python version 2.7.12, has not been tested with Python 3

### Go
Go version 1.8.x or above

From Tools/dqa, run `go get` followed by `make build`

The executable pedsnet-dqa will then be available from the user's GOBIN directory.

## Investigate differences

```
Update causes of previous secondary reports 
pedsnet-dqa feedback sync --cycle={Previous Cycle Name} --token={GitHub Personal Access Token} /path/to/previous/reports/



Generate templates for new secondary reports
pedsnet-dqa generate-templates --version={PEDSnet Convention Version} --copy-persistent=/path/to/previous/reports/ --root=/path/to/new/reports/ {Site_Name} ETLv{x}

Investigate differences between previous and new issues and resolve conflicts
pedsnet-dqa merge-issues \
   --token=<token> \
   --program=/path/to/ConflictResolution/resolve.py \
   --resolvers=/path/to/ConflictResolution/resolvers \
   /path/to/new/reports/
   /path/to/site_directory/issues/*
```

## Report and Track Issues

```
pedsnet-dqa query - /path/to/new/reports < print_new_issues.sql

```
### Review new issues 
```
pedsnet-dqa feedback generate --cycle={Current Cycle Name} --token=abc123 --post /path/to/new/reports
```

This command creates GitHub issues for the identified DQA issues

**Notes** :

```
ETLv{x}:  
   {x} = Current ETL Script Version
   Example: ETLv14
   
{Previous Cycle Name}:
   Month and Year of previous cycle in quotes
   Example: "August 2017"

{Current Cycle Name}"
   Month and year of current cycle in quotes
   Example: "November 2017"
   
{PEDSnet Convention Version}
   Semantic version number of current PEDSnet Convention in quotes
   Example: "2.7.0"

{GitHub Personal Access Token}
   Personal access token generated from GitHub
```

Generate GitHub Personal Access Token [here](https://github.com/settings/tokens). Include all permissions under the "Repo" category.
