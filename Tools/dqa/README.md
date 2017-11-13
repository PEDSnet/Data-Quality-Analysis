# Generate Feedback (GitHub)

## Required Downloads

### Python 
Python version 2.7

Note: Tested with Python version 2.7.12, has not been tested with Python 3

### Go
Go version 1.8.x or above


## Investigate differences

```
../pedsnet-dqa feedback sync --cycle="M0 YYYY" --token=abc123 ./Site/ETLv{x-1}
../pedsnet-dqa generate-templates --version="a.b.c" --copy-persistent={Site}/ETLv{x-1} --root={Site}/ETLv{x} {Site} ETLv{x}

../pedsnet-dqa merge-issues \
   --token=<token> \
   --program=./ConflictResolution/resolve.py \
   --resolvers=./ConflictResolution/resolvers \
   {Site}/ETLv{x}
   /path/to/site_directory/issues/*
```
These commands sync up causes of previous reports from Github, generate the templates for secondary reports, and merge the automatically identified issues (resolving any conflicts).


**Notes** :

```
ETLv{x} = Current ETL Script Version
ETLv{x - 1} = Previous ETL Script Version
```

## Report and Track Issues

```
./pedsnet-dqa query - {Site]/ETLv{x} < print_new_issues.sql

```
Review new issues 
```
../pedsnet-dqa feedback generate --cycle="M1 YYYY" --token=abc123 --post ./{Site}/ETLv{x}
```
This command creates GitHub issues for the identified DQA issues
