# Instructions for Executing the DQA Workflow

## Prerequisites

[Required Downloads](https://github.com/PEDSnet/Data-Quality-Analysis#required-downloads)

### Create RxNorm Reference Tables

Execute [DDL_RxNorm_Tables.sql](../Library/DDL_RxNorm_Tables.sql)  

**Note**: If using a custom schema for DQA tables, please change all references to schema "dqa" in the above script into the custom schema. 

### (Oracle-only) Create Views

Create views in database by running [Library/Oracle_LowerCaseViews.sql](https://github.com/PEDSnet/Data-Quality-Analysis/blob/master/Library/Oracle_LowerCaseViews.sql)
  * Replace "CDM_VIEW", "DQA_VIEW", and "VOCABULARY_VIEW" with appropriate schemas before running

## Apply Data Quality Checks

### Create Configuration Files
Create a DQA configuration file named `PEDSnet_config.yml` using the template specified in [PEDSnet_config_sample.yml](../Resources/PEDSnet_config_sample.yml) of the same folder and an Argos configuration file named `argos_config.json` using the template specified in [Argos Configuration File Structure](https://github.com/baileych/ohdsi-argos#configuration-file-structure).

#### Example

```
db:
 driver             : PostgreSQL (PostgreSQL, Oracle, MySQL, SQLite, or SQL Server)
 dbname	            : TestDb (Name of Database)
 dbuser	            : TestUser
 dbpass	            : TestUserPassword123
 dbhost             : http://testdbhost.com/
 dbport	            : 5432
 schema	            : pedsnet_domain_schema (Name of schema that holds PEDSnet domain tables)
 vocab_schema       : vocabulary (Name of schema that holds OMOP vocab tables)
 dqa_schema         : dqa (Name of schema that holds intermediate tables created for DQA purposes)
 argos_config_file_path : ~/Documents/.argos/argos_config.json (User-created location of ohdsi_argos configuration JSON file)
reporting:
 site_directory     : ~/Documents/test_directory (User-created temporary location for output of DQA reports)
 etl_script_version : v10
 dqa_script_version : 2.5.0_1 (See note)
 site               : SiteName 
 cdm                : PEDSnet
 conventions_version : 2.5.0 (the PEDSnet CDM ETL conventions version)
```


**Notes**:

* User created directories can be replaced with other locations, so long as the settings indicate them appropriately
* See [release](https://github.com/PEDSnet/Data-Quality-Analysis/releases) for `dqa_script_verison`.  This corresponds to the latest release version of the DQA Scripts.
* See [ohdsi-argos](https://github.com/baileych/ohdsi-argos) for installation and configuration instructions.
  * For Oracle users, Argos `src_name` should be `"oracle"`, and Argos `dbname` should be a connection string formatted as `"(DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(HOST=your_host)(PORT=1521))(CONNECT_DATA=(SID=your_sid)))"`.

### Setup
```R
setwd("/path/to/repo/")
source("Run_DQA.R")
```

### Level 1

#### All Tables
``` R
runDQA(level=1)
```

#### Single Table
```R
generateReportDependencies()

generateSingleReport(level=1, report="table_name")

"table_name" examples include: "person", "fact_relationship", "visit_occurrence", etc
```

** Note: `generateReportDependencies()` only needs to be run once, before generating any number of single reports. It is included in the beginning of `runDQA(level=1)`, and does not need to be run if `runDQA(level=1)` was used.

### Level 2

#### All Tables:
``` R
runDQA(level=2)
```
  * After Level 1, the database connection may occasionally close. To re-establish it, run `source("Run_DQA.R")` again before starting Level 2

#### Single Table:
```R
generateSingleReport(level=2, report="table_name")

"table_name" examples include: "procedure", "drug", "measurement", etc
```

## Generate feedback (GitHub) (DCC-Only)
To investigate differences between the previous data version and the current and generate feedback, follow the instructions [here](../Tools/dqa/README.md).
