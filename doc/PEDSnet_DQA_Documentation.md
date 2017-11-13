# Instructions for Executing the DQA Workflow

## Prerequisites

[Required Downloads](https://github.com/PEDSnet/Data-Quality#required-downloads)

## Apply Data Quality Checks

### Create Configuration File
Create a configuration file named `PEDSnet_config.yml` using the template specified in [PEDSnet_config_sample.yml](../Resources/PEDSnet_config_sample.yml) of the same folder.

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
 dqa_schema			: dqa (Name of schema that holds intermediate tables created for DQA purposes)
reporting:
 site_directory     : ~/Documents/test_directory (Temporary location for output of DQA reports)
 etl_script_version : v10
 dqa_script_version : 2.5.0_1 (See note)
 site               : SiteName 
 cdm                : PEDSnet
 conventions_version : 2.5.0 (the PEDSnet CDM ETL conventions version)
```

**Notes**:

* See [release](https://github.com/PEDSnet/Data-Quality-Analysis/releases) for `dqa_script_verison`.  This corresponds to the latest release version of the DQA Scripts.

### Create RxNorm Reference Tables

Execute [DDL_RxNorm_Tables.sql](../Library/DDL_RxNorm_Tables.sql)  

**Note**: If using a custom schema for DQA tables, please change all references to schema "dqa" in the above script into the custom schema.  


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
generateSingleReport(level=1, report="table_name")

"table_name" examples include: "person", "fact_relationship", "visit_occurrence", etc
```

### Level 2

#### All Tables: Postgres
``` R
runDQA(level=2)
```

#### Single Table: Postgres
```R
generateSingleReport(level=2, report="table_name")

"table_name" examples include: "procedure", "drug", "measurement", etc
```

## Generate feedback (GitHub)
To investigate differences between the previous data version and the current and generate feedback, follow the instructions [here](../Tools/dqa/README.md).
