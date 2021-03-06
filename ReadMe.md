# Data Quality Assessment  in PEDSnet

## Additional Resources

### Introduction to the Tool

A summary of how to execute the tool for an initial run can be found here: ([Initial Run](https://github.com/PEDSnet/Data-Quality-Analysis/blob/master/Doc/PEDSnet_DQA_Documentation.md))

### Running for a PEDSnet Site

Instructions for how to execute the toolkit for a PEDSnet data submission can be found here: ([PEDSnet Site](https://github.com/PEDSnet/Data-Quality-Analysis/blob/master/Doc/DQA-HowtoexecuteDQAonaPEDSnetSiteDataset.pdf))

#### Uploading to the Database

Issues are uploaded at the end of each cycle in their raw form to the database. The script to do this is included in the package here and utlizes the argos package in the standard approach: ([Upload Issues](https://github.com/PEDSnet/Data-Quality-Analysis/blob/master/Tools/upload_issues.R))

To upload issues, set the <path> variable to the directory where the resulting issue .csv files were output, specifcy the data version in the <cdm> variable, and specify the site. Sourcing the script will upload the issues.

## Objective
This toolkit has been designed for conducting data quality assessments on clinical datasets modeled using the OMOP common data model. The toolkit includes a wide variety of data quality checks and a GitHub-based issue reporting mechanism. The toolkit is being routinely used by the PEDSnet CDRN. 

## Contents 

- Data: the data quality catalog of checks, summaries of previous data cycle, and acceptable valuesets for various fields.
- Doc:  documentation and set up instruction for the program 
- Infrastructure: constants and internal helper functions 
- Library: contains data quality checks and utility functions
- Main: single and multi-variable data quality scripts
- Resources: configuration file 
- Tools: scripts for GitHub-based feedback generation 

## Required Downloads


### R
R version 3.2.x or above, 64-bit ([Comprehensive R Archive Network](http://cran.r-project.org/))

### R Packages 

```
install.packages(c("DBI","yaml","ggplot2","RJDBC","devtools","futile.logger","plyr","dplyr",
"dbplyr","lubridate", "tictoc", "testthat", "data.table"))

install.packages("RPostgres")
library(devtools)
install_github("baileych/ohdsi-argos")
```
* Minimum Versions Required:
  * R: 3.2
  * DBI: 0.7
  * dplyr: 0.7
  * dbplyr: 1.2
  * readr: 1.1
  * rlang: 0.1.4
  * stringr: 1.2
* The `RPostgres` package is _not_ required if PostgreSQL is not the target database type
* For Oracle users, the [`ROracle`](https://cran.r-project.org/web/packages/ROracle/index.html) package should be installed

Note: if previously installed, run `update.packages()` to get the latest version of each library
