# Data Quality Assessment  in PEDSnet

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

### JDK  
Java version 5

If using OSX, download [here](https://support.apple.com/kb/dl1572?locale=en_US)

Note: version 5 must be installed, even if a higher version of Java is installed

### R
R version 3.1.x or above, 64-bit ([Comprehensive R Archive Network](http://cran.r-project.org/))

### R Packages 

```
install.packages(c("RPostgres","DBI","yaml","ggplot2","RJDBC","devtools","futile.logger"))
library(devtools)
install_github("ohdsi/SqlRender")
install_github("ohdsi/DatabaseConnector")
install_github("baileych/ohdsi-argos")
```
For troubleshooting with `install_github("ohdsi/SqlRender")`, please see [here](https://github.com/OHDSI/SqlRender/issues/28). 

Note: if previously installed, run `update.packages()` to get the latest version of each library
