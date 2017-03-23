# Data Quality Assessment (DQA) in PEDSnet

## Contents 

- DQA_Catalog: The inventory of different types of DQA checks implemented in PEDSnet 
- Level1: The code for DQA checks involving single variable
- Level2: The code for DQA checks involving multiple variables 
- Infrastructure: Constants and internal helper functions
- Data: Historical information needed for DQA trends over time

## Required Downloads

### JDK

### R
R version 3.1.x or above, 64-bit ([Comprehensive R Archive Network](http://cran.r-project.org/))

### R Packages 

```
install.packages(c("RPostgreSQL","DBI","yaml","ggplot2","RJDBC","devtools","futile.logger))
library(devtools)
install_github("ohdsi/SqlRender") 
install_github("ohdsi/DatabaseConnector")       

```
For troubleshooting with `install_github("ohdsi/SqlRender")`, please see [here](https://github.com/OHDSI/SqlRender/issues/28). 


