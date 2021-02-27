library(Argos)
library(dplyr)
library(DBI)
source('~/Documents/code/dqa/Data-Quality-Analysis/Tools/upload_helpers.R')

#####################USER INPUT#####################
#Specify Path
path = "~/Documents/code/dqa/Data-Quality-Results/SecondaryReports/CCHMC/ETLv25/"
#Specify Site (lower case) 
site = 'CCHMC'
#Specify CDM
cdm = 'v4.0'
#Name of target table for storing dqa issues
target_table <- "dqa_issues"
#Toggle for if this is the first instance of dqa issues upload
first = F
#####################END USER INPUT##################


##Run
db <- src_argos(paths = '/Users/callahanc5/.argos/argos_upload.json')
dbSendQuery(db, 'set role peds_staff')


new_issues <- suppressWarnings(read_issues(path = path)) %>% mutate(site = site, cdm = cdm)

if(first){ 
  dplyr::copy_to(dest = db, new_issues, name = 'temp', overwrite = F) %>%
    compute_new(temporary = F, name = target_table)} else{
  issues_tbl <- tbl(db, target_table)
  issues_hold <- issues_tbl %>% compute(name = 'issues_hold', temporary = T)
  dplyr::union(x = issues_hold, new_issues, copy = T) %>% 
  distinct() %>% 
  compute_new(name = target_table,temporary = F)
}

##Done






