library(DBI)
library(yaml)
library(dplyr)

generateLevel2Patient <- function() {
  table_name<-"person"
  
  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(g_config$reporting$site_directory),"./reports/Level2_Patient_Report_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",g_config)
  
  # Then reference a tbl within that src
  patient_tbl<-cdm_tbl(req_env$db_src, "person")
  
  log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/person_issue.csv",sep="")
  
  ##AA009 date time inconsistency 
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('birth_datetime', 
                                                                                                 'year_of_birth'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('birth_datetime', 
                                                                                                 'month_of_birth'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('birth_datetime', 
                                                                                                 'day_of_birth'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  ## total patients
  ###########DQA CHECKPOINT############## 
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconCohort(), c(table_name), NULL)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)
  
}