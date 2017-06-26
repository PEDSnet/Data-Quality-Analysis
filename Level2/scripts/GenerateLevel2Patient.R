library(DBI)
library(yaml)
library(dplyr)
library(RPostgreSQL)

generateLevel2Patient <- function() {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work
  
  big_data_flag<-TRUE
  table_name<-"person"
  config = yaml.load_file(g_config_path)
  
  
  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/Level2_Patient_Report_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",config)
  
  
  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:
  my_db <- src_postgres(dbname=config$db$dbname,
                        host=config$db$dbhost,
                        user =config$db$dbuser,
                        password =config$db$dbpass,
                        options=paste("-c search_path=",config$db$schema,sep=""))
  
  # Then reference a tbl within that src
  #observation_period_tbl <- tbl(my_db, "observation_period")
  #visit_tbl <- tbl(my_db, "visit_occurrence")
  patient_tbl<-tbl(my_db, "person")
  #condition_tbl<-tbl(my_db, "condition_occurrence")
  
  log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/person_issue.csv",sep="")
  
 # concept_tbl <- tbl(my_db, dplyr::sql('SELECT * FROM vocabulary.concept'))
  
  patient_dob_tbl <- tbl(my_db, dplyr::sql
                         ('SELECT person_id, to_date(year_of_birth||\'-\'||month_of_birth||\'-\'||day_of_birth,\'YYYY-MM-DD\') as dob
                            FROM person'))
  ##AA009 date time inconsistency 
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('time_of_birth', 
                                                                                                 'year_of_birth'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('time_of_birth', 
                                                                                                 'month_of_birth'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('time_of_birth', 
                                                                                                 'day_of_birth'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  #table_name<-"person"
  ## total patients
  ###########DQA CHECKPOINT############## 
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconCohort(), c(table_name), NULL,my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)
  
  #close the connection
  #close_database_connection_OHDSI(con,config)
}
