library(DBI)
library(yaml)
library(dplyr)
library(RPostgreSQL)

generateLevel2Visit <- function () {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work
  
  big_data_flag<-TRUE
  table_name<-"visit_occurrence"
  
  
  # load the configuration file
  #get path for current script
  config = yaml.load_file(g_config_path)
  
  
  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/Level2_Visit_Report_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",config)
  log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/visit_occurrence_issue.csv",sep="")
  
  
  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:
 
  my_db <- dbConnect(RPostgres::Postgres(),dbname=config$db$dbname,
                        host=config$db$dbhost,
                        user =config$db$dbuser,
                        password =config$db$dbpass, sslmode="verify-full",
                        options=paste("-c search_path=",config$db$schema,sep=""))
            
  # Then reference a tbl within that src
  visit_tbl <- tbl(my_db, "visit_occurrence")
  
  
  #patient_tbl<-tbl(my_db, "person")
  
  death_tbl <- tbl(my_db, "death")
  
  concept_tbl <- tbl(my_db, dplyr::sql(paste('SELECT * FROM ',config$db$vocab_schema,'.concept',sep='')))
  
  #patient_dob_tbl <- tbl(my_db, dplyr::sql
   #                      ('SELECT person_id, to_date(year_of_birth||\'-\'||month_of_birth||\'-\'||day_of_birth,\'YYYY-MM-DD\') as dob FROM person'))
  
  ### AA009 datetime inconsistency
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('visit_start_datetime', 
                                                                                                 'visit_start_date'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('visit_end_datetime', 
                                                                                                 'visit_end_date'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  fileContent<-c(fileContent,"##Implausible Events")
  
  ## Temporal checks --- facts before birth date
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(PreBirth(), c(table_name, "person"), c('visit_start_date', 
                                                                                                 'birth_datetime'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  ## Temporal checks --- facts after death date
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(PostDeath(), c(table_name, "death"), c('visit_start_date', 
                                                                                                      'death_date'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  

  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(MissVisitFact(), c(table_name), NULL,my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)

  
  ### inpatient visits with no DRG data 
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(MissVisitTypeFact(), c(table_name, 'observation'),
                                                               c('9201','observation_concept_id','3040464'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  #print('CHECKPOINT')
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)
  
  #close the connection
  #close_database_connection_OHDSI(con,config)
}