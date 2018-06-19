library(DBI)
library(yaml)
library(dplyr)

generateLevel2Visit <- function () {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work
  
  big_data_flag<-TRUE
  table_name<-"visit_occurrence"
  
  
  # load the configuration file
  #get path for current script
#  config = yaml.load_file(g_config_path)
  
  
  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(g_config$reporting$site_directory),"./reports/Level2_Visit_Report_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",g_config)
  log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/visit_occurrence_issue.csv",sep="")
  
  
  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:
 
           
  # Then reference a tbl within that src
  visit_tbl <- cdm_tbl(req_env$db_src, "visit_occurrence")
  
  
  #patient_tbl<-tbl(my_db, "person")
  
  death_tbl <- cdm_tbl(req_env$db_src, "death")
  
  concept_tbl <- vocab_tbl(req_env$db_src, 'concept')
  
  #patient_dob_tbl <- tbl(my_db, dplyr::sql
   #                      ('SELECT person_id, to_date(year_of_birth||\'-\'||month_of_birth||\'-\'||day_of_birth,\'YYYY-MM-DD\') as dob FROM person'))
  ### temporal outlier check 
  field_name<-"visit_start_date"
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(TempOutlier(), c(table_name), 
                                                               c(field_name, 'visit_concept_id'), c(9201,'inpatient'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"(inpatient)","\n"))
  fileContent<-c(fileContent,paste_image_name(table_name,paste0(field_name,'-yyyy-mm-inpatient')));
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(TempOutlier(), c(table_name), 
                                                               c(field_name, 'visit_concept_id'), c(9202,'outpatient'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"(outpatient)","\n"))
  fileContent<-c(fileContent,paste_image_name(table_name,paste0(field_name,'-yyyy-mm-outpatient')));
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(TempOutlier(), c(table_name), 
                                                               c(field_name, 'visit_concept_id'), c(9203,'ED'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"(ED)","\n"))
  fileContent<-c(fileContent,paste_image_name(table_name,paste0(field_name,'-yyyy-mm-ED')));
  
  
  ### AA009 datetime inconsistency
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('visit_start_datetime', 
                                                                                                 'visit_start_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('visit_end_datetime', 
                                                                                                 'visit_end_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  fileContent<-c(fileContent,"##Implausible Events")
  
  ## Temporal checks --- facts before birth date
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,
                                  applyCheck(PreBirth(), c(table_name), c('visit_start_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  ## Temporal checks --- facts after death date
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(PostDeath(), c(table_name, "death"), c('visit_start_date', 
                                                                                                      'death_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  


  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(MissVisitFact(), c(table_name), NULL)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)


  ### inpatient visits with no DRG data 
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(MissVisitTypeFact(), c(table_name, 'observation'),
                                                                 c('9201','observation_concept_id','3040464'),
                                                               "inpatient visits with no DRG data"
                                                               )) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  ### inpatient visits with 
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(MissVisitTypeFact(), c(table_name, 'visit_payer'),
                                                               c('9201','visit_payer_id')
                                                               ,"inpatient visits with  no insurance data"
                                                               )) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  #print('CHECKPOINT')
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)
  
  #close the connection
  #close_database_connection_OHDSI(con,config)
}