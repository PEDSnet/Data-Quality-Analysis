library(DBI)
library(yaml)
library(dplyr)
library(RPostgreSQL)

generateLevel2Patient <- function() {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work
  
  big_data_flag<-TRUE
  
  config = yaml.load_file(g_config_path)
  
  #establish connection to database
  #con <- establish_database_connection_OHDSI(config)
  
  #con <- establish_database_connection(config)
  
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
  observation_period_tbl <- tbl(my_db, "observation_period")
  visit_tbl <- tbl(my_db, "visit_occurrence")
  patient_tbl<-tbl(my_db, "person")
  condition_tbl<-tbl(my_db, "condition_occurrence")
  
  concept_tbl <- tbl(my_db, dplyr::sql('SELECT * FROM vocabulary.concept'))
  
  patient_dob_tbl <- tbl(my_db, dplyr::sql
                         ('SELECT person_id, to_date(year_of_birth||\'-\'||month_of_birth||\'-\'||day_of_birth,\'YYYY-MM-DD\') as dob FROM person'))
  
  ## log file
  log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/person_issue.csv",sep="")
  log_entry_content<-(read.csv(log_file_name))
  
  table_name<-"person"
  ## total patients
  all_patients<-select(patient_tbl,person_id)
  
  ## Patients  satisfying inclusion criteria
  valid_patients_by_visit<-select(filter(visit_tbl,visit_start_date >='2009-01-01'
                                  & (visit_concept_id ==9201
                                    |visit_concept_id== 9202
                                    |visit_concept_id== 9203
                                    |visit_concept_id== 42898160
                                    |visit_concept_id==44814710
                                    |visit_concept_id==2000000048)
                                    ), person_id
                                  )
  
  valid_patients_by_condition<-select(filter(condition_tbl,condition_start_date >='2009-01-01'),person_id)
  
  #patients not satisfying inclusion criteria
  invalid_patients<-setdiff(all_patients,intersect(valid_patients_by_visit,valid_patients_by_condition))
  df_invalid_patients<-as.data.frame(invalid_patients)
  fileContent<-c(fileContent,"##Inclusion Criteria Check")
  
  if(nrow(df_invalid_patients)>0)
  {
    fileContent<-c(fileContent,paste(nrow(df_invalid_patients)," patients found outside the inclusion criteria"))
  
    ### open the person log file for appending purposes.
    log_entry_content<-custom_rbind(log_entry_content,
                              apply_check_type_0('AA-006', nrow(df_invalid_patients), table_name, g_data_version))
  
    write.csv(log_entry_content,file=log_file_name,row.names=FALSE)
  }
  
  # difference between observation period and person table
  df_result<-as.data.frame(setdiff(all_patients,select(observation_period_tbl,person_id)))
  if(nrow(df_result)>0)
  {
    fileContent<-c("\n",fileContent,paste(nrow(df_result)," different person_ids found between person and observation_period"))
    ### open the person log file for appending purposes.
    log_entry_content<-custom_rbind(log_entry_content,apply_check_type_0('AA-006',
                                          nrow(df_result), table_name, g_data_version)
    )
    write.csv(log_entry_content,file=log_file_name,row.names=FALSE)
  
    }
  
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)
  
  #close the connection
  #close_database_connection_OHDSI(con,config)
}
