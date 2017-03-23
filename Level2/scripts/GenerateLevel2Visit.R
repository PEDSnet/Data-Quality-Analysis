library(DBI)
library(yaml)
library(dplyr)
library(RPostgreSQL)

generateLevel2Visit <- function () {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work
  
  big_data_flag<-TRUE
  
  # load the configuration file
  #get path for current script
  config = yaml.load_file(g_config_path)
  
  
  #establish connection to database
  #con <- establish_database_connection_OHDSI(config)
  
  #con <- establish_database_connection(config)
  
  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/Level2_Visit_Report_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",config)
  
  
  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:
  my_db <- src_postgres(dbname=config$db$dbname,
                        host=config$db$dbhost,
                        user =config$db$dbuser,
                        password =config$db$dbpass,
                        options=paste("-c search_path=",config$db$schema,sep=""))
  
  # Then reference a tbl within that src
  visit_tbl <- tbl(my_db, "visit_occurrence")
  
  total_visit_count<-  as.data.frame(summarise(visit_tbl,n = n()))[1,1]
  
  patient_tbl<-tbl(my_db, "person")
  condition_tbl<-tbl(my_db, "condition_occurrence")
  procedure_tbl<-tbl(my_db, "procedure_occurrence")
  drug_tbl<-tbl(my_db, "drug_exposure")
  measurement_tbl<-tbl(my_db, "measurement")
  
  death_tbl <- tbl(my_db, "death")
  
  concept_tbl <- tbl(my_db, dplyr::sql(paste('SELECT * FROM ',config$db$vocab_schema,'.concept',sep='')))
  
  patient_dob_tbl <- tbl(my_db, dplyr::sql
                         ('SELECT person_id, to_date(year_of_birth||\'-\'||month_of_birth||\'-\'||day_of_birth,\'YYYY-MM-DD\') as dob FROM person'))
  
  
  
  fileContent<-c(fileContent,"##Implausible Events")
  
  table_name<-"visit_occurrence"
  log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/visit_occurrence_issue.csv",sep="")
  
  ## Temporal checks --- facts before birth date
  df_visit_before_dob<-as.data.frame(
    select(
      filter(inner_join(visit_tbl,patient_dob_tbl, by =c("person_id"="person_id")),visit_start_date<dob)
      ,person_id, dob, visit_start_date
    ))
  
  if(nrow(df_visit_before_dob)>0)
  {
  df_visit_prenatal<-subset(df_visit_before_dob,elapsed_months(dob,visit_start_date)<=9)
  message<-paste(nrow(df_visit_before_dob)
                 ,"visits before birth ( including",nrow(df_visit_prenatal),"prenatal visits)");
  fileContent<-c(fileContent,"\n",message)
  ### open the person log file for appending purposes.
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,
                                  apply_check_type_2_diff_tables('CA-003',"Person","time_of_birth","visit_occurrence", "visit_start_date", message)
  )
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  }
  
  
  
  
  ## Temporal checks --- facts after death date
  df_visit_after_death<-as.data.frame(
    select(
      filter(inner_join(visit_tbl,death_tbl, by =c("person_id"="person_id")),visit_start_date>death_date)
      ,person_id
    ))
  ## Temporal checks --- facts after death date
  
  if(nrow(df_visit_after_death)>0)
  {
      message<-paste(nrow(df_visit_after_death),"visits after death")
      fileContent<-c(fileContent,"\n",message)
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/visit_occurrence_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-custom_rbind(log_entry_content,
                                    apply_check_type_2_diff_tables('CA-004',"Death","death_date","visit_occurrence", "visit_start_date", message)
    )
    write.csv(log_entry_content, file = log_file_name
              ,row.names=FALSE)
  }
  
  
  ### % of visits with no facts associated. 
  ## step 1 print # visits in 9202 , 9201, and 9203
  key_visits<-select(filter(visit_tbl, visit_concept_id==9201| visit_concept_id==9202|visit_concept_id==9203)
                     , visit_occurrence_id)
  total_key_visits<-nrow(key_visits)
  
  ## step 2 get  key visits that dont have any associated facts 
  temp<-union(select(condition_tbl, visit_occurrence_id), select(procedure_tbl, visit_occurrence_id), 
              select(measurement_tbl, visit_occurrence_id),   select(drug_tbl, visit_occurrence_id))
  
  ## step 
  result<-setdiff(key_visits,temp)
  final_result<-summarize(result, n=n())
  key_visits_without_facts<-as.data.frame(final_result)[1,1]
  ## step 3 get % of visits that dont have any facts and are key visits. 
  no_fact_percentage<-((key_visits_without_facts)*100/total_visit_count)
  print(no_fact_percentage)
  
  ## write to log file 
  fileContent<-c(fileContent,paste(no_fact_percentage,"% visits have no facts assoicated with them and fall under the 9201, 9202 or 9303 visit type"
                                   , sep=""))
  
  
  ### open the person log file for appending purposes.
  log_entry_content<-custom_rbind(log_entry_content,
                                  apply_check_type_0('BA-004', no_fact_percentage, table_name, g_data_version))
  
  write.csv(log_entry_content,file=log_file_name,row.names=FALSE)
  
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)
  
  #close the connection
  #close_database_connection_OHDSI(con,config)
}