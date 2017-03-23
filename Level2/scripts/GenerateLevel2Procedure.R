library(DBI)
library(yaml)
library(dplyr)
library(RPostgreSQL)


generateLevel2Procedure <- function () {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work

  big_data_flag<-TRUE

  # load the configuration file
  #get path for current script
  config = yaml.load_file(g_config_path)

  #establish connection to database
  #con <- establish_database_connection_OHDSI(config)

  #con <- establish_database_connection(config)

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/Level2_Procedure_Automatic.md",sep=""))
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
  observation_tbl <- tbl(my_db, "observation")
  visit_tbl <- tbl(my_db, "visit_occurrence")
  patient_tbl<-tbl(my_db, "person")
  condition_tbl<-tbl(my_db, "condition_occurrence")
  procedure_tbl<-tbl(my_db, "procedure_occurrence")
  drug_tbl <- tbl(my_db, "drug_exposure")
  measurement_tbl <- tbl(my_db, "measurement")
  death_tbl <- tbl(my_db, "death")

  concept_tbl <- tbl(my_db, dplyr::sql(paste('SELECT * FROM ',config$db$vocab_schema,'.concept',sep='')))

  patient_dob_tbl <- tbl(my_db, dplyr::sql
                         ('SELECT person_id, to_date(year_of_birth||\'-\'||month_of_birth||\'-\'||day_of_birth,\'YYYY-MM-DD\') as dob FROM person'))


  #filter by inpatient and outpatient visits and select visit occurrence id column
  inpatient_visit_tbl<-select(filter(visit_tbl, visit_concept_id==9201),visit_occurrence_id)
  outpatient_visit_tbl<-select(filter(visit_tbl, visit_concept_id==9202),visit_occurrence_id)

  fileContent<-c(fileContent,
                 get_top_concepts(inpatient_visit_tbl,procedure_tbl, "procedure_concept_id", "procedure_occurrence_id", "Inpatient Procedures", concept_tbl))
  fileContent<-c(fileContent,
                 get_top_concepts(outpatient_visit_tbl,procedure_tbl, "procedure_concept_id", "procedure_occurrence_id", "Outpatient Procedures", concept_tbl))





  table_name<-"procedure_occurrence"
  
  ### Print top 100 no matching concept source values in procedure table 
  procedure_no_match<- select( filter(procedure_tbl, procedure_concept_id==0)
                               , procedure_source_value)
  
  no_match_procedure_counts <-
    filter(
      arrange(
        summarize(
          group_by(procedure_no_match, procedure_source_value)
          , count=n())
        , desc(count))
      , row_number()>=1 & row_number()<=100) ## printing top 100
  
  df_no_match_procedure_counts<-as.data.frame(
    no_match_procedure_counts
  )
  
  if(nrow(df_no_match_procedure_counts)>0)
  {
  ## writing to the issue log file
  data_file<-data.frame(concept_id=character(0), concept_name=character(0), procedure_source_value=character(0))
  
  data_file<-rbind(df_no_match_procedure_counts)
  colnames(data_file)<-c("procedure_source_value","occurrence_counts")
  write.csv(data_file, file = paste(normalize_directory_path( g_config$reporting$site_directory),
                                    "./data/no_match_procedures.csv",sep="")
            ,row.names=FALSE)
  }
  ##### Printing top 100 inpatient procedures ##### 
  procedure_tbl_enhanced<- distinct(select(inner_join(concept_tbl,procedure_tbl, by = c("concept_id"="procedure_concept_id"))
                                           , visit_occurrence_id, procedure_concept_id, concept_name))
  #print(head(condition_tbl_enhanced))
  #print(head(inpatient_visit_tbl))
  inpatient_visit_procedures<-
    distinct(select(
      inner_join(procedure_tbl_enhanced, 
                 inpatient_visit_tbl)#, by =c("visit_occurrence_id", "visit_occurrence_id"))
      ,visit_occurrence_id,concept_name, procedure_concept_id))
  
  procedure_counts_by_visit <-
    filter(
      arrange(
        summarize(
          group_by(inpatient_visit_procedures, procedure_concept_id)
          , visit_count=n())
        , desc(visit_count))
      , row_number()>=1 & row_number()<=100) ## printing top 100
  
  df_procedure_counts_by_visit_all<-as.data.frame(
    arrange(distinct(
      select(inner_join(procedure_counts_by_visit, procedure_tbl_enhanced, 
                        by = c("procedure_concept_id"="procedure_concept_id"))
             ,procedure_concept_id, concept_name, visit_count)
    ), desc(visit_count)
    ))
  
  #print(df_condition_counts_by_visit_all)
  
  ## writing to the issue log file
  data_file<-data.frame(concept_id=character(0), concept_name=character(0), visit_counts=character(0))
  
  data_file<-rbind(df_procedure_counts_by_visit_all)
  colnames(data_file)<-c("concept_id", "concept_name","visit_counts")
  write.csv(data_file, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./data/top_inpatient_procedures.csv",sep="")
            ,row.names=FALSE)
  
  
  ### printing top 100 outpatient procedures by patient counts
  outpatient_visit_tbl<-select(filter(visit_tbl,visit_concept_id==9202)
                               ,visit_occurrence_id, person_id)
  
  outpatient_visit_procedures<-
    distinct(select(
      inner_join(procedure_tbl_enhanced, 
                 outpatient_visit_tbl)#, by =c("visit_occurrence_id", "visit_occurrence_id"))
      ,person_id,concept_name, procedure_concept_id))
  
  procedure_counts_by_patients <-
    filter(
      arrange(
        summarize(
          group_by(outpatient_visit_procedures, procedure_concept_id)
          , pt_count=n())
        , desc(pt_count))
      , row_number()>=1 & row_number()<=100) ## printing top 100
  
  df_procedure_counts_by_patients_all<-as.data.frame(
    arrange(distinct(
      select(inner_join(procedure_counts_by_patients, procedure_tbl_enhanced, 
                        by = c("procedure_concept_id"="procedure_concept_id"))
             ,procedure_concept_id, concept_name, pt_count)
    ), desc(pt_count)
    ))
  
  #print(df_condition_counts_by_visit_all)
  
  ## writing to the issue log file
  data_file<-data.frame(concept_id=character(0), concept_name=character(0), pt_counts=character(0))
  
  data_file<-rbind(df_procedure_counts_by_patients_all)
  colnames(data_file)<-c("concept_id", "concept_name","pt_counts")
  write.csv(data_file, file = paste(normalize_directory_path( g_config$reporting$site_directory),
                                    "./data/top_outpatient_procedures.csv",sep="")
            ,row.names=FALSE)
  
  
  fileContent<-c(fileContent,"##Implausible Events")
  
  df_proc_before_dob<-as.data.frame(
    select(
      filter(inner_join(procedure_tbl,patient_dob_tbl, by =c("person_id"="person_id")),procedure_date<dob)
      ,person_id, dob, procedure_date
    ))
  if(nrow(df_proc_before_dob)>0)
  {
    df_proc_prenatal<-subset(df_proc_before_dob,elapsed_months(dob,procedure_date)<=9)
    message<-paste(nrow(df_proc_before_dob)
                   ,"procedures before birth ( including",nrow(df_proc_prenatal),"prenatal procedures)")
    fileContent<-c(fileContent,"\n",message)
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/procedure_occurrence_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-custom_rbind(log_entry_content,
                                    apply_check_type_2_diff_tables('CA-003',"Person","time_of_birth",table_name, "procedure_date", message)
          )
    write.csv(log_entry_content, file = log_file_name
              ,row.names=FALSE)
  }


  table_name<-"procedure_occurrence"
  df_proc_after_death<-as.data.frame(
    select(
      filter(inner_join(procedure_tbl,death_tbl, by =c("person_id"="person_id")),procedure_date>death_date)
      ,person_id
    ))

  if(nrow(df_proc_after_death)>0)
  {
    message<-paste(nrow(df_proc_after_death),"procedures after death")
    fileContent<-c(fileContent,"\n",message)
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/procedure_occurrence_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-custom_rbind(log_entry_content,
                                    apply_check_type_2_diff_tables('CA-004',"Death","death_date",table_name, "procedure_date", message)
    )
    write.csv(log_entry_content, file = log_file_name
              ,row.names=FALSE)
  }


  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  #close the connection
  #close_database_connection_OHDSI(con,config)
}
