library(DBI)
library(yaml)
library(dplyr)
library(RPostgreSQL)

generateLevel2Drug <- function() {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work

  big_data_flag<-TRUE

  table_name<-"drug_exposure"
  # load the configuration file
  #get path for current script

  log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/drug_exposure_issue.csv",sep="")
  
  #g_data_version<-paste("pedsnet-2.3.0-", g_config$reporting$site,"-ETLv", g_config$reporting$etl_script_version, sep="")

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/Level2_Drug_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2", g_config)


  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:
  my_db <- src_postgres(dbname= g_config$db$dbname,
                        host= g_config$db$dbhost,
                        user = g_config$db$dbuser,
                        password = g_config$db$dbpass,
                        options=paste("-c search_path=", g_config$db$schema,sep=""))

  # Then reference a tbl within that src
  visit_tbl <- tbl(my_db, "visit_occurrence")
  patient_tbl<-tbl(my_db, "person")
  condition_tbl<-tbl(my_db, "condition_occurrence")
  drug_tbl <- tbl(my_db, "drug_exposure")
  death_tbl <- tbl(my_db, "death")

  concept_tbl <- tbl(my_db, dplyr::sql(paste('SELECT * FROM ', g_config$db$vocab_schema,'.concept',sep='')))

  patient_dob_tbl <- tbl(my_db, dplyr::sql
                         ('SELECT person_id, to_date(year_of_birth||\'-\'||month_of_birth||\'-\'||day_of_birth,\'YYYY-MM-DD\') as dob FROM person'))


  ##AA009 date time inconsistency 
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('drug_exposure_start_datetime', 
                                                                                                 'drug_exposure_start_date'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('drug_exposure_end_datetime', 
                                                                                                 'drug_exposure_end_date'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('drug_exposure_order_datetime', 
                                                                                                 'drug_exposure_order_date'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  #filter by inpatient and outpatient visits and select visit occurrence id column
  inpatient_visit_tbl<-select(filter(visit_tbl, visit_concept_id==9201),visit_occurrence_id)
  outpatient_visit_tbl<-select(filter(visit_tbl, visit_concept_id==9202),visit_occurrence_id)

  

  fileContent<-c(fileContent,
                 get_top_concepts(inpatient_visit_tbl,drug_tbl, "drug_concept_id", "drug_exposure_id", "Inpatient Medications", concept_tbl))
  fileContent<-c(fileContent,
                 get_top_concepts(outpatient_visit_tbl,drug_tbl, "drug_concept_id", "drug_exposure_id", "Outpatient Medications", concept_tbl))


  ### Print top 100 no matching concept source values in drug table 
  drug_no_match<- select( filter(drug_tbl, drug_concept_id==0)
                               , drug_source_value)
  
  no_match_drug_counts <-
    filter(
      arrange(
        summarize(
          group_by(drug_no_match, drug_source_value)
          , count=n())
        , desc(count))
      , row_number()>=1 & row_number()<=100) ## printing top 100
  
  df_no_match_drug_counts<-as.data.frame(
    no_match_drug_counts
  )
  
  if(nrow(df_no_match_drug_counts)>0)
  {
  ## writing to the issue log file
  data_file<-data.frame(concept_id=character(0), concept_name=character(0), drug_source_value=character(0))
  
  data_file<-rbind(df_no_match_drug_counts)
  colnames(data_file)<-c("drug_source_value","occurrence_counts")
  write.csv(data_file, file = paste(normalize_directory_path( g_config$reporting$site_directory),
                                    "./data/no_match_drugs.csv",sep="")
            ,row.names=FALSE)
  }
  ##### Printing top 100 inpatient drugs ##### 
  drug_tbl_enhanced<- distinct(select(inner_join(concept_tbl,drug_tbl, by = c("concept_id"="drug_concept_id"))
                                           , visit_occurrence_id, drug_concept_id, concept_name))
  #print(head(condition_tbl_enhanced))
  #print(head(inpatient_visit_tbl))
  inpatient_visit_drugs<-
    distinct(select(
      inner_join(drug_tbl_enhanced, 
                 inpatient_visit_tbl)#, by =c("visit_occurrence_id", "visit_occurrence_id"))
      ,visit_occurrence_id,concept_name, drug_concept_id))
  
  drug_counts_by_visit <-
    filter(
      arrange(
        summarize(
          group_by(inpatient_visit_drugs, drug_concept_id)
          , visit_count=n())
        , desc(visit_count))
      , row_number()>=1 & row_number()<=100) ## printing top 100
  
  df_drug_counts_by_visit_all<-as.data.frame(
    arrange(distinct(
      select(inner_join(drug_counts_by_visit, drug_tbl_enhanced, 
                        by = c("drug_concept_id"="drug_concept_id"))
             ,drug_concept_id, concept_name, visit_count)
    ), desc(visit_count)
    ))
  
  #print(df_condition_counts_by_visit_all)
  
  ## writing to the issue log file
  data_file<-data.frame(concept_id=character(0), concept_name=character(0), visit_counts=character(0))
  
  data_file<-rbind(df_drug_counts_by_visit_all)
  colnames(data_file)<-c("concept_id", "concept_name","visit_counts")
  write.csv(data_file, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./data/top_inpatient_drugs.csv",sep="")
            ,row.names=FALSE)
  
  
  ### printing top 100 outpatient drugs by patient counts
  outpatient_visit_tbl<-select(filter(visit_tbl,visit_concept_id==9202)
                               ,visit_occurrence_id, person_id)
  
  outpatient_visit_drugs<-
    distinct(select(
      inner_join(drug_tbl_enhanced, 
                 outpatient_visit_tbl)#, by =c("visit_occurrence_id", "visit_occurrence_id"))
      ,person_id,concept_name, drug_concept_id))
  
  drug_counts_by_patients <-
    filter(
      arrange(
        summarize(
          group_by(outpatient_visit_drugs, drug_concept_id)
          , pt_count=n())
        , desc(pt_count))
      , row_number()>=1 & row_number()<=100) ## printing top 100
  
  df_drug_counts_by_patients_all<-as.data.frame(
    arrange(distinct(
      select(inner_join(drug_counts_by_patients, drug_tbl_enhanced, 
                        by = c("drug_concept_id"="drug_concept_id"))
             ,drug_concept_id, concept_name, pt_count)
    ), desc(pt_count)
    ))
  
  #print(df_condition_counts_by_visit_all)
  
  ## writing to the issue log file
  data_file<-data.frame(concept_id=character(0), concept_name=character(0), pt_counts=character(0))
  
  data_file<-rbind(df_drug_counts_by_patients_all)
  colnames(data_file)<-c("concept_id", "concept_name","pt_counts")
  write.csv(data_file, file = paste(normalize_directory_path( g_config$reporting$site_directory),
                                    "./data/top_outpatient_drugs.csv",sep="")
            ,row.names=FALSE)
  
  


  fileContent<-c(fileContent,"##Implausible Events")


  table_name<-"drug_exposure"
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(PreBirth(), c(table_name, "person"), c('drug_exposure_start_date', 
                                                                                                      'birth_datetime'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  table_name<-"drug_exposure"
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(PostDeath(), c(table_name, "death"), c('drug_exposure_start_date', 
                                                                                                      'death_date'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  #close the connection
  #close_database_connection_OHDSI(con, g_config)
}
