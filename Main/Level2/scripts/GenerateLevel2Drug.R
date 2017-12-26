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
 
  my_db <- dbConnect(RPostgres::Postgres(),dbname=config$db$dbname,
                        host=config$db$dbhost,
                        user =config$db$dbuser,
                        password =config$db$dbpass, sslmode="verify-full",
                        options=paste("-c search_path=",config$db$schema,sep=""))
            
  # Then reference a tbl within that src
  visit_tbl <- tbl(my_db, "visit_occurrence")
  patient_tbl<-tbl(my_db, "person")
  drug_tbl <- tbl(my_db, "drug_exposure")
  death_tbl <- tbl(my_db, "death")

  concept_tbl <- tbl(my_db, dplyr::sql(paste('SELECT * FROM ', g_config$db$vocab_schema,'.concept',sep='')))
  drug_concept_tbl <- select(filter(concept_tbl, domain_id=='Drug'), concept_id, concept_name)
  drug_in_map_tbl <- tbl(my_db, dplyr::sql(paste('SELECT * FROM ',g_config$db$dqa_schema,'.drug_in_concept_id_map',sep='')))
  
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
  inpatient_visit_tbl<-select(filter(visit_tbl,
                                     (visit_concept_id==9201
                                      | visit_concept_id==2000000048) & !is.na(visit_end_date)
  )
  ,visit_occurrence_id,visit_start_date, visit_end_date)
  
  
  outpatient_visit_tbl<-select(filter(visit_tbl, visit_concept_id==9202),visit_occurrence_id, person_id)

  

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

  ###### Identifying outliers in top inpatient drugs 
  inpatient_visit_gte_2days_tbl<-select(filter(inpatient_visit_tbl, visit_end_date - visit_start_date >=2)
                                        , visit_occurrence_id)
  
  
  temp_join <- inner_join(drug_concept_tbl,drug_tbl, by = c("concept_id"="drug_concept_id"))
  
  drug_tbl_restricted <- select (temp_join, visit_occurrence_id, drug_concept_id)
  
  drug_visit_join_tbl <- distinct(
    select (
      inner_join(inpatient_visit_gte_2days_tbl,
                 drug_tbl_restricted,
                 by = c("visit_occurrence_id" = "visit_occurrence_id"))
      ,visit_occurrence_id, drug_concept_id)
  )
  
  drug_ingredient_visit_join_tbl<- distinct(
    select (
      inner_join(drug_visit_join_tbl,
                 drug_in_map_tbl,
                 by = c("drug_concept_id" = "drug_concept_id"))
      ,visit_occurrence_id, in_concept_id, in_concept_name)
  )
  
  drug_counts_by_visit <-
    filter(
      arrange(
        summarize(
          group_by(drug_ingredient_visit_join_tbl, in_concept_id)
          , count=n())
        , desc(count))
      , row_number()>=1 & row_number()<=20) ## look at top 20
  
  df_drug_counts_by_visit<-as.data.frame(
    select(
      inner_join(drug_counts_by_visit, concept_tbl,
                 by=c("in_concept_id"="concept_id"))
      , in_concept_id, concept_name, count)
  )
  print(df_drug_counts_by_visit)
  
  outlier_inpatient_drugs<-applyCheck(UnexTop(),table_name,'drug_concept_id',my_db, 
                                           c(df_drug_counts_by_visit,'vt_counts','top_inpatient_drugs.csv',
                                             'outlier inpatient drug (ingredient-level):',g_top50_inpatient_drugs_path
                                             , 'Drug'))
  
  if(nrow(outlier_inpatient_drugs)>0)
  for ( issue_count in 1: nrow(outlier_inpatient_drugs))
  {
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/drug_exposure_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-
      custom_rbind(log_entry_content,c(outlier_inpatient_drugs[issue_count,1:8]))
    
    write.csv(log_entry_content, file = log_file_name ,row.names=FALSE)
  }
  
  
  ### outlier outpatient drugs 
  outpatient_visit_tbl<-select(filter(visit_tbl,visit_concept_id==9202)
                               ,visit_occurrence_id, person_id)
  
  drug_visit_join_tbl <- distinct(
    select (
      inner_join(outpatient_visit_tbl,
                 drug_tbl_restricted,
                 by = c("visit_occurrence_id" = "visit_occurrence_id"))
      ,person_id, drug_concept_id)
  )
  
  drug_ingredient_visit_join_tbl<- distinct(
    select (
      inner_join(drug_visit_join_tbl,
                 drug_in_map_tbl,
                 by = c("drug_concept_id" = "drug_concept_id"))
      ,person_id, in_concept_id, in_concept_name)
  )
  
  drug_counts_by_person <-
    filter(
      arrange(
        summarize(
          group_by(drug_ingredient_visit_join_tbl, in_concept_id)
          , count=n())
        , desc(count))
      , row_number()>=1 & row_number()<=20) ## look at top 20
  
  df_drug_counts_by_person<-as.data.frame(
    select(
      inner_join(drug_counts_by_person, concept_tbl,
                 by=c("in_concept_id"="concept_id"))
      , in_concept_id, concept_name, count)
  )
 # print(df_drug_counts_by_visit)
  
  outlier_outpatient_drugs<-applyCheck(UnexTop(),table_name,'drug_concept_id',my_db, 
                                      c(df_drug_counts_by_person,'pt_counts','top_outpatient_drugs.csv',
                                        'outlier outpatient drug (ingredient-level):',g_top50_outpatient_drugs_path
                                        , 'Drug'))
  
  if( nrow(outlier_outpatient_drugs)>0)
  for ( issue_count in 1: nrow(outlier_outpatient_drugs))
  {
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),
                         "./issues/drug_exposure_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-
      custom_rbind(log_entry_content,c(outlier_outpatient_drugs[issue_count,1:8]))
    
    write.csv(log_entry_content, file = log_file_name ,row.names=FALSE)
  }

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
