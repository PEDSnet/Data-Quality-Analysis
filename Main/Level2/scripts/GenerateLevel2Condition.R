library(DBI)
library(yaml)
library(dplyr)

generateLevel2Condition <- function() {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work

  big_data_flag<-TRUE

  # load the configuration file
  #get path for current script

  #establish connection to database
  #con <- establish_database_connection_OHDSI(config)

  #con <- establish_database_connection(config)

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(g_config$reporting$site_directory),"./reports/Level2_Condition_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",g_config)

  table_name<-"condition_occurrence"
  log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/condition_occurrence_issue.csv",sep="")
  
  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:
 
  # Then reference a tbl within that src
  visit_tbl <- cdm_tbl(req_env$db_src, "visit_occurrence")
  patient_tbl<-cdm_tbl(req_env$db_src, "person")
  condition_tbl<-cdm_tbl(req_env$db_src, "condition_occurrence")
  death_tbl <- cdm_tbl(req_env$db_src, "death")

  concept_tbl <- vocab_tbl(req_env$db_src,'concept')
  #print(glimpse(concept_tbl))
  condition_concept_tbl <- select(filter(concept_tbl, domain_id=='Condition'), concept_id, concept_name)

  ### CA008 temporal outlier check 
  field_name<-"condition_start_date"
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(TempOutlier(), c(table_name), 
                                                               c(field_name), NULL)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  fileContent<-c(fileContent,paste_image_name(table_name,paste0(field_name,'-yyyy-mm')));
  
  
  ##AA009 date time inconsistency 
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('condition_start_datetime', 
                                                                                                 'condition_start_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('condition_end_datetime', 
                                                                                                 'condition_end_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconVisitType(), c(table_name, "visit_occurrence"),
                                                   c("condition_type_concept_id", "visit_concept_id"), c(
                                                     "inpatient visits linked to outpatient condition headers",
                                                     9201
                                                     , 2000000095, 2000000096, 2000000097, 2000000101, 2000000102
                                                     , 2000000103))) 
  
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
 
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconVisitType(), c(table_name, "visit_occurrence"),
                                                               c("condition_type_concept_id", "visit_concept_id"), c(
                                                                 "outpatient visits linked to inpatient condition headers",
                                                                 9202, 
                                                                 2000000092, 2000000093, 2000000094, 2000000098, 2000000099, 2000000100))) 
  
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
 #print(head(sibling_concepts_tbl))
  ### Print top 100 no matching concept source values in condition table 
  condition_no_match<- select( filter(condition_tbl, condition_concept_id==0)
                               , condition_occurrence_id, condition_source_value)
  
 
    no_match_condition_counts <-condition_no_match %>% 
    group_by(condition_source_value) %>%
    dplyr::summarise(count=n()) %>%
      dplyr::arrange (desc(count)) %>% 
    filter(row_number()>=1 & row_number()<=100)
  
  #  filter(
  #    arrange(
  #     summarize(
  #        group_by(condition_no_match, condition_source_value)
  #        , count=n(condition_occurrence_id))
  #      , desc(count))
  #    , row_number()>=1 & row_number()<=100) ## printing top 100
  
  df_no_match_condition_counts<-as.data.frame(
    no_match_condition_counts
    )
  
  ## writing to the issue log file
  data_file<-data.frame(concept_id=character(0), concept_name=character(0), condition_source_value=character(0))
  
  data_file<-rbind(df_no_match_condition_counts)
  if(nrow(df_no_match_condition_counts)>0)
  {
  colnames(data_file)<-c("condition_source_value","occurrence_counts")
  write.csv(data_file, file = paste(normalize_directory_path( g_config$reporting$site_directory),
                                    "./data/no_match_conditions.csv",sep="")
            ,row.names=FALSE)
  
  }
 #print(head(df_no_match_condition_counts))
  
  ### implementation of unexpected top inpatient conditions check 
  #filter by inpatient and outpatient visits and select visit occurrence id column
  inpatient_visit_tbl<-select(filter(visit_tbl,
                                     (visit_concept_id==9201
                                      | visit_concept_id==2000000048) & !is.na(visit_end_date)
  )
  ,visit_occurrence_id,visit_start_date, visit_end_date)


  #print(glimpse(inpatient_visit_tbl))
  
 
  
  ###### Identifying outliers in top inpatient conditions 
  #inpatient_visit_df<-as.data.frame(inpatient_visit_tbl)
  inpatient_visit_gte_2days_tbl<-select(filter(inpatient_visit_tbl, visit_end_date - visit_start_date >=2)
                                        , visit_occurrence_id)


  temp_join <- inner_join(condition_concept_tbl,condition_tbl, by = c("concept_id"="condition_concept_id"))
  condition_tbl_restricted <- select (temp_join, visit_occurrence_id, concept_id, concept_name)

  condition_visit_join_tbl <- distinct(
    select (
      inner_join(inpatient_visit_gte_2days_tbl,
                 condition_tbl_restricted,
                 by = c("visit_occurrence_id" = "visit_occurrence_id"))
      ,visit_occurrence_id, concept_id, concept_name)
  )

  #print(glimpse(condition_visit_join_tbl))
  
  condition_counts_by_visit <-
    filter(
      dplyr::arrange(
        dplyr::summarize(
          group_by(condition_visit_join_tbl, concept_id)
          , count=n_distinct(visit_occurrence_id))
        , desc(count))
      , row_number()>=1 & row_number()<=20) ## look at top 20

  #print(glimpse(condition_counts_by_visit))
  
  
  df_condition_counts_by_visit<-as.data.frame(
    select(
      inner_join(condition_counts_by_visit, condition_concept_tbl,
                 by=c("concept_id"="concept_id"))
      , concept_id, concept_name, count)
  )
  #print(nrow(df_condition_counts_by_visit))
  if(nrow(df_condition_counts_by_visit)>0)
  {
  outlier_inpatient_conditions<-applyCheck(UnexTop(),table_name,'condition_concept_id', 
                                            c(df_condition_counts_by_visit,'vt_counts','top_inpatient_conditions.csv',
                                              'outlier inpatient condition:',g_top50_inpatient_conditions_path
                                              , 'Condition'))
  
  print(outlier_inpatient_conditions)
  print(nrow(outlier_inpatient_conditions))
  
  if(nrow(outlier_inpatient_conditions)>0)
  {
  for ( issue_count in 1: nrow(outlier_inpatient_conditions))
  {
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/condition_occurrence_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-
      custom_rbind(log_entry_content,c(outlier_inpatient_conditions[issue_count,1:8]))
    
    write.csv(log_entry_content, file = log_file_name ,row.names=FALSE)
  }
  }
  }

  
  
  ### printing top 100 outpatient conditions by patient counts
  outpatient_visit_tbl<-select(filter(visit_tbl,visit_concept_id==9202)
                               ,visit_occurrence_id, person_id)
  
  #print(glipmse(outpatient_visit_tbl))
  
  ### implementation of unexpected top outpatient conditions check 
 
  
  out_condition_visit_join_tbl <- distinct(
    select (
      inner_join(outpatient_visit_tbl,
                 condition_tbl_restricted,
                 by = c("visit_occurrence_id" = "visit_occurrence_id"))
      ,person_id, concept_id, concept_name)
  )
  
  #print(glipmse(out_condition_visit_join_tbl))
  
  out_condition_counts_by_person <-
    filter(
      dplyr::arrange(
        dplyr::summarize(
          group_by(out_condition_visit_join_tbl, concept_id)
          , count=n_distinct(person_id))
        , desc(count))
      , row_number()>=1 & row_number()<=20) ## look at top 20

  #print(glipmse(out_condition_counts_by_person))

    
  df_out_condition_counts_by_person<-as.data.frame(
    select(
      inner_join(out_condition_counts_by_person, condition_concept_tbl,
                 by=c("concept_id"="concept_id"))
      , concept_id, concept_name, count)
  )
  
  if(nrow(df_out_condition_counts_by_person)>0)
  {
  outlier_outpatient_conditions<-applyCheck(UnexTop(),table_name,'condition_concept_id', 
                                           c(df_out_condition_counts_by_person,'pt_counts','top_outpatient_conditions.csv',
                                             'outlier outpatient condition:',g_top50_outpatient_conditions_path
                                             , 'Condition'))
  
  if(nrow(outlier_outpatient_conditions)>0)
  for ( issue_count in 1: nrow(outlier_outpatient_conditions))
  {
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/condition_occurrence_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-
      custom_rbind(log_entry_content,c(outlier_outpatient_conditions[issue_count,1:8]))
    
    write.csv(log_entry_content, file = log_file_name ,row.names=FALSE)
  }
  
  }
  fileContent<-c(fileContent,"##Unexpected Events")
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(PreBirth(), c(table_name), 
                                                               c('condition_start_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  

  table_name<-"condition_occurrence"
  ## Temporal checks --- facts after death date
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(PostDeath(), c(table_name, "death"), c('condition_start_date', 
                                                                                                      'death_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  


  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  #close the connection
  #close_database_connection_OHDSI(con,config)
}
