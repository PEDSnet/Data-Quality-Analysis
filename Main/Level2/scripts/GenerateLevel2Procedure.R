library(DBI)
library(yaml)
library(dplyr)

generateLevel2Procedure <- function () {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work
  
  big_data_flag<-TRUE
  table_name<-"procedure_occurrence"
  # load the configuration file
  #get path for current script
  #config = yaml.load_file(g_config_path)
  log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/procedure_occurrence_issue.csv",sep="")
  
  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(g_config$reporting$site_directory),"./reports/Level2_Procedure_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",g_config)
  
  
  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:
 
             
  # Then reference a tbl within that src
  visit_tbl <- cdm_tbl(req_env$db_src, "visit_occurrence")
  patient_tbl<-cdm_tbl(req_env$db_src, "person")
  procedure_tbl<-cdm_tbl(req_env$db_src, "procedure_occurrence")
  death_tbl <- cdm_tbl(req_env$db_src, "death")
  
  concept_tbl <- vocab_tbl(req_env$db_src, 'concept')
  concept_ancestor_tbl <- vocab_tbl(req_env$db_src, 'concept_ancestor')
  procedure_concept_tbl <- select(filter(concept_tbl, domain_id=='Procedure'), concept_id, concept_name)
  
  ### CA008 temporal outlier check 
  field_name<-"procedure_date"
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(TempOutlier(), c(table_name), 
                                                               c(field_name), NULL)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  fileContent<-c(fileContent,paste_image_name(table_name,paste0(field_name,'-yyyy-mm')));
  
  
  ### AA009 date time inconsistency 
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('procedure_datetime', 
                                                                                                 'procedure_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  ## sibling concepts 
  procedure_concept_ancestor_tbl<-  inner_join(concept_ancestor_tbl, procedure_concept_tbl, 
                                               by = c("ancestor_concept_id" = "concept_id"))
  
  temp1<-inner_join(procedure_concept_ancestor_tbl, procedure_concept_ancestor_tbl, 
                    by =c("ancestor_concept_id"="ancestor_concept_id"))
  temp2<-filter(temp1
                , max_levels_of_separation.x==1 & max_levels_of_separation.y==1)
  #print(head(temp2))
  sibling_concepts_tbl<-
    (select (temp2,
             descendant_concept_id.x, descendant_concept_id.y)
    )
  
  table_name<-"procedure_occurrence"
  
  ### Print top 100 no matching concept source values in procedure table 
  procedure_no_match<- select( filter(procedure_tbl, procedure_concept_id==0)
                               , procedure_source_value, procedure_occurrence_id)
  
  no_match_procedure_counts <-
    filter(
      dplyr::arrange(
        dplyr::summarize(
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
  
  ### implementation of unexpected top inpatient procedures check 
  #filter by inpatient and outpatient visits and select visit occurrence id column
  inpatient_visit_tbl<-select(filter(visit_tbl,
                                     (visit_concept_id==9201
                                      | visit_concept_id==2000000048) & !is.na(visit_end_date)
  )
  ,visit_occurrence_id,visit_start_date, visit_end_date)
 
 
  ###### Identifying outliers in top inpatient procedures 
  #inpatient_visit_df<-as.data.frame(inpatient_visit_tbl)
  inpatient_visit_gte_2days_tbl<-select(filter(inpatient_visit_tbl, visit_end_date - visit_start_date >=2)
                                        , visit_occurrence_id)
  
  
  temp_join <- inner_join(procedure_concept_tbl,procedure_tbl, by = c("concept_id"="procedure_concept_id"))
  
  procedure_tbl_restricted <- select (temp_join, visit_occurrence_id, concept_id, concept_name)
  
  procedure_visit_join_tbl <- distinct(
    select (
      inner_join(inpatient_visit_gte_2days_tbl,
                 procedure_tbl_restricted,
                 by = c("visit_occurrence_id" = "visit_occurrence_id"))
      ,visit_occurrence_id, concept_id, concept_name)
  )
  
  #print(head(procedure_visit_join_tbl))
  
 # main_tbl<-metadata[1]
  #print(head(main_tbl))
  procedure_counts_by_visit <-
    filter(
      dplyr::arrange(
        dplyr::summarize(
          group_by(procedure_visit_join_tbl, concept_id)
          , count=n())
        , desc(count))
      , row_number()>=1 & row_number()<=20) ## look at top 20
  
  df_procedure_counts_by_visit<-as.data.frame(
    select(
      inner_join(procedure_counts_by_visit, procedure_concept_tbl,
                 by=c("concept_id"="concept_id"))
      , concept_id, concept_name, count)
  )
  #print(df_procedure_counts_by_visit)
  
     if(nrow(df_procedure_counts_by_visit)>0)
     {
    outlier_inpatient_procedures<-applyCheck(UnexTop(),table_name,'procedure_concept_id', 
                                           c(df_procedure_counts_by_visit,'vt_counts',
                                             'top_inpatient_procedure.csv',
                                             'outlier inpatient procedure:',
                                             g_top50_inpatient_procedures_path
                                             , 'Procedure'))
     
  
    if(nrow(outlier_inpatient_procedures)>0)
    for ( issue_count in 1: nrow(outlier_inpatient_procedures))
    {
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/procedure_occurrence_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-
    custom_rbind(log_entry_content,c(outlier_inpatient_procedures[issue_count,1:8]))
                  
    write.csv(log_entry_content, file = log_file_name ,row.names=FALSE)
    }
  
     }
  ### implementation of unexpected top outpatient procedures check 
  outpatient_visit_tbl<-select(filter(visit_tbl,visit_concept_id==9202)
                               ,visit_occurrence_id, person_id)
  
  
  out_procedure_visit_join_tbl <- distinct(
    select (
      inner_join(outpatient_visit_tbl,
                 procedure_tbl_restricted,
                 by = c("visit_occurrence_id" = "visit_occurrence_id"))
      ,person_id, concept_id, concept_name)
  )
  
  
  out_procedure_counts_by_person <-
    filter(
      dplyr::arrange(
        dplyr::summarize(
          group_by(out_procedure_visit_join_tbl, concept_id)
          , count=n())
        , desc(count))
      , row_number()>=1 & row_number()<=20) ## look at top 20
  
  df_out_procedure_counts_by_person<-as.data.frame(
    select(
      inner_join(out_procedure_counts_by_person, procedure_concept_tbl,
                 by=c("concept_id"="concept_id"))
      , concept_id, concept_name, count)
  )
  
  if(nrow(df_out_procedure_counts_by_person)>0)
  {
  outlier_outpatient_procedures<-applyCheck(UnexTop(),table_name,'procedure_concept_id', 
                                           c(df_out_procedure_counts_by_person,'pt_counts','top_outpatient_procedure.csv',
                                             'outlier outpatient procedure:',g_top50_outpatient_procedures_path
                                             , 'Procedure'))
  
  if(nrow(outlier_outpatient_procedures)>0)
  for ( issue_count in 1: nrow(outlier_outpatient_procedures))
  {
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/procedure_occurrence_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-
      custom_rbind(log_entry_content,c(outlier_outpatient_procedures[issue_count,1:8]))
    
    write.csv(log_entry_content, file = log_file_name ,row.names=FALSE)
  }
  
  }
  
  fileContent<-c(fileContent,"##Implausible Events")
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(PreBirth(), c(table_name), c('procedure_date')
                                                               )) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  table_name<-"procedure_occurrence"
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(PostDeath(), c(table_name, "death"), c('procedure_date', 
                                                                                                      'death_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)
  
  #close the connection
  #close_database_connection_OHDSI(con,config)
}
