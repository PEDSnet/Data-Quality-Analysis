library(DBI)
library(yaml)
library(dplyr)

generateLevel2Measurement <- function () {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work

  table_name<-"measurement"
  big_data_flag<-TRUE

  # load the configuration file

  #establish connection to database
  #con <- establish_database_connection_OHDSI(config)

  #con <- establish_database_connection(config)

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(g_config$reporting$site_directory),"./reports/Level2_Measurement_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",g_config)

  log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/measurement_issue.csv",sep="")
  
  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:
 
  # Then reference a tbl within that src
  patient_tbl<-cdm_tbl(req_env$db_src, "person")
  measurement_tbl <- cdm_tbl(req_env$db_src, "measurement")
  death_tbl <- cdm_tbl(req_env$db_src, "death")

  concept_tbl <- vocab_tbl(req_env$db_src, 'concept')
 
  ## CA008 temporal outliers
  field_name = 'measurement_date'
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(TempOutlier(), c(table_name), 
                                                               c(field_name, 'measurement_type_concept_id'), c(2000000033,'vitals'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"vitals","\n"))
  fileContent<-c(fileContent,paste_image_name(table_name,paste0(field_name,'-yyyy-mm-vitals')));
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(TempOutlier(), c(table_name), 
                                                               c(field_name, 'measurement_type_concept_id'), c(44818702,'labs'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"(labs)","\n"))
  fileContent<-c(fileContent,paste_image_name(table_name,paste0(field_name,'-yyyy-mm-labs')));
  
  ### checking consistency between date and date/time fields. 
  
  ## AA009 check type 
  
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('measurement_datetime', 
                                                                                                 'measurement_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('measurement_order_datetime', 
                                                                                                 'measurement_order_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('measurement_result_datetime', 
                                                                                                 'measurement_result_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  lab_tbl<-select(filter(measurement_tbl, measurement_type_concept_id==44818702),measurement_id, measurement_concept_id)
  vital_tbl<-select(filter(measurement_tbl, measurement_type_concept_id==2000000033
                           |measurement_type_concept_id==2000000032),measurement_id, measurement_concept_id)
  
  
  
  ### Print top 100 no matching concept source values in measurement table 
  measurement_no_match<- select( filter(measurement_tbl, measurement_concept_id==0)
                                                             , measurement_source_value, measurement_id)
  
  #head(measurement_no_match)
  no_match_measurement_counts <-
    filter(
      dplyr::arrange(
        dplyr::summarize(
          group_by(measurement_no_match, measurement_source_value)
          , count=n())
        , desc(count))
      , row_number()>=1 & row_number()<=100) ## printing top 100
  
  
  
  df_no_match_measurement_counts<-as.data.frame(
    no_match_measurement_counts
  )

  
  #print(df_no_match_measurement_counts)
  if(nrow(df_no_match_measurement_counts)>0)
  {
  ## writing to the issue log file
  data_file<-data.frame(measurement_source_value=character(0), counts=character(0))
  
  data_file<-rbind(df_no_match_measurement_counts)
  colnames(data_file)<-c("measurement_source_value","occurrence_counts")
  write.csv(data_file, file = paste(normalize_directory_path( g_config$reporting$site_directory),
                                    "./data/no_match_measurements.csv",sep="")
            ,row.names=FALSE)
  }
  
  ##### Printing top 100 vital measurements ##### 
  #print("here")
  
  vital_tbl_enhanced<- distinct(select(inner_join(concept_tbl,vital_tbl, 
                                                  by = c("concept_id"="measurement_concept_id"))
                                           ,measurement_id, concept_id, concept_name))
  #head(vital_tbl_enhanced)
  
  vital_counts <-
    filter(
      dplyr::arrange(
        dplyr::summarize(
          group_by(vital_tbl_enhanced,concept_id)
          , occurrence_counts=n())
        , desc(occurrence_counts))
      , row_number()>=1 & row_number()<=100) ## printing top 100
  
  #head(vital_counts)
  
  df_vital_counts_all<-as.data.frame(
    dplyr::arrange(distinct(
      select(inner_join(vital_counts, vital_tbl_enhanced, 
                        by = c("concept_id"="concept_id"))
             ,concept_id, concept_name, occurrence_counts)
    ), desc(occurrence_counts)
    ))
  
  #print(df_vital_counts_all)
  #print(df_lab_counts)
  
  ## writing to the issue log file
  data_file<-data.frame(concept_id=character(0), concept_name=character(0), occurrence_counts=character(0))
  
  data_file<-rbind(df_vital_counts_all)
  colnames(data_file)<-c("concept_id", "concept_name","occurrence_counts")
  write.csv(data_file, file = paste(normalize_directory_path( g_config$reporting$site_directory),
                                    "./data/vital_measurements.csv",sep="")
            ,row.names=FALSE)
  

  ##### Printing top 100 lab measurements ##### 
  #print("here")
  
  lab_tbl_enhanced<- distinct(select(inner_join(concept_tbl,lab_tbl, by = c("concept_id"="measurement_concept_id"))
                                       ,measurement_id, concept_id, concept_name))
  #print(head(lab_tbl_enhanced))
  
  lab_counts <-
    filter(
      dplyr::arrange(
        dplyr::summarize(
          group_by(lab_tbl_enhanced, concept_id)
          , occurrence_counts=n())
        , desc(occurrence_counts))
      , row_number()>=1 & row_number()<=100) ## printing top 100
  
  #print(head(lab_counts))
  
  df_lab_counts_all<-as.data.frame(
    dplyr::arrange(distinct(
      select(inner_join(lab_counts, lab_tbl_enhanced, 
                        by = c("concept_id"="concept_id"))
             ,concept_id, concept_name, occurrence_counts)
    ), desc(occurrence_counts)
    ))
  
 # print(df_vital_counts_all)
  #print(df_lab_counts)
  
  ## writing to the issue log file
  data_file<-data.frame(concept_id=character(0), concept_name=character(0), occurrence_counts=character(0))
  
  data_file<-rbind(df_lab_counts_all)
  colnames(data_file)<-c("concept_id", "concept_name","occurrence_counts")
  write.csv(data_file, file = paste(normalize_directory_path( g_config$reporting$site_directory),
                                    "./data/lab_measurements.csv",sep="")
            ,row.names=FALSE)
  
  
  fileContent<-c(fileContent,"##Implausible Events")
  table_name<-"measurement"
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(PreBirth(), c(table_name), c('measurement_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  table_name<-"measurement"
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(PostDeath(), c(table_name, "death"), c('measurement_date', 
                                                                                                      'death_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  #close the connection
  #close_database_connection_OHDSI(con,config)
}
