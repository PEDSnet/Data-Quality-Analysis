library(DBI)
library(yaml)
library(dplyr)

generateLevel2Observation <- function () {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work

  big_data_flag<-TRUE
  table_name<-"observation"
  # load the configuration file
  #get path for current script
  #config = yaml.load_file(g_config_path)

  #establish connection to database
  #con <- establish_database_connection_OHDSI(config)

  #con <- establish_database_connection(config)

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(g_config$reporting$site_directory),"./reports/Level2_ObservationAutomatic.md",sep=""))
  fileContent <-get_report_header("Level 2",g_config)

  log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/observation_issue.csv",sep="")
  
  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:
  
            
  # Then reference a tbl within that src
  observation_tbl <- cdm_tbl(req_env$db_src, "observation")
  visit_tbl <- cdm_tbl(req_env$db_src, "visit_occurrence")
  concept_tbl <- vocab_tbl(req_env$db_src, 'concept')

  
  ### CA008 temporal outlier check 
  field_name<-"observation_date"
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(TempOutlier(), c(table_name), 
                                                               c(field_name), NULL)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  fileContent<-c(fileContent,paste_image_name(table_name,paste0(field_name,'-yyyy-mm')));
  
  ### AA009 datetime inconsistency
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('observation_datetime', 
                                                                                                 'observation_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  #print('PRINTING')
  
  #filter by inpatient and outpatient visits and select visit occurrence id column
  inpatient_visit_tbl<-select(filter(visit_tbl, visit_concept_id==9201),visit_occurrence_id)
  outpatient_visit_tbl<-select(filter(visit_tbl, visit_concept_id==9202),visit_occurrence_id)

  #fileContent<-c(fileContent,
  #               get_top_concepts(inpatient_visit_tbl,observation_tbl, 
   #                               "observation_concept_id", "observation_id", "Inpatient Observations", concept_tbl))
  #fileContent<-c(fileContent,
  #               get_top_concepts(outpatient_visit_tbl,observation_tbl, 
  #                                "observation_concept_id", "observation_id", "Outpatient Observations", concept_tbl))




  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  #close the connection
  #close_database_connection_OHDSI(con,config)
}
