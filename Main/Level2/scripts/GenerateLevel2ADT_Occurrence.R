library(DBI)
library(yaml)
library(dplyr)

generateLevel2ADT_Occurrence <- function () {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work
  
  big_data_flag<-TRUE
  table_name<-"adt_occurrence"
  
  
  # load the configuration file
  #get path for current script
  #config = yaml.load_file(g_config_path)
  
  
  #establish connection to database
  #con <- establish_database_connection_OHDSI(config)
  
  #con <- establish_database_connection(config)
  
  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(g_config$reporting$site_directory),"./reports/Level2_ADT_Occurrence_Report_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",g_config)
  
  log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/adt_occurrence_issue.csv",sep="")
  
  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:
 # my_db <- dbConnect(RPostgres::Postgres(),dbname=config$db$dbname,
  #                        host=config$db$dbhost,
 #                       user =config$db$dbuser,
  #                      password =config$db$dbpass, sslmode="verify-full",
  #                      options=paste("-c search_path=",config$db$schema,sep=""))
            
  #print(req_env$db_src)  
  # Then reference a tbl within that src
  adt_tbl <- cdm_tbl(req_env$db_src, 'adt_occurrence')
  

  total_adt_count<-  as.data.frame(dplyr::summarise(adt_tbl,count = n()))[1,1]
  
  ### temporal outlier check 
  field_name<-"adt_date"
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(TempOutlier(), c(table_name), 
                                                               c(field_name), NULL)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  fileContent<-c(fileContent,paste_image_name(table_name,paste0(field_name,'-yyyy-mm')));
  
  
  #total_adt_count<-  as.data.frame(summarise(adt_tbl,count = n()))[1,1]
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('adt_datetime', 
                                                                                                 'adt_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  ### DQA checkpoint --- incosnistent visit types
  log_entry_content<-(read.csv(log_file_name))
  
  log_entry_content<-custom_rbind(log_entry_content,
                                      applyCheck(InconVisitType(), 
                                                 c(table_name, "visit_occurrence"),
                                                   c("visit_occurrence_id", "visit_concept_id"),
                                                    #my_db, 
                                                   c("adt_occurrences linked to outpatient visits",
                                                   9202)
                                                 )
                                  ) 
  write.csv(log_entry_content, file = log_file_name
          ,row.names=FALSE)
  
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)
  
  #close the connection
  #close_database_connection_OHDSI(con,config)
}
