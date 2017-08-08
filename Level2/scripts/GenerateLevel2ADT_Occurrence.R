library(DBI)
library(yaml)
library(dplyr)
library(RPostgreSQL)

generateLevel2ADT_Occurrence <- function () {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work
  
  big_data_flag<-TRUE
  table_name<-"adt_occurrence"
  
  
  # load the configuration file
  #get path for current script
  config = yaml.load_file(g_config_path)
  
  
  #establish connection to database
  #con <- establish_database_connection_OHDSI(config)
  
  #con <- establish_database_connection(config)
  
  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/Level2_ADT_Occurrence_Report_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",config)
  
  log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/adt_occurrence_issue.csv",sep="")
  
  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:
  my_db <- src_postgres(dbname=config$db$dbname,
                        host=config$db$dbhost,
                        user =config$db$dbuser,
                        password =config$db$dbpass,
                        options=paste("-c search_path=",config$db$schema,sep=""))
  
  # Then reference a tbl within that src
  adt_tbl <- tbl(my_db, "adt_occurrence")
  
  total_adt_count<-  as.data.frame(summarise(adt_tbl,n = n()))[1,1]
  
  
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('adt_datetime', 
                                                                                                 'adt_date'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)
  
  #close the connection
  #close_database_connection_OHDSI(con,config)
}