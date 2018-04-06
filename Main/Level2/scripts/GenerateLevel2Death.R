library(DBI)
library(yaml)
library(dplyr)

generateLevel2Death <- function () {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work
  
  big_data_flag<-TRUE
  table_name<-"death"
  
  
  # load the configuration file
  #get path for current script
  #config = yaml.load_file(g_config_path)
  
  #my_db <- dbConnect(RPostgres::Postgres(),dbname=config$db$dbname,
  #                   host=config$db$dbhost,
  #                   user =config$db$dbuser,
  #                   password =config$db$dbpass, sslmode="verify-full",
  #                   options=paste("-c search_path=",config$db$schema,sep=""))
  
  #establish connection to database
  #con <- establish_database_connection_OHDSI(config)
  
  #con <- establish_database_connection(config)
  
  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(g_config$reporting$site_directory),"./reports/Level2_Death_Report_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",g_config)
  
  log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/death_issue.csv",sep="")
  
  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:

  # Then reference a tbl within that src
  #death_tbl <- tbl(my_db, "death")
  death_tbl <- cdm_tbl(req_env$db_src, "death")
  
  
  total_death_count<-  as.data.frame(dplyr::summarise(death_tbl,n = n()))[1,1]

  
  ##AA009 date time consistency
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('death_datetime', 
                                                                                                 'death_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)
  
  #close the connection
  #close_database_connection_OHDSI(con,config)
}