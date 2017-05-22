library(DBI)
library(yaml)
library(dplyr)
library(RPostgreSQL)

generateLevel2Death <- function () {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work
  
  big_data_flag<-TRUE
  table_name<-"death"
  
  
  # load the configuration file
  #get path for current script
  config = yaml.load_file(g_config_path)
  
  
  #establish connection to database
  #con <- establish_database_connection_OHDSI(config)
  
  #con <- establish_database_connection(config)
  
  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/Level2_Death_Report_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",config)
  
  
  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:
  my_db <- src_postgres(dbname=config$db$dbname,
                        host=config$db$dbhost,
                        user =config$db$dbuser,
                        password =config$db$dbpass,
                        options=paste("-c search_path=",config$db$schema,sep=""))
  
  # Then reference a tbl within that src
  death_tbl <- tbl(my_db, "death")
  
  total_death_count<-  as.data.frame(summarise(death_tbl,n = n()))[1,1]
  
  
  
  mismatch_death_date_tbl <- tbl(my_db, dplyr::sql(paste('SELECT * FROM ',config$db$schema,'.',table_name,
                                                              " WHERE cast(death_time as date) <> death_date",sep=''))
  )
  
  df_incon<-as.data.frame(mismatch_death_date_tbl)
  if(nrow(df_incon)>0)
  {
    
    message<-paste(nrow(df_incon)," deaths with inconsistency between date and date/time fields")
    fileContent<-c(fileContent,"\n",message)
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/death_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-custom_rbind(log_entry_content,
                                    apply_check_type_2('AA-009',"death_time", "death_date",nrow(df_incon), 
                                                       table_name, g_data_version)
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