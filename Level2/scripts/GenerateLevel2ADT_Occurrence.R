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
  
  
  
  mismatch_adt_date_tbl <- tbl(my_db, dplyr::sql(paste('SELECT * FROM ',config$db$schema,'.',table_name,
                                                              " WHERE cast(adt_time as date) <> adt_date",sep=''))
  )
  
  df_incon<-as.data.frame(mismatch_adt_date_tbl)
  if(nrow(df_incon)>0)
  {
    
    message<-paste(nrow(df_incon)," adts with inconsistency between date and date/time fields")
    fileContent<-c(fileContent,"\n",message)
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/adt_occurrence_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-custom_rbind(log_entry_content,
                                    apply_check_type_2('AA-009',"adt_time", "adt_date",nrow(df_incon), 
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