library(DBI)
library(yaml)

 flog.info(Sys.time())

config = yaml.load_file(g_config_path)

#establish connection to database
con <- establish_database_connection_OHDSI(config)
schema<-config$db$schema

# read a table into an R dataframe
table_name<-"observation_period"
df_table <- retrieve_dataframe_OHDSI(con, config,table_name)

big_data_flag<-FALSE

#writing to the final DQA Report
fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
fileContent <-get_report_header(table_name,config)

#PRIMARY FIELD
field_name<-"observation_period_id"   # 1  minute
fileContent<-c(fileContent,paste("The total number of",table_name,"is: ", describeIdentifier(df_table,field_name),"\n"))

 flog.info(Sys.time())

test <-1
#NOMINAL Fields

# DATE Fields

field_name<-"observation_period_start_date" # 30 sec
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeDateField(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
message<-describeTimeField(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_time",sep="")));


 flog.info(Sys.time())

field_name<-"observation_period_end_date" #  30 sec
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeDateField(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
message<-describeTimeField(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_time",sep="")));


 flog.info(Sys.time())

#FOREIGN KEY fields

field_name<-"person_id" #  minutes
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

 flog.info(Sys.time())


#write all contents to the report file and close it.
writeLines(fileContent, fileConn)
close(fileConn)

#close the connection
close_database_connection_OHDSI(con,config)
