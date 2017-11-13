library(DBI)
library(yaml)

config = yaml.load_file(g_config_path)

#establish connection to database
con <- establish_database_connection_OHDSI(config)

# read a table into an R dataframe
table_name<-"location"
df_table <- retrieve_dataframe_OHDSI(con,config,table_name)

big_data_flag<-FALSE

#writing to the final DQA Report
fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
fileContent <-get_report_header(table_name,config)

#PRIMARY FIELD(s)
field_name<-"location_id"
fileContent<-c(fileContent,paste("The total number of",field_name,"is: ", describeIdentifier(df_table, field_name),"\n"))
field_name<-"location_source_value"
fileContent<-c(fileContent,paste("The total number of",field_name,"is: ", describeIdentifier(df_table, field_name),"\n"))

test<-1


# ORDINAL Fields

field_name="address_1"
fileContent<-c(fileContent,paste("The total number of",field_name,"is: ", describeIdentifier(df_table, field_name),"\n"))

field_name="address_2"
fileContent<-c(fileContent,paste("The total number of",field_name,"is: ", describeIdentifier(df_table, field_name),"\n"))

field_name="city"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name));

field_name="state"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
describeOrdinalField(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name));


field_name="zip"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);



field_name="county"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);

#write all contents to the report file and close it.
writeLines(fileContent, fileConn)
close(fileConn)

#close the connection
close_database_connection_OHDSI(con,config)
