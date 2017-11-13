library(DBI)
library(yaml)

 flog.info(Sys.time())

config = yaml.load_file(g_config_path)

#establish connection to database
con <- establish_database_connection_OHDSI(config)

# read a table into an R dataframe
table_name<-"condition_occurrence"
df_table <- retrieve_dataframe_OHDSI(con,config,table_name)
# flog.info(nrow(df_table))
 flog.info(Sys.time())

#writing to the final DQA Report
fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
fileContent <-get_report_header(table_name,config)


test<-1


big_data_flag<-FALSE


#PRIMARY FIELD
field_name<-"condition_occurrence_id"
fileContent<-c(fileContent,paste("The total number of",field_name,"is:", describeIdentifier(df_table,field_name),"\n"))
 flog.info(Sys.time())

#NOMINAL Fields
df_condition_type_concept_id<-retrieve_dataframe_clause(con,config,"concept","concept_id,concept_name","CONCEPT_CLASS='Condition Occurrence Type' and VOCABULARY_ID in (37)")
order_bins <-c(df_condition_type_concept_id$concept_id,NA)

# this is a nominal field - work on it
field_name<-"condition_type_concept_id" #
fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
fileContent<-c(fileContent,reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag))
df_table_condition_type_enhanced<-EnhanceFieldValues(df_table,field_name,df_condition_type_concept_id);
describeNominalField_basic(df_table_condition_type_enhanced,table_name,field_name,big_data_flag);
fileContent<-c(fileContent,paste_image_name(table_name,field_name));


# ORDINAL Fields
 flog.info(Sys.time())

test<-1

field_name<-"condition_concept_id" #

fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

#print (fileContent)
#}
field_name<-"condition_start_date"

fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeDateField(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
message<-describeTimeField(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_time",sep="")));

#print (fileContent)

field_name<-"condition_end_date"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeDateField(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
message<-describeTimeField(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_time",sep="")));

#print (fileContent)



field_name<-"condition_source_value"

fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

#print (fileContent)


field_name<-"stop_reason"

fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent, paste_image_name(table_name,field_name),message);

#print (fileContent)


#FOREIGN KEY fields

field_name<-"person_id" #
fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

# flog.info(fileContent)

 flog.info(Sys.time())

field_name<-"visit_occurrence_id"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

 flog.info(Sys.time())

  field_name<-"associated_provider_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

     flog.info(Sys.time())


#write all contents to the report file and close it.
writeLines(fileContent, fileConn)
close(fileConn)

#close the connection
close_database_connection_OHDSI(con,config)
