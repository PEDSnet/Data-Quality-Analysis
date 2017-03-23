library(DBI) 
library(yaml)

 flog.info(Sys.time())

big_data_flag<-FALSE

# load the configuration file 
#get path for current script
full.fpath <- tryCatch(normalizePath(parent.frame(2)$ofile),  # works when using source
                       error=function(e) # works when using R CMD
                         normalizePath(unlist(strsplit(commandArgs()[grep('^--file=', commandArgs())], '='))[2]))
script.dir <- dirname(full.fpath)
config = yaml.load_file(paste(script.dir,"/../PEDSnet_config.yml",sep=""))

#establish connection to database
con <- establish_database_connection_OHDSI(config)

# read a table into an R dataframe
table_name<-"procedure_occurrence"
df_table <- retrieve_dataframe_OHDSI(con,config,table_name)

#writing to the final DQA Report
fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
fileContent <-get_report_header(table_name,config)

#PRIMARY FIELD
field_name<-"procedure_occurrence_id"  
fileContent<-c(fileContent,paste("The total number of",table_name,"is: ", describeIdentifier(df_table,field_name),"\n"))


#NOMINAL Fields
# this is a nominal field - retrieving the valid concept ids directly from the concept table
df_procedure_type_concept_id<-retrieve_dataframe_clause(con,config,"concept","concept_id,concept_name","CONCEPT_CLASS='Procedure Occurrence Type' and VOCABULARY_ID in (38)")
order_bins <-c(df_procedure_type_concept_id$concept_id,NA)

field_name<-"procedure_type_concept_id" # 

fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
# flog.info(null_message)
unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
# flog.info(unexpected_message)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,unexpected_message)
#update values of the field before plotting
df_table_procedure_type_enhanced<-EnhanceFieldValues(df_table,field_name,df_procedure_type_concept_id);
describeNominalField_basic(df_table_procedure_type_enhanced,table_name,field_name,big_data_flag);
fileContent<-c(fileContent,paste_image_name(table_name,field_name));


 flog.info(Sys.time())
# ORDINAL Fields

field_name<-"procedure_concept_id" # 
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeOrdinalField_large(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);

 flog.info(Sys.time())


field_name<-"procedure_date" # 
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeDateField(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);
message<-describeTimeField(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_time",sep="")));

 flog.info(Sys.time())

field_name<-"relevant_condition_concept_id" # 
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeOrdinalField_large(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

 flog.info(Sys.time())


field_name<-"procedure_source_value" #  3 minutes 
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeOrdinalField_large(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

 flog.info(Sys.time())


#FOREIGN KEY fields

field_name<-"person_id" 
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

 flog.info(Sys.time())

field_name<-"visit_occurrence_id" 
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

 flog.info(Sys.time())

field_name<-"associated_provider_id" #  4 minutes 
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

