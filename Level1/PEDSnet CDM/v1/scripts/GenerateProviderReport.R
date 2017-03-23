library(DBI)
library(yaml)

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
table_name<-"provider"
df_provider <- retrieve_dataframe_OHDSI(con, config,table_name)

big_data_flag<-FALSE

#writing to the final DQA Report
fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
fileContent <-get_report_header(table_name,config)

#PRIMARY FIELD(s)
field_name<-"provider_id"
fileContent<-c(fileContent,paste("The total number of",field_name,"is: ", describeIdentifier(df_provider, field_name),"\n"))
field_name<-"provider_source_value"
fileContent<-c(fileContent,paste("The total number of",field_name,"is: ", describeIdentifier(df_provider,field_name),"\n"))

# remove following fields - DCC has asked not to transmit that info
#Secondary identifiers
#field_name<-"npi"
#fileContent<-c(fileContent,paste("The total number of",field_name,"is: ", describeIdentifier(df_provider, field_name),reportMissingCount(df_provider,table_name,field_name,big_data_flag),"\n"))
#field_name<-"dea"
#fileContent<-c(fileContent,paste("The total number of",field_name,"is: ", describeIdentifier(df_provider, field_name),reportMissingCount(df_provider,table_name, field_name,big_data_flag),"\n"))


# ORDINAL Fields

#Specialty concept id
field_name="specialty_concept_id"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_provider,table_name,field_name,big_data_flag))
describeOrdinalField(df_provider, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name));

#Specialty source value
field_name="specialty_source_value"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_provider,table_name,field_name,big_data_flag))
describeOrdinalField(df_provider, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name));

#FOREIGN KEY fields
#Care site id
field_name="care_site_id"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_provider,table_name,field_name,big_data_flag))
message<-describeForeignKeyIdentifiers(df_provider,table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

#write all contents to the report file and close it. 
writeLines(fileContent, fileConn)
close(fileConn)

#close the connection 
close_database_connection_OHDSI(con,config)

