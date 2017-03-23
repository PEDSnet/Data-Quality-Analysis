library(DBI) 
library(yaml)

 flog.info(Sys.time())

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
table_name<-"visit_occurrence"

#df_table <- retrieve_dataframe_OHDSI(con,config,table_name)
# flog.info(nrow(df_table))

#writing to the final DQA Report
fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
fileContent <-get_report_header(table_name,config)

test <-1
big_data_flag<-TRUE

#PRIMARY FIELD
field_name<-"visit_occurrence_id"   
fileContent<-c(fileContent,paste("The total number of",field_name,"is:", retrieve_dataframe_count(con, config,table_name,field_name),"\n"))

#NOMINAL Fields

# ORDINAL Fields

field_name<-"visit_start_date"
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeDateField(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);
message<-describeTimeField(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,paste(field_name,"_time",sep="")),message);

 flog.info(Sys.time())

field_name<-"visit_end_date"
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeDateField(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);
message<-describeTimeField(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,paste(field_name,"_time",sep="")),message);

 flog.info(Sys.time())


field_name<-"place_of_service_concept_id" 
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
order_bins <-c("9201","9202","9203","42898160","44814710","44814711","44814713","44814714","44814712",NA)
label_bins<-c("Inpatient Hospital Stay (9201)","Ambulatory Visit(9202)","Emergency Department(9203)","Long term care visit (42898160)","Non-Acute Institutional Stay(44814710)","Other ambulatory visit(44814711)","Unknown (44814713)","Other(44814714)","No Information(44814712)","NULL")
color_bins <-c("9201"="lightcoral","9202"="steelblue1","9203"="lightgreen","42898160"="tan1","44814710"="deeppink1","44814711"="antiquewhite2","44814713"="grey64","44814714"="grey64","44814712"="grey64")
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportNullFlavors(df_table,table_name,field_name,44814713,44814714,44814712,big_data_flag))
unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
describeNominalField(df_table, table_name, field_name, label_bins, order_bins, color_bins,big_data_flag)
fileContent<-c(fileContent,unexpected_message,paste_image_name(table_name,field_name));

 flog.info(Sys.time())

field_name<-"place_of_service_source_value" 
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
describeOrdinalField(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name));

 flog.info(Sys.time())

#FOREIGN KEY fields

field_name<-"person_id" 
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

 flog.info(Sys.time())

field_name<-"provider_id" 
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

 flog.info(Sys.time())
field_name<-"care_site_id" # 8 minutes 
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
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

