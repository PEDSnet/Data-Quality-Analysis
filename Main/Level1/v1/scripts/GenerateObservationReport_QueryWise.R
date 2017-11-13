library(DBI)
library(yaml)

 flog.info(Sys.time())

big_data_flag<-TRUE

config = yaml.load_file(g_config_path)

#establish connection to database
con <- establish_database_connection_OHDSI(config)

table_name<-"observation"
#df_table <- retrieve_dataframe_OHDSI(con,config,table_name)
# flog.info(nrow(df_table))

#writing to the final DQA Report
fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
fileContent <-get_report_header(table_name,config)

test <-1

#PRIMARY FIELD

  field_name<-"observation_id"
  fileContent<-c(fileContent,paste("The total number of",field_name,"is: ", retrieve_dataframe_count(con, config,table_name,field_name),"\n"))



#NOMINAL Fields

field_name<-"person_id" #
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

# flog.info(fileContent)

field_name<-"associated_provider_id" #
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

# flog.info(fileContent)

field_name<-"visit_occurrence_id" #
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name));

# flog.info(fileContent)

 flog.info(Sys.time())


# ORDINAL Fields
 flog.info(Sys.time())
field_name<-"observation_date" #
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeDateField(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
message<-describeTimeField(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_time",sep="")));

#print (fileContent)

#print (fileContent)

#print (fileContent)


# not plotting the value_as_string column as it's a free text field

field_name<-"unit_concept_id" #
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
describeNominalField_basic(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name));

#print (fileContent)

field_name<-"units_source_value" # 3 minutes
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
describeNominalField_basic(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name));

#print (fileContent)

field_name<-"observation_type_concept_id" # 3 minutes
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
describeNominalField_basic(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name));

#print (fileContent)


field_name<-"relevant_condition_concept_id" #
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

#print (fileContent)

#ordinal field


field_name="observation_source_value"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
describeNominalField_basic(df_table, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name));

# this is a nominal field - work on it
field_name<-"observation_concept_id" #
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name, big_data_flag))
describeNominalField_basic(df_table, table_name,field_name,big_data_flag)

fileContent<-c(fileContent,paste_image_name(table_name,field_name));
# flog.info(fileContent)


# get a list of all observation_concept_ids
concept_id_list <- unique(df_table[,1])


#generating concept wise graphs for numerical readings

field_name<-"value_as_number"
#column_index <- which(colnames(df_table)==field_name)
for (i in 1:length(concept_id_list))
{
  df_table_subset<-retrieve_dataframe_group_clause(con,config,table_name,field_name,paste(" observation_concept_id=",concept_id_list[i]))
  field_name_subset<-paste(field_name,concept_id_list[i],sep="_")
  colnames(df_table_subset)[1] <- field_name_subset
  fileContent <-c(fileContent,paste("## Barplot for",field_name_subset,"(",get_concept_name(concept_id_list[i],con, g_config),")","\n"))
  fileContent<-c(fileContent,reportMissingCount(df_table_subset,table_name,field_name_subset,big_data_flag))
  message<-describeRatioField(df_table_subset, table_name,field_name_subset,"",big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name_subset));

  #print (fileContent)
}


 flog.info(Sys.time())

field_name<-"range_high" #
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeNominalField_basic(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

#print (fileContent)

field_name<-"range_low" #
df_table<-retrieve_dataframe_group(con,config,table_name,field_name)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
message<-describeNominalField_basic(df_table, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));


#write all contents to the report file and close it.
writeLines(fileContent, fileConn)
close(fileConn)

#close the connection
close_database_connection_OHDSI(con,config)
