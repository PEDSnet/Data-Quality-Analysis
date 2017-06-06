generateAdtOccurrenceReport <- function() {
  flog.info(Sys.time())

  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)

  # read a table into an R dataframe
  table_name<-"adt_occurrence"

  #df_table <- retrieve_dataframe_OHDSI(con, g_config,table_name)
  # flog.info(nrow(df_table))

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), 
                          issue_code=character(0), issue_description=character(0), check_alias=character(0)
                          , finding=character(0), prevalence=character(0))
  
  test <-1
  big_data_flag<-TRUE

#PRIMARY FIELD
field_name<-"adt_occurrence_id"
df_total_visit_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
current_total_count<-as.numeric(df_total_visit_count[1][1])
fileContent<-c(fileContent,paste("The total number of",field_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
###########DQA CHECKPOINT############## difference from previous cycle
logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name),NULL,current_total_count)) 

  #df_total_patient_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct person_id")
  #fileContent<-c(fileContent,paste("The visit to patient ratio is ",round(df_total_visit_count[1][1]/df_total_patient_count[1][1],2),"\n"))

  field_name<-"person_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);



  field_name<-"visit_occurrence_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);


  ### DQA checkpoint --- incosnistent visit types
  df_outpatient_adts_count<-retrieve_dataframe_join_clause(con,g_config,g_config$db$schema,table_name, 
                                   g_config$db$schema,"visit_occurrence","count(*)",
                                   "adt_occurrence.visit_occurrence_id = visit_occurrence.visit_occurrence_id
                                      and visit_concept_id in (9202, 44814711)") 
  
  ###########DQA CHECKPOINT############## difference from previous cycle
  if(df_outpatient_adts_count[1,1]>0)
  {
    logFileData<-custom_rbind(logFileData,
                              apply_check_type_2_diff_tables("CA-013",table_name, "visit_occurrence_id", 
                                                             "visit_occurrence", "visit_concept_id", 
                                                             paste(df_outpatient_adts_count[1,1], "adts are outpatient visits"))
                              );
  }
  
  #NOMINAL Fields

  # ORDINAL Fields


    field_name<-"adt_time"
    df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
    #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
    message<-describeDateField(df_table, table_name, field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);
    message<-describeTimeField(df_table, table_name, field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,paste(field_name,"_time",sep="")),message);

    field_name<-"adt_date"
    df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
    #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
    message<-describeDateField(df_table, table_name, field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);

  #}
  ###########DQA CHECKPOINT##############
if(length(message)==3)
{
        if(grepl("future",message[3]))
  {
    logFileData<-custom_rbind(logFileData,apply_check_type_1("CA-001", field_name, "future visits should not be included", table_name, g_data_version));
  }

}
     flog.info(Sys.time())

     flog.info(Sys.time())
    field_name<-"care_site_id" # 8 minutes
    df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
     ###########DQA CHECKPOINT -- missing information##############
    logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
    message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);



  # service concept id
  df_service_concept_id <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name"
                                       ,"vocabulary_id ='PEDSnet' and concept_class_id='Service Type' and standard_concept='S'")
  order_bins <-c(df_service_concept_id$concept_id,0,44814650,44814653, 44814649,NA)
  field_name="service_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version));
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  fileContent<-c(fileContent,unexpected_message)
  df_service_concept_id_enhanced<-EnhanceFieldValues(df_table,field_name,df_service_concept_id);
  describeNominalField_basic(df_service_concept_id_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  # adt type concept id
  df_adt_type_concept_id <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name"
                                                    ,"vocabulary_id ='PEDSnet' and domain_id='ADT Event Type'")
  order_bins <-c(df_adt_type_concept_id$concept_id,0,NA)
  field_name="adt_type_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  #print(logFileData)
  if(missing_percent<100)
  {
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  #print("line 160")
  #print(apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version))
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version));
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  fileContent<-c(fileContent,unexpected_message)
  df_adt_type_concept_id_enhanced<-EnhanceFieldValues(df_table,field_name,df_adt_type_concept_id);
  describeNominalField_basic(df_adt_type_concept_id_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  }
  
   flog.info(Sys.time())
  field_name<-"prior_adt_occurrence_id" # 8 minutes
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

   flog.info(Sys.time())
  field_name<-"next_adt_occurrence_id" # 8 minutes
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);



   flog.info(Sys.time())
  field_name<-"service_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  describeNominalField_basic(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

   flog.info(Sys.time())
  field_name<-"adt_type_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  describeNominalField_basic(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  flog.info(Sys.time())

  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)


  #close the connection
  close_database_connection_OHDSI(con, g_config)
}
