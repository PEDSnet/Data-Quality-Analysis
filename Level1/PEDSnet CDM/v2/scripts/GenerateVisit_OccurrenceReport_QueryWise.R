generateVisitOccurrenceReport <- function() {
  flog.info(Sys.time())
  big_data_flag<-TRUE

  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)

  # read a table into an R dataframe
  table_name<-"visit_occurrence"

  #df_table <- retrieve_dataframe_OHDSI(con, g_config,table_name)
  # flog.info(nrow(df_table))

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), issue_description=character(0)
                          , finding=character(0), prevalence=character(0))

  test <-1

  #PRIMARY FIELD
  field_name<-"visit_occurrence_id"
  df_total_visit_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  current_total_count<-as.numeric(df_total_visit_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
  prev_total_count<-get_previous_cycle_total_count( g_config$reporting$site, table_name)
  percentage_diff<-get_percentage_diff(prev_total_count, current_total_count)
  fileContent<-c(fileContent, get_percentage_diff_message(percentage_diff))
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,apply_check_type_0("CA-005", percentage_diff, table_name, g_data_version));


  df_total_patient_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct person_id")
  fileContent<-c(fileContent,paste("The visit to patient ratio is ",round(df_total_visit_count[1][1]/df_total_patient_count[1][1],2),"\n"))

  #NOMINAL Fields

  # ORDINAL Fields


    field_name<-"visit_start_time"
    df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
    #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
    message<-describeDateField(df_table, table_name, field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);
    message<-describeTimeField(df_table, table_name, field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,paste(field_name,"_time",sep="")),message);
  
  
  ###########DQA CHECKPOINT##############
  if(grepl("future",message[3]))
  {
    logFileData<-custom_rbind(logFileData,apply_check_type_1("CA-001", field_name, "future visits should not be included", table_name, g_data_version));
  }


   flog.info(Sys.time())

  field_name<-"visit_end_date"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeDateField(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);
  ###########DQA CHECKPOINT##############
  if(grepl("future",message[3]))
  {
    logFileData<-custom_rbind(logFileData,apply_check_type_1("CA-001", field_name, "visits ending in the future", table_name, g_data_version));
  }

  # check for plausibility
  df_implausible_date_count<-retrieve_dataframe_clause(con, g_config, g_config$db$schema,table_name,"count(*) as count","visit_start_date>visit_end_date")
  if(df_implausible_date_count[1][1]>0)
  {
    ###########DQA CHECKPOINT##############
    implausible_message<-paste("There are ",df_implausible_date_count[1][1]," records with visit_start_date>visit_end_date")
    fileContent<-c(fileContent,implausible_message);
    #logFileData<-custom_rbind(logFileData,apply_check_type_2("CA-016", table_name, "visit_start_date",field_name, implausible_message, table_name, g_data_version));
  }
   flog.info(Sys.time())


  ## visit end time
  #field_name<-"visit_end_time"
  #df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  #missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  #missing_percent<- extract_numeric_value(missing_percent_message)
  #fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  #logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));


  # visit type concept id
  df_visit_type <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name"
                                       ,"concept_class_id='Visit Type'")
  order_bins <-c(df_visit_type$concept_id,0,NA)
  field_name="visit_type_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version));
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));
  fileContent<-c(fileContent,unexpected_message)
  df_table_visit_type_enhanced<-EnhanceFieldValues(df_table,field_name,df_visit_type);
  describeNominalField_basic(df_table_visit_type_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));


   flog.info(Sys.time())

  field_name<-"visit_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent_source_value, table_name, g_data_version));
  describeNominalField_basic(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  # visit source concept id
  field_name="visit_source_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));
  if(missing_percent<100)
  {
  describeNominalField_basic(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  }


  # visit concept id
  df_visit <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name",
                              "
                              domain_id='Visit' or (vocabulary_id='PCORNet' and concept_class_id='Encounter Type' and
  	                        	(concept_code not like  '%-ED'
                              and concept_code not like '%-IP'
                              and concept_code not like '%-AV'))
                              or
                              (vocabulary_id='PCORNet' and concept_class_id='Undefined' and
                              (concept_code not like  '%-ED'
                              and concept_code not like '%-IP'
                              and concept_code not like '%-AV')
                              ) and invalid_reason is null
                              "
                                       )
  order_bins <-c(df_visit$concept_id,2000000088,0,NA)
  field_name="visit_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650,big_data_flag)
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,apply_check_type_2("CA-014", field_name,"visit_source_value",
                                                           (missing_percent_source_value -
                                                            extract_ni_missing_percent( null_message)), table_name, g_data_version))
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version));
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT##############
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));
  ###########DQA CHECKPOINT##############
  if(nrow(subset(df_table,df_table$visit_concept_id==9201))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No Inpatient visits found","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No inpatient visits found", table_name, g_data_version));
    
  }
  if(nrow(subset(df_table,df_table$visit_concept_id==9202))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No outpatient visits found","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No outpatient visits found", table_name, g_data_version));
    
  }
  if(nrow(subset(df_table,df_table$visit_concept_id==9203))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No ED visits found","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No ED visits found", table_name, g_data_version));
    
  }
  if(nrow(subset(df_table,df_table$visit_concept_id==44814711))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No other ambulatory visits found","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No other ambulatory visits found", table_name, g_data_version));
    
  }
  
  fileContent<-c(fileContent,unexpected_message)
  df_table_visit_enhanced<-EnhanceFieldValues(df_table,field_name,df_visit);
  describeNominalField_basic(df_table_visit_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));

  #visit / person id by visit types
  fileContent <-c(fileContent,paste("## Barplot for Visit:Patient ratio by visit type\n"))
  df_visit_patient_ratio<-retrieve_dataframe_ratio_group(con, g_config,table_name,"round(count(distinct visit_occurrence_id)/count(distinct person_id),2)", "visit_concept_id")

  for(i in 1:nrow(df_visit_patient_ratio))
      {
          #df_visit[df_visit$concept_id==9201,2]
            label<-df_visit[df_visit$concept_id==df_visit_patient_ratio[i,1],2]
        df_visit_patient_ratio[i,1]<-paste(df_visit_patient_ratio[i,1],"(",label,")",sep="")
          #df_visit_patient_ratio[i,1]
          }
  describeOrdinalField(df_visit_patient_ratio,table_name,"Visit:Patient ratio by visit type",big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,"Visit:Patient ratio by visit type"));

  #FOREIGN KEY fields

  field_name<-"person_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

   flog.info(Sys.time())

  field_name<-"provider_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);



   flog.info(Sys.time())
  field_name<-"care_site_id" # 8 minutes
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);



   flog.info(Sys.time())

  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)


  #close the connection
  close_database_connection_OHDSI(con, g_config)
}
