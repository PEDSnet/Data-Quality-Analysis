generateConditionOccurrenceReport <- function() {
  flog.info(Sys.time())

  #establish connection to database
  con <- establish_database_connection_OHDSI(g_config)

  # read a table into an R dataframe
  table_name<-"condition_occurrence"
  #df_table <- retrieve_dataframe_OHDSI(con,config,table_name)
  # flog.info(nrow(df_table))
   flog.info(Sys.time())

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name,g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), issue_description=character(0)
                          , finding=character(0), prevalence=character(0))

  test<-1

  big_data_flag<-TRUE # for query wise analysis


  #PRIMARY FIELD
  field_name<-"condition_occurrence_id"
  df_total_condition_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  current_total_count<-as.numeric(df_total_condition_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
  prev_total_count<-get_previous_cycle_total_count(g_config$reporting$site, table_name)
  percentage_diff<-get_percentage_diff(prev_total_count, current_total_count)
  fileContent<-c(fileContent, get_percentage_diff_message(percentage_diff))
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,apply_check_type_0("CA-005", percentage_diff, table_name, g_data_version));


  df_total_patient_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct person_id")
  fileContent<-c(fileContent,paste("The condition to patient ratio is ",round(df_total_condition_count[1][1]/df_total_patient_count[1][1],2),"\n"))
  df_total_visit_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct visit_occurrence_id")
  fileContent<-c(fileContent,paste("The condition to visit ratio is ",round(df_total_condition_count[1][1]/df_total_visit_count[1][1],2),"\n"))

  # visit concept id
  df_visit <-retrieve_dataframe_clause(con,g_config,g_config$db$vocab_schema,"concept","concept_id,concept_name"
                                        ,"vocabulary_id ='Visit' or (vocabulary_id='PCORNet' and concept_class_id='Encounter Type')
                                       or (vocabulary_id = 'PCORNet' and concept_class_id='Undefined')")

  #condition / person id by visit types
  fileContent <-c(fileContent,paste("## Barplot for Condition:Patient ratio by visit type\n"))
  df_condition_patient_ratio<-retrieve_dataframe_ratio_group_join(con,g_config,table_name,"visit_occurrence",
   "round(count(distinct condition_occurrence_id)/count(distinct condition_occurrence.person_id),2)",
   "visit_concept_id","visit_occurrence_id")

    for(i in 1:nrow(df_condition_patient_ratio))
      {
          #df_visit[df_visit$concept_id==9201,2]
          label<-df_visit[df_visit$concept_id==df_condition_patient_ratio[i,1],2]
          df_condition_patient_ratio[i,1]<-paste(df_condition_patient_ratio[i,1],"(",label,")",sep="")
          #df_visit_patient_ratio[i,1]
      }
  describeOrdinalField(df_condition_patient_ratio,table_name,"Condition:Patient ratio by visit type",big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,"Condition:Patient ratio by visit type"));


  #NOMINAL Fields
 # df_condition_type_concept_id<-retrieve_dataframe_clause(con,g_config,g_config$db$vocab_schema,
  #                                                        "concept","concept_id,concept_name",
   #                                                       "domain_id ='Condition Type'
    #                                                      and vocabulary_id='PEDSnet'")
  order_bins <-c(2000000095, 2000000096, 2000000097, 
                 2000000092, 2000000093, 2000000094, 
                 2000000098, 2000000099, 2000000100, 
                 2000000101, 2000000102, 2000000103, 
                 2000000089, 2000000090, 2000000091,0,NA)


  # this is a nominal field - work on it
  field_name<-"condition_type_concept_id" #
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  order_bins <-c("38000199","38000230","38000201","38000231","38000245","0",NA)
  label_bins<-c("Inpatient header - primary (38000199)","Outpatient header - 1st position (38000230)","Inpatient header - 2nd position (38000201)"
                ,"Outpatient header - 2nd position (38000231)","EHR problem list entry (38000245)","Other (0)","NULL")
  color_bins <-c("38000199"="lightcoral","38000230"="steelblue1","38000201"="red","38000231"="grey64","38000245"="grey64","0"="grey64")
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version));
  fileContent<-c(fileContent,unexpected_message)
  describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  if(nrow(subset(df_table,df_table$condition_type_concept_id==38000199))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No Inpatient header primary Records","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No inpatient header primary records found", table_name, g_data_version));

  }
  if(nrow(subset(df_table,df_table$condition_type_concept_id==38000230))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No Outpatient header 1st position Records","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No outpatient header 1st position records found", table_name, g_data_version));

  }
  if(nrow(subset(df_table,df_table$condition_type_concept_id==38000201))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No Inpatient header - 2nd position Records","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No Inpatient header - 2nd position records found", table_name, g_data_version));

  }
  if(nrow(subset(df_table,df_table$condition_type_concept_id==38000231))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No Outpatient header - 2nd position Records","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No Outpatient header - 2nd position records found", table_name, g_data_version));

  }
  if(nrow(subset(df_table,df_table$condition_type_concept_id==38000245))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No EHR problem list entry Records","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No EHR problem list entry records found", table_name, g_data_version));

  }

  # ORDINAL Fields

  field_name<-"condition_source_value"
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  # some fields can have multiple vocabularies
  #fileContent<-c(fileContent,paste("\n The source vocabulary is",get_vocabulary_name_by_concept_codes(con, g_config, config$db$schema,table_name, field_name,config$db$vocab_schema, "CONDITION"),"\n"))
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  #new_message<-create_meaningful_message_concept_code(message,field_name,con,config)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  field_name<-"condition_source_concept_id"
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  # some fields can have multiple vocabularies
  used_vocabulary<-get_vocabulary_name_by_concept_ids(con, g_config, table_name, field_name, "CONDITION")
  fileContent<-c(fileContent,paste("\n The source vocabulary is",used_vocabulary,"\n"))
  if(!is.na(used_vocabulary))
  {
    # check if each vocabulary is a flavor of ICD
    for(vocab_index in 1:length(unlist(strsplit(used_vocabulary,"\\|"))))
    {
      vocabulary<-unlist(strsplit(used_vocabulary,"\\|"))[vocab_index];
      if(!grepl("ICD",vocabulary))
      {
      ###########DQA CHECKPOINT -- vocabulary incorrect ##############
        logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-005", field_name,
                                                                  paste("invalid vocabulary found:",vocabulary," ; please use ICD9 or ICD10 only"), table_name, g_data_version));
      }
    }
  }
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  # create meaningful message
  new_message<-create_meaningful_message_concept_id(message,field_name,con,g_config)
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));

   flog.info(Sys.time())
  field_name<-"condition_concept_id" #
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  # add % of no matching concept (concept id = 0). for the completeness report
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  ###########DQA CHECKPOINT -- no matching concept percentage ##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message), table_name, g_data_version)); # custom threshold
  # some fields can have multiple vocabularies
  fileContent<-c(fileContent,paste("\n The standard/prescribed vocabulary is SNOMED\n"))
  used_vocabulary<-get_vocabulary_name_by_concept_ids(con, g_config, table_name, field_name, "CONDITION")
  fileContent<-c(fileContent,paste("\n The vocabulary used by the site is",used_vocabulary,"\n"))
  if(!is.na(used_vocabulary) && used_vocabulary!='SNOMED|')
  {
    ###########DQA CHECKPOINT -- vocabulary incorrect ##############
    logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-005", field_name, "invalid vocabulary used, please use SNOMEDCT", table_name, g_data_version));
  }
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  # create meaningful message
  new_message<-create_meaningful_message_concept_id(message,field_name,con,g_config)
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));


  field_name<-"condition_concept_id" #
  df_table_new<-retrieve_dataframe_count_group(con,g_config,table_name,"person_id", field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"by person_id","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table_new,table_name,field_name,big_data_flag))
  #fileContent<-c(fileContent,paste("\n The standard/prescribed vocabulary is SNOMED CT\n"))
  #fileContent<-c(fileContent,paste("\n The vocabulary used by the site is",get_vocabulary_name_by_concept_ids(con, g_config, table_name, field_name, "CONDITION"),"\n"))
  #field_name<-paste(field_name,"_by_person_id",sep="")
  #message<-describeOrdinalField_large(df_table_new, table_name,field_name,big_data_flag)
  message<-describeOrdinalField_large(df_table_new, "person_id",field_name,big_data_flag)
  # create meaningful message
  new_message<-create_meaningful_message_concept_id(message,field_name,con,g_config)
  #fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));
  fileContent<-c(fileContent,new_message,paste_image_name("person_id",field_name));

  

  field_name<-"condition_start_date"
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)
  if(grepl("future",message[3]))
  {
    logFileData<-custom_rbind(logFileData,apply_check_type_1("CA-001", field_name, "conditions cannot start in the future", table_name, g_data_version));
  }
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  field_name<-"condition_end_date"
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)
  if(grepl("future",message[3]))
  {
    logFileData<-custom_rbind(logFileData,apply_check_type_1("CA-001", field_name, "conditions cannot end in the future", table_name, g_data_version));
  }
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  
  #Sys.sleep(10)

  df_implausible_date_count<-retrieve_dataframe_clause(con,g_config,g_config$db$schema,table_name,"count(*)","condition_start_date>condition_end_date")
  if(df_implausible_date_count[1][1]>0)
  {
    ###########DQA CHECKPOINT##############
    implausible_message<-paste("There are ",df_implausible_date_count[1][1]," records with condition_start_date>condition_end_date")
    fileContent<-c(fileContent,implausible_message);
    #logFileData<-custom_rbind(logFileData,apply_check_type_1("CA-016", table_name, "condition_start_date",field_name, implausible_message, table_name, g_data_version));
  }
  #Sys.sleep(10)

  #print (fileContent)
  field_name<-"stop_reason"
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent, paste_image_name(table_name,field_name),message);
  #print (fileContent)


  #FOREIGN KEY fields

  field_name<-"person_id" #
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);


  # flog.info(fileContent)

  # flog.info(Sys.time())


  field_name<-"visit_occurrence_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
 
    df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
    message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,message)
    ## compute missing % for visits stratified by condition_type_concept_id (problem list vs non-problem list)
    ## the expectation is that there shouldnt be any missing visit in non-problem list. and there could be missing for problem list entries
    count_nonproblemlist_novisit<-retrieve_dataframe_clause(con,g_config,g_config$db$schema,table_name,"count(*)"
                                                            ,"visit_occurrence_id is null and condition_type_concept_id<>38000245")
    count_nonproblemlist<-retrieve_dataframe_clause(con,g_config,g_config$db$schema,table_name,"count(*)"
                                                    ,"condition_type_concept_id<>38000245")
    # compute % (# of records with missing visit info for problem list visits) / (# problem list visits)
    missing_visit_percent_nonproblemlist<-round(count_nonproblemlist_novisit*100/count_nonproblemlist,2)
    message_visit_percent_nonproblemlist<-paste("The percentage of conditions (besides problem list) with missing information:",missing_visit_percent_nonproblemlist,"%")
    fileContent<-c(fileContent,message_visit_percent_nonproblemlist)

    ###########DQA CHECKPOINT -- missing information##############
    missing_percent<-extract_numeric_value(message)
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_visit_percent_nonproblemlist, table_name, g_data_version));

    message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);
  
 


    field_name<-"provider_id"
    df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
    message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,message)
    ###########DQA CHECKPOINT -- missing information##############
    missing_percent<-extract_numeric_value(message)
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
    message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

       flog.info(Sys.time())

   ## condition status source value 
    field_name<-"condition_status_source_value"
    df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
    missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
    missing_percent<- extract_numeric_value(missing_percent_message)
    fileContent<-c(fileContent,missing_percent_message)
    ###########DQA CHECKPOINT##############
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
    
    order_bins <-c(df_condition_type_concept_id$concept_id,NA)
    
    
    # this is a nominal field - work on it
    field_name<-"condition_status_concept_id" #
    df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
    order_bins <-c("4230359","0",NA)
    label_bins<-c("Final Diagnosis (4230359)","Other (0)","NULL")
    color_bins <-c("4230359"="lightcoral","0"="grey64")
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
    missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
    missing_percent<- extract_numeric_value(missing_percent_message)
    fileContent<-c(fileContent,missing_percent_message)
    ###########DQA CHECKPOINT##############
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
    unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
    ###########DQA CHECKPOINT##############
    logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version));
    fileContent<-c(fileContent,unexpected_message)
    describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name));
    
    
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)

  #close the connection
  close_database_connection_OHDSI(con,g_config)
}
