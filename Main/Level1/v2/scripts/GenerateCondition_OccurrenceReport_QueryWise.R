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
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), 
                          issue_code=character(0), issue_description=character(0), check_alias=character(0)
                          , finding=character(0), prevalence=character(0))
  
  test<-1

  big_data_flag<-TRUE # for query wise analysis


  #PRIMARY FIELD
  field_name<-"condition_occurrence_id"
  df_total_condition_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  current_total_count<-as.numeric(df_total_condition_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name),NULL,current_total_count)) 
  

  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  
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
  df_condition_type_concept_id<-retrieve_dataframe_clause(con,g_config,g_config$db$vocab_schema,
                                                          "concept","concept_id,concept_name",
                                                          "concept_class_id ='Condition Type'
                                                          and vocabulary_id='PEDSnet'")



  # this is a nominal field - work on it
  field_name<-"condition_type_concept_id" #
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  order_bins <-c(df_condition_type_concept_id$concept_id,NA)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT##############
 
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "condition_type_concept_id.txt")) 
  df_condition_type_concept_id <-generate_df_concepts(con, table_name, "condition_type_concept_id.txt")
  
  df_table_condition_type_enhanced<-EnhanceFieldValues(df_table,field_name,df_condition_type_concept_id);
  describeNominalField_basic(df_table_condition_type_enhanced,table_name,field_name, big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name),con, 
                                                   list(
                                                   list(2000000092,2000000093,2000000094,  "inpatient header primary"),
                                                   list(2000000095,2000000096,2000000097,  "outpatient header 1st position"), 
                                                   list(2000000098,2000000099,2000000100,  "inaptient header 2nd position"), 
                                                   list(2000000101,2000000102,2000000103,  "outpatient header 2nd position"), 
                                                   list(2000000089,2000000090,2000000091,  "EHR problem list entry")
                                                   ))) 
  
  
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
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidFormat(), c(table_name),c(field_name)
                                                   ,con,  3))  ## number of components in condition_source_value
  
  
  field_name<-"condition_source_concept_id"
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  # some fields can have multiple vocabularies
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidVocab(), c(table_name),c(field_name),con, c('Condition',c('ICD9','ICD9CM', 'ICD10', 'ICD10CM')))) 
  
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
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  # some fields can have multiple vocabularies

  ###########DQA CHECKPOINT -- invalid vocab ##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidVocab(), c(table_name),c(field_name),con, c('Condition','SNOMED'))) 
  
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
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
  
  
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  field_name<-"condition_end_date"
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)
  if(missing_percent<100)
  {
    ### DQA checkpoint - future date
    logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
    
    
  }
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  
  #Sys.sleep(10)

  logFileData<-custom_rbind(logFileData,applyCheck(ImplEvent(), c(table_name), c('condition_start_date','condition_end_date'),con)) 
  
  #Sys.sleep(10)

  #print (fileContent)
  field_name<-"stop_reason"
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
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
    #missing_percent<-extract_numeric_value(message)
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_visit_percent_nonproblemlist, table_name, g_data_version));

    #message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);
  
 


    field_name<-"provider_id"
    df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
    message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,message)
    ###########DQA CHECKPOINT -- missing information##############
    missing_percent<-extract_numeric_value(message)
    logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
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
     #order_bins <-c(df_condition_type_concept_id$concept_id,NA)
    
    
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
  
    logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                     ,con,  "condition_status_concept_id.csv")) 
    #df_condition_status_concept_id <-generate_list_concepts(table_name, "condition_status_concept_id.csv")
    
    # this is a nominal field - work on it
    field_name<-"poa_concept_id" #
    df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
    order_bins <-c("4188539","4188540","44814653", "44814649","44814650","0",NA)
    label_bins<-c("Yes (4188539)","No (4188540)","0 (No matching)","Unknown (44814653)",
                  "Other (44814649)","No Information (44814650 )","NULL")
    color_bins <-c("4188539"="lightcoral","4188540"="steelblue1","0"="red","44814653"="grey64","44814649"="grey64","44814650 "="grey64")
    
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
    missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
    missing_percent<- extract_numeric_value(missing_percent_message)
    fileContent<-c(fileContent,missing_percent_message)
    ###########DQA CHECKPOINT##############
    
    logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                     ,con,  "poa_concept_id.csv")) 

    describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name));
    
    
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)

  #close the connection
  close_database_connection_OHDSI(con,g_config)
}
