generateConditionOccurrenceReport <- function() {
  flog.info(Sys.time())

  # read a table into an R dataframe
  table_name<-"condition_occurrence"
  data_tbl <- cdm_tbl(req_env$db_src, table_name)
  concept_tbl <- vocab_tbl(req_env$db_src, "concept")

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(g_config$reporting$site_directory),
                       "./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name,g_config)
  
  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), 
                          issue_code=character(0), issue_description=character(0), check_alias=character(0)
                          , finding=character(0), prevalence=character(0))
  
  #PRIMARY FIELD
  field_name<-"condition_occurrence_id"
  df_total_condition_count<-retrieve_dataframe_count(data_tbl, field_name)
  current_total_count<-as.numeric(df_total_condition_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", 
                                   formatC(current_total_count, format="d", big.mark=','),"\n"))

  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name),NULL,current_total_count)) 
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  df_total_patient_count<-retrieve_dataframe_count(data_tbl, "person_id", distinction = T)
  fileContent<-c(fileContent,paste("The condition to patient ratio is ",
                                   round(df_total_condition_count[1][1]/df_total_patient_count[1][1],2),"\n"))

  df_total_visit_count<-retrieve_dataframe_count(data_tbl,"visit_occurrence_id", distinction = T)
  fileContent<-c(fileContent,paste("The condition to visit ratio is ",
                                   round(df_total_condition_count[1][1]/df_total_visit_count[1][1],2),"\n"))

  # visit concept id
  df_visit <-retrieve_dataframe_clause(concept_tbl, c("concept_id","concept_name"),
                                        "vocabulary_id == 'Visit' | (vocabulary_id =='PCORNet' & concept_class_id == 'Encounter Type') |
                                       (vocabulary_id == 'PCORNet' & concept_class_id=='Undefined') |
                                       (concept_class_id == 'Encounter Type' & domain_id == 'Visit')")

  #condition / person id by visit types
  fileContent <-c(fileContent,paste("## Barplot for Condition:Patient ratio by visit type\n"))
  
  df_condition_patient_ratio <- retrieve_dataframe_ratio_group_join(data_tbl,
                                                                    cdm_tbl(req_env$db_src,"visit_occurrence"),
                                                                    num = "condition_occurrence_id", "person_id",
                                                                    "visit_concept_id", "visit_occurrence_id")

  for(i in 1:nrow(df_condition_patient_ratio)){
    label<-df_visit[df_visit$concept_id==df_condition_patient_ratio[i,1],2]
    df_condition_patient_ratio[i,1]<-paste(df_condition_patient_ratio[i,1]," (",label,") ",sep="")
  }

  describeOrdinalField(df_condition_patient_ratio,table_name,"condition_concept_id_person_id_ratio");

  fileContent<-c(fileContent,paste_image_name(table_name,"condition_concept_id_person_id_ratio"));
  #NOMINAL Fields
  df_condition_type_concept_id<-retrieve_dataframe_clause(concept_tbl,
                                                          c("concept_id" ,"concept_name"),
                                          "concept_class_id =='Condition Type' & vocabulary_id=='PEDSnet'")

  # this is a nominal field - work on it
  field_name<-"condition_type_concept_id" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"condition_type_concept_id_dplyr.txt",
                                                   concept_tbl, data_tbl)) 

  df_condition_type_concept_id <-generate_df_concepts(table_name,"condition_type_concept_id_dplyr.txt",
                                                      concept_tbl)
  df_table_condition_type_enhanced<-EnhanceFieldValues(df_table,field_name,df_condition_type_concept_id);
  describeNominalField(df_table_condition_type_enhanced,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name),
                                                   list(
                                                   list(2000000092,2000000093,2000000094,  "inpatient header primary"),
                                                   list(2000000095,2000000096,2000000097,  "outpatient header 1st position"), 
                                                   list(2000000098,2000000099,2000000100,  "inaptient header 2nd position"), 
                                                   list(2000000101,2000000102,2000000103,  "outpatient header 2nd position"), 
                                                   list(2000000089,2000000090,2000000091,  "EHR problem list entry")
                                                   ), data_tbl)) 

  fact_type_count<-sum(df_table[df_table[,field_name] %in% c(2000000092,2000000093,2000000094,2000000098,2000000099,2000000100),2])
  write_total_fact_type_counts(table_name,"Inpatient_Condition_Headers" , fact_type_count)
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiffFactType(), c(table_name), c(field_name)
                                                   ,c("Inpatient_Conditon_Headers",fact_type_count))) 

  fact_type_count<-sum(df_table[df_table[,field_name] %in% c(2000000095,2000000096,2000000097,2000000101,2000000102,2000000103),2])
  write_total_fact_type_counts(table_name,"Outpatient_Condition_Headers" , fact_type_count)
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiffFactType(), c(table_name), c(field_name)
                                                   ,c("Outpatient_Condition_Headers",fact_type_count))) 
  
  fact_type_count<-sum(df_table[df_table[,field_name] %in% c(2000001280,2000001281,2000001282,2000001283,2000001284,2000001285),2])
  write_total_fact_type_counts(table_name,"ED_Condition_Headers" , fact_type_count)
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiffFactType(), c(table_name), c(field_name)
                                                   ,c("ED_Condition_Headers",fact_type_count)))

  # ORDINAL Fields
  field_name<-"condition_source_value"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  # some fields can have multiple vocabularies
  message<-describeOrdinalField(df_table, table_name,field_name, ggplotting = F)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  logFileData<-custom_rbind(logFileData,applyCheck(InvalidFormat(), c(table_name),c(field_name)
                                                   ,3, data_tbl))  ## number of components in condition_source_value

  field_name<-"condition_source_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)

  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  
  # some fields can have multiple vocabularies  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidVocab(), c(table_name),c(field_name),
                                                   c('Condition','ICD9','ICD9CM', 'ICD10', 'ICD10CM'),
                                                   concept_tbl, data_tbl)) 

  message<-describeOrdinalField(df_table, table_name,field_name,ggplotting = F)
  # create meaningful message
  new_message<-create_meaningful_message_concept_id(concept_tbl,message,field_name)
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));
  flog.info(Sys.time())
  field_name<-"condition_concept_id" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))

  # add % of no matching concept (concept id = 0). for the completeness report
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 

  ###########DQA CHECKPOINT -- invalid vocab ##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidVocab(), c(table_name),c(field_name),
                                                   c('Condition','SNOMED'), concept_tbl, data_tbl)) 
  
  message<-describeOrdinalField(df_table, table_name,field_name, ggplotting = F)

  # create meaningful message
  new_message<-create_meaningful_message_concept_id(concept_tbl, message,field_name)
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));

  field_name<-"condition_concept_id" #
  df_table_new<-retrieve_dataframe_count_group(data_tbl,"person_id", field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"by person_id","\n"))
  message<-describeOrdinalField(df_table_new, "person_id",field_name, ggplotting = F)

  # create meaningful message
  new_message<-create_meaningful_message_concept_id(concept_tbl, message,field_name)
  fileContent<-c(fileContent,new_message,paste_image_name("person_id",field_name));

  field_name<-"condition_start_date"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-describeDateField(df_table, table_name,field_name)
  
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name),
                                                   c(field_name),data_tbl)) 
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  field_name<-"condition_end_date"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeDateField(df_table, table_name,field_name)

  if(missing_percent<100)
  {
    ### DQA checkpoint - future date
    logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name),
                                                     c(field_name), data_tbl)) 
  }
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  logFileData<-custom_rbind(logFileData,applyCheck(ImplEvent(),c(table_name),
                                                   c('condition_start_date','condition_end_date'),data_tbl)) 
  
  field_name<-"stop_reason"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), table_name, field_name, data_tbl)) 
  message<-describeOrdinalField(df_table, table_name,field_name,ggplotting = F)
  fileContent<-c(fileContent, paste_image_name(table_name,field_name),message);

  #FOREIGN KEY fields
  field_name<-"person_id" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                 paste_image_name_sorted(table_name,field_name),message);

  field_name<-"visit_occurrence_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))

    ## compute missing % for visits stratified by condition_type_concept_id (problem list vs non-problem list)
    ## the expectation is that there shouldnt be any missing visit in non-problem list. and there could be missing for problem list entries
    count_nonproblemlist_novisit<-retrieve_dataframe_clause(data_tbl,"count(*)"
                                                            ,"is.na(visit_occurrence_id) & condition_type_concept_id != 38000245")
    count_nonproblemlist<-retrieve_dataframe_clause(data_tbl,"count(*)"
                                                    ,"condition_type_concept_id != 38000245")

    # compute % (# of records with missing visit info for problem list visits) / (# problem list visits)
    missing_visit_percent_nonproblemlist<-round(count_nonproblemlist_novisit*100/count_nonproblemlist,2)
    message_visit_percent_nonproblemlist<-paste("The percentage of conditions (besides problem list) with missing information:",
                                                missing_visit_percent_nonproblemlist,"%")
    fileContent<-c(fileContent,message_visit_percent_nonproblemlist)

    ###########DQA CHECKPOINT -- missing information##############
    #missing_percent<-extract_numeric_value(message)
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name,
                                                             missing_visit_percent_nonproblemlist,
                                                             table_name, g_data_version));

    fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                   paste_image_name_sorted(table_name,field_name),message);

    field_name<-"provider_id"
    df_table<-retrieve_dataframe_group(data_tbl,field_name)
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
    message<-reportMissingCount(df_table,table_name,field_name)
    fileContent<-c(fileContent,message)

    ###########DQA CHECKPOINT -- missing information##############
    missing_percent<-extract_numeric_value(message)
    logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
    message<-describeForeignKeyIdentifiers(df_table, table_name,field_name)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                   paste_image_name_sorted(table_name,field_name),message);

       flog.info(Sys.time())

   ## condition status source value 
    field_name<-"condition_status_source_value"
    df_table<-retrieve_dataframe_group(data_tbl,field_name)
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
    missing_percent_message<-reportMissingCount(df_table,table_name,field_name)
    missing_percent<- extract_numeric_value(missing_percent_message)
    fileContent<-c(fileContent,missing_percent_message)
    
    ###########DQA CHECKPOINT#############
    # this is a nominal field - work on it
    field_name<-"condition_status_concept_id" #
    df_table<-retrieve_dataframe_group(data_tbl,field_name)
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
    missing_percent_message<-reportMissingCount(df_table,table_name,field_name)
    missing_percent<- extract_numeric_value(missing_percent_message)
    fileContent<-c(fileContent,missing_percent_message)

    ###########DQA CHECKPOINT##############
    logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                     ,"condition_status_concept_id.csv", concept_tbl,
                                                     data_tbl)) 

    # this is a nominal field - work on it
    field_name <- "poa_concept_id" 
    ###This is a new field not currently in oracle test db
    if(field_name %in% colnames(data_tbl)){
    df_table<-retrieve_dataframe_group(data_tbl,field_name)

    fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
    missing_percent_message<-reportMissingCount(df_table,table_name,field_name)
    missing_percent<- extract_numeric_value(missing_percent_message)
    fileContent<-c(fileContent,missing_percent_message)
    ###########DQA CHECKPOINT##############

    logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                     ,"poa_concept_id.csv", concept_tbl, data_tbl)) 
 
    describeNominalField(df_table,table_name,field_name)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name));
    }
    
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
}
