generateProcedureOccurrenceReport <- function() {
  flog.info(Sys.time())

  big_data_flag<-TRUE # for query wise analysis

  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)
  # read a table into an R dataframe
  table_name<-"procedure_occurrence"
  #df_table <- retrieve_dataframe_OHDSI(con, g_config,table_name)

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), issue_description=character(0)
                          , finding=character(0), prevalence=character(0))


  #PRIMARY FIELD
  field_name<-"procedure_occurrence_id"
  df_total_procedure_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  current_total_count<-as.numeric(df_total_procedure_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
  prev_total_count<-get_previous_cycle_total_count( g_config$reporting$site, table_name)
  percentage_diff<-get_percentage_diff(prev_total_count, current_total_count)
  fileContent<-c(fileContent, get_percentage_diff_message(percentage_diff))
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,apply_check_type_0("CA-005", percentage_diff, table_name, g_data_version));

  df_total_patient_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct person_id")
  fileContent<-c(fileContent,paste("The procedure to patient ratio is ",round(df_total_procedure_count[1][1]/df_total_patient_count[1][1],2),"\n"))
  df_total_visit_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct visit_occurrence_id")
  fileContent<-c(fileContent,paste("The procedure to visit ratio is ",round(df_total_procedure_count[1][1]/df_total_visit_count[1][1],2),"\n"))

  # visit concept id
  df_visit <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name"
      ,"vocabulary_id ='Visit' or (vocabulary_id='PCORNet' and concept_class_id='Encounter Type')
                                       or (vocabulary_id = 'PCORNet' and concept_class_id='Undefined')")

  #procedure / person id by visit types
  fileContent <-c(fileContent,paste("## Barplot for Procedure:Patient ratio by visit type\n"))
  df_procedure_patient_ratio<-retrieve_dataframe_ratio_group_join(con, g_config,table_name, "visit_occurrence",
                                                       "round(count(distinct procedure_occurrence_id)/count(distinct procedure_occurrence.person_id),2)",
                                                               "visit_concept_id","visit_occurrence_id")

    for(i in 1:nrow(df_procedure_patient_ratio))
    {
       #df_visit[df_visit$concept_id==9201,2]
        label<-df_visit[df_visit$concept_id==df_procedure_patient_ratio[i,1],2]
        df_procedure_patient_ratio[i,1]<-paste(df_procedure_patient_ratio[i,1],"(",label,")",sep="")
       #df_visit_patient_ratio[i,1]
      }
  describeOrdinalField(df_procedure_patient_ratio,table_name,"Procedure:Patient ratio by visit type",big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,"Procedure:Patient ratio by visit type"));

  #procedure type concept id
  field_name="procedure_type_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650,big_data_flag)

  order_bins <-c("38000275","0",NA)
  label_bins<-c("EHR order list entry (38000275)","Other (0)","NULL")
  color_bins <-c("38000275"="lightcoral","0"="steelblue1")
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version));
  fileContent<-c(fileContent,unexpected_message)
  describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

   flog.info(Sys.time())
  # ORDINAL Fields

  field_name<-"procedure_source_value" #  3 minutes
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  # some fields can have multiple vocabularies
  #fileContent<-c(fileContent,paste("\n The source vocabulary is",get_vocabulary_name_by_concept_codes(con, g_config, g_config$db$schema,table_name, field_name,  g_config$db$vocab_schema,"PROCEDURE"),"\n"))
  message<-describeOrdinalField_large(df_table, table_name, field_name,big_data_flag)
  #new_message<-create_meaningful_message_concept_code(message,field_name,con, g_config)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

   flog.info(Sys.time())


  field_name<-"procedure_source_concept_id" #  3 minutes
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  # some fields can have multiple vocabularies
  used_vocabulary<-get_vocabulary_name_by_concept_ids(con, g_config, table_name, field_name, "PROCEDURE")
  fileContent<-c(fileContent,paste("\n The source vocabulary is",used_vocabulary,"\n"))
  if(!is.na(used_vocabulary))
  {
    # check if each vocabulary is one of the prescribed ones
    for(vocab_index in 1:length(unlist(strsplit(used_vocabulary,"\\|"))))
    {
      vocabulary<-unlist(strsplit(used_vocabulary,"\\|"))[vocab_index];
      if(grepl("ICD",vocabulary)||grepl("CPT",vocabulary)
         ||grepl("HCPCS",vocabulary)||grepl("OPCS",vocabulary))
      {

      } else {
        ###########DQA CHECKPOINT -- vocabulary incorrect ##############
        logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-005", field_name,
                                                                  paste("invalid vocabulary found:",vocabulary," ; please use {ICD-9, ICD-10 Proc, CPT-4, HCPCS, or OPCS-4} only"), table_name, g_data_version));
      }
    }
  }
  message<-describeOrdinalField_large(df_table, table_name, field_name,big_data_flag)
  new_message<-""
  if(length(message)>0)
  {
    new_message<-create_meaningful_message_concept_id(message,field_name,con, g_config)
  }
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));

   flog.info(Sys.time())

  field_name<-"procedure_concept_id" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  # add % of no matching concept (concept id = 0). for the completeness report
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  ###########DQA CHECKPOINT -- no matching concept percentage ##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version)); # custom threshold
  # some fields can have multiple vocabularies
  used_vocabulary<-get_vocabulary_name_by_concept_ids(con, g_config, table_name, field_name, "PROCEDURE")
  fileContent<-c(fileContent,paste("\n The vocabulary used is",used_vocabulary,"\n"))
  if(!is.na(used_vocabulary))
  {
    # check if each vocabulary is one of the prescribed ones
    for(vocab_index in 1:length(unlist(strsplit(used_vocabulary,"\\|"))))
    {
      vocabulary<-unlist(strsplit(used_vocabulary,"\\|"))[vocab_index];
      if(grepl("ICD",vocabulary)||grepl("CPT",vocabulary)
         ||grepl("HCPCS",vocabulary)||grepl("SNOMED",vocabulary))
      {

      } else {
        ###########DQA CHECKPOINT -- vocabulary incorrect ##############
        logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-005", field_name,
                                                                  paste("invalid vocabulary found:",vocabulary," ; please use {ICD-9, ICD-10 Proc, CPT-4, HCPCS, or OPCS-4} only"), table_name, g_data_version));
      }
    }
  }
  message<-describeOrdinalField_large(df_table, table_name, field_name,big_data_flag)
  new_message<-create_meaningful_message_concept_id(message,field_name,con, g_config)
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));

   flog.info(Sys.time())

  field_name<-"procedure_concept_id" #
  df_table_new<-retrieve_dataframe_count_group(con, g_config,table_name,"person_id", field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"by person_id","\n"))
  message<-describeOrdinalField_large(df_table_new, "person_id",field_name,big_data_flag)
  # create meaningful message
  new_message<-create_meaningful_message_concept_id(message,field_name,con, g_config)
  #fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));
  fileContent<-c(fileContent,new_message,paste_image_name("person_id",field_name));

  field_name<-"procedure_date" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeDateField(df_table, table_name, field_name,big_data_flag)
  ###########DQA CHECKPOINT##############
  if(grepl("future",message[3]))
  {
    logFileData<-custom_rbind(logFileData,apply_check_type_1("CA-001", field_name, "procedures cannot occur in the future", table_name, g_data_version));
  }
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);

  field_name<-"procedure_time" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeTimeField(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_time",sep="")));

   flog.info(Sys.time())

  field_name<-"modifier_source_value" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,missing_message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(missing_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent_source_value, table_name, g_data_version));
  if(grepl("100",missing_message)==FALSE) # if 100% missing
  {
    message<-describeOrdinalField_large(df_table, table_name, field_name,big_data_flag)
    fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  }
   flog.info(Sys.time())


  field_name="modifier_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)

  df_modifier <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name"
                                          ,"concept_class_id like '%Modifier%'")

  order_bins <-c(df_modifier$concept_id,44814650,0,NA)
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version));
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));
  fileContent<-c(fileContent,unexpected_message)
  df_table_modifier_enhanced<-EnhanceFieldValues(df_table,field_name,df_modifier);
  missing_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,missing_message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(missing_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  if(grepl("100",missing_message)==FALSE) # if not 100% missing
  {
    describeNominalField_basic(df_table_modifier_enhanced,table_name,field_name,big_data_flag);
    fileContent<-c(fileContent,paste_image_name(table_name,field_name));

    ## run the CA-014 DQA check only if there is some data in modifier_concept_id
    ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
    logFileData<-custom_rbind(logFileData,apply_check_type_2("CA-014", field_name,"modifier_source_value",
                                                             (missing_percent_source_value-
                                                                extract_ni_missing_percent( null_message)), table_name, g_data_version))
  }






  field_name<-"qualifier_source_value" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,missing_message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(missing_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  if(grepl("100",missing_message)==FALSE) # if 100% missing
  {
    message<-describeOrdinalField_large(df_table, table_name, field_name,big_data_flag)
    fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  }
   flog.info(Sys.time())

  field_name<-"quantity" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeOrdinalField_large(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  #FOREIGN KEY fields

  field_name<-"person_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

   flog.info(Sys.time())

    field_name<-"visit_occurrence_id"
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))

    df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
    message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,message)
    ###########DQA CHECKPOINT -- missing information##############
    missing_percent<-extract_numeric_value(message)
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
    message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

    flog.info(Sys.time())

    field_name<-"provider_id" #  4 minutes
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
