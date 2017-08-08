generateMeasurementReport <- function() {
  flog.info(Sys.time())

  big_data_flag<-TRUE

  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)

  table_name<-"measurement"
  #df_table <- retrieve_dataframe_OHDSI(con, g_config,table_name)
  # flog.info(nrow(df_table))

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), 
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))

  test <-1

  #PRIMARY FIELD
  field_name<-"measurement_id"
  df_total_measurement_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  current_total_count<-as.numeric(df_total_measurement_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name),NULL, current_total_count)) 
  
  
  df_total_patient_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct person_id")
  fileContent<-c(fileContent,paste("The measurement to patient ratio is ",round(df_total_measurement_count[1][1]/df_total_patient_count[1][1],2),"\n"))
  df_total_visit_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct visit_occurrence_id")
  fileContent<-c(fileContent,paste("The measurement to visit ratio is ",round(df_total_measurement_count[1][1]/df_total_visit_count[1][1],2),"\n"))

  # visit concept id
  df_visit <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name"
                                     ,"vocabulary_id ='Visit' or (vocabulary_id='PCORNet' and concept_class_id='Encounter Type')
                                       or (vocabulary_id = 'PCORNet' and concept_class_id='Undefined')")

  #measurement / person id by visit types
  fileContent <-c(fileContent,paste("## Barplot for Measurement:Patient ratio by visit type\n"))
  df_measurement_patient_ratio<-retrieve_dataframe_ratio_group_join(con, g_config,table_name, "visit_occurrence",
                                        "round(count(distinct measurement_id)/count(distinct measurement.person_id),2)",
                                                           "visit_concept_id","visit_occurrence_id")
  for(i in 1:nrow(df_measurement_patient_ratio))
      {
         #df_visit[df_visit$concept_id==9201,2]
          label<-df_visit[df_visit$concept_id==df_measurement_patient_ratio[i,1],2]
          df_measurement_patient_ratio[i,1]<-paste(df_measurement_patient_ratio[i,1],"(",label,")",sep="")
          #df_visit_patient_ratio[i,1]
          }
  describeOrdinalField(df_measurement_patient_ratio,table_name,"Measurement:Patient ratio by visit type",big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,"Measurement:Patient ratio by visit type"));


  #NOMINAL Fields

  field_name<-"person_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

  # flog.info(fileContent)

  field_name<-"provider_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

  field_name<-"visit_occurrence_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))

    df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
    ###########DQA CHECKPOINT -- missing information##############
    logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
    message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

  # ORDINAL Fields
   flog.info(Sys.time())
  field_name<-"measurement_date" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
  
  
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  field_name<-"measurement_datetime" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeTimeField(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_datetime",sep="")));

  field_name<-"measurement_result_date" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
  
  
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  #field_name<-"measurement_result_datetime" #
  #df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  #fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  #fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  #missing_percent<-extract_numeric_value(message)
  #logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  #message<-describeTimeField(df_table, table_name,field_name,big_data_flag)
  ###########DQA CHECKPOINT -- future dates##############
  #fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));



  field_name<-"measurement_order_date" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
  
  
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  #field_name<-"measurement_order_datetime" #
  #df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  #fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  #fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  #missing_percent<-extract_numeric_value(message)
  #logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  #message<-describeTimeField(df_table, table_name,field_name,big_data_flag)
  ###########DQA CHECKPOINT -- future dates##############
  #fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));



  # not plotting the value_as_string column as it's a free text field
  df_value_as_concept_id<- generate_df_concepts(con, table_name,"value_as_concept_id.txt")
  order_bins <-c(df_value_as_concept_id$concept_id,0)
  field_name<-"value_as_concept_id" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "value_as_concept_id.txt")) 
  
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeOrdinalField(df_table,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));



  #print (fileContent)
  field_name<-"unit_source_value" # 3 minutes
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
 ###########DQA CHECKPOINT -- missing information##############
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);



  #unit concept id
  df_unit <-generate_df_concepts(con, table_name,"unit_concept_id.txt")
  
  order_bins <-c(df_unit$concept_id,0,44814650,NA)
  field_name="unit_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "unit_concept_id.txt")) 
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  df_table_unit_enhanced<-EnhanceFieldValues(df_table,field_name,df_unit);
  describeNominalField_basic(df_table_unit_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650,big_data_flag)
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "unit_source_value"),con
  )) 
  

  #Operator Concept Id
  field_name = "operator_concept_id"
  order_bins <-c("4171754","4171755","4171756","4172703","4172704","0",NA)
  label_bins<-c("<= (4171754)",">= (4171755)","< (4171756)","= (4172703)","> (4172704)","No Match (0 )","NULL")
  color_bins <-c("4171754"="lightcoral","4171755"="steelblue1","4171756"="red","4172703"="grey64","4172704"="grey64","0 "="grey64")
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  #
  # flog.info( null_message)
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  #unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "operator_concept_id.csv")) 
  # flog.info(unexpected_message)
  #fileContent<-c(fileContent,unexpected_message)
  describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));


  #print (fileContent)

  field_name<-"priority_source_value" # 3 minutes
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- missing information##############
  missing_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,missing_message)
  missing_percent_source_value<-extract_numeric_value(missing_message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  if(grepl("100",missing_message)==FALSE) # if 100% missing
  {
    message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);
  }


  #priority concept id
  df_unit <-generate_df_concepts(con, table_name,"priority_concept_id.txt")
  order_bins <-c(df_unit$concept_id,0,44814650,44814653,44814649,NA)
  field_name="priority_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  #unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "priority_concept_id.txt")) 
  
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,unexpected_message)
  df_table_unit_enhanced<-EnhanceFieldValues(df_table,field_name,df_unit);
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT -- missing information##############
  missing_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,missing_message)
  missing_percent<-extract_numeric_value(missing_message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  if(grepl("100",missing_message)==FALSE) # if not 100% missing
  {
    describeNominalField_basic(df_table_unit_enhanced,table_name,field_name,big_data_flag);
    fileContent<-c(fileContent,paste_image_name(table_name,field_name));


    null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650,big_data_flag)
    
    ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
    logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "priority_source_value"),con
    )) 
    
  }



  field_name = "measurement_type_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  order_bins <-c("2000000033","2000000032","44818702","44818703","44818704","45754907",NA)
  label_bins<-c("Vital Sign from healthcare delivery setting (2000000033)","Vital Sign from healthcare device (2000000032)",
                "Lab result (44818702)","Pathology finding (44818703)","Patient reported value (44818704)",
                "Derived Value (45754907)","NULL")
  color_bins <-c("2000000033"="lightcoral","2000000032"="steelblue1","44818702"="red","44818703"="grey64","44818704"="grey64","45754907"="grey64")
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  #unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "measurement_type_concept_id.csv")) 
  #fileContent<-c(fileContent,unexpected_message)
  describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name),con, 
                                                   c(2000000033,  2000000032, "vital signs"))) 
  
  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name),con, 
                                                   c(44818702, "lab records"))) 
  

  #print (fileContent)

  field_name="measurement_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);

  field_name<-"measurement_source_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  new_message<-""
  if(length(message)>0)
  {
    # create meaningful message
    new_message<-create_meaningful_message_concept_id(message,field_name,con, g_config)
  }
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));

  # this is a nominal field - work on it
  #measurement Concept Id
  ## reading specific subset of the concept table to retrieve race concepts
  df_measurement_concept <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name"
                                      ,"(domain_id='Measurement' and  (invalid_reason is null or invalid_reason=''))
                                   or (vocabulary_id = 'PCORNet' and (concept_class_id = 'Undefined' or concept_class_id = 'UnDefined'))")
  
  order_bins <-c(df_measurement_concept$concept_id,0,NA)

  
  field_name<-"measurement_concept_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  
  
  ###########DQA CHECKPOINT############## FOR LABS only 
  df_labs<-retrieve_dataframe_group_clause(con,g_config,table_name,field_name, "measurement_type_concept_id=44818702")
  acceptable_fevs<-generate_list_concepts(table_name,"fev_list.csv")$concept_id ## read from lablist
  acceptable_labs<-generate_list_concepts(table_name,"lab_list.csv")$concept_id ## read from lablist
  acceptable_labs<-c(acceptable_labs, acceptable_fevs)
  unexpected_message<- reportUnexpected(df_labs,table_name,field_name,acceptable_labs,big_data_flag)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, 
                                                           g_data_version));
  
  
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name, big_data_flag))
  # add % of no matching concept (concept id = 0). for the completeness report
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  # create meaningful message -- adding description labels to top 5 concept identifiers
  new_message<-create_meaningful_message_concept_id(message,field_name,con, g_config)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),new_message);
  # flog.info(fileContent)

  ## Specific lab checks
  ## for creatinine
  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name),con, 
                                                   c(3016723,3017250,  "creatinine"))) 

  concept_id_list <- unique(df_table[,1])



  ## compute missing % for labs only 
  field_name<-"specimen_source_value" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  
  ## the expectation is that there shouldnt be any missing visit in non-problem list. and there could be missing for problem list entries
  count_lab_no_specimen<-retrieve_dataframe_clause(con,g_config,g_config$db$schema,table_name,"count(*)"
                                                          ,"specimen_source_value is null and measurement_type_concept_id = 44818702")
  count_lab<-retrieve_dataframe_clause(con,g_config,g_config$db$schema,table_name,"count(*)"
                                                  ,"measurement_type_concept_id = 44818702")
  # compute % (# of records with missing visit info for problem list visits) / (# problem list visits)
  missing_specimen_percent_lab<-round(count_lab_no_specimen*100/retrieve_dataframe_clause,2)
 ###########DQA CHECKPOINT -- missing information##############
  #missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_specimen_percent_lab, table_name, g_data_version));
  
  #message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  #fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);
  
  
  
  #generating concept wise graphs for numerical readings

  dqa_for_lab<-FALSE


  if(dqa_for_lab==TRUE)
  {
  field_name<-"value_as_number"
  #column_index <- which(colnames(df_table)==field_name)
  for (i in 1:length(concept_id_list))
  {
    df_table_subset<-retrieve_dataframe_group_clause(con, g_config,table_name,field_name,paste(" measurement_concept_id=",concept_id_list[i]))
    field_name_subset<-paste(field_name,concept_id_list[i],sep="_")
    colnames(df_table_subset)[1] <- field_name_subset
    fileContent <-c(fileContent,paste("## Barplot for",field_name_subset,"(",get_concept_name(concept_id_list[i],con, g_config),")","\n"))
    #check if missing % is > 0 and if yes, add the dictionary codes BA-001  Missing Information
    missing_message<-reportMissingCount(df_table_subset,table_name,field_name_subset,big_data_flag)
    if(!strEndsWith(missing_message," 0%"))
    {
      fileContent<-c(fileContent,paste("DQA WARNING","BA-001|Missing Information",missing_message))
    }

    message<-describeRatioField(df_table_subset, table_name,field_name_subset,"",big_data_flag)
    fileContent<-c(fileContent,message,paste_image_name(table_name,field_name_subset));

    #print (fileContent)
    #break;
    # compute how many value as numbers are out of range
    df_out_of_range_values<-retrieve_dataframe_clause(con, g_config, g_config$db$schema,table_name,"count(*)"
                                                      ,paste("(value_as_number>range_high or value_as_number < range_low) and measurement_concept_id="
                                                             ,concept_id_list[i])
    )
    if(df_out_of_range_values[1][1]>0)
      fileContent<-c(fileContent,paste("DQA WARNING: There are ",df_out_of_range_values[1][1]," out of range values in value_as_number based on range_low and range_high"));

  }
  }

  field_name<-"range_high" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  

  field_name<-"range_high_source_value" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  field_name<-"range_high_operator_concept_id" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT######################
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "operator_concept_id.csv")) 

  #print (fileContent)

  field_name<-"range_low" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  field_name<-"range_low_source_value" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  field_name<-"range_low_operator_concept_id" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT################
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "operator_concept_id.csv")) 

  # other fields not plotted : operator concept id, value as concept id, value source value, measurement source concept id
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  # write to log file
  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
  #close the connection
  close_database_connection_OHDSI(con, g_config)
}
