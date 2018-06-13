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
  
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
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
  
  #### Check for unexpected differences from prev cycle
  fact_type_count<-df_table[df_table$measurement_type_concept_id==44818702,2]
  write_total_fact_type_counts(table_name,"Labs" , fact_type_count)
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiffFactType(), c(table_name), c(field_name)
                                                   ,c("Labs",fact_type_count))) 
  
  fact_type_count<-df_table[df_table$measurement_type_concept_id==2000000033|
                              df_table$measurement_type_concept_id==2000000032,2]
  write_total_fact_type_counts(table_name,"Vitals" , fact_type_count)
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiffFactType(), c(table_name), c(field_name)
                                                   ,c("Vitals",fact_type_count))) 

  
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
                                                   list(
                                                   list(2000000033,  2000000032, "vital signs"),
                                                   list(44818702, "lab records"))) )
  
  

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
  
  
  
  ### Vitals 
  df_vitals<-retrieve_dataframe_group_clause(con,g_config,table_name,field_name, "measurement_type_concept_id in (2000000033, 2000000032)")
  acceptable_vitals<-generate_list_concepts(table_name,"vital_list.csv")$concept_id ## read from vitals list
  acceptable_fevs<-generate_list_concepts(table_name,"fev_list.csv")$concept_id ## read from fev list
  acceptable_vitals<-rbind(acceptable_vitals, acceptable_fevs)
  unexpected_message<- reportUnexpected(df_vitals,table_name,field_name,acceptable_vitals,big_data_flag)
  if(length(trim(unexpected_message))>1)
	  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, paste0("VITALS: ",unexpected_message), table_name, 
                                                           g_data_version));
  
  ### patient reported 
   df_pro<-retrieve_dataframe_group_clause(con,g_config,table_name,field_name, "measurement_type_concept_id in (44818704)")
  acceptable_pt_reported<-generate_list_concepts(table_name,"patient_reported_list.csv")$concept_id ## read from vitals list
  unexpected_message<- reportUnexpected(df_pro,table_name,field_name,acceptable_pt_reported,big_data_flag)
  if(length(trim(unexpected_message))>1)
	  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, paste0("Patient reported: "
	  ,unexpected_message), table_name, 
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
  ### CBC (complete blood count) components: 
  
	## white blood cell
	logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name),con, 
	                                                 list(
                                                   list(3010813, 1125563, 228323, 27245,
                                                     " White Blood cell (WBC) count (leukocyte)"),
                                                   list(3020416,  " Erythrocytes /volume in Blood by Automated count"),
                                                   list(3009744,  " Erythrocyte mean corpuscular hemoglobin concentration [Mass/volume] by Automated count"), 
                                                   list(3012030,  " Erythrocyte mean corpuscular hemoglobin [Entitic mass] by Automated count"), 
                                                   list(3002385,3049383, 3002888,  3019897, " Erythrocyte distribution width [Ratio]"), 
                                                   list(3009542, 3023230, 3028813, 
                                                     3034976, 3034037, 3013752, 3023314, 
                                                     "Hematocrit"), 
                                                   list(3043111, 3007461,3024929,3010834,  " 	Platelet count"), 
                                                   list(3000963,  " Hemoglobin (Hbg)"), 
                                      ### CMP 14
  	                                                 list(3024561, 3000034, 3049506, 3001802,  "Albumin"),  
  	                                                 list(3013682,  "Blood Urea Nitrogen (BUN)"),
  	                                                 list(3006490, 3006906, 3007501, 3021119,  "Calcium"), 
  	                                                 list(3016293,3015632,  "Carbon Dioxide (Bicarbonate)"), 
  	                                                 list(3014576,  "Chloride"), 
  	                                                 list(3016723, 3017250, 3030354
  	                                                   , 3001802, 3001582,  "creatinine"), 
  	                                                 list(3015621, 3000845, 3037187, 
  	                                                   3004501, 3022548, 3020399, 
  	                                                   3004501, 3024629,  "Glucose"), 
  	                                                 list(3023103,  "Potassium"), 
  	                                                 list(3019550, 3000285,  "Sodium"), 
  	                                                 list(3019676, 3024128, 3007359
  	                                                   , 3027597, 3018834,  "Total Bilirubin"), 
  	                                                 list(3010156, 3020460, 3023221, 
  	                                                   3017756, 3001582, 3020630, 3019473, 3005897,  "Total Protein"), 
  	                                                 list(3006923,  "Alanine Aminotransferase (ALT)"), 
  	                                                 list(3035995,  "Alkaline Phosphatase (ALP)"),
  	                                                 list(3013721,  "Aspartate Aminotransferase (AST)")
  	                                                 ))) 
  	
  	concept_id_list <- unique(df_table[,1])



  ## compute missing % for labs only 
  field_name<-"specimen_source_value" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- missing information##############
  #logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  
  
  ## the expectation is that there shouldnt be any missing visit in non-problem list. and there could be missing for problem list entries
  count_lab_no_specimen<-retrieve_dataframe_clause(con,g_config,g_config$db$schema,table_name,"count(*)"
                                                          ,"specimen_source_value is null and measurement_type_concept_id = 44818702")
  count_lab<-retrieve_dataframe_clause(con,g_config,g_config$db$schema,table_name,"count(*)"
                                                  ,"measurement_type_concept_id = 44818702")
  # compute % (# of records with missing visit info for problem list visits) / (# problem list visits)
  missing_specimen_percent_lab<-round(count_lab_no_specimen*100/count_lab,2)
 ###########DQA CHECKPOINT -- missing information##############
  #missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_specimen_percent_lab, table_name, g_data_version));
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidFormat(), c(table_name),c(field_name)
                                                   ,con,  2))  ## number of components in _source_value
  
  #message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  #fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);
  
 # specimen concept id
  df_concept_id<- generate_df_concepts(con, table_name,"specimen_concept_id.txt")
  order_bins <-c(df_concept_id$concept_id,0)
  field_name<-"specimen_concept_id" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "specimen_concept_id.txt")) 
  
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeOrdinalField(df_table,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  
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
