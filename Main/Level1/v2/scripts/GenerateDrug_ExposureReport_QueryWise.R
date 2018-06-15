flog.info(Sys.time())

generateDrugExposureReport <- function() {
  big_data_flag<-TRUE

  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)

  table_name<-"drug_exposure"
  #df_table <- retrieve_dataframe_OHDSI(con, g_config,table_name)
  # flog.info(nrow(df_table))

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0),
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))


  test <-1

  #PRIMARY FIELD
  field_name<-"drug_exposure_id"
  df_total_measurement_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  current_total_count<-as.numeric(df_total_measurement_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
 ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 
  
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  
  df_total_patient_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct person_id")
  fileContent<-c(fileContent,paste("The drug exposure to patient ratio is ",round(df_total_measurement_count[1][1]/df_total_patient_count[1][1],2),"\n"))
  df_total_visit_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct visit_occurrence_id")
  fileContent<-c(fileContent,paste("The drug exposure to visit ratio is ",round(df_total_measurement_count[1][1]/df_total_visit_count[1][1],2),"\n"))

  # visit concept id
  df_visit <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name"
                                       ,"vocabulary_id ='Visit' or (vocabulary_id='PCORNet' and concept_class_id='Encounter Type')
                                       or (vocabulary_id = 'PCORNet' and concept_class_id='Undefined')")

  #condition / person id by visit types
  fileContent <-c(fileContent,paste("## Barplot for Drug:Patient ratio by visit type\n"))
  df_drug_patient_ratio<-retrieve_dataframe_ratio_group_join(con, g_config,table_name,"visit_occurrence",
                                                                  "round(count(distinct drug_exposure_id)/count(distinct drug_exposure.person_id),2)",
                                                                  "visit_concept_id","visit_occurrence_id")

  for(i in 1:nrow(df_drug_patient_ratio))
  {
    #df_visit[df_visit$concept_id==9201,2]
    label<-df_visit[df_visit$concept_id==df_drug_patient_ratio[i,1],2]
    df_drug_patient_ratio[i,1]<-paste(df_drug_patient_ratio[i,1],"(",label,")",sep="")
    #df_visit_patient_ratio[i,1]
  }
  describeOrdinalField(df_drug_patient_ratio,table_name,"Drug:Patient ratio by visit type",big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,"Drug:Patient ratio by visit type"));

  #NOMINAL Fields

  field_name<-"person_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

  field_name<-"drug_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  
  field_name<-"drug_source_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  if(nrow(df_table)>1)
  {
    fileContent<-c(fileContent,paste("\n The source vocabulary is",get_vocabulary_name(df_table[2,1],con, g_config),"\n"))
  }
  if(nrow(df_table)==1)
  {
      if(is.na(df_table[1,1]))
      {
          fileContent<-c(fileContent,paste("\n The source vocabulary is NA \n"))
      }
      else
      {
          fileContent<-c(fileContent,paste("\n The source vocabulary is",get_vocabulary_name(df_table[1,1],con, g_config),"\n"))
      }
  }
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  new_message<-""
  if(length(message)>0)
  {
    # create meaningful message
    new_message<-create_meaningful_message_concept_id(message,field_name,con, g_config)
  }
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));

  
  #print("Crossed drug source concept id")
  
   flog.info(Sys.time())
  field_name<-"drug_concept_id" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  #message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  #fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  # add % of no matching concept (concept id = 0). for the completeness report
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  
  ### DQA CHECKPOINT ##########
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidVocab(), c(table_name),c(field_name),con, 
                                                   c('Drug','RxNorm'))) 
  
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  # create meaningful message
  new_message<-create_meaningful_message_concept_id(message,field_name,con, g_config)
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));


   null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650 ,big_data_flag)
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "drug_source_value"),con
  )) 
  
  #if(no_matching_concept_number>10)
  #{
  #  fileContent<-c(fileContent,"###Frequent source values for non-matching concepts:")
  #  df_no_match_result<-retrieve_dataframe_top_20_clause(con,g_config,
  #                                                       table_name,"drug_source_value","drug_concept_id=0")
  #  fileContent<-c(fileContent,print_2d_dataframe(df_no_match_result))
  #}

  fileContent<-c(fileContent,"###Distribution of drug concept class:")
  df_concept_class<-retrieve_dataframe_join_clause_group(con, g_config,g_config$db$schema,table_name, g_config$db$vocab_schema,"concept","concept_class_id",
                                                         "drug_concept_id = concept_id and drug_concept_id<>0")
  fileContent<-c(fileContent,print_2d_dataframe(df_concept_class))

  ### ingredient-level normalization
  
  # also draw distribution of drug concept id vs person_id
  field_name<-"drug_concept_id" #
  df_table_new<-retrieve_dataframe_count_group(con, g_config,table_name,"person_id", field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"by person_id","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table_new,table_name,field_name,big_data_flag))
  #fileContent<-c(fileContent,"\n The standard vocabulary is RxNorm \n")
  message<-describeOrdinalField_large(df_table_new, "person_id",field_name,big_data_flag)
  # create meaningful message
  new_message<-create_meaningful_message_concept_id(message,field_name,con, g_config)
  fileContent<-c(fileContent,new_message,paste_image_name("person_id",field_name));


  field_name<-"drug_exposure_start_datetime"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
  
  
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  message<-describeTimeField(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_datetime",sep="")));


  field_name<-"drug_exposure_end_datetime"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  if(missing_percent<100)
  {
    message<-describeDateField(df_table, table_name,field_name,big_data_flag)
    ### DQA checkpoint - future date
    logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
    
    
    fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
    message<-describeTimeField(df_table, table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_datetime",sep="")));
  }
  #implausible event clause
  logFileData<-custom_rbind(logFileData,applyCheck(ImplEvent(), c(table_name), 
                                                   c('drug_exposure_start_date','drug_exposure_end_date'),con)) 
  

  # drug exposure end date
  field_name<-"drug_exposure_end_date"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)

  field_name<-"drug_exposure_order_datetime"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)
  ###########DQA CHECKPOINT -- future dates ##############
  if(missing_percent!=100)
  {
    ### DQA checkpoint - future date
    logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
    
    
  }
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  message<-describeTimeField(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_datetime",sep="")));

  # drug exposure order date
  field_name<-"drug_exposure_order_date"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)


  #drug type concept id
  field_name="drug_type_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  
  #### Check for unexpected differences from prev cycle
  

  fact_type_count<-df_table[df_table$drug_type_concept_id==38000175,2]
  write_total_fact_type_counts(table_name,"Dispensing" , fact_type_count)
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiffFactType(), c(table_name), c(field_name)
                                                   ,c("Dispensing",fact_type_count))) 
  
  fact_type_count<-df_table[df_table$drug_type_concept_id==38000180,2]
  write_total_fact_type_counts(table_name,"InpatientMAR" , fact_type_count)
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiffFactType(), c(table_name), c(field_name)
                                                   ,c("InpatientMAR",fact_type_count))) 
  
  fact_type_count<-df_table[df_table$drug_type_concept_id==38000177,2]
  write_total_fact_type_counts(table_name,"Prescriptions" , fact_type_count)
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiffFactType(), c(table_name), c(field_name)
                                                   ,c("Prescriptions",fact_type_count))) 
  
  
  order_bins <-c("38000175","38000180","38000177","0",NA)
  label_bins<-c("Prescription dispensed in pharmacy (dispensed meds pharma information) (38000175)"
                ,"Inpatient administration (MAR entries) (38000180)"
                ,"Prescription written (38000177)","Other (0)","NULL")
  color_bins <-c("38000175"="lightcoral","38000180"="steelblue1","38000177"="red","0"="grey64")
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "drug_type_concept_id.csv")) 

  #fileContent<-c(fileContent,unexpected_message)
  describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  ## DQA check for recall of various drug types
  
  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name),con, 
                                                   list(
                                                     list(38000175,  "Dispensing"),
                                                   list(38000180,  "Inpatient Administration"),
                                                   list(38000177,  "prescription") 
  )))
  

  field_name<-"stop_reason"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent, paste_image_name(table_name,field_name),message);
  #print (fileContent)

  field_name="refills"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeRatioField(df_table, table_name,field_name,"",big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  field_name="quantity"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeRatioField(df_table, table_name,field_name,"",big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  field_name="days_supply"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeRatioField(df_table, table_name,field_name,"",big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  # not sure how to handle the sig field
  #field_name<-"sig"
  #df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  #fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  #message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  #fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  #missing_percent<-extract_numeric_value(message)
  #logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));

  field_name<-"lot_number"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  

  field_name<-"frequency"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);


  field_name<-"route_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  describeNominalField_basic(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  df_table<-retrieve_dataframe_top_5(con, g_config, table_name, field_name)
  fileContent<-c(fileContent,paste("The most frequent values for",field_name,"are:"))
  for(row_count in 1:5)
  {
    fileContent<-c(fileContent,paste(df_table[row_count,1],"(count=",df_table[row_count,2],")"))
  }

  field_name="route_concept_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  
  ######## DQA Checkpoint ####################
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "route_concept_id.txt")) 
  df_route_concept_id <-generate_df_concepts(con, table_name,"route_concept_id.txt")
  
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  
  ###########DQA CHECKPOINT -- missing information##############
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT##############
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  #fileContent<-c(fileContent,unexpected_message)
  df_table_route_enhanced<-EnhanceFieldValues(df_table,field_name,df_route_concept_id);
  describeNominalField_basic(df_table_route_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

   null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650 ,big_data_flag)
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "route_source_value"),con
  )) 
  

  #print (fileContent)

  field_name<-"dose_unit_source_value" # 3 minutes
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  describeNominalField_basic(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  df_table<-retrieve_dataframe_top_5(con, g_config, table_name, field_name)
  fileContent<-c(fileContent,paste("The most frequent values for",field_name,"are:"))
  for(row_count in 1:5)
  {
    fileContent<-c(fileContent,paste(df_table[row_count,1],"(count=",df_table[row_count,2],")"))
  }


  field_name<-"dose_unit_concept_id" #
  ########## DQA checkpoint ################## 
  
  field_name="dose_unit_concept_id"
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "dose_unit_concept_id.txt")) 
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  
  df_dose_unit_concept_id <-generate_df_concepts(con, table_name,"dose_unit_concept_id.txt")

  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,unexpected_message)
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  df_table_unit_enhanced<-EnhanceFieldValues(df_table,field_name,df_dose_unit_concept_id);
  describeNominalField_basic(df_table_unit_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  df_table_top_5<-retrieve_dataframe_top_5(con, g_config, table_name, field_name)
  fileContent<-c(fileContent,paste("The most frequent values for",field_name,"are:"))
  for(row_count in 1:5)
  {
    fileContent<-c(fileContent,paste(df_table_top_5[row_count,1],"(count=",df_table_top_5[row_count,2],")"))
  }


   null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650 ,big_data_flag)
   #print(null_message)
   #print(extract_ni_missing_percent( null_message))
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
   logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "dose_unit_source_value"),con
   )) 
   

  fileContent<-c(fileContent,"\nDQA NOTE: There should be a one-to-one correspondence between dose_unit source value and concept id fields; compare the top 5 values")
  fileContent<-c(fileContent,"\nDQA NOTE: Look for cases where dose_unit_concept_id =0, this would happen when dose_unit_source_value is either NULL or cannot be mapped")


  field_name<-"effective_drug_dose" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);

  fileContent<-c(fileContent,"\nDQA NOTE: effective_drug_dose cannot be generated if dose_unit_source_value is NULL")

  field_name<-"eff_drug_dose_source_value" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);

  field_name<-"provider_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

  # flog.info(fileContent)

  field_name<-"visit_occurrence_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
 
    df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
    message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,message)
    ###########DQA CHECKPOINT -- missing information##############
    missing_percent<-extract_numeric_value(message)
    logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
    message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);
  

  # flog.info(fileContent)

   flog.info(Sys.time())

  #print (fileContent)

  #print (fileContent)

  field_name="lot_number"
  fileContent <-c(fileContent,paste("## Description for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  # not implementing a check here as its site preference

  
  # this is a nominal field - work on it
  field_name<-"dispense_as_written_concept_id" #
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  order_bins <-c("4188539","4188540","44814653", "44814649","44814650","0",NA)
  label_bins<-c("Yes (4188539)","No (4188540)","0 (No matching)","Unknown (44814653)","Other (44814649)","No Information (44814650 )","NULL")
  color_bins <-c("4188539"="lightcoral","4188540"="steelblue1","0"="red","44814653"="grey64","44814649"="grey64","44814650 "="grey64")
  
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "dispense_as_written_concept_id.csv")) 
  
  describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  
  
  # other fields not plotted : operator concept id, value as concept id, value source value, measurement source concept id
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
