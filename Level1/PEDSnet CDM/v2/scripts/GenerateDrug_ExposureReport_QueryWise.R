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

  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), issue_description=character(0)
                          , finding=character(0), prevalence=character(0))


  test <-1

  #PRIMARY FIELD
  field_name<-"drug_exposure_id"
  df_total_measurement_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  current_total_count<-as.numeric(df_total_measurement_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
  prev_total_count<-get_previous_cycle_total_count( g_config$reporting$site, table_name)
  percentage_diff<-get_percentage_diff(prev_total_count, current_total_count)
  fileContent<-c(fileContent, get_percentage_diff_message(percentage_diff))
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,apply_check_type_0("CA-005", percentage_diff, table_name, g_data_version));


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
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

  field_name<-"drug_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent_source_value, table_name, g_data_version));
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  field_name<-"drug_source_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message), table_name, g_data_version));
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
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

   flog.info(Sys.time())
  field_name<-"drug_concept_id" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  #message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  #fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  # add % of no matching concept (concept id = 0). for the completeness report
  no_matching_concept_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_concept_message)
  no_matching_concept_number<-extract_numeric_value(no_matching_concept_message)
  # if more than 10% concepts are not being mapped to RxNorm
  ###########DQA CHECKPOINT -- no matching concept percentage ##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_concept_message), table_name, g_data_version)); # custom threshold

  fileContent<-c(fileContent,"\n The standard vocabulary is RxNorm \n")
  used_vocabulary<-get_vocabulary_name_by_concept_ids(con, g_config, table_name, field_name, "DRUG")
  fileContent<-c(fileContent,paste("\n The vocabulary used by the site is",used_vocabulary,"\n"))
  if(!grepl('RxNorm',used_vocabulary))
  {
    ###########DQA CHECKPOINT -- vocabulary incorrect ##############
    logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-005", field_name, "invalid vocabulary used, please use RxNorm", table_name, g_data_version));
  }
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  # create meaningful message
  new_message<-create_meaningful_message_concept_id(message,field_name,con, g_config)
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));


   null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650 ,big_data_flag)
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,apply_check_type_2("CA-014", field_name,"drug_source_value",
                                                           (missing_percent_source_value-
                                                              extract_ni_missing_percent( null_message)), table_name, g_data_version))

  if(no_matching_concept_number>10)
  {
    fileContent<-c(fileContent,"###Frequent source values for non-matching concepts:")
    df_no_match_result<-retrieve_dataframe_top_20_clause(con,g_config,
                                                         table_name,"drug_source_value","drug_concept_id=0")
    fileContent<-c(fileContent,print_2d_dataframe(df_no_match_result))
  }

  fileContent<-c(fileContent,"###Distribution of drug concept class:")
  df_concept_class<-retrieve_dataframe_join_clause_group(con, g_config,g_config$db$schema,table_name, g_config$db$vocab_schema,"concept","concept_class_id",
                                                         "drug_concept_id = concept_id and drug_concept_id<>0")
  fileContent<-c(fileContent,print_2d_dataframe(df_concept_class))

  ### ingredient-level normalization
  fileContent<-c(fileContent,"### ingredient-level normalization")

  in_set_1<-retrieve_dataframe_join_clause_group(con, g_config, g_config$db$schema,table_name,  g_config$db$vocab_schema,"concept","concept_id , concept_name"
                                                 ,"DRUG_CONCEPT_ID = CONCEPT_ID and CONCEPT_CLASS_ID='Ingredient'")
  colnames(in_set_1)[1]<-"in_concept_id"
  colnames(in_set_1)[2]<-"in_concept_name"

  in_set_2<-retrieve_dataframe_join_clause_group(con, g_config, g_config$db$schema,table_name,  g_config$db$dqa_schema,"rxnorm_in_scd"
                                                 ,"in_concept_id, in_concept_name"
                                                 ,"drug_concept_id = SCD_concept_id")

  in_set_3<-retrieve_dataframe_join_clause_group(con, g_config, g_config$db$schema,table_name,  g_config$db$dqa_schema,"rxnorm_in_bcd"
                                                 ,"in_concept_id, in_concept_name"
                                                 ,"drug_concept_id = BCD_concept_id")

  in_set_4<-retrieve_dataframe_join_clause_group(con, g_config, g_config$db$schema,table_name,  g_config$db$dqa_schema,"rxnorm_in_bpck"
                                                 ,"in_concept_id, in_concept_name"
                                                 ,"drug_concept_id = BPCK_concept_id")

  total<-rbind(in_set_1,in_set_2,in_set_3,in_set_4)
  # aggregate 4 datasets by counts
  new_total<-aggregate(count~in_concept_id+in_concept_name, sum, data=total)
  colnames(new_total)[3]<-"frequency"
  # order by sum
  ordered_total <- new_total[with(new_total, order(-frequency)), ]
  ordered_total$concept <- paste(ordered_total$in_concept_id,'-',ordered_total$in_concept_name)
  enhanced_total<-subset(ordered_total[1:20,],select=c(concept,frequency))
  fileContent<-c(fileContent,print_2d_dataframe(enhanced_total))


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


  field_name<-"drug_exposure_start_time"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)
  ###########DQA CHECKPOINT##############
  if(grepl("future",message[3]))
  {
    logFileData<-custom_rbind(logFileData,apply_check_type_1("CA-001", field_name, "drug exposures cannot start in the future", table_name, g_data_version));
  }
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  message<-describeTimeField(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_time",sep="")));


  field_name<-"drug_exposure_end_time"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  if(missing_percent<100)
  {
    message<-describeDateField(df_table, table_name,field_name,big_data_flag)
    ###########DQA CHECKPOINT##############
    if(grepl("future",message[3]))
    {
      logFileData<-custom_rbind(logFileData,apply_check_type_1("CA-001", field_name, "drug exposures cannot end in the future", table_name, g_data_version));
    }
    fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
    message<-describeTimeField(df_table, table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_time",sep="")));
  }
  #implausible event clause
  df_implausible_date_count<-retrieve_dataframe_clause(con, g_config, g_config$db$schema,table_name,"count(*)","drug_exposure_start_date>drug_exposure_end_date")
  if(df_implausible_date_count[1][1]>0)
  {
    ###########DQA CHECKPOINT##############
    implausible_message<-paste("There are ",df_implausible_date_count[1][1]," records with drug_exposure_start_date>drug_exposure_end_date")
    fileContent<-c(fileContent,implausible_message);
    #logFileData<-custom_rbind(logFileData,apply_check_type_1("CA-016", table_name, "drug_exposure_start_date",field_name, implausible_message, table_name, g_data_version));
  }

  # drug exposure end date
  field_name<-"drug_exposure_end_date"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)

  field_name<-"drug_exposure_order_time"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)
  ###########DQA CHECKPOINT -- future dates ##############
  if(missing_percent!=100)
  {
    if(grepl("future",message[3]))
    {
      logFileData<-custom_rbind(logFileData,apply_check_type_1("CA-001", field_name, "drug orders cannot occur in the future", table_name, g_data_version));
    }
  }
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  message<-describeTimeField(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_time",sep="")));

  # drug exposure order date
  field_name<-"drug_exposure_order_date"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)


  #drug type concept id
  field_name="drug_type_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  order_bins <-c("38000175","38000180","38000177","0",NA)
  label_bins<-c("Prescription dispensed in pharmacy (dispensed meds pharma information) (38000175)"
                ,"Inpatient administration (MAR entries) (38000180)"
                ,"Prescription written (38000177)","Other (0)","NULL")
  color_bins <-c("38000175"="lightcoral","38000180"="steelblue1","38000177"="red","0"="grey64")
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message), table_name, g_data_version));
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version));
  fileContent<-c(fileContent,unexpected_message)
  describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  ## DQA check for recall of various drug types
  if(nrow(subset(df_table,df_table$drug_type_concept_id==38000175))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No Dispensing Records");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No dispensing records found", table_name, g_data_version));

  }
  if(nrow(subset(df_table,df_table$drug_type_concept_id==38000180))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No Inpatient Administration Records");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No inpatient administration records found", table_name, g_data_version));

  }
  if(nrow(subset(df_table,df_table$drug_type_concept_id==38000177))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No Prescription Records");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No prescription records found", table_name, g_data_version));

  }

  field_name<-"stop_reason"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
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
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeRatioField(df_table, table_name,field_name,"",big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  field_name="quantity"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeRatioField(df_table, table_name,field_name,"",big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  field_name="days_supply"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
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
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));


  field_name<-"frequency"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);


  field_name<-"route_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent_source_value, table_name, g_data_version));
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
  df_route <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name"
                                           ,"domain_id ='Route'")
  order_bins <-c(df_route$concept_id,0,44814650,NA)
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version));
  fileContent<-c(fileContent,unexpected_message)
  df_table_route_enhanced<-EnhanceFieldValues(df_table,field_name,df_route);
  describeNominalField_basic(df_table_route_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

   null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650 ,big_data_flag)
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,apply_check_type_2("CA-014", field_name,"route_source_value",
                                                           (missing_percent_source_value-
                                                              extract_ni_missing_percent( null_message)), table_name, g_data_version))


  #print (fileContent)

  field_name<-"dose_unit_source_value" # 3 minutes
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent_source_value, table_name, g_data_version));
  describeNominalField_basic(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  df_table<-retrieve_dataframe_top_5(con, g_config, table_name, field_name)
  fileContent<-c(fileContent,paste("The most frequent values for",field_name,"are:"))
  for(row_count in 1:5)
  {
    fileContent<-c(fileContent,paste(df_table[row_count,1],"(count=",df_table[row_count,2],")"))
  }


  field_name<-"dose_unit_concept_id" #
  df_unit <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name"
                                      ,"domain_id='Unit' and vocabulary_id ='UCUM'")
  order_bins <-c(df_unit$concept_id,0,44814650,NA)
  field_name="dose_unit_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version));
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  fileContent<-c(fileContent,unexpected_message)
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));
  df_table_unit_enhanced<-EnhanceFieldValues(df_table,field_name,df_unit);
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
  logFileData<-custom_rbind(logFileData,apply_check_type_2("CA-014", field_name,"dose_unit_source_value",
                                                           (missing_percent_source_value-
                                                              extract_ni_missing_percent( null_message)), table_name, g_data_version))


  fileContent<-c(fileContent,"\nDQA NOTE: There should be a one-to-one correspondence between dose_unit source value and concept id fields; compare the top 5 values")
  fileContent<-c(fileContent,"\nDQA NOTE: Look for cases where dose_unit_concept_id =0, this would happen when dose_unit_source_value is either NULL or cannot be mapped")


  field_name<-"effective_drug_dose" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
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
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);

  field_name<-"provider_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
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
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
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

  # other fields not plotted : operator concept id, value as concept id, value source value, measurement source concept id
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
