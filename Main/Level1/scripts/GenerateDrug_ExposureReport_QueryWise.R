flog.info(Sys.time())

generateDrugExposureReport <- function() {
  
  table_name<-"drug_exposure"
  data_tbl <- cdm_tbl(req_env$db_src, table_name)
  concept_tbl <- vocab_tbl(req_env$db_src, "concept")

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),
                       "./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0),
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))

  #PRIMARY FIELD
  field_name<-"drug_exposure_id"
  df_total_measurement_count<-retrieve_dataframe_count(data_tbl,field_name)
  current_total_count<-as.numeric(df_total_measurement_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:",
                                   formatC(current_total_count, format="d", big.mark=','),"\n"))
  
 ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 
  
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count) 
  
  df_total_patient_count<-retrieve_dataframe_count(data_tbl,"person_id", distinction = T)
  fileContent<-c(fileContent,paste("The drug exposure to patient ratio is ",round(df_total_measurement_count[1][1]/df_total_patient_count[1][1],2),"\n"))
  df_total_visit_count<-retrieve_dataframe_count(data_tbl,"visit_occurrence_id", distinction = T)
  fileContent<-c(fileContent,paste("The drug exposure to visit ratio is ",round(df_total_measurement_count[1][1]/df_total_visit_count[1][1],2),"\n"))

  # visit concept id
  df_visit <-retrieve_dataframe_clause(concept_tbl,c("concept_id" ,"concept_name")
 ,"vocabulary_id =='Visit' | (vocabulary_id == 'PCORNet' & concept_class_id == 'Encounter Type') | 
 (concept_class_id == 'Encounter Type' & domain_id == 'Visit') |
 (vocabulary_id == 'PCORNet' & concept_class_id == 'Undefined')")
  
  #condition / person id by visit types
  fileContent <-c(fileContent,paste("## Barplot for Drug:Patient ratio by visit type\n"))

  df_drug_patient_ratio <- retrieve_dataframe_ratio_group_join(data_tbl,
                                                                    cdm_tbl(req_env$db_src,"visit_occurrence"),
                                                                    num ="drug_exposure_id", den = "person_id",
                                                                    "visit_concept_id", "visit_occurrence_id")

  for(i in 1:nrow(df_drug_patient_ratio))
  {
    label<-df_visit[df_visit$concept_id==df_drug_patient_ratio[i,1],2]
    df_drug_patient_ratio[i,1]<-paste(df_drug_patient_ratio[i,1],"(",label,")",sep="")
  }
  describeOrdinalField(df_drug_patient_ratio,table_name,"drug_exposure_id_person_id_ratio", 
                       group_ret = 1);
  fileContent<-c(fileContent,paste_image_name(table_name,"drug_exposure_id_person_id_ratio"));

  #NOMINAL Fields

  field_name<-"person_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                 paste_image_name_sorted(table_name,field_name),message);

  field_name<-"drug_source_value"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeOrdinalField(df_table, table_name,field_name,ggplotting = F)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  
  field_name<-"drug_source_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)
  
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name), data_tbl)) 
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 

  if(nrow(df_table)>1)
  {
    fileContent<-c(fileContent,paste("\n The source vocabulary is",get_vocabulary_name(concept_tbl, df_table[2,1]),"\n"))
  }
  if(nrow(df_table)==1){
      if(is.na(df_table[1,1]))
      {
          fileContent<-c(fileContent,paste("\n The source vocabulary is NA \n"))
      }
      else
      {
          fileContent<-c(fileContent,paste("\n The source vocabulary is",get_vocabulary_name(concept_tbl, df_table[1,1]),"\n"))
      }
  }
  message<-describeOrdinalField(df_table, table_name,field_name, ggplotting = F)
  new_message<-""
  if(length(message)>0)
  {
    # create meaningful message
    new_message<-create_meaningful_message_concept_id(concept_tbl, message,field_name)
  }
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));

  flog.info(Sys.time())
  
  field_name<-"drug_concept_id" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))

  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<- custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  
  ### DQA CHECKPOINT ##########
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidVocab(), c(table_name),c(field_name), 
                                                   c('Drug','RxNorm', 'RxNorm Extension'), 
                                                   concept_tbl, data_tbl)) 

  message<-describeOrdinalField(df_table, table_name,field_name,ggplotting = F)
  # create meaningful message
  new_message<-create_meaningful_message_concept_id(concept_tbl, message,field_name)
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));
  null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650)
  
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),
                                                   c(field_name, "drug_source_value"), data_tbl)) 

  fileContent<-c(fileContent,"###Distribution of drug concept class:")
  df_concept_class<-retrieve_dataframe_join_clause_group(data_tbl, concept_tbl, "drug_concept_id",
                                                         "concept_class_id","drug_concept_id!=0")
  fileContent<-c(fileContent,print_2d_dataframe(df_concept_class))

  ### ingredient-level normalization
  # also draw distribution of drug concept id vs person_id
  field_name<-"drug_concept_id" #
  df_table_new<-retrieve_dataframe_count_group(data_tbl,"person_id", field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"by person_id","\n"))

  message<-describeOrdinalField(df_table_new, "person_id",field_name, ggplotting = F)
  # create meaningful message
  new_message<-create_meaningful_message_concept_id(concept_tbl, message,field_name)
  fileContent<-c(fileContent,new_message,paste_image_name("person_id",field_name));
  field_name<-"drug_exposure_start_datetime"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)

  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-describeDateField(df_table, table_name,field_name, datetime = 1)

  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),data_tbl)) 
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  message<-describeTimeField(df_table, table_name,field_name)
  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_datetime",sep="")));

  field_name<-"drug_exposure_end_datetime"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 

  if(missing_percent<100){
    message<-describeDateField(df_table, table_name,field_name, datetime = 1)
    ### DQA checkpoint - future date
    logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),data_tbl)) 
    fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
    message<-describeTimeField(df_table, table_name,field_name)
    fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_datetime",sep="")));
  }

  #implausible event clause
  logFileData<-custom_rbind(logFileData,applyCheck(ImplEvent(), c(table_name), 
                                                   c('drug_exposure_start_date','drug_exposure_end_date'),
                                                   data_tbl)) 

  # drug exposure end date
  field_name<-"drug_exposure_end_date"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeDateField(df_table, table_name,field_name)

  field_name<-"drug_exposure_order_datetime"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeDateField(df_table, table_name,field_name)

  ###########DQA CHECKPOINT -- future dates ##############
  if(missing_percent!=100){
    ### DQA checkpoint - future date
    logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name),
                                                     c(field_name),data_tbl))
  }
  
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  message<-describeTimeField(df_table, table_name,field_name)
  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_datetime",sep="")));

  # drug exposure order date
  field_name<-"drug_exposure_order_date"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeDateField(df_table, table_name,field_name)
  #drug type concept id
  field_name="drug_type_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)

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

  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))

  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name), data_tbl)) 
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"drug_type_concept_id.csv", concept_tbl, data_tbl)) 

  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  ## DQA check for recall of various drug types
  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name), 
                                                   list(
                                                     list(38000175,  "Dispensing"),
                                                   list(38000180,  "Inpatient Administration"),
                                                   list(38000177,  "prescription")), data_tbl))
  

  field_name<-"stop_reason"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeOrdinalField(df_table, table_name,field_name, ggplotting = F)
  fileContent<-c(fileContent, paste_image_name(table_name,field_name),message);

  field_name="refills"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeRatioField(df_table, table_name,field_name,"")
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  field_name="quantity"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeRatioField(df_table, table_name,field_name,"")
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  field_name="days_supply"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeRatioField(df_table, table_name,field_name,"")
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  field_name<-"lot_number"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name), data_tbl)) 

  field_name<-"frequency"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message)

  field_name<-"route_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  describeNominalField(df_table, table_name, field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  df_table<-retrieve_dataframe_top_5(data_tbl, field_name)
  fileContent<-c(fileContent,paste("The most frequent values for",field_name,"are:"))

  for(row_count in 1:5)
  {
    fileContent<-c(fileContent,paste(df_table[row_count,1],"(count=",df_table[row_count,2],")"))
  }

  field_name="route_concept_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  
  ######## DQA Checkpoint ####################
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"route_concept_id_dplyr.txt", concept_tbl, data_tbl)) 
  df_route_concept_id <-generate_df_concepts(table_name,"route_concept_id_dplyr.txt", concept_tbl)
  df_table<-retrieve_dataframe_group(data_tbl,field_name)

  ###########DQA CHECKPOINT -- missing information##############
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 

  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name), data_tbl)) 
  df_table_route_enhanced<-EnhanceFieldValues(df_table,field_name,df_route_concept_id);
  describeNominalField(df_table_route_enhanced,table_name,field_name);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650)

  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),
                                                   c(field_name, "route_source_value"), data_tbl)) 
  field_name<-"dose_unit_source_value" # 3 minutes
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  describeNominalField(df_table, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  df_table<-retrieve_dataframe_top_5(data_tbl, field_name)

  fileContent<-c(fileContent,paste("The most frequent values for",field_name,"are:"))
  for(row_count in 1:5)
  {
    fileContent<-c(fileContent,paste(df_table[row_count,1],"(count=",df_table[row_count,2],")"))
  }
  field_name<-"dose_unit_concept_id" #
  
  ########## DQA checkpoint ################## 
  field_name="dose_unit_concept_id"
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"dose_unit_concept_id_dplyr.txt", concept_tbl, data_tbl)) 
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  df_dose_unit_concept_id <-generate_df_concepts(table_name,"dose_unit_concept_id_dplyr.txt", concept_tbl)

  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 

  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))

  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  df_table_unit_enhanced<-EnhanceFieldValues(df_table,field_name,df_dose_unit_concept_id);
  describeNominalField(df_table_unit_enhanced,table_name,field_name);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  df_table_top_5<-retrieve_dataframe_top_5(data_tbl, field_name)

  fileContent<-c(fileContent,paste("The most frequent values for",field_name,"are:"))
  for(row_count in 1:5)
  {
    fileContent<-c(fileContent,paste(df_table_top_5[row_count,1],"(count=",df_table_top_5[row_count,2],")"))
  }

  null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650)

  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
   logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), 
                                                    c(table_name),c(field_name, "dose_unit_source_value"),
                                                    data_tbl)) 

  fileContent<-c(fileContent,"\nDQA NOTE: There should be a one-to-one correspondence between dose_unit source value and concept id fields; compare the top 5 values")
  fileContent<-c(fileContent,"\nDQA NOTE: Look for cases where dose_unit_concept_id =0, this would happen when dose_unit_source_value is either NULL or cannot be mapped")

  field_name<-"effective_drug_dose" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);
  fileContent<-c(fileContent,"\nDQA NOTE: effective_drug_dose cannot be generated if dose_unit_source_value is NULL")

  field_name<-"eff_drug_dose_source_value" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name), data_tbl)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);
  
  field_name<-"provider_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                 paste_image_name_sorted(table_name,field_name),message);

  field_name<-"visit_occurrence_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
 
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name), data_tbl)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                   paste_image_name_sorted(table_name,field_name),message);

  flog.info(Sys.time())

  field_name="lot_number"
  fileContent <-c(fileContent,paste("## Description for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name, group_ret = 1))

  # this is a nominal field - work on it
  field_name<-"dispense_as_written_concept_id" #
  if(field_name %in% colnames(data_tbl)){ ##field currently is not in Oracle test db
  df_table<-retrieve_dataframe_group(data_tbl,field_name)

  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)

  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"dispense_as_written_concept_id.csv",
                                                   concept_tbl, data_tbl)) 
  
  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  }

  # other fields not plotted : operator concept id, value as concept id, value source value, measurement source concept id
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
}
