generateMeasurementReport <- function() {
  flog.info(Sys.time())

  
  big_data_flag<-TRUE
  
  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)
  
  table_name<-"measurement"
  
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)
  
  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), 
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))
  #PRIMARY FIELD
  field_name<-"measurement_id"
  df_total_measurement_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  current_total_count<-as.numeric(df_total_measurement_count[1][1])
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name),NULL, current_total_count)) 
  
  print(field_name)
  
  
  #NOMINAL Fields
  
  field_name<-"provider_id" #
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  field_name<-"visit_occurrence_id"
  
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  print(field_name)
  
  
  field_name<-"measurement_date" #
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
  
  
  print(field_name)
  
  
  field_name<-"measurement_result_date" #
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
  
  print(field_name)
  
  field_name<-"measurement_order_date" #
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
  
  print(field_name)
  
  field_name<-"value_as_concept_id" #
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "value_as_concept_id.txt")) 
  
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  print(field_name)
  
  #print (fileContent)
  field_name<-"unit_source_value" # 3 minutes
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  print(field_name)
  
  field_name="unit_concept_id"
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "unit_concept_id.txt")) 
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "unit_source_value"),con
  )) 
  
  print(field_name)
  
  #Operator Concept Id
  field_name = "operator_concept_id"
  
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  #unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "operator_concept_id.csv")) 
  
  field_name<-"priority_source_value" # 3 minutes
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  print(field_name)
  
  #priority concept id
  field_name="priority_concept_id"
  #unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "priority_concept_id.txt")) 
  
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  
  print(field_name)
  
  field_name = "measurement_type_concept_id"
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "measurement_type_concept_id.csv")) 
  
  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name),con, 
                                                   list(
                                                     list(2000000033,  2000000032, "vital signs"),
                                                     list(44818702, "lab records"))) )
  
  
  
  #print (fileContent)
  print(field_name)
  
  field_name="measurement_source_value"
  
  field_name<-"measurement_source_concept_id"
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  # this is a nominal field - work on it
  
  print(field_name)
  
  
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
  
  
  
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name, big_data_flag))
  # add % of no matching concept (concept id = 0). for the completeness report
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  
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
  
  #concept_id_list <- unique(df_table[,1])
  
  
  print(field_name)
  
  ## compute missing % for labs only 
  field_name<-"specimen_source_value" #
  
  
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
  print(field_name)
  
  # specimen concept id
  field_name<-"specimen_concept_id" #
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "specimen_concept_id.txt")) 
  
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  #generating concept wise graphs for numerical readings
  
  print(field_name)
  
  field_name<-"range_high" #
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  
  field_name<-"range_high_source_value" #
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  field_name<-"range_high_operator_concept_id" #
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT######################
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "operator_concept_id.csv")) 
  
  #print (fileContent)
  
  field_name<-"range_low" #
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  field_name<-"range_low_source_value" #
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  field_name<-"range_low_operator_concept_id" #
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT################
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "operator_concept_id.csv")) 
  
  
  # write to log file
  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
  #close the connection
  close_database_connection_OHDSI(con, g_config)
  
  
}
