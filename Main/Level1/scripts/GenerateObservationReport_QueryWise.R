generateObservationReport <- function() {
  flog.info(Sys.time())
  #Read table
  table_name<-"observation"
  data_tbl <- cdm_tbl(req_env$db_src, table_name)
  concept_tbl <- vocab_tbl(req_env$db_src, "concept")
  
  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0),
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))

  #PRIMARY FIELD and related measures
  field_name<-"observation_id"
  df_total_observation_count<-retrieve_dataframe_count(data_tbl,field_name)
  current_total_count<-as.numeric(df_total_observation_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", 
                                   formatC(current_total_count, format="d", big.mark=','),"\n"))

  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)

  df_total_patient_count<-retrieve_dataframe_count(data_tbl,"person_id", distinction = T)
  fileContent<-c(fileContent,paste("The observation to patient ratio is ",
                                   round(df_total_observation_count[1][1]/df_total_patient_count[1][1],2),"\n"))
  df_total_visit_count<-retrieve_dataframe_count(data_tbl,"visit_occurrence_id", distinction = T)
  fileContent<-c(fileContent,paste("The observation to visit ratio is ",
                                   round(df_total_observation_count[1][1]/df_total_visit_count[1][1],2),"\n"))

  # visit concept id
  df_visit <-retrieve_dataframe_clause(concept_tbl, c("concept_id", "concept_name"),
    "vocabulary_id =='Visit' | (vocabulary_id=='PCORNet' & concept_class_id=='Encounter Type') |
    (vocabulary_id == 'PCORNet' & concept_class_id=='Undefined')")

  #observation / person id by visit types
  fileContent <-c(fileContent,paste("## Barplot for Observation:Patient ratio by visit type\n"))
  df_observation_patient_ratio<-retrieve_dataframe_ratio_group_join(data_tbl,
                                                                  cdm_tbl(req_env$db_src,"visit_occurrence"),
                                                                  "observation_id", "person_id",
                                                                  "visit_concept_id", "visit_occurrence_id")

  for(i in 1:nrow(df_observation_patient_ratio)){
         label<-df_visit[df_visit$concept_id==df_observation_patient_ratio[i,1],2]
         df_observation_patient_ratio[i,1]<-paste(df_observation_patient_ratio[i,1],"(",label,")",sep="")
  }
  describeOrdinalField(df_observation_patient_ratio,table_name,"observation_id_person_id_ratio");
  fileContent<-c(fileContent,paste_image_name(table_name,"observation_id_person_id_ratio"));

  field_name="observation_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650)

  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)
  missing_percent_source_value<-extract_numeric_value(message)

  field_name="observation_source_concept_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(data_tbl,field_name)

  ###########DQA CHECKPOINT -- missing information##############
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
 
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  fileContent<-c(fileContent,message)
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  describeNominalField(df_table, table_name, field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  field_name<-"observation_concept_id" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"observation_concept_id.csv", concept_tbl, df_table)) 
  #fileContent<-c(fileContent,unexpected_message)
  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  ###########DQA CHECKPOINT############## missing expected concepts 
  
  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name),
                                                   list(
                                                     list(44813951,  " discharge status ") ,
                                                     list(3040464,  "DRG") ,
                                                    list(40760190,  "Delivery Mode") ,
                                                    list(4005823, 4219336, 4275495,  "Tobacco")
                                                    ), df_table)
)
  
  #NOMINAL Fields
  field_name<-"person_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                 paste_image_name_sorted(table_name,field_name),message);

  field_name<-"provider_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                 paste_image_name_sorted(table_name,field_name),message);

  flog.info(Sys.time())
  
  field_name<-"visit_occurrence_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  count_novisit<-retrieve_dataframe_clause(data_tbl,"count(*)"
                                           ,"is.na(visit_occurrence_id)")
  missing_visit_percent<-round(count_novisit*100/df_total_observation_count,2)
  
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name,
                                                           missing_visit_percent, table_name, g_data_version));

  # ORDINAL Fields
  field_name<-"observation_date" 
  df_table<-retrieve_dataframe_group(table_df = data_tbl,field_name = field_name)
  try(fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n")))
  try(message<-describeDateField(df_table, table_name,field_name))
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name))

  ###########DQA CHECKPOINT##############
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),data_tbl))
  field_name<-"observation_datetime" 
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))

  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeTimeField(df_table, table_name,field_name)
  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_datetime",sep="")));
  # not plotting the value_as_string column as it's a free text field
  field_name<-"value_as_string"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)

  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  # not plotting the value_as_string column as it's a free text field
  field_name<-"value_as_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))

  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 

  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 

  ###########DQA CHECKPOINT##############
  ##Can't work with date condition on db
  condition_2 <- concept_tbl %>% filter(concept_class_id=='DRG' & invalid_reason == 'D') %>% as.data.frame()
  condition_2 <- condition_2 %>% filter(valid_end_date == "2007-09-30") %>% select(concept_id, concept_name)
  df_concepts <- generate_df_concepts(table_name,"value_as_concept_id_dplyr.txt", concept_tbl)
  df_concepts <- rbind(df_concepts, condition_2)
  remove(condition_2)

  unexpected_message<- reportUnexpected(df_table, field_name, df_concepts$concept_id)
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,df_concepts, concept_tbl, data_tbl)) 
  field_name<-"unit_source_value" # 3 minutes
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))

  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  missing_message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  test = df_table %>% as.data.frame()
  fileContent<-c(fileContent,missing_message)
  missing_percent_source_value<-extract_numeric_value(missing_message)
  if(grepl("100",missing_message)==FALSE) # if 100% missing
  {
    describeNominalField(df_table, table_name,field_name)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  }

  df_unit <-generate_df_concepts(table_name,"unit_concept_id_dplyr.txt", concept_tbl)
  order_bins <-c(df_unit$concept_id,0,44814650,NA)
  field_name="unit_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650)

  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),
                                                 c(field_name, "unit_source_value"),data_tbl)) 

  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"unit_concept_id_dplyr.txt", concept_tbl, data_tbl)) 
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))

  ###########DQA CHECKPOINT -- no matching concept ##############
  try(logFileData<- custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl))) 

  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 

  df_table_unit_enhanced<-EnhanceFieldValues(df_table,field_name,df_unit);

  if( is.na(df_table_unit_enhanced[1,1]) && nrow(df_table_unit_enhanced) == 1)
  {
     flog.info("unit concept id data not available")
  } else
  {
    describeNominalField(df_table_unit_enhanced,table_name,field_name);
    fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  }

  field_name = "observation_type_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"observation_type_concept_id.csv", concept_tbl, data_tbl)) 
  
  
  fact_type_count<-df_table[df_table$observation_type_concept_id==38000280,2]
  write_total_fact_type_counts(table_name,"EHR" , fact_type_count)
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiffFactType(), c(table_name), c(field_name)
                                                   ,c("EHR",fact_type_count))) 
  
  fact_type_count<-df_table[df_table$observation_type_concept_id==44814721,2]
  write_total_fact_type_counts(table_name,"Patient_Reported" , fact_type_count)
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiffFactType(), c(table_name), c(field_name)
                                                   ,c("Patient_Reported",fact_type_count))) 
  
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  field_name<-"qualifier_source_value" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeOrdinalField(df_table, table_name,field_name, ggplotting = F)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  field_name<-"qualifier_concept_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 

  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650)
  
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),
                                                   c(field_name, "qualifier_source_value"),data_tbl)) 
  message<-describeOrdinalField(df_table, table_name,field_name, ggplotting = F)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  ##########DQA CHECKPOINT################
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"qualifier_concept_id_dplyr.txt", concept_tbl, data_tbl)) 

  
  ###TESTING FOR COMPLETELY MISSING FIELD###
  field_name<-"fake_date" 
  df_table<-retrieve_dataframe_group(table_df = data_tbl,field_name = field_name)
  try(fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n")))
  try(message<-describeDateField(df_table, table_name,field_name))
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name))
  
  ###########DQA CHECKPOINT##############
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),data_tbl))
  
  #ordinal field
  field_name<-"value_as_number"
    df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeRatioField(df_table, table_name,field_name,"")
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  flog.info(Sys.time())

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)
}
