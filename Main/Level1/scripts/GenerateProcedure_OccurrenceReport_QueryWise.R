generateProcedureOccurrenceReport <- function() {
  flog.info(Sys.time())

  # read a table
  table_name<-"procedure_occurrence"
  data_tbl <- cdm_tbl(req_env$db_src, table_name)
  concept_tbl <- vocab_tbl(req_env$db_src, "concept")

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0),
                          issue_description=character(0), alias=character()
                          , finding=character(0), prevalence=character(0))

  #PRIMARY FIELD
  field_name<-"procedure_occurrence_id"
  df_total_procedure_count<-retrieve_dataframe_count(data_tbl,field_name)
  current_total_count<-as.numeric(df_total_procedure_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", 
                                   formatC(current_total_count, format="d", big.mark=','),"\n"))
  
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 

  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
    
  df_total_patient_count<-retrieve_dataframe_count(data_tbl,"person_id", distinction = T)
  fileContent<-c(fileContent,paste("The procedure to patient ratio is ",round(df_total_procedure_count[1][1]/df_total_patient_count[1][1],2),"\n"))
  df_total_visit_count<-retrieve_dataframe_count(data_tbl,"visit_occurrence_id", distinction = T)
  fileContent<-c(fileContent,paste("The procedure to visit ratio is ",
                                   round(df_total_procedure_count[1][1]/df_total_visit_count[1][1],2),"\n"))
  # visit concept id
  df_visit <-retrieve_dataframe_clause(concept_tbl, c("concept_id","concept_name"),
      "vocabulary_id =='Visit' | (vocabulary_id=='PCORNet' & concept_class_id=='Encounter Type') |
                                       (vocabulary_id == 'PCORNet' & concept_class_id=='Undefined')")

  #procedure / person id by visit types
  fileContent <-c(fileContent,paste("## Barplot for Procedure:Patient ratio by visit type\n"))
  df_procedure_patient_ratio<-retrieve_dataframe_ratio_group_join(data_tbl, 
                                                                  cdm_tbl(req_env$db_src, "visit_occurrence"),
                                                                  "procedure_occurrence_id", "person_id",
                                                               "visit_concept_id","visit_occurrence_id")

  for(i in 1:nrow(df_procedure_patient_ratio)){
        label<-df_visit[df_visit$concept_id==df_procedure_patient_ratio[i,1],2]
        df_procedure_patient_ratio[i,1]<-paste(df_procedure_patient_ratio[i,1],"(",label,")",sep="")
      }
  describeOrdinalField(df_procedure_patient_ratio,table_name,"ratio");
  fileContent<-c(fileContent,paste_image_name(table_name,"Procedure:Patient ratio by visit type"));

  #procedure type concept id
  field_name="procedure_type_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650)

  df_procedure_type_concept_id <-generate_list_concepts(table_name,"procedure_type_concept_id.csv")
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 

  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"procedure_type_concept_id.csv", concept_tbl, data_tbl)) 
  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  # ORDINAL Fields
  field_name<-"procedure_source_value" #  3 minutes
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  # some fields can have multiple vocabularies
   message<-describeOrdinalField(df_table, table_name, field_name,ggplotting = F)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidFormat(), c(table_name),c(field_name)
                                                   ,2, data_tbl))  ## number of components
  
  flog.info(Sys.time())

  field_name<-"procedure_source_concept_id" #  3 minutes
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name)
  fileContent<-c(fileContent,message)
  
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl))
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  
  # some fields can have multiple vocabularies
  ### DQA CHECKPOINT ##########
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidVocab(), c(table_name),c(field_name), 
                                                   c('Procedure',c('HCPCS','CPT4', 'OPCS', 'ICD9Proc','ICD10PCS')),
                                                   concept_tbl, data_tbl)) 
  
  message<-describeOrdinalField(df_table, table_name, field_name,ggplotting = F)
  new_message<-""
  if(length(message)>0)
  {
    new_message<-create_meaningful_message_concept_id(concept_tbl, message,field_name)
  }
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));

  field_name<-"procedure_concept_id" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  
  # add % of no matching concept (concept id = 0). for the completeness report
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  
  ### DQA CHECKPOINT ##########
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidVocab(), c(table_name),c(field_name),
                                                   c('Procedure',c('HCPCS','CPT4', 'SNOMED','ICD9Proc','ICD10PCS')),
                                                   concept_tbl, data_tbl)) 
  
  message<-describeOrdinalField(df_table, table_name, field_name,ggplotting = F)
  new_message<-create_meaningful_message_concept_id(concept_tbl , message,field_name)
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));

  field_name<-"procedure_concept_id" #
  df_table_new<-retrieve_dataframe_count_group(data_tbl,"person_id", field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"by person_id","\n"))
  message<-describeOrdinalField(df_table_new, "person_id",field_name, ggplotting = F)
  # create meaningful message
  new_message<-create_meaningful_message_concept_id(concept_tbl, message,field_name)
  fileContent<-c(fileContent,new_message,paste_image_name("person_id",field_name));

  field_name<-"procedure_date" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeDateField(df_table, table_name, field_name)
  
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),data_tbl)) 
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);

  field_name<-"procedure_datetime" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeTimeField(df_table, table_name, field_name)
  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_datetime",sep="")));

  field_name<-"modifier_source_value" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_message<-reportMissingCount(df_table,table_name,field_name)
  fileContent<-c(fileContent,missing_message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(missing_message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  if(grepl("100",missing_message)==FALSE) # if 100% missing
  {
    message<-describeOrdinalField(df_table, table_name, field_name,ggplotting = F)
    fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  }
  
  flog.info(Sys.time())

  field_name="modifier_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  df_modifier <-generate_df_concepts(table_name,"modifier_concept_id_dplyr.txt", concept_tbl)
  order_bins <-c(df_modifier$concept_id,44814650,0,NA)

  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"modifier_concept_id_dplyr.txt", concept_tbl, data_tbl)) 
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  df_table_modifier_enhanced<-EnhanceFieldValues(df_table,field_name,df_modifier);
  missing_message<-reportMissingCount(df_table,table_name,field_name)
  fileContent<-c(fileContent,missing_message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(missing_message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  if(grepl("100",missing_message)==FALSE) # if not 100% missing
  {
    describeNominalField(df_table_modifier_enhanced,table_name,field_name);
    fileContent<-c(fileContent,paste_image_name(table_name,field_name));

    ## run the CA-014 DQA check only if there is some data in modifier_concept_id
    ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
    logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),
                                                     c(field_name, "modifier_source_value"),
                                                     data_tbl)) 
  }

  field_name<-"quantity" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name)
  fileContent<-c(fileContent,message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeOrdinalField(df_table, table_name, field_name,ggplotting = F)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  #FOREIGN KEY fields
  field_name<-"person_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                 paste_image_name_sorted(table_name,field_name),message);

  field_name<-"visit_occurrence_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  count_novisit<-retrieve_dataframe_clause(data_tbl,"count(*)","is.na(visit_occurrence_id)")
  missing_visit_percent<-round(count_novisit*100/df_total_procedure_count,2)
  
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_visit_percent, 
                                                           table_name, g_data_version));

  field_name<-"provider_id" #  4 minutes
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name)
  fileContent<-c(fileContent,message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                 paste_image_name_sorted(table_name,field_name),message);

  flog.info(Sys.time())
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
}
