generateImmunizationReport <- function() {
  flog.info(Sys.time())

  # read a table
  table_name <- "immunization"
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
  field_name<-"immunization_id"
  df_total_procedure_count<-retrieve_dataframe_count(data_tbl,field_name)
  current_total_count<-as.numeric(df_total_procedure_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:",
                                   formatC(current_total_count, format="d", big.mark=','),"\n"))
  
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  field_name<-"immunization_source_value" #  3 minutes
  
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeOrdinalField(df_table, table_name, field_name, ggplotting = F)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidFormat(), c(table_name),c(field_name)
                                                   , 2, data_tbl))  ## number of components

  field_name<-"immunization_source_concept_id" #  3 minutes
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
                                                   c('Drug',c('NDC','RxNorm', 'CVX', 'CPT4','ICD9CM'
                                                   , 'ICD10','HCPCS','OPCS4')),
                                                   concept_tbl, data_tbl)) 
  paste("check 4")
  message<-describeOrdinalField(df_table, table_name, field_name, ggplotting = F)
  new_message<-""
  if(length(message)>0)
  {
    new_message<-create_meaningful_message_concept_id(concept_tbl, message,field_name)
  }
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));

   flog.info(Sys.time())

  field_name<-"immunization_concept_id" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))

  # add % of no matching concept (concept id = 0). for the completeness report
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  ### DQA CHECKPOINT ##########
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidVocab(), c(table_name),c(field_name), 
                                                    c('Drug',c('CVX')),
                                                   concept_tbl, data_tbl))
  
  message<-describeOrdinalField(df_table, table_name, field_name, ggplotting = F)
  new_message<-create_meaningful_message_concept_id(concept_tbl, message,field_name)
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));

   flog.info(Sys.time())

  field_name<-"immunization_date" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeDateField(df_table, table_name, field_name)
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),data_tbl)) 
  
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);

  field_name<-"immunization_datetime" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeTimeField(df_table, table_name, field_name)
  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_datetime",sep="")));

  field_name<-"imm_route_source_value" #
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_message<-reportMissingCount(df_table,table_name,field_name)
  fileContent<-c(fileContent,missing_message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(missing_message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  if(grepl("100",missing_message)==FALSE) # if 100% missing
  {
    message<-describeOrdinalField(df_table, table_name, field_name, ggplotting = F)
    fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  }
  field_name="imm_route_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  df_modifier <-generate_df_concepts(table_name,"imm_route_concept_id_dplyr.txt", concept_tbl)

  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"imm_route_concept_id_dplyr.txt", concept_tbl, data_tbl)) 
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  df_table_modifier_enhanced<-EnhanceFieldValues(df_table,field_name,df_modifier);
  missing_message<-reportMissingCount(df_table,table_name,field_name)
  fileContent<-c(fileContent,missing_message)
  print("TEST 5")
  
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
                                                     c(field_name, "imm_route_source_value"),data_tbl)) 
  }

 field_name<-"imm_dose_unit_source_value" #
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
   print("TEST 6")

  field_name="imm_dose_unit_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)

  df_modifier <-generate_df_concepts(table_name,"imm_dose_unit_concept_id_dplyr.txt", concept_tbl)

  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"imm_dose_unit_concept_id_dplyr.txt",
                                                   concept_tbl, data_tbl)) 
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  print("TEST 7")
  
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  df_table_modifier_enhanced<-EnhanceFieldValues(df_table,field_name,df_modifier);
  missing_message<-reportMissingCount(df_table,table_name,field_name)
  fileContent<-c(fileContent,missing_message)
  print("TEST 8")
  
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
                                                     c(field_name, "imm_dose_unit_source_value"),data_tbl)) 
  }
  
  field_name<-"immunization_dose" #
    df_table<-retrieve_dataframe_group(data_tbl,field_name)
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
    message<-reportMissingCount(df_table,table_name,field_name)
    fileContent<-c(fileContent,message)
    
    ###########DQA CHECKPOINT -- missing information##############
    missing_percent<-extract_numeric_value(message)
    logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
    message<-describeForeignKeyIdentifiers(df_table, table_name,field_name)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);
 
  #FOREIGN KEY fields

  field_name<-"person_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

    field_name<-"visit_occurrence_id"
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
    
     field_name<-"procedure_occurrence_id"
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

    field_name<-"provider_id" #  4 minutes
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name)
  fileContent<-c(fileContent,message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name), data_tbl)) 
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
