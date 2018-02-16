generateImmunizationReport <- function() {
  flog.info(Sys.time())

  big_data_flag<-TRUE # for query wise analysis

  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)
  # read a table into an R dataframe
  table_name<-"immunization"
  #df_table <- retrieve_dataframe_OHDSI(con, g_config,table_name)

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0),
                          issue_description=character(0), alias=character()
                          , finding=character(0), prevalence=character(0))


  #PRIMARY FIELD
  field_name<-"immunization_id"
  df_total_procedure_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  current_total_count<-as.numeric(df_total_procedure_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 

  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)

    
  #df_total_patient_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct person_id")
  #fileContent<-c(fileContent,paste("The immunization to patient ratio is ",round(df_total_procedure_count[1][1]/df_total_patient_count[1][1],2),"\n"))
  #df_total_visit_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct visit_occurrence_id")
  #fileContent<-c(fileContent,paste("The immunization to visit ratio is ",round(df_total_procedure_count[1][1]/df_total_visit_count[1][1],2),"\n"))

  
  
  field_name<-"immunization_source_value" #  3 minutes
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeOrdinalField_large(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidFormat(), c(table_name),c(field_name)
                                                   ,con,  2))  ## number of components
  
  
   flog.info(Sys.time())


  field_name<-"immunization_source_concept_id" #  3 minutes
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  # some fields can have multiple vocabularies
  ### DQA CHECKPOINT ##########
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidVocab(), c(table_name),c(field_name),con, 
                                                   c('Drug',c('NDC','RxNorm', 'CVX', 'CPT4','ICD9CM'
                                                   , 'ICD10','HCPCS','OPCS4')))) 
  
  message<-describeOrdinalField_large(df_table, table_name, field_name,big_data_flag)
  new_message<-""
  if(length(message)>0)
  {
    new_message<-create_meaningful_message_concept_id(message,field_name,con, g_config)
  }
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));

   flog.info(Sys.time())

  field_name<-"immunization_concept_id" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  # add % of no matching concept (concept id = 0). for the completeness report
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ### DQA CHECKPOINT ##########
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidVocab(), c(table_name),c(field_name),con, 
                                                    c('Drug',c('CVX'))))
  
  message<-describeOrdinalField_large(df_table, table_name, field_name,big_data_flag)
  new_message<-create_meaningful_message_concept_id(message,field_name,con, g_config)
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));

   flog.info(Sys.time())

  field_name<-"immunization_date" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeDateField(df_table, table_name, field_name,big_data_flag)
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
  
  
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);

  field_name<-"immunization_datetime" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeTimeField(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_datetime",sep="")));

   flog.info(Sys.time())

  field_name<-"imm_route_source_value" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,missing_message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(missing_message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  if(grepl("100",missing_message)==FALSE) # if 100% missing
  {
    message<-describeOrdinalField_large(df_table, table_name, field_name,big_data_flag)
    fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  }
   flog.info(Sys.time())


  field_name="imm_route_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)

  df_modifier <-generate_df_concepts(con, table_name,"imm_route_concept_id.txt")

  order_bins <-c(df_modifier$concept_id,44814650,0,NA)
  #unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "imm_route_concept_id.txt")) 
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  #fileContent<-c(fileContent,unexpected_message)
  df_table_modifier_enhanced<-EnhanceFieldValues(df_table,field_name,df_modifier);
  missing_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,missing_message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(missing_message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  if(grepl("100",missing_message)==FALSE) # if not 100% missing
  {
    describeNominalField_basic(df_table_modifier_enhanced,table_name,field_name,big_data_flag);
    fileContent<-c(fileContent,paste_image_name(table_name,field_name));

    ## run the CA-014 DQA check only if there is some data in modifier_concept_id
    ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
    logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "imm_route_source_value"),con
    )) 
    
  }

 field_name<-"imm_dose_unit_source_value" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,missing_message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(missing_message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  if(grepl("100",missing_message)==FALSE) # if 100% missing
  {
    message<-describeOrdinalField_large(df_table, table_name, field_name,big_data_flag)
    fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  }
   flog.info(Sys.time())


  field_name="imm_dose_unit_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)

  df_modifier <-generate_df_concepts(con, table_name,"imm_dose_unit_concept_id.txt")

  order_bins <-c(df_modifier$concept_id,44814650,0,NA)
  #unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "imm_dose_unit_concept_id.txt")) 
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  #fileContent<-c(fileContent,unexpected_message)
  df_table_modifier_enhanced<-EnhanceFieldValues(df_table,field_name,df_modifier);
  missing_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,missing_message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(missing_message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  if(grepl("100",missing_message)==FALSE) # if not 100% missing
  {
    describeNominalField_basic(df_table_modifier_enhanced,table_name,field_name,big_data_flag);
    fileContent<-c(fileContent,paste_image_name(table_name,field_name));

    ## run the CA-014 DQA check only if there is some data in modifier_concept_id
    ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
    logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "imm_dose_unit_source_value"),con
    )) 
    
  }
  

field_name<-"immunization_dose" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);


 

  #FOREIGN KEY fields

  field_name<-"person_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

   flog.info(Sys.time())

    field_name<-"visit_occurrence_id"
    df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
    message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,message)
    ###########DQA CHECKPOINT -- missing information##############
    missing_percent<-extract_numeric_value(message)
    logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
    message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);
    
    flog.info(Sys.time())
    
     field_name<-"procedure_occurrence_id"
     df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
     fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
     message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
     fileContent<-c(fileContent,message)
     ###########DQA CHECKPOINT -- missing information##############
     missing_percent<-extract_numeric_value(message)
     logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
     message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
     fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);
     
     flog.info(Sys.time())
    

    field_name<-"provider_id" #  4 minutes
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

   flog.info(Sys.time())

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
