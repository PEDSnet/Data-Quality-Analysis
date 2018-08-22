generateVisitOccurrenceReport <- function() {
  flog.info(Sys.time())
  big_data_flag<-TRUE

  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)

  # read a table into an R dataframe
  table_name<-"visit_occurrence"

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
  field_name<-"visit_occurrence_id"
  df_total_visit_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  current_total_count<-as.numeric(df_total_visit_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 
  
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  df_total_patient_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct person_id")
  fileContent<-c(fileContent,paste("The visit to patient ratio is ",round(df_total_visit_count[1][1]/df_total_patient_count[1][1],2),"\n"))

  #NOMINAL Fields

  # ORDINAL Fields


    field_name<-"visit_start_datetime"
    df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
    fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
    #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
    message<-describeDateField(df_table, table_name, field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);
    message<-describeTimeField(df_table, table_name, field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,paste(field_name,"_datetime",sep="")),message);
  
  
    ### DQA checkpoint - future date
    logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
    
    
    
   flog.info(Sys.time())

  field_name<-"visit_end_date"
  print('crossed1')
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  
  print('crossed2')
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  #fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  #missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  
  message<-describeDateField(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);
  
  
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
  print('crossed3')
  
  # check for plausibility
  
   flog.info(Sys.time())

   logFileData<-custom_rbind(logFileData,applyCheck(ImplEvent(), c(table_name), c('visit_start_date','visit_end_date'),con)) 
   
  ## visit end time
  #field_name<-"visit_end_datetime"
  #df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  #missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  #missing_percent<- extract_numeric_value(missing_percent_message)
  #fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  #logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));

  # visit type concept id
  field_name="visit_type_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  df_visit_type <-generate_df_concepts(con, table_name,"visit_type.txt")
  order_bins <-c(df_visit_type$concept_id,0,NA)
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "visit_type.txt")) 
  
  #print('crossed2')
  
  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  #fileContent<-c(fileContent,unexpected_message)
  df_table_visit_type_enhanced<-EnhanceFieldValues(df_table,field_name,df_visit_type);
  describeNominalField_basic(df_table_visit_type_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

   flog.info(Sys.time())

  field_name<-"visit_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  describeNominalField_basic(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  # visit source concept id
  field_name="visit_source_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  if(missing_percent<100)
  {
  describeNominalField_basic(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  }


  # visit concept id
  field_name="visit_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  ### write fact type counts 
  fact_type_count<-df_table[df_table$visit_concept_id==9201,2]
  write_total_fact_type_counts(table_name,"Inpatient" , fact_type_count)
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiffFactType(), c(table_name), c(field_name)
                                                   ,c("Inpatient",fact_type_count))) 
  
  fact_type_count<-df_table[df_table$visit_concept_id==9202,2]
  write_total_fact_type_counts(table_name,"Outpatient" , fact_type_count)
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiffFactType(), c(table_name), c(field_name)
                                                   ,c("Outpatient",fact_type_count))) 
  
  fact_type_count<-df_table[df_table$visit_concept_id==9203,2]
  write_total_fact_type_counts(table_name,"ED" , fact_type_count)
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiffFactType(), c(table_name), c(field_name)
                                                   ,c("ED",fact_type_count))) 
  
  
  
  ###########DQA CHECKPOINT##############
  df_visit <-generate_df_concepts(con, table_name,"visit_concept_id.txt")
  order_bins <-c(df_visit$concept_id, 2000000088, 0, NA)
  #unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "visit_concept_id.txt")) 
  ###########DQA CHECKPOINT##############
  null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650,big_data_flag)
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "visit_source_value"),con
  )) 
  
  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name),con, 
                                                   list(
                                                   list(9201, "inpatient"),
                                            list(9202, "outpatient") ,
                                  list(9203, "ED") ,
                                      list(44814711,"other ambulatory")
                                          )))

  #fileContent<-c(fileContent,unexpected_message)
  df_table_visit_enhanced<-EnhanceFieldValues(df_table,field_name,df_visit);
  describeNominalField_basic(df_table_visit_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));

  #visit / person id by visit types
  fileContent <-c(fileContent,paste("## Barplot for Visit:Patient ratio by visit type\n"))
  df_visit_patient_ratio<-retrieve_dataframe_ratio_group(con, g_config,table_name,"round(count(distinct visit_occurrence_id)/count(distinct person_id),2)", "visit_concept_id")

  for(i in 1:nrow(df_visit_patient_ratio))
      {
          #df_visit[df_visit$concept_id==9201,2]
            label<-df_visit[df_visit$concept_id==df_visit_patient_ratio[i,1],2]
        df_visit_patient_ratio[i,1]<-paste(df_visit_patient_ratio[i,1],"(",label,")",sep="")
          #df_visit_patient_ratio[i,1]
          }
  describeOrdinalField(df_visit_patient_ratio,table_name,"Visit:Patient ratio by visit type",big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,"Visit:Patient ratio by visit type"));

  
  
  
  ### admitting source value 
  field_name="admitting_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,missing_message)
  missing_percent_source_value<-extract_numeric_value(missing_message)
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  if(missing_percent_source_value<100)
  {
  	describeNominalField_basic(df_table,table_name,field_name,big_data_flag)
  }
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  
  ### admitting source concept id 
  field_name="admitting_source_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "admitting_source_concept_id.csv")) 
  df_admitting_source_concept_id <-generate_list_concepts(table_name,"admitting_source_concept_id.csv")
  
   ###########DQA CHECKPOINT##############
  # flog.info(unexpected_message)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #update values of the field before plotting
  df_admitting_source_concept_id_enhanced<-EnhanceFieldValues(df_table,field_name,df_admitting_source_concept_id);
  describeNominalField_basic(df_admitting_source_concept_id_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con))
  
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "admitting_source_value"),con
  )) 
  
  
  
  ### admitting source value 
  field_name="discharge_to_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  missing_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,missing_message)
  missing_percent_source_value<-extract_numeric_value(missing_message)
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  describeNominalField_basic(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  
  ### discharge to concept id 
  field_name="discharge_to_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "discharge_to_concept_id.csv")) 
  df_discharge_to_concept_id <-generate_list_concepts(table_name,"discharge_to_concept_id.csv")
  
  ###########DQA CHECKPOINT##############
  # flog.info(unexpected_message)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #update values of the field before plotting
  df_discharge_to_concept_id_enhanced<-EnhanceFieldValues(df_table,field_name,df_discharge_to_concept_id);
  describeNominalField_basic(df_discharge_to_concept_id_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con))
  
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "discharge_to_source_value"),con
  )) 
  
  #FOREIGN KEY fields

  field_name<-"person_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

   flog.info(Sys.time())

  field_name<-"provider_id"
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
  field_name<-"care_site_id" # 8 minutes
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

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description",
                           "alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)


  #close the connection
  close_database_connection_OHDSI(con, g_config)
}
