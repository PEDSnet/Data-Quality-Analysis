generateVisitOccurrenceReport <- function() {
  flog.info(Sys.time())

  # read a table
  table_name<-"visit_occurrence"
  data_tbl <- cdm_tbl(req_env$db_src, table_name)
  concept_tbl <- vocab_tbl(req_env$db_src, "concept")

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),
                       "./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), 
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))

  #PRIMARY FIELD
  field_name<-"visit_occurrence_id"
  df_total_visit_count<-retrieve_dataframe_count(data_tbl,field_name)
  current_total_count<-as.numeric(df_total_visit_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", 
                                   formatC(current_total_count, format="d", big.mark=','),"\n"))
  
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  df_total_patient_count<-retrieve_dataframe_count(data_tbl,"person_id", distinction = T)
  fileContent<-c(fileContent,paste("The visit to patient ratio is ",
                                   round(df_total_visit_count[1][1]/df_total_patient_count[1][1],2),"\n"))
  
  # ORDINAL Fields
  
  field_name<-"visit_start_datetime"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeDateField(df_table, table_name, field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);
  message<-describeTimeField(df_table, table_name, field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,paste(field_name,"_datetime",sep="")),message);
  
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),data_tbl)) 
  field_name<-"visit_end_date"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))

  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  
  message<-describeDateField(df_table, table_name, field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);
  
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),data_tbl)) 
  
  flog.info(Sys.time())

  logFileData<-custom_rbind(logFileData,applyCheck(ImplEvent(), c(table_name), 
                                                   c('visit_start_date','visit_end_date'),data_tbl)) 
   
  # visit type concept id
  field_name="visit_type_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  df_visit_type <-generate_df_concepts(table_name,"visit_type_dplyr.txt", concept_tbl)
  order_bins <-c(df_visit_type$concept_id,0,NA)
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"visit_type_dplyr.txt", concept_tbl, data_tbl)) 
  
  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  df_table_visit_type_enhanced<-EnhanceFieldValues(df_table,field_name,df_visit_type);
  describeNominalField(df_table_visit_type_enhanced,table_name,field_name);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  field_name<-"visit_source_value"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name)
  fileContent<-c(fileContent,message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  describeNominalField(df_table, table_name, field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  # visit source concept id
  field_name="visit_source_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  if(missing_percent<100)
  {
  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  }

  # visit concept id
  field_name="visit_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
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
  df_visit <-generate_df_concepts(table_name,"visit_concept_id_dplyr.txt", concept_tbl)
  order_bins <-c(df_visit$concept_id, 2000000088, 0, NA)
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"visit_concept_id_dplyr.txt", concept_tbl, data_tbl)) 
  
  ###########DQA CHECKPOINT##############
  null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650)
  
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),
                                                   c(field_name, "visit_source_value"),data_tbl)) 
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl))
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name),
                                                   list(
                                                   list(9201, "inpatient"),
                                                   list(9202, "outpatient") ,
                                                   list(9203, "ED") ,
                                                   list(44814711,"other ambulatory")), data_tbl))

  df_table_visit_enhanced<-EnhanceFieldValues(df_table,field_name,df_visit);
  describeNominalField(df_table_visit_enhanced,table_name,field_name);
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));

  #visit / person id by visit types
  fileContent <-c(fileContent,paste("## Barplot for Visit:Patient ratio by visit type\n"))

  df_visit_patient_ratio<-retrieve_dataframe_ratio_group(data_tbl,
                                                         "visit_occurrence_id", "person_id",
                                                         "visit_concept_id")

  df_visit_patient_ratio[,1]<-paste(df_visit_patient_ratio[,1],"(",
                     df_visit[df_visit$concept_id==df_visit_patient_ratio[,1],2],")",sep="")

  describeOrdinalField(df_visit_patient_ratio,table_name,"ratio");
  fileContent<-c(fileContent,paste_image_name(table_name,"Visit:Patient ratio by visit type"));
  
  ### admitting source value 
  field_name="admitting_source_value"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_message<-reportMissingCount(df_table,table_name,field_name);
  fileContent<-c(fileContent,missing_message)
  missing_percent_source_value<-extract_numeric_value(missing_message)
  
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  if(missing_percent_source_value<100)
  {
  	describeNominalField(df_table,table_name,field_name)
  }
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  
  ### admitting source concept id 
  field_name="admitting_source_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"admitting_source_concept_id.csv", concept_tbl, data_tbl)) 
  df_admitting_source_concept_id <-generate_list_concepts(table_name,"admitting_source_concept_id.csv")
  
   ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #update values of the field before plotting
  df_admitting_source_concept_id_enhanced<-EnhanceFieldValues(df_table,field_name,df_admitting_source_concept_id);
  describeNominalField(df_admitting_source_concept_id_enhanced,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name))
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl))
  
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),
                                                   c(field_name, "admitting_source_value"),data_tbl)) 

  ### admitting source value 
  field_name="discharge_to_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  missing_message<-reportMissingCount(df_table,table_name,field_name);
  fileContent<-c(fileContent,missing_message)
  missing_percent_source_value<-extract_numeric_value(missing_message)
  
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  
  ### discharge to concept id 
  field_name="discharge_to_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"discharge_to_concept_id.csv", concept_tbl, data_tbl)) 
  df_discharge_to_concept_id <-generate_list_concepts(table_name,"discharge_to_concept_id.csv")
  
  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #update values of the field before plotting
  df_discharge_to_concept_id_enhanced<-EnhanceFieldValues(df_table,field_name,df_discharge_to_concept_id);
  describeNominalField(df_discharge_to_concept_id_enhanced,table_name,field_name);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl))
  
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),
                                                   c(field_name, "discharge_to_source_value"),data_tbl)) 
  
  #FOREIGN KEY field
  field_name<-"person_id"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                 paste_image_name_sorted(table_name,field_name),message);

  flog.info(Sys.time())

  field_name<-"provider_id"
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

  field_name<-"care_site_id" # 8 minutes
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

  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description",
                           "alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
}
