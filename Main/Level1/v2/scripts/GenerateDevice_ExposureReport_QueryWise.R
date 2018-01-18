library(DBI)
library(yaml)

flog.info(Sys.time())

generateDeviceExposureReport <- function(g_data_version) {
  big_data_flag<-TRUE
  
  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)
  
  table_name<-"device_exposure"
  #df_table <- retrieve_dataframe_OHDSI(con, g_config,table_name)
  # flog.info(nrow(df_table))
  
  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)
  
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), 
                          issue_code=character(0), issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))
  
  
  test <-1
  
  #PRIMARY FIELD
  field_name<-"device_exposure_id"
  df_total_measurement_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  current_total_count<-as.numeric(df_total_measurement_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
   ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 
  
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  df_total_patient_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct person_id")
  fileContent<-c(fileContent,paste("The device exposure to patient ratio is ",round(df_total_measurement_count[1][1]/df_total_patient_count[1][1],2),"\n"))
  df_total_visit_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct visit_occurrence_id")
  fileContent<-c(fileContent,paste("The device exposure to visit ratio is ",round(df_total_measurement_count[1][1]/df_total_visit_count[1][1],2),"\n"))
  
  # visit concept id
  df_visit <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name"
                                       ,"vocabulary_id ='Visit' or (vocabulary_id='PCORNet' and concept_class_id='Encounter Type')
                                       or (vocabulary_id = 'PCORNet' and concept_class_id='Undefined')")
  
  #condition / person id by visit types
  fileContent <-c(fileContent,paste("## Barplot for Device:Patient ratio by visit type\n"))
  df_device_patient_ratio<-retrieve_dataframe_ratio_group_join(con, g_config,table_name,"visit_occurrence",
                                                             "round(count(distinct device_exposure_id)/count(distinct device_exposure.person_id),2)",
                                                             "visit_concept_id","visit_occurrence_id")
  
  for(i in 1:nrow(df_device_patient_ratio))
  {
    #df_visit[df_visit$concept_id==9201,2]
    label<-df_visit[df_visit$concept_id==df_device_patient_ratio[i,1],2]
    df_device_patient_ratio[i,1]<-paste(df_device_patient_ratio[i,1],"(",label,")",sep="")
    #df_visit_patient_ratio[i,1]
  }
  describeOrdinalField(df_device_patient_ratio,table_name,"Device:Patient ratio by visit type",big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,"Device:Patient ratio by visit type"));
  
  #NOMINAL Fields
  
  field_name<-"person_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);
  
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
  
  
  ##### ##### ##### ##### May Need Work ##### ##### ##### ##### ##### #####
  
  field_name<-"device_concept_id" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  #unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "device_concept_id.txt")) 
  df_device_concept_id <-generate_df_concepts(con, table_name, "device_concept_id.txt")
  
  order_bins <-c(df_device_concept$concept_id,0,NA)
  
  ###########DQA CHECKPOINT -- missing information##############
  # add % of no matching concept (concept id = 0). for the completeness report
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  
  ### DQA CHECKPOINT ##########
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidVocab(), c(table_name),c(field_name),con, 
                                                   c('Device','HCPCS'))) 
  
  
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  # create meaningful message
  new_message<-create_meaningful_message_concept_id(message,field_name,con, g_config)
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));
  
  
  null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650 ,big_data_flag)
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "device_source_value"),con
  )) 
  
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
  
  field_name<-"device_source_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  new_message<-""
  if(length(message)>0)
  {
    # create meaningful message
    new_message<-create_meaningful_message_concept_id(message,field_name,con, g_config)
  }
  fileContent<-c(fileContent,new_message,paste_image_name(table_name,field_name));
  
  
  field_name<-"device_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  
  
  ######## Need to have discussion on what goes into this field before we can implement #######
  
  # field_name = "device_type_concept_id"
  # df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  # order_bins <-c("2000000033","2000000032","44818702","44818703","44818704","45754907",NA)
  # label_bins<-c("Vital Sign from healthcare delivery setting (2000000033)","Vital Sign from healthcare device (2000000032)","Lab result (44818702)","Pathology finding (44818703)","Patient reported value (44818704)","Derived Value (45754907)","NULL")
  # color_bins <-c("2000000033"="lightcoral","2000000032"="steelblue1","44818702"="red","44818703"="grey64","44818704"="grey64","45754907"="grey64")
  # fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  # #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  # unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  # ###########DQA CHECKPOINT##############
  # no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  # fileContent<-c(fileContent,no_matching_message)
  # logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));
  # logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version));
  # fileContent<-c(fileContent,unexpected_message)
  # describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
  # fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  # 
  # if( nrow(subset(df_table,df_table$measurement_type_concept_id==2000000033))==0 && nrow(subset(df_table,df_table$measurement_type_concept_id==2000000032))==0 )  
  # {
  #   fileContent<-c(fileContent,"DQA WARNING: No Device Type","\n");
  #   logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No Vitals sign records found", table_name, g_data_version));
  #   
  # }
  # if(nrow(subset(df_table,df_table$measurement_type_concept_id==44818702))==0)
  # {
  #   fileContent<-c(fileContent,"DQA WARNING: No Lab Records","\n");
  #   logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No Lab records found", table_name, g_data_version));
  #   
  # }
  # 
  
  
  
  
  field_name<-"device_exposure_start_date"
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
  
  
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  
  
  field_name<-"device_exposure_end_date"
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)
  if(missing_percent<100)
  {
    ### DQA checkpoint - future date
    logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
    
    
  }
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  
  
  
  field_name<-"unique_device_id" 
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);
  
  
  
  field_name<-"quantity" 
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);
  
  
  
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)
  
  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
  
  #close the connection
  close_database_connection_OHDSI(con,g_config)
}
