generateVisitPayerReport <- function() {
  flog.info(Sys.time())

  big_data_flag<-TRUE # for query wise analysis

  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)

  # read a table into an R dataframe
  table_name<-"visit_payer"
  #df_table <- retrieve_dataframe_OHDSI(con, g_config,table_name)

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0),
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))



  #PRIMARY FIELD
  field_name<-"visit_payer_id"
  df_total_procedure_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  current_total_count<-as.numeric(df_total_procedure_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name),NULL,current_total_count)) 

  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  df_total_visit_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct visit_occurrence_id")
  fileContent<-c(fileContent,paste("The visit_payer to visit ratio is ",round(df_total_procedure_count[1][1]/df_total_visit_count[1][1],2),"\n"))

  field_name<-"visit_occurrence_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  # not showing dut to memory issues
  flog.info(Sys.time())
  

  field_name = "plan_type"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  order_bins <-c("HMO","PPO","POS","Fee for service","Other/Unknown",NA)
  label_bins<-c("HMO","PPO","POS","Fee for service","Other/Unknown","NULL")
  color_bins <-c("HMO"="lightcoral","PPO"="steelblue1","POS"="red","Fee for service"="grey64","Other/Unknown"="grey64")
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  #unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ############# DQA WARNING ######################
  #logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-001", field_name, unexpected_message, table_name, g_data_version));
  # flog.info(unexpected_message)
  #fileContent<-c(fileContent,unexpected_message)
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidValue(), c(table_name),c(field_name)
                                                   ,con,  "plan_type.csv")) 
  
  describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  field_name = "plan_class"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  order_bins <-c("Private/Commercial","Medicaid/sCHIP","Medicare","Other public","Self-pay","Other/Unknown",NA)
  label_bins<-c("Private/Commercial","Medicaid/sCHIP","Medicare","Other public","Self-pay","Other/Unknown","NULL")
  color_bins <-c("Private/Commercial"="lightcoral","Medicaid/sCHIP"="steelblue1","Medicare"="red","Other public"="grey64","Self-pay"="grey64","Other/Unknown"="grey64")
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ############# DQA WARNING ######################
  #logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-001", field_name, unexpected_message, table_name, g_data_version));
  # flog.info(unexpected_message)
  #fileContent<-c(fileContent,unexpected_message)
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidValue(), c(table_name),c(field_name)
                                                   ,con,  "plan_class.csv")) 
  
  describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

   flog.info(Sys.time())

  ### visit payer type concept id 
  field_name<-"visit_payer_type_concept_id" #
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  order_bins <-c("31968","31969","44814653", "44814649","44814650","0",NA)
  label_bins<-c("primary (31968)","secondary (31969)","0 (No matching)","Unknown (44814653)",
                "Other (44814649)","No Information (44814650 )","NULL")
  color_bins <-c("31968"="lightcoral","31969"="steelblue1","0"="red","44814653"="grey64","44814649"="grey64","44814650 "="grey64")
  
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "visit_payer_type_concept_id.csv")) 
  
  describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  
  
  
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
