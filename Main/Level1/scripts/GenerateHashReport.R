generateHashReport <- function() {

  # read a table 
  table_name<-"hash_token"
  data_tbl <- cdm_tbl(req_env$db_src, table_name)
  concept_tbl <- vocab_tbl(req_env$db_src, "concept")

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),
                       "./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),
                          field=character(0), issue_code=character(0), 
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))

  #PRIMARY FIELD(s)
  field_name<-"person_id"
  df_total_person_id<-retrieve_dataframe_count(data_tbl,field_name)
  message<-reportMissingCount(df_total_person_id,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl))  

  current_total_count<-as.numeric(df_total_person_id[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", 
                                   formatC(current_total_count, format="d", big.mark=','),"\n"))
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 

  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  #####Check Missingness of Tokens#####
  field_name<-"token_01"
  df_total_person_id<-retrieve_dataframe_count(data_tbl,field_name)
  message<-reportMissingCount(df_total_person_id,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  
  field_name<-"token_02"
  df_total_person_id<-retrieve_dataframe_count(data_tbl,field_name)
  message<-reportMissingCount(df_total_person_id,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  
  field_name<-"token_03"
  df_total_person_id<-retrieve_dataframe_count(data_tbl,field_name)
  message<-reportMissingCount(df_total_person_id,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  
  field_name<-"token_04"
  df_total_person_id<-retrieve_dataframe_count(data_tbl,field_name)
  message<-reportMissingCount(df_total_person_id,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  
  field_name<-"token_05"
  df_total_person_id<-retrieve_dataframe_count(data_tbl,field_name)
  message<-reportMissingCount(df_total_person_id,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 

  field_name<-"token_16"
  df_total_person_id<-retrieve_dataframe_count(data_tbl,field_name)
  message<-reportMissingCount(df_total_person_id,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 

  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)
  
  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
}
