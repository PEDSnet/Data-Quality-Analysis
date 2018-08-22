generateVisitPayerReport <- function() {

  # read a table 
  table_name<-"visit_payer"
  data_tbl <- cdm_tbl(req_env$db_src, table_name)
  concept_tbl <- vocab_tbl(req_env$db_src, "concept")
  
  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0),
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))

  #PRIMARY FIELD
  field_name<-"visit_payer_id"
  df_total_procedure_count<-retrieve_dataframe_count(data_tbl,field_name)
  current_total_count<-as.numeric(df_total_procedure_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", 
                                   formatC(current_total_count, format="d", big.mark=','),"\n"))
  
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name),NULL,current_total_count)) 
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  df_total_visit_count<-retrieve_dataframe_count(data_tbl,"visit_occurrence_id", distinction = T)
  fileContent<-c(fileContent,paste("The visit_payer to visit ratio is ",
                                   round(df_total_procedure_count[1][1]/df_total_visit_count[1][1],2),"\n"))

  field_name<-"visit_occurrence_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))

  field_name = "plan_type"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name)
  fileContent<-c(fileContent,message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl))
  
  ############# DQA WARNING ######################
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidValue(), c(table_name),c(field_name)
                                                   ,"plan_type.csv", data_tbl)) 
  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  field_name = "plan_class"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  
  ############# DQA WARNING ######################
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidValue(), c(table_name),c(field_name)
                                                   ,"plan_class.csv", data_tbl)) 
  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  flog.info(Sys.time())

  ### visit payer type concept id 
  field_name<-"visit_payer_type_concept_id" #
  if(field_name %in% colnames(data_tbl)){  ##Field is not in oracle test db
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"visit_payer_type_concept_id.csv", concept_tbl, data_tbl)) 
  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name))
  }
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
}
