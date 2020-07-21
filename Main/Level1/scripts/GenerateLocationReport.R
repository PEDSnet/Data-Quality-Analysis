generateLocationReport <- function() {

  # read a table into an R dataframe
  table_name<-"location"
  data_tbl <- cdm_tbl(req_env$db_src, table_name)
  concept_tbl <- vocab_tbl(req_env$db_src, "concept")  

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), 
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),
                       "./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  #PRIMARY FIELD(s)
  field_name<-"location_id"
  message(field_name)
  current_total_count<-as.numeric(describeIdentifier(data_tbl,field_name))
  fileContent<-c(fileContent,paste("The total number of unique values for ",
                                   field_name,"is: ",current_total_count ,"\n"))
  print('test 1')
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 
  
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  field_name<-"location_source_value"
  message(field_name)
  fileContent<-c(fileContent,paste("The total number of",field_name,"is: ",
                                   describeIdentifier(data_tbl, field_name),"\n"))
  print('test 2')
  ###########DQA CHECKPOINT##############  total id different from source value
  logFileData<-custom_rbind(logFileData,applyCheck(InconPK(), c(table_name), 
                                                   c("location_id",field_name),data_tbl)) 
  print('test 3')
    ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 

  field_name="state"
  message(field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  describeOrdinalField(data_tbl, table_name,field_name, ggplotting = F, group_ret = 0)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidValue(), c(table_name),c(field_name)
                                                   ,"zip.csv", data_tbl)) 
  print('test 4')
  field_name="city"
  message(field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  describeOrdinalField(data_tbl, table_name,field_name, ggplotting = F, group_ret = 0)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  print('test 5')
  field_name="zip"
  message(field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeOrdinalField(data_tbl, table_name,field_name,ggplotting = F, group_ret = 0)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);
  print('test 6')
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
}
