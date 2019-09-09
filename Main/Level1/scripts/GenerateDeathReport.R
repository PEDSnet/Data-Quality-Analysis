generateDeathReport <- function() {

    # read a table 
  table_name<-"death"
  data_tbl <- cdm_tbl(req_env$db_src, table_name)
  concept_tbl <- vocab_tbl(req_env$db_src, "concept")

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), 
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))

  #PRIMARY FIELD(s)
  field_name<-"death_cause_id"
  current_total_count<-as.numeric(describeIdentifier(data_tbl,field_name))
  fileContent<-c(fileContent,paste("The total number of unique values for ",field_name,"is: ",current_total_count ,"\n"))
 
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 
  
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  field_name<-"person_id" 
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  message<-describeForeignKeyIdentifiers(data_tbl, "death_cause",field_name, group_ret = 0) ### error here?
  fileContent<-c(fileContent,paste_image_name("death_cause",field_name),paste_image_name_sorted("death_cause",field_name),message);

  #ORDINAL Fields
  
  field_name="death_date"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeDateField(data_tbl, table_name,field_name, group_ret = 0)
  
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),data_tbl)) 
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message)
  field_name<-"death_datetime"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeTimeField(data_tbl, table_name,field_name)
  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_datetime",sep="")));

  #death type concept id

  field_name="death_type_concept_id"

  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"death_type_dplyr.txt", concept_tbl, data_tbl)) 
  
  df_death_type_concept_id <-generate_df_concepts(table_name, "death_type_dplyr.txt", concept_tbl)
  
  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  describeOrdinalField(data_tbl, table_name,field_name, group_ret = 0)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  #cause of death source valueapply_check_type_1("BA-002"
  field_name="cause_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(data_tbl, table_name, field_name, group_ret = 0)
  fileContent<-c(fileContent,message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  describeOrdinalField(data_tbl, table_name,field_name, group_ret = 0)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  field_name="cause_source_concept_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(data_tbl,table_name,field_name, group_ret = 0)
  fileContent<-c(fileContent,message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  describeOrdinalField(data_tbl, table_name,field_name, group_ret = 0)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #cause of death concept id
  field_name="cause_concept_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(data_tbl,table_name,field_name, group_ret = 0)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 

  ###########DQA CHECKPOINT --vocabulary check ##############
  
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidVocab(), c(table_name),c(field_name),
                                                   c('Condition','SNOMED','PCORNet'), concept_tbl, data_tbl)) 

   ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),
                                                   c(field_name, "cause_source_value"),data_tbl)) 
  
  
  describeOrdinalField(data_tbl, table_name,field_name, group_ret = 0)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  field_name="death_impute_concept_id"
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"death_impute_dplyr.txt", concept_tbl, data_tbl)) 
  df_death_impute_concept_id <-generate_df_concepts(table_name, "death_impute_dplyr.txt", concept_tbl)
  
  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name), data_tbl)) 
  describeOrdinalField(data_tbl, table_name,field_name, group_ret = 0)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
}
