generateCareSiteReport <- function() {

  # read a table 
  table_name<-"care_site"
  data_tbl <- cdm_tbl(req_env$db_src, table_name)
  concept_tbl <- vocab_tbl(req_env$db_src, "concept")

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name,g_config)

  flog.info(g_data_version)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), 
                          issue_code=character(0), issue_description=character(0), check_alias=character(0)
                          , finding=character(0), prevalence=character(0))
  


  #PRIMARY FIELD(s)
  field_name<-"care_site_id"
  current_total_count<-as.numeric(describeIdentifier(data_tbl,field_name))
  fileContent<-c(fileContent,paste("The total number of unique values for ",field_name,"is: ",current_total_count ,"\n"))
   ### DQA CHECKPOINT ####################
   logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name),NULL,current_total_count)) 
  
  
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  #add care site identifier
  field_name<-"care_site_name"
  fileContent<-c(fileContent,paste("The total number of unique values for ",field_name,"is: ",
                                   describeIdentifier(data_tbl,field_name),"\n"))
  #get_previous_cycle_total_count()

  field_name<-"care_site_source_value"
  fileContent<-c(fileContent,paste("The total number of unique values for ",field_name,"is: ",
                                   describeIdentifier(data_tbl,field_name),"\n"))
  ### DQA CHECKPOINT ####################
  logFileData<-custom_rbind(logFileData,applyCheck(InconPK(), c(table_name), c("care_site_id",field_name),data_tbl)) 
  
  # ORDINAL Fields
  #place of service source value
  field_name="place_of_service_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(data_tbl,table_name,field_name)
  missing_percent_source_value<-extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_source_value)
  describeNominalField_basic(data_tbl, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  

  #place of service concept id

  field_name="place_of_service_concept_id"
   ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"place_of_service.csv", concept_tbl, data_tbl)) 
  
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  df_place_of_service <-generate_list_concepts(table_name, "place_of_service.csv")
  null_message<-reportNullFlavors(data_tbl,table_name,field_name,44814653,44814649,44814650)
  # flog.info( null_message)
  missing_percent_message<-reportMissingCount(data_tbl,table_name,field_name)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  
  ###########DQA CHECKPOINT##############
  #df_place_of_service <- as.data.frame(acceptable_PLOS)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table_place_of_service_enhanced<-EnhanceFieldValues(data_tbl,field_name,df_place_of_service);
  describeNominalField_basic(df_table_place_of_service_enhanced,table_name,field_name);
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,
                            applyCheck(InconSource(),c(table_name),
                                       c(field_name, "place_of_service_source_value"),data_tbl))

  # specialty source value
  field_name="specialty_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(data_tbl,table_name,field_name)
  missing_percent_source_value<-extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_source_value)
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  describeNominalField_basic(data_tbl, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #specialty concept id
  field_name="specialty_concept_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(data_tbl,table_name,field_name)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  
  describeNominalField_basic(data_tbl, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),
                                                   c(field_name, "specialty_source_value"),data_tbl)) 
  ##########DQA CHECKPOINT##############

  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"specialty.csv", concept_tbl, data_tbl)) 
  
  
  ## check for caresites with specific specialties
  ## for nephrology and GI 
  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name), 
                                                   list(
                                                   list(38004479,45756813, "nephrology") ,
                                                    list(45756810,38004455, "GI(Gastroenterology)")),
                                                   data_tbl))
  


  #FOREIGN KEY fields
  #location id
  field_name="location_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(data_tbl,table_name,field_name)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  
  message<-describeForeignKeyIdentifiers(data_tbl,table_name, field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                 paste_image_name_sorted(table_name,field_name),message);


  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  #logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
}
