generateCareSiteReport <- function() {

  big_data_flag<-FALSE
  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)

  # read a table into an R dataframe
  table_name<-"care_site"
  df_care_site <- retrieve_dataframe_OHDSI(con, g_config,table_name)

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
  current_total_count<-as.numeric(describeIdentifier(df_care_site,field_name))
  fileContent<-c(fileContent,paste("The total number of unique values for ",field_name,"is: ",current_total_count ,"\n"))
   ### DQA CHECKPOINT ####################
   logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name),NULL,current_total_count)) 
  
  
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  #add care site identifier
  field_name<-"care_site_name"
  fileContent<-c(fileContent,paste("The total number of unique values for ",field_name,"is: ", describeIdentifier(df_care_site,field_name),"\n"))
  #get_previous_cycle_total_count()

  field_name<-"care_site_source_value"
  fileContent<-c(fileContent,paste("The total number of unique values for ",field_name,"is: ", describeIdentifier(df_care_site,field_name),"\n"))
  ### DQA CHECKPOINT ####################
  logFileData<-custom_rbind(logFileData,applyCheck(InconPK(), c(table_name), c("care_site_id",field_name),con)) 
  
  # ORDINAL Fields
  #place of service source value
  field_name="place_of_service_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_care_site,table_name,field_name,big_data_flag)
  missing_percent_source_value<-extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_source_value)
  describeNominalField_basic(df_care_site, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  

  #place of service concept id

  field_name="place_of_service_concept_id"
   ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "place_of_service.csv")) 
  
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  df_place_of_service <-generate_list_concepts(table_name, "place_of_service.csv")
  null_message<-reportNullFlavors(df_care_site,table_name,field_name,44814653,44814649,44814650,big_data_flag)
  # flog.info( null_message)
  missing_percent_message<-reportMissingCount(df_care_site,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  
  ###########DQA CHECKPOINT##############
  #df_place_of_service <- as.data.frame(acceptable_PLOS)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table_place_of_service_enhanced<-EnhanceFieldValues(df_care_site,field_name,df_place_of_service);
  describeNominalField_basic(df_table_place_of_service_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "place_of_service_source_value"),con
  )) 
  
  


  # specialty source value
  field_name="specialty_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_care_site,table_name,field_name,big_data_flag)
  missing_percent_source_value<-extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_source_value)
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  describeNominalField_basic(df_care_site, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #specialty concept id
  field_name="specialty_concept_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_care_site,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  
  describeNominalField_basic(df_care_site, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "specialty_source_value"),con
  )) 
  ##########DQA CHECKPOINT##############


  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "specialty.csv")) 
  
  
  ## check for caresites with specific specialties
  ## for nephrology and GI 
  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name),con, 
                                                   list(
                                                   list(38004479,45756813, "nephrology") ,
                                                    list(45756810,38004455, "GI(Gastroenterology)")
                                                          )))
  


  #FOREIGN KEY fields
  #location id
  field_name="location_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_care_site,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  message<-describeForeignKeyIdentifiers(df_care_site,table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);


  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  #logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)

  #close the connection
  close_database_connection_OHDSI(con, g_config)
}
