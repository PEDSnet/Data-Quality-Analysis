generateProviderReport <- function() {
  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)

  # read a table into an R dataframe
  table_name<-"provider"
  df_provider <- retrieve_dataframe_OHDSI(con, g_config,table_name)

  big_data_flag<-FALSE

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), 
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))


  #PRIMARY FIELD(s)
  field_name<-"provider_id"
  current_total_count<-as.numeric(describeIdentifier(df_provider,field_name))
  fileContent<-c(fileContent,paste("The total number of unique values for ",field_name,"is: ",current_total_count ,"\n"))
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 
  
  field_name<-"provider_source_value"
  fileContent<-c(fileContent,paste("The total number of",field_name,"is: ", describeIdentifier(df_provider,field_name),"\n"))
  ###########DQA CHECKPOINT##############  total id different from source value
  logFileData<-custom_rbind(logFileData,apply_check_type_2("AA-003","provider_id" ,field_name, (current_total_count-
                                                           describeIdentifier(df_provider,field_name)), table_name, g_data_version));



  #Gender Source Value
  field_name="gender_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_message<-reportMissingCount(df_provider,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,missing_message)
  missing_percent_source_value<-extract_numeric_value(missing_message)
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  describeNominalField_basic(df_provider,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #Gender Source Concept id
  field_name="gender_source_concept_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_message<-reportMissingCount(df_provider,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,missing_message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(missing_message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  no_matching_message<-reportNoMatchingCount(df_provider,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));
  describeNominalField_basic(df_provider,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #Gender Concept Id
  field_name = "gender_concept_id"
  label_bins<-c("Male (8507)","Female (8532)","Ambiguous (44814664)","Unknown (44814653)","Other (44814649)","No Information (44814650 )","NULL")
  color_bins <-c("8507"="lightcoral","8532"="steelblue1","44814664"="red","44814653"="grey64","44814649"="grey64","44814650 "="grey64")
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
   null_message<-reportNullFlavors(df_provider,table_name,field_name,44814653,44814649,44814650 ,big_data_flag)

  missing_percent_message<-reportMissingCount(df_provider,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  no_matching_message<-reportNoMatchingCount(df_provider,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));

  ###########DQA CHECKPOINT##############
  df_gender <-generate_df_concepts(con, table_name,"gender.txt")
  order_bins <-c(df_gender$concept_id,NA)
  #unexpected_message<- reportUnexpected(df_provider,table_name,field_name,order_bins,big_data_flag)
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "gender.txt")) 
  
  
  # flog.info( null_message)
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,apply_check_type_2("CA-014", field_name, "gender_source_value",(missing_percent_source_value -
                                                            extract_ni_missing_percent( null_message)), table_name, g_data_version))
  #unexpected_message<- reportUnexpected(df_provider,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  
  # flog.info(unexpected_message)
  #fileContent<-c(fileContent,unexpected_message)
  describeNominalField(df_provider,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));

  if(nrow(subset(df_provider,df_provider$gender_concept_id==8532))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No Female Provider Records","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No female provider records found", table_name, g_data_version));

  }
  if(nrow(subset(df_provider,df_provider$gender_concept_id==8507))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No Male Provider Records","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No male provider records found", table_name, g_data_version));

  }


  # flog.info("here")
  # ORDINAL Fields
  #Year of Birth
  field_name="year_of_birth"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_provider,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  describeOrdinalField(df_provider, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #Specialty source value
  field_name="specialty_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_provider,table_name,field_name,big_data_flag)
  missing_percent_source_value<-extract_numeric_value(message)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  describeOrdinalField(df_provider, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));


  ## specialty_source_concept_id
  field_name="specialty_source_concept_id"
  missing_percent_message<-reportMissingCount(df_provider,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  no_matching_message<-reportNoMatchingCount(df_provider,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));
  describeNominalField_basic(df_provider,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #Specialty concept id
  field_name="specialty_concept_id"
   null_message<-reportNullFlavors(df_provider,table_name,field_name,44814653,44814649,44814650,big_data_flag)
   
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
   #print(missing_percent_source_value)
   #print(extract_ni_missing_percent( null_message))
  logFileData<-custom_rbind(logFileData,apply_check_type_2("CA-014", field_name, "specialty_source_value",(missing_percent_source_value-
                                                            extract_ni_missing_percent( null_message)), table_name, g_data_version))
  missing_percent_message<-reportMissingCount(df_provider,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  ##########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "specialty.csv")) 
  
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version));
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,unexpected_message)
  no_matching_message<-reportNoMatchingCount(df_provider,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));
  df_table_specialty_enhanced<-EnhanceFieldValues(df_provider,field_name,as.data.frame(acceptable_specialty));
  describeNominalField_basic(df_table_specialty_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));
  ## check for providers with specific specialties
  ## for nephrology
  num_neph_records<-retrieve_dataframe_clause(con, g_config, g_config$db$schema,table_name,"count(*)","specialty_concept_id in (38004479,45756813)")[1,1]
  fileContent<-c(fileContent,paste("\nThe number of nephrology providers is: ",num_neph_records,"(",
                                   round((num_neph_records/current_total_count),2),"%)"))
  if(num_neph_records==0)
  {
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "no nephrology providers found", table_name, g_data_version));
  }

  ## for GI
  num_gi_records<-retrieve_dataframe_clause(con, g_config, g_config$db$schema,table_name,"count(*)","specialty_concept_id in (45756810,38004455)")[1,1]
  fileContent<-c(fileContent,paste("\nThe number of GI (Gastroenterology) providers is: ",num_gi_records,"(",
                                   round((num_gi_records/current_total_count),2),"%)"))
  if(num_gi_records==0)
  {
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "no GI providers found", table_name, g_data_version));
  }

  #FOREIGN KEY fields
  #Care site id
  field_name="care_site_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeForeignKeyIdentifiers(df_provider,table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

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
