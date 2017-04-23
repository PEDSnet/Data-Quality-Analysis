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
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), issue_description=character(0)
                          , finding=character(0), prevalence=character(0))



  #PRIMARY FIELD(s)
  field_name<-"care_site_id"
  current_total_count<-as.numeric(describeIdentifier(df_care_site,field_name))
  fileContent<-c(fileContent,paste("The total number of unique values for ",field_name,"is: ",current_total_count ,"\n"))
  prev_total_count<-get_previous_cycle_total_count( g_config$reporting$site, table_name)
  percentage_diff<-get_percentage_diff(prev_total_count, current_total_count)
  fileContent<-c(fileContent, get_percentage_diff_message(percentage_diff))
  ### DQA CHECKPOINT ####################
  check_result<-apply_check_type_0("CA-005", percentage_diff, table_name, g_data_version)
  logFileData<-custom_rbind(logFileData,check_result);


  #add care site identifier
  field_name<-"care_site_name"
  fileContent<-c(fileContent,paste("The total number of unique values for ",field_name,"is: ", describeIdentifier(df_care_site,field_name),"\n"))
  #get_previous_cycle_total_count()

  field_name<-"care_site_source_value"
  fileContent<-c(fileContent,paste("The total number of unique values for ",field_name,"is: ", describeIdentifier(df_care_site,field_name),"\n"))
  ### DQA CHECKPOINT ####################
  logFileData<-rbind(logFileData,apply_check_type_2("AA-003", "care_site_id",field_name, (current_total_count
                                                                                          - describeIdentifier(df_care_site,field_name)), table_name, g_data_version));
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
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent_source_value, table_name, g_data_version));


  #place of service concept id
                                       
  order_bins <-c(8782, 8761, 8756, 8940, 8971, 8717, 8716, 8870, 8844, 8892, 44814653, 44814649,44814650,0,NA)

  field_name="place_of_service_concept_id"
  null_message<-reportNullFlavors(df_care_site,table_name,field_name,44814653,44814649,44814650,big_data_flag)
  # flog.info( null_message)
  missing_percent_message<-reportMissingCount(df_care_site,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  no_matching_concept_message<-reportNoMatchingCount(df_care_site,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_concept_message)
  no_matching_concept_number<-extract_numeric_value(no_matching_concept_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_concept_message), table_name, g_data_version));

  unexpected_message<- reportUnexpected(df_care_site,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version));
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  fileContent<-c(fileContent,unexpected_message)
  df_table_place_of_service_enhanced<-EnhanceFieldValues(df_care_site,field_name,df_place_of_service);
  describeNominalField_basic(df_table_place_of_service_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,apply_check_type_2("CA-014", field_name, "place_of_service_source_value",
                                                           (missing_percent_source_value -
                                                            extract_ni_missing_percent( null_message)), table_name, g_data_version))



  # specialty source value
  field_name="specialty_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_care_site,table_name,field_name,big_data_flag)
  missing_percent_source_value<-extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_source_value)
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent_source_value, table_name, g_data_version));
  describeNominalField_basic(df_care_site, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #specialty concept id
  field_name="specialty_concept_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_care_site,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  no_matching_concept_message<-reportNoMatchingCount(df_care_site,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_concept_message)
  no_matching_concept_number<-extract_numeric_value(no_matching_concept_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_concept_message), table_name, g_data_version));

  describeNominalField_basic(df_care_site, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,apply_check_type_2("CA-014", field_name, "specialty_source_value",
                                                           (missing_percent_source_value -
                                                            extract_ni_missing_percent( null_message)), table_name, g_data_version))
  ##########DQA CHECKPOINT##############
  df_specialty <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name","domain_id = 'Provider Specialty' and concept_class_id = 'Specialty' and standard_concept = 'S'")
  order_bins <-c(df_specialty$concept_id,44814650,44814653, 44814649, 0,NA)
  unexpected_message<- reportUnexpected(df_care_site,table_name,field_name,order_bins,big_data_flag)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, unexpected_message, table_name, g_data_version));


  ## check for caresites with specific specialties
  ## for nephrology
  num_neph_records<-retrieve_dataframe_clause(con, g_config, g_config$db$schema,table_name,"count(*)","specialty_concept_id in (38004479,45756813)")[1,1]
  fileContent<-c(fileContent,paste("\nThe number of nephrology caresites is: ",num_neph_records,"(",
                                   round((num_neph_records/current_total_count),2),"%)"))
  if(num_neph_records==0)
  {
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "no nephrology caresites found", table_name, g_data_version));
  }

  ## for GI
  num_gi_records<-retrieve_dataframe_clause(con, g_config, g_config$db$schema,table_name,"count(*)","specialty_concept_id in (45756810,38004455)")[1,1]
  fileContent<-c(fileContent,paste("\nThe number of GI (Gastroenterology) caresites is: ",num_gi_records,"(",
                                   round((num_gi_records/current_total_count),2),"%)"))
  if(num_gi_records==0)
  {
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "no GI caresites found", table_name, g_data_version));
  }


  #FOREIGN KEY fields
  #location id
  field_name="location_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_care_site,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  # flog.info(missing_percent)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));

  message<-describeForeignKeyIdentifiers(df_care_site,table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);


  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","finding", "prevalence")
  #logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)

  #close the connection
  close_database_connection_OHDSI(con, g_config)
}
