generatePersonReport <- function() {
  #setting up big data flag
  big_data_flag<-TRUE
  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)

  # read a database table into an R dataframe
  table_name<-"person"
  #df_table <- retrieve_dataframe_OHDSI(con, g_config,table_name)

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), 
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))

  #PRIMARY FIELD
  field_name<-"person_id"
  df_total_person_id<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  current_total_count<-as.numeric(df_total_person_id[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), table_name,current_total_count)) 
  


  field_name<-"person_source_value"
  df_total_person_source_value<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  fileContent<-c(fileContent,paste("The total number of",field_name,"is: ", formatC(df_total_person_source_value[1,1], format="d", big.mark=','),"\n"))
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_2("AA-003", "person_id",field_name,
                                                           (current_total_count-df_total_person_source_value[1][1]), table_name, g_data_version));
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));

    null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650,big_data_flag)


  #NOMINAL Fields

  #Gender Source Value
  field_name="gender_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  describeNominalField_basic(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #Gender Source Concept id
  field_name="gender_source_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  describeNominalField_basic(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));



  #Gender Concept Id
  
  
  field_name = "gender_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  label_bins<-c("Male (8507)","Female (8532)","Ambiguous (44814664)","Unknown (44814653)","Other (44814649)","No Information (44814650 )","NULL")
  color_bins <-c("8507"="lightcoral","8532"="steelblue1","44814664"="red","44814653"="grey64","44814649"="grey64","44814650 "="grey64")
 
  
  ###########DQA CHECKPOINT############## For gender only 
  file_txt <- "Data/PEDSnet_gender.txt"
  gender_clause <- readChar(file_txt, file.info(file_txt)$size)
  gender_clause_trunc <- gsub("\n", '', noquote(gender_clause), fixed = T) # takes off extra characters
  df_gender <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name", gender_clause_trunc)
  order_bins <-c(df_gender$concept_id,NA)
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, paste(unexpected_message), table_name, g_data_version));
  

  ############DQA CHECKPOINT############## source value Nulls and NI concepts should match
  #logFileData<-custom_rbind(logFileData,apply_check_type_2("G1-002", field_name, missing_percent_source_value,
  #                                                            extract_ni_missing_percent( null_message), table_name, g_data_version))
  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));
  describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));

  # flog.info("here")
  if(nrow(subset(df_table,df_table$gender_concept_id==8532))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No Female Patients Records","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No female patient records found", table_name, g_data_version));

  }
  if(nrow(subset(df_table,df_table$gender_concept_id==8507))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No Male Patients Records","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No male patient records found", table_name, g_data_version));

  }




  #Race Source Value
  field_name="race_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  describeNominalField_basic(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #race source Concept id
  field_name="race_source_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  describeNominalField_basic(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));


  #Race Concept Id
  
  field_name="race_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  
  ###########DQA CHECKPOINT##############
  acceptable_race<- read.csv(paste(getwd(), "/Data/PEDSnet_race.csv", sep= ""))$concept_id ## read from gender list
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,acceptable_race,big_data_flag)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, paste(unexpected_message), table_name, g_data_version));
  df_race <- as.data.frame(acceptable_race)
  
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  #logFileData<-custom_rbind(logFileData,apply_check_type_2("G1-002", field_name, missing_percent_source_value,
  #                                                          extract_ni_missing_percent( null_message), table_name, g_data_version))
  # flog.info( null_message)
  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));
  #update values of the field before plotting
  df_table_race_enhanced<-EnhanceFieldValues(df_table,field_name,df_race);
  describeNominalField_basic(df_table_race_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));

  if(nrow(subset(df_table,df_table$race_concept_id==8527))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No White Patient Records","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No white Patient records found", table_name, g_data_version));

  }
  if(nrow(subset(df_table,df_table$race_concept_id ==8516))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No Black Patient Records","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No black Patient records found", table_name, g_data_version));

  }



  #Ethnicity Source Value
  field_name="ethnicity_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  describeNominalField_basic(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));


  #Ethnicity source Concept id
  field_name="ethnicity_source_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));


  #Ethnicity Concept Id
  field_name="ethnicity_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)


  ###########DQA CHECKPOINT############## For ethinicity only 
  file.txt <- "Data/PEDSnet_ethnicity.txt"
  ethnicity_clause <- readChar(file.txt, file.info(file.txt)$size) # call in where statement from txt file
  ethnicity_clause_trunc <- gsub("\n", '', noquote(ethnicity_clause), fixed = T) #remove extra characters
  df_ethnicity<-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name",ethnicity_clause_trunc)
  order_bins <-c(df_ethnicity$concept_id,NA)
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, paste(unexpected_message), table_name, g_data_version));
  

  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  #logFileData<-custom_rbind(logFileData,apply_check_type_2("G1-002", field_name, missing_percent_source_value,
  #                                                          extract_ni_missing_percent( null_message), table_name, g_data_version))
  # flog.info( null_message)
  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));
  # flog.info(paste("Unexpected Values:",unexpected_message))
  describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins,big_data_flag)
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));
  

  if(nrow(subset(df_table,df_table$ethnicity_concept_id == 38003563))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No Hispanic Patient Records","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No Hispanic patient records found", table_name, g_data_version));

  }
  if(nrow(subset(df_table,df_table$ethnicity_concept_id == 38003564))==0)
  {
    fileContent<-c(fileContent,"DQA WARNING: No non-Hispanic Patient Records","\n");
    logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-003", field_name, "No non-Hispanic patient records found", table_name, g_data_version));

  }


  ### language field
  #language Source Value
  field_name="language_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  describeNominalField_basic(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));


  #language source Concept id
  field_name="language_source_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  describeNominalField_basic(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));


  #language Concept Id
  ## reading specific subset of the concept table to retrieve language concepts
  field_name="language_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  ###########DQA CHECKPOINT##############
  acceptable_lang<- read.csv(paste(getwd(), "/Data/PEDSnet_language.csv", sep= ""))$concept_id ## read from gender list
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,acceptable_lang,big_data_flag)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-002", field_name, paste(unexpected_message), table_name, g_data_version));
  df_lang <- as.data.frame(acceptable_lang)
  ###########DQA CHECKPOINT##############
  
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  #logFileData<-custom_rbind(logFileData,apply_check_type_2("G1-002", field_name, missing_percent_source_value,
  #                                                          extract_ni_missing_percent( null_message), table_name, g_data_version))
  ###########DQA CHECKPOINT##############
  # flog.info(unexpected_message)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #update values of the field before plotting
  df_table_language_enhanced<-EnhanceFieldValues(df_table,field_name,df_lang);
  describeNominalField_basic(df_table_language_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,no_matching_message)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-002", field_name,extract_numeric_value(no_matching_message ), table_name, g_data_version));


  # ORDINAL Fields
  #Year of Birth
  field_name="year_of_birth"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  ###########DQA CHECKPOINT##############
  describeOrdinalField(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #Month of Birth
  field_name="month_of_birth"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  describeOrdinalField(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  #######DQA CHECKPOINT################
  order_bins <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-001", field_name, unexpected_message, table_name, g_data_version));
  fileContent<-c(fileContent,unexpected_message)

  #Day of Birth
  field_name="day_of_birth"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  describeOrdinalField(df_table,table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  ###########DQA checkpoint#####################
  order_bins <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  logFileData<-custom_rbind(logFileData,apply_check_type_1("AA-001", field_name, unexpected_message, table_name, g_data_version));
  fileContent<-c(fileContent,unexpected_message)


  #Time of Birth --
  field_name<-"time_of_birth"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  #message<-describeTimeField(df_table, table_name,field_name,big_data_flag)
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)
  ###########DQA CHECKPOINT############## ... time of birth cannot be in future
  if(length(message)==3)
  {
    if(grepl("future",message[3]))
    {
    logFileData<-custom_rbind(logFileData,apply_check_type_1("CA-001", field_name, "birthdays cannot occur in the future", table_name, g_data_version));
    }
  }

  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_time",sep="")));

  #RATIO Fields

  #pn_gestational_field
  field_name<-"pn_gestational_age"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  unit<-"weeks"
  fileContent <-c(fileContent,paste("## Histogram for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeRatioField(df_table, table_name, field_name, unit,big_data_flag)
  if (missing_percent<100)
  {
  ###########DQA CHECKPOINT############## gestational age cannot be above 45
  logFileData<-custom_rbind(logFileData,apply_check_type_1("CA-011",field_name,extract_maximum_value(message), table_name, g_data_version));
  }
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  #FOREIGN KEY fields
  #LocationID
  field_name<-"location_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeForeignKeyIdentifiers(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

  #provider_id
  field_name="provider_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_percent, table_name, g_data_version));
  message<-describeForeignKeyIdentifiers(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

  #Care site id
  field_name="care_site_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
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
