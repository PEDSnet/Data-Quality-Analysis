generateProviderReport <- function() {

  # read a table
  table_name<-"provider"
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
  field_name<-"provider_id"
  current_total_count<-as.numeric(describeIdentifier(data_tbl,field_name))
  fileContent<-c(fileContent,paste("The total number of unique values for ",field_name,"is: ",
                                   current_total_count ,"\n"))
  
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  field_name<-"provider_source_value"
  fileContent<-c(fileContent,paste("The total number of",field_name,"is: ", 
                                   describeIdentifier(data_tbl,field_name),"\n"))
  
  ###########DQA CHECKPOINT##############  total id different from source value
  logFileData<-custom_rbind(logFileData,applyCheck(InconPK(), c(table_name), 
                                                   c("provider_id",field_name),data_tbl)) 

  #Gender Source Value
  field_name="gender_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_message<-reportMissingCount(data_tbl,table_name,field_name, group_ret = 0);
  fileContent<-c(fileContent,missing_message)
  missing_percent_source_value<-extract_numeric_value(missing_message)
  
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  describeNominalField(data_tbl,table_name,field_name, group_ret = 0)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #Gender Source Concept id
  field_name="gender_source_concept_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_message<-reportMissingCount(data_tbl,table_name,field_name, group_ret = 0);
  fileContent<-c(fileContent,missing_message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(missing_message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  describeNominalField(data_tbl,table_name,field_name, group_ret = 0)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #Gender Concept Id
  field_name = "gender_concept_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  null_message<-reportNullFlavors(data_tbl,table_name,field_name,
                                  44814653,44814649,44814650)

  missing_percent_message<-reportMissingCount(data_tbl,table_name,field_name, group_ret = 0)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)

  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 

  ###########DQA CHECKPOINT##############
  df_gender <-generate_df_concepts(table_name,"gender_dplyr.txt", concept_tbl)
  order_bins <-c(df_gender$concept_id,NA)
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"gender_dplyr.txt", concept_tbl, data_tbl)) 

  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), 
                                                   c(table_name),c(field_name, "gender_source_value"),
                                                   data_tbl)) 
  
  ###########DQA CHECKPOINT##############

  describeNominalField(data_tbl,table_name,field_name, group_ret = 0)
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));

  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name), 
                                                   list(
                                                    list(8507, "male"), 
                                                    list(8532, "female")),
                                                   data_tbl)) 

  # ORDINAL Fields
  #Year of Birth
  field_name="year_of_birth"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(data_tbl,table_name,field_name)
  fileContent<-c(fileContent,message)
  
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  describeOrdinalField(data_tbl, table_name, field_name, group_ret = 0)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #Specialty source value
  field_name="specialty_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(data_tbl,table_name,field_name, group_ret = 0)
  missing_percent_source_value<-extract_numeric_value(message)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  describeOrdinalField(data_tbl, table_name,field_name, group_ret = 0)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  ## specialty_source_concept_id
  field_name="specialty_source_concept_id"
  missing_percent_message<-reportMissingCount(data_tbl,table_name,field_name, group_ret = 0)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  describeNominalField(data_tbl,table_name,field_name, group_ret = 0)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #Specialty concept id
  field_name="specialty_concept_id"
  null_message<-reportNullFlavors(data_tbl,table_name,field_name,44814653,44814649,44814650)
   
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),
                                                    c(field_name, "specialty_source_value"),data_tbl)) 
  missing_percent_message<-reportMissingCount(data_tbl,table_name,field_name, group_ret = 0)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  
  ##########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"specialty_dplyr.txt", concept_tbl, data_tbl)) 
  
  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  
  acceptable_specialty<- read.csv(paste(getwd(), "/Data/ValueSets/provider/specialty.csv", 
                                        sep= ""))$concept_id ## read from specialty list
  df_table_specialty_enhanced<-EnhanceFieldValues(data_tbl,field_name,as.data.frame(acceptable_specialty));
  describeNominalField(df_table_specialty_enhanced,table_name,field_name, group_ret = 0);
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));
  
  ## check for providers with specific specialties
  ## for nephrology, GI
  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name), 
                                                   list(
                                                      list(38004479,45756813, "nephrology"), 
                                                      list(45756810,38004455, "GI(Gastroenterology)")),
                                                   data_tbl)) 
  
  #FOREIGN KEY fields
  #Care site id
  field_name="care_site_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeForeignKeyIdentifiers(data_tbl,table_name, field_name, group_ret = 0)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                 paste_image_name_sorted(table_name,field_name),message);

  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
}
