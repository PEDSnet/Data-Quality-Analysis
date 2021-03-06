library(DBI)
library(yaml)
library(dplyr)
library(tictoc)

generatePersonReport <- function() {
  # read a database
  table_name<-"person"
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
  field_name<-"person_id"
  df_total_person_id<-retrieve_dataframe_count(data_tbl,field_name)
  current_total_count<-as.numeric(df_total_person_id[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)

  field_name<-"person_source_value"
  df_total_person_source_value<-retrieve_dataframe_count(data_tbl,field_name)
  fileContent<-c(fileContent,paste("The total number of",field_name,"is: ",
                                   formatC(df_total_person_source_value, format="d", big.mark=','),"\n"))
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InconPK(), c(table_name),
                                                   c("person_id",field_name),data_tbl)) 
    
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  
    null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650)

  #NOMINAL Fields

  #Gender Source Value
  field_name="gender_source_value"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)

  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))

  describeNominalField(df_table,table_name,field_name)

  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #Gender Source Concept id
  field_name="gender_source_concept_id"

  df_table<-retrieve_dataframe_group(data_tbl, field_name)

  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))

  describeNominalField(df_table,table_name,field_name)

  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name), data_tbl)) 
  
  #Gender Concept Id
  
  field_name = "gender_concept_id"

  df_table<-retrieve_dataframe_group(data_tbl, field_name)

  label_bins<-c("Male (8507)","Female (8532)","Ambiguous (44814664)","Unknown (44814653)","Other (44814649)","No Information (44814650 )","NULL")
  color_bins <-c("8507"="lightcoral","8532"="steelblue1","44814664"="red","44814653"="grey64","44814649"="grey64","44814650 "="grey64")
 
  
  ###########DQA CHECKPOINT############## For gender only 
  df_gender <-generate_df_concepts(table_name,"gender_dplyr.txt", concept_tbl)
  order_bins <-c(df_gender$concept_id,NA)
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                    ,"gender_dplyr.txt", concept_tbl, data_tbl)) 
  
  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name), data_tbl)) 
  describeNominalField(df_table,table_name,field_name)

  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));
  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name), 
                                                   list(
                                                   list(8532, "female"),
                                                   list(8507, "male")
                                                   ), data_tbl
                                                   )) 

  #Race Source Value
  field_name="race_source_value"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  
  #race source Concept id
  field_name="race_source_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name), data_tbl)) 
  
  #Race Concept Id
  
  field_name="race_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"race_dplyr.txt", concept_tbl, data_tbl)) 
  df_race <- generate_df_concepts( table_name,"race_dplyr.txt", concept_tbl)

  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name), data_tbl)) 
  #update values of the field before plotting
  df_table_race_enhanced<-EnhanceFieldValues(df_table,field_name,df_race);
  describeNominalField(df_table_race_enhanced,table_name,field_name);
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));

  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name), 
                                              list(                                               
                                                    list(8527, "white"),
                                                   list(8516, "black")), data_tbl
                                                   )) 

  #Ethnicity Source Value
  field_name="ethnicity_source_value"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #Ethnicity source Concept id
  field_name="ethnicity_source_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  
  #Ethnicity Concept Id
  field_name="ethnicity_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)

  ###########DQA CHECKPOINT############## For ethinicity only 
  df_ethnicity<-generate_df_concepts( table_name,"ethnicity_dplyr.txt", concept_tbl)
  order_bins <-c(df_ethnicity$concept_id,NA)
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"ethnicity_dplyr.txt", concept_tbl, data_tbl)) 
  

  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));
  
  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name), 
                                                   list(
                                                   list(38003563, "hispanic"),
                                                   list(38003564, "non-hispanic")
                                                   ), data_tbl
                                                   )) 
  ### language field
  #language Source Value
  field_name="language_source_value"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #language source Concept id
  field_name="language_source_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  describeNominalField(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  

  #language Concept Id
  ## reading specific subset of the concept table to retrieve language concepts
  field_name="language_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"language.csv", concept_tbl, data_tbl)) 

  df_lang <-generate_list_concepts(table_name,"language.csv")
  ###########DQA CHECKPOINT##############
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #update values of the field before plotting
  df_table_language_enhanced<-EnhanceFieldValues(df_table,field_name,df_lang);
  describeNominalField(df_table_language_enhanced,table_name,field_name);
  fileContent<-c(fileContent, null_message,paste_image_name(table_name,field_name));
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  
  # ORDINAL Fields
  #Year of Birth
  field_name="year_of_birth"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))

  ###########DQA CHECKPOINT##############
  describeOrdinalField(df_table, table_name, field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  #Month of Birth
  field_name="month_of_birth"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name), data_tbl)) 
  describeOrdinalField(df_table, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  
  #######DQA CHECKPOINT################
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidValue(), c(table_name),c(field_name),
                                                   "month_of_birth.csv", data_tbl)) 

  #Day of Birth
  field_name="day_of_birth"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  describeOrdinalField(df_table,table_name, field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  ###########DQA checkpoint#####################
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidValue(), c(table_name),c(field_name)
                                                   ,  "day_of_birth.csv", data_tbl)) 

  #birth_date 
  field_name<-"birth_date"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)

  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeDateField(df_table, table_name,field_name, datetime = 0)

  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), 
                                                   c(field_name), data_tbl)) 

  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_date",sep="")));
  
  #birth_datetime --
  field_name<-"birth_datetime"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeDateField(df_table, table_name,field_name, datetime = 1)
  
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), 
                                                   c(field_name), data_tbl)) 
  
  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_datetime",sep="")));

  #pn_gestational_field
  field_name<-"pn_gestational_age"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  unit<-"weeks"
  fileContent <-c(fileContent,paste("## Histogram for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeRatioField(df_table, table_name, field_name, unit)
  if (missing_percent<100)
  {
  ###########DQA CHECKPOINT############## gestational age cannot be above 45
    logFileData<-custom_rbind(logFileData,applyCheck(NumOutlier(), c(table_name),
                                                     c(field_name),data_tbl)) 
  }
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  #FOREIGN KEY fields
  #LocationID
  field_name<-"location_id"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeForeignKeyIdentifiers(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

  #provider_id
  field_name="provider_id"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),data_tbl)) 
  message<-describeForeignKeyIdentifiers(df_table,table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

  #Care site id
  field_name="care_site_id"
  df_table<-retrieve_dataframe_group(data_tbl, field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
}
