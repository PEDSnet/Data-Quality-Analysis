generateMeasurementOrganismReport <- function() {
  flog.info(Sys.time())

  #Read in table
  table_name<-"measurement_organism"
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
  field_name<-"meas_organism_id"
  df_total_measurement_count<-retrieve_dataframe_count(data_tbl,field_name)
  current_total_count<-as.numeric(df_total_measurement_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:",
                                   formatC(current_total_count, format="d", big.mark=','),"\n"))

  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  df_total_patient_count<-retrieve_dataframe_count(data_tbl,"person_id", distinction = T)
  fileContent<-c(fileContent,paste("The measurement-organism to patient ratio is ",
                                   round(df_total_measurement_count[1][1]/df_total_patient_count[1][1],2),"\n"))
  df_total_visit_count<-retrieve_dataframe_count(data_tbl,"visit_occurrence_id", distinction = T)
  fileContent<-c(fileContent,paste("The measurement-organism to visit ratio is ",
                                   round(df_total_measurement_count[1][1]/df_total_visit_count[1][1],2),"\n"))

  #NOMINAL Fields
  field_name<-"person_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                 paste_image_name_sorted(table_name,field_name),message);

  field_name<-"measurement_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(data_tbl,field_name)

  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name)

  fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                 paste_image_name_sorted(table_name,field_name),message);

  field_name<-"visit_occurrence_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))

  ###########DQA CHECKPOINT -- missing information##############
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), table_name,field_name,data_tbl)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name)

  fileContent<-c(fileContent,paste_image_name(table_name,field_name),
                 paste_image_name_sorted(table_name,field_name),message);

  # ORDINAL Fields
  flog.info(Sys.time())
  field_name<-"positivity_datetime" 
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))

  ###########DQA CHECKPOINT -- missing information##############
  message<-reportMissingCount(df_table,table_name,field_name, group_ret = 1)
  fileContent<-c(fileContent,message)

  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), 
                                                   c(table_name),c(field_name),data_tbl)) 

  message<-describeDateField(df_table, table_name,field_name, datetime = 1)
 
  if(missing_percent<100)
  {
    ### DQA checkpoint - future date
    logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name),
                                                     c(field_name),data_tbl)) 
  }
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  #organism_concept_id
  df_unit <-generate_df_concepts(table_name,"organism_concept_id_dplyr.txt", concept_tbl)
  field_name="organism_concept_id"
  df_table<-retrieve_dataframe_group(data_tbl,field_name)

  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"organism_concept_id_dplyr.txt", concept_tbl, data_tbl)) 
  
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))

  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  df_table_unit_enhanced<-EnhanceFieldValues(df_table,field_name,df_unit);
  describeNominalField(df_table_unit_enhanced,table_name,field_name);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));
 
  field_name<-"organism_source_value" # 3 minutes
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),message);

  # other fields not plotted : operator concept id, value as concept id, value source value, measurement source concept id
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
}
