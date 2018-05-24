generateObservationReport <- function() {
  flog.info(Sys.time())

  big_data_flag<-TRUE

  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)

  table_name<-"observation"
  #df_table <- retrieve_dataframe_OHDSI(con, g_config,table_name)
  # flog.info(nrow(df_table))

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  ## writing to the issue log file
  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0),
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))


  #PRIMARY FIELD and related measures
  field_name<-"observation_id"
  df_total_observation_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  current_total_count<-as.numeric(df_total_observation_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name), NULL,current_total_count)) 
  
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  df_total_patient_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct person_id")
  fileContent<-c(fileContent,paste("The observation to patient ratio is ",round(df_total_observation_count[1][1]/df_total_patient_count[1][1],2),"\n"))
  df_total_visit_count<-retrieve_dataframe_count(con, g_config,table_name,"distinct visit_occurrence_id")
  fileContent<-c(fileContent,paste("The observation to visit ratio is ",round(df_total_observation_count[1][1]/df_total_visit_count[1][1],2),"\n"))


  # visit concept id
  df_visit <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept","concept_id,concept_name"
    ,"vocabulary_id ='Visit' or (vocabulary_id='PCORNet' and concept_class_id='Encounter Type')
      or (vocabulary_id = 'PCORNet' and concept_class_id='Undefined')")

  #observation / person id by visit types
  fileContent <-c(fileContent,paste("## Barplot for Observation:Patient ratio by visit type\n"))
  df_observation_patient_ratio<-retrieve_dataframe_ratio_group_join(con, g_config,table_name, "visit_occurrence",
                    "round(count(distinct observation_id)/count(distinct observation.person_id),2)",
                "visit_concept_id","visit_occurrence_id")

  for(i in 1:nrow(df_observation_patient_ratio))
    {
        #df_visit[df_visit$concept_id==9201,2]
         label<-df_visit[df_visit$concept_id==df_observation_patient_ratio[i,1],2]
         df_observation_patient_ratio[i,1]<-paste(df_observation_patient_ratio[i,1],"(",label,")",sep="")
        #df_visit_patient_ratio[i,1]
        }
  describeOrdinalField(df_observation_patient_ratio,table_name,"Observation:Patient ratio by visit type",big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,"Observation:Patient ratio by visit type"));




  field_name="observation_source_value"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650,big_data_flag)

  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  missing_percent_source_value<-extract_numeric_value(message)
  #describeOrdinalField(df_table, table_name, field_name,big_data_flag)
  #fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  #df_table<-retrieve_dataframe_top_5(con, g_config, table_name, field_name)
  #fileContent<-c(fileContent,paste("The most frequent values for",field_name,"are:"))
  #for(row_count in 1:5)
  #{
  #  fileContent<-c(fileContent,paste(df_table[row_count,1],"(count=",df_table[row_count,2],")"))
  #}

  field_name="observation_source_concept_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  ###########DQA CHECKPOINT -- missing information##############
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  fileContent<-c(fileContent,message)
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  describeNominalField_basic(df_table, table_name, field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));


  # this is a nominal field - work on it
  field_name<-"observation_concept_id" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  order_bins <-c("4145666","44813951","4137274","4005823","4219336","4275495","3040464","0",NA)
  label_bins<-c("Admitting source (4145666)","Discharge disposition (44813951)","Discharge status (4137274)"
                ,"Tobacco (4005823)","Tobacco Type (4219336)","Smoking (4275495)","DRG (3040464)","Delivery Mode (40760190)","Others (0)","NULL")
  color_bins <-c("4145666"="lightcoral",
                 "44813951"="steelblue1",
                 "4137274"="red"
                 ,"4005823"="grey64"
                 ,"4219336"="grey64",
                 "4275495"="grey64",
                 "3040464"="grey64",
                 "40760190"="yellow", 
                  "0"="grey64" )
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "observation_concept_id.csv")) 
  #fileContent<-c(fileContent,unexpected_message)
  describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));



  ###########DQA CHECKPOINT############## missing expected concepts 
  
  logFileData<-custom_rbind(logFileData,applyCheck(MissFact(), c(table_name),c(field_name),con, 
                                                   list(
                                                     list(44813951,  " discharge status ") ,
                                                     list(3040464,  "DRG") ,
                                                    list(40760190,  "Delivery Mode") ,
                                                    list(4005823, 4219336, 4275495,  "Tobacco")
                                                    ))
)
  
  
  #NOMINAL Fields

  field_name<-"person_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

  # flog.info(fileContent)

  field_name<-"provider_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeForeignKeyIdentifiers(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

  flog.info(Sys.time())
  
  field_name<-"visit_occurrence_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  
  count_novisit<-retrieve_dataframe_clause(con,g_config,g_config$db$schema,table_name,"count(*)"
                                           ,"visit_occurrence_id is null")
  missing_visit_percent<-round(count_novisit*100/df_total_observation_count,2)
  
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,apply_check_type_1("BA-001", field_name, missing_visit_percent, table_name, g_data_version));
  


  # ORDINAL Fields
   flog.info(Sys.time())
  field_name<-"observation_date" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  #fileContent<-c(fileContent,reportMissingCount(df_table,table_name,field_name,big_data_flag))
  message<-describeDateField(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));
  ###########DQA CHECKPOINT##############
  ### DQA checkpoint - future date
  logFileData<-custom_rbind(logFileData,applyCheck(ImplFutureDate(), c(table_name), c(field_name),con)) 
  
  
  field_name<-"observation_datetime" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeTimeField(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_datetime",sep="")));

  # not plotting the value_as_string column as it's a free text field
  field_name<-"value_as_string"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  
  # not plotting the value_as_string column as it's a free text field
  field_name<-"value_as_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT##############
  order_bins <-generate_df_concepts(con, table_name,"value_as_concept_id.txt")$concept_id
  unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "value_as_concept_id.txt")) 
  
  field_name<-"unit_source_value" # 3 minutes
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- missing information##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  missing_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,missing_message)
  missing_percent_source_value<-extract_numeric_value(missing_message)
  
  if(grepl("100",missing_message)==FALSE) # if 100% missing
  {
    describeNominalField_basic(df_table, table_name,field_name,big_data_flag)
    fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  }


  df_unit <-generate_df_concepts(con, table_name,"unit_concept_id.txt")
  order_bins <-c(df_unit$concept_id,0,44814650,NA)
  field_name="unit_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650 ,big_data_flag)
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "unit_source_value"),con
)) 
#unexpected_message<- reportUnexpected(df_table,table_name,field_name,order_bins,big_data_flag)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "unit_concept_id.txt")) 
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  #fileContent<-c(fileContent,unexpected_message)
  df_table_unit_enhanced<-EnhanceFieldValues(df_table,field_name,df_unit);
  if( is.na(df_table_unit_enhanced[1,1]) && nrow (df_table_unit_enhanced==1))
  {
     flog.info("unit concept id data not available")
  } else
  {
    describeNominalField_basic(df_table_unit_enhanced,table_name,field_name,big_data_flag);
    fileContent<-c(fileContent,paste_image_name(table_name,field_name));
  }

  #print (fileContent)
  field_name = "observation_type_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  order_bins <-c("38000280","44814721",NA)
  label_bins<-c("observation recorded from EMR (38000280)","Patient reported (44814721)","NULL")
  color_bins <-c("38000280"="lightcoral","44814721"="steelblue1")
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "observation_type_concept_id.csv")) 
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  describeNominalField(df_table,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  field_name<-"qualifier_source_value" #
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent_source_value<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  field_name<-"qualifier_concept_id"
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  missing_percent_message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  missing_percent<- extract_numeric_value(missing_percent_message)
  fileContent<-c(fileContent,missing_percent_message)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  null_message<-reportNullFlavors(df_table,table_name,field_name,44814653,44814649,44814650 ,big_data_flag)
  
  
  ###########DQA CHECKPOINT############## source value Nulls and NI concepts should match
  logFileData<-custom_rbind(logFileData,applyCheck(InconSource(), c(table_name),c(field_name, "qualifier_source_value"),con
  )) 
  message<-describeOrdinalField_large(df_table, table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

  ##########DQA CHECKPOINT################
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "qualifier_concept_id.txt")) 

  #ordinal field

  field_name<-"value_as_number"
    df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  message<-reportMissingCount(df_table,table_name,field_name,big_data_flag)
  fileContent<-c(fileContent,message)
  ###########DQA CHECKPOINT -- missing information##############
  missing_percent<-extract_numeric_value(message)
  logFileData<-custom_rbind(logFileData,applyCheck(MissData(), c(table_name),c(field_name),con)) 
  message<-describeRatioField(df_table, table_name,field_name,"",big_data_flag)
  fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

    #print (fileContent)



   flog.info(Sys.time())

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)

  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  #close the connection
  close_database_connection_OHDSI(con, g_config)
}
