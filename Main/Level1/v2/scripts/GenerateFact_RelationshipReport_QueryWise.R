flog.info(Sys.time())

generateFactRelationshipReport <- function() {
  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)

  # read a table into an R dataframe
  table_name<-"fact_relationship"
  #df_table <- retrieve_dataframe_OHDSI(con, g_config,table_name)
  # flog.info(nrow(df_table))
   flog.info(Sys.time())

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), 
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))


  test<-1

  big_data_flag<-TRUE # for query wise analysis


  #PRIMARY FIELD
  field_name<-"*"
  df_total_relationship_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  current_total_count<-as.numeric(df_total_relationship_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",table_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))
  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name),NULL, current_total_count)) 
  
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  #NOMINAL Fields
  df_relationship_concept_id <-generate_df_concepts(con, table_name,"relationship_concept_id.txt")
  
  order_bins <-c(df_relationship_concept_id$concept_id,0,NA)

  # this is a nominal field - work on it
  field_name<-"relationship_concept_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "relationship_concept_id.txt")) 
  
  df_table_relationship_enhanced<-EnhanceFieldValues(df_table,field_name,df_relationship_concept_id);
  describeNominalField_basic(df_table_relationship_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));


  #NOMINAL Fields
  df_domain_concept_id_1<-generate_df_concepts(con, table_name,"domain_id.txt")
  order_bins <-c(df_domain_concept_id_1$concept_id,NA)
  # this is a nominal field - work on it
  field_name<-"domain_concept_id_1" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "domain_id.txt")) 
  df_table_domain_concept_id_1_enhanced<-EnhanceFieldValues(df_table,field_name,df_domain_concept_id_1);
  describeNominalField_basic(df_table_domain_concept_id_1_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));


  df_domain_concept_id_2<-generate_df_concepts(con, table_name,"domain_id.txt")
  order_bins <-c(df_domain_concept_id_2$concept_id,NA)
  # this is a nominal field - work on it
  field_name<-"domain_concept_id_2" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),con)) 
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,con,  "domain_id.txt")) 
  df_table_domain_concept_id_2_enhanced<-EnhanceFieldValues(df_table,field_name,df_domain_concept_id_2);
  describeNominalField_basic(df_table_domain_concept_id_2_enhanced,table_name,field_name,big_data_flag);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  # check pair wise correspondence for visits
  df_faulty_visit_count<-retrieve_dataframe_clause(con, g_config, g_config$db$schema,paste(table_name,"a"),"count(*)",
      paste("a.relationship_concept_id=44818783 and ( a.fact_id_1 not in (select b.fact_id_2 from ",
       g_config$db$schema,".", table_name,
      " b where b.relationship_concept_id=44818881) or a.fact_id_2 not in (select b.fact_id_1 from ",
       g_config$db$schema,".", table_name,
      " b where b.relationship_concept_id=44818881))" ,sep="")
    )

  faulty_visit_message<-paste("The number of erraneous (no pairwise correspondence) visit relationship is", df_faulty_visit_count[1][1]);
  fileContent<-c(fileContent,faulty_visit_message);
  #if( df_faulty_visit_count[1][1]>0)
  #{
  ###########DQA CHECKPOINT##############
  #  logFileData<-custom_rbind(logFileData,apply_check_type_1("G3-001", field_name, faulty_visit_message, table_name, g_data_version));
  #}

  # check for BP siblings is very expensive.

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
