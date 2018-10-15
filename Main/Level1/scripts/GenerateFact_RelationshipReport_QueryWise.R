generateFactRelationshipReport <- function() {
  
  # read a table 
  table_name<-"fact_relationship"
  data_tbl <- cdm_tbl(req_env$db_src, table_name)
  concept_tbl <- vocab_tbl(req_env$db_src, "concept")

  flog.info(Sys.time())

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),
                       "./reports/",table_name,"_Report_Automatic.md",sep=""))
  fileContent <-get_report_header(table_name, g_config)

  logFileData<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), 
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))
  #PRIMARY FIELD
  field_name<-colnames(data_tbl)
  df_total_relationship_count<-retrieve_dataframe_count(data_tbl,field_name)
  current_total_count<-as.numeric(df_total_relationship_count[1][1])
  fileContent<-c(fileContent,paste("The total number of",table_name,"is:", formatC(current_total_count, format="d", big.mark=','),"\n"))

  ###########DQA CHECKPOINT############## difference from previous cycle
  logFileData<-custom_rbind(logFileData,applyCheck(UnexDiff(), c(table_name),NULL, current_total_count)) 
  ## write current total count to total counts 
  write_total_counts(table_name, current_total_count)
  
  #NOMINAL Fields
  df_relationship_concept_id <-generate_df_concepts(table_name,
                                                    "relationship_concept_id_dplyr.txt", concept_tbl)

  # this is a nominal field - work on it
  field_name<-"relationship_concept_id" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"relationship_concept_id_dplyr.txt", concept_tbl, data_tbl)) 
  
  df_table_relationship_enhanced<-EnhanceFieldValues(df_table,field_name,df_relationship_concept_id);
  describeNominalField(df_table_relationship_enhanced,table_name,field_name);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));


  #NOMINAL Fields
  df_domain_concept_id_1<-generate_df_concepts(table_name,"domain_id_dplyr.txt", concept_tbl)
  # this is a nominal field - work on it
  field_name<-"domain_concept_id_1" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl))
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"domain_id_dplyr.txt", concept_tbl, data_tbl)) 
  df_table_domain_concept_id_1_enhanced<-EnhanceFieldValues(df_table,field_name,df_domain_concept_id_1);
  describeNominalField(df_table_domain_concept_id_1_enhanced,table_name,field_name);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  df_domain_concept_id_2<-generate_df_concepts(table_name,"domain_id_dplyr.txt", concept_tbl)
  # this is a nominal field - work on it
  field_name<-"domain_concept_id_2" #
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"\n"))
  df_table<-retrieve_dataframe_group(data_tbl,field_name)
  
  ###########DQA CHECKPOINT -- no matching concept ##############
  logFileData<-custom_rbind(logFileData,applyCheck(MissConID(), c(table_name),c(field_name),data_tbl)) 
  
  ###########DQA CHECKPOINT##############
  logFileData<-custom_rbind(logFileData,applyCheck(InvalidConID(), c(table_name),c(field_name)
                                                   ,"domain_id_dplyr.txt", concept_tbl ,data_tbl)) 
  df_table_domain_concept_id_2_enhanced<-EnhanceFieldValues(df_table,field_name,df_domain_concept_id_2);
  describeNominalField(df_table_domain_concept_id_2_enhanced,table_name,field_name);
  fileContent<-c(fileContent,paste_image_name(table_name,field_name));

  # check pair wise correspondence for visits
  data_tbl_a = data_tbl %>% 
    filter(relationship_concept_id == 44818783) %>% collect()
  data_tbl_b = data_tbl %>%
    filter(relationship_concept_id == 44818881) %>%
    select(fact_id_1, fact_id_2) %>% collect() 
  df_faulty_visit_count = data_tbl_a %>%
    filter(!(fact_id_1 %in% data_tbl_b$fact_id_2) | !(fact_id_2 %in% data_tbl_b$fact_id_1)) %>%
    summarise(counts = n()) %>%
    as.data.frame()
  
  remove(data_tbl_a)
  remove(data_tbl_b)

  faulty_visit_message<-paste("The number of erraneous (no pairwise correspondence) visit relationship is", 
                              df_faulty_visit_count[1][1]);
  fileContent<-c(fileContent,faulty_visit_message);

  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  colnames(logFileData)<-c("g_data_version", "table","field", "issue_code", "issue_description","alias","finding", "prevalence")
  logFileData<-subset(logFileData,!is.na(issue_code))
  write.csv(logFileData, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./issues/",table_name,"_issue.csv",sep="")
            ,row.names=FALSE)
}
