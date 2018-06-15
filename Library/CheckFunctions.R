## generate concept_id from clause 
generate_df_concepts<-function(con,table_name, filename)
{
  file_txt<-as.character(paste("Data/ValueSets",table_name,filename, sep="/"))
  #print(file_txt)
  clause <- readChar(file_txt, file.info(file_txt)$size)
  clause_trunc <- gsub("\n", '', noquote(clause), fixed = T) # takes off extra characters
  clause_trunc<-as.character(clause_trunc)

  #check_list_entry<-get_check_entry_table_level(theObject$check_code, table_name)

  #print(clause_trunc)
  #print(class(clause_trunc))
  concept_id_list <-retrieve_dataframe_clause(con, g_config, g_config$db$vocab_schema,"concept" ,"concept_id,concept_name" ,clause_trunc)
return(concept_id_list)
}

generate_list_concepts<-function(table_name, filename)
{
  #print(getwd());
  df_csv<-read.csv(paste(getwd(),"Data/ValueSets", table_name,filename, sep= "/"))
  #print(df_csv$concept_id)
  return(df_csv)##

}

# read the master catalog
get_catalog_entry<-function(check_code_value)
{

  return(
            subset(g_df_check_catalogue,g_df_check_catalogue$check_code==check_code_value)
    )
}

## read specific check list
## read specific check list
get_check_entry_table_level<-function(check_code, table_name)
{
  ## read specific checklist file for the given check code.
  file_list<-list.files(g_catalog_folder_path)
  check_filename<-file_list[grep(check_code,file_list)]
  # flog.info()
  # flog.info(check_filename)
  df_check_list<-read.csv(paste(g_catalog_folder_path,check_filename,sep=""), header = TRUE, sep = ",", quote = "\"",
                          dec = ".", fill = TRUE, comment.char = "")

  return(
    subset(df_check_list, tolower(df_check_list$PEDSnet_Table)==tolower(table_name)))
}

get_check_entry_one_variable<-function(check_code, table_name,field)
{
  ## read specific checklist file for the given check code.
  file_list<-list.files(g_catalog_folder_path)
  check_filename<-file_list[grep(check_code,file_list)]
  # flog.info()
  # flog.info(check_filename)
  df_check_list<-read.csv(paste(g_catalog_folder_path,check_filename,sep=""), header = TRUE, sep = ",", quote = "\"",
                          dec = ".", fill = TRUE, comment.char = "")

  return(
    subset(df_check_list, tolower(df_check_list$PEDSnet_Table)==tolower(table_name)
           & tolower(df_check_list$PEDSnet_Field)==tolower(field)))
}

get_check_entry_two_variables<-function(check_code, table_name, field1,field2)
{
  ## read specific checklist file for the given check code.
  file_list<-list.files(g_catalog_folder_path)
  check_filename<-file_list[grep(check_code,file_list)]
  
  # flog.info()
  # flog.info(check_filename)
  df_check_list<-read.csv(paste(g_catalog_folder_path,check_filename,sep=""), header = TRUE, sep = ",", quote = "\"",
                          dec = ".", fill = TRUE, comment.char = "")

  #print(df_check_list)
  #print(table_name)
  #print(field1)
  #print(field2)
  #print(subset(df_check_list,
  #             tolower(df_check_list$PEDSnet_Table)==tolower(table_name)))
    return(
    subset(df_check_list,
           tolower(df_check_list$PEDSnet_Table)==tolower(table_name)
           & tolower(df_check_list$PEDSnet_Field_1)==tolower(field1)
           & tolower(df_check_list$PEDSnet_Field_2)==tolower(field2)
           )
    )
}
get_check_entry_two_variables_diff_tables<-function(check_code, table1, field1,table2,field2)
{
  ## read specific checklist file for the given check code.
  file_list<-list.files(g_catalog_folder_path)
  check_filename<-file_list[grep(check_code,file_list)]
  # flog.info()
  # flog.info(check_filename)
  df_check_list<-read.csv(paste(g_catalog_folder_path,check_filename,sep=""), header = TRUE, sep = ",", quote = "\"",
                          dec = ".", fill = TRUE, comment.char = "")

  #print(df_check_list)
  return(
    subset(df_check_list,
           tolower(df_check_list$PEDSnet_Table_1)==tolower(table1)
           & tolower(df_check_list$PEDSnet_Field_1)==tolower(field1)
           &  tolower(df_check_list$PEDSnet_Table_2)==tolower(table2)
           & tolower(df_check_list$PEDSnet_Field_2)==tolower(field2)
    )
  )
}


## one variable and a value to compare against a threshold or print a message
apply_check_type_1<-function(check_code, field, value, table_name, g_data_version)
{

	# read check code from catalogue
  # flog.info(check_code)
  # flog.info(get_catalog_entry(check_code))
  check_entry <- cbind(get_catalog_entry(check_code),
                       get_check_entry_one_variable(check_code, table_name, field))
  # flog.info(check_entry)


  if(check_entry$check_code=='BA-001')
  {
	  if(value<check_entry$Lower_Threshold || value>check_entry$Upper_Threshold)
	  {
	    log_file_entry<-c(as.character(g_data_version),
	                      as.character(table_name),
	                      as.character(field),
	                      as.character(check_entry$check_code),
	                      as.character(check_entry$check_type),
	                      ' ',
	                      as.character(paste(value,'%'))  ,
	                      as.character(normalize_prevalence(value))
	                      )
	    # flog.info(log_file_entry)
	    return (log_file_entry);
	  }
  else
  {
    return(c()); # i.e. no issue
  }
  }

  if(
     check_entry$check_code=='AA-002')
  {
    if(nchar(trim(value))>0)
    {
      log_file_entry<-c(as.character(g_data_version),
                        as.character(table_name),
                        as.character(field),
                        as.character(check_entry$check_code),
                        as.character(check_entry$check_type),
                        ' ', 
                        as.character(value)
                            ,as.character('unknown'))
      # flog.info(log_file_entry)
      return (log_file_entry);
      #}
    }
    else
    {
      return(c()); # i.e. no issue
    }
  }
}


normalize_prevalence<-function(value)
{
  if (abs(value)<=1) { return('low')
    } else
  if (abs(value)<30) { return('medium')
  } else if (abs(value)<100) { return('high')
    } else {return('full')};

}

custom_rbind<-function(data_frame_x, row_y)
{
  # flog.info(row_y)
  if(class(row_y)!="NULL") {
    colnames(data_frame_x)<-c("g_data_version", "table","field", "issue_code", "issue_description",
                              "alias", "finding", "prevalence")
    data_frame_x$g_data_version<-as.character(data_frame_x$g_data_version)
    data_frame_x$table<-as.character(data_frame_x$table)
    data_frame_x$field<-as.character(data_frame_x$field)
    data_frame_x$issue_code<-as.character(data_frame_x$issue_code)
    data_frame_x$issue_description<-as.character(data_frame_x$issue_description)
    data_frame_x$alias<-as.character(data_frame_x$alias)
    data_frame_x$finding<-as.character(data_frame_x$finding)
    data_frame_x$prevalence<-as.character(data_frame_x$prevalence)

    return(rbind(data_frame_x,row_y)); }else {
    return(data_frame_x);}
}

prepare_csv_entry<-function(row_y)
{
  data_frame_x<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), issue_description=character(0)
                          , finding=character(0), prevalence=character(0))
  colnames(data_frame_x)<-c("g_data_version", "table","field", "issue_code", "issue_description","finding", "prevalence")

  if(class(row_y)!="NULL") {
    data_frame_x$field<-as.character(data_frame_x$field)
    data_frame_x$issue_code<-as.character(data_frame_x$issue_code)
    data_frame_x$issue_description<-as.character(data_frame_x$issue_description)
    data_frame_x$finding<-as.character(data_frame_x$finding)
    data_frame_x$prevalence<-as.character(data_frame_x$prevalence)

    return(rbind(data_frame_x,row_y)); }
  else {
      return(data_frame_x);}
}

get_prevalence<-function(value1,value2)
{
  difference_percent<-(value2-value1)/(current_total_count)
  return (normalize_prevalence(difference_percent))
}
