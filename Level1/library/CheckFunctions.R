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

  return(
    subset(df_check_list,
           tolower(df_check_list$PEDSnet_Table_1)==tolower(table1)
           & tolower(df_check_list$PEDSnet_Field_1)==tolower(field1)
           &  tolower(df_check_list$PEDSnet_Table_2)==tolower(table2)
           & tolower(df_check_list$PEDSnet_Field_2)==tolower(field2)
    )
  )
}

## no variables
## a custom upper threshold is provided
#apply_check_type_0<-function(check_code, message, upper_threshold)
apply_check_type_0<-function(check_code, value,table_name, g_data_version)
{

  # read check code from catalogue
  check_entry <- cbind(get_catalog_entry(check_code),
                       get_check_entry_table_level(check_code, table_name))
  #print(check_entry)
  
  if(any(check_entry$check_code=='CA-005'))
  {
    if(value<check_entry$Lower_Threshold || value>check_entry$Upper_Threshold)
    {
      log_file_entry<-c(as.character(g_data_version),
                        as.character(table_name),
                        "",
                        as.character(check_entry$check_code),
                        as.character(check_entry$check_type),
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

  if(check_entry$check_code=='AA-006'
     )
  {

      log_file_entry<-c(as.character(g_data_version),
                        as.character(table_name),
                        "",
                        as.character(check_entry$check_code),
                        as.character(check_entry$check_type),
                        as.character(value),
                        as.character(normalize_prevalence(value))
      )
      # flog.info(log_file_entry)
      return (log_file_entry);
  }
 
  if(check_entry$check_code=='BA-004')
  {
    if(value<check_entry$Lower_Threshold || value>check_entry$Upper_Threshold)
    {
    log_file_entry<-c(as.character(g_data_version),
                      as.character(table_name),
                      "",
                      as.character(check_entry$check_code),
                      as.character(check_entry$check_type),
                      as.character(paste(value,"%",sep="")),
                      "unknown")
    
    # flog.info(log_file_entry)
    return (log_file_entry);
    }
    else
    {
      return(c()); # i.e. no issue
    }
    
  }
  



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


  if(check_entry$check_code=='BA-001' || check_entry$check_code=='BA-002')
  {
	  if(value<check_entry$Lower_Threshold || value>check_entry$Upper_Threshold)
	  {
	    log_file_entry<-c(as.character(g_data_version),
	                      as.character(table_name),
	                      as.character(field),
	                      as.character(check_entry$check_code),
	                      as.character(check_entry$check_type),
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

  if(any(check_entry$check_code=='CA-011'))
  {
    if(any(value>=check_entry$Upper_Threshold))
    {
      log_file_entry<-c(as.character(g_data_version),
                        as.character(table_name),
                        as.character(field),
                        as.character(check_entry$check_code),
                        as.character(check_entry$check_type),
                        as.character(paste(value))  ,
                        "low"
      )
      # flog.info(log_file_entry)
      # flog.info(log_file_entry)
      return (log_file_entry);
    }
    else
    {
      return(c()); # i.e. no issue
    }
  }


  if(check_entry$check_code=='AA-001'
     ||check_entry$check_code=='AA-002' || check_entry$check_code=='CA-001'
     || check_entry$check_code=='AA-005'
      || check_entry$check_code=='BA-003'|| check_entry$check_code=='CB-002')
  {
    if(nchar(trim(value))>0)
    {
      log_file_entry<-c(as.character(g_data_version),
                        as.character(table_name),
                        as.character(field),
                        as.character(check_entry$check_code),
                        as.character(check_entry$check_type),
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

## two variables from same table and two associated values
apply_check_type_2<-function(check_code, field1, field2, diff, table_name, g_data_version)
{
  # flog.info("here")
  # read check code from catalogue
  #print("here")
  #print(get_catalog_entry(check_code))
  #print(get_check_entry_two_variables(check_code, table_name, field1, field2))
  
  check_entry <- cbind(get_catalog_entry(check_code),
                       get_check_entry_two_variables(check_code, table_name, field1, field2))

  #print("here")
  # flog.info(get_catalog_entry(check_code))
  if(diff<check_entry$Lower_Threshold || diff>check_entry$Upper_Threshold)
  {
    # create issue
    if(any(check_entry$check_code=='AA-003' )||check_entry$check_code=='AA-009')
    {
         log_file_entry<-c(as.character(g_data_version), as.character(table_name),
                           paste(as.character(field1),",",as.character(field2)), as.character(check_entry$check_code),
                        as.character(check_entry$check_type),
                        as.character(diff),
                        as.character(normalize_prevalence(diff)))
        #  flog.info(log_file_entry)
      return (log_file_entry);
    }
    if( any(check_entry$check_code=='CA-014'))
    {
      log_file_entry<-c(as.character(g_data_version),
                        as.character(table_name),
                        paste(as.character(field1),",",as.character(field2)),
                        as.character(check_entry$check_code),
                        as.character(check_entry$check_type),
                        paste(as.character(diff),"%"),
                        as.character(normalize_prevalence(diff)))
      #  flog.info(log_file_entry)
      return (log_file_entry);
    }

  }
  else
  {
    return(c()); # i.e. no issue
  }
  if(any(check_entry$check_code=='CA-016'))
  {
    log_file_entry<-c(as.character(g_data_version),
                      as.character(table_name),
                      paste(as.character(field1),",",as.character(field2)),
                      as.character(check_entry$check_code),
                      as.character(check_entry$check_type),
                      diff,
                     'unknown')
    #  flog.info(log_file_entry)
    return (log_file_entry);
  }
}

# two variables from diff tables
apply_check_type_2_diff_tables<-function(check_code,table1, field1, table2, field2, message)
{
 check_entry <- cbind(get_catalog_entry(check_code),
                       get_check_entry_two_variables_diff_tables(check_code, table1, field1,table2, field2))

  # flog.info(check_entry)
    # create issue
    if(check_entry$check_code=='CA-003'
       || check_entry$check_code=='CA-004')
    {
      log_file_entry<-c(as.character(g_data_version),
                        table2,
                        paste(as.character(field1),",",as.character(field2)),
                        as.character(check_entry$check_code),
                        as.character(check_entry$check_type),
                        message,
                        "unknown")
      #  flog.info(log_file_entry)
      return (log_file_entry);
    } else
      {
        if ( any(check_entry$check_code=='BA-004')) {
          log_file_entry<-c(as.character(g_data_version),
                            #as.character(paste(table1,",",table2)), # causes error during merging of issues so picking only the first table
                            as.character(table1),
                            paste(as.character(field1),",",as.character(field2)),
                            as.character(check_entry$check_code),
                            as.character(check_entry$check_type),
                            message,
                            "unknown")
          # flog.info(log_file_entry)
          return (log_file_entry);
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
    colnames(data_frame_x)<-c("g_data_version", "table","field", "issue_code", "issue_description","finding", "prevalence")
    data_frame_x$table<-as.character(data_frame_x$table)
    data_frame_x$field<-as.character(data_frame_x$field)
    data_frame_x$issue_code<-as.character(data_frame_x$issue_code)
    data_frame_x$issue_description<-as.character(data_frame_x$issue_description)
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
