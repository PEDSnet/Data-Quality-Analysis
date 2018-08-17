### implementation of the unexpected difference class 
InvalidFormat <- function()
{
  check_code="AA-010"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"InvalidFormat")
  return(me)
}


applyCheck.InvalidFormat <- function(theObject, table_list, field_list, format, table_df)
{
  table_name<-table_list[1]
  field_name<-field_list[1]
  print(summary(table_df))
  df_table<-retrieve_dataframe_group(table_df,field_name)
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
  
  ## dropping NAs
  df_table<-df_table[!is.na(df_table[,1]),]
  
  invalid_message<-""
   ## check is source value has 2 pipes
    for(i in 1:nrow(df_table))  
    {
      source_value<-  df_table[i,1]
      if(length(unlist(strsplit(source_value, '\\|')))!=format) 
      {
        invalid_message<-paste(invalid_message, source_value, sep=";")
      }
      
      if(nchar(trim(invalid_message)>500)) ## if sufficient examples have been collected
        break;
    }

  if( nchar(trim(invalid_message))>0)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, substring(invalid_message,0,1000))
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

