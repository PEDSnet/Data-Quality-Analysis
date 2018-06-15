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


applyCheck.InvalidFormat <- function(theObject, table_list, field_list, con, format)
{
  table_name<-table_list[1]
  field_name<-field_list[1]
  #print(table_name)
  #print(field_name)
  df_table<-retrieve_dataframe_group(con,g_config,table_name,field_name)
  #print(head(df_table))   
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
  
  ## dropping NAs
  df_table<-df_table[!is.na(df_table[,1]),]
  
  invalid_message<-""
  #print(check_list_entry)
   ## check is source value has 2 pipes
    for(i in 1:nrow(df_table))  
    {
      source_value<-  df_table[i,1]
      #print(length(unlist(strsplit(source_value, '\\|'))))
      #print(format)
      #print(source_value)
      if(length(unlist(strsplit(source_value, '\\|')))!=format) 
      {
        invalid_message<-paste(invalid_message, source_value, sep=";")
      }
      
      if(nchar(trim(invalid_message)>500)) ## if sufficient examples have been collected
        break;
    }
  
   
  #print(substring(invalid_message,0,500))
  #invalid_message<-""
  if( nchar(trim(invalid_message))>0)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, substring(invalid_message,0,1000))
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

