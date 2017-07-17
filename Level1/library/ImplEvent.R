### implementation of the start date after end date class
ImplEvent <- function()
{
  check_code="CA-016"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"ImplEvent")
  return(me)
}


applyCheck.ImplEvent<- function(theObject, table_list, field_list, con)
{
  table_name<-table_list[1]
  start_field<-field_list[1]
  end_field<-field_list[2]
  
  check_list_entry<-get_check_entry_two_variables(theObject$check_code, table_name, start_field, end_field)
  
  #print(date_field)

  df_implausible_date_count<-retrieve_dataframe_clause(con, g_config, g_config$db$schema,table_name,
                                                       "count(*) as count",
                                                       paste(start_field,">",end_field))
  
  if(df_implausible_date_count[1][1]>0)
  {
     
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, df_implausible_date_count[1][1])
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

