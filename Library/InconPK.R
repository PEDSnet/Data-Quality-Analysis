### implementation of the unexpected difference class 
InconPK <- function()
{
  check_code="AA-003"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"InconPK")
  return(me)
}


applyCheck.InconPK <- function(theObject, table_list, field_list, con)
{
  table_name<-table_list[1]
  field_name_1<-field_list[1]
  field_name_2<-field_list[2]
  
  check_list_entry<-get_check_entry_two_variables(theObject$check_code, table_name, field_name_1, field_name_2)
  
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name_1)
  count1<-describeIdentifier(df_table,field_name_1)
  
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name_2)
  count2<-describeIdentifier(df_table,field_name_2)
  
  diff<-count1-count2
  
  if(diff<check_list_entry$Lower_Threshold || diff>check_list_entry$Upper_Threshold)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, diff)
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

