### implementation of the unexpected difference class 
MissConID <- function()
{
  check_code="BA-002"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"MissConID")
  return(me)
}


applyCheck.MissConID<- function(theObject, table_list, field_list, con)
{
  table_name<-table_list[1]
  field_name<-field_list[1]
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
  
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
 
  no_matching_message<-reportNoMatchingCount(df_table,table_name,field_name,TRUE)
  no_matching_perc<-extract_numeric_value(no_matching_message)
 
  #print(no_matching_perc)
  if(no_matching_perc<check_list_entry$Lower_Threshold || no_matching_perc>check_list_entry$Upper_Threshold)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, paste(no_matching_perc,"%",sep=""))
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

