### implementation of the unexpected difference class 
MissData <- function()
{
  check_code="BA-001"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"MissData")
  return(me)
}


applyCheck.MissData <- function(theObject, table_list, field_list, table_df)
{
  table_name<-table_list[1]
  field_name<-field_list[1]
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
  
  df_null<-retrieve_dataframe_clause(table_df,"count(*)",paste0('is.na(',field_name,')') )
  df_count<-retrieve_dataframe_record_count(table_df)
  missing_percent<-round(((100 * df_null)/ df_count),digits=2)

  if(missing_percent<check_list_entry$Lower_Threshold || missing_percent>check_list_entry$Upper_Threshold)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, paste(missing_percent,'%'))
    # log issue 
    return(logIssue(issue_obj))
    
  }
  NextMethod("applyCheck",theObject)
  return(c())
}

