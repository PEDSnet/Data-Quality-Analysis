### implementation of the unexpected difference class 
UnexDiffFactType <- function()
{
  check_code="CA-015"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"UnexDiffFactType")
  return(me)
}


applyCheck.UnexDiffFactType <- function(theObject, table_list, field_list, metadata)
{
  table_name<-table_list[1]
  field_name<-field_list[1]
  fact_type<-metadata[1]
  current_total_count<-as.numeric(metadata[2])
  prev_total_count<-get_previous_cycle_total_fact_type_count( g_config$reporting$site, paste0(table_name, ".",fact_type))
  
  if(is.na(prev_total_count)==TRUE) ## if previously this table was not generated
    return(c())
  
  percentage_diff<-get_percentage_diff(prev_total_count, current_total_count)
  
  
  check_list_entry<-get_check_entry_table_level(theObject$check_code, table_name)
  #print(check_list_entry)
  
  #print(table_list)
  if (is.na(percentage_diff)==FALSE)
  {
  if(percentage_diff<check_list_entry$Lower_Threshold || percentage_diff>check_list_entry$Upper_Threshold)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, fact_type, paste(percentage_diff,'%',sep=""))
    #print(issue_obj)
    # log issue 
    #print(logIssue(issue_obj))
    return(logIssue(issue_obj))
    
  }
  }
  NextMethod("applyCheck",theObject)
  return(c())
}

