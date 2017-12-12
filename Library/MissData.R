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


applyCheck.MissData <- function(theObject, table_list, field_list, con)
{
  table_name<-table_list[1]
  field_name<-field_list[1]
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
  
  df_null<-retrieve_dataframe_clause(con, g_config,g_config$db$schema,table_name,"count(*)",paste0(field_name,' is null') )
  #print(df_null)
  df_count<-retrieve_dataframe_record_count(con, g_config, table_name )
  #print(df_count)
  missing_percent<-
       round(
          ((100 * df_null[1,1])/ df_count[1,1]),digits=2)
       
      
  #print(missing_percent)  
  #print(check_list_entry$Lower_Threshold)
  
  if(missing_percent<check_list_entry$Lower_Threshold || missing_percent>check_list_entry$Upper_Threshold)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, paste(missing_percent,'%'))
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

