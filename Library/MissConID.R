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
 
  #print(check_list_entry)
  #df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)

  df_nomatch<-retrieve_dataframe_clause(con, g_config,g_config$db$schema, table_name,"count(*)", paste0(field_name,"=0"))
  
  df_total<-retrieve_dataframe_record_count(con,g_config,table_name)


  no_matching_perc<-round(df_nomatch[1,1]*100/df_total[1,1], 2)

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

