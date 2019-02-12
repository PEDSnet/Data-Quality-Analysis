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


applyCheck.MissConID<- function(theObject, table_list, field_list, table_df)
{ 
  table_name<-table_list[1]
  field_name<-field_list[1]
  print("New test 0 ")
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)

  print("New test 1")
  df_nomatch <- tryCatch(retrieve_dataframe_clause(table_df,"count(*)", paste0(field_name,"==0")),
           error = function(e) 0)
  print("New test 2")
  df_total <- tryCatch(retrieve_dataframe_record_count(table_df),
                       error = function(e) 0)
  print("New test 3")
  
  if(df_total > 0){ no_matching_perc<-round(df_nomatch*100/df_total, 2)}
  else{ no_matching_perc = NA}
  print("New test 4")
  if(no_matching_perc<check_list_entry$Lower_Threshold || no_matching_perc>check_list_entry$Upper_Threshold)
  {
    # create an issue 
    print("New test 5")
    issue_obj<-Issue(theObject, table_list, field_list, paste(no_matching_perc,"%",sep=""))
    print("New test 6")
    # log issue 
    return(logIssue(issue_obj))
    
  }
  NextMethod("applyCheck",theObject)
  return(c())
}

