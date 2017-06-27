### implementation of the unexpected difference class 
NumOutlier <- function()
{
  check_code="CA-011"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"NumOutlier")
  return(me)
}


applyCheck.NumOutlier<- function(theObject, table_list, field_list, con)
{
  table_name<-table_list[1]
  field_name<-field_list[1]
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
  #print(check_list_entry)
   
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)

  colnames(df_table)[1] <- "Var1"
  #colnames(df_table)[2] <- "Freq"
  # identify row with null value
  new_df_table<-subset(df_table, !is.na(df_table[1]))
  if(min(new_df_table$Var1)<check_list_entry$Lower_Threshold || max(new_df_table$Var1)>check_list_entry$Upper_Threshold)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, paste(min(new_df_table$Var1), "-", max(new_df_table$Var1)))
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

