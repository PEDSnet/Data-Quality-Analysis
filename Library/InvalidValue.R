### implementation of the unexpected difference class 
InvalidValue <- function()
{
  check_code="AA-001"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"InvalidValue")
  return(me)
}


applyCheck.InvalidValue <- function(theObject, table_list, field_list,metadata, table_df)
{
  table_name<-table_list[1]
  field_name<-field_list[1]

  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
  
  value_set<-generate_list_concepts(table_name, metadata)
 
  value_set[,1]<-as.character(value_set[,1])
  order_bins <-c(value_set[,1], NA)

  df_table<-retrieve_dataframe_group(table_df,field_name)
  current_values<-c(df_table[,1])
  unexpected_message<-""

  if(nrow(df_table) > 0){
  for(i in 1:nrow(df_table))
  {
    value <-df_table[i,1]
    if(!is.element(trim(value),order_bins) & !is.na(value))
      unexpected_message<-paste(unexpected_message, trim(value),";")
    
  }
  }
  
  if( nchar(trim(unexpected_message))>0)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, unexpected_message)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

