### implementation of the unexpected difference class 
InvalidConID <- function()
{
  check_code="AA-002"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"InvalidConID")
  return(me)
}


applyCheck.InvalidConID <- function(theObject, table_list, field_list, con, metadata)
{
  table_name<-table_list[1]
  field_name<-field_list[1]

  
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
  
  if(grepl('csv$', metadata)==TRUE)
  { 
    concept_id_list<-generate_list_concepts(table_name, metadata)
    } else
  {
    concept_id_list <-generate_df_concepts(con, table_name, metadata)
    
  }
  order_bins <-c(concept_id_list$concept_id,0,44814650,44814653, 44814649,NA)
  #print(order_bins)
  
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  current_values<-c(df_table[,1])
  unexpected_message<-""
  for(i in 1:nrow(df_table))
  {
    value <-df_table[i,1]
    # flog.info(df_table[i,1])
    if(!is.element(value,order_bins) && !is.na(value))
      unexpected_message<-paste(unexpected_message, value,";")
    
  }
  
  if( nchar(trim(unexpected_message))>0)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, unexpected_message)
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

