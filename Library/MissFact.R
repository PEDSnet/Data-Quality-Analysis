### implementation of the missing expected fact class
MissFact <- function()
{
  check_code="BA-003"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"MissFact")
  return(me)
}


applyCheck.MissFact <- function(theObject, table_list, field_list, con, metadata)
{
  table_name<-table_list[1]
  field_name<-field_list[1]

  value_name<-metadata[length(metadata)]
    
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
  
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  
  
  if(length(metadata)==2)
  {
    number_of_records<-nrow(subset(df_table,df_table[,1]==metadata[1]))
  } else {
      values="";
      for(value_index in 1:length(metadata)-1)
      {
        if(value_index>1)
        values<-paste(values,",", metadata[value_index])   
        else values<-paste(values, metadata[value_index]) 
      }
      number_of_records<-retrieve_dataframe_clause(con, g_config, g_config$db$schema,table_name,
                                                "count(*)",
                                                paste(field_name,"in (",values,")")
                                                )[1,1]
    
  }
  
  if( number_of_records==0)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, paste(metadata[1], ":", value_name))
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

