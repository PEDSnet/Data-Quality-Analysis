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


applyCheck.MissFact <- function(theObject, table_list, field_list, con, list_of_facts)
{
  issue_list<-data.frame(g_data_version=character(0), table=character(0),field=character(0), issue_code=character(0), 
                          issue_description=character(0), alias=character(0)
                          , finding=character(0), prevalence=character(0))  
  table_name<-table_list[1]
  field_name<-field_list[1]
  
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
  
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  #print(length(list_of_facts))
  
  for(list_index in 1:length(list_of_facts))
  {
  #print(list_index)
  metadata<- unlist(list_of_facts[list_index])
  value_name<-metadata[length(metadata)]
  #print(value_name)
  
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
    issue_list<-custom_rbind( issue_list, logIssue(issue_obj))
    
  }
  } ## for loop ends
  
  NextMethod("applyCheck",theObject)
  #return(c())
  #print(issue_list)
  colnames(issue_list)<-c("g_data_version", "table","field", "issue_code", "issue_description",
                            "alias", "finding", "prevalence")
  
  return(issue_list)
}

