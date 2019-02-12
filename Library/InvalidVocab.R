### implementation of the unexpected difference class 
InvalidVocab <- function()
{
  check_code="AA-005"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"InvalidVocab")
  return(me)
}


applyCheck.InvalidVocab <- function(theObject, table_list, field_list, domain_vocabulary, table_df, table_df2)
{
  table_name<-table_list[1]
  field_name<-field_list[1]
  domain<-domain_vocabulary[1]
  
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
   
  used_vocabulary<-get_vocabulary_name_by_concept_ids(table_name, field_name,domain, table_df, table_df2)
  
  acceptable_vocabulary<-c(domain_vocabulary[2:length(domain_vocabulary)], NA)
  
  unexpected_message<-NULL
 
   if(length(used_vocabulary)>0)
  {
    for(i in 1:length(used_vocabulary))  
    {
      if(is.na(match(used_vocabulary[i,1], acceptable_vocabulary)))
      {
      unexpected_message<-paste(used_vocabulary[i,1], sep=";")
      }
    }
  }
  
  if(length(unexpected_message)>0)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, unexpected_message)
    # log issue 
    return(logIssue(issue_obj))
  }
  NextMethod("applyCheck",theObject)
  return(c())
}

