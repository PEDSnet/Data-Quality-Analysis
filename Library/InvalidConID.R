### implementation of the unexpected difference class 
library(tictoc)
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


applyCheck.InvalidConID <- function(theObject, table_list, field_list, metadata, table_df, table_df2)
{
  table_name<-table_list[1]
  field_name<-field_list[1]

  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
  if(is.character(metadata)){
   if(grepl('csv$', metadata)){ 
      concept_id_list<-generate_list_concepts(table_name, metadata)
    } else{
      concept_id_list <-generate_df_concepts(table_name, metadata, table_df)
    }
  }
  else{
    concept_id_list <- metadata
  }

  order_bins <-c(concept_id_list$concept_id,0,44814650,44814653, 44814649,NA)
  df_table<-retrieve_dataframe_group(table_df2,field_name)

  invalid <- setdiff(df_table[[1]], order_bins)
  
  unexpected_message<-  paste("", invalid, collapse = ';')
  if( length(invalid) > 0)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, unexpected_message)
    # log issue 
    return(logIssue(issue_obj))
  }
  NextMethod("applyCheck",theObject)
  return(c())
}

