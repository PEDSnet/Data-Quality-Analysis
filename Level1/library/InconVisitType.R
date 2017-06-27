### implementation of the inconsistent visit type class
InconVisitType <- function()
{
  check_code="CA-013"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"InconVisitType")
  return(me)
}


applyCheck.InconVisitType<- function(theObject, table_list, field_list, con)
{
  table_name_1<-table_list[1]
  table_name_2<-table_list[2]
  field_name_1<-field_list[1]
  field_name_2<-field_list[2]
  
  
  check_list_entry<-get_check_entry_two_variables_diff_tables(theObject$check_code, table_name_1, field_name_1, 
                                                              table_name_2, field_name_2)
  
  df_outpatient_adts_count<-retrieve_dataframe_join_clause(con,g_config,g_config$db$schema,table_name_1, 
                                                           g_config$db$schema,
                                                           table_name_2,"count(*)",
                                                           "adt_occurrence.visit_occurrence_id = visit_occurrence.visit_occurrence_id
                                                           and visit_concept_id in (9202, 44814711)") 
  
  ###########DQA CHECKPOINT############## 
  if(df_outpatient_adts_count[1,1]>0)
  {
    
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, df_outpatient_adts_count[1,1])
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

