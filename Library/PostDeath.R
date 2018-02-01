### implementation of post death facts class
PostDeath<- function()
{
  check_code="CA-004"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"PostDeath")
  return(me)
}


applyCheck.PostDeath<- function(theObject, table_list, field_list)
{
  table_name_1<-table_list[1]
  table_name_2<-table_list[2]
  field_name_1<-field_list[1]
  field_name_2<-field_list[2]
  
  check_list_entry<-get_check_entry_two_variables_diff_tables(theObject$check_code, table_name_1, field_name_1, 
                                                              table_name_2, field_name_2)
  
  fact_tbl <- cdm_tbl(req_env$db_src, table_name_1)
  
  
  patient_tbl<-cdm_tbl(req_env$db_src, table_name_2)
  
 # print(field_name_1)
  df_after_death<-as.data.frame(
    select_(
      filter_(inner_join(fact_tbl,patient_tbl, by =c("person_id"="person_id")),
              paste0(field_name_1, '>', field_name_2))
      ,quote(person_id), field_name_1
    ))
 
  #print(head(df_after_death))
  if(nrow(df_after_death)>0)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, (nrow(df_after_death)))
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

