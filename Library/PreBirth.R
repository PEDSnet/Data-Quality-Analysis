### implementation of the inconsistent date / time check
PreBirth <- function()
{
  check_code="CA-003"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"PreBirth")
  return(me)
}


applyCheck.PreBirth<- function(theObject, table_list, field_list)
{
  fact_table_name<-table_list[1]
  date_field<-field_list[1]

  fact_tbl <- cdm_tbl(req_env$db_src, fact_table_name)
  
  patient_tbl<-cdm_tbl(req_env$db_src, 'person') %>% 
    select(person_id, birth_datetime) 
                   
  df_before_dob<- fact_tbl %>% 
      inner_join(patient_tbl, by =c("person_id"="person_id")
                             ) %>%
    mutate(birth_datetime = sql('cast("birth_datetime" as date)')) %>%
    filter_(paste0('birth_datetime >', date_field)) %>%
    select_(quote(person_id), date_field) %>% collect() 
  
   if(nrow(df_before_dob)>0)
  {
    # create an issue 
    issue_obj<-Issue(theObject, fact_table_name, date_field, (nrow(df_before_dob)))
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

