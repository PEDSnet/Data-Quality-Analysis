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
  table_name_1<-table_list[1]
  table_name_2<-table_list[2]
  field_name_1<-field_list[1]
  field_name_2<-field_list[2]
  
  
  #print(check_list_entry)
  fact_tbl <- cdm_tbl(req_env$db_src, table_name_1)
  
  
  patient_tbl<-cdm_tbl(req_env$db_src, table_name_2) %>%
    dplyr::mutate(birth_date = sql('cast("birth_datetime" as date)'))
  
  #glimpse(patient_tbl)
  field_name_2<-"birth_date"
                   
  df_before_dob<-as.data.frame(
    select_(
      filter_(inner_join(fact_tbl,patient_tbl, by =c("person_id"="person_id")),
              paste0(field_name_1, '<', field_name_2))
      ,quote(person_id), field_name_1
    ))

  print(nrow(df_before_dob))  
   if(nrow(df_before_dob)>0)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, (nrow(df_before_dob)))
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

