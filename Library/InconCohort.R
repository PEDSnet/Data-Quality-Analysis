### implementation of the inclusion criteria violation class 
InconCohort <- function()
{
  check_code="AA-006"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"InconCohort")
  return(me)
}


applyCheck.InconCohort <- function(theObject, table_list, field_list)
{
  table_name<-table_list[1]
  check_list_entry<-get_check_entry_table_level(theObject$check_code, table_name)
  
  visit_tbl <- cdm_tbl(req_env$db_src, "visit_occurrence")
  patient_tbl<-cdm_tbl(req_env$db_src, table_name)
  condition_tbl<-cdm_tbl(req_env$db_src, "condition_occurrence")
  
  all_patients<-select(patient_tbl,person_id)

  visit_tbl_new<-visit_tbl %>% 
     dplyr::mutate(visit_year = sql('extract(year from "visit_start_date")')) %>%
    select(visit_concept_id, visit_year, person_id) 
                
  ## Patients  satisfying inclusion criteria
  valid_patients_by_visit<-select(filter(visit_tbl_new,visit_year >=2009
                                         & (visit_concept_id==9201
                                            |visit_concept_id==9202
                                            |visit_concept_id==9203
                                            |visit_concept_id==2000000469
                                            |visit_concept_id==42898160
                                            |visit_concept_id==44814710
                                            |visit_concept_id==2000000048
                                            |visit_concept_id==2000000088)
  ), person_id
  )
  
  condition_tbl_new<- condition_tbl %>%  
    dplyr::mutate(condition_year = sql('extract(year from "condition_start_date")')) %>%
    select (condition_year, person_id)
  
  valid_patients_by_condition<-condition_tbl_new %>% 
    filter(condition_year >=2009) %>%
                  select(person_id)
  
  all_valid_patients<-inner_join(valid_patients_by_visit,valid_patients_by_condition, by ="person_id")

  #patients not satisfying inclusion criteria
  invalid_patients<-anti_join(all_patients,all_valid_patients, by ="person_id")
  df_invalid_patients<-as.data.frame(invalid_patients)
  
  if(nrow(df_invalid_patients)>1)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, nrow(df_invalid_patients))
    # log issue 
    return(logIssue(issue_obj))
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

