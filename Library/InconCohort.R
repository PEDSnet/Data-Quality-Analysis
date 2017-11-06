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


applyCheck.InconCohort <- function(theObject, table_list, field_list, my_db)
{
  table_name<-table_list[1]
  #field_name<-field_list[1]
  check_list_entry<-get_check_entry_table_level(theObject$check_code, table_name)
  
  visit_tbl <- tbl(my_db, "visit_occurrence")
  patient_tbl<-tbl(my_db, table_name)
  condition_tbl<-tbl(my_db, "condition_occurrence")
  
  all_patients<-select(patient_tbl,person_id)
  
  ## Patients  satisfying inclusion criteria
  valid_patients_by_visit<-select(filter(visit_tbl,visit_start_date >='2009-01-01'
                                         & (visit_concept_id ==9201
                                            |visit_concept_id== 9202
                                            |visit_concept_id== 9203
                                            |visit_concept_id==42898160
                                            |visit_concept_id==44814710
                                            |visit_concept_id==2000000048
                                            |visit_concept_id==2000000088)
  ), person_id
  )
  
  valid_patients_by_condition<-select(filter(condition_tbl,condition_start_date >='2009-01-01'),person_id)
  
  #patients not satisfying inclusion criteria
  invalid_patients<-setdiff(all_patients,intersect(valid_patients_by_visit,valid_patients_by_condition))
  #print(head(invalid_patients))
  df_invalid_patients<-as.data.frame(invalid_patients)

  #print(nrow(df_invalid_patients))
  
  if(nrow(df_invalid_patients)>0)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, nrow(df_invalid_patients))
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

