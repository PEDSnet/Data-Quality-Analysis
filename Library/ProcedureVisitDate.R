### implementation of date alignment check
ProcedureVisitDate <- function()
{
  check_code="CA-018"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"ProcedureVisitDate")
  return(me)
}


applyCheck.ProcedureVisitDate<- function(theObject, table_list, field_list)
{
  visit_tbl <- cdm_tbl(req_env$db_src, "visit_occurrence")
  procedure_tbl<-cdm_tbl(req_env$db_src, "procedure_occurrence")
  
  visits_off <-  procedure_tbl %>%
    inner_join(visit_tbl %>% select(visit_occurrence_id, visit_start_date, visit_end_date),
               by = 'visit_occurrence_id') %>%
    filter(procedure_date < visit_start_date |
           procedure_date > visit_end_date) %>%
    distinct() %>%
    tally() %>%
    as.data.frame()

  visits_off <- as.integer(visits_off[[1]])
  
  if(visits_off > 0){
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, paste("Found", visits_off,
                                                  "rows with procedure_date before associated visit_start_date or after visit_end_date"))
    # log issue 
    return(logIssue(issue_obj))
  }
  NextMethod("applyCheck",theObject)
  return(c())
}