### implementation of the inclusion criteria violation class 
MissVisitFact <- function()
{
  check_code="BA-004"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"MissVisitFact")
  return(me)
}


applyCheck.MissVisitFact <- function(theObject, table_list, field_list, my_db)
{
  table_name<-table_list[1]
  #field_name<-field_list[1]
  check_list_entry<-get_check_entry_table_level(theObject$check_code, table_name)
  
  visit_tbl <- tbl(my_db, "visit_occurrence")
  condition_tbl<-tbl(my_db, "condition_occurrence")
  procedure_tbl<-tbl(my_db, "procedure_occurrence")
  drug_tbl<-tbl(my_db, "drug_exposure")
  measurement_tbl<-tbl(my_db, "measurement")
 
  total_visit_count<-  as.data.frame(summarise(visit_tbl,n = n()))[1,1]
  
  ### % of visits with no facts associated. 
  ## step 1 print # visits in 9202 , 9201, and 9203
  key_visits<-select(filter(visit_tbl, visit_concept_id==9201| visit_concept_id==9202|visit_concept_id==9203)
                     , visit_occurrence_id)
  total_key_visits<-nrow(key_visits)
  
  ## step 2 get  key visits that dont have any associated facts 
  temp<-union(select(condition_tbl, visit_occurrence_id), select(procedure_tbl, visit_occurrence_id), 
              select(measurement_tbl, visit_occurrence_id),   select(drug_tbl, visit_occurrence_id))
  
  ## step 
  result<-setdiff(key_visits,temp)
  final_result<-summarize(result, n=n())
  key_visits_without_facts<-as.data.frame(final_result)[1,1]
  ## step 3 get % of visits that dont have any facts and are key visits. 
  no_fact_percentage<-((key_visits_without_facts)*100/total_visit_count)
  #print(no_fact_percentage)
  
  if(no_fact_percentage>0)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, paste(no_fact_percentage,"%",sep=""))
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

