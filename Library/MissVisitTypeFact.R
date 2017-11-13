### implementation of the inclusion criteria violation class 
MissVisitTypeFact <- function()
{
  check_code="BA-005"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"MissVisitTypeFact")
  return(me)
}


applyCheck.MissVisitTypeFact <- function(theObject, table_list, field_list, my_db)
{
  table_name<-table_list[1]
  #field_name<-field_list[1]
  check_list_entry<-get_check_entry_two_variables_diff_tables(theObject$check_code, table_name, "visit_concept_id" , table_list[2], field_list[2])
  #print(check_list_entry)
  visit_tbl <- tbl(my_db, "visit_occurrence")
  second_tbl<-tbl(my_db, table_list[2])
  
  #print(head(second_tbl))
  #total_visit_count<-  as.data.frame(summarise(visit_tbl,n = n()))[1,1]
  
  ### % of visits with no facts associated. 
  ## limit to key visits
  key_visits<-select(filter(visit_tbl, visit_concept_id==field_list[1])
                     , visit_occurrence_id)
  #print(head(key_visits))
  total_key_visits<-nrow(key_visits)
  
  #print(total_key_visits)
  
  ## step 2 get  key visits that dont have any associated facts 
  
  #print(field_list[2])
  #print(noquote(field_list[2]))
  #print(noquote(field_list[3]))
  #print(head(second_tbl))
  #temp<-filter(second_tbl, observation_concept_id==3040464) ### working 
  #temp<-filter(second_tbl, observation_concept_id==field_list[3]) ## working correctly 
  temp<-filter_(second_tbl, paste0(field_list[2],"==",field_list[3])) ## working correctly 
  
  #temp<-filter_(second_tbl, 'observation_concept_id'==quote('3040464')) ## working with incorrect result
  #temp<-filter_(second_tbl, noquote(field_list[2])==3040464) ### working but incorrect result
  #print(nrow(temp))
  second_tbl_with_facts<-select(temp, visit_occurrence_id)
  
  #print(head(second_tbl_with_facts))
  ## step 
  #result<-setdiff(key_visits$visit_occurrence_id,second_tbl_with_facts$visit_occurrence_id)
  result<-setdiff(key_visits,second_tbl_with_facts)
  
  #print(nrow(result))
  final_result<-dplyr::summarize(result, n=n())
  key_visits_without_facts<-as.data.frame(final_result)[1,1]
  
  ## step 3 get % of visits that dont have any facts and are key visits. 
  no_fact_percentage<-((key_visits_without_facts)*100/total_key_visits)
  #print(no_fact_percentage)
  
  if(check_list_entry$Lower_Threshold >no_fact_percentage || 
  check_list_entry$Upper_Threshold <no_fact_percentage )
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, paste("visit_concept_id","observation_concept_id",sep=","), 
                     paste0(no_fact_percentage,"% (for visit_concept_id=",field_list[1], " and ",field_list[2]
                            , "=",field_list[3]))
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

