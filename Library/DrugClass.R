### implementation of lab value range
DrugClass <- function()
{
  check_code="CB-004"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"DrugClass")
  return(me)
}


applyCheck.DrugClass <- function(theObject, table_list, field_list)
{
  table_name <- table_list[1]
  field_name <- field_list[1]
 
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
  
  drug_tbl <- cdm_tbl(req_env$db_src, "drug_exposure") %>%
    inner_join(vocab_tbl(req_env$db_src, "concept") %>%
                 select(concept_id, concept_class_id), by = c("drug_concept_id" = "concept_id")) 
  
  total <- as.data.frame(drug_tbl %>% tally())
  valid <- as.data.frame(drug_tbl %>%
    filter(concept_class_id %in% c("Branded Drug Form", "Clinical Pack", "Clinical Drug","Branded Drug Comp",
                                   "Clinical Drug Comp","Branded Drug","Branded Pack","Clinical Dose Group",
                                   "Clinical Drug Form","Branded Dose Group","Brand Name","Quant Branded Drug",
                                   "Quant Clinical Drug","Clinical Pack")) %>%
      tally())

  perc <- round(valid/total*100,2)

 if(perc<check_list_entry$Lower_Threshold || perc>check_list_entry$Upper_Threshold){
      # create an issue 
      issue_obj<-Issue(theObject, table_list, field_list,
                       paste0(perc,"%"))
      # log issue 
      return(logIssue(issue_obj))
 }
  NextMethod("applyCheck",theObject)
  return(c())
}






