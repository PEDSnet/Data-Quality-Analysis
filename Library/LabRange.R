### implementation of lab value range
LabRange <- function()
{
  check_code="CB-003"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"LabRange")
  return(me)
}


applyCheck.LabRange <- function(theObject, table_list, field_list)
{
  table_name <- table_list[1]
  field_name <- field_list[1]
    
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
  
  lab_tbl <- cdm_tbl(req_env$db_src, "measurement") %>% 
    filter(measurement_type_concept_id == 44818702,
           !is.na(value_as_number))
  
  total <- as.data.frame(lab_tbl %>% tally())
  valid <- as.data.frame(lab_tbl %>%
    filter(value_as_number >= range_low,
           value_as_number <= range_high) %>%
    distinct() %>%
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






