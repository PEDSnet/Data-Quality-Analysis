### implementation of date alignment check
DateAlign <- function()
{
  check_code="CA-017"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"DateAlign")
  return(me)
}


applyCheck.DateAlign<- function(theObject, table_list, field_list)
{

###Opting to not use dplyr here since it's slower for this operation
  max_visit <- as.integer(dbGetQuery(conn = req_env$db_src, 
             paste0("select max(visit_start_date) from ",req_env$cdm_schema ,
                    ".visit_occurrence where visit_start_date <= CURRENT_DATE")))
  
  max_condition <- as.integer(dbGetQuery(conn = req_env$db_src, 
                          paste0("select max(condition_start_date) from ",req_env$cdm_schema ,
                                 ".condition_occurrence where condition_start_date <= CURRENT_DATE")))
  
  max_drug <- as.integer(dbGetQuery(conn = req_env$db_src, 
                          paste0("select max(drug_exposure_start_date) from ",req_env$cdm_schema ,
                                 ".drug_exposure where drug_exposure_start_date <= CURRENT_DATE")))
  
  max_meas <- as.integer(dbGetQuery(conn = req_env$db_src, 
                          paste0("select max(measurement_date) from ",req_env$cdm_schema ,
                                 ".measurement where measurement_date <= CURRENT_DATE")))
  
  align_message = ""
  
  fields <- NULL
  
  if(abs(max_visit - max_condition) > 14){
    align_message <- paste(align_message, "Max condition_start_date has a ",
                           max_visit - max_condition, "date misalignment with max visit_start_date ;")
    fields <- c(fields, "condition_start_date")
  }
  if(abs(max_visit - max_drug) > 14){
    align_message <- paste(align_message, "Max drug_exposure_start_date has a ",
                           max_visit - max_drug, "date misalignment with max visit_start_date ;")
    fields <- c(fields, "drug_exposure_start_date")
  }
  if(abs(max_visit - max_meas) > 14){
    align_message <- paste(align_message, "Max measurement_date has a ",
                           max_visit - max_meas, "date misalignment with max visit_start_date ;")
    fields <- c(fields, "measurement_date")
  }
  
 if(align_message != ""){
      # create an issue 
      issue_obj<-Issue(theObject, table_list, paste(field_list,fields, sep = ","), align_message)
      # log issue 
      return(logIssue(issue_obj))
  }
  NextMethod("applyCheck",theObject)
  return(c())
}






