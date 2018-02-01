### implementation of the inconsistent visit type class
InconVisitType <- function()
{
  check_code="CA-013"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"InconVisitType")
  return(me)
}


applyCheck.InconVisitType<- function(theObject, table_list, field_list, metadata)
{
  #print("inside applyCheck.InconVisitType")
  
  table_name_1<-table_list[1]
  table_name_2<-table_list[2]
  field_name_1<-field_list[1]
  field_name_2<-field_list[2]
  
  error_message<-metadata[1]
  visit_type<-metadata[2]
  
  check_list_entry<-get_check_entry_two_variables_diff_tables(theObject$check_code, table_name_1, field_name_1, 
                                                              table_name_2, field_name_2)
  
  fact_tbl <- cdm_tbl(req_env$db_src, table_name_1) 
  visit_tbl <- cdm_tbl(req_env$db_src, table_name_2) %>% filter_(
    paste0(field_name_2,  '==', visit_type))
  
  fact_visit_tbl <- fact_tbl %>% inner_join(visit_tbl, by ="visit_occurrence_id")
  #print(glimpse(visit_tbl))      
  
  #print(glimpse(fact_visit_tbl))      
  
  pk_field<-paste0(table_name_1,"_id")
  
  if(length(metadata)>2)
  {
    fact_type_list = c(metadata[3:length(metadata)])
   # print(fact_type_list)
    
    #print(glimpse(temp1))
    #print(glimpse(fact_visit_tbl))
   # print(field_name_1)
    fact_visit_tbl<- fact_visit_tbl %>%
      #filter(condition_type_concept_id == metadata[4]) # works 
      filter_(paste0(field_name_1,' == ',metadata[3], 
                    '|', field_name_1,' == ',metadata[4],
                    '|', field_name_1,' == ',metadata[5],
                    '|', field_name_1,' == ',metadata[6],
                    '|', field_name_1,' == ',metadata[7],
                    '|', field_name_1,' == ',metadata[8]
          ))
      #filter_(field_name_1 %in% fact_type_list) #%>%
      #summarize(count=n(condition_occurrence_id))
  # print(glimpse(fact_visit_tbl))
  }
  #print(glimpse(fact_visit_tbl))      
  df_fact_visit<-as.data.frame(fact_visit_tbl)
  #print(nrow(df_fact_visit))
  #print(temp1)
  ###########DQA CHECKPOINT############## 
  if(nrow(df_fact_visit)>0)
  {
    
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, paste0(nrow(df_fact_visit), " ", error_message))
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

