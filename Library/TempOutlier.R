### implementation of the temporal outlier check 
TempOutlier <- function()
{
  check_code="CA-008"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"TempOutlier")
  return(me)
}


applyCheck.TempOutlier<- function(theObject, table_list, field_list, fact_type)
{
  table_name<-table_list[1]
  date_field<-field_list[1]
  
  #date_field<-field_list[2]
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, date_field)
  #print(fact_type)
  if(is.null(fact_type)) ### if generating summary for entire table
  {
  date_dist_tbl<-cdm_tbl(req_env$db_src, table_name) %>%
             group_by_(date_field) %>% 
              dplyr::summarise(date_level_count = n()) %>%
              collect()
  } else 
  {
    fact_type_concept_id<-fact_type[1]
    fact_type_concept_id_colname<-field_list[2]
    
    date_dist_tbl<-cdm_tbl(req_env$db_src, table_name) %>%
      filter_(paste0(fact_type_concept_id_colname,'==',fact_type_concept_id)) %>%
      group_by_(date_field) %>% 
      dplyr::summarise(date_level_count = n()) %>%
      collect()
  }
  
  #print(class(date_dist_tbl[,1]))
  #glimpse(date_dist_tbl)
  date_dist_tbl<-as.data.frame(date_dist_tbl)
  
  #print(date_dist_tbl[1,1])
  date_dist_tbl$year = year(as.Date(date_dist_tbl[,1]))
  date_dist_tbl$month = str_pad(month(as.Date(date_dist_tbl[,1])), 2, pad = "0")
  
  if(table_name=="death")
    date_dist_tbl<-date_dist_tbl %>% filter(month <'12' & month > '01')
  #print(glimpse(date_dist_tbl))
  
  ## this table contains monthly distributions 
  date_dist_tbl_simplified<- date_dist_tbl %>%
      filter(year>= 2012) %>%
      filter(year<= 2017) %>%
    mutate(yyyymm = paste0(year,'-',month)) %>%
    group_by (yyyymm) %>%
    dplyr::summarise(yyyymm_level_count = sum(date_level_count)) %>%
    dplyr::mutate(rnum = row_number()) %>%
    dplyr::mutate(next_rnum = rnum+1)
  
  ## plot this table 
  describeYYMMField(date_dist_tbl_simplified%>% select(yyyymm, yyyymm_level_count)
                    , table_name, date_field, fact_type)
  
  
  #print(glimpse(date_dist_tbl_simplified))
  
  ## this table contains deltas and the data points used for outlier detection 
  date_dist_delta<- date_dist_tbl_simplified %>% 
      inner_join(date_dist_tbl_simplified, by = c("next_rnum" = "rnum")) %>%
      dplyr::mutate(change_over_last_month = yyyymm_level_count.y- yyyymm_level_count.x) %>%
      select(yyyymm.y, change_over_last_month)
  
  #print(glimpse(date_dist_delta))  
  
  iqr_value<- IQR(date_dist_delta$change_over_last_month)
  lower_bound<- -1.5*iqr_value
  upper_bound<-1.5*iqr_value
  
  date_dist_delta$change_over_last_month<-as.integer(date_dist_delta$change_over_last_month)
  
  if(nrow(date_dist_delta)>0)
  {
  date_dist_delta$outlier<-FALSE
  for(i in 1:nrow(date_dist_delta))
  {
    #print(date_dist_delta[i,2])
    if(date_dist_delta[i,2]<lower_bound) 
      date_dist_delta[i,3]<-TRUE
    if(date_dist_delta[i,2]>upper_bound) 
      date_dist_delta[i,3]<-TRUE
  }

  
  #print(mismatch_date_tbl)
  df_outliers<-date_dist_delta[date_dist_delta$outlier==TRUE,c(1,2)]
  if(is.null(fact_type)) 
  outlier_message<- "list of months with unexpected changes in number of records YYYY-MM (change since last month):\n"
  else
    outlier_message<- paste0("list of months with unexpected changes in number of records ",fact_type[2],
                             "YYYY-MM (change since last month):\n")
  
  for( i in 1:nrow(df_outliers))
  {
    outlier_message<-paste(outlier_message,df_outliers[i,1], "(", df_outliers[i,2],")")
  }
  
    
  if(nrow(df_outliers)>0)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, outlier_message)
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  }
  NextMethod("applyCheck",theObject)
  return(c())
}

