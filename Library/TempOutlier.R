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
  
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, date_field)

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
  
  date_dist_tbl<-as.data.frame(date_dist_tbl)
  
  date_dist_tbl$year = year(as.Date(date_dist_tbl[,1]))
  date_dist_tbl$month = str_pad(month(as.Date(date_dist_tbl[,1])), 2, pad = "0")
  
  if(table_name=="death")
    date_dist_tbl<-date_dist_tbl %>% filter(month <'12' & month > '01')

  
  ## this table contains monthly distributions 
  date_dist_tbl_simplified<- date_dist_tbl %>%
    filter(year>= 2012) %>%
    filter(year<= year(Sys.Date())) %>%
    mutate(yyyymm = paste0(year,'-',month)) %>%
    filter(yyyymm <= paste0(year(Sys.Date() - 31),'-',month(Sys.Date() - 31))) %>%
    group_by(yyyymm) %>%
    dplyr::summarise(yyyymm_level_count = sum(date_level_count)) %>%
    dplyr::mutate(rnum = row_number()) %>%
    dplyr::mutate(next_rnum = rnum+1)
  
  print(date_dist_tbl_simplified)
  
  ## plot this table 
  describeYYMMField(date_dist_tbl_simplified%>% select(yyyymm, yyyymm_level_count)
                    , table_name, date_field, fact_type)
  
  ## this table contains deltas and the data points used for outlier detection 
  date_dist_delta<- date_dist_tbl_simplified %>% 
      inner_join(date_dist_tbl_simplified, by = c("next_rnum" = "rnum")) %>%
      dplyr::mutate(change_over_last_month = yyyymm_level_count.y- yyyymm_level_count.x) %>%
      select(yyyymm.y, change_over_last_month)

  sd_value <- sd(date_dist_delta$change_over_last_month)
  mean_value <- mean(date_dist_delta$change_over_last_month)

  lower_bound<-as.integer(round(mean_value-3*sd_value))
  upper_bound<-as.integer(round(mean_value+4*sd_value))

  if(!(is.na(lower_bound) | is.na(upper_bound))){
    png(paste(normalize_directory_path(g_config$reporting$site_directory),
            get_image_name(table_list[1],paste(field_list[1],"_",fact_type[1],sep="")),sep=""))
    hist(as.integer(date_dist_delta$change_over_last_month),col = 'blue', 
       main = paste0(field_list[1]," Difference Month-to-Month"))
    dev.off()
  }
  
  date_dist_delta$change_over_last_month<-as.integer(date_dist_delta$change_over_last_month)
  
  if(nrow(date_dist_delta)>0) 
  {
  date_dist_delta$outlier<-FALSE
  for(i in 1:nrow(date_dist_delta))
  {
    if(date_dist_delta[i,2]<lower_bound) 
      date_dist_delta[i,3]<-TRUE
    if(date_dist_delta[i,2]>upper_bound) 
      date_dist_delta[i,3]<-TRUE
  }

  df_outliers<-date_dist_delta[date_dist_delta$outlier==TRUE,c(1,2)]
  if(is.null(fact_type)) 
  outlier_message<- "list of months with unexpected changes in number of records YYYY-MM (change since last month):\n"
  else
    outlier_message<- paste0("list of months for ", field_list[1], "-",fact_type[1] ,
                             " with unexpected changes in number of records below ",floor(lower_bound), 
                             " or above ", ceiling(upper_bound), " for ",fact_type[2], 
                             " YYYY-MM (change since last month):\n")
  
  for( i in 1:nrow(df_outliers))
  {
    outlier_message<-paste(outlier_message,df_outliers[i,1], "(", df_outliers[i,2],")")
  }
  
  if(nrow(df_outliers)>0)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, outlier_message)
    # log issue 
    return(logIssue(issue_obj))
  }
  }
  NextMethod("applyCheck",theObject)
  return(c())
}

