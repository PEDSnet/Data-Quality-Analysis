### implementation of the inconsistent date / time check
InconDateTime <- function()
{
  check_code="AA-009"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"InconDateTime")
  return(me)
}


applyCheck.InconDateTime<- function(theObject, table_list, field_list)
{
  table_name<-table_list[1]
  time_field<-field_list[1]
  date_field<-field_list[2]
  
  check_list_entry<-get_check_entry_two_variables(theObject$check_code, table_name, time_field, date_field)

  if(grepl('year', date_field)==TRUE)
  {
  
  mismatch_date_tbl <- tbl(req_env$db_src, dplyr::sql(paste('SELECT * FROM ',g_config$db$schema,'.',table_name,
                                                        " WHERE extract(year from ",time_field,") <> ",date_field,sep=''))
  )
  }
  
  if(grepl('month',date_field)==TRUE)
  {
    
    mismatch_date_tbl <- tbl(req_env$db_src, dplyr::sql(paste('SELECT * FROM ',g_config$db$schema,'.',table_name,
                                                     " WHERE extract(month from ",time_field,") <> ",date_field,sep=''))
    )
  } 
  
  if(grepl('day',date_field)==TRUE)
  {
    
    mismatch_date_tbl <- tbl(req_env$db_src, dplyr::sql(paste('SELECT * FROM ',g_config$db$schema,'.',table_name,
                                                     " WHERE extract(day from ",time_field,") <> ",date_field,sep=''))
    )
  }

  #Note: removed distinct.
  #Removed lubridate::date() function since it's comparing sql with R datatypes
  if(grepl('date', date_field)==TRUE) 
  {
   mismatch_date_tbl <- 
     cdm_tbl(req_env$db_src, table_name) %>%
      select(date_field, time_field) %>%
     rename_(time_field = time_field) %>% 
     mutate(time_field = sql('cast("time_field" as date)')) %>%
     filter_(paste0(date_field,'!=time_field')) %>%
     as.data.frame() 

   #Double check by removing timestamps
   mismatch_date_tbl[,1] = as.Date(mismatch_date_tbl[,1])
   mismatch_date_tbl[,2] = as.Date(mismatch_date_tbl[,2])

   mismatch_date_tbl <- mismatch_date_tbl %>%
     filter_(paste0(date_field,'!=time_field')) 
  }

  df_incon<-as.data.frame(mismatch_date_tbl)
  
 
  if(nrow(df_incon)>0)
  {
    issue_obj<-Issue(theObject, table_list, field_list, nrow(df_incon))

    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

