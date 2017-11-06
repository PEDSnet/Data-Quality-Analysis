### implementation of the unexpected difference class 
MissData <- function()
{
  check_code="BA-001"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"MissData")
  return(me)
}


applyCheck.MissData <- function(theObject, table_list, field_list, con)
{
  table_name<-table_list[1]
  field_name<-field_list[1]
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
  
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  
  colnames(df_table)[2] <- "Freq"
  # identify row with null value
  new_df_table<-subset(df_table, is.na(df_table[1]))
  if(nrow(new_df_table)>0) # if there is a null value
  {
    #add a new column to this new dataframe containing the percentage frequency information rounded to 2 digits
    
    df_table$label <- as.character(
      paste(
        round(100 * df_table$Freq / sum(df_table$Freq),digits=2)
        ,'%')	# add percentage
    )
    # find the row that contains the frequency for the NA value
    na_df_table<-subset(df_table, is.na(df_table[1]))
    missing_percent<-na_df_table[1,3]
  } else missing_percent<-0
  
  if(missing_percent<check_list_entry$Lower_Threshold || missing_percent>check_list_entry$Upper_Threshold)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, field_list, missing_percent)
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

