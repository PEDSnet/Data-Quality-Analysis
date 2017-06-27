### implementation of the inconsistent null distributions between source value and concept ids check
InconSource <- function()
{
  check_code="CA-014"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"InconSource")
  return(me)
}


applyCheck.InconSource<- function(theObject, table_list, field_list, con)
{
  table_name<-table_list[1]
  concept_id_field<-field_list[1]
  source_value_field<-field_list[2]
  
  check_list_entry<-get_check_entry_two_variables(theObject$check_code, table_name, concept_id_field, source_value_field)
  
  df_table<-retrieve_dataframe_group(con, g_config,table_name,source_value_field)
  colnames(df_table)[2] <- "Freq"
  # identify row with null value
  new_df_table<-subset(df_table, is.na(df_table[1]))
  if(nrow(new_df_table)>0) # if there is a null value
  {
    #add a new column to this new dataframe containing the percentage frequency information rounded to 2 digits
    
    df_table$label <- 
        round(100 * df_table$Freq / sum(df_table$Freq),digits=2)
    
    # find the row that contains the frequency for the NA value
    na_df_table<-subset(df_table, is.na(df_table[1]))
    missing_percent_source_value<-na_df_table[1,3]
  } else missing_percent_source_value<-0
  
  df_table<-retrieve_dataframe_group(con, g_config,table_name,concept_id_field)
  colnames(df_table)[2] <- "Freq"
  colnames(df_table)[1] <- "Var1"
  # identify row with NI concept value
  new_df_table<-subset(df_table, df_table$Var1==44814650)
  if(nrow(new_df_table)>0) # if there is a null value
  {
    #add a new column to this new dataframe containing the percentage frequency information rounded to 2 digits
    
    df_table$label <- 
        round(100 * df_table$Freq / sum(df_table$Freq),digits=2)
    
    # find the row that contains the frequency for the NA value
    na_df_table<-subset(df_table, df_table$Var1==44814650)
    missing_percent_concept_id<-na_df_table[1,3]
  } else missing_percent_concept_id<-0
  
   
  diff<-missing_percent_source_value -missing_percent_concept_id

  if(diff>0)
  {
    # create an issue 
    issue_obj<-Issue(theObject, table_list, paste(concept_id_field, ",",source_value_field), paste(diff, "%",sep=""))
    #print(issue_obj)
    # log issue 
    return(logIssue(issue_obj))
    
  }
  
  NextMethod("applyCheck",theObject)
  return(c())
}

