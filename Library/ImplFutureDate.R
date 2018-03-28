### implementation of the unexpected difference class 
ImplFutureDate <- function()
{
  check_code="CA-001"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"ImplFutureDate")
  return(me)
}


applyCheck.ImplFutureDate <- function(theObject, table_list, field_list, con)
{
  table_name<-table_list[1]
  field_name<-field_list[1]
  check_list_entry<-get_check_entry_one_variable(theObject$check_code, table_name, field_name)
  
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  
  df_table<-subset(df_table,!is.na(df_table[,1]))
  if(nrow(df_table)>0)
  {
    # df table is actually a dataframe of two dataframes
    colnames(df_table)[1] <- "Var1"
    colnames(df_table)[2] <- "Freq"
    
    # remove NA values - we dont want to plot missing values
    #df_table<-subset(df_table,!is.na(Var1))
    
    df_table$Var1<-as.Date(df_table[,1])
    #df_table$Var1 <- as.factor(df_table$Var1)
    df_table$Var1 <- as.character(df_table$Var1)
    
    # aggregate df_table again by summing frequency for all equivalent dates
    df_table<-aggregate(df_table$Freq,FUN=sum, by = list(df_table$Var1))
    colnames(df_table)[1] <- "Var1"
    colnames(df_table)[2] <- "Freq"
 
    comparison_date<-Sys.Date()
    
    if(table_name == 'death') ## due to the imputation rule in PEDSnet
    {
      d = as.POSIXlt(Sys.Date())
      d$mon = d$mon + 1
      d$mday = 0
      comparison_date<- as.Date(d)
    } 
      if(comparison_date<max(df_table$Var1))
      {
    # create an issue 
      issue_obj<-Issue(theObject, table_list, field_list, max(df_table$Var1))
    #print(issue_obj)
    # log issue 
      return(logIssue(issue_obj))
    
      }
    
  }
  NextMethod("applyCheck",theObject)
  return(c())
}

