### implementation of the checktype class 
CheckType <- function(check_code,check_name, check_alias)
{
  
  me <- list(
    check_code = check_code,
    check_name = check_name, 
    check_alias =check_alias
  )
  
  ## Set the name for the class
  class(me) <- append(class(me),"CheckType")
  return(me)
}
applyCheck <- function(theObject, table_list, field_list, metadata, table_df, table_df2)
{
  #print("Calling the base applyCheck function")
  UseMethod("applyCheck",theObject)
}

applyCheck.default <- function(theObject, table_list, field_list, metadata, table_df, table_df2)
{
  return(theObject)
}
