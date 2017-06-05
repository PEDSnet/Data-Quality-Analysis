### implementation of the checktype class 
Issue <- function(check_type_obj, table1, table2, field1, field2, message)
{
  
  me <- list(
    check_code = check_type_obj$check_code,
    check_name = check_type_obj$check_name, 
    check_alias = check_type_obj$check_alias, 
    table1 = table1, 
    table2=table2, 
    field1=field1, 
    field2=field2, 
    message = message
  )
  
  ## Set the name for the class
  class(me) <- append(class(me),"Issue")
  return(me)
}

logIssue <- function(theObject)
{
  #print("Calling the base applyCheck function")
  UseMethod("logIssue",theObject)
}

logIssue.default <- function(theObject)
{
  #print(noquote(paste("Well, this is awkward. Just make",
  #                    getFavoriteBreakfast(theObject))))
  
  # flog.info(log_file_entry)
}

logIssue.Issue <- function(theObject)
{
  log_file_entry<-c(as.character(g_data_version),
                    as.character(theObject$table1),
                    "",
                    as.character(theObject$check_code),
                    as.character(theObject$check_name),
                    as.character(theObject$check_alias),
                    as.character(theObject$message),
                    ' ')
  
  #print("fdfdfdf")
  NextMethod("logIssue",theObject)
  #print(log_file_entry)
  return (log_file_entry);
  
}

