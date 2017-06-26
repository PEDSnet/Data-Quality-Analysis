### implementation of the checktype class 
Issue <- function(check_type_obj, table_list, field_list, message)
{
  
  me <- list(
    check_code = check_type_obj$check_code,
    check_name = check_type_obj$check_name, 
    check_alias = check_type_obj$check_alias, 
    table_list= table_list, 
    field_list = field_list,
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
  table_name<-theObject$table_list[1]
  #print(length(theObject$field_list))
  if(length(theObject$field_list)==0)
  {
  field_list_character<-""
  } else {
    for(i in 1:length(theObject$field_list))
  {
      if(i==1)
        field_list_character<-(theObject$field_list)[i]
      else
        field_list_character<-paste(field_list_character,(theObject$field_list)[i],sep=",")
    }
  }
  
  
  log_file_entry<-c(as.character(g_data_version),
                    as.character(table_name),
                    as.character(field_list_character),
                    as.character(theObject$check_code),
                    as.character(theObject$check_name),
                    as.character(theObject$check_alias),
                    as.character(theObject$message),
                    ' ')
  
  #print(log_file_entry)
  NextMethod("logIssue",theObject)
  #print(log_file_entry)
  return (log_file_entry);
  
}

