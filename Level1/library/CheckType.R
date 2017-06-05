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