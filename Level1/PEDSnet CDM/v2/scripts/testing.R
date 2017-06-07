buildArgs<-function(tables) {
  dict<-new.env()
  for (table in tables) {
    dict[[paste("table_", table, sep="")]] = table    
  }
 return(dict)
}