

get_top_concepts<-function(typed_visit_tbl,fact_tbl, fact_concept_id, fact_id, title, concept_tbl)
{
  fileContent<-c(paste("##", title))
  # flog.info("1")
  join_table<-inner_join(fact_tbl,typed_visit_tbl)

  #join_table<-summarize(inner_join(fact_tbl,typed_visit_tbl), n=n())
  # flog.info(fact_concept_id)
  column_index_1 <- which(colnames(join_table)==fact_concept_id)
  column_index_2 <- which(colnames(join_table)==fact_id)
  typed_facts<-select(join_table, column_index_1,column_index_2)
  # flog.info(head(typed_facts))
  typed_facts_count<-as.data.frame(summarize(typed_facts,n=n()))[1,1]
  # flog.info(typed_facts_count)
  #column_index_3 <- which(colnames(typed_facts)==fact_concept_id)
  # flog.info(column_index_3)
  #head(typed_facts)
  fact_concept_id<-as.name(fact_concept_id)
  # flog.info(fact_concept_id)

  if(grepl("Observations",title)==TRUE)
  {

  typed_facts_freq<-arrange(group_by(typed_facts,observation_concept_id)%>%
                                         summarise(freq = count(fact_id))
                                       , desc(freq))

  top_15_typed_facts<-filter(typed_facts_freq,row_number()>=1 & row_number()<=15)
  df_typed_facts<-as.data.frame(select(inner_join(top_15_typed_facts
                                                  ,concept_tbl, by=c(observation_concept_id= "concept_id"))
                                     ,observation_concept_id,concept_name,freq)
                      )
  # flog.info(df_typed_facts)
  }
  if(grepl("Conditions",title)==TRUE)
  {
    typed_facts_freq<-arrange(group_by(typed_facts,condition_concept_id)%>%
                                summarise(freq = count(fact_id))
                              , desc(freq))
    top_15_typed_facts<-filter(typed_facts_freq,row_number()>=1 & row_number()<=15)
    df_typed_facts<-as.data.frame(select(inner_join(top_15_typed_facts
                                                    ,concept_tbl, by=c(condition_concept_id= "concept_id"))
                                         ,condition_concept_id,concept_name,freq)
    )
  }
  if(grepl("Procedures",title)==TRUE)
  {
    typed_facts_freq<-arrange(group_by(typed_facts,procedure_concept_id)%>%
                                summarise(freq = count(fact_id))
                              , desc(freq))
    top_15_typed_facts<-filter(typed_facts_freq,row_number()>=1 & row_number()<=15)
    df_typed_facts<-as.data.frame(select(inner_join(top_15_typed_facts
                                                    ,concept_tbl, by=c(procedure_concept_id= "concept_id"))
                                         ,procedure_concept_id,concept_name,freq)
    )
  }
  if(grepl("Medications",title)==TRUE)
  {
    typed_facts_freq<-arrange(group_by(typed_facts,drug_concept_id)%>%
                                summarise(freq = count(fact_id))
                              , desc(freq))
    top_15_typed_facts<-filter(typed_facts_freq,row_number()>=1 & row_number()<=15)
    df_typed_facts<-as.data.frame(select(inner_join(top_15_typed_facts
                                                    ,concept_tbl, by=c(drug_concept_id= "concept_id"))
                                         ,drug_concept_id,concept_name,freq)
    )
  }
  if(grepl("Labs",title)==TRUE)
  {
    typed_facts_freq<-arrange(group_by(typed_facts,measurement_concept_id)%>%
                                summarise(freq = count(fact_id))
                              , desc(freq))
    top_15_typed_facts<-filter(typed_facts_freq,row_number()>=1 & row_number()<=15)
    df_typed_facts<-as.data.frame(select(inner_join(top_15_typed_facts
                                                    ,concept_tbl, by=c(measurement_concept_id= "concept_id"))
                                         ,measurement_concept_id,concept_name,freq)
    )
  }
  for(i in 1:nrow(df_typed_facts))
  {
    #print conceptname, id, count and %
    fileContent<-c(fileContent,paste("\n",df_typed_facts[i,2],"(concept_id:",df_typed_facts[i,1],")"
                                     , "; count=",df_typed_facts[i,3],"("
                                     ,round(df_typed_facts[i,3]*100/typed_facts_count,2),"%)"))
  }

  return(fileContent)

}

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}
