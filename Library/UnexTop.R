### implementation of the unexpected difference class 
UnexTop <- function()
{
  check_code="CB-002"
  check_entry<-get_catalog_entry(check_code)
  check_name <- check_entry$check_type
  check_alias<-check_entry$alias
  me <- CheckType(check_code,check_name, check_alias)
  
  ## Add the name for the class
  class(me) <- append(class(me),"UnexTop")
  return(me)
}


applyCheck.UnexTop <- function(theObject, table_name, field_name, metadata)
{
 count_colname<-metadata[4]
 data_filename<-metadata[5]
 error_message<-metadata[6]
 ranked_facts_previous_cycle_path<-as.character(metadata[7])
 domain<-as.character(metadata[8])
 #print(top_20_facts)
  
  ## writing to the data log file
  data_file = matrix("", ncol = 3, nrow = 20)
                 
   for (i in 1:20)
  {
     concept_id<-unlist(metadata[1])[i]
    concept_name<-unlist(metadata[2])[i]
    count<-unlist(metadata[3])[i]
    
    #datafile[i,] <-c(concept_id, concept_name, count)
    data_file[i,1]<-concept_id
    data_file[i,2]<-concept_name
    data_file[i,3]<-count
    
  }
  colnames(data_file)<-c("concept_id", "concept_name",count_colname)
  
  write.csv(data_file, file = paste(normalize_directory_path(g_config$reporting$site_directory),
                                    "./data/",data_filename,sep="")
            ,row.names=FALSE)
  
  # print(df_procedure_counts_by_visit) 
  
  ## create a list to be compared with.
  all_top_facts<-read.csv(ranked_facts_previous_cycle_path)
  colnames(all_top_facts)<-tolower(colnames(all_top_facts))
  site_column_number<-which(colnames(all_top_facts)==tolower(g_config$reporting$site))
  top_facts_other_sites<-all_top_facts[-c(1,site_column_number)]
  top_facts_other_sites_list <- unique(c(top_facts_other_sites[,1]
                                        , top_facts_other_sites[,2], top_facts_other_sites[,3]
                                        , top_facts_other_sites[,4], top_facts_other_sites[,5]))
  
  if(domain!='Drug')
  {
  concept_ancestor_tbl <- vocab_tbl(req_env$db_src, 'concept_ancestor')
  
  ## sibling concepts 
  concept_tbl <- vocab_tbl(req_env$db_src, 'concept')
  procedure_concept_tbl <- select(filter_(concept_tbl, paste0("domain_id=='",domain,"'")), concept_id, concept_name)
  procedure_concept_ancestor_tbl<-  inner_join(concept_ancestor_tbl, procedure_concept_tbl, 
                                               by = c("ancestor_concept_id" = "concept_id"))
  
  temp1<-inner_join(procedure_concept_ancestor_tbl, procedure_concept_ancestor_tbl, 
                    by =c("ancestor_concept_id"="ancestor_concept_id"))
  temp2<-filter(temp1
                , max_levels_of_separation.x==1 & max_levels_of_separation.y==1)
  sibling_concepts_tbl<-
    (select (temp2,
             descendant_concept_id.x, descendant_concept_id.y)
      )
  
  extended_list<-list()
  #print('PRINTING')
  #print(top_facts_other_sites_list)
  for (list_index in 1:length(top_facts_other_sites_list))
  {
    #print('PRINTING')
    #print(list_index)
    temp1<-select(
      filter(concept_ancestor_tbl, ancestor_concept_id==top_facts_other_sites_list[list_index]
      ), descendant_concept_id) %>% 
      dplyr::rename(final_concept_id=descendant_concept_id )
    #print('temp1')
    #print(glimpse(temp1))
    
    temp2<-select(
      filter(concept_ancestor_tbl, descendant_concept_id==top_facts_other_sites_list[list_index]
      ), ancestor_concept_id) %>% 
      dplyr::rename(final_concept_id=ancestor_concept_id)
    #print('temp2')
    #print(glimpse(temp2))
    temp<- 
      as.data.frame(dplyr::union(temp1 ,temp2))
    
    #print(temp)
    
    extended_list<-(c(extended_list, unique(temp$final_concept_id)))
    #if(list_index==2)
    #break;
  }
  
  extended_list<-unique(extended_list)
  #print('PRINTING')
  
  ## further extend by including siblings of those concepts 
  for (list_index in 1:length(top_facts_other_sites_list))
  {
    temp3<-filter(sibling_concepts_tbl, descendant_concept_id.x==top_facts_other_sites_list[list_index])
    sibling_table<- as.data.frame(select(temp3, descendant_concept_id.y))
    extended_list<-(c(extended_list, unique(sibling_table$descendant_concept_id.y))) 
    
  }  
  extended_list<-unique(extended_list)
  }
  else {
    #print('here')
    extended_list<-top_facts_other_sites_list
  }
  
  #print('PRINTING')
  issues_list<-matrix("",ncol=8, nrow=0)
  #print(data_file)
  for(row in 1:20)
  {
    ## add to descriptive report
    ## match with lists from other sites.
    if(is.element(data_file[row,1],extended_list)==FALSE
        && (data_file[row,1]!=444093) # filter out "patient status finding" concept - for procedure domain
      )
    {
        # create an issue 
        issue_obj<-Issue(theObject, table_name, field_name, paste(error_message,
                                                                  data_file[row,1],
                                                                  data_file[row,2])
                         )
        # log issue 
         issues_list<-rbind(issues_list, logIssue(issue_obj));
        
     
    }
    
  }
  NextMethod("applyCheck",theObject)
  return(issues_list)
}

