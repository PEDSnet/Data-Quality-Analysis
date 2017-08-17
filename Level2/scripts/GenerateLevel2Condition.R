library(DBI)
library(yaml)
library(dplyr)
library(RPostgreSQL)

generateLevel2Condition <- function() {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work

  big_data_flag<-TRUE

  # load the configuration file
  #get path for current script
  config = yaml.load_file(g_config_path)

  #establish connection to database
  #con <- establish_database_connection_OHDSI(config)

  #con <- establish_database_connection(config)

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/Level2_Condition_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",config)

  table_name<-"condition_occurrence"
  log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/condition_occurrence_issue.csv",sep="")
  
  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:
  my_db <- src_postgres(dbname=config$db$dbname,
                        host=config$db$dbhost,
                        user =config$db$dbuser,
                        password =config$db$dbpass,
                        options=paste("-c search_path=",config$db$schema,sep=""))

  # Then reference a tbl within that src
  visit_tbl <- tbl(my_db, "visit_occurrence")
  patient_tbl<-tbl(my_db, "person")
  condition_tbl<-tbl(my_db, "condition_occurrence")
  death_tbl <- tbl(my_db, "death")

  concept_tbl <- tbl(my_db,sql(paste('SELECT * FROM ',config$db$vocab_schema,'.concept',sep='')))
  concept_ancestor_tbl <- tbl(my_db,sql(paste('SELECT * FROM ',config$db$vocab_schema,'.concept_ancestor',sep='')))
  condition_concept_tbl <- select(filter(concept_tbl, domain_id=='Condition'), concept_id, concept_name)

  patient_dob_tbl <- tbl(my_db,sql
                         ('SELECT person_id, to_date(year_of_birth||\'-\'||month_of_birth||\'-\'||day_of_birth,\'YYYY-MM-DD\') as dob FROM person'))

  ##AA009 date time inconsistency 
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('condition_start_datetime', 
                                                                                                 'condition_start_date'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('condition_end_datetime', 
                                                                                                 'condition_end_date'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  ## sibling concepts 
  condition_concept_ancestor_tbl<-  inner_join(concept_ancestor_tbl, condition_concept_tbl, 
                                               by = c("ancestor_concept_id" = "concept_id"))
  
  temp1<-inner_join(condition_concept_ancestor_tbl, condition_concept_ancestor_tbl, 
                    by =c("ancestor_concept_id"="ancestor_concept_id"))
  temp2<-filter(temp1
                , max_levels_of_separation.x==1 & max_levels_of_separation.y==1)
  #print(head(temp2))
  sibling_concepts_tbl<-
    (select (temp2,
      descendant_concept_id.x, descendant_concept_id.y)
    )
 
 #print(head(sibling_concepts_tbl))
  ### Print top 100 no matching concept source values in condition table 
  condition_no_match<- select( filter(condition_tbl, condition_concept_id==0)
                               , condition_source_value)
  
  no_match_condition_counts <-
    filter(
      arrange(
       summarize(
          group_by(condition_no_match, condition_source_value)
          , count=n())
        , desc(count))
      , row_number()>=1 & row_number()<=100) ## printing top 100
  
  df_no_match_condition_counts<-as.data.frame(
    no_match_condition_counts
    )
  
  ## writing to the issue log file
  data_file<-data.frame(concept_id=character(0), concept_name=character(0), condition_source_value=character(0))
  
  data_file<-rbind(df_no_match_condition_counts)
  if(nrow(df_no_match_condition_counts)>0)
  {
  colnames(data_file)<-c("condition_source_value","occurrence_counts")
  write.csv(data_file, file = paste(normalize_directory_path( g_config$reporting$site_directory),
                                    "./data/no_match_conditions.csv",sep="")
            ,row.names=FALSE)
  
  }
 #print(head(df_no_match_condition_counts))
  
  ### implementation of unexpected top inpatient conditions check 
  #filter by inpatient and outpatient visits and select visit occurrence id column
  inpatient_visit_tbl<-select(filter(visit_tbl,
                                     (visit_concept_id==9201
                                      | visit_concept_id==2000000048) & !is.na(visit_end_date)
  )
  ,visit_occurrence_id,visit_start_date, visit_end_date)


  
  
  ##### Printing top 100 inpatient visits ##### 
  condition_tbl_enhanced<- distinct(select(inner_join(condition_concept_tbl,condition_tbl, by = c("concept_id"="condition_concept_id"))
                                  , visit_occurrence_id, condition_concept_id, concept_name))
  #print(head(condition_tbl_enhanced))
  #print(head(inpatient_visit_tbl))
  
  inpatient_visit_conditions<-
    distinct(select(
      inner_join(condition_tbl_enhanced, 
                                         inpatient_visit_tbl)#, by =c("visit_occurrence_id", "visit_occurrence_id"))
        ,visit_occurrence_id,concept_name, condition_concept_id))

  #print(head(inpatient_visit_conditions))
  
  condition_counts_by_visit <-
    filter(
      arrange(
       summarize(
          group_by(inpatient_visit_conditions, condition_concept_id)
          , visit_count=n())
        , desc(visit_count))
     , row_number()>=1 & row_number()<=100) ## printing top 100
  
  df_condition_counts_by_visit_all<-as.data.frame(
    arrange(distinct(
      select(inner_join(condition_counts_by_visit, condition_tbl_enhanced, 
                        by = c("condition_concept_id"="condition_concept_id"))
             ,condition_concept_id, concept_name, visit_count)
      ), desc(visit_count)
      ))
      
  #print(df_condition_counts_by_visit_all)
  
  ## writing to the issue log file
  data_file<-data.frame(concept_id=character(0), concept_name=character(0), visit_counts=character(0))
  
  data_file<-rbind(df_condition_counts_by_visit_all)
  colnames(data_file)<-c("concept_id", "concept_name","visit_counts")
  write.csv(data_file, file = paste(normalize_directory_path( g_config$reporting$site_directory),"./data/top_inpatient_conditions.csv",sep="")
            ,row.names=FALSE)
  
  
  ###### Identifying outliers in top inpatient conditions 
  #inpatient_visit_df<-as.data.frame(inpatient_visit_tbl)
  inpatient_visit_gte_2days_tbl<-select(filter(inpatient_visit_tbl, visit_end_date - visit_start_date >=2)
                                        , visit_occurrence_id)

  
  temp_join <- inner_join(condition_concept_tbl,condition_tbl, by = c("concept_id"="condition_concept_id"))

  condition_tbl_restricted <- select (temp_join, visit_occurrence_id, concept_id, concept_name)

  condition_visit_join_tbl <- distinct(
    select (
      inner_join(inpatient_visit_gte_2days_tbl,
                 condition_tbl_restricted,
                 by = c("visit_occurrence_id" = "visit_occurrence_id"))
      ,visit_occurrence_id, concept_id, concept_name)
  )


  condition_counts_by_visit <-
    filter(
      arrange(
       summarize(
          group_by(condition_visit_join_tbl, concept_id)
          , count=n())
        , desc(count))
      , row_number()>=1 & row_number()<=20) ## look at top 20

  df_condition_counts_by_visit<-as.data.frame(
    select(
      inner_join(condition_counts_by_visit, condition_concept_tbl,
                 by=c("concept_id"="concept_id"))
      , concept_id, concept_name, count)
  )

  ## create a list to be compared with.
  all_top_conditions<-read.csv(g_top50_inpatient_conditions_path)
  colnames(all_top_conditions)<-tolower(colnames(all_top_conditions))
  site_column_number<-which(colnames(all_top_conditions)==tolower(config$reporting$site))
  top_conditions_other_sites<-all_top_conditions[-c(1,site_column_number)]
  top_cond_other_sites_list <- unique(c(top_conditions_other_sites[,1]
                                        , top_conditions_other_sites[,2], top_conditions_other_sites[,3]
                                        , top_conditions_other_sites[,4], top_conditions_other_sites[,5]))

  extended_list<-list()
  for (list_index in 1:length(top_cond_other_sites_list))
  {

    temp<- as.data.frame(union(
      select(
        filter(concept_ancestor_tbl, ancestor_concept_id==top_cond_other_sites_list[list_index]
        ), descendant_concept_id)
      ,
      select(
        filter(concept_ancestor_tbl, descendant_concept_id==top_cond_other_sites_list[list_index]
        ), ancestor_concept_id)
    ))
    extended_list<-(c(extended_list, unique(temp$descendant_concept_id)))
    #if(list_index==2)
    #break;
  }

  extended_list<-unique(extended_list)

  ## further extend by including siblings of those concepts 
  for (list_index in 1:length(top_cond_other_sites_list))
  {
    temp3<-filter(sibling_concepts_tbl, descendant_concept_id.x==top_cond_other_sites_list[list_index])
    sibling_table<- as.data.frame(select(temp3, descendant_concept_id.y))
    extended_list<-(c(extended_list, unique(sibling_table$descendant_concept_id.y))) 
    
  }  
  extended_list<-unique(extended_list)
  
  fileContent<-c(fileContent,"##Inpatient Conditions")
  for(row in 1:20)
  {
    ## add to descriptive report
    fileContent<-c(fileContent,paste(
      df_condition_counts_by_visit[row,1],
      df_condition_counts_by_visit[row,2],"(count=",
      df_condition_counts_by_visit[row,3],")","\n"))
    ## match with lists from other sites.
    # flog.info("Testing for : ")
    # flog.info(df_condition_counts_by_visit[row,1])
    if(is.element(df_condition_counts_by_visit[row,1],extended_list)==FALSE)
    {
      # flog.info("HERE")
      ### open the person log file for appending purposes.
      log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/condition_occurrence_issue.csv",sep="")
      log_entry_content<-(read.csv(log_file_name))

      log_entry_content<-custom_rbind(log_entry_content,
                                      apply_check_type_1('CB-002', "condition_concept_id",
                                                         paste("outlier inpatient condition:",df_condition_counts_by_visit[row,1],df_condition_counts_by_visit[row,2]), table_name, g_data_version)
      )
      # flog.info(df_condition_counts_by_visit[row,1])
      write.csv(log_entry_content, file = log_file_name
                ,row.names=FALSE)

      #break;
    }

  }

  
  
  ### printing top 100 outpatient conditions by patient counts
  outpatient_visit_tbl<-select(filter(visit_tbl,visit_concept_id==9202)
                               ,visit_occurrence_id, person_id)
  
  #condition_tbl_enhanced<- distinct(select(inner_join(concept_tbl,condition_tbl, by = c("concept_id"="condition_concept_id"))
   #                                        , visit_occurrence_id, condition_concept_id, concept_name))
  #print(head(condition_tbl_enhanced))
  #print(head(inpatient_visit_tbl))
  outpatient_visit_conditions<-
    distinct(select(
      inner_join(condition_tbl_enhanced, 
                 outpatient_visit_tbl)#, by =c("visit_occurrence_id", "visit_occurrence_id"))
      ,person_id,concept_name, condition_concept_id))
  
  condition_counts_by_patients <-
    filter(
      arrange(
       summarize(
          group_by(outpatient_visit_conditions, condition_concept_id)
          , pt_count=n())
        , desc(pt_count))
      , row_number()>=1 & row_number()<=100) ## printing top 100
  
  df_condition_counts_by_visit_all<-as.data.frame(
    arrange(distinct(
      select(inner_join(condition_counts_by_patients, condition_tbl_enhanced, 
                        by = c("condition_concept_id"="condition_concept_id"))
             ,condition_concept_id, concept_name, pt_count)
    ), desc(pt_count)
    ))
  
  #print(df_condition_counts_by_visit_all)
  
  ## writing to the issue log file
  data_file<-data.frame(concept_id=character(0), concept_name=character(0), visit_counts=character(0))
  
  data_file<-rbind(df_condition_counts_by_visit_all)
  colnames(data_file)<-c("concept_id", "concept_name","pt_counts")
  write.csv(data_file, file = paste(normalize_directory_path( g_config$reporting$site_directory),
                                    "./data/top_outpatient_conditions.csv",sep="")
            ,row.names=FALSE)
  
  ### implementation of unexpected top outpatient conditions check 
 
  
  out_condition_visit_join_tbl <- distinct(
    select (
      inner_join(outpatient_visit_tbl,
                 condition_tbl_restricted,
                 by = c("visit_occurrence_id" = "visit_occurrence_id"))
      ,person_id, concept_id, concept_name)
  )
  
  
  out_condition_counts_by_person <-
    filter(
      arrange(
       summarize(
          group_by(out_condition_visit_join_tbl, concept_id)
          , count=n())
        , desc(count))
      , row_number()>=1 & row_number()<=20) ## look at top 20
  
  df_out_condition_counts_by_person<-as.data.frame(
    select(
      inner_join(out_condition_counts_by_person, condition_concept_tbl,
                 by=c("concept_id"="concept_id"))
      , concept_id, concept_name, count)
  )
  
  ## create a list to be compared with.
  all_top_conditions<-read.csv(g_top50_outpatient_conditions_path)
  #print(all_top_conditions)
  colnames(all_top_conditions)<-tolower(colnames(all_top_conditions))
  site_column_number<-which(colnames(all_top_conditions)==tolower(config$reporting$site))
  top_conditions_other_sites<-all_top_conditions[-c(1,site_column_number)]
  top_cond_other_sites_list <- unique(c(top_conditions_other_sites[,1]
                                        , top_conditions_other_sites[,2], top_conditions_other_sites[,3]
                                        , top_conditions_other_sites[,4], top_conditions_other_sites[,5]))
  #print(top_cond_other_sites_list)
  extended_list<-list()
  for (list_index in 1:length(top_cond_other_sites_list))
  {
    
    parent_child_table<- as.data.frame(union(
      select(
        filter(concept_ancestor_tbl, ancestor_concept_id==top_cond_other_sites_list[list_index]
        ), descendant_concept_id)
      ,
      select(
        filter(concept_ancestor_tbl, descendant_concept_id==top_cond_other_sites_list[list_index]
        ), ancestor_concept_id)
    ))
    extended_list<-(c(extended_list, unique(parent_child_table$descendant_concept_id)))
    #if(list_index==2)
    #break;
  }
  
  extended_list<-unique(extended_list)
  #print(match(254761,extended_list)
  #      )
  #print(extended_list)
  
  ## further extend by including siblings of those concepts 
  for (list_index in 1:length(top_cond_other_sites_list))
  {
     temp3<-filter(sibling_concepts_tbl, descendant_concept_id.x==top_cond_other_sites_list[list_index])
    sibling_table<- as.data.frame(select(temp3, descendant_concept_id.y))
    extended_list<-(c(extended_list, unique(sibling_table$descendant_concept_id.y))) 
   
  }  
  extended_list<-unique(extended_list)
  #print(match(254761,extended_list)
  #)
  
  fileContent<-c(fileContent,"##Outpatient Conditions")
  for(row in 1:20)
  {
    ## add to descriptive report
    fileContent<-c(fileContent,paste(
      df_out_condition_counts_by_person[row,1],
      df_out_condition_counts_by_person[row,2],"(count=",
      df_out_condition_counts_by_person[row,3],")","\n"))
    ## match with lists from other sites.
    # flog.info("Testing for : ")
    # flog.info(df_condition_counts_by_visit[row,1])
    if(is.element(df_out_condition_counts_by_person[row,1],extended_list)==FALSE
       && df_out_condition_counts_by_person[row,1]!=444093) # filter out "patient status finding" concept
    {
      # flog.info("HERE")
      ### open the person log file for appending purposes.
      log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/condition_occurrence_issue.csv",sep="")
      log_entry_content<-(read.csv(log_file_name))
      
      log_entry_content<-custom_rbind(log_entry_content,
                                      apply_check_type_1('CB-002', "condition_concept_id",
                                                         paste("outlier outpatient condition:",
                                                               df_out_condition_counts_by_person[row,1],
                                                               df_out_condition_counts_by_person[row,2]), 
                                                        table_name, g_data_version)
      )
      # flog.info(df_condition_counts_by_visit[row,1])
      write.csv(log_entry_content, file = log_file_name
                ,row.names=FALSE)
      
      #break;
    }
    
  }

  fileContent<-c(fileContent,"##Unexpected Events")
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(PreBirth(), c(table_name, "person"), c('condition_start_date', 
                                                                                                      'time_of_birth'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  

  table_name<-"condition_occurrence"
  ## Temporal checks --- facts after death date
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(PostDeath(), c(table_name, "death"), c('condition_start_date', 
                                                                                                      'death_date'),my_db)) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  


  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  #close the connection
  #close_database_connection_OHDSI(con,config)
}
