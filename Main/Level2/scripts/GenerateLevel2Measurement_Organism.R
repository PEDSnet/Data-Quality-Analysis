library(DBI)
library(yaml)
library(dplyr)

generateLevel2MeasurementOrganism <- function () {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work

  big_data_flag<-TRUE

  # load the configuration file
  #config = yaml.load_file(g_config_path)

  #establish connection to database
  #con <- establish_database_connection_OHDSI(config)

  #con <- establish_database_connection(config)

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(g_config$reporting$site_directory),"./reports/Level2_Measurement_Organism_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",g_config)

  
  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:

            
  # Then reference a tbl within that src

  measurement_organism_tbl <- cdm_tbl(req_env$db_src, "measurement_organism")

  concept_tbl <- vocab_tbl(req_env$db_src, 'concept')

  ### Print top 100 no matching concept source values in measurement org table 
  measurement_organism_no_match<- select( filter(measurement_organism_tbl, organism_concept_id==0)
                                                             , organism_source_value,meas_organism_id)

  no_match_measurement_organism_counts <-
    filter(
      dplyr::arrange(
        dplyr::summarize(
          group_by(measurement_organism_no_match, organism_source_value)
          , count=n())
        , desc(count))
      , row_number()>=1 & row_number()<=100) ## printing top 100
 
  
  df_no_match_measurement_organism_counts<-as.data.frame(
    no_match_measurement_organism_counts
  )

  
  #print(df_no_match_measurement_counts)
  if(nrow(df_no_match_measurement_organism_counts)>0)
  {
  ## writing to the issue log file
  data_file<-data.frame(organism_source_value=character(0), counts=character(0))
  
  data_file<-rbind(df_no_match_measurement_organism_counts)
  colnames(data_file)<-c("organism_source_value","occurrence_counts")
  write.csv(data_file, file = paste(normalize_directory_path( g_config$reporting$site_directory),
                                    "./data/no_match_measurement_organism.csv",sep="")
            ,row.names=FALSE)
  }
  
  ##### Printing top 100 vital measurements ##### 
  #print("here")
  
  measurement_organism_tbl_enhanced<- distinct(select(inner_join(concept_tbl,measurement_organism_tbl, by = c("concept_id"="organism_concept_id"))
                                           ,meas_organism_id, concept_id, concept_name))
  #head(vital_tbl_enhanced)
  
  
  measurement_organism_counts <-
    filter(
      dplyr::arrange(
        dplyr::summarize(
          group_by(measurement_organism_tbl_enhanced, concept_id)
          , occurrence_counts=n())
        , desc(occurrence_counts))
      , row_number()>=1 & row_number()<=100) ## printing top 100
  
  #print(head(measurement_organism_counts))
  
  df_measurement_organism_counts_all<-as.data.frame(
    dplyr::arrange(distinct(
      select(inner_join(measurement_organism_counts, measurement_organism_tbl_enhanced, 
                        by = c("concept_id"="concept_id"))
             ,concept_id, concept_name, occurrence_counts)
    ), desc(occurrence_counts)
    ))
  
  #print(df_measurement_organism_counts_all)
  #print(df_lab_counts)
  
  ## writing to the issue log file
  data_file<-data.frame(concept_id=character(0), concept_name=character(0), occurrence_counts=character(0))
  
  data_file<-rbind(df_measurement_organism_counts_all)
  colnames(data_file)<-c("concept_id", "concept_name","occurrence_counts")
  write.csv(data_file, file = paste(normalize_directory_path( g_config$reporting$site_directory),
                                    "./data/measurement_organisms.csv",sep="")
            ,row.names=FALSE)
  


  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  #close the connection
  #close_database_connection_OHDSI(con,config)
}
