library(DBI)
library(yaml)
library(dplyr)
library(RPostgreSQL)

generateLevel2Observation <- function () {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work

  big_data_flag<-TRUE

  # load the configuration file
  #get path for current script
  config = yaml.load_file(g_config_path)

  #establish connection to database
  #con <- establish_database_connection_OHDSI(config)

  #con <- establish_database_connection(config)

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/Level2_ObservationAutomatic.md",sep=""))
  fileContent <-get_report_header("Level 2",config)


  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:
  my_db <- src_postgres(dbname=config$db$dbname,
                        host=config$db$dbhost,
                        user =config$db$dbuser,
                        password =config$db$dbpass,
                        options=paste("-c search_path=",config$db$schema,sep=""))

  # Then reference a tbl within that src
  observation_tbl <- tbl(my_db, "observation")
  visit_tbl <- tbl(my_db, "visit_occurrence")
  concept_tbl <- tbl(my_db, dplyr::sql(paste('SELECT * FROM ',config$db$vocab_schema,'.concept',sep='')))

  patient_dob_tbl <- tbl(my_db, dplyr::sql
                         ('SELECT person_id, to_date(year_of_birth||\'-\'||month_of_birth||\'-\'||day_of_birth,\'YYYY-MM-DD\') as dob FROM person'))


  #filter by inpatient and outpatient visits and select visit occurrence id column
  inpatient_visit_tbl<-select(filter(visit_tbl, visit_concept_id==9201),visit_occurrence_id)
  outpatient_visit_tbl<-select(filter(visit_tbl, visit_concept_id==9202),visit_occurrence_id)

  fileContent<-c(fileContent,
                 get_top_concepts(inpatient_visit_tbl,observation_tbl, "observation_concept_id", "observation_id", "Inpatient Observations", concept_tbl))
  fileContent<-c(fileContent,
                 get_top_concepts(outpatient_visit_tbl,observation_tbl, "observation_concept_id", "observation_id", "Outpatient Observations", concept_tbl))




  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  #close the connection
  #close_database_connection_OHDSI(con,config)
}
