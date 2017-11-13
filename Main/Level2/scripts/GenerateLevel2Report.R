library(DBI)
library(yaml)
library(dplyr)
library(RPostgreSQL)

generateLevel2Report <- function () {
  #detach("package:plyr", unload=TRUE) # otherwise dplyr's group by , summarize etc do not work

  big_data_flag<-TRUE

  # load the configuration file
  #get path for current script
  config = yaml.load_file(g_config_path)

  #establish connection to database
  #con <- establish_database_connection_OHDSI(config)

  #con <- establish_database_connection(config)

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/Level2_Report_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",config)


  # Connection basics ---------------------------------------------------------
  # To connect to a database first create a src:
  my_db <- src_postgres(dbname=config$db$dbname,
                        host=config$db$dbhost,
                        user =config$db$dbuser,
                        password =config$db$dbpass,
                        options=paste("-c search_path=",config$db$schema,sep=""))

  # Then reference a tbl within that src
  observation_period_tbl <- tbl(my_db, "observation_period")
  observation_tbl <- tbl(my_db, "observation")
  visit_tbl <- tbl(my_db, "visit_occurrence")
  patient_tbl<-tbl(my_db, "person")
  condition_tbl<-tbl(my_db, "condition_occurrence")
  procedure_tbl<-tbl(my_db, "procedure_occurrence")
  drug_tbl <- tbl(my_db, "drug_exposure")
  measurement_tbl <- tbl(my_db, "measurement")
  death_tbl <- tbl(my_db, "death")

  concept_tbl <- tbl(my_db, dplyr::sql('SELECT * FROM vocabulary.concept'))

  patient_dob_tbl <- tbl(my_db, dplyr::sql
                         ('SELECT person_id, to_date(year_of_birth||\'-\'||month_of_birth||\'-\'||day_of_birth,\'YYYY-MM-DD\') as dob FROM person'))

  ## total patients
  all_patients<-select(patient_tbl,person_id)

  ## Patients  satisfying inclusion criteria
  valid_patients_by_visit<-select(filter(visit_tbl,visit_start_date >='2009-01-01'
                                  & (visit_concept_id ==9201
                                    |visit_concept_id== 9202
                                    |visit_concept_id== 9203
                                    |visit_concept_id== 42898160
                                    |visit_concept_id==44814711
                                    |visit_concept_id==44814710
                                    |visit_concept_id==2000000048)
                                    ), person_id
                                  )

  valid_patients_by_condition<-select(filter(condition_tbl,condition_start_date >='2009-01-01'),person_id)

  #patients not satisfying inclusion criteria
  invalid_patients<-setdiff(all_patients,intersect(valid_patients_by_visit,valid_patients_by_condition))
  df_invalid_patients<-as.data.frame(invalid_patients)
  fileContent<-c(fileContent,"##Inclusion Criteria Check")
  fileContent<-c(fileContent,paste(nrow(df_invalid_patients)," patients found outside the inclusion criteria"))

  if(nrow(df_invalid_patients)>0)
  {
    ### open the person log file for appending purposes.
    #log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/person_issue.csv",sep="")
    #log_entry_content<-apply_check_type_1('G1-001', "person_id", paste(nrow(df_invalid_patients)," patients found outside the inclusion criteria"), table_name)
    #write(log_entry_content,file=log_file_name,append=TRUE)
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/person_issue.csv",sep="")
    log_entry_content<-custom_rbind(log_entry_content,
                              apply_check_type_1('G1-001', "person_id", paste(nrow(df_invalid_patients)," patients found outside the inclusion criteria"), table_name))
    write(log_entry_content,file=log_file_name,append=TRUE)
  }

  # difference between observation period and person table
  df_result<-as.data.frame(setdiff(all_patients,select(observation_period_tbl,person_id)))
  if(nrow(df_result)>0)
  {
    fileContent<-c("\n",fileContent,paste(nrow(df_result)," different person_ids found between person and observation_period"))
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/person_issue.csv",sep="")
    log_entry_content<-custom_rbind(log_entry_content,apply_check_type_1('G1-001', "person_id",
                                          paste(nrow(df_result)," different person_ids found between person and observation_period"), table_name)
    )
    write(log_entry_content,file=log_file_name,append=TRUE)

    }

  #filter by inpatient and outpatient visits and select visit occurrence id column
  inpatient_visit_tbl<-select(filter(visit_tbl, visit_concept_id==9201),visit_occurrence_id)
  outpatient_visit_tbl<-select(filter(visit_tbl, visit_concept_id==9202),visit_occurrence_id)

  fileContent<-c(fileContent,
                 get_top_concepts(inpatient_visit_tbl,observation_tbl, "observation_concept_id", "observation_id", "Inpatient Observations", concept_tbl))
  fileContent<-c(fileContent,
                 get_top_concepts(outpatient_visit_tbl,observation_tbl, "observation_concept_id", "observation_id", "Outpatient Observations", concept_tbl))

  fileContent<-c(fileContent,
                 get_top_concepts(inpatient_visit_tbl,condition_tbl, "condition_concept_id", "condition_occurrence_id", "Inpatient Conditions", concept_tbl))
  fileContent<-c(fileContent,
                 get_top_concepts(outpatient_visit_tbl,condition_tbl, "condition_concept_id", "condition_occurrence_id", "Outpatient Conditions", concept_tbl))

  fileContent<-c(fileContent,
                 get_top_concepts(inpatient_visit_tbl,procedure_tbl, "procedure_concept_id", "procedure_occurrence_id", "Inpatient Procedures", concept_tbl))
  fileContent<-c(fileContent,
                 get_top_concepts(outpatient_visit_tbl,procedure_tbl, "procedure_concept_id", "procedure_occurrence_id", "Outpatient Procedures", concept_tbl))


  fileContent<-c(fileContent,
                 get_top_concepts(inpatient_visit_tbl,drug_tbl, "drug_concept_id", "drug_exposure_id", "Inpatient Medications", concept_tbl))
  fileContent<-c(fileContent,
                 get_top_concepts(outpatient_visit_tbl,drug_tbl, "drug_concept_id", "drug_exposure_id", "Outpatient Medications", concept_tbl))

  labs<-filter(measurement_tbl,measurement_type_concept_id==44818702)

  fileContent<-c(fileContent,
                 get_top_concepts(inpatient_visit_tbl,labs, "measurement_concept_id", "measurement_id", "Inpatient Labs", concept_tbl))
  fileContent<-c(fileContent,
                 get_top_concepts(outpatient_visit_tbl,labs, "measurement_concept_id", "measurement_id", "Outpatient Labs", concept_tbl))






  fileContent<-c(fileContent,"##Facts occurring before patient's birth")

  table_name<-"visit_occurrence"
  ## Temporal checks --- facts before birth date
  df_visit_before_dob<-as.data.frame(
    select(
      filter(inner_join(visit_tbl,patient_dob_tbl, by =c("person_id"="person_id")),visit_start_date<dob)
      ,person_id, dob, visit_start_date
    ))
  
  if(nrow(df_visit_before_dob)>0)
  {
  df_visit_prenatal<-subset(df_visit_before_dob,elapsed_months(dob,visit_start_date)<=9)
  message<-paste(nrow(df_visit_before_dob)
                 ,"visits before birth ( including",nrow(df_visit_prenatal),"prenatal visits)");
  fileContent<-c(fileContent,"\n",message)
  ### open the person log file for appending purposes.
  log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/visit_occurrence_issue.csv",sep="")
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,
                                  apply_check_type_1('G2-003', "visit_start_date", message, table_name)
                      )
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)

  }

  table_name<-"condition_occurrence"
  df_cond_before_dob<-as.data.frame(
    select(
      filter(inner_join(condition_tbl,patient_dob_tbl, by =c("person_id"="person_id")),condition_start_date<dob)
      ,person_id, dob, condition_start_date
    ))

  if(nrow(df_cond_before_dob)>0)
  {
    df_cond_prenatal<-subset(df_cond_before_dob,elapsed_months(dob,condition_start_date)<=9)
    message<-paste(nrow(df_cond_before_dob)
                 ,"conditions before birth ( including",nrow(df_cond_prenatal),"prenatal conditions)")
    fileContent<-c(fileContent,"\n",message)
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/condition_occurrence_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-custom_rbind(log_entry_content,
                                    apply_check_type_1('G2-003', "condition_start_date", message, table_name)
      )
    write.csv(log_entry_content, file = log_file_name
              ,row.names=FALSE)

  }

  table_name<-"procedure_occurrence"
  df_proc_before_dob<-as.data.frame(
    select(
      filter(inner_join(procedure_tbl,patient_dob_tbl, by =c("person_id"="person_id")),procedure_date<dob)
      ,person_id, dob, procedure_date
    ))
  if(nrow(df_proc_before_dob)>0)
  {
    df_proc_prenatal<-subset(df_proc_before_dob,elapsed_months(dob,procedure_date)<=9)
    message<-paste(nrow(df_proc_before_dob)
                   ,"procedures before birth ( including",nrow(df_proc_prenatal),"prenatal procedures)")
    fileContent<-c(fileContent,"\n",message)
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/procedure_occurrence_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-custom_rbind(log_entry_content,
                                    apply_check_type_1('G2-003', "procedure_date", message, table_name)
          )
    write.csv(log_entry_content, file = log_file_name
              ,row.names=FALSE)
  }

  table_name<-"measurement"
  df_meas_before_dob<-as.data.frame(
    select(
      filter(inner_join(measurement_tbl,patient_dob_tbl, by =c("person_id"="person_id")),measurement_date<dob)
      ,person_id, dob, measurement_date
    ))
  if(nrow(df_meas_before_dob)>0)
  {
    df_meas_prenatal<-subset(df_meas_before_dob,elapsed_months(dob,measurement_date)<=9)
    message<-paste(nrow(df_meas_before_dob)
                 ,"measurements before birth ( including",nrow(df_meas_prenatal),"prenatal measurements)")
    fileContent<-c(fileContent,"\n",message)
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/measurement_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-custom_rbind(log_entry_content,
                                    apply_check_type_1('G2-003', "measurement_date", message, table_name)
                )
    write.csv(log_entry_content, file = log_file_name
              ,row.names=FALSE)
  }

  table_name<-"drug_exposure"
  df_drug_before_dob<-as.data.frame(
    select(
      filter(inner_join(drug_tbl,patient_dob_tbl, by =c("person_id"="person_id")),drug_exposure_start_date<dob)
      ,person_id, dob, drug_exposure_start_date
    ))
  if(nrow(df_drug_before_dob)>0)
  {
    df_drug_prenatal<-subset(df_drug_before_dob,elapsed_months(dob,drug_exposure_start_date)<=9)
    message<-paste(nrow(df_drug_before_dob)
                 ,"drug exposures before birth ( including",nrow(df_drug_prenatal),"prenatal drug exposures)")
    fileContent<-c(fileContent,"\n",message)
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/drug_exposure_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
     log_entry_content<-custom_rbind(log_entry_content,
                                     apply_check_type_1('G2-003', "drug_exposure_start_date", message, table_name)
     )
     write.csv(log_entry_content, file = log_file_name
               ,row.names=FALSE)

  }

  fileContent<-c(fileContent,"##Facts occurring after patient's death")

  ## Temporal checks --- facts after death date
  table_name<-"visit_occurrence"
  df_visit_after_death<-as.data.frame(
        select(
        filter(inner_join(visit_tbl,death_tbl, by =c("person_id"="person_id")),visit_start_date>death_date)
        ,person_id
        ))

  if(nrow(df_visit_after_death)>0)
  {
      message<-paste(nrow(df_visit_after_death),"visits after death")
      fileContent<-c(fileContent,"\n",message)
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/visit_occurrence_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-custom_rbind(log_entry_content,
                                    apply_check_type_1('G2-003', "visit_start_date", message, table_name)
              )
    write.csv(log_entry_content, file = log_file_name
              ,row.names=FALSE)
  }

  table_name<-"condition_occurrence"
  df_cond_after_death<-as.data.frame(
    select(
      filter(inner_join(condition_tbl,death_tbl, by =c("person_id"="person_id")),condition_start_date>death_date)
      ,person_id
    ))

  if(nrow(df_cond_after_death)>0)
  {
    message<-paste(nrow(df_cond_after_death),"conditions after death")
    fileContent<-c(fileContent,"\n",message)
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/condition_occurrence_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-custom_rbind(log_entry_content,
                                    apply_check_type_1('G2-003', "condition_start_date", message, table_name)
                      )
    write.csv(log_entry_content, file = log_file_name
              ,row.names=FALSE)
  }


  table_name<-"procedure_occurrence"
  df_proc_after_death<-as.data.frame(
    select(
      filter(inner_join(procedure_tbl,death_tbl, by =c("person_id"="person_id")),procedure_date>death_date)
      ,person_id
    ))

  if(nrow(df_proc_after_death)>0)
  {
    message<-paste(nrow(df_proc_after_death),"procedures after death")
    fileContent<-c(fileContent,"\n",message)
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/procedure_occurrence_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-custom_rbind(log_entry_content,
                                    apply_check_type_1('G2-003', "procedure_date", message, table_name)
                      )
    write.csv(log_entry_content, file = log_file_name
              ,row.names=FALSE)
  }

  table_name<-"measurement"
  df_meas_after_death<-as.data.frame(
    select(
      filter(inner_join(measurement_tbl,death_tbl, by =c("person_id"="person_id")),measurement_date>death_date)
      ,person_id
    ))

  if(nrow(df_meas_after_death)>0)
  {
    message<-paste(nrow(df_meas_after_death),"measurements after death")
    fileContent<-c(fileContent,"\n",message)
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/measurement_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-custom_rbind(log_entry_content,
                                    apply_check_type_1('G2-003', "measurement_date", message, table_name)
                    )
    write.csv(log_entry_content, file = log_file_name
              ,row.names=FALSE)
  }


  table_name<-"drug_exposure"
  df_drug_after_death<-as.data.frame(
    select(
      filter(inner_join(drug_tbl,death_tbl, by =c("person_id"="person_id")),drug_exposure_start_date>death_date)
      ,person_id
    ))

  if(nrow(df_drug_after_death)>0)
  {
    message<-paste(nrow(df_drug_after_death),"drug exposures after death")
    fileContent<-c(fileContent,"\n",message)
    ### open the person log file for appending purposes.
    log_file_name<-paste(normalize_directory_path(config$reporting$site_directory),"./issues/drug_exposure_issue.csv",sep="")
    log_entry_content<-(read.csv(log_file_name))
    log_entry_content<-custom_rbind(log_entry_content,
                                    apply_check_type_1('G2-003', "drug_exposure_start_date", message, table_name)
                  )
    write.csv(log_entry_content, file = log_file_name
              ,row.names=FALSE)
  }

  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  #close the connection
  #close_database_connection_OHDSI(con,config)
}
