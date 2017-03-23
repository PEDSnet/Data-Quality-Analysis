library(DBI)
library(yaml)

generateSummaryReport <- function (g_data_version) {
  #establish connection to database
  con <- establish_database_connection_OHDSI( g_config)

  #########################################################################

  # read a database table into an R dataframe
  table_name<-"person"
  df_person <- retrieve_dataframe_OHDSI(con, g_config,table_name)

  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path( g_config$reporting$site_directory),"./reports/Level1_Summary_Report_Automatic.md",sep=""))
  #fileContent <-get_report_header(table_name, g_config)
  #fileContent<-"# Person"

  #setting up big data flag
  big_data_flag<-FALSE

  #PRIMARY FIELD
  field_name<-"person_id"
  fileContent<-paste("Person", describeIdentifier(df_person,field_name))

  #fileContent<-c(fileContent)
  #NOMINAL Fields

  #Gender Concept Id
  field_name <- "gender_concept_id"
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])

  # flog.info("here")

  #Gender Source Value
  field_name="gender_source_value"
  #fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])

  #Gender Source Concept id
  field_name="gender_source_concept_id"
  #fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])

  #Race Concept Id
  field_name="race_concept_id"
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])

  #Race Source Value
  field_name="race_source_value"
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])

  #race source Concept id
  field_name="race_source_concept_id"
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])


  #Ethnicity Concept Id
  field_name="ethnicity_concept_id"
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])


  #Ethnicity Source Value
  field_name="ethnicity_source_value"
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])


  #Ethnicity source Concept id
  field_name="ethnicity_source_concept_id"
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])

  # ORDINAL Fields
  #Year of Birth
  field_name="year_of_birth"
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])

  #Month of Birth
  field_name="month_of_birth"
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])

  #Day of Birth
  field_name="day_of_birth"
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])

  #Time of Birth --
  field_name<-"time_of_birth"
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])

  #RATIO Fields

  #pn_gestational_field
  field_name<-"pn_gestational_age"
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])

  #FOREIGN KEY fields
  #LocationID
  field_name<-"location_id"
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])

  #provider_id
  field_name="provider_id"
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])

  #Care site id
  field_name="care_site_id"
  missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
  fileContent<-paste(fileContent,"|",unlist(strsplit(missing_message," is "))[[2]])

  #############################################
  table_name<-"visit_occurrence"
  big_data_flag<-TRUE
  #fileContent<-c(fileContent,"# Visit Occurrence")

  field_name<-"visit_occurrence_id"
  df_total_visit_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent, "Visit Occurrence", df_total_visit_count[1][1])

  field_name<-"visit_start_time"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"visit_end_time"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"visit_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"visit_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"person_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"provider_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"care_site_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])


  #########################################################################

  table_name<-"condition_occurrence"
  big_data_flag<-TRUE
  fileContent<-c(fileContent,"# Condition Occurrence")

  field_name<-"condition_occurrence_id"
  df_total_visit_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", df_total_visit_count[1][1],"\n"))

  field_name<-"condition_type_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"condition_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"condition_source_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"condition_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"condition_start_time"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"condition_end_time"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"stop_reason"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"person_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"visit_occurrence_id"
  #df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  #fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])
  fileContent<-paste(fileContent,"|X")

  field_name<-"provider_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  #########################################################################
  table_name<-"procedure_occurrence"
  big_data_flag<-TRUE
  fileContent<-c(fileContent,"# Procedure Occurrence")

  field_name<-"procedure_occurrence_id"
  df_total_visit_count<-retrieve_dataframe_count(con, g_config,table_name,field_name)
  fileContent<-c(fileContent,paste("The total number of",field_name,"is:", df_total_visit_count[1][1],"\n"))

  field_name<-"procedure_type_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"procedure_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"procedure_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"procedure_source_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"procedure_date"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"procedure_time"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"modifier_concept_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])
  field_name<-"modifier_source_value"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])
  field_name<-"quantity"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"person_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  field_name<-"visit_occurrence_id"
  #df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  #fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])
  fileContent<-paste(fileContent,"|X")


  field_name<-"provider_id"
  df_table<-retrieve_dataframe_group(con, g_config,table_name,field_name)
  fileContent<-paste(fileContent,"|",unlist(strsplit(reportMissingCount(df_table,table_name,field_name,big_data_flag)," is "))[[2]])

  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)

  #close the connection
  close_database_connection_OHDSI(con, g_config)
}
