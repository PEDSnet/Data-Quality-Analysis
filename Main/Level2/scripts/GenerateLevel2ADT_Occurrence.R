library(DBI)
library(yaml)
library(dplyr)

generateLevel2ADT_Occurrence <- function () {

  table_name<-"adt_occurrence"
  
  #writing to the final DQA Report
  fileConn<-file(paste(normalize_directory_path(g_config$reporting$site_directory),"./reports/Level2_ADT_Occurrence_Report_Automatic.md",sep=""))
  fileContent <-get_report_header("Level 2",g_config)
  
  log_file_name<-paste(normalize_directory_path(g_config$reporting$site_directory),"./issues/adt_occurrence_issue.csv",sep="")

  adt_tbl <- cdm_tbl(req_env$db_src, 'adt_occurrence')
  

  total_adt_count<-  as.data.frame(dplyr::summarise(adt_tbl,count = n()))[1,1]
  
  ### temporal outlier check 
  field_name<-"adt_date"
  log_entry_content<-(read.csv(log_file_name))
  try(log_entry_content<-custom_rbind(log_entry_content,applyCheck(TempOutlier(), c(table_name), 
                                                               c(field_name), NULL))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
  fileContent<-c(fileContent,paste_image_name(table_name,paste0(field_name,'-yyyy-mm')));
  
  log_entry_content<-(read.csv(log_file_name))
  log_entry_content<-custom_rbind(log_entry_content,applyCheck(InconDateTime(), c(table_name), c('adt_datetime', 
                                                                                                 'adt_date'))) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  
  ### DQA checkpoint --- incosnistent visit types
  log_entry_content<-(read.csv(log_file_name))
  
  log_entry_content<-custom_rbind(log_entry_content,
                                      applyCheck(InconVisitType(), 
                                                 c(table_name, "visit_occurrence"),
                                                   c("visit_occurrence_id", "visit_concept_id"),
                                                   c("adt_occurrences linked to outpatient visits",
                                                   9202)
                                                 )
                                  ) 
  write.csv(log_entry_content, file = log_file_name
          ,row.names=FALSE)
  
  log_entry_content<-custom_rbind(log_entry_content,
                                  applyCheck(InconVisitType(), 
                                             c(table_name, "visit_occurrence"),
                                             c("visit_occurrence_id", "visit_concept_id"),
                                             c("adt_occurrences linked to op non-physician visits",
                                               2000000469)
                                  )
  ) 
  write.csv(log_entry_content, file = log_file_name
            ,row.names=FALSE)
  
  #write all contents to the report file and close it.
  writeLines(fileContent, fileConn)
  close(fileConn)
}
