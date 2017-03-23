library(yaml)
source("../Level1/library/DocumentationModules.R", chdir=T)
source("./scripts/Utility_Functions.R", chdir=T)
source("../Level1/library/CheckFunctions.R", chdir=T)

executeLevel2DQA<- function() {
  tryCatch(
    expr = {
      # otherwise dplyr's group by , summarize etc do not work
      detach("package:plyr", unload = TRUE)
    }
    ,error = function(e) {
       flog.info("Plyr package already detached")
    },warning = function(w) {
       flog.info(w)
    }
  )
  
  dir.create(file.path(normalize_directory_path( g_config$reporting$site_directory), "reports"), showWarnings = FALSE)
  dir.create(file.path(normalize_directory_path( g_config$reporting$site_directory), "images"), showWarnings = FALSE)
  dir.create(file.path(normalize_directory_path( g_config$reporting$site_directory), "issues"), showWarnings = FALSE)
  dir.create(file.path(normalize_directory_path( g_config$reporting$site_directory), "data"), showWarnings = FALSE)
  
  g_data_version<-paste("pedsnet-",g_config$reporting$conventions_version,"-",g_config$reporting$site,"-ETL",g_config$reporting$etl_script_version, sep="")
  
  
 flog.info("GENERATING LEVEL 2 PATIENT REPORT")
  runAndLog(
      FUN=generateLevel2Patient,
   success_log = 'Level 2 patient report succesfully generated.',
   error_log='Failed to generate level 2 patient report, see dqa.log for more details.')
  
  flog.info("GENERATING LEVEL 2 VISIT REPORT")
  runAndLog(
        FUN=generateLevel2Visit,
    success_log = 'Level 2 visit report succesfully generated.',
   error_log='Failed to generate level 2 visit report, see dqa.log for more details.')
  
  flog.info("GENERATING LEVEL 2 OBSERVATION REPORT")
  runAndLog(
    FUN=generateLevel2Observation,
    success_log = 'Level 2 observation report succesfully generated.',
    error_log='Failed to generate level 2 observation report, see dqa.log for more details.')
  
  flog.info("GENERATING LEVEL 2 CONDITION REPORT")
  runAndLog(
  FUN=generateLevel2Condition,
    success_log = 'Level 2 condition report succesfully generated.',
    error_log='Failed to generate level 2 condition report, see dqa.log for more details.')
  
  flog.info("GENERATING LEVEL 2 PROCEDURE REPORT")
  runAndLog(
   FUN=generateLevel2Procedure,
   success_log = 'Level 2 procedure report succesfully generated.',
   error_log='Failed to generate level 2 procedure report, see dqa.log for more details.')
  
  flog.info("GENERATING LEVEL 2 DRUG REPORT")
  runAndLog(
   FUN=generateLevel2Drug,
   success_log = 'Level 2 drug report succesfully generated.',
   error_log='Failed to generate level 2 drug report, see dqa.log for more details.')
  
  flog.info("GENERATING LEVEL 2 MEASUREMENT REPORT")
  runAndLog(
  
   FUN=generateLevel2Measurement,
   
   success_log = 'Level 2 measurement report succesfully generated.',
   error_log='Failed to generate level 2 measurement report, see dqa.log for more details.')
  
  flog.info("GENERATING LEVEL 2 MEASUREMENT ORGANISM REPORT")
  runAndLog(
    
    FUN=generateLevel2MeasurementOrganism,
    
   success_log = 'Level 2 measurement organism report succesfully generated.',
   error_log='Failed to generate level 2 measurement organism report, see dqa.log for more details.')
}
