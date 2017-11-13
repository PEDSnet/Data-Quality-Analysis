source("../../Library/DocumentationModules.R", chdir=T)
source("./scripts/Utility_Functions.R", chdir=T)
source("../../Library/CheckFunctions.R", chdir=T)
source("../../Library/InconCohort.R", chdir = T)
source("../../Library/InconDateTime.R", chdir = T)
source("../../Library/UnexTop.R", chdir = T)

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
  

  flog.info("Starting Level 2 DQA")
  for (available_report in ls(g_level2_reports)) {
      report <- g_level2_reports[[available_report]]

      flog.info("GENERATING %s REPORT", toupper(available_report))

      runAndLog(
        FUN = report,
        success_log = paste(toupper(available_report), " report successfully generated.", sep=""),
        error_log = paste("Failed to generate ", toupper(available_report), " report, see dqa.log for more details.", sep="")
        )
    }
}
