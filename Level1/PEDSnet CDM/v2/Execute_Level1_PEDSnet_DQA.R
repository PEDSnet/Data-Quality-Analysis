source("../../library/CreatePlots.R", chdir = T)
source("../../library/PerformDatabaseOperations.R", chdir = T)
source("../../library/DocumentationModules.R", chdir = T)
source("../../library/CheckFunctions.R", chdir = T)
source("../../library/CheckType.R", chdir = T)
source("../../library/UnexDiff.R", chdir = T)
source("../../library/Issue.R", chdir = T)
source("../../library/MissData.R", chdir = T)
source("../../library/InvalidConID.R", chdir = T)
source("../../library/InvalidValue.R", chdir = T)
source("../../library/InconPK.R", chdir = T)
source("../../library/InvalidVocab.R", chdir = T)
source("../../library/MissConID.R", chdir = T)
source("../../library/MissFact.R", chdir = T)
source("../../library/MissVisitFact.R", chdir = T)
source("../../library/MissVisitTypeFact.R", chdir = T)
source("../../library/ImplFutureDate.R", chdir = T)
source("../../library/PreBirth.R", chdir = T)
source("../../library/PostDeath.R", chdir = T)
source("../../library/NumOutlier.R", chdir = T)
source("../../library/InconVisitType.R", chdir = T)
source("../../library/InconSource.R", chdir = T)
source("../../library/ImplEvent.R", chdir = T)
source("../../library/UnexTop.R", chdir = T)


executeLevel1DQA <- function() {
  dir.create(file.path(normalize_directory_path(
    g_config$reporting$site_directory), "reports"), showWarnings = FALSE)

  dir.create(file.path(normalize_directory_path(
    g_config$reporting$site_directory), "images"), showWarnings = FALSE)

  dir.create(file.path(normalize_directory_path(
    g_config$reporting$site_directory), "issues"), showWarnings = FALSE)

  ## read the check catalogue

  # data version
  g_data_version<-paste("pedsnet-",g_config$reporting$conventions_version,"-",g_config$reporting$site,"-ETL",g_config$reporting$etl_script_version, sep="")

  flog.info("Starting Level 1 DQA")
  for (available_report in ls(g_level1_reports)) {
      report <- g_level1_reports[[available_report]]

      runAndLog(
        FUN = report,
        success_log = paste(toupper(available_report), " report successfully generated.", sep=""),
        error_log = paste("Failed to generate ", toupper(available_report), " report, see dqa.log for more details.", sep="")
        )
    }
}
