source("../../../Library/CreatePlots.R", chdir = T)
source("../../../Library/PerformDatabaseOperations.R", chdir = T)
source("../../../Library/DocumentationModules.R", chdir = T)
source("../../../Library/CheckFunctions.R", chdir = T)
source("../../../Library/CheckType.R", chdir = T)
source("../../../Library/UnexDiff.R", chdir = T)
source("../../../Library/Issue.R", chdir = T)
source("../../../Library/MissData.R", chdir = T)
source("../../../Library/InvalidConID.R", chdir = T)
source("../../../Library/InvalidValue.R", chdir = T)
source("../../../Library/InvalidFormat.R", chdir = T)
source("../../../Library/InconPK.R", chdir = T)
source("../../../Library/InvalidVocab.R", chdir = T)
source("../../../Library/MissConID.R", chdir = T)
source("../../../Library/MissFact.R", chdir = T)
source("../../../Library/MissVisitFact.R", chdir = T)
source("../../../Library/MissVisitTypeFact.R", chdir = T)
source("../../../Library/ImplFutureDate.R", chdir = T)
source("../../../Library/PreBirth.R", chdir = T)
source("../../../Library/PostDeath.R", chdir = T)
source("../../../Library/NumOutlier.R", chdir = T)
source("../../../Library/InconVisitType.R", chdir = T)
source("../../../Library/InconSource.R", chdir = T)
source("../../../Library/ImplEvent.R", chdir = T)
source("../../../Library/UnexTop.R", chdir = T)


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
