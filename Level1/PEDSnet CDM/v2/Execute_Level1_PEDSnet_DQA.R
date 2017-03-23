source("../../library/CreatePlots.R", chdir = T)
source("../../library/PerformDatabaseOperations.R", chdir = T)
source("../../library/DocumentationModules.R", chdir = T)
source("../../library/CheckFunctions.R", chdir = T)
source("../../../Infrastructure/ReportImports.R", chdir = T)

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

  flog.info("GENERATING PERSON REPORT")
  runAndLog(
    FUN = generatePersonReport,
    success_log = 'Person report succesfully generated.',
    error_log = 'Failed to generate person report, see dqa.log for more details.',
    g_data_version)
  
  flog.info("GENERATING PROVIDER REPORT")
    runAndLog(
    FUN = generateProviderReport,
    success_log = 'Provider report succesfully generated.',
    error_log = 'Failed to generate provider report, see dqa.log for more details.',
    g_data_version)


    flog.info("GENERATING CARE_SITE REPORT")
    runAndLog(  
    FUN = generateCareSiteReport,
    success_log = 'Care site report succesfully generated.',
    error_log = 'Failed to generate Care site report, see dqa.log for more details.',
    g_data_version)

   flog.info("GENERATING LOCATION REPORT")
   runAndLog(
    FUN = generateLocationReport,
    success_log = 'Location report succesfully generated.',
    error_log = 'Failed to generate location report, see dqa.log for more details.',
    g_data_version)

  flog.info("GENERATING DEATH REPORT")
  runAndLog(
    FUN = generateDeathReport,
    success_log = 'Death report succesfully generated.',
    error_log = 'Failed to generate death report, see dqa.log for more details.',
    g_data_version)

   flog.info("GENERATING VISIT_OCCURRENCE REPORT")
   runAndLog(
    FUN = generateVisitOccurrenceReport,
    success_log = 'Visit occurrence report succesfully generated.',
    error_log = 'Failed to generate visit occurrence report, see dqa.log for more details.',
    g_data_version)

  flog.info("GENERATING PROCEDURE_OCCURRENCE REPORT")
  runAndLog(
   FUN = generateProcedureOccurrenceReport,
   success_log = 'Procedure occurrence report succesfully generated.',
   error_log = 'Failed to generate procedure occurrence report, see dqa.log for more details.',
   g_data_version)
  
  flog.info("GENERATING CONDITION_OCCURRENCE REPORT")
  runAndLog(
   FUN = generateConditionOccurrenceReport,
   success_log = 'Condition occurrence report succesfully generated.',
   error_log = 'Failed to generate condition occurrence report, see dqa.log for more details.',
   g_data_version)
  
  flog.info("GENERATING OBSERVATION REPORT")
  runAndLog(
    FUN = generateObservationReport,
    success_log = 'Observation report succesfully generated.',
    error_log = 'Failed to generate observation report, see dqa.log for more details.',
    g_data_version)

  flog.info("GENERATING MEASUREMENT REPORT")
  runAndLog(
    FUN = generateMeasurementReport,
    success_log = 'Measurement report succesfully generated.',
    error_log = 'Failed to generate measurement report, see dqa.log for more details.',
    g_data_version)

  flog.info("GENERATING MEASUREMENT ORGANISM REPORT")
     runAndLog(
   FUN = generateMeasurementOrganismReport,
   success_log = 'Measurement organism report succesfully generated.',
   error_log = 'Failed to generate measurement organism report, see dqa.log for more details.',
   g_data_version)

   flog.info("GENERATING DRUG_EXPOSURE REPORT")
   runAndLog(
   FUN = generateDrugExposureReport,
   success_log = 'Drug exposure report succesfully generated.',
   error_log = 'Failed to generate drug exposure report, see dqa.log for more details.',
   g_data_version)

   flog.info("GENERATING VISIT_PAYER REPORT")
   runAndLog(
   FUN = generateVisitPayerReport,
   success_log = 'Visit payer report succesfully generated.',
   error_log = 'Failed to generate visit payer report, see dqa.log for more details.',
   g_data_version)

   flog.info("GENERATING FACT_RELATIONSHIP REPORT")
   runAndLog(
   FUN = generateFactRelationshipReport,
   success_log = 'Fact relationship report succesfully generated.',
   error_log = 'Failed to generate fact relationship report, see dqa.log for more details.',
   g_data_version)
  
   flog.info("GENERATING ADT_OCCURRENCE REPORT")
   runAndLog(
   FUN = generateAdtOccurrenceReport,
   success_log = 'ADT occurrence report succesfully generated.',
   error_log = 'Failed to generate ADT occurrence report, see dqa.log for more details.',
   g_data_version)

}
