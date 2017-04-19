g_config_path <<- paste(getwd(),
  "/Level1/PEDSnet CDM/PEDSnet_config.yml", sep = "")

g_catalog_folder_path <<- paste(getwd(), "/DQA_Catalog/", sep = "")

g_config <<- yaml.load_file(g_config_path)

g_total_counts_path <<- paste(getwd(),
                              "/Data/total_counts.csv", sep= "")

g_executive_summary_path <<- paste(getwd(),
                                   "/ExecutiveSummary/", sep="")
g_top50_inpatient_conditions_path <<- paste(getwd(),
                                        "/Data/top50_inpatient_conditions_ranked.csv", sep= "")

g_top50_outpatient_conditions_path <<- paste(getwd(),
                                        "/Data/top50_outpatient_conditions_ranked.csv", sep= "")

g_df_check_catalogue<<-read.csv(paste(g_catalog_folder_path,"DQA_Check_Type_Inventory.csv",sep=""), header = TRUE, sep = ",", quote = "\"",
                             dec = ".", fill = TRUE, comment.char = "")

g_data_version<<-paste("pedsnet-",g_config$reporting$conventions_version,"-", g_config$reporting$site,"-ETL", g_config$reporting$etl_script_version, sep="")


g_level1_reports<<-new.env()
g_level1_reports[["person"]] = generatePersonReport
g_level1_reports[["provider"]] = generateProviderReport
g_level1_reports[["care site"]] = generateCareSiteReport
g_level1_reports[["location"]] = generateLocationReport
g_level1_reports[["death"]] = generateDeathReport
g_level1_reports[["visit occurrence"]] = generateVisitOccurrenceReport
g_level1_reports[["procedure occurrence"]] = generateProcedureOccurrenceReport
g_level1_reports[["condition occurrence"]] = generateConditionOccurrenceReport
g_level1_reports[["observation"]] = generateObservationReport
g_level1_reports[["measurement"]] = generateMeasurementReport
g_level1_reports[["measurement organism"]] = generateMeasurementOrganismReport
g_level1_reports[["drug exposure"]] = generateDrugExposureReport
g_level1_reports[["visit payer"]] = generateVisitPayerReport
g_level1_reports[["fact relationship"]] = generateFactRelationshipReport
g_level1_reports[["adt occurrence"]] = generateAdtOccurrenceReport
g_level1_reports[["device exposure"]] = generateDeviceExposureReport

g_level2_reports<<-new.env()
g_level2_reports[["patient"]] = generateLevel2Patient
g_level2_reports[["visit"]] = generateLevel2Visit
g_level2_reports[["observation"]] = generateLevel2Observation
g_level2_reports[["condition"]] = generateLevel2Condition
g_level2_reports[["procedure"]] = generateLevel2Procedure
g_level2_reports[["drug"]] = generateLevel2Drug
g_level2_reports[["measurement"]] = generateLevel2Measurement
g_level2_reports[["measurement organism"]] = generateLevel2MeasurementOrganism
