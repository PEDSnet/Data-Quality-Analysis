g_config_path <<- paste(getwd(),
  "/Resources/PEDSnet_config.yml", sep = "")

g_catalog_folder_path <<- paste(getwd(), "/Data/DQACatalog/", sep = "")

g_config <<- yaml.load_file(g_config_path)

g_total_counts_path <<- paste(getwd(),
                              "/Data/PreviousDataSummary/total_counts.csv", sep= "")

g_total_fact_type_counts_path <<- paste(getwd(),
                              "/Data/PreviousDataSummary/total_fact_type_counts.csv", sep= "")

g_executive_summary_path <<- paste(getwd(),
                                   "/ExecutiveSummary/", sep="")
g_top50_inpatient_conditions_path <<- paste(getwd(),
                                        "/Data/PreviousDataSummary/top50_inpatient_conditions_ranked.csv", sep= "")

g_top50_outpatient_conditions_path <<- paste(getwd(),
                                        "/Data/PreviousDataSummary/top50_outpatient_conditions_ranked.csv", sep= "")

g_top50_inpatient_procedures_path <<- paste(getwd(),
                                            "/Data/PreviousDataSummary/top50_inpatient_procedures_ranked.csv", sep= "")

g_top50_outpatient_procedures_path <<- paste(getwd(),
                                             "/Data/PreviousDataSummary/top50_outpatient_procedures_ranked.csv", sep= "")

g_top50_inpatient_drugs_path <<- paste(getwd(),
                                            "/Data/PreviousDataSummary/top50_inpatient_drugs_ranked.csv", sep= "")

g_top50_outpatient_drugs_path <<- paste(getwd(),
                                             "/Data/PreviousDataSummary/top50_outpatient_drugs_ranked.csv", sep= "")


g_df_check_catalogue<<-read.csv(paste(g_catalog_folder_path,"DQA_Check_Type_Inventory.csv",sep=""), header = TRUE, sep = ",", quote = "\"",
                             dec = ".", fill = TRUE, comment.char = "")

g_data_version<<-paste("pedsnet-",g_config$reporting$conventions_version,"-", g_config$reporting$site,"-ETL", g_config$reporting$etl_script_version, sep="")


g_level1_reports<<-new.env()
g_level1_reports[["person"]] = generatePersonReport
g_level1_reports[["provider"]] = generateProviderReport
g_level1_reports[["care_site"]] = generateCareSiteReport
g_level1_reports[["location"]] = generateLocationReport
g_level1_reports[["death"]] = generateDeathReport
g_level1_reports[["visit_occurrence"]] = generateVisitOccurrenceReport
g_level1_reports[["procedure_occurrence"]] = generateProcedureOccurrenceReport
g_level1_reports[["condition_occurrence"]] = generateConditionOccurrenceReport
g_level1_reports[["observation"]] = generateObservationReport
g_level1_reports[["measurement"]] = generateMeasurementReport
g_level1_reports[["measurement_organism"]] = generateMeasurementOrganismReport
g_level1_reports[["drug_exposure"]] = generateDrugExposureReport
g_level1_reports[["visit_payer"]] = generateVisitPayerReport
g_level1_reports[["fact_relationship"]] = generateFactRelationshipReport
g_level1_reports[["adt_occurrence"]] = generateAdtOccurrenceReport
g_level1_reports[["device_exposure"]] = generateDeviceExposureReport
g_level1_reports[["immunization"]] = generateImmunizationReport

g_level2_reports<<-new.env()
g_level2_reports[["adt_occurrence"]] = generateLevel2ADT_Occurrence
g_level2_reports[["death"]] = generateLevel2Death
g_level2_reports[["patient"]] = generateLevel2Patient
g_level2_reports[["visit_occurrence"]] = generateLevel2Visit
g_level2_reports[["observation"]] = generateLevel2Observation
g_level2_reports[["condition_occurrence"]] = generateLevel2Condition
g_level2_reports[["procedure_occurrence"]] = generateLevel2Procedure
g_level2_reports[["drug_exposure"]] = generateLevel2Drug
g_level2_reports[["measurement"]] = generateLevel2Measurement
g_level2_reports[["measurement_organism"]] = generateLevel2MeasurementOrganism

