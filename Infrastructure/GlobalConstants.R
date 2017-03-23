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
