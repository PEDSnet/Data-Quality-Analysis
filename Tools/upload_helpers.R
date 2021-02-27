###Helper Functions for upload_issues.R


read_issues <- function(path){
  adt <- read.csv(paste0(path,'issues/adt_occurrence_issue.csv'))
  care_site <- read.csv(paste0(path,'issues/care_site_issue.csv'))
  condition <- read.csv(paste0(path,'issues/condition_occurrence_issue.csv'))
  death <- read.csv(paste0(path,'issues/death_issue.csv'))
  drug <- read.csv(paste0(path,'issues/drug_exposure_issue.csv'))
  fact <- read.csv(paste0(path,'issues/fact_relationship_issue.csv'))
  imm <- read.csv(paste0(path,'issues/immunization_issue.csv'))
  location <- read.csv(paste0(path,'issues/location_issue.csv'))
  location_history <- read.csv(paste0(path,'issues/location_history_issue.csv'))
  measurement <- read.csv(paste0(path,'issues/measurement_issue.csv'))
  organism <- read.csv(paste0(path,'issues/measurement_organism_issue.csv'))
  observation <- read.csv(paste0(path,'issues/observation_issue.csv'))
  person <- read.csv(paste0(path,'issues/person_issue.csv'))
  procedure <- read.csv(paste0(path,'issues/procedure_occurrence_issue.csv'))  
  provider <- read.csv(paste0(path,'issues/provider_issue.csv'))
  visit <- read.csv(paste0(path,'issues/visit_occurrence_issue.csv'))
  payer <- read.csv(paste0(path,'issues/visit_payer_issue.csv'))
  
  issues <- data.frame(rbind(adt, care_site, condition, death, drug, fact, imm, location, location_history,
                             measurement, organism, observation, person, procedure, provider, visit, payer))
  
  return(issues)
}



compute_new <- function(tblx,
                        name = paste0(sample(letters, 12, replace = TRUE),
                                      collapse = ""),
                        temporary = T,
                        ...) {
  con <- if (inherits(tblx$src, 'src_dbi')) tblx$src$con else tblx$src
  name <- gsub('\\s+','_', name, perl = TRUE)
  if (dbExistsTable(con, name)) dplyr::db_drop_table(con, name)
  rslt <- dplyr::compute(tblx, name = name, temporary = temporary, ...)
  rslt
}