#!/usr/bin/env Rscript

#!/usr/bin/env Rscript

if (!exists('req_env')) req_env <- new.env(parent = emptyenv())

#' Retrieve an item of site-specific configuration
#'
#' @param label The name of the configuration datum
#' @param ... Optionally, a value to which to set the configuration
#'     datum.  Only a single value is read; the `...` formalism is
#'     used to make it possible to detect when a value is present.
#'
#' @return The (possibly new) configuration value, or `NULL` if it
#'     does not exist.
config <- function(label, ...) {
  dots <- list(...)
  if (length(dots) > 0)
    assign(label, dots[[1]], pos = req_env)
  get0(label, envir = req_env)
}

#' Site-specific information for data request execution.
#'
#' Please edit information as noted in lines beginning with `#>`
#' below.

if (!exists('req_env')) req_env <- new.env(parent = emptyenv())

#> Please alter to reflect your site name, 
config('site', g_config$reporting$site)

#> Please replace with code to establish a database connection at your
#> site. The connection must be able to reach CDM data, the
#> vocabularies, and any result schemata needed.  The connection may be
#> either a dplyr-style src_foo() object (as below), or a DBI-style
#> dbConnect() object (e.g. dbConnect(MySQL(), host = ...)).
config_path<-c(g_config$db$argos_config_file_path)

config('db_src',
       { require(Argos);
         src_argos(paths=config_path) 
       })

#> Name of the schema, if any, to be prepended to CDM fact table names.
#> If `NA`, no schema qualifier is added.
config('cdm_schema',  g_config$db$schema)

#> Name of the schema, if any, to be prepended to vocabulary tables
#> If `NA`, no schema qualifier is added.
config('vocabulary_schema', g_config$db$vocab_schema)

#> Name of the schema, if any, to be added to dqa schema tables
#> If `NA`, no schema qualifier is added.
config('dqa_schema', g_config$db$dqa_schema)

#> Names of standard tables used in queries.  Please edit only the
#> right-hand side of each assignment.  Table names must be lower-case.
config('table_names',
       list(adt_occurrence = 'adt_occurrence',
            care_site = 'care_site',
            condition_occurrence = 'condition_occurrence',
            death = 'death',
            dose_era = 'dose_era',
            drug_era = 'drug_era',
            drug_exposure = 'drug_exposure',
            fact_relationship = 'fact_relationship',
            location = 'location',
            measurement = 'measurement',
            measurement_organism = 'measurement_organism',
            observation = 'observation',
            observation_period = 'observation_period',
            person = 'person',
            procedure_occurrence = 'procedure_occurrence',
            provider = 'provider',
            visit_occurrence = 'visit_occurrence',
            visit_payer = 'visit_payer',
            concept = 'concept',
            concept_ancestor = 'concept_ancestor',
            concept_relationship = 'concept_relationship',
            drug_in_concept_id_map = 'drug_in_concept_id_map'))

#> ##################### End of site-specific configuration