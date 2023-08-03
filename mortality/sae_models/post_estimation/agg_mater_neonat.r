####################################################################################################
## Description: The cause matern_neonat is not modeled. This is because of the discontinuties in
##              age and sex. The maternal part only has sex = 2 and ages 10-50. The neonatal part
##              has both sexes and ages 0-1. For these reasons we don't model it. But, we still
##              want results for this cause. This script takes the results for the two child causes
##              of mater_neonat (maternal and neonatal) and adds them together.
##
##
## Inputs:      dir [character] -- the directory for all models
##
####################################################################################################

stopifnot(grepl("mortality/sae_models$", getwd()))

suppressPackageStartupMessages({
  library(R.utils)
  library(data.table)
  sourceDirectory("functions/")
})


## Get and check settings --------------------------------------------------------------------------
# passed arguments
args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]
resub <- as.logical(args[2])

testing <- F 

get_settings(dir)

## Define functions --------------------------------------------------------------------------------
aggregate_causes <- function(maternal, neonatal, var) {

  stopifnot(dim(maternal) == dim(neonatal))

  # set keys on both so that adding is done correctly
  key_vals <- c('level', 'area', 'year', 'sex', 'race', 'edu', 'age', 'sim')
  if("adjusted" %in% names(maternal)) {
    kep_vals <- c(key_vals, 'adjusted')
  }

  setkeyv(maternal, key_vals)
  setkeyv(neonatal, key_vals)

  # set mx/yll column to value
  setnames(maternal, var, "value")
  setnames(neonatal, var, "value")

  # addition
  if("adjusted" %in% names(maternal)) {
    mater_neonat <- data.table::copy(maternal[, .(level, area, year, sex, race, edu, age, sim, adjusted)])  # get id columns
  } else {
    mater_neonat <- data.table::copy(maternal[, .(level, area, year, sex, race, edu, age, sim)])  # get id columns
  }

  mater_neonat[, value := maternal[, value] + neonatal[, value]]  # add rates from both causes together

  # return column name to mx/yll
  setnames(mater_neonat, "value", var)
  stopifnot(var %in% colnames(mater_neonat))

  return(mater_neonat)
}


core_function <- function(var, geo_level, year, sex, race, edu, save=FALSE) {

  # a base filename. Note that it does not have cause
  filename <- paste0(var, "_draws_", geo_level, "_", year, "_", sex, "_", race, "_", edu, ".rds")

  if (sex == 1) {
      mater_neonat_draws = readRDS(paste0(dir, "/neonatal/", filename)
    )
  } else {

    neonatal <- readRDS(paste0(dir, "/neonatal/", filename))
    maternal <- readRDS(paste0(dir, "/maternal/", filename))

    mater_neonat_draws <- aggregate_causes(maternal = maternal, neonatal = neonatal, var = var)
  }

  # collapse_draws
  collapse_vars <- c("level", "area", "year", "sex", "race", "edu", "age")
  if("adjusted" %in% names(mater_neonat_draws)) {
    collapse_vars <- c(collapse_vars, "adjusted")
  }
  mater_neonat_est <- collapse_draws(draws = mater_neonat_draws, var = eval(var), id_vars = collapse_vars)

  # save both
  if (save) {
    saveRDS(object = mater_neonat_draws,
            file = paste0(dir, "/mater_neonat/", var, "_draws_", geo_level, "_", year, "_", sex, "_", race, "_", edu, ".rds"))

    saveRDS(object = mater_neonat_est,
            file = paste0(dir, "/mater_neonat/", var, "_est_", geo_level, "_", year, "_", sex, "_", race, "_", edu, ".rds"))
  }
}

main <- function() {

  # Read in array info
  draw_args <- fread(paste0(dir, "/mater_neonat/", "/model_args.csv"))
  task_id <- ifelse(is.na(as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))), 1, as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID")))

  # Get arguments from the array argument CSV
  sex <- draw_args[task_id, sex]
  year <- draw_args[task_id, year]
  geo_level <- draw_args[task_id, geo_level]
  race <- draw_args[task_id, race]
  var <- draw_args[task_id, var]
  edu <- draw_args[task_id, edu]

  message(paste0("Working on: Sex :",sex,
                 "\nYear: ",year,
                 "\nGeography: ",geo_level,
                 "\nRace: ",race,
                 "\nEdu: ",edu,
                 "\nVariable: ",var))

  if(file.exists(paste0(dir,"/mater_neonat/",paste0(var,"_est_",geo_level,"_",year,"_",sex,"_",race,"_",edu,".rds"))) &
     resub) {
    stop("Resubmitting, so not running cause")
  }

  core_function(var = var, geo_level = geo_level, year = year,
                sex = sex, race = race, edu = edu, save = TRUE)
}

## Run ----------------------------------------------------------------------------------------
main()
