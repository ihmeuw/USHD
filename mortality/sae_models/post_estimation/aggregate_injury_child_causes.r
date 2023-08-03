####################################################################################################
## Purpose: aggregate inj_trans, _intent, and _unintent models
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

testing <- F  # checks if calc_all_ages had to be run for development.

get_settings(dir)

## Define functions --------------------------------------------------------------------------------
aggregate_causes <- function(inj_trans, intent, unintent, var) {
  
  stopifnot(dim(intent) == dim(unintent))
  stopifnot(dim(intent) == dim(inj_trans))
  
  # set keys on both so that adding is done correctly
  key_vals <- c('level', 'area', 'year', 'sex', 'race', 'edu', 'age', 'sim')
  if("adjusted" %in% names(inj_trans)) {
    kep_vals <- c(key_vals, 'adjusted')
  }
  
  setkeyv(inj_trans, key_vals)
  setkeyv(intent, key_vals)
  setkeyv(unintent, key_vals)
  
  # set mx/yll column to value
  setnames(inj_trans, var, "value")
  setnames(intent, var, "value")
  setnames(unintent, var, "value")
  
  # addition
  if("adjusted" %in% names(inj_trans)) {
    inj <- data.table::copy(inj_trans[, .(level, area, year, sex, race, edu, age, sim, adjusted)])  # get id columns
  } else {
    inj <- data.table::copy(inj_trans[, .(level, area, year, sex, race, edu, age, sim)])  # get id columns
  }
  
  inj[, value := inj_trans[, value] + intent[, value] + unintent[, value]]  # add rates from all causes together
  
  # return column name to mx/yll
  setnames(inj, "value", var)
  stopifnot(var %in% colnames(inj))
  
  return(inj)
}


core_function <- function(var, geo_level, year, sex, race, edu, save=FALSE) {
  
  # a base filename. Note that it does not have cause
  filename <- paste0(var, "_draws_", geo_level, "_", year, "_", sex, "_", race, "_", edu, ".rds")
  
  inj_trans <- readRDS(paste0(dir, "/inj_trans/", filename))
  intent <- readRDS(paste0(dir, "/_intent/", filename))
  unintent <- readRDS(paste0(dir, "/_unintent/", filename))
  
  inj_draws <- aggregate_causes(inj_trans = inj_trans,
                                intent = intent,
                                unintent = unintent, var = var)
  
  # collapse_draws
  collapse_vars <- c("level", "area", "year", "sex", "race", "edu", "age")
  if("adjusted" %in% names(inj_draws)) {
    collapse_vars <- c(collapse_vars, "adjusted")
  }
  inj_est <- collapse_draws(draws = inj_draws, var = eval(var), id_vars = collapse_vars)
  
  # save both
  if (save) {
    saveRDS(object = inj_draws,
            file = paste0(dir, "/_inj/", var, "_draws_", geo_level, "_", year, "_", sex, "_", race, "_", edu, ".rds"))
    
    saveRDS(object = inj_est,
            file = paste0(dir, "/_inj/", var, "_est_", geo_level, "_", year, "_", sex, "_", race, "_", edu, ".rds"))
  }
}

main <- function() {
  
  # Read in array info
  draw_args <- fread(paste0(dir, "/_inj/", "/model_args.csv"))
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
  
  if(file.exists(paste0(dir,"/_inj/",paste0(var,"_est_",geo_level,"_",year,"_",sex,"_",race,"_",edu,".rds"))) &
     resub) {
    stop("Resubmitting, so not running cause")
  }
  
  core_function(var = var, geo_level = geo_level, year = year,
                sex = sex, race = race, edu = edu, save = TRUE)
}

## Run ----------------------------------------------------------------------------------------
main()

