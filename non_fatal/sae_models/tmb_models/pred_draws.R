####################################################################################################
## Description: Generate all draws from the model are simultaneously
##              so that the correlation structure is appropriately reflected. Save the random effect
##              draws, fixed effect draws, and mean object (names/order of the effects).
##              Submits an array job to generate the draws by subdraw, as well as one to
##              concatenate and save the draws. The latter array job holds until the first is complete.
##
## Passed args: output_dir [character] -- home directory for settings and final output
##              sex [integer] -- sex to generate predictions for
##              race [integer] -- race to generate predictions for
##              validate [logical] -- is this a validation model? if so, only mx draws for
##                areas in the validation set are created
##              NOTE: the rest of the arguments are nexessary because I call the downstream scripts
##                from this script. In the future, we could move all of this to the submission script
##              resub [logical] -- are you resubmitting? if yes, it will not re-save files that already
##                exist (or where the final file with all draws exists)
##              queue [string] -- queue for the array jobs
##              m_mem_free_pred [string] -- memory allocation for the array job that predicts mx
##              m_mem_free_save [string] -- memory allocation for the array job that concatenates/save
##                the mx draws
##              fthread [integer] -- threads for both array jobs
##              h_rt [string] -- run time for both array jobs (backfilling not in place so this doesn't)
##                really matter
##
## Requires:    fitted model object ("[dir]/model_fit_[sex]_[race].rds")
##              prepped data file ("[dir]/data.rds")
##              file specifying areas in the validation set, if validate is T (gs_file)
##              populations (pop_file)
##              age standard file (age_std_file)
##
## Outputs:     random effect draws: [dir]/initial_sims_[sex]_[race].rds
##              fixed effect draws: [dir]/fe_sims_[sex]_[race].rds
##              labels for the REs: [dir]/mean_[sex]_[race].rds
##
####################################################################################################

###### Load required libraries
pacman::p_load(tidyr, R.utils, data.table, Matrix, splines, dplyr)

###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (output_dir_draws_est <- commandArgs(TRUE)[[3]])
  (settings_loc <- commandArgs(TRUE)[[4]])
  (sex <- commandArgs(TRUE)[[5]])
  (race <- as.integer(commandArgs(TRUE)[6]))
  (validate <- as.logical(commandArgs(TRUE)[7]))
  (resub <- as.logical(commandArgs(TRUE)[8]))
  (by_sex <- as.logical(commandArgs(TRUE)[9]))
  (draw_width <- as.logical(commandArgs(TRUE)[10]))
  (imp <- as.integer(commandArgs(TRUE)[11]))
} else {
  imp <- 1
  sex <- 1
  race <- 99
}

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in  funcs[!funcs %in% "load_sae_shared.R"]) {
  source(paste0(repo, "/functions/", func))
}

if (!by_sex) {
  sex <- NULL
}

###### Assign settings from settings file
get_settings(settings_loc)

###### Set random seed for draws
#### Set default seed
set.seed(12345)

#### If a seed was specified in the config, set it to that value
if (exists("random_seed")) {
  if (!is.null(random_seed)) {
    set.seed(random_seed)
  }
}

message(output_dir)

## Load model fit -------------------------------------------------------------------------
if (is.null(race_together)) race_together <- FALSE

if (race_together) {
  if (by_sex) {
    # read in the model
    out <- readRDS(paste0(output_dir, "/model_fit_", sex, "_", imp, ".rds"))
  } else {
    # read in the model
    out <- readRDS(paste0(output_dir, "/model_fit_", imp, ".rds"))
  }
} else {
  if (by_sex) {
    # read in the model
    out <- readRDS(paste0(output_dir, "/model_fit_", sex, "_", race, "_", imp, ".rds"))
  } else {
    # read in the model
    out <- readRDS(paste0(output_dir, "/model_fit_", imp, ".rds"))
  }
}

# Generate draws -------------------------------------------------------------------------------
if ((by_sex & resub & file.exists(paste0(output_dir_draws_est, "/fe_sims_", sex, "_", race, "_", imp, ".rds")) &
     file.exists(paste0(output_dir_draws_est, "/initial_sims_", sex, "_", race, "_", imp, ".rds")) &
     file.exists(paste0(output_dir_draws_est, "/mean_", sex,"_", race, "_", imp, ".rds"))) |
    (!by_sex & resub & file.exists(paste0(output_dir_draws_est, "/fe_sims_", race, "_", imp, ".rds")) &
     file.exists(paste0(output_dir_draws_est, "/initial_sims_", race, ".rds")) & file.exists(paste0(output_dir_draws_est, "/mean_", race, "_", imp, ".rds")))) {
  message("Skipping simulation because draws already exist and resubmission specified")
  
} else {
  # extract mean and precision matrix for all model parameters, and align the ordering of the mean
  # vector to match the ordering of the precision matrix
  prec <- out$jointPrecision
  mean <- c(out$par.random, out$par.fixed)
  mean <- unlist(lapply(unique(rownames(prec)), function(x) mean[names(mean) == x]))
  stopifnot(all.equal(rownames(prec), names(mean)))
  
  # draw from a multivariate normal distribution given the mean and precision matrix for all parameters
  # then subset to those required for constructing predictions
  gen_sims <- function(mu, prec, n.sims) {
    z = matrix(rnorm(length(mu) * n.sims), ncol = n.sims)
    L_inv = Cholesky(prec)
    mu + solve(as(L_inv, "pMatrix"), solve(t(as(L_inv, "Matrix")), z))
  }
  
  sims <- gen_sims(mu = mean, prec = prec, n.sims = n.sims)
  sims <- sims[grepl("^B|^re|^covar_subpop", names(mean)), ]
  mean <- mean[grepl("^B|^re|^covar_subpop", names(mean))]
  rm(prec, gen_sims); gc()
  
  # merge on weights for collapsing across stratifying variables
  if (!is.null(strat_vars)) {
    #### Load pops for post-stratification
    if (!exists("post_stratification_wt_file_under20")) {
      stratpop <- readRDS(post_stratification_wt_file)  
    } else {
      if (!is.null(post_stratification_wt_file_under20) & any(ages < 20)) {
        stratpop <- rbindlist(list(readRDS(post_stratification_wt_file), readRDS(post_stratification_wt_file_under20)), use.names = TRUE, fill = TRUE)
      } else {
        stratpop <- readRDS(post_stratification_wt_file)  
      }
    }
    
    #### Merge on state and state_name if necessary
    if (is.null(stratpop$state)) {
      locs <- fread("FILEPATH")
      stratpop <- merge(stratpop, unique(locs[, list(mcnty, state, state_name)]), by = "mcnty", all.x = TRUE)
    }
    
    #### Restrict to requested sex
    stratpop <- stratpop[sex == get("sex", .GlobalEnv)]
    
    #### Collapse across un-needed stratifiers
    stratpopage <- unique(as.character(stratpop$age))
    if (max(stratpopage) != max(as.character(ages)) &
        max(head(stratpopage, -1)) == max(as.character(ages))) {
      stratpop[, age := ifelse(age == max(stratpopage), max(ages), as.character(age))]
      stratpop$age <- as.integer(as.character(stratpop$age))
    }
    rm(stratpopage)
    
    #### Subset to requested years, ages, sexes, and races
    stratpop <- stratpop[, list(value = sum(value)), by = c(area_var, "year", "age", "race", "sex", "state", "state_name", strat_vars)]
    
    if (by_sex) {
      saveRDS(stratpop, paste0(output_dir_draws_est, "/stratpop_", sex, "_", race, "_", imp, "_", edu, ".rds"))
    } else {
      saveRDS(stratpop, paste0(output_dir_draws_est, "/stratpop_", race, "_", imp, "_", edu, ".rds"))
    }
  }
  
  if (by_sex) {
    saveRDS(sims, paste0(output_dir_draws_est, "/initial_sims_", sex, "_", race, "_", imp, "_", edu, ".rds"))
    saveRDS(mean, paste0(output_dir_draws_est, "/mean_", sex, "_", race, "_", imp, "_", edu, ".rds"))
  } else {
    saveRDS(sims, paste0(output_dir_draws_est, "/initial_sims_", race, "_", imp, "_", edu, ".rds"))
    saveRDS(mean, paste0(output_dir_draws_est, "/mean_", race, "_", imp, "_", edu, ".rds"))
  }
}
