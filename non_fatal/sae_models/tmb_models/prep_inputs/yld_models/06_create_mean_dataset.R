#########################################################################################################
## Description: Combine best runs of all indicators for state models into one file for modeling (means of the values, 
##              not for models incorporating uncertainty done at the draw level). u
##
## Passed args: read in FILEPATH 
##              which has best run dates for each indicators. This file is updated but the version of the file
##              at the time the script was run is saved along with the data output.
##
## Requires:    FILEPATH
## 
## Outputs:     prepped data file ("FILEPATH")
##              version of FILEPATH when it was run
##              Tracked in following file: FILEPATH
##
#########################################################################################################

#########################################################################################################
############## Setup ####################################################################################
#########################################################################################################

pacman::p_load(data.table, tidyverse, ggplot2, foreach, doParallel, tidyr, broom)
source("FILEPATH")
source("FILEPATH")

###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
}

###### Source functions
repo <- paste0("FILEPATH")
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

# Best run dates for sae_indics
sae_indics <- fread(paste0(output_dir, "/sae_indics_run_dates.csv"))
cov_map <- fread(paste0(output_dir, "/cov_map.csv"))

mcnty <- fread("FILEPATH")
setnames(mcnty, "mcnty", "area") # to match predictions


#########################################################################################################
############## GBD Data #################################################################################
#########################################################################################################

causes <- c("inj_trans", "unintent", "intent", "subs", "diab_ckd",
            "neuro", "comm", "neo", "cvd", "resp", "digest", "skin", "msk", "otherncd", 
            "mental", "sense") 

gbd_yld_files <- paste0(output_dir, "/temp_mean_yld_rate_", causes, ".rds")
gbd <- lapply(gbd_yld_files, readRDS)

gbd <- Reduce(function(...) merge(..., by = c("state", "state_name", "year", "age", "sex"), all = TRUE), gbd)

#########################################################################################################
############## Race-ethn specific mcnty-level YLLs ######################################################
#########################################################################################################

yll_dir <- "FILEPATH"
causes <- c("inj_trans", "_unintent", "_intent", "_subs", "diab_ckd",
              "_neuro", "_comm", "_neo", "cvd", "resp", "digest", "skin", "msk", "_otherncd") 

yll_files <- paste0(yll_dir, "/",causes, "/yll_est_all_raked.rds")

stopifnot(file.exists(yll_files))

ylls_all_ls <- lapply(yll_files, readRDS)
ylls_all <- rbindlist(ylls_all_ls, use.names = TRUE)

# subset
ylls_age <- ylls_all[!(age %in% c(98, 99))]
ylls_age <- ylls_age[sex %in% 1:2 & race != 1 & level == "mcnty"] # adjusted refers to misclassification ratio, 1 is all race for new race/ethn codes

# Add mcnty/location info to prepare for merge later
# Make each cause a column instead of a row
ylls_mcnty <- dcast.data.table(ylls_age, area + race + year + age + sex + level ~ paste0("yll_rate_", gsub("^\\_", "", acause)), value.var = "yll_mean")
ylls_mcnty <- merge(ylls_mcnty, unique(mcnty[, list(area, state)]), by = "area", all.x = TRUE)

merge_vars <-  intersect(names(ylls_mcnty), names(gbd))
final_mcnty <- merge(ylls_mcnty, gbd, all = TRUE, by = merge_vars)

rm(list = c("ylls_all_ls", "ylls_all", "ylls_age",  "ylls_mcnty"))

#########################################################################################################
############## SAE Indicators ###########################################################################
#########################################################################################################

#### Combine preds from SAE models

est_mcnty <- data.table()
for (i in 1:nrow(sae_indics)) {
  print(paste0("Reading in ", sae_indics[i, indicator], ": ", sae_indics[i, run_date_or_path]))
  
  current <- readRDS(paste0("FILEPATH"))

  #### Rename pred_mean
  setnames(current, "pred_mean", sae_indics[i, indicator])
  
  #### Drop unused variables
  current[, c("pred_lb", "pred_ub", "pred_se", "pred_median") := NULL]
  
  ##### Load settings file and restrict to gold standard source
  settings <- fread(paste0("FILEPATH"), header = FALSE)
  if ("source" %in% colnames(current)) {
    current <- current[source == str_replace_all(settings[V1 == "gold_standard_source", V2], '""', '')]
    current$source <- NULL
  }
  
  #### Subset
  current_mcnty <- current[level == "mcnty"]
  current_mcnty <- current_mcnty[!(age %in% c(98, 99))]
  current_mcnty <- current_mcnty[sex != 3]
  current_mcnty <- current_mcnty[race != 1]
  
  #### Combine with other preds
  if (nrow(est_mcnty) == 0) {
    est_mcnty <- copy(current_mcnty)
  } else {
    est_mcnty <- merge(est_mcnty, current_mcnty, by = c("level", "area", "year", "sex", "race", "age"), all = TRUE)
  }
}

est_mcnty <- merge(est_mcnty, unique(mcnty[, list(area, state)]), by = "area", all.x = TRUE)

########### Merge with GBD data and ylls
merge_vars <- intersect(names(final_mcnty), names(est_mcnty))
final_mcnty <- merge(est_mcnty, final_mcnty, by = merge_vars, all = TRUE)
rm("current", "current_mcnty", "est_mcnty")

### Race/ethn & mcnty-specific Other Covariates (hard-coded for now)
covs <- vector(mode = "list", length = nrow(cov_map))
for (i in 1:length(covs)) {
  covs[[i]] <- readRDS(paste0("FILEPATH"))
  covs[[i]][, c("race_set", "race_label") := NULL]
}

covariates_by_mcnty_1 <- Reduce(function(...) merge(..., by = c("mcnty", "year", "race"), all = TRUE), covs)

######## Non-race/ethn specific covariates + not modeled
best_run_dates <- fread(paste0(output_dir, "/best_run_dates.csv"))
cvs_path <- "FILEPATH"

pop_density <- readRDS(paste0(cvs_path, "/pop_density/", best_run_dates[indicator == "pop_density", run_date_or_path], ".rds"))

other_covs_mcnty <- merge(covariates_by_mcnty_1, pop_density, by = c("mcnty", "year"), all = TRUE)
other_covs_mcnty <- other_covs_mcnty[year %in% 2009:2019]
setnames(other_covs_mcnty, "mcnty", "area")

########### Merge with other data
merge_vars <- intersect(names(final_mcnty), names(other_covs_mcnty))
final_mcnty <- merge(final_mcnty, other_covs_mcnty, by = merge_vars, all = TRUE)
final_mcnty <- final_mcnty[year %in% 2009:2019]

saveRDS(final_mcnty, paste0(output_dir, "/model_data_0.rds"))
