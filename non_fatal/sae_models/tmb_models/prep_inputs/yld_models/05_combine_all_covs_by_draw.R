library(tidyverse)
library(data.table)

###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (sims <- commandArgs(TRUE)[[3]])
}

###### Subset to particular task ID
task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

#########################################################################################################
############## Self-reported health and disability vars #################################################
#########################################################################################################

sae_indics <- fread(paste0(output_dir, "/sae_indics_run_dates.csv"))
sae_indics <- sae_indics$indicator

file_names <- paste0(output_dir, "/temp_draw_", task_id, "_", sae_indics, ".rds")

sae_indics <- lapply(file_names, readRDS)
sae_indics <- Reduce(function(...) merge(..., all = TRUE), sae_indics)

#########################################################################################################
############## GBD YLDs #################################################################################
#########################################################################################################

# create param map
included_causes <- c("inj_trans", "_unintent", "_intent", "_subs", "diab_ckd",
                     "_neuro", "_comm", "_neo", "cvd", "resp", "digest", "_mental", "skin", "_sense", "msk", "_otherncd")

file_names <- paste0(output_dir, "/temp_draw_", task_id, "_yld_rate_", gsub("^\\_", "", included_causes), ".rds")
ylds <- lapply(file_names, readRDS)
ylds <- Reduce(function(...) merge(..., all = TRUE), ylds)

#########################################################################################################
############## USHD YLLs ################################################################################
#########################################################################################################

## no ylls currently available for mental disorders (not modeled by ushd) or sense organ diseases (NF only)
included_causes <- included_causes[!(included_causes %in% c("_mental", "_sense"))]
file_names <- paste0(output_dir, "/temp_draw_yll_rate_", task_id, "_", gsub("^\\_", "", included_causes), ".rds")
ylls <- lapply(file_names, readRDS)
for (i in 1:length(ylls)) {
  setnames(ylls[i][[1]], "yll", paste0("yll_rate_", gsub("^\\_", "", ylls[i][[1]][1, acause])))
  ylls[i][[1]][, acause := NULL]
}

ylls <- Reduce(function(...) merge(..., all = TRUE), ylls)

#########################################################################################################
############## Sociodemographic Covariates ##############################################################
#########################################################################################################

sd_covs <- fread(paste0(output_dir, "/cov_map.csv"))
sd_covs <- gsub("_by_race_ethn", "", sd_covs$indicator)

file_names <- paste0(output_dir, "/temp_draw_", task_id, "_", sd_covs, ".rds")

sd_covs <- lapply(file_names, readRDS)
sd_covs <- Reduce(function(...) merge(..., all = TRUE), sd_covs)
colnames(sd_covs) <- gsub("_by_race_ethn", "", colnames(sd_covs))

#########################################################################################################
############## Other Sociodemographic Covariates (not modeled so no draws) ##############################
#########################################################################################################

######## Non-race/ethn specific covariates, not modeled
best_run_dates <- fread(paste0(output_dir, "/best_run_dates.csv"))
cvs_path <- "FILEPATH"

pop_density <- readRDS(paste0(cvs_path, "/pop_density/", best_run_dates[indicator == "pop_density", run_date_or_path], ".rds"))

setnames(pop_density, "mcnty", "area")
pop_density[, level := "mcnty"]

#########################################################################################################
############## Merge all covariates into one dataset ####################################################
#########################################################################################################

#### First merge sex, age, race/ethn, year, mcnty-specific files
final_dt <- merge(ylls, sae_indics, by = c("year", "age", "sex", "area", "level","race", "sim"), all = TRUE)

#### Add state metadata
mcnty <- fread("FILEPATH")
setnames(mcnty, "mcnty", "area") # to match predictions
final_dt <- merge(final_dt, unique(mcnty[, list(area, state)]), by = "area", all.x = TRUE)

#### Now merge on sex, age, year, STATE-specific ylds
final_dt <- merge(final_dt, ylds, by = c("state", "year", "age", "sex", "sim"), all.x = TRUE)

#### Now merge on race/ethn, year, mcnty-specific sociodemographic covariates
final_dt <- merge(final_dt, sd_covs, by = c("year", "area", "level", "race", "sim"), all.x = TRUE)

#### Finally merge on year, mcnty-specific sociodemographic covariates
final_dt <- merge(final_dt, pop_density, by = c("year", "area", "level"), all.x = TRUE)

#### Check for excpected number of rows -> 2009-2019, 2 sexes, 5 race/ethn groups, age bins 0, 1, 5, 10...85, 3110 mcnties
expected <- length(2009:2019)*2*5*length(c(0, 1, seq(5, 85, 5)))*3110
stopifnot(nrow(final_dt) == expected)

#### Save
saveRDS(final_dt, paste0(output_dir, "/model_data_", task_id, ".rds"))
