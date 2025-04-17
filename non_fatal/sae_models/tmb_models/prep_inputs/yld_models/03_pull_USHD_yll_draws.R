###############################################################################################################
## Description: Pull draws of YLL covariates for all-cause YLD models
##              Child script of pull draws
###############################################################################################################

###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (sims <- commandArgs(TRUE)[[3]])
}


###### Load libraries and functions
pacman::p_load(data.table, tidyverse, ggplot2, foreach, doParallel, tidyr, broom, INLA, doParallel)
source("FILEPATH")
source("FILEPATH")


###### Subset to particular task ID
task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
sims <- as.integer(sims)

yll_dir <- "FILEPATH"
causes <- c("inj_trans", "_unintent", "_intent", "_subs", "diab_ckd",
            "_neuro", "_comm", "_neo", "cvd", "resp", "digest", "skin", "msk", "_otherncd")

yll_cause <- causes[task_id]
cat(paste0("Pull results for ", yll_cause, "\n"))
all_draws <- list.files(paste0(yll_dir, yll_cause), pattern = "yll_draws_mcnty*")

##### Get combos of all years, sexes, and races (mortality uses new race vars)
years <- 2009:2019
sexes <- 1:2
races <-  c(5, 4, 6, 7, 2)
combos <- expand.grid(year = years, sex = sexes, race = races)
files <- paste("yll_draws_mcnty", combos$year, combos$sex, combos$race, sep = "_")
files <- paste0(files, "_1_raked.rds") # last 1 indicates raked?
files_not_found <- files[!(files %in% all_draws)]
cat(paste0("Warning: following expected files not found: \n ", files_not_found))
all_draws <- all_draws[all_draws %in% files]
all_draws_full_name <- paste0(yll_dir, yll_cause, "/", all_draws, sep = "")

##### Read in all files (lapply slightly faster but more mem intensive; mclapply has random cores fail)
cat("\nReading in files\n")
start.time <- Sys.time()
draws_ls <- lapply(all_draws_full_name, function(x) {
  message(x)
  draws <- readRDS(x)
  draws <- draws[sim %in% 1:sims]
  draws <- draws[!(age %in% c(98, 99))]
  })
draws_dt <- rbindlist(draws_ls)
end.time <- Sys.time()
applied <- end.time - start.time
print(applied)
rm(draws_ls); gc()

##### Save each sim separately to be combined with other covariates in that sim
registerDoParallel(cores = 4)
foreach(i = 1:sims) %dopar% {
  dt_sim <- draws_dt[sim == i]
  file_name <- paste0("temp_draw_yll_rate_", i, "_", gsub("^\\_", "", yll_cause), ".rds")
  saveRDS(dt_sim, file = paste0(output_dir, file_name))
  rm(dt_sim)
}

