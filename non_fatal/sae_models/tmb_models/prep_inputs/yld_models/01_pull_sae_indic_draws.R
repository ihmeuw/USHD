###############################################################################################################
## Description: Pull draws of each covariate in all-cause models that was modeled using SAE model (except YLLs).
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

sae_indics <- fread(paste0(output_dir, "/sae_indics_run_dates.csv"))

cat(paste0("Pulling draws for: ", sae_indics[task_id, indicator]))
all_draws <- list.files(paste0("FILEPATH"))
##### Get combos of all years, sexes, and races
years <- 2009:2019
sexes <- 1:2
races <-  c(5, 4, 6, 7, 2)
combos <- expand.grid(year = years, sex = sexes, race = races)
files <- paste("draws_mcnty", combos$year, combos$sex, combos$race, sep = "_")
files <- paste0(files, "_1.rds")
# not all files have all years
files_not_found <- files[!(files %in% all_draws)]
if (length(files_not_found) > 0) {
  cat(paste0("Following expected files not found: \n ", files_not_found))
}
all_draws <- all_draws[all_draws %in% files]
all_draws_full_name <- paste0("FILEPATH", sae_indics[task_id, run_date_or_path], "/draws/", all_draws, sep = "")


##### Read in all files (lapply slightly faster but more mem intensive; mclapply has random cores fail)
start.time <- Sys.time()
draws_ls <- lapply(all_draws_full_name, function(file) {
  print(file)
  temp <- readRDS(file)
  temp <- temp[sim %in% 1:sims]
  })
draws_dt <- rbindlist(draws_ls)
end.time <- Sys.time()
applied <- end.time - start.time
print(applied)

setnames(draws_dt, "pred", sae_indics[task_id, indicator])
draws_dt <- draws_dt[!(age %in% c(98, 99))]

##### Load settings file and restrict to gold standard source
settings <- fread(paste0("FILEPATH"), header = FALSE)
if ("source" %in% colnames(draws_dt)) {
  draws_dt <- draws_dt[source == str_replace_all(settings[V1 == "gold_standard_source", V2], '""', '')]
  draws_dt$source <- NULL
}

##### Save each sim separately to be combined with other covariates in that sim
foreach(i = 1:sims) %do% {
  cat(paste0("\n", i))
  dt_sim <- draws_dt[sim == i]
  file_name <- paste0("temp_draw_", i, "_", sae_indics[task_id, indicator], ".rds")
  saveRDS(dt_sim, file = paste0(output_dir, file_name))
  rm(dt_sim)
}
