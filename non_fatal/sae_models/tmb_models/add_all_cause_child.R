####################################################################################################
## Description: Launch script for adding separate YLD models that are collectively exhaustive of
##              all-cause YLDs to get final estimates of all-cause YLDs, created compiled file and
##              plots of the results
##
## Passed args: output_dir [character] -- home directory for settings and final output
##              save_dir [character] -- same as output dir
##              run_date_file [character] -- location of best run dates for causes that make up all YLDs
##              settings_file [character] -- settings file for all-cause YLDs, necessary for compile and collapse scripts
##              imp [character] -- should be 0
##
## Outputs:     all-cause YLD estimates in output_dir / save_dir
##              plots of results in output_dir / save dir
####################################################################################################

###### Load libraries
library(data.table)
library(parallel)
library(stringr)
library(matrixStats)
library(doParallel)
library(foreach)

###### Set up objects from command args
args <- commandArgs(trailingOnly = TRUE)
save_dir <- args[1]
combos_path <- args[2]
run_date_file <- args[3]
repo <- args[4]
imp <- as.integer(args[5])

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

###### Array job settings
print("Working")
strata_combos <- fread(combos_path)
task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
sex_i <- strata_combos[task_num == task_id, sex]
race_i <- strata_combos[task_num == task_id, race]
year_i <- strata_combos[task_num == task_id, year]
geo_i <- strata_combos[task_num == task_id, geo]


cat(paste0("Producing all-cause YLDs by ", geo_i," for year ", year_i, ", sex ", sex_i, ", and race ", race_i))
run_dates <- fread(run_date_file)
###### Read in files
all_draws <- lapply(1:nrow(run_dates), function (i) {
  print(i)
  draws <- readRDS(paste0("FILEPATH"))
  
  #### Renumber sims
  draws$sim <- as.integer(as.factor(draws$sim))
  
  stopifnot(length(unique(draws$sim)) == 1000)
  setkeyv(draws, c("level", "area", "year","sex", "race", "age", "sim"))
  setnames(draws, "pred", run_dates[i, outcome])
})

###### Combine and add together
cat("\n Merging data tables")
all_draws <- Reduce(function(...) merge(..., all=TRUE, by = c("level", "area", "year", "sex", "race", "age", "sim")), all_draws)
cat(paste0("Adding together: ", paste0(run_dates$outcome, collapse = " + ")))

if (length(run_dates[outcome %in% names(all_draws), outcome]) != nrow(run_dates)) {
  stop("Error: not all outcomes are in data.table to be added together")
}

## YLDs are NA for msk under 5
all_draws[age < 5, eval(parse(text = paste0("yld := ", paste0(run_dates$outcome[run_dates$outcome != "yld_rate_msk"], collapse = " + "))))]
all_draws[age >= 5, eval(parse(text = paste0("yld := ", paste0(run_dates$outcome, collapse = " + "))))]

###### Collapse draws into summary est
merge_vars <- c("level", "area", "year", "sex", "race", "age", "sim")
est <- collapse_draws(all_draws, "yld", merge_vars[!(merge_vars == "sim")])

###### Add GBD age_group_id (requested)
age_group_ids <- fread("FILEPATH")
age_group_ids$age_end <- NULL
setnames(age_group_ids, "age_start", "age")
all_draws <- merge(all_draws, age_group_ids, by = "age", all.x = T)
all_draws[age == 98, age_group_id := 22]
all_draws[age == 99, age_group_id := 27]
est <- merge(est, age_group_ids, by = "age", all.x = T)
est[age == 98, age_group_id := 22]
est[age == 99, age_group_id := 27]

###### Delete unnecessary columns
keep_vars <- c("level", "area", "year", "sex", "race", "age", "age_group_id","sim", "yld")
all_draws <- all_draws[, ..keep_vars]

###### Save files
## last 1 refers to education
race_i <- unique(est$race)
saveRDS(all_draws, paste0(save_dir, "/draws/yld_draws_", geo_i, "_", year_i ,"_", sex_i, "_", race_i, "_1.rds"))
saveRDS(est, paste0(save_dir, "/est/yld_est_", geo_i,"_", year_i, "_", sex_i, "_", race_i, "_1.rds"))
