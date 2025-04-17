####################################################################################################
## Description: Reads in the draws for a particular year, concatenates them, and saves the draws
##              and point estimates, confidence intervals, and standard errors for all areas/ages/.
##              Deletes the subdraw files once the final files are saved.
##
## Passed args: dir [character] -- home directory for settings and final output
##              sex [integer] -- sex to generate predictions for
##              race [integer] -- race to generate predictions for
##              draw_width [integer] -- width of the subdraw interval (e.g. 1-51 has a draw width of
##                50). NOTE: Assumes that this draw width is the same for all subdraws. If not, this
##                script will not work.
##
## Requires:    mx draws saved by subdraw for the year of interest:
##                [dir]/mx_est_[area_var]_[year]_[sex]_[race]_[max_subdraw].rds
##
## Outputs:     mx draws and estimates, saved in separate files by year:
##                "[dir]/mx_draws_[area_var]_[year]_[sex]_[race].rds"
##                "[dir]/mx_est_[area_var]_[year]_[sex]_[race].rds"
##
####################################################################################################

###### Load required libraries
pacman::p_load(R.utils, data.table, Matrix, splines, dplyr)

message(commandArgs())
###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (settings_loc <- commandArgs(TRUE)[[3]])
  (sex <- commandArgs(TRUE)[[4]])
  (race <- as.integer(commandArgs(TRUE)[5]))
  (draw_width <- as.integer(commandArgs(TRUE)[6]))
  (output_dir_draws_est <- commandArgs(TRUE)[7])
  (imp <- commandArgs(TRUE)[8])
}

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs[!funcs %in% "load_sae_shared.R"]) {
  source(paste0(repo, "/functions/", func))
}

message(commandArgs())

###### Assign settings from settings file
get_settings(settings_loc)

if (outcome == "pred_bmi") {
  dir_string <- ""
} else {
  dir_string <- paste0("_", imp)
}

if (!exists("save_ests_data_strat_only")) {
  save_ests_data_strat_only <- FALSE
}

set.seed(12345)

task_id <- ifelse(is.na(as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))), 1, as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID")))

ids <- fread(paste0(output_dir_draws_est, "/save_args_", sex, "_", race, "_", edu, ".csv")) 

this_year <- ids[task_id, year]

# load the age standard
std_wt <- readRDS(age_std_file)[, list(age, wt = wt / sum(wt))]

# function to save the draws at the end
save_draws <- function(draws, race_together_temp, expected_files, y, r = NULL, by_sex, save_ests_data_strat_only, var_name = "pred") {
  if (by_source) {
    merge_vars <- c("level", "area", "year", "sex", "race", "source", "sim")
  } else {
    merge_vars <- c("level", "area", "year", "sex", "race", "sim")  
  }
  
  merge_vars <- c(merge_vars[!(merge_vars == "sim")], "age")
  
  est <- collapse_draws(draws, var_name, merge_vars)
  
  if (race_together_temp) {
    saveRDS(draws[race == r], file = paste0(output_dir_draws_est, "/draws/draws_", area_var, "_", y, "_", sex, "_", r, dir_string, "_", edu, ".rds"))
    saveRDS(est[race == r], file = paste0(output_dir_draws_est, "/est/est_", area_var, "_", y, "_", sex, "_", r, dir_string, "_", edu, ".rds"))
  } else {
    saveRDS(draws, file = paste0(output_dir_draws_est, "/draws/draws_", area_var, "_", y, "_", sex, "_", race, dir_string, "_", edu, ".rds"))
    saveRDS(est, file = paste0(output_dir_draws_est, "/est/est_", area_var, "_", y, "_", sex, "_", race, dir_string, "_", edu, ".rds"))
  }
  
  # delete intermediate files
  sapply(paste0(output_dir_draws_est, "/draws/", expected_files), unlink)
}

sub_draws <- seq(1, n.sims, draw_width)

if (!race_together) { # Not yet tested
} else {
  for (r in races) {
    expected_files <- paste0("draws_", area_var, "_", this_year, "_", sex, "_", r, "_",
                             sub_draws + (draw_width - 1), dir_string, ".rds")

    message(paste0("Processing race ", r, " and year ", this_year))
    
    # read in the data
    all_draws <- tryCatch({
      lapply(paste0(output_dir_draws_est, "/draws/", expected_files), readRDS) %>% rbindlist(use.names = TRUE)
    }, error = function(cond) {
      message("Files not done saving because could not save")
      message(cond)
      return(NULL)
    })
    
    if(!is.null(all_draws)) {
      save_draws(all_draws, race_together, expected_files, y = this_year, r = r, by_sex = by_sex, save_ests_data_strat_only)
    }
  }
}

message(paste0("Done with ", this_year))
