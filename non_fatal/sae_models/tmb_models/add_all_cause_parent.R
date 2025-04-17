####################################################################################################
## Description: Launch script for adding separate YLD models that are collectively exhaustive of
##              all-cause YLDs to get final estimates of all-cause YLDs, created compiled file and
##              plots of the results
##
## Passed args: output_dir [character] -- home directory for settings and final output
##              output_dir_draws_est [character] -- same as output dir
##              run_date_file [character] -- location of best run dates for causes that make up all YLDs
##              settings_file [character] -- settings file for 
##              output_dir [character] -- home directory for settings and final output
##
## Outputs:     all-cause YLD estimates in output_dir / save_dir
##              plots of results in output_dir / save dir
####################################################################################################

rm(list = ls())

library(data.table)
###### Source functions
repo <- paste0("FILEPATH")
funcs <- list.files(paste0(repo, "/functions/"))

for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

## Settings file has little information except run date, geoagg files and other things necessary to use for the
## compile estimates script

settings_path <- "FILEPATH"
settings_file <- "all_cause_settings_20231004"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc)

###### Generate run date and create model output folder
if (is.null(run_date)) {
  run_date <- make_time_stamp()
}

message(run_date)
output_dir <- paste0(main_output_dir, run_date)
output_dir_draws_est <- paste0(draws_est_output_dir, run_date)
message(output_dir)
message(output_dir_draws_est)
dir.create(output_dir)
dir.create(output_dir_draws_est)
dir.create(paste0(output_dir_draws_est, "/draws/"))
dir.create(paste0(output_dir_draws_est, "/est/"))

## Read and save file of best run date for each cause to save directory
run_date_file <- fread("FILEPATH")
run_date_file_loc <- paste0(output_dir, "/model_run_dates.csv")
fwrite(run_date_file, file = run_date_file_loc)

n.imp <-  0

for (i in 1:nrow(run_date_file)) {
  
  cat(paste0("\nStarting: ", run_date_file[i, outcome]))
  settings <- as.data.table(read.csv(paste0(main_output_dir,"/", run_date_file[i, run_date],"/settings.csv"), stringsAsFactors = F, header = F))
  
  if (unique(settings[V1 == "outcome", V2]) != run_date_file[i, outcome]) {
    stop("\nRun dates don't match outcome in settings file")
  }

  if (unique(settings[V1 == "outcome", V2]) != run_date_file[i, outcome]) {
    stop("\nRun dates don't match outcome in settings file")
  }
  if (!file.exists(paste0(main_output_dir,"/", run_date_file[i, run_date], "/imputation0/est/pred_est_all.rds"))) {
    print(run_date_file[i, run_date])
    cat("\npred_est_all.rds does not exist -> not all estimates exist for this cause")
  }
  stopifnot(file.exists(paste0(main_output_dir,"/", run_date_file[i, run_date], "/imputation0/est/pred_est_all.rds")))
}


##################### Create parameter map for array jobs and launch all scripts

## Fill out geo-levels for which to calculate _all YLD rates - this is the array job param map
geos <- c("mcnty", "natl", "state")

combos_path <- paste0(output_dir, "/strata_combos.csv")
strata_combos <- data.table(expand.grid("sex"= c(sexes, 3), "race" = unique(c(races, 1)), "year" = years, 
                                               "geo" = geos))
strata_combos[, task_num := 1:.N]
fwrite(strata_combos, file = combos_path)

## Array job settings
memory <- 15
if (by_race & n.sims == 1000) {
  memory <- memory * 20
}
threads <- 4
script <- paste0(repo, '/tmb_models/add_all_cause_child.R')
n_jobs <- nrow(strata_combos)
h_rt <- "0-04:00:00"
archive <- TRUE

## Necessary to set imp to 0, required argument
## But all-cause YLDs will never be run with imputations because it's not
## directly modeled here, but added from several modeled causes 
imp <- 0

## Set queue
auto_queue <- set_queue_dynamically(queue = queue, fthread = threads, m_mem_free = paste0(memory, "G"), h_rt = h_rt, archive = TRUE, priority = "RAM")

## Launch job to add causes to all-cause YLDs
add_jid <- sbatch(code = script,
       arguments = c(output_dir, combos_path, run_date_file_loc, repo, imp),
       name = "child_add_all_cause",
       fthread = threads, m_mem_free = paste0(memory, "G"),
       h_rt = h_rt, archive = archive,
       sgeoutput = output_dir,
       array = paste0("1-", n_jobs), array_throttle = 50, 
       queue = auto_queue)

## Launch jobs to create compiled file
mem_comp <- 15
if (by_race & n.sims == 1000) {
  mem_comp <- mem_comp * 3
}
var_name <- "yld"

compile_jid <- sbatch(code = paste0(repo, "post_estimation/compile_estimates.R"),
                         arguments = c(repo, output_dir, settings_loc, output_dir_draws_est, FALSE, var_name, imp),
                         name = paste0("compile_estimates"),
                         hold = add_jid,
                         fthread = 2, m_mem_free = paste0(mem_comp,"G"), h_rt = "01:00:00", archive = TRUE,
                         project = "PROJECT", queue = "...",
                         sgeoutput = output_dir, sing_image = sing_image)

## Launch plotting script
sbatch(code = paste0(repo, "diagnostics/plot_state_model_results_parent.R"),
       arguments = c(repo, output_dir, settings_loc, output_dir_draws_est, imp),
       name = paste0("parent_results"),
       hold = compile_jid,
       fthread = 2, m_mem_free = paste0("5G"), h_rt = "01:00:00", archive = FALSE,
       project = "PROJECT", queue = "...",
       sgeoutput = output_dir, sing_image = sing_image)


## Launch plotting script
sbatch(code = paste0(repo, "diagnostics/check_extreme_estimates.R"),
       arguments = c(repo, output_dir, settings_loc, output_dir_draws_est, imp),
       name = paste0("extreme_plots"),
       hold = compile_jid,
       fthread = 5, m_mem_free = paste0("15G"), h_rt = "01:00:00", archive = FALSE,
       project = "PROJECT", queue = "...",
       sgeoutput = output_dir, sing_image = sing_image)

#### Plot maps and GBD compare
## Set queue
auto_queue <- set_queue_dynamically(queue = queue, fthread = 8, m_mem_free = "100G", h_rt = "0-04:00:00", archive = TRUE, priority = "RAM")

sbatch(code = paste0(repo, "diagnostics/plot_maps_and_gbd_compare.R"),
                              arguments = c(repo, output_dir, settings_loc, "NULL", output_dir_draws_est, TRUE, imp),
                              name = "plot_maps",
                              #hold = compile_jid,
                              fthread = 8, m_mem_free = "100G",
                              h_rt = "04:00:00", archive = T,
                              project = "PROJECT", queue = auto_queue,
                              sgeoutput = output_dir, sing_image = sing_image)
