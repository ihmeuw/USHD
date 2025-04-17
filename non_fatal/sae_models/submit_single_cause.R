###############################################################################################################
## Description: Submit nonfatal or risk factor model run.
##
## Passed args: repo [character] -- location of nonfatal repository
##              output_dir [character] -- location for model outputs (data and plots)
##              output_dir_draws_est [character] -- location for model outputs (draws and est files)
##              settings_loc [character] -- file path of settings file
##              queue [character] -- submission queue for sbatch jobs
##              sing_image [character] -- Singularity image for sbatch jobs
##
## Requires:    N/A
##
## Outputs:     submitted jobs for all processes required to estimate nonfatal or risk factor burden.
##              "runtime_info.txt" which includes information about provided settings, system
##                settings, and job IDs for submitted jobs.
##
###############################################################################################################

###############################################################################################################
########## 1. Initial setup ##########
###############################################################################################################
###### Load required libraries
pacman::p_load(R.utils, data.table, stringr)

###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (output_dir_draws_est <- commandArgs(TRUE)[[3]])
  (settings_loc <- commandArgs(TRUE)[[4]])
  (queue <- commandArgs(TRUE)[[5]])
  (sing_image <- commandArgs(TRUE)[[6]])
  (initial_fit_for_starting_values <- as.logical(commandArgs(TRUE)[[7]]))
  (run_date_hold <- commandArgs(TRUE)[[8]])
} else {
  run_date_hold <- run_date
}

message(paste0("Run date: ", run_date_hold))

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

###### Assign settings from settings file
get_settings(settings_loc)

message('print sys.getenv')
print(Sys.getenv("SLURM_ARRAY_TASK_ID"))

if (!exists("n.imp")) {
  n.imp <- 0
}

if (!interactive()) {
  message('code imp')
  imp <- ifelse(is.na(as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))), 0, as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID")))
  print(imp)
} else {
  imp <- 1
}

if (n.imp == 0 | initial_fit_for_starting_values == "TRUE") {
  imp <- 0
}

if (!exists("prep_inputs_file")) {
  prep_inputs_file <- "prep_inputs.R"
}

if (!exists("hold_post_estimation")) {
  hold_post_estimation <- FALSE
}

if (!exists("by_sex")) {
  by_sex <- TRUE
}

if (!exists("fit_model_only")) {
  fit_model_only <- FALSE
}

if (!exists("fit_model_for_starting_values")) {
  fit_model_for_starting_values <- FALSE
}

if (!exists("initial_fit_for_starting_values")) {
  initial_fit_for_starting_values <- FALSE
}

if (n.imp > 0) {
  output_dir_draws_est <- paste0(output_dir_draws_est, "/imputation", imp)
  output_dir <- paste0(output_dir, "/imputation", imp)
  dir.create(output_dir)
  dir.create(output_dir_draws_est)
}

###### Output git status
get_git_status(repo = paste0("FILEPATH"), repo_name = "non_fatal", show_diff = TRUE)

###### Create a table for holding job ids
jids <- CJ(level = c(area_var, names(geoagg_files)), year = years, sex = c(sexes, 3), race = unique(c(races, 1)), imp = imp)

if (!is.null(geoagg_files)) { # we may not be able to aggregate in all years, so year-level combinations with no crosswalk need to be removed.
  for (this_level in names(geoagg_files)) {
    weights <- readRDS(geoagg_files[this_level])
    if ("year" %in% names(weights)) {
      jids <- jids[level != this_level | year %in% unique(weights$year),]
    }
    rm(weights)
  }
}

if (!exists("fit_model_for_starting_values")) {
  fit_model_for_starting_values <- FALSE
}

###### Submit stage 1 jobs (data prep, models)
if (type %in% c("models_only", "all", "validation")) {
  ## Set queue
  auto_queue <- "..."
  
  ## Prep input data
  jids[, "prep_inputs"] <- sbatch(code = paste0(repo, model_class, "/prep_inputs/", prep_inputs_file),
                                  name = "prep_inputs",
                                  arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, imp),
                                  fthread = 1, m_mem_free = "300G", h_rt = "0-04:00:00", archive = TRUE,
                                  skip_if_exists = if (resub) paste0(output_dir, "/data_", imp, ".rds"),
                                  queue = auto_queue, sgeoutput = output_dir,
                                  sing_image = sing_image)
  
  Sys.sleep(30)
  
  ## Set queue
  auto_queue <- "..."
  
  if (race_together) {
    if (by_sex) {
      jids[sex != 3 & (!by_race | race != 1),
           fit_mod := sbatch(code = paste0(repo, model_class, "/models/fit_mod_", model, ".R"),
                             name = paste0("fit_mod_", model, "_", sex), 
                             arguments = c(repo, output_dir, settings_loc, sex, initial_fit_for_starting_values, imp, paste0(main_output_dir, "/", run_date_hold)),
                             hold = na.omit(unique(c(prep_inputs))),
                             fthread = fthread_mod, m_mem_free = m_mem_free_mod, queue = auto_queue, h_rt = h_rt_mod,
                             skip_if_exists = if (resub) paste0(output_dir, "/model_fit_", sex, "_", imp, ".rds"),
                             archive = FALSE, intel = T, project = "PROJECT",
                             mkl_threads = fthread_mod, omp_threads = 1, # fastest combination of threads
                             shell = "FILEPATH",
                             sing_image = "FILEPATH",
                             sgeoutput = output_dir,
                             type = "TMB"), by = c("sex", "imp")]
      Sys.sleep(30)      
    } else {
      ## Set queue
      auto_queue <- "..."
      
      jids[sex != 3 & (!by_race | race != 1), "fit_mod"] <- sbatch(code = paste0(repo, model_class, "/models/fit_mod_", model, ".R"),
                                                                   name = paste0("fit_mod_", model), 
                                                                   arguments = c(repo, output_dir, settings_loc, "NULL", initial_fit_for_starting_values, imp, run_date_hold),
                                                                   hold = na.omit(unique(c(jids[sex != 3 & (!by_race | race != 1)]$prep_inputs))),
                                                                   fthread = fthread_mod, m_mem_free = m_mem_free_mod, queue = auto_queue, h_rt = h_rt_mod,
                                                                   skip_if_exists = if (resub) paste0(output_dir, "/model_fit_", sex, "_", imp, ".rds"),
                                                                   archive = FALSE, intel = T, project = "PROJECT",
                                                                   mkl_threads = fthread_mod, omp_threads = 1, # fastest combination of threads
                                                                   shell = "FILEPATH",
                                                                   sing_image = "FILEPATH",
                                                                   sgeoutput = output_dir,
                                                                   type = "TMB")
      
      Sys.sleep(30)
    }
  }
}

while (!(file.exists(paste0(output_dir, "/model_fit_time_", 1, "_", imp, ".csv")) & file.exists(paste0(output_dir, "/model_fit_time_", 2, "_", imp, ".csv")))) {
  Sys.sleep(120)
}

if (initial_fit_for_starting_values) {
  cat("\nQuitting because predictions should not be run for initial fits")
  saveRDS(jids, paste0(output_dir, '/submit_single_cause_jids_', type, '_', imp, '.rds'))
  quit()
} else if (fit_model_only) {
  cat("\nQuitting because only model fitting was requested")
  quit()
}

## Arguments for the prediction array job
m_mem_free_pred <- 50
if (by_race) m_mem_free_pred <- m_mem_free_pred * 3

## Arguments for the save array jobs
m_mem_free_save <- 15
if (by_race) m_mem_free_save <- m_mem_free_save * 3

## Common arguments across both array jobs
fthread_array <- 4
h_rt_array <- "0-04:00:00"

# Predict area-sex-level (by sex, race)
if (race_together) {
  # put a dummy in for the race argument
  # and skips if files for all races exist
  mem_pred <- 20
  if (by_race) mem_pred <- mem_pred * 10
  races_submit <- 99 # used for the array jobs
  
  if (by_sex) {
    ## Set queue
    auto_queue <- "..."
    
    jids[sex != 3 & (!by_race | race != 9),
         pred_draws := sbatch(code = paste0(repo, model_class, "/pred_draws.R"),
                              arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, sex, races_submit, validate, resub, by_sex, draw_width, imp),
                              name = paste0("pred_draws_", sex, "_", imp),
                              hold = na.omit(unique(fit_mod)),
                              skip_if_exists = if (resub) paste0(output_dir_draws_est, "/initial_sims_", sex, "_99_", imp, "_1.rds"),
                              fthread = 4, m_mem_free = paste0(mem_pred, "G"),
                              h_rt = "4:00:00", archive = FALSE,
                              project = "PROJECT", queue = auto_queue,
                              sgeoutput = output_dir, sing_image = sing_image), by = c("sex", "imp")]
  } else {
    ## Set queue
    auto_queue <- "..."
    
    jids[sex != 3 & (!by_race | race != 9),
         pred_draws := sbatch(code = paste0(repo, model_class, "/pred_draws.R"),
                              arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, 99, races_submit, validate, resub, by_sex, draw_width, imp),
                              name = paste0("pred_draws_", imp),
                              hold = na.omit(unique(fit_mod)),
                              skip_if_exists = if (resub) paste0(output_dir_draws_est, "/initial_sims_99.rds"),
                              fthread = 4, m_mem_free = paste0(mem_pred, "G"),
                              h_rt = "4:00:00", archive = FALSE,
                              project = "PROJECT", queue = auto_queue,
                              sgeoutput = output_dir, sing_image = sing_image), by = c("imp")]
  }
}