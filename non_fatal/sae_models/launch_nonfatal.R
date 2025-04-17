###############################################################################################################
## Description: Launches a nonfatal model run.
###############################################################################################################

library(data.table)

if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (save_loc <- commandArgs(TRUE)[[2]])
  
  load(save_loc)
  
  task <- ifelse(is.na(as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))), 0, as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID")))
  settings_file <- settings_files[task]
} else {
  repo <- paste0("FILEPATH")
  settings_file <- "SETTINGS FILE"
}

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))

for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

###### Set data location and settings locations, and read in settings
settings_path <- "FILEPATH"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc)

###### Generate run date and create model output folder
if (!exists("run_date")) {
  if (is.null(run_date)) {
    run_date <- make_time_stamp()
  }
} else if (is.null(run_date) | !resub) {
  run_date <- make_time_stamp()
}

message(run_date)
output_dir <- paste0(main_output_dir, run_date)
output_dir_draws_est <- paste0(draws_est_output_dir, run_date)
message(output_dir)
message(output_dir_draws_est)
dir.create(output_dir)
dir.create(output_dir_draws_est)

## Retrieve and save settings file
file.copy(settings_loc, c(paste0(output_dir, "/settings.csv")), overwrite = TRUE)
file.copy(settings_loc, c(paste0(output_dir_draws_est, "/settings.csv")), overwrite = TRUE)

if (n.imp == 0 && !fit_model_for_starting_values) { ## Launch models that don't use imputations and are NOT fit for starting values
  imp <- 0
  auto_queue <- "..."
  
  sbatch(code = paste0(repo, "/submit_single_cause.R"), name = paste0("nonfatal_model_", run_date), 
         arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, auto_queue, sing_image, initial_fit_for_starting_values, run_date),
         queue = auto_queue, fthread = 1, m_mem_free = "10G", h_rt = "1-00:00:00", archive = TRUE, 
         sgeoutput = output_dir, sing_image = sing_image)
} else if (n.imp > 0 && fit_model_for_starting_values) { ## Launch models that use imputations and are fit for starting values
  # Launch model that will be used for starting values
  auto_queue <- "..."
  
  initial_fit_for_starting_values <- TRUE
  starting_vals_hold <- sbatch(code = paste0(repo, "/submit_single_cause.R"),
                               name = paste0("starting_val_nonfatal_model_", run_date), 
                               arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, auto_queue, sing_image, initial_fit_for_starting_values, run_date), 
                               queue = auto_queue, fthread = 1, m_mem_free = "1G", h_rt = "1-00:00:00", archive = TRUE, 
                               sgeoutput = output_dir, sing_image = sing_image)
  
  Sys.sleep(30)
  
  flag <- 0
  while (flag == 0) {
    stats <- fread(text = system(paste0("squeue --me --format \"%F,%j,%t,%K\""), intern = TRUE))
    job_stats <- stats[ARRAY_JOB_ID == starting_vals_hold]
    if (nrow(job_stats) == 0) {
      flag <- 1
    } else {
      Sys.sleep(300)
    }
  }
  
  # Launch all other models
  auto_queue <- "..."
  
  initial_fit_for_starting_values <- FALSE
  imphold <- sbatch(code = paste0(repo, "/submit_single_cause.R"), 
                    name = paste0("nonfatal_model_", run_date),
                    hold = starting_vals_hold,
                    arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, auto_queue, sing_image, initial_fit_for_starting_values, run_date),
                    queue = auto_queue, fthread = 1, m_mem_free = "1G", h_rt = "1-00:00:00", archive = TRUE,
                    sgeoutput = output_dir, sing_image = sing_image, array = paste0("1-", n.imp), array_throttle = 10)
  
  Sys.sleep(30)
  
  flag <- 0
  while (flag == 0) {
    stats <- fread(text = system(paste0("squeue --me --format \"%F,%j,%t,%K\""), intern = TRUE))
    job_stats <- stats[ARRAY_JOB_ID == imphold]
    if (nrow(job_stats) == 0) {
      flag <- 1
    } else {
      Sys.sleep(300)
    }
  }
  
  # Combine draw-models
  if (!fit_model_only) {
    auto_queue <- "..."
    
    output_dir <- paste0(output_dir, "/imputation0/")
    output_dir_draws_est <- paste0(output_dir_draws_est, "/imputation0/")
    combine_id <- sbatch(code = paste0(repo, "/submit_combine_imputations.R"),
           name = paste0("submit_imp_", run_date),
           hold = imphold,
           arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, queue, sing_image),
           queue = auto_queue, fthread = 1, m_mem_free = "1G", h_rt = "1-00:00:00", archive = FALSE,
           sgeoutput = output_dir, sing_image = sing_image)
    
  }
} else if (n.imp > 0 && !cross_val) { # launch models that are NOT fit for starting values but use imputations
  
  initial_fit_for_starting_values <- FALSE
  imphold <- sbatch(code = paste0(repo, "/submit_single_cause.R"), name = paste0("nonfatal_model_", run_date), 
                    arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, queue, sing_image), 
                    queue = auto_queue, fthread = 1, m_mem_free = "10G", h_rt = "1-00:00:00", archive = TRUE, 
                    sgeoutput = output_dir, sing_image = sing_image, array = paste0("1-", n.imp), array_throttle = 5)
  
  if (!fit_model_only) {
    sbatch(code = paste0(repo, "/submit_combine_imputations.R"),
           name = paste0("submit_imp_", run_date),
           hold = imphold,
           arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, queue, sing_image),
           queue = auto_queue, fthread = 1, m_mem_free = "20G", h_rt = "1-00:00:00", archive = FALSE,
           sgeoutput = output_dir, sing_image = sing_image)
  }
} else if (cross_val) {
  initial_fit_for_starting_values <- FALSE
  imphold <- sbatch(code = paste0(repo, "/submit_single_cause.R"), name = paste0("nonfatal_model_", run_date), 
                    arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, queue, sing_image, initial_fit_for_starting_values), 
                    queue = queue, fthread = 1, m_mem_free = "10G", h_rt = "1-00:00:00", archive = TRUE, 
                    sgeoutput = output_dir, sing_image = sing_image, array = paste0("1-", n.imp), array_throttle = 5)
}
