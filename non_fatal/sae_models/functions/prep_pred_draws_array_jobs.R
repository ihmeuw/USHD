####################################################################################################
## Description: Prepare and submit array jobs for generating and summarizing prediction jobs.
##
## Inputs:      repo [character]            path to the code repository
##              settings_loc [character]    path to the settings file
##              dir [character]             path to the main output directory (data and plots)
##.             dir_draws_est [character]   path to the output directory for draws and predictions
##              sex [integer]               sex for predictions
##              race [integer]              race for predictions
##              validate [logical]          is this a validation run?
##              resub [logical]             is this a resubmission?
##              queue [character]           queue for submitting jobs
##              m_mem_free_pred [character] RAM request for prediction
##              m_mem_free_save [character] RAM request for save script
##              fthread [integer]           thread request for jobs
##              h_rt [character]            runtime request for jobs
##              draw_width [integer]        number of draws
##              p_draws_job [integer]       job ID of pred_draws job
##              by_sex [logical]            is the model run separately by sex 
##
## Outputs:     submitted jobs for prediction generation and summary estimates 
####################################################################################################

prep_draws_array_jobs <- function(repo, settings_loc, dir, sex, race, validate, resub, queue, m_mem_free_pred, 
                                  m_mem_free_save, fthread, h_rt, draw_width, p_draws_job, by_sex, dir_draws_est,
                                  archive = TRUE, shell = NULL,
                                  project = "PROJECT", pred_sub_code = "/complete_pred_subdraw.R", save_sub_code = "/save_pred.R") {
  get_settings(settings_loc)
  
  current_sex <- sex
  rm(sex)
  
  if (!(n.sims %% draw_width == 0)) stop(paste0("Draw width (", draw_width, ") does not divide cleanly into n.sims (", n.sims, ")"))
  
  sub_draws <- seq(1, n.sims, draw_width)
  draw_args <- data.table(start_draw = sub_draws, end_draw = sub_draws + (draw_width - 1))
  
  if (by_sex) {
    fwrite(draw_args, paste0(dir_draws_est,"/draw_args_", current_sex, "_", race, "_", edu, ".csv"))
  } else {
    fwrite(draw_args, paste0(dir_draws_est, "/draw_args_", race, "_", edu, ".csv"))
    fwrite(draw_args, paste0(dir_draws_est, "/draw_args_1_", race, "_", edu, ".csv"))
    fwrite(draw_args, paste0(dir_draws_est, "/draw_args_2_", race, "_", edu, ".csv"))
  }
  
  dir.create(paste0(dir_draws_est, "/draws"), showWarnings = FALSE)
  dir.create(paste0(dir_draws_est, "/est"), showWarnings = FALSE)
  
  ## Set queue
  auto_queue <- "QUEUE"
  
  draws_files <- c()
  est_files <- c()
  
  #### Determine expected draws and est files
  combos <- as.data.table(expand.grid("area_var" = area_var, "year" = years, "sex" = current_sex, "race" = races, "edu" = edu))
  combos[, draws_file := paste0("draws_", area_var, "_", year, "_", sex, "_", race, "_", edu, ".rds")]
  combos[, est_file := paste0("est_", area_var, "_", year, "_", sex, "_", race, "_", edu, ".rds")]
  
  #### Retrieve existing file names
  current_draws_files <- list.files(paste0(dir_draws_est, "/draws/"), pattern = CJ("draws_", area_var, "_", years, "_", current_sex, "_", races, "_", edu, ".rds")[, paste("draws_", area_var, "_", years, "_", current_sex, "_", races, "_", edu, ".rds", sep ="|")])
  current_est_files <- list.files(paste0(dir_draws_est, "/est/"), pattern = CJ("est", area_var, "_", years, "_", current_sex, "_", races, "_", edu, ".rds")[, paste("draws_", area_var, "_", years, "_", current_sex, "_", races, "_", edu, ".rds", sep ="|")])
  
  combos[, missing_draw_file := as.integer(!(draws_file %in% current_draws_files))]
  combos[, missing_est_file := as.integer(!(est_file %in% current_est_files))]
  # lists missing files
  rerun <- combos[missing_draw_file == 1 | missing_est_file == 1]
  
  # don't rerun if ALL expected files exists
  if (nrow(rerun) == 0) {
    message("No jobs to rerun")
    jids[sex == current_sex, pred_sub := as.numeric(NA)]
    jids[sex == current_sex, save_draws := as.numeric(NA)]
    return()
  }
  # If ANY expected files do NOT exist, rerun the whole array. 
  if (nrow(draw_args) > 0 & nrow(rerun) > 0) {
    message("Sending array job")
    id <- sbatch(code = paste0(repo, model_class, "/", pred_sub_code),
                 arguments = c(repo, dir, settings_loc, current_sex, race, validate, resub, by_sex, dir_draws_est, by_source, imp),
                 name = paste0("pred_sub_", basename(dir_draws_est), "_", current_sex, "_", race),
                 fthread = fthread, m_mem_free = m_mem_free_pred,
                 h_rt = h_rt, archive = TRUE,
                 project = project, queue = auto_queue,
                 sgeoutput = output_dir, sing_image = sing_image, array = paste0("1-", nrow(draw_args)), array_throttle = 100)
    Sys.sleep(30)
    
    jids[sex == current_sex, pred_sub := as.numeric(id)]
  } else {
    message("Nothing to run!")
    id <- NA
  }
  
  ## then launch the jobs that will save the results
  years_run <- years
  
  save_jobs <- data.table(year = years_run)
  if (by_sex) {
    fwrite(save_jobs, paste0(dir_draws_est, "/save_args_", current_sex, "_", race, "_", edu, ".csv"))
  } else {
    fwrite(save_jobs, paste0(dir_draws_est, "/save_args_", race, "_", edu, ".csv"))
    fwrite(save_jobs, paste0(dir_draws_est, "/save_args_1_", race, "_", edu, ".csv"))
    fwrite(save_jobs, paste0(dir_draws_est, "/save_args_2_", race, "_", edu, ".csv"))
  }
  
  if (by_sex) {
    if (nrow(save_jobs) > 0 & nrow(rerun) > 0) {
      message("Sending save array job")
      
      id2 <- sbatch(code = paste0(repo, model_class, "/", save_sub_code),
                    arguments = c(repo, dir, settings_loc, current_sex, race, draw_width, dir_draws_est),
                    name = paste0("save_draws_", basename(dir_draws_est), "_", current_sex, "_", race),
                    hold = id,
                    fthread = 1, m_mem_free = m_mem_free_save,
                    h_rt = h_rt, archive = TRUE,
                    project = project, queue = auto_queue,
                    sgeoutput = output_dir, sing_image = sing_image, array = paste0("1-", nrow(save_jobs)), array_throttle = 100)
      
    } else {
      message("Nothing to save!")
    }
  } else {
    for (s in 1:2) {
      message("Sending save array job")
      id2 <- sbatch(code = paste0(repo, model_class, "/", save_sub_code),
                    arguments = c(repo, dir, settings_loc, s, race, draw_width, dir_draws_est),
                    name = paste0("save_draws_", basename(dir_draws_est), "_", s, "_", race),
                    hold = id,
                    fthread = 1, m_mem_free = m_mem_free_save,
                    h_rt = h_rt, archive = TRUE,
                    project = project, queue = auto_queue,
                    sgeoutput = output_dir, sing_image = sing_image, array = paste0("1-", nrow(save_jobs)), array_throttle = 100)
    }
  }
  
  Sys.sleep(30)
  
  jids[sex == current_sex, save_draws := id2]
}
