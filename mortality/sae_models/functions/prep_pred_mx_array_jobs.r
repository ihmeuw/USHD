####################################################################################################
## Description: submit array jobs that predict mortality rates and save results for subsets of draws
##
## Passed args: dir [character] -- home directory for settings and final output
##              root [character] -- path to directory of all models
##              sex [integer] -- sex to generate predictions for
##              race [integer] -- race to generate predictions for. NOTE: this can be a dummy value
##                of 99 if race_together was TRUE in _submit_single_cause.r
##              edu [integer] -- education to generate predictions for.
##              validate [logical] -- is this a validation model? if so, only mx draws for
##                areas in the validation set are created
##              resub [logical] -- are you resubmitting? if yes, it will not re-save files that already
##                exist (or where the final file with all draws exists)
##              queue [string] -- queue for the array jobs
##              m_mem_free_pred [string] -- memory allocation for the array job that predicts mx
##              m_mem_free_save [string] -- memory allocation for the array job that concatenates
##                and saves the mx draws
##              fthread [integer] -- threads for both array jobs
##              h_rt [string] -- run time for both array jobs (backfilling not in place so this doesn't)
##                really matter
##              draw_width [integer] -- length of the segment of draws
##              p_mx_job [integer] -- list of job_ids for pred_mx, for this sex and race
##
## Requires:    fitted model object ("[dir]/model_fit_[sex]_[race]_[edu].rds")
##              prepped data file ("FILEPATH) from limited use directory
##              file specifying areas in the validation set, if validate is T (gs_file)
##              populations (pop_file)
##              age standard file (age_std_file)
##
## Outputs:     random effect draws: [dir]/initial_sims_[sex]_[race]_[edu].rds
##              fixed effect draws: [dir]/fe_sims_[sex]_[race]_[edu].rds
##              labels for the REs: [dir]/mean_[sex]_[race]_[edu].rds
##
####################################################################################################

prep_mx_array_jobs <- function(dir, root, sex, race, edu, validate, resub, queue, m_mem_free_pred,
                               m_mem_free_save, fthread, h_rt, draw_width, p_mx_job, jids) {
  
  get_settings(dir)
  
  current_sex <- sex 
  current_race <- race
  current_edu <- edu
  
  ## Common arguments across both array jobs - these are hard coded for now, but could be changed to be arguments
  archive <- T
  intel <- T
  project <- "PROJECT"
  pred_sub_code <- paste0("tmb_models/complete_pred_mx_subdraw.r")
  save_sub_code <- paste0("tmb_models/save_mx_pred.r")
  testing <- T
  
  if(!(n.sims %% draw_width == 0 )) stop(paste0("Draw width (", draw_width,
                                                ") does not divide cleanly into n.sims (",
                                                n.sims, ")"))
  
  sub_draws <- seq(1, n.sims, draw_width)
  draw_args <- data.table(start_draw = sub_draws, end_draw = sub_draws + (draw_width - 1))
  
  if(resub) {
    # take out the draws that have already run
    for(i in 1:nrow(draw_args)) {
      message(i)
      draw_val <- draw_args[i, end_draw]
      
      files_to_find <- c()
      final_files <- c()
      
      if ((!race_together & by_race) | (!edu_together & by_edu)) {
        
        for(this_year in years) {
          # get list of expected draw files for the current iteration of the draws loop
          files_to_find <- c(files_to_find,
                             paste0("mx_draws_", area_var, "_", this_year, "_", sex, "_", race, "_", edu, "_",
                                    draw_val, ".rds"))
          
          # get list of expected est files
          final_files <- c(final_files, paste0("mx_est_", area_var, "_",
                                               this_year, "_", sex, "_", race, "_", edu, ".rds"))
        }
        
        # find existing files that have sex, race, edu, and draw val in them
        current_files <- list.files(dir, pattern=paste0("_", sex, "_", race, "_", edu, "_",
                                                        draw_val, ".rds"))
        
        current_files <- current_files[current_files %like% paste0("mx_draws_", area_var)]
        
        # similar to above, look for existing files that have sex, race, edu, and draw val in them
        # but this time subset to files that have "mx_est_" in the filepath.
        final_current <- list.files(dir, pattern=paste0("_", sex, "_", race, "_", edu, ".rds"))
        final_current <- final_current[final_current %like% paste0("mx_est_", area_var)]
        
        # also need to accommodate the case where there are some draw files and some final files
        # first, evaluate which draws files are missing
        missing_files <- setdiff(files_to_find, current_files)
        # then see which of them need to actually be re-run
        re_run_files <- setdiff(
          gsub("draws", "est", gsub(paste0("_", draw_val, ".rds"), ".rds", missing_files)),  # set 1
          final_current  # set 2
        )
        
        if(length(re_run_files) == 0) {  # if there is nothing to re-run
          draw_args <- draw_args[end_draw == draw_val, end_draw := NA]
        }
      } else {
        for(this_year in years) {
          for(r in races) {
            for (e in edu_groups) {
              # get list of expected draw files
              files_to_find <- c(files_to_find,
                                 paste0("mx_draws_", area_var, "_", this_year, "_", sex, "_", r, "_", e, "_",
                                        draw_val, ".rds"))
              
              # get list of expected est files
              final_files <- c(final_files, paste0("mx_est_", area_var, "_",
                                                   this_year, "_", sex, "_", r, "_", e, ".rds"))
            } 
          } 
        } 
        
        # detect existing files that look like, e.g., mx_draws_[area_var_[sex]_[race]_[edu].rds
        current_files <- list.files(dir, pattern = paste(paste0("_", sex, "_", races, "_", edu_groups, "_",
                                                                draw_val, ".rds"), collapse = "|"))
        current_files <- current_files[current_files %like% paste0("mx_draws_", area_var)]
        
        # detect existing files that look like, e.g., mx_est_[area_var_[sex]_[race]_[edu].rds
        final_current <- list.files(dir, pattern=paste(paste0("_", sex, "_", races, "_", edu_groups, ".rds"), collapse="|"))
        final_current <- final_current[final_current %like% paste0("mx_est_", area_var)]
        
        # also need to accommodate the case where there are some draw files and some final files
        # first, evaluate which draws files are missing by comparing expected draw files to
        # detected draw files
        missing_files <- setdiff(files_to_find, current_files)
        # then see which of them need to actually be re-run by making the found draw files look
        re_run_files <- setdiff(gsub("draws", "est", gsub(paste0("_", draw_val, ".rds"), ".rds", missing_files)),
                                final_current)
        
        if(length(re_run_files) == 0) {  # if there is nothing to re-run
          draw_args <- draw_args[end_draw == draw_val, end_draw := NA]
        }
      }
    }
    
    # drop the segments of draws for which there is nothing that needs to be re-run
    draw_args <- draw_args[!is.na(end_draw), ]
  }
  
  # save the draw segments that do need to be re-ran
  fwrite(draw_args, paste0(dir, "/draw_args_sex_", sex, "_", race, "_", edu, ".csv"))
  
  # if draw_args has any rows, it means that there are segments of draws that needs to be re-ran
  if(nrow(draw_args) > 0) {
    message("Sending array job")
    
    if (!is.null(p_mx_job)) {
      if (nrow(p_mx_job) > 0) {
        hold = p_mx_job
      } else {
        p_mx_job <- NULL
      }
    } else {
      p_mx_job <- NULL
    }
    
    id <- sbatch(code = pred_sub_code,
                 arguments = c(dir, sex, race, edu, validate, resub),
                 name = paste0("pred_mx_sub_", gsub("/", "_", gsub(root, "", dir)), "_", sex, "_", race, "_", edu),
                 hold = p_mx_job,
                 fthread = fthread, 
                 m_mem_free = m_mem_free_pred,
                 h_rt = h_rt, 
                 archive = archive,
                 project = project, 
                 queue = queue,
                 sgeoutput = dir, 
                 array = paste0("1-", nrow(draw_args)), 
                 array_throttle = 50)
    
    jids[race_fit == current_race & edu_fit == current_edu & sex == current_sex, pred_sub := id]
  } else {
    message("Nothing to run!")
    jids[race_fit == current_race & edu_fit == current_edu & sex == current_sex, pred_sub := 1]
    id <- 1
  }
  
  ## then launch the jobs that will save the results
  years_run <- years
  
  if(resub) {
    for(y in years) {
      if(!race_together) {
        if(file.exists(paste0(dir, "/mx_est_mcnty_", y, "_", sex, "_", race, "_", edu_groups, ".rds"))) {
          years_run <- years_run[years_run != y]
        }
      } else if (!edu_together) { 
        if(file.exists(paste0(dir, "/mx_est_mcnty_", y, "_", sex, "_", races, "_", edu, ".rds"))) {
          years_run <- years_run[years_run != y]
        }
      } else {
        if(length(unique(file.exists(paste0(dir, "/mx_est_mcnty_", y, "_", sex, "_", races, "_", edu_groups, ".rds")))) == 1) {
          if(unique(file.exists(paste0(dir, "/mx_est_mcnty_", y, "_", sex, "_", races, "_", edu_groups, ".rds")))) {
            years_run <- years_run[years_run != y]
          }
        }
      } 
    }  
  } 
  
  save_jobs <- data.table(year = years_run)
  fwrite(save_jobs, paste0(dir, "/mx_save_args_", sex, "_", race, "_", edu, ".csv"))
  
  if(nrow(save_jobs) > 0) {
    message("Sending save array job")
    
    id2 <- sbatch(code = save_sub_code,
                  arguments = c(dir, sex, race, edu, draw_width, resub),
                  name = paste0("save_mx_draws_", gsub("/", "_", gsub(root, "" , dir)), "_", sex, "_", race, "_", edu),
                  hold = id,
                  fthread = fthread, 
                  m_mem_free = m_mem_free_save,
                  h_rt = h_rt, 
                  archive = archive,
                  project = project, 
                  queue = queue,
                  sgeoutput = dir, 
                  array = paste0("1-", nrow(save_jobs)), 
                  array_throttle = 50)
    
    jids[race_fit == current_race & edu_fit == current_edu & sex == current_sex, save_sub := id2]
  } else {
    jids[race_fit == current_race & edu_fit == current_edu & sex == current_sex, save_sub := 1]
  }
  
  return(jids)
}
