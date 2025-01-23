####################################################################################################
## Description: submit array jobs that predict mortality rates and save results for subsets of draws
##
##
##
## Passed args: dir [character] -- home directory for settings and final output
##              root [character] -- path to directory of all models
##              sex [integer] -- sex to generate predictions for

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
##              p_mx_job [] -- list of job_ids for pred_mx, for this sex and race
##
## Requires:    fitted model object ("[dir]/model_fit_[sex]_[race]_[edu].rds")
##              prepped data
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
                               m_mem_free_save, fthread, h_rt, draw_width, p_mx_job, jids, years_arg) {
  
  old_path <- Sys.getenv("PATH")
  Sys.setenv(PATH = paste(old_path, "FILEPATH", sep = ":"))

  get_settings(dir)
  
  # make copy so that we can subset jids easier after the job submission
  current_sex <- sex 
  current_race <- race
  current_edu <- edu
  
  if(current_sex == 99) {
    model_sexes <- sexes
  } else {
    model_sexes <- sex
  }

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

      # files_to_find: draw files, e.g. FILEPATH
      # final_files: est_files (not draws), e.g., FILEPATH
      # current_files: draw files detected
      # final_current: est files detected
      # missing_files: difference between files_to_find (draw files) and current_files (draw files)
      # re_run_files: difference between missing est files and est files detected

      files_to_find <- c()
      final_files <- c()
      current_files <- c()
      final_current <- c()

      # race_together affects the format of the file paths that are expected to exist when checking
      
      

      # i.e. race_together is FALSE and this is a by_race model, or, edu_together is FALSE and this
      # is a by_edu model in either case, looking for files with the race and edu command line args.
      if ((!race_together & by_race) | (!edu_together & by_edu)) {
        # If races_together is FALSE, then there will be separate files for each of the values
        # in races (from settings.csv)
        # Recall that if race_together is TRUE then race (the command line argument into this
        # script) will be 99
        for(sx in model_sexes) {
          for(this_year in years_arg) {

            # get list of expected draw files for the current iteration of the draws loop
            files_to_find <- c(files_to_find,
                               paste0("mx_draws_", area_var, "_", this_year, "_", sx, "_", race, "_", edu, "_",
                                      draw_val, ".rds"))
            
            # get list of expected est files
            final_files <- c(final_files, paste0("mx_est_", area_var, "_",
                                                 this_year, "_", sx, "_", race, "_", edu, ".rds"))
          } # end year for loop

        } # end sex for loop
        
        
        # find existing files that have sex, race, edu, and draw val in them
        current_files <- list.files(dir, pattern=paste(paste0("_", model_sexes, "_", race, "_", edu, "_",
                                                        draw_val, ".rds"), collapse = "|"))
        # The line above would find more than just mx_draws files, so subset to files that
        # have "mx_draws_" in the filepath
        current_files <- current_files[current_files %like% paste0("mx_draws_", area_var)]
        
        # similar to above, look for existing files that have sex, race, edu, and draw val in them
        # but this time subset to files that have "mx_est_" in the filepath.
        final_current <- list.files(dir, pattern=paste(paste0("_", model_sexes, "_", race, "_", edu, ".rds"), collapse = "|"))
        final_current <- final_current[final_current %like% paste0("mx_est_", area_var)]
        
        # also need to accommodate the case where there are some draw files and some final files
        # first, evaluate which draws files are missing
        missing_files <- setdiff(files_to_find, current_files)
        # then see which of them need to actually be re-run
        # missing_files contains missing draws files. convert the draws part of the file path and
        # compare that to detected est files
        re_run_files <- setdiff(
          gsub("draws", "est", gsub(paste0("_", draw_val, ".rds"), ".rds", missing_files)),  # set 1
          final_current  # set 2
        )
        
        if(length(re_run_files) == 0) {  # if there is nothing to re-run
          draw_args <- draw_args[end_draw == draw_val, end_draw := NA]
        }

        

        

      } else {  # races_together is TRUE or edu_together is TRUE
        # This could be three things:
        # 1) by_edu and edu_together are True
        # 2) by_race and race_together are True
        # 3) by_race and by_edu are both False, and then it doesn't really matter what race_together and
        # edu_together are, but they should be False. I.e., a county level model (not by race or by edu.)

        # If races_together is TRUE, then the value for race (the command line argument into
        # this script) will be 99. This loops over every race in the settings.csv races variable,
        # looking for files specific to those race values. The branch above, where race_together
        # was F, looked for files with the race variable that is passed into this function,
        # i.e., one single race value. Same thing for if edu_together is True.

        # and if this isn't a by-race or by-edu model, then races/edu_groups will just be 9,
        # and there will only be one iteration of these for-loops
        for(sx in model_sexes) {
          
          for(this_year in years_arg) {
            for(r in races) {
              for (e in edu_groups) {
                # get list of expected draw files
                files_to_find <- c(files_to_find,
                                   paste0("mx_draws_", area_var, "_", this_year, "_", sx, "_", r, "_", e, "_",
                                          draw_val, ".rds"))
                
                # get list of expected est files
                final_files <- c(final_files, paste0("mx_est_", area_var, "_",
                                                     this_year, "_", sx, "_", r, "_", e, ".rds"))
              }  # end edu for loop
            }  # end race for loop
          }  # end year for loop
          
          # detect existing files that look like, e.g., mx_draws_[area_var_[sex]_[race]_[edu].rds
          
          # combinations of races and edu_groups, it'll just pair them up.
          current_files_tmp <- list.files(dir, pattern = paste(paste0("_", sx, "_", races, "_", edu_groups, "_",
                                                                  draw_val, ".rds"), collapse = "|"))
          current_files_tmp <- current_files_tmp[current_files_tmp %like% paste0("mx_draws_", area_var)]
          
          current_files <- c(current_files, current_files_tmp)
          
          
          # detect existing files that look like, e.g., mx_est_[area_var_[sex]_[race]_[edu].rds
          final_current_tmp <- list.files(dir, pattern=paste(paste0("_", sx, "_", races, "_", edu_groups, ".rds"), collapse="|"))
          final_current_tmp <- final_current_tmp[final_current_tmp %like% paste0("mx_est_", area_var)]
          final_current <- c(final_current, final_current_tmp)
          
        } # end sex for loop



        # also need to accommodate the case where there are some draw files and some final files
        # first, evaluate which draws files are missing by comparing expected draw files to
        # detected draw files
        missing_files <- setdiff(files_to_find, current_files)
        # then see which of them need to actually be re-run by making the found draw files look
        # like expected est files, and comparing that to found est files
        re_run_files <- setdiff(gsub("draws", "est", gsub(paste0("_", draw_val, ".rds"), ".rds", missing_files)),
                                final_current)

        if(length(re_run_files) == 0) {  # if there is nothing to re-run
          draw_args <- draw_args[end_draw == draw_val, end_draw := NA]
        }
      } # end else part of the race_together/edu_together if/else
    }  # end for loop over the rows of draw_args

    # drop the segments of draws for which there is nothing that needs to be re-run
    draw_args <- draw_args[!is.na(end_draw), ]
  }

  # save the draw segments that do need to be re-ran
  fwrite(draw_args, paste0(dir, "/draw_args_sex_", sex, "_", race, "_", edu, ".csv"))

  # Try printing out this arg
  cat("\n")
  print(p_mx_job)
  cat("\n")

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
                 hold = na.omit(p_mx_job),
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
    jids[race_fit == current_race & edu_fit == current_edu & sex == current_sex, pred_sub := NA]
    id <- NA
  }
  
  message(is.na(id))

  ## then launch the jobs that will save the results
  years_run <- years_arg
  # starting with all years in the run, filter down the years for which there are already results
  # the files that are present will vary depending on the value of races_together. If races_together
  # is TRUE, then one file will have every race inside it, and the value for race (the function)
  # argument into this script will be 99. If races_together is FALSE, then there will be separate
  # files for each value in races (from settings.csv)
  saved_files <- c()
  
  if(resub) {
    for(y in years_arg) {
      
      for(sx in model_sexes) {
        
        if(!race_together) {  # race_together is FALSE, then we only need to check for one race
          
          # race could be values 1, 2, 3, 4, 7, or all_pop_id
          saved_files <- c(saved_files, paste0(dir, "/mx_est_mcnty_", y, "_", sx, "_", race, "_", edu_groups, ".rds"))
          
        } else if (!edu_together) { # edu_together is FALSE, then we only need to check for one edu
          
          saved_files <- c(saved_files, paste0(dir, "/mx_est_mcnty_", y, "_", sx, "_", races, "_", edu, ".rds"))
          
        } else { # race_together is TRUE, so race is 99, and races could be (1, 2, 3, 4, 7)
          
          
          if(length(unique(file.exists(paste0(dir, "/mx_est_mcnty_", y, "_", sx, "_", races, "_", edu_groups, ".rds")))) == 1) {
            
            saved_files <- c(saved_files, paste0(dir, "/mx_est_mcnty_", y, "_", sx, "_", races, "_", edu_groups, ".rds"))
            
          }
          
        }  # end else statement for race_together (race_together is TRUE)
        
      }
      
      if(all(file.exists(saved_files))) {
        years_run <- years_run[years_run != y]
      }
      

    }  # end loop over years
  }  # end resub if statement

  save_jobs <- data.table(year = years_run)
  fwrite(save_jobs, paste0(dir, "/mx_save_args_", sex, "_", race, "_", edu, ".csv"))

  if(nrow(save_jobs) > 0) { ## false for now to preserve cluster space
    message("Sending save array job")
    
    id2 <- sbatch(code = save_sub_code,
                 arguments = c(dir, sex, race, edu, draw_width, resub),
                 name = paste0("save_mx_draws_", gsub("/", "_", gsub(root, "" , dir)), "_", sex, "_", race, "_", edu),
                 hold = na.omit(as.numeric(id)),
                 fthread = fthread, 
                 m_mem_free = m_mem_free_save,
                 h_rt = h_rt, 
                 archive = archive,
                 project = project, 
                 queue = queue,
                 sgeoutput = dir, 
                 array = paste0("1-", nrow(save_jobs)), 
                 array_throttle = 50)
    
    jids[race_fit == current_race & edu_fit == current_edu & sex_fit == current_sex, save_sub := id2]

  } else {
    jids[race_fit == current_race & edu_fit == current_edu & sex_fit == current_sex, save_sub := NA]
  }
  
  return(jids)

}
