####################################################################################################
## Description: Launch population SD (BMI) scripts
## 
##            Child script will generate estimates 
##              saved in separate files by year and draw "chunk":
##              "[dir]/draws_[area_var]_[year]_[sex]_[race]_[edu]_draw_[draw_num].rds"
##            Once those jobs complete, combine draws into a single file and create collapsed estimates
##        
## Input: Input is specified in settings.csv
## 
## Output: Creates new run directory FILEPATH
##          including settings file, array csv, log to track runtimes, git history, and ensemble weight cache.
##
####################################################################################################

settings_file <- "bmi_sd_opt_full_run_v44_raked_inputs"

library(data.table)

###### Source functions
library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                      R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
if(!exists("ushd_client")){
  lbd.loader::load_package("sae.shared")
  library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
  ushd_client$save_covariate_population
}

nf_repo <- paste0("FILEPATH")
risk_repo <- paste0("FILEPATH")
funcs <- list.files(paste0(nf_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(nf_repo, "/functions/", func)))
}
##### Set data location and settings locations, and read in settings
settings_path <- "FILEPATH"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc)


###### Generate run date and create model output folder
if (resub==F | is.null(resub) | !exists('resub')){
  run_date <- make_time_stamp()
} else if (resub==T){
  run_date <- run_date
}

message(run_date)
run_date_new <- run_date

# directory for estimates of exp_sd
output_dir_draws_est <- paste0(draws_sd_output_dir,'/', run_date)
message(output_dir_draws_est)

if (resub==F | is.null(resub) | !exists('resub')){
  # directory for estimates of mean BMI
  dir.create(paste0(output_dir_draws_est, "/draws/"), recursive = T)
  dir.create(paste0(output_dir_draws_est, "/draws/intermediate/"), recursive = T)
  dir.create(paste0(output_dir_draws_est, "/est/"), recursive = T)
}

## Retrieve and save copy of settings file
settings <- setDT(read.csv(settings_loc, header = F))[, 1:2]
# add columns required by database if they're not already present
# copy the following from exp_mean settings file
# pop_file, in_PREV_est dirs.
# Also, copy extra_dim_values as races if extra_dim == race
rows <- c("pop_file", "in_underweight_est", "in_overweight_est", "in_obese_est")
rows <- rows[!(rows %in% settings$V1)] # make sure not already in exp_sd settings

# read exp_mean settings (parent model for exp_sd)
exp_mean_settings <- setDT(read.csv(file = paste0(settings[V1 == "mean_bmi_dir", V2], "settings.csv"),
                              header = F))[, 1:2]
# get the relevant rows
copy_rows <- exp_mean_settings[V1 %in% rows]

# If setting for using raked prevalence is not passed, set to FALSE
if(!exists("opt_raked_prev")){
  opt_raked_prev <- FALSE
  # add this to the settings file
  copy_rows <- rbind(copy_rows, data.table(V1 = "opt_raked_prev", V2 = opt_raked_prev))
} else {
  # otherwise, assert that it is a logical
  stopifnot(is.logical(opt_raked_prev))
}

# If setting for using raked mean BMI model is not passed, set to FALSE
if(!exists("opt_raked_exp_mean")){
  opt_raked_exp_mean <- FALSE
  # add this to the settings file
  copy_rows <- rbind(copy_rows, data.table(V1 = "opt_raked_exp_mean", V2 = opt_raked_exp_mean))
} else {
  # otherwise, assert that it is a logical
  stopifnot(is.logical(opt_raked_exp_mean))
}

# also get the extra_dim_val
if(!("races" %in% settings$V1) & settings[V1 == "extra_dim", V2] == "race"){
  temp <- settings[V1 == "extra_dim_values", V2]
  copy_rows <- rbind(copy_rows, data.table(V1 = "races", V2 = temp))
}

# add rows to settings file
settings <- rbind(settings, copy_rows)
stopifnot(setequal(eval(parse(text = settings[V1 == "races", "V2"])), c(2,4,5,6,7)))

run_settings <- paste0(output_dir_draws_est, "/settings.csv") # save so can ensure that we're using the same settings.csv in the child scripts
fwrite(settings, run_settings, col.names=FALSE)

code <- paste0('FILEPATH')

# save git status
cat(get_git_status(repo = risk_repo, repo_name = "risk_factors", show_diff = TRUE), file = paste0(output_dir_draws_est, "/git_log.txt"))


# Load and cache the ensemble weights -------------------------------------
cache_filename <- sprintf("%s/ens_wt_cache.rds", output_dir_draws_est)
if(file.exists(cache_filename)){
  wlist <- readRDS(cache_filename)
  skip_ensemble_weight_check <- F
} else{
  if(!exists("ens_weight_version_name") & !exists("ensemble_weight_version_ids")){
    warning("ens_weight_version_name and ensemble_weight_version_ids
          are not specified in settings file so skipping checks. Careful!")
    skip_ensemble_weight_check <- T
  } else if (!exists("ens_weight_version_name")){
    # if version exists but not name, set name as NULL
    ens_weight_version_name = NULL
    skip_ensemble_weight_check <- F
  } else if (!exists("ensemble_weight_version_ids")){
    # if name exists but not version, set version as NULL
    ensemble_weight_version_ids = NULL
    skip_ensemble_weight_check <- F
  } else{
    # if both exist, do nothing
    skip_ensemble_weight_check <- F
  } 
  
  if(!skip_ensemble_weight_check){
    if(is.numeric(ensemble_weight_version_ids)) must_be_best <- F else must_be_best <- T
    
    tryCatch(
      {
        
        call_string <- sprintf(
          "get_ensemble_weights(model_run_name = NULL,  version_name = %s,version_type = NULL,ensemble_weight_version_ids = %s, get_best = %s)", 
          ifelse(!is.null(ens_weight_version_name), ens_weight_version_name, "NULL"), 
          ifelse(!is.null(ensemble_weight_version_ids), ensemble_weight_version_ids, "NULL"), 
          must_be_best
        )
        
        if(!is.null(ensemble_weight_version_ids)) ensemble_weight_version_ids <- as.list(ensemble_weight_version_ids) # expects a list of numbers (or a NULL object)
        wlist <- get_ensemble_weights(
          model_run_name = NULL,
          version_name = ens_weight_version_name,
          version_type = NULL,
          ensemble_weight_version_ids = as.list(ensemble_weight_version_ids),
          get_best = must_be_best
        )
        
        if(nrow(wlist) == 0){
          warning("get_ensemble_weights returned DT with length 0 -- something went wrong")
          wlist <- paste("get_ensemble_weights returned DT with length 0 -- check db call", call_string, sep = "\n\n")
          
        }
        
        if(wlist[, uniqueN(ensemble_weight_version_id)] != 1){
          wlist <- paste("get_ensemble_weights returned multiple sets of ensemble weights -- check db call", call_string, sep = "\n\n")
        } 
        
        if(is.data.table(wlist)){
          saveRDS(wlist, cache_filename)
        }
      },
      error = function(e){
        message(e)
        wlist <- paste("get_ensemble_weights failed -- check db call", call_string, sep = "\n\n")
        print(wlist)
      },
      finally = {
        print(call_string)
      }
    )
  }
}
# check that the demographics in the ensemble weight are present in mean/SD settings
# note that this only works for race currently!
if(!skip_ensemble_weight_check){
  # get the race vars that are in the mean BMI model specified in exp_sd settings
  temp <- read.csv(paste0(mean_bmi_dir, "settings.csv"), header = F)
  races <- eval(parse(text = temp[which(temp$V1 == "races"), "V2"]))
  if(!setequal(races, extra_dim_values)) stop("The races in mean BMI settings don't match extra_dim_val in exp_sd settings")
  ensemble_races <- wlist[, sort(unique(population_group_id))]
  if(length(ensemble_races) == 0){
    message("There are no races specific in ensemble weights -- carry on")
  } else{
    if(identical(sort(races), ensemble_races)){
      message("The races in the ensemble weights match the races in the exp_mean and exp_sd models!")
    } else{
      print("ensemble weight races:")
      print(ensemble_races)
      print("Races in exp_mean model:")
      print(races)
      stop("The races in the ensemble weight do not match the races in the exp_mean data")
    }  
  }
}


# Submit SD optimization jobs ---------------------------------------------

if(is.null(location_id)){
  # if location_id is NULL, set as zero and the child script will interpret this 
  # as "all counties"
  locid <- 0
} else{
  # otherwise, create a string that holds all of the location ids (so that they're run as one task)
  locid <- sprintf("c(%s)", paste(location_id, collapse = ","))
}

### Strata combos -> 1 job per stratum

# if opt_raked_prev is TRUE, the we will load the raked prevalence estimates.
# These are all stored in imputation 0 b/c we combine all imp before raking
if(opt_raked_prev){
  n_draws_per_imp <- n_draws_per_imp*n.imp
  n.imp <- 1
}

combos <- data.table(expand.grid(year = years, # initially tried running all years in one task but race/edu in separate tasks; changed to that one year/task to avoid excessive i/o
                                 extra_dim = sprintf("c(%s)", paste(extra_dim_values, collapse = ",")), # pass all extra_dim_values in together to avoid too many separate tasks
                                 sex = c(sexes), 
                                 imp = 1:n.imp, 
                                 locid = locid, #location_id,
                                 draw = 1:n_draws_per_imp)
                     )
if(opt_raked_prev){
  combos[, imp := 0]
} else{
  combos[, draw := (imp-1)*100+draw] # create draw number based on imputation num
}

setnames(combos, "extra_dim", extra_dim)
setkey(combos, "year")
combos[, task_num := 1:.N]
tasks_path <- paste0(output_dir_draws_est, "/sd_opt_tasks.csv")
fwrite(combos, tasks_path)

# Remember that these settings should be adjusted in resub_ushd_sd_optimization.R is we update here
memory <- "10G"
threads <- 3
h_rt= "0-01:00:00" 
n_jobs <- nrow(combos)

job_lim = 500

q <- set_queue_dynamically(queue = "auto", num_jobs = n_jobs, fthread = threads, m_mem_free=memory, h_rt = h_rt, archive = F)
# skip if resub is TRUE, and just to go the resubmission script
if(!resub){
  exp_sd_opt <- sbatch(code = paste0(risk_repo, "/ensemble/modeling/2_curve_fitting_component/child_ushd_sd_optimization.R"),
                           arguments = c(risk_repo, run_settings, output_dir_draws_est, tasks_path),
                           sing_image = "FILEPATH",
                           project = "PROJECT",
                           name = "ushd_exp_sd_opt",
                           fthread = threads,
                           m_mem_free = memory,
                           sgeoutput = paste0(output_dir_draws_est, "/"),
                           h_rt = h_rt,
                           archive = F,
                           array = paste0("1-", n_jobs),
                           queue = q,
                           array_throttle = job_lim,
                    submit = T)
  # make template for runtime log (completed in child scripts)

  temp <- combos[0]
  temp[, runtime_hrs := NA]
  fwrite(x = temp, file = paste0(output_dir_draws_est, "/timelog_child_ushd_sd_optimization.csv"), col.names = T, append = F)

} else {
  exp_sd_opt <- NA
}


# Resubmit failed jobs ----------------------------------------------------
q <- set_queue_dynamically(queue = "auto", num_jobs = 1, fthread = 1, m_mem_free="1G", h_rt = "2-00:00:00", archive = F)

# note that this is sbatch is for a long time b/c it sleeps until all of the child jobs are done (so that it can be a hold for the combine script)
# Our sbatch function uses the hold argument "afterok", so this job only runs
# if the previous job completes successsfully. That defeats the point of a resubmission
# script. To avoid modifying the sbatch function, use the submit = FALSE argument
# to construct the sbatch command. Then, replace "afterok" with "afterany" in the
# command. Then, submit the job with the modified command.
resub_exp_sd_opt_command <- sbatch(code = paste0(risk_repo, "/ensemble/modeling/2_curve_fitting_component/resub_ushd_sd_optimization.R"),
                           arguments = c(risk_repo, run_settings, output_dir_draws_est, tasks_path, job_lim),
                           sing_image = "FILEPATH",
                           project = "PROJECT", 
                           name = "launch_exp_sd_opt_resub",
                           kill_on_invalid_dep = F,
                           hold = na.omit(exp_sd_opt),
                           fthread = 2,
                           m_mem_free = "5G",
                           sgeoutput = paste0(output_dir_draws_est, "/"),
                           h_rt = "2-00:00:00",
                           archive = F,
                           queue = q,
                           submit = FALSE)
resub_exp_sd_opt_command <- gsub("afterok", "afterany", resub_exp_sd_opt_command)
# submit 
id <- system(resub_exp_sd_opt_command, intern = T)
message(id)
resub_exp_sd_opt <- as.numeric(strsplit(id, " ")[[1]][4])
# Collapse draws ----------------------------------------------------------

memory <- "150G"
threads <- 15
h_rt= "0-08:00:00" 

q <- set_queue_dynamically(queue = "auto", num_jobs = 1, fthread = threads, m_mem_free=memory, h_rt = h_rt, archive = F)

combine_exp_sd_opt <- sbatch(code = paste0(risk_repo, "/ensemble/modeling/2_curve_fitting_component/combine_ushd_sd_optimization.R"),
                           arguments = c(risk_repo, run_settings, output_dir_draws_est, tasks_path),
                           sing_image = "FILEPATH",
                           project = "PROJECT", 
                           name = "combine_exp_sd_opt",
                           hold = resub_exp_sd_opt,
                           fthread = threads,
                           m_mem_free = memory,
                           sgeoutput = paste0(output_dir_draws_est, "/"),
                           h_rt = h_rt,
                           archive = T, # need archive to read the mcnty file
                           queue = q,
                           submit = T)


# Vetting plots -----------------------------------------------------------


memory <- "100G"
threads <- 10
h_rt= "0-01:00:00" 

q <- set_queue_dynamically(queue = "auto", num_jobs = 1, fthread = threads, m_mem_free=memory, h_rt = h_rt, archive = F)

sbatch(code = paste0(risk_repo, "/ensemble/modeling/2_curve_fitting_component/vetting/fast_diagnostics.R"),
                             arguments = c("--version", run_date),
                             sing_image = "FILEPATH",
                             project = "PROJECT", 
                             name = "exp_sd_fast_diagnostics",
                             hold = combine_exp_sd_opt,
                             fthread = threads,
                             m_mem_free = memory,
                             sgeoutput = paste0(output_dir_draws_est, "/"),
                             h_rt = h_rt,
                             archive = T,
                             queue = q,
                             submit = T)

sbatch(code = paste0(risk_repo, "/ensemble/modeling/2_curve_fitting_component/vetting/draw_level_correlation_mean_sd.R"),
       arguments = c("--version", run_date, "--year", max(years)),
       sing_image = "FILEPATH",
       project = "PROJECT", 
       name = "exp_sd_mean_sd_draw_cor",
       hold = combine_exp_sd_opt,
       fthread = threads,
       m_mem_free = memory,
       sgeoutput = paste0(output_dir_draws_est, "/"),
       h_rt = h_rt,
       archive = T,
       queue = q,
       submit = T)


sbatch(code = paste0(risk_repo, "/ensemble/modeling/2_curve_fitting_component/vetting/evaluate_edensity_performance.R"),
       arguments = c("--version", run_date),
       sing_image = "FILEPATH",
       project = "PROJECT", 
       name = "exp_sd_edensity_performance",
       hold = combine_exp_sd_opt,
       fthread = threads,
       m_mem_free = memory,
       sgeoutput = paste0(output_dir_draws_est, "/"),
       h_rt = "0-02:00:00",
       archive = T,
       queue = q,
       submit = T)

sbatch(code = paste0(risk_repo, "/ensemble/modeling/2_curve_fitting_component/vetting/plot_curve_results.R"),
       arguments = c("--version", run_date), # there are additional optional args (see script to see, but otherwise uses defaults)
       sing_image = "FILEPATH",
       project = "PROJECT", 
       name = "exp_sd_plot_curves",
       hold = combine_exp_sd_opt,
       fthread = threads,
       m_mem_free = memory,
       sgeoutput = paste0(output_dir_draws_est, "/"),
       h_rt = h_rt,
       archive = T,
       queue = q,
       submit = T)


# print summary for easier manual logging
paste(settings_file, run_date, sep = " ---- ")
