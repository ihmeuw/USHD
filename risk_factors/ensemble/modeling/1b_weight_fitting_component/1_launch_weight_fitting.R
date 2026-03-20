library(data.table)
library(magrittr)
library(ggplot2)
library(dplyr)
library(stats)

nf_repo <- paste0("FILEPATH")
risk_repo <- paste0("FILEPATH")
funcs <- list.files(paste0(nf_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(nf_repo, "/functions/", func)))
}
source(paste0(risk_repo, "ensemble/modeling/1b_weight_fitting_component/1_parallel_helper_functions.R"))

settings_file <- "bmi_ens_weights_by_sex_detailedrace_age70_4yr_v24"
settings_path <- "FILEPATH"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc) # loads settings needed to set up run
settings <- read.csv(settings_loc, header = F)

# --------------------------------------------------------------------------------------------------------------------
#Set up parameters in param_map

# Filepath to microdata (NOT COLLAPSED) that you want to work with
# Must have columns called "data", "nid_loc_yr_index"
data_filepath = sprintf("FILEPATH", data_version)

# # METHODS:
# # 1) You are looking at the largest difference between the model and the data at any point along the distribution 
# # 2) You are looking at the largest difference between the model and the data at any point along the left side of the distribution 
# # 3) You are looking for the biggest difference at one of the three tails/thresholds, whichever one of the three that has the biggest difference is chosen
# # 4) You are adding a weighted sum of the differences between data and model at the three thresholds/tails
# # 5) You are adding a weighted sum of the SQUARED differences between data and model at the three thresholds/tails
# 
# # TYPES:
# # Thresholds: Prioritizes a 'vertical' cutoff (meaning it prioritizes fitting at a certain value in the data)
# # Tails: Prioritizes a 'horizontal' cutoff (meaning it prioritizes fitting at certain prevalence values)
# # Thresholds approach makes the most sense for USHD BMI work because we are trying
# #   to optimize fit at BMI valus 18.5, 25, 30

# Enter the actual tails (doesn't matter what they are if you chose thresholds)
low_tail <<- 0
middle_tail <<- 0
high_tail <<- 0

if(!abs(sum(low_weight, medium_weight, high_weight) - 1) < 1e-6){
  warning("Rescaling the importances weights on thresholds b/c sums to ", sum(low_weight, medium_weight, high_weight), ". Should sum to 1.")
  temp_sum <- sum(low_weight, medium_weight, high_weight) 
  low_weight <- low_weight/temp_sum
  medium_weight <- medium_weight/temp_sum
  high_weight <- high_weight/temp_sum 
}

###### Generate run date and create model output folder
if (resub==F | is.null(resub) | !exists('resub')){
  run_date <- make_time_stamp()
  dir.create(paste0(weight_root, run_date))
} else if (resub==T){
  run_date <- run_date
}
launch.date <- run_date

if(!exists("by_demo")) by_demo <- FALSE

if(by_demo){
  data <- readRDS(data_filepath)
  demo_paths <- vector(mode = "list", length(demos))
  names(demo_paths) <- demos
  demo_vals <- demo_paths
  for(dd in demos){
    demo_paths[[dd]] <- paste(dd, tolower(gsub(" ", "_", c(data[, unique(get(dd))]))), sep = "_")
    demo_vals[[dd]] <- c(data[, unique(get(dd))])
  }
  demo_vals <- as.data.table(expand.grid(demo_vals))
  demo_vals_string <- as.data.table(expand.grid(demo_paths))
  launch_paths_expanded <- paste(launch.date, apply(demo_vals_string, 1, paste, collapse="_"), sep = "/")
  
  # by creating subdirectories for each demographic combination, we can create nested directories of model fits within one run date
  lapply(paste0(weight_root, launch_paths_expanded), dir.create)
  fwrite(demo_vals, paste0(weight_root, run_date, "/demo_vals_map.csv"))
} else{
  launch_paths_expanded <- launch.date
  demo_vals <- NA
}

fwrite(settings, paste0(weight_root, run_date, "/settings.csv"), col.names=FALSE)

message(run_date)
message(paste0(weight_root, run_date))

holds <- c()
for(i in 1:length(launch_paths_expanded)){
  message(launch_paths_expanded[i])  
  
  
  # max.time = 8 # in hours # nvm this is set in the parallel script
  
  # Prep Param Map
  metadata <- expand.grid(data_filepath = data_filepath, 
                          me_type = me_type,
                          initial_condition = 1:num_of_initial_conditions,
                          option = option, 
                          low_threshold = low_threshold,
                          medium_threshold = medium_threshold,
                          high_threshold = high_threshold,
                          low_tail = low_tail,
                          middle_tail = middle_tail,
                          high_tail = high_tail,
                          low_weight = low_weight,
                          medium_weight = medium_weight,
                          high_weight = high_weight,
                          launch.date = launch.date,
                          demo_val_index = i,
                          launch_paths_expanded = launch_paths_expanded[i]
  )
  
  # THEN LAUNCH
  # --------------------------------------------------------------------------------------------------------------------
  
  if(run_m1 == TRUE){
    stop("Method m1 is not supported for USHD")
  }
  
  if(run_m2 == TRUE){
    param_map_m2 <- cbind(metadata, strategy = "m2")
    if (length(option) == 1) {
      param_map_m2_fp = paste0(weight_root, launch_paths_expanded[i], "/param_map_", option, "_", "m2.csv")  
    }
    if (length(option) > 1) {
    }
    write.csv(param_map_m2, param_map_m2_fp)
    
    ## sbatch Command
    job_name <- paste(me_type, i, "m2", sep = "_")   # name of the job
    thread_flag <- "-c 3" 
    mem_flag <- "--mem=4G" 
    runtime_flag <- "-t 0-08:00:00"
    queue_flag <- "-p QUEUE" # long or all
    throttle_flag <- "800" # how many tasks are allowed to run at once. 800 is a good limit for smaller jobs. Bigger jobs will need smaller throttles
    throttle_flag <- as.character(pmax(ceiling(as.numeric(throttle_flag)/length(launch_paths_expanded)), 50))
    n_jobs <- paste0("1-", nrow(param_map_m2), "%", throttle_flag) # this means you're running one task for every row of the param map you made. 
    next_script <- paste0(risk_repo, "ensemble/modeling/1b_weight_fitting_component/1_parallel_m2_weight_fitting.R") # filepath to the script you want to launch. Make sure you saved it first.
    output_filepath <- paste0("-o FILEPATH")
    project_flag<- "-A PROJECT"  # make sure this is one you have permissions for
    # add jdrive_flag if needed
    sbatch_command <- paste( "sbatch", "-J", job_name, mem_flag,thread_flag,  project_flag, 
                             mem_flag, runtime_flag, queue_flag, "-a", n_jobs, # paste0(c(18, 30,53, 58, 60, 65, 82, 89, 100), collapse = ","),
                             output_filepath,  
                             "FILEPATH -i FILEPATH -s", 
                             next_script, param_map_m2_fp, settings_loc)
    job_id_weight_fit <- system(sbatch_command, intern = T)
    print(job_id_weight_fit)
    job_id_weight_fit <- stringr::str_extract(job_id_weight_fit, "[[:digit:]]+")
    holds <- c(holds, job_id_weight_fit)
  }
  
  #creating run-time log 
  if (run_m1 == TRUE) {
    stop("not updated for USHD yet")
    dir.create(paste0("FILEPATH"))
    for (each.option in option) {
      dir.create(paste0("FILEPATH", each.option))
    }
  }
  if (run_m2 == TRUE) {
    if(!dir.exists(paste0(weight_root, launch_paths_expanded[i], "/runtime_records"))){
      dir.create(paste0(weight_root, "/", launch_paths_expanded[i], "/runtime_records"))  
    }
  }
}

# launch the script the selects the best weights from a given version
sbatch(code = paste0(risk_repo, "ensemble/modeling/1b_weight_fitting_component/2_process_best_weights.R"),
       name = paste0(me_type, "_process_best_weight"),
       arguments = c(launch.date),
       hold = paste(holds, collapse = ","),
       queue = "QUEUE",
       fthread = 2,
       m_mem_free = "10G",
       h_rt = "0-00:25:00",
       archive = F,
       sgeoutput = paste0("FILEPATH"),
       kill_on_invalid_dep = T
)
