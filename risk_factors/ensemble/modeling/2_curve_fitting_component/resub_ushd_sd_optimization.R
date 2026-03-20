####################################################################################################
## Description: Resubmit child_ushd_sd_optimization.R for any tasks that failed
##        
## Input: risk_repo
##        settings_loc
##        output_dir_draws_est
##        tasks_path
##        job_lim
## 
## Output:  Resubmits tasks for which the expected output is missing
##
##
####################################################################################################


rm(list = ls())
if (!interactive()) {
  args <- commandArgs(TRUE)
} else{
}


message("args")
print(args)

repo <- args[[1]]
settings_loc <- args[[2]]
output_dir <- args[[3]]
tasks_path <- args[[4]]
job_lim <- args[[5]]

# Set up ------------------------------------------------------------------

library(data.table)
source(paste0(repo, "ensemble/modeling/2_curve_fitting_component/sd_opt_utils/get_sd_draw_files.R"))
source(paste0(repo, "0_functions/cluster_utils.R"))

# read settings and arguments
nf_repo <- paste0("FILEPATH")
risk_repo <- paste0("FILEPATH")
funcs <- list.files(paste0(nf_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(nf_repo, "/functions/", func)))
}

##### Set data location and settings locations, and read in settings
get_settings(settings_loc)
tasks <- fread(tasks_path, stringsAsFactors = F)


# Generate list of expected outputs ---------------------------------------
expected_files <- get_sd_draw_files(tasks)

if(expected_files[, sum(failed)] > 0){
  print(sprintf("%i outputs (%#.1f percent) from %i tasks (%#.1f percent) are missing", 
                expected_files[, sum(failed)], 
                expected_files[, mean(failed)*100], 
                expected_files[failed == 1, uniqueN(task_num)],  
                expected_files[failed == 1, uniqueN(task_num)]/expected_files[, uniqueN(task_num)]*100))
  
  if(expected_files[, mean(failed)] ==1){
    stop("Resubmission cancelled -- 100% of previous jobs failed. Check code!")
  }
  
  
  # tasks to resubmit
  
  resub_tasks <- expected_files[failed == 1, unique(task_num)]
  resub_tasks <- tasks[task_num %in% resub_tasks]
  resub_tasks[, task_num := 1:.N]
  
  tasks_path <- paste0(output_dir, "/sd_opt_tasks_resub.csv")
  fwrite(resub_tasks, tasks_path)
  
  memory <- "12G"
  threads <- 3
  h_rt= "0-08:00:00" 
  n_jobs <- nrow(resub_tasks)
  
  q <- set_queue_dynamically(queue = "auto", num_jobs = n_jobs, fthread = threads, m_mem_free=memory, h_rt = h_rt, archive = F)
  
  print("Resubmitting:")
  
  exp_sd_opt <- sbatch(code = paste0(risk_repo, "/ensemble/modeling/2_curve_fitting_component/child_ushd_sd_optimization.R"),
                       arguments = c(risk_repo, settings_loc, output_dir, tasks_path),
                       sing_image = "FILEPATH",
                       project = "PROJECT", 
                       name = "ushd_exp_sd_opt_resub",
                       kill_on_invalid_dep = F,
                       fthread = threads,
                       m_mem_free = memory,
                       sgeoutput = paste0(output_dir, "/"),
                       h_rt = h_rt,
                       archive = F,
                       array = paste0("1-", n_jobs),
                       queue = q,
                       array_throttle = job_lim,
                       submit = T)
  print(sprintf("Task number %i", exp_sd_opt))
  
  # save the job id so that it can be used as a hold in the parent script
  jid_path <- paste0(output_dir, "/sd_opt_resub_job_id.rds")
  saveRDS(exp_sd_opt, jid_path)
  

# sleep -------------------------------------------------------------------

  # let this job sleep until all of the tasks have finished
  job_hold(exp_sd_opt) # from CC
  
} else{
  message("All jobs succeeded")
  # if no jobs failed, no need to create a hold
  jid_path <- paste0(output_dir, "/sd_opt_resub_job_id.rds")
  saveRDS(NULL, jid_path)
  
}

