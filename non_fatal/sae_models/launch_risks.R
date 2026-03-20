###############################################################################################################
## Description: Launches a risk factors model run.
###############################################################################################################
rm(list=ls())
library(data.table)

###### Source functions
repo <- paste0("FILEPATH")
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

###### Set data location and settings locations, and read in settings
settings_file <- 'brfss_gallup_model154_overweight_natl_2024_10_09'
settings_path <- "FILEPATH"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc)
###### Generate run date and create model output folder

if (outcome[1] %like% 'obese'){
  main_output_dir <- paste0(main_output_dir,'obese/')
  draws_est_output_dir <- paste0(draws_est_output_dir,'/obese/')
} else if (outcome[1] %like% 'overwt'){
  main_output_dir <- paste0(main_output_dir,'overweight/')
  draws_est_output_dir <- paste0(draws_est_output_dir,'/overweight/')
} else if (outcome[1] %like% 'underwt'){
  main_output_dir <- paste0(main_output_dir,'underweight/')
  draws_est_output_dir <- paste0(draws_est_output_dir,'/underweight/')
}

###### Generate run date and create model output folder
if (!exists("run_date")) {
  if (is.null(run_date)) {
    run_date <- make_time_stamp()
  }
} else if (is.null(run_date) | !resub) {
  run_date <- make_time_stamp()
}

output_dir <- paste0(main_output_dir, run_date, model)
output_dir_draws_est <- paste0(draws_est_output_dir,'/', run_date, model)

message(run_date)
message(output_dir)
message(output_dir_draws_est)

if (is.null(resub)|isFALSE(resub)){
  dir.create(output_dir)
  dir.create(output_dir_draws_est)
}

if ((outcome[1] %like% 'obese'| outcome[1] %like% 'overwt') & n.imp > 0) {
  output_dir_draws_est0 <- paste0(output_dir_draws_est,'/imputation0')
  dir.create(paste0(output_dir_draws_est0, "/draws/"), recursive = T)
  dir.create(paste0(output_dir_draws_est0, "/est/"), recursive = T)
  dir.create(paste0(output_dir, "/imputation0"))
}

## Retrieve and save settings file
settings <- read.csv(settings_loc, header = F)
# if resub is FALSE, then put the current run_date in the settings file
if (is.null(resub)|isFALSE(resub)){
  settings[settings$V1 == "run_date", 2] <- run_date
  # update the settings file in settings_loc too
  fwrite(settings, settings_loc,  col.names=FALSE)
}
fwrite(settings, paste0(output_dir, "/settings.csv"),  col.names=FALSE)
fwrite(settings, paste0(output_dir_draws_est, "/settings.csv"),  col.names=FALSE)
# write a text file with the settings file name
write(settings_loc, paste0(output_dir, "/settings_file_name.txt"))

####################################################################################################
if (n.imp == 0) {
  auto_queue <- set_queue_dynamically(queue = queue, fthread = 1, m_mem_free = "10G", h_rt = "0-1:00:00", archive = TRUE, priority = "RAM")
  
  submit <- sbatch(code = paste0(repo, "/submit_single_cause.R"), name = paste0("risks_model_", run_date),
                   arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, queue, sing_image, FALSE, run_date),
                   queue = auto_queue, fthread = 1, m_mem_free = "10G", h_rt = "01:00:00", archive = TRUE,
                   sgeoutput = output_dir, sing_image = sing_image, array = 0, array_throttle = 50)
  
} else if (n.imp > 0) {
  ## Set queue
  auto_queue <- set_queue_dynamically(queue = queue, fthread = 1, m_mem_free = "10G", h_rt = "0-1:00:00", archive = TRUE, priority = "RAM")
  
  sbatch(code = paste0(repo, "/submit_single_cause.R"), name = paste0("risks_model_", run_date),
         arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, queue, sing_image, FALSE, run_date),
         queue = auto_queue, fthread = 1, m_mem_free = "10G", h_rt = "01:00:00", archive = TRUE,
         sgeoutput = output_dir, sing_image = sing_image, array = paste0("1-", n.imp), array_throttle = 50)
}

file.copy(paste0(output_dir,'/imputation1/recode.rds'),paste0(output_dir,'/imputation0/recode.rds'))
# also copy population file
file.copy(paste0(output_dir_draws_est,'/imputation1/population.rds'),paste0(output_dir_draws_est,'/imputation0/population.rds'))

print(run_date)
print(output_dir)
print(settings_file)
