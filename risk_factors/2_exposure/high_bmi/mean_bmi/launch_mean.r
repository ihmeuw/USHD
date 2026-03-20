####################################################################################################
## Description: Launches mean BMI model run
##
####################################################################################################

library(data.table)

###### Source functions
nonfatal_repo <- paste0("FILEPATH")
funcs <- list.files(paste0(nonfatal_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(nonfatal_repo, "/functions/", func)))
}

# ##### Set data location and settings locations, and read in settings
settings_file <- 'mean_bmi_model_84_v6'
settings_path <- "FILEPATH"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc)

###### Generate run date and create model output folder
if (resub == F | is.null(resub) | !exists('resub')) {
  run_date <- make_time_stamp()
}

message(run_date)

output_dir <- paste0(output_dir_settings, run_date, '_model', mean_model)
output_dir_draws_est <- paste0(output_dir_draws_est_settings, run_date, '_model', mean_model)

if (is.null(resub) | isFALSE(resub)) {
  dir.create(output_dir)
  dir.create(output_dir_draws_est)
}

message(run_date)
message(output_dir)
message(output_dir_draws_est)

# directories for estimates of mean BMI
dir.create(paste0(output_dir_draws_est, "/draws/"))
dir.create(paste0(output_dir_draws_est, "/draws_mcnty/"))
dir.create(paste0(output_dir_draws_est, "/est/"))

## Retrieve and save settings file
settings <- read.csv(settings_loc)
fwrite(settings, paste0(output_dir, "/settings.csv"))
fwrite(settings, paste0(output_dir_draws_est, "/settings.csv"))

risks_repo <- paste0('FILEPATH')
auto_queue <- "QUEUE,QUEUE"

run_model <- sbatch(code = paste0(risks_repo, "/submit_mean.r"), name = paste0("submit_mean", run_date), 
       arguments = c(nonfatal_repo, output_dir, output_dir_draws_est, settings_file, sing_image), 
       queue = auto_queue, fthread = 1, m_mem_free = "1G", h_rt = "1-00:00:00", archive = FALSE, 
       sgeoutput = output_dir, sing_image = sing_image)
