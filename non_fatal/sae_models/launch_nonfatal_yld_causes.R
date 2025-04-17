###############################################################################################################
## Description: Launches YLD models for nonfatal causes using array job.
###############################################################################################################

library(data.table)

###### Source functions
repo <- paste0("FILEPATH")
funcs <- list.files(paste0(repo, "/functions/"))

for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

settings_files <- c(
  "state_model49_otherncd_20240301"#,
  )

run_date <- make_time_stamp()
dir.create(paste0("FILEPATH"))
save_loc <- paste0("FILEPATH")

save(settings_files, file = save_loc)

#### Launch array job
auto_queue <- "..."

launch_id <- sbatch(code = paste0(repo, "/launch_nonfatal.R"), name = paste0("yld_array_launch_", run_date), 
       arguments = c(repo, save_loc),
       queue = auto_queue, fthread = 1, m_mem_free = "1G", h_rt = "0-01:00:00", archive = TRUE, 
       sgeoutput = paste0("FILEPATH"), sing_image = "latest",
       array = paste0("1-", length(settings_files)), array_throttle = 4)
