###############################################################################################################
## Description: launch PS frame pipeline
##
###############################################################################################################

###############################################################################################################
########## 1. Initial setup ##########
###############################################################################################################
rm(list=ls())

###### Load required libraries
pacman::p_load(R.utils, data.table, stringr)

###### Source functions
non_fatal_repo <- "FILEPATH"
funcs <- list.files(paste0(non_fatal_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(non_fatal_repo, "/functions/", func)))
}

settings_file <- "FILEPATH"

settings_path <- "FILEPATH"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc)

joint_dir <- "FILEPATH"

# name output file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
if (resub==F | is.null(resub)){
  date_time_stamp <- make_time_stamp()
  dir.create(paste0(joint_dir, date_time_stamp,"/"), recursive = T)
} else if (resub==T){
  date_time_stamp <- date_time_stamp
}

## Retrieve and save settings file
settings <- read.csv(settings_loc)
fwrite(settings, paste0(joint_dir, date_time_stamp, "/settings.csv"))

message(date_time_stamp)
message(paste0(joint_dir, date_time_stamp))

code <- "FILEPATH"

prep_data <- sbatch(code = paste0(code, "/1_prep_data_raking.R"),
                    name = paste0("prep_inputs"),
                    arguments = c(non_fatal_repo, settings_file, date_time_stamp), 
                    fthread = 5, m_mem_free = "200G", h_rt = "0-02:00:00", archive = TRUE,
                    queue = "QUEUE", sgeoutput = paste0(joint_dir, date_time_stamp),
                    skip_if_exists = paste0(joint_dir, date_time_stamp,'/joint.initial.st.rdata'),
                    sing_image = "FILEPATH")

plot_joint <- sbatch(code = paste0(code, "plot_ipums.r"),
                     name = paste0("plot_joint"),
                     arguments = c(non_fatal_repo, settings_file, date_time_stamp),
                     hold = prep_data,
                     fthread = 5, m_mem_free = "100G", h_rt = "0-05:00:00", archive = TRUE,
                     queue = "QUEUE", sgeoutput = paste0(joint_dir, date_time_stamp),
                     sing_image = "FILEPATH")

std_data <- sbatch(code = paste0(code, "/2_standardize_pop.r"),
                   name = paste0("std_data"),
                   arguments = c(non_fatal_repo, settings_file, date_time_stamp),
                   hold = prep_data,
                   fthread = 5, m_mem_free = "200G", h_rt = "0-03:00:00", archive = TRUE,
                   queue = "QUEUE", sgeoutput = paste0(joint_dir, date_time_stamp),
                   skip_if_exists = paste0(joint_dir, date_time_stamp,'/marginal_dist_nophone.rdata'),
                   sing_image = "FILEPATH")

if (withphone ==T){
  raking <- sbatch(code = paste0(code, "/3_raking_final_phone.r"),
                 name = paste0("raking"),
                 arguments = c(non_fatal_repo, settings_file, date_time_stamp), hold=c(std_data),
                 fthread = 10, m_mem_free = "400G", h_rt = "0-48:00:00", archive = TRUE,
                 queue = "QUEUE", sgeoutput = paste0(joint_dir, date_time_stamp),
                 skip_if_exists = paste0(joint_dir, date_time_stamp,'/ps_frame.rds'),
                 sing_image = "FILEPATH")
} else if (withphone ==F){
  raking <- sbatch(code = paste0(code, "/3_raking_final.R"),
                   name = paste0("raking"),
                   arguments = c(non_fatal_repo, settings_file, date_time_stamp), hold=c(std_data),
                   fthread = 10, m_mem_free = "400G", h_rt = "0-48:00:00", archive = TRUE,
                   queue = "QUEUE", sgeoutput = paste0(joint_dir, date_time_stamp),
                   skip_if_exists = paste0(joint_dir, date_time_stamp,'/ps_frame_vetting.rds'),
                   sing_image = "FILEPATH")
}

if (smooth_ps_weights==T){
  smooth <- sbatch(code = paste0(code, "/4_smooth_ps_weights.R"),
                   name = paste0("smooth_wts"),
                   arguments = c(non_fatal_repo, settings_file, date_time_stamp), hold=c(raking),
                   fthread = 5, m_mem_free = "200G", h_rt = "0-48:00:00", archive = TRUE,
                   queue = "QUEUE", sgeoutput = paste0(joint_dir, date_time_stamp),
                   skip_if_exists = paste0(joint_dir, date_time_stamp,'/ps_frame.rds'),
                   sing_image = "FILEPATH")
}


vetting <- sbatch(code = paste0(code, "/5_diagnostics.r"),
                  name = paste0("vetting"),
                  arguments = c(non_fatal_repo, settings_file, date_time_stamp), hold=c(raking),
                  fthread = 10, m_mem_free = "300G", h_rt = "0-12:00:00", archive = TRUE,
                  queue = "QUEUE", sgeoutput = paste0(joint_dir, date_time_stamp) ,
                  sing_image = "FILEPATH")


path <- paste0(joint_dir, date_time_stamp,"/")
system(paste0("ln -sfn ", path, " ", path, "../_BEST"))

print(paste0(joint_dir, date_time_stamp,"/"))
