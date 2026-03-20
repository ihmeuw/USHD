####################################################################################################
## Description: Launch script for the INLA-based BMI correction model.
## 
##  
## Inputs: settings file: FILEPATH
##              
## Outputs: FILEPATH
##         
####################################################################################################

settings_path <- "FILEPATH"
settings_file <- "settings_vers59_inla_cw_form_36"
settings_loc <- paste0(settings_path, settings_file, ".csv")
stopifnot(file.exists(settings_loc))
# Common set-up -----------------------------------------------------------

if (dir.exists(paste0('FILEPATH', Sys.info()['user']))) {
  repo <- paste0('FILEPATH')
} else {
  repo <- paste0('FILEPATH', Sys.info()['user'], '/us_counties/risk_factors/')
}

source(paste0(repo, '0_functions/helper/_versioning_functions.R')) # includes make_time_stamp() + get_git_status()
source(paste0(repo, "../non_fatal/sae_models/functions/sbatch.R"))
source(paste0(repo, "../non_fatal/sae_models/functions/set_queue_dynamically.R"))
source(paste0(repo, "../non_fatal/sae_models/functions/settings.R"))

library(data.table, quietly = TRUE)

get_settings(settings_loc)


root_LU <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
out_dir_main <- paste0(root_LU, "FILEPATH")

# Generate run date and create model output folder
if(!resub){
  run_date <- make_time_stamp()
  run_date <- paste0(run_date, "_", model_num)
}
message(run_date)
output_dir <- paste0(out_dir_main, run_date, "/")
message(output_dir)
if(!resub){
  dir.create(output_dir, recursive = T)
}


# Save git history to output_dir
cat(get_git_status(repo = repo, "Risk Factor Repo", show_diff = T), file = paste0(output_dir, "git_info.txt"),sep="\n")

################################################################################
# Define final model  ----------------------------------------
################################################################################



# save the settings csv to the directory
file.copy(settings_loc, paste0(output_dir, "/settings.csv"), overwrite = T)
# write the name of the settings file to a text file
cat(settings_file, file = paste0(output_dir, "settings_file_name.txt"))

################################################################################
# Launch models  ----------------------------------------
################################################################################

memory <- "50G"
threads <- 10
h_rt= "00-6:00:00" 
q <- set_queue_dynamically(queue = "auto", num_jobs = n_jobs, fthread = threads, m_mem_free=memory, h_rt = h_rt, archive = F)
model_jid <- sbatch(code = paste0(repo, "/2_exposure/high_bmi/correction_models/model_based_cw/INLA_based_bmi_correction_binned_quantiles.R"),
                     arguments = output_dir,
                     sing_image = "FILEPATH", # updated to use latest image instead of 422 on 2024-08-23
                     name = paste0("bmi_crosswalk_",run_date),
                     fthread = threads,
                     m_mem_free = memory,
                     sgeoutput = output_dir,
                     h_rt = h_rt,
                     archive = F,
                     queue = "QUEUE")

# Launch vetting plot (compare self-reported to adjusted to measured trends)
memory <- "120G"
threads <- 4
h_rt= "00-18:00:00" 
q <- set_queue_dynamically(queue = "auto", num_jobs = n_jobs, fthread = threads, m_mem_free=memory, h_rt = h_rt, archive = F)
vetting_jid <- sbatch(code = paste0(repo, "/2_exposure/high_bmi/correction_models/vetting/detailed_compare_cw_sr_trends.R"),
                     arguments = run_date,
                     hold = model_jid,
                     sing_image = "FILEPATH",
                     name = paste0("plot_bmi_crosswalk_",run_date),
                     fthread = threads,
                     m_mem_free = memory,
                     sgeoutput = output_dir,
                     h_rt = h_rt,
                     archive = T, # needed for the state location files
                     queue = "QUEUE,QUEUE")


# plot model fit & parameters
memory <- "40G"
threads <- 1
h_rt= "00-1:00:00" 
q <- set_queue_dynamically(queue = "auto", num_jobs = n_jobs, fthread = threads, m_mem_free=memory, h_rt = h_rt, archive = F)
vetting_jid <- sbatch(code = paste0(repo, "/2_exposure/high_bmi/correction_models/model_based_cw/INLA_BMI_cw_plot_model.R"),
                     arguments = output_dir,
                     hold = model_jid,
                     sing_image = "FILEPATH",
                     name = paste0("plot_fit_crosswalk_",run_date),
                     fthread = threads,
                     m_mem_free = memory,
                     sgeoutput = output_dir,
                     h_rt = h_rt,
                     archive = T, # needed for the state location files
                     queue = q)

# There is an additional vetting plot that compares versions. This currently must be run separately.

print(run_date)
print(output_dir)
print(settings_file)


# code to upload to DB
# stop("Only run this after vetting models")
# source(paste0(repo, "0_functions/load_ushd_db.R"))
# micro_meta <- get_compile_microdata(get_best = T)
# ushd.dbr::save_crosswalk_version(settings_path = settings_loc,
#                         rei_id = 370,
#                         parent_survey_crosswalk_version_id = NULL,
#                         survey_compile_microdata_version_id = unique(micro_meta$survey_compile_microdata_version_id),
#                         output_file_path = output_dir,
#                         modeler = Sys.info()[["user"]],
#                         is_best = TRUE,
#                         prev_issues = "Did not include NHANES 2021-2023 wave",
#                         description = "INLA based BMI crosswalk using formula 36. Update data & pairs of years to include NHANES 2021-2023",
#                         discussion = "not much yet")
# ushd.dbr::update_crosswalk_version(
#   best_status = 1, # for best
#   survey_crosswalk_version_id = 28, # remember to update
#   prev_issues = "Quantiles were calculated separately by race. Decade effect questionable. Used 20-year age groups, resulting in artificial age patterns"
# )
