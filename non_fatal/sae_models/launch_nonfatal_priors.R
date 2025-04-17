####################################################################################################
## Description: Launch a series of models with different hyperprior specifications 
##
## Inputs:      A "base" directory. The expectation for the purpose of the life expectancy model
##                is that this base directory uses the hyperprior specification for the final model
##              If you run with resub = T, this will just re-launch the models without copying any 
##                of the files over. This should be used when the models are already set up and they
##                just need to be resubmitted starting from whatever modeling stage they last finished.
##
## Outputs:     A new directory with new settings.csv that only has the hyperprior values changed.
##              Submits job to the cluster.
##
## NOTE:        One confusing thing is that I use the argument resub = T to indicate when you don't
##                want to recopy the files over from the base model, but you should ALWAYS
##                run with resub = T in the sbatch() otherwise the code will re-prep the data.
####################################################################################################

library(data.table)

###### Source functions
repo <- paste0("FILEPATH")
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

# Read in the settings from the base model and adapt
settings_path <- "FILEPATH"
settings_file <- "state_model49_inj_trans_20231117_priors"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc)
settings_base <- read.csv(settings_loc, stringsAsFactors = FALSE, header = FALSE)
settings_base <- as.data.table(settings_base)

###### Generate run date and create model output folder
if (!exists("run_date")) {
  if (is.null(run_date)) {
    run_date <- make_time_stamp()
  }
} else if (is.null(run_date) | !resub) {
  run_date <- make_time_stamp()
}

message(run_date)
output_dir <- paste0(main_output_dir, run_date)
output_dir_draws_est <- paste0(draws_est_output_dir, run_date)
message(output_dir)
message(output_dir_draws_est)

#### Read and format hyperprior choices
hyperprior_choices <- fread("FILEPATH")

for (i in 1:nrow(hyperprior_choices)) {
  
  message(paste0("Working on row number ", i))
  
  prior_choice_tmp <- hyperprior_choices[i, prior_type_choice]
  par1_tmp <- hyperprior_choices[i, par1]
  par2_tmp <- hyperprior_choices[i, par2]
  par3_tmp <- hyperprior_choices[i, par3]
  
  prior_string_tmp <- paste0("list(re_cov_par1 = ", paste(par1_tmp), ", re_cov_par2 = ", paste(par2_tmp), ", re_cov_log_sigma = ", paste(par3_tmp), ")")
  
  # make a new directory
  new_mod <- paste0(output_dir, "/" , paste0(run_date, "_", par1_tmp, "_", par2_tmp, "_", prior_choice_tmp))
  
  message("Copying over data and making new settings")
  
  settings_temp <- copy(settings_base)
  
  settings_temp[V1 == "covar_subpop_hyperpriors_settings", V2 := prior_string_tmp]
  
  dir.create(new_mod)
  
  # also save settings in there
  write.csv(settings_temp, paste0(new_mod, "/settings.csv"), row.names = FALSE)
  
  if (n.imp == 0 && !fit_model_for_starting_values) { ## Launch models that don't use imputations and are NOT fit for starting values
    imp <- 0
    
    auto_queue <- set_queue_dynamically(queue = queue, num_jobs = 1, fthread = 1, m_mem_free = "10G", h_rt = "0-10:00:00", archive = TRUE, priority = "RAM")
    
    sbatch(code = paste0(repo, "/submit_single_cause.R"), name = paste0("nonfatal_model_", run_date), 
           arguments = c(repo, new_mod, new_mod, settings_loc, queue, sing_image, fit_model_for_starting_values, run_date), 
           queue = auto_queue, fthread = 1, m_mem_free = "10G", h_rt = "10:00:00", archive = TRUE, 
           sgeoutput = output_dir, sing_image = sing_image)
  }
}
