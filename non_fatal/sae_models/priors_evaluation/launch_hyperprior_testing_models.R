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
settings_file <- "FILEPATH"
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

rep_val <- 20 # Set this to a large number to cover even our most complex models; it shouldn't matter if the string is longer than it needs to be

prior_test_type <- "rho"
if (prior_test_type == "variance") {
  
  #### Read and format hyperprior choices
  hyperprior_choices <- fread("FILEPATH")
  hyperprior_choices[, combo := paste0("c(", par1, ", " , par2 , ", ", par3, ")")]
  
  # create the string that needs to go in the settings
  hyperprior_choices[, entire_string := paste0("list(", paste(rep(combo, rep_val), collapse = ", ") , ")"), by = c("prior_type_choice", "par1", "par2", "par3")]
  
  for (i in 1:nrow(hyperprior_choices)) {
    
    message(paste0("Working on row number ", i))
    
    prior_choice_tmp <- hyperprior_choices[i, prior_type_choice]
    par1_tmp <- hyperprior_choices[i, par1]
    par2_tmp <- hyperprior_choices[i, par2]
    par3_tmp <- hyperprior_choices[i, par3]
    
    prior_choice_tmp <- hyperprior_choices[i, prior_type_choice]
    prior_string_tmp <- hyperprior_choices[i, entire_string]
    
    # make a new directory
    new_mod <- paste0(output_dir, "/" , paste0(run_date, "_", par1_tmp, "_", par2_tmp, "_", prior_choice_tmp))
    
    message("Copying over data and making new settings")
    
    settings_temp <- copy(settings_base)
    
    settings_temp[V1 == "prior_type", V2 := prior_choice_tmp]
    settings_temp[V1 == "prior_list", V2 := prior_string_tmp]
    
    dir.create(new_mod)
    
    # copy needed files
    file.copy(paste0(output_dir, "/data_pre_factor.rds"), new_mod)
    file.copy(paste0(output_dir, "/recode.rds"), new_mod)
    file.copy(paste0(output_dir, "/num_vars.RData"), new_mod)
    file.copy(paste0(output_dir, "/re_graphs.RData"), new_mod)
    file.copy(paste0(output_dir, "/data.rds"), new_mod)
    
    # also save settings in there
    write.csv(settings_temp, paste0(new_mod, "/settings.csv"), row.names = FALSE)
    
    # now make the non-LU directories
    new_mod2 <- paste0(output_dir_draws_est, "/" , paste0(run_date, "_", par1_tmp, "_", par2_tmp, "_", prior_choice_tmp))
    dir.create(new_mod2)
    
    message(paste0("Launching model for ", paste0(new_mod)))
    
    auto_queue <- set_queue_dynamically(queue = queue, num_jobs = 1, fthread = 1, m_mem_free = "1G", h_rt = "0-10:00:00", archive = TRUE, priority = "RAM")
    
    sbatch(code = paste0(repo, "/submit_single_cause.R"),
           name = paste0("nonfatal_model_", paste0(run_date, "_", par1_tmp, "_", par2_tmp, "_", prior_choice_tmp)),
           arguments = c(repo, new_mod, new_mod2, paste0(new_mod, "/settings.csv"), queue, sing_image, FALSE, run_date),
           queue = auto_queue, fthread = 1, m_mem_free = "1G", h_rt = "0-10:00:00",
           archive = TRUE, sgeoutput = new_mod,
           sing_image = sing_image)
  }
} else if (prior_test_type == "rho") {
  #### Read and format hyperprior choices
  hyperprior_choices <- fread("FILEPATH")
  hyperprior_choices[, entire_string := paste0("c(", rho_mean, ", " , rho_variance, ")")]
  
  for (i in 1:nrow(hyperprior_choices)) {
    
    # make a new directory
    new_mod <- paste0(output_dir, "/" , paste0(run_date, "_rho_", hyperprior_choices[i, rho_mean], "_", hyperprior_choices[i, rho_variance]))
    
    message("Copying over data and making new settings")
    
    settings_temp <- copy(settings_base)
    
    settings_temp <- rbindlist(list(settings_temp, data.table(V1 = "rho_prior", V2 = hyperprior_choices[i, entire_string])), use.names = TRUE, fill = TRUE)
    
    dir.create(new_mod)
    
    # copy needed files
    file.copy(paste0(output_dir, "/data_pre_factor.rds"), new_mod)
    file.copy(paste0(output_dir, "/recode.rds"), new_mod)
    file.copy(paste0(output_dir, "/num_vars.RData"), new_mod)
    file.copy(paste0(output_dir, "/re_graphs.RData"), new_mod)
    file.copy(paste0(output_dir, "/data.rds"), new_mod)
    
    # also save settings in there
    write.csv(settings_temp, paste0(new_mod, "/settings.csv"), row.names = FALSE)
    
    # now make the non-LU directories
    new_mod2 <- paste0(output_dir_draws_est, "/" , paste0(run_date, "_rho_", hyperprior_choices[i, rho_mean], "_", hyperprior_choices[i, rho_variance]))
    dir.create(new_mod2)
    
    message(paste0("Launching model for ", paste0(new_mod)))
    
    auto_queue <- set_queue_dynamically(queue = queue, num_jobs = 1, fthread = 1, m_mem_free = "1G", h_rt = "0-10:00:00", archive = TRUE, priority = "RAM")
    
    sbatch(code = paste0(repo, "/submit_single_cause.R"),
           name = paste0("nonfatal_model_", paste0(run_date, "_rho_", hyperprior_choices[i, rho_mean], "_", hyperprior_choices[i, rho_variance])),
           arguments = c(repo, new_mod, new_mod2, paste0(new_mod, "/settings.csv"), queue, sing_image, FALSE, run_date),
           queue = auto_queue, fthread = 1, m_mem_free = "1G", h_rt = "0-10:00:00",
           archive = TRUE, sgeoutput = new_mod,
           sing_image = sing_image)
  }
}
