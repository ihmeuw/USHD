####################################################################################################
## Description: Upload high bmi exposure models to USHD database
####################################################################################################

library(R.utils)
library(data.table)
library(yaml)
## load database central functions
library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                      R.version$major,
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
# running this line ensures successful database connection and prevents interference from other packages
ushd_client$save_covariate_population

# ID values. Fill these in to skip duplicative work
# alternatively set to NULL and FALSE to re-do work
# Update descriptions for each function call if necessary
bmi_mean_model_metadata_id <- 100
bmi_mean_model_id <- 114
bmi_mean_draws_saved <- TRUE
bmi_mean_summary_saved <- TRUE

bmi_sd_model_metadata_id <- NULL
bmi_sd_model_id <- NULL
bmi_sd_draws_saved <- FALSE
bmi_sd_summary_saved <- FALSE

sae_model_run_ids <- c(43,44) # or provide a list of IDs -- otherwise, determined below

population_group_set_id <- 1 # OMB 1977 race codes

bmi_mean_model <- "FILEPATH"
bmi_mean_model_raked <- FALSE # UPDATE IF NEEDED
bmi_exp_sd_model <- "FILEPATH"

is_best_exp_sd <- TRUE
prev_issues_exp_sd <- "Based on older exp_mean" # if uploading new best, specify any previous issues
exp_sd_desc = "1000 draw exp_sd run 2000-2019"

# resolve the symlink
bmi_mean_model_name <- basename(bmi_mean_model)
bmi_exp_sd_model_name <- basename(bmi_exp_sd_model)

stopifnot(dir.exists(bmi_mean_model) & dir.exists(bmi_exp_sd_model))

# get ensemble weight IDs from exp_sd model (these weights are uploaded in the ensemble weight
# portion of repo)
all_wts <- get_ensemble_weights_metadata(version_type = "race", get_best = F)
exp_sd_settings <- fread(paste0(bmi_exp_sd_model, "/settings.csv"), header = F)

bmi_sd_ensemble_weights_id <- as.numeric(exp_sd_settings[V1 == "ensemble_weight_version_ids", V2])

# # Note -- in newer settings files, we've specified the population_set_id instead of
# # the population file path. The metadata uploader expected the filepath. 
# # If the settings file for overweight/obesity includes pop_covariate_dataset_id but
# # not pop_file (check that it is not there), the following code will determine the
# # appropriate pop file associated with covariate dataset ID and insert it into
# # the settings file.

# # This code is commented out because it does not need to be run every time, but
# # is still necessary in some cases.
# mean_settings_dt <- fread(paste0(bmi_mean_model, "/settings.csv"), header = F)
# for(prev in c("overweight", "obese")){
#   # get the {prev}_settings file
#   prev_settings <- mean_settings_dt[V1 == sprintf("in_%s_est", prev), V2]
#   prev_settings_path <- paste0(gsub("\"", "", prev_settings), "/settings.csv")
#   prev_settings <- read.csv(prev_settings_path, header = F)
#   setDT(prev_settings)
#   # check if the settings file includes the pop_covariate_dataset_id, check that
#   # pop_file is not there.
#   if("pop_covariate_dataset_id" %in% prev_settings$V1 & 
#     !"pop_file" %in% prev_settings$V1){
#     # get the pop_covariate_dataset_id
#     pop_covariate_dataset_id <- as.numeric(prev_settings[V1 == "pop_covariate_dataset_id", V2])
#     # get the pop file associated with this covariate dataset ID
#     pop_versions <- get_covariate_version("pop_by_race_ethn_1977")
#     # which version corresponds to the covariate dataset ID?
#     pop_filepath <- pop_versions[pop_covariate_dataset_id == covariate_dataset_id, dataset_filename]
#     # get the full path to the pop file
#     pop_filepath <- paste0("FILEPATH", pop_versions[pop_covariate_dataset_id == covariate_dataset_id]$covariate_name_short, "/", pop_filepath)
#     print(pop_filepath)
#     # insert the pop_file into the settings file
#     prev_settings <- rbind(prev_settings[, c(1,2)], data.table(V1 = "pop_file", V2 = pop_filepath), fill = T)
#     # save the updated settings file
#     fwrite(prev_settings, prev_settings_path, col.names = F)
#   }
# }

# 1. save metadata AKA settings
if(is.null(bmi_mean_model_metadata_id)){
  bmi_mean_model_settings <- paste0(bmi_mean_model, "/settings.csv")
  # Note -- this line sometimes gives an error b/c it will check the 
  #   filepaths for underweight, overweight, and obese models. If the 
  #   underweight models is omitted, delete that line from the settings
  #   file.
  bmi_mean_model_metadata_id <- save_risk_exp_model_metadata(bmi_mean_model_settings)
  message(paste("bmi_mean_model_metadata_id is", bmi_mean_model_metadata_id))
}

# 2. save model

if(is.null(sae_model_run_ids)){
  # determine the sae_model_run_id associated with this mean BMI model
  # unless otherwise specified above, use the current "best" associated 
  # with ov, ob (MEI 27262, 27263, respectively)
  sae_model_run_ids <- rbindlist(sapply(list(27262, 27263), function(mei) list(get_sae_model_run(modelable_entity_id = mei, is_best = F)[,.(sae_model_run_id, sae_model_run_filepath, sae_model_run_name)])))
  sae_model_run_ids[, sae_model_run_filepath := gsub("/$", "", sae_model_run_filepath)]
  
  # check that the sae_model_run_ids correspond to the model dirs in the mean BMI settings file
  metadata <- get_risk_exp_model_metadata(model_exp_metadata_id = bmi_mean_model_metadata_id)
  in_dirs <- unlist(metadata[grepl(pattern= "in_[a-zA-Z]+_est", x= key) , value])
  in_dirs <- gsub("/$", "", in_dirs) # remove trailing slash
  in_dirs <- gsub("\"", "", in_dirs) # remove escaped quotations
  names(in_dirs) <- basename(dirname(in_dirs))
  
  sae_model_run_ids[, inputs_mean_model := sae_model_run_name  %in% sapply(in_dirs, basename)]
  # check that the basename of the model associated with model IDs matches basename of input to mean BMI model
  # They won't be the exact same because the LU version of the path is saved in DB
  if(sae_model_run_ids[, sum(inputs_mean_model)] != length(in_dirs)){
    stop("The directories in the mean BMI model setting don't match the directories associated
       with the sae_model_run_ids.")
  }
  sae_model_run_ids <- sae_model_run_ids[inputs_mean_model == TRUE, sae_model_run_id]
}

if(is.null(bmi_mean_model_id)){
  if(bmi_mean_model_raked){ # find parent model if raked
    unraked_model_run <- get_risk_exp_model_run(model_run_name = paste("bmi_mean_", bmi_mean_model_name, sep = ""))
  }
    
  bmi_mean_model_id <- save_risk_exp_model_run(
    model_run_name=paste("bmi_mean_", bmi_mean_model_name, ifelse(bmi_mean_model_raked, "_raked", ""), sep = ""),
    parent_run_id = if(bmi_mean_model_raked) {unraked_model_run$model_exp_run_id} else {NULL},
    model_exp_metadata_id=bmi_mean_model_metadata_id,
    sae_model_run_ids = as.list(sae_model_run_ids),
    model_focus_type="race",
    model_output_type=ifelse(bmi_mean_model_raked, "exp_raked", "exp_unraked"),
    population_group_set_id = population_group_set_id,
    rei_id=370,  # BMI
    modelable_entity_id=27160,
    description= mean_desc,
    is_best = is_best_exp_mean,
    prev_issues = prev_issues_exp_mean
  )
  message(paste("bmi_mean_model_id is", bmi_mean_model_id))
}

# 3. save draw files
if(!bmi_mean_draws_saved){
  bmi_mean_model_draws_dir <- paste0(bmi_mean_model, "/draws/")
  bmi_mean_model_draws <- grep(pattern = "raked.rds", 
                               list.files(bmi_mean_model_draws_dir,
                                          "^draws", 
                                          full.names = TRUE),
                               invert = !bmi_mean_model_raked,
                               value = T)
  bmi_mean_model_draws <- grep(pattern = "all", 
                               bmi_mean_model_draws,
                               invert = T,
                               value = T)
  # remove any paths where the path does not end if .rds (ignore capitalization)
  bmi_mean_model_draws <- bmi_mean_model_draws[grepl("rds$", bmi_mean_model_draws, ignore.case = T)]
  
  
  bmi_mean_model_draw_file_ids = c()
  for(draw_file in bmi_mean_model_draws){
    draw_file_id <- save_risk_exp_model_draws_file(bmi_mean_model_id,
                                                   draw_file,
                                                   draw_type=ifelse(bmi_mean_model_raked, "exp_raked", "exp_unraked"))
    bmi_mean_model_draw_file_ids <- c(bmi_mean_model_draw_file_ids, draw_file_id)
  }
  message(paste("Loaded", length(bmi_mean_model_draw_file_ids),"draw files"))
}

# 4. summary data
if(!bmi_mean_summary_saved){
  bmi_mean_model_est_dir <- paste0(bmi_mean_model, "/est/")
  bmi_mean_model_estimates <-  grep(pattern = "raked.rds", 
                                    list.files(bmi_mean_model_est_dir,
                                               "^est_",
                                               full.names = TRUE),
                                    invert = !bmi_mean_model_raked,
                                    value = T)   
  bmi_mean_model_estimates <- grep(pattern = "all", 
                                   bmi_mean_model_estimates,
                                   invert = T,
                                   value = T)
  # remove any paths where the path does not end if .rds (ignore capitalization)
  bmi_mean_model_estimates <- bmi_mean_model_estimates[grepl("rds$", bmi_mean_model_estimates, ignore.case = T)]

  bmi_mean_model_est_file_ids = c()
  for(draw_file in bmi_mean_model_estimates){
    draw_file_id <- save_risk_exp_model_summary_data(bmi_mean_model_id,
                                                     draw_file,
                                                     summary_type=ifelse(bmi_mean_model_raked, "exp_raked", "exp_unraked"))
    bmi_mean_model_est_file_ids <- c(bmi_mean_model_est_file_ids, draw_file)
  }
  message(paste("Loaded", length(bmi_mean_model_est_file_ids), "est files"))
}

# 6. save metadata AKA settings
if(is.null(bmi_sd_model_metadata_id)){
  bmi_sd_model_settings = paste0(bmi_exp_sd_model, "/settings.csv")
  bmi_sd_model_metadata_id = save_risk_exp_model_metadata(bmi_sd_model_settings)
  message(paste("bmi_sd_model_metadata_id is", bmi_sd_model_metadata_id))
}

# 7. save model
if(is.null(bmi_sd_model_id)){
  bmi_sd_model_id = save_risk_exp_model_run(
    model_run_name=paste("bmi_exp_sd", bmi_exp_sd_model_name, sep = "_"),
    model_exp_metadata_id=bmi_sd_model_metadata_id,
    sae_model_run_id = sae_model_run_ids,
    model_focus_type="race",
    model_output_type="exp_sd",
    population_group_set_id = population_group_set_id,
    rei_id=370,  # BMI
    modelable_entity_id=27161,
    description=exp_sd_desc,
    is_best = is_best_exp_sd,
    prev_issues = prev_issues_exp_sd,
    # NOTE: these args are for exp_sd ONLY
    ensemble_weight_version_id=bmi_sd_ensemble_weights_id,
    parent_run_id=bmi_mean_model_id
  )
  message(paste("bmi_sd_model_id is", bmi_sd_model_id))
}

# 8. save draw files
if(!bmi_sd_draws_saved){
  bmi_sd_model_draws_dir <- paste0(bmi_exp_sd_model, "/draws/")
  bmi_sd_model_draws <- list.files(bmi_sd_model_draws_dir,
                                   "^draws_",
                                   full.names = TRUE)
  
  bmi_sd_model_draw_file_ids <- c()
  for(draw_file in bmi_sd_model_draws){
    draw_file_id <- save_risk_exp_model_draws_file(bmi_sd_model_id, draw_file, draw_type="exp_sd")
    bmi_sd_model_draw_file_ids <- c(bmi_sd_model_draw_file_ids, draw_file_id)
  }
  message(paste("Loaded", length(bmi_sd_model_draw_file_ids), "draw files"))
}

# 9. summary data
if(!bmi_sd_summary_saved){
  bmi_sd_model_est_dir <- paste0(bmi_exp_sd_model, "/est/")
  bmi_sd_model_estimates <- list.files(bmi_sd_model_est_dir,
                                       "^est_",
                                       full.names = TRUE)
  
  bmi_sd_model_est_file_ids <- c()
  for(draw_file in bmi_sd_model_estimates){
    draw_file_id <- save_risk_exp_model_summary_data(bmi_sd_model_id, draw_file, summary_type="exp_sd")
    bmi_sd_model_est_file_ids <- c(bmi_sd_model_est_file_ids, draw_file_id)
  }
  message(paste("Loaded", length(bmi_sd_model_est_file_ids), "est files"))
}