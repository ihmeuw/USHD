####################################################################################################
## Description: Upload prepped risk factor SAE models to USHD database
####################################################################################################

library(R.utils)
library(data.table)
library(yaml)

# load database central functions
library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                      R.version$major,
                                      strsplit(R.version$minor, ".", fixed = TRUE)[[1]][[1]]))
library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
# running this line ensures successful database connection and prevents interference from other packages
ushd_client$save_covariate_population


# set directories
measure <- "obese"  # "overweight" or "obese"
rundate <- "2024_10_09_13_52_29model154"
sae_model_dir <- paste0("FILEPATH", rundate)
sae_model_dir_LU <- paste0("FILEPATH", rundate)
is_best_sae <- TRUE # whether this should be new BEST model in DB
my_description = "Run SAE model based on new BMI CW model (includes additional years of NHANES & BRFSS data)"
prev_issues_sae = "Used slightly different version of CW. Need to update to match the same version used in GBD 2023 final run" # if uploading new best, specify any previous issues

# upload setup
final_risk_run_id <- 62
measure_id <- 5
survey_crosswalk_version_id <- 29
raking_status <- "unraked"  # raked or unraked, depending what you want to upload

# rerun variable settings
sae_run_id <- NULL
sae_model_input_file_id <- NULL
parent_run_id <- NULL # NULL if not raked

if(raking_status == 'raked'){
  imputations <- 0
}else if(raking_status == 'unraked'){
  imputations <- c(1:10)
}
sexes <- c(1:2)
geos <- c("mcnty", "state", "natl")

# set prevalence type based on directory name
prev_type <- ifelse(grepl("overweight", sae_model_dir), "overweight",
                    ifelse(grepl("underweight", sae_model_dir), "underweight",
                           ifelse(grepl("obese", sae_model_dir), "obese", 
                                  stop("unrecognized prevalence type"))))

# set modelable entity id (MEI) based on prevalence type
# Relevant MEIs:
# 27261	Adult prevalence of underweight [USHD]
# 27262	Adult prevalence of overweight [USHD]
# 27263	Adult prevalence of obesity [USHD]
modelable_entity_id <- switch(prev_type,
                              underweight = 27261,
                              overweight = 27262, 
                              obese = 27263)

# read in model settings and get year and race info
model_settings <- model_settings_test <- fread(paste0(sae_model_dir, "/settings.csv"), 
                                               col.names = c('setting_name','setting_value'), 
                                               header = FALSE)
years <- unlist(strsplit(model_settings[setting_name == "years", setting_value], ":"))  # convert to character vector of min and max years
years <- years[1]:years[2]  # convert to integer vector of years
races <- eval(parse(text = model_settings[setting_name == "races", setting_value]))

# get base folder name
sae_model_run_name <- paste0(basename(sae_model_dir), 
                             ifelse(raking_status == 'raked', '_raked', ''))

# upload model run metadata
if(is.null(sae_run_id)){
  sae_run_id <- save_sae_model_run(final_risk_run_id = final_risk_run_id, 
                                   measure_id = measure_id,
                                   modelable_entity_id = modelable_entity_id,
                                   model_output_type = raking_status,
                                   survey_crosswalk_version_id = survey_crosswalk_version_id,
                                   sae_model_run_name = sae_model_run_name, 
                                   path = sae_model_dir, # specify the "share" path so that this directory corresponds to the pred directories
                                   description = my_description,
                                   is_best = is_best_sae, 
                                   prev_issues = prev_issues_sae,
                                   status = 1,
                                   
                                   parent_run_id = parent_run_id)
  message(sae_run_id)
}

# determine whether there is phone information (if phone is a stratification variable)
if (grepl("phone", model_settings[setting_name == "strat_vars", setting_value])) {
  phone_status <- "has phone info"
} else {
  phone_status <- "no phone"
}

# upload model input file metadata
if(is.null(sae_model_input_file_id)){
  sae_model_input_file_id <- save_sae_input_file(sae_model_run_id = sae_run_id, 
                                                 input_file_type = phone_status,
                                                 filepath = paste0(sae_model_dir, "/settings.csv"))
}

# upload metadata for model fit object, parameter draws, and prediction draws
# upload data for summary of draws
for (imp in imputations) {
  message("imp: ", imp)
  for (sex in sexes) {
    message("sex: ", sex)
    # model fit object
    model_object_file <- paste0(sae_model_dir_LU, "/imputation", imp, "/model_fit_", sex, ".rds")
    sae_model_object_id <- save_sae_model_object(sae_run_id, model_object_file)
    
    # parameter draws
    model_parameter_file <- paste0(sae_model_dir, "/imputation", imp, "/initial_sims_", sex, "_99.rds")
    sae_model_parameter_draw_file_id <- save_sae_model_parameter_draw_file(sae_model_object_id, model_parameter_file)
  
    for (geo in geos) {
      message("geo: ", geo)
      for (year in years) {
        # message("year: ", year)
        for (race in races) {
          # message("race: ", race)
          # prediction draws
          model_prediction_file <-
            paste0(sae_model_dir, "/imputation", imp, "/draws/draws_", geo, "_", year, "_", sex, "_", race, "_1", 
                   ifelse(raking_status == 'raked', '_raked', ''), ".rds")
          sae_model_prediction_draw_file_id <-
            save_sae_model_prediction_draw_file(sae_model_parameter_draw_file_id, model_prediction_file)
          
          # summary of draws
          model_prediction_result <-
            paste0(sae_model_dir, "/imputation", imp, "/est/est_", geo, "_", year, "_", sex, "_", race, "_1", 
                   ifelse(raking_status == 'raked', '_raked', ''), ".rds")
          # comment out for now -- issue b/c of the source_V2 column in ests
        }
      }
    }
  }
}
