####################################################################################################
## Description: Upload prepped risk factor input data to USHD database
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

# dataset inputs
# (The rundate should be an SAE directory with a settings file and recode.rds.)
run_date <- "2024_10_09_13_45_27model154"
prepped_inputs_dir <- paste0('FILEPATH', run_date, '/')

imputations <- c(1:10) # imputation 0 is all-draws/ests
mcnty_mapping_crosswalk_id <- 9
post_stratification_frame_id <- 11
covariate_dataset_ids <- as.list(256, 225, 226)
survey_crosswalk_version_id <- 29

# make these args null if you'd like a new one
BRFSS_survey_direct_estimate_collapse_microdata_run_id <- 95
is_best_brfss <- TRUE # if uploading a new set of collapsed BRFSS (otherwise, is_best_brfss will be ignored, and a run_id should be provided)
prev_issues_brfss <- NULL # UPDATEME for each best upload
GALLUP_survey_direct_estimate_collapse_microdata_run_id <- NULL
is_best_gallup <- TRUE # if uploading a new set of collapsed GALLUP (otherwise, is_best_gallup will be ignored, and a run_id should be provided)
prev_issues_gallup <- NULL # UPDATEME for each best upload

final_risk_run_id <- NULL
is_best_final_risk_run <- TRUE # if uploading a new final risk run (otherwise, is_best_final_risk_run will be ignored)
prev_issues_final_risk_run <- NULL # UPDATEME for each upload

# if is_best_brfss is TRUE, then prev_issues_brfss must NOT be NULL. Ditto for gallup & final risk run
# Assert that the above is true
stopifnot(is_best_brfss == !is.null(prev_issues_brfss))
stopifnot(is_best_gallup == !is.null(prev_issues_gallup))
stopifnot(is_best_final_risk_run == !is.null(prev_issues_final_risk_run))

input_settings <- setDT(read.csv(paste0(prepped_inputs_dir, "/settings.csv"), header = F))

tmp_direct_est <- eval(parse(text = input_settings[V1 == "direct_estimates_file_with_strat", V2]))
tmp_collapse <- eval(parse(text = input_settings[V1 == "data_file", V2]))

BRFSS_direct_estimates <- tmp_direct_est[["BRFSS"]]
BRFSS_collapse_microdata <- tmp_collapse[["BRFSS"]]
GALLUP_direct_estimates <- tmp_direct_est[["Gallup"]]
GALLUP_collapse_microdata <- tmp_collapse[["Gallup"]]

message(paste("INPUT DATA VERSIONS:",BRFSS_direct_estimates,GALLUP_direct_estimates, BRFSS_collapse_microdata,GALLUP_collapse_microdata, sep = "\n\n"))

# Make sure none of these are NA, otherwise check that the settings file did not change
stopifnot(!any(sapply(list(BRFSS_direct_estimates,GALLUP_direct_estimates, BRFSS_collapse_microdata,GALLUP_collapse_microdata), is.null)))

# check that the direct est IDs match the results from the settings file, if not NULL
normalize_path <- function(path) {
  return("FILEPATH")
}
ignore_no_strat_diffs <- function(path){
  # ignore differences between brfss_direct_estimates_no_strat and brfss_direct_estimates
  return(gsub("_no_strat", "", normalize_path(path)))
}
if(!is.null(BRFSS_survey_direct_estimate_collapse_microdata_run_id)){
  tmp <- get_survey_direct_estimate_collapse_microdata_run(
    BRFSS_survey_direct_estimate_collapse_microdata_run_id
  )
  tmp <- c(tmp[, .(direct_estimate_file_path,
          collapse_microdata_file_path)])
  stopifnot(normalizePath(BRFSS_direct_estimates) == normalizePath(tmp$direct_estimate_file_path) &  
              normalizePath(BRFSS_collapse_microdata) == normalizePath(tmp$collapse_microdata_file_path))
}

if(!is.null(GALLUP_survey_direct_estimate_collapse_microdata_run_id)){
  tmp <- get_survey_direct_estimate_collapse_microdata_run(
    GALLUP_survey_direct_estimate_collapse_microdata_run_id
  )
  tmp <- c(tmp[, .(direct_estimate_file_path,
                   collapse_microdata_file_path)])
  stopifnot(normalizePath(GALLUP_direct_estimates) == normalizePath(tmp$direct_estimate_file_path) &  
              normalizePath(GALLUP_collapse_microdata) == normalizePath(tmp$collapse_microdata_file_path))
}

# make sure that final_risk_run_id corresponds correctly to direct ests
if(!is.null(final_risk_run_id)){
  tmp <- get_final_risk_run(final_risk_run_id)  
  tmp <- as.numeric(stringr::str_split(tmp[, survey_direct_estimate_collapse_microdata_run_id], pattern = ",")[[1]])
  
  stopifnot(setequal(sort(tmp), 
            sort(c(BRFSS_survey_direct_estimate_collapse_microdata_run_id, GALLUP_survey_direct_estimate_collapse_microdata_run_id))))
}



# Upload survey direct estimates BRFSS
if(is.null(BRFSS_survey_direct_estimate_collapse_microdata_run_id)){
  BRFSS_survey_direct_estimate_collapse_microdata_run_id <-
    save_survey_direct_estimate_collapse_microdata_run(
      survey_source_id = 1, # 1 = BRFSS # aka the input_type to step 3
      survey_crosswalk_version_id = survey_crosswalk_version_id, # id of version from step 5
      mcnty_mapping_crosswalk_id = mcnty_mapping_crosswalk_id, # id of crosswalk from step 1
      direct_estimate_file_path = BRFSS_direct_estimates,
      collapse_microdata_file_path = BRFSS_collapse_microdata,
      description = "Direct ests w/ more county data",
      is_best = is_best_brfss,
      prev_issues = prev_issues_brfss
    )
  message(BRFSS_survey_direct_estimate_collapse_microdata_run_id)
}


# Upload survey direct estimates GALLUP
if(is.null(GALLUP_survey_direct_estimate_collapse_microdata_run_id)){
  GALLUP_survey_direct_estimate_collapse_microdata_run_id <-
    save_survey_direct_estimate_collapse_microdata_run(
      survey_source_id = 2, # aka the input_type to step 3
      survey_crosswalk_version_id = survey_crosswalk_version_id, # id of version from step 5
      mcnty_mapping_crosswalk_id = mcnty_mapping_crosswalk_id, # id of crosswalk from step 1
      direct_estimate_file_path = GALLUP_direct_estimates,
      collapse_microdata_file_path = GALLUP_collapse_microdata,
      description = "Based on newer crosswalk version",
      is_best = is_best_gallup,
      prev_issues = prev_issues_gallup
    )
  message(GALLUP_survey_direct_estimate_collapse_microdata_run_id)
}

# Create final risk run
if(is.null(final_risk_run_id)){
  final_risk_run_id <- save_final_risk_run(
    mcnty_mapping_crosswalk_id = mcnty_mapping_crosswalk_id, 
    post_stratification_frame_id = post_stratification_frame_id,
    rei_id=370,
    recode_path=paste0(prepped_inputs_dir, 'imputation1/recode.rds'),
    survey_direct_estimate_collapse_microdata_run_ids = as.list(c(BRFSS_survey_direct_estimate_collapse_microdata_run_id,
                                                                GALLUP_survey_direct_estimate_collapse_microdata_run_id)), # step 18
    covariate_dataset_ids = covariate_dataset_ids,
    description="Newer data/crosswalk",
    is_best = is_best_final_risk_run,
    prev_issues = prev_issues_final_risk_run
  )
  message(final_risk_run_id)
}

# Not uploading data (not currently used, and it's very large)
# for(i in imputations){
#   message(paste('Uploading input data from imputation', i))
#   save_final_risk_input_data(
#     path = paste0(prepped_inputs_dir, 'imputation', i, '/data.rds'),
#     final_risk_run_id = final_risk_run_id
#   )
# }



