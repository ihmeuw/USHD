#################################################################
# Description: Launcher script to impute data by individual year
#              impute_single_year.R
#################################################################

rm(list = ls())

args <- commandArgs(trailingOnly = TRUE)
run_type <- args[1]  # get run type. "mortality" or "births" based on parameter passed in _run_all.py/_run_all_births.py
run_type <- gsub('\r', '', run_type)
message('Run type: ', run_type)
print(run_type)
archive_date <- args[2]

library(data.table)
library(fs)
ushd_dir <- "FILEPATH"
repo_dir <- "FILEPATH"
functions_dir <- 'FILEPATH/data_prep/counties/02_clean_pre_pipeline/_functions/'

# source backcasting script
options(run.main = FALSE) 
if (file.exists(file.path("FILEPATH/data_prep/counties/02_clean_pre_pipeline/backcast_births_proportions.r"))) {
  source(file.path("FILEPATH/data_prep/counties/02_clean_pre_pipeline/backcast_births_proportions.r"))
} else {
  source("FILEPATH/data_prep/counties/02_clean_pre_pipeline/backcast_births_proportions.r")
}

if (run_type == "mortality") {
  for (run_subtype in c('mortality','linked_births')) {
    data_out_dir <- "FILEPATH"

    if(run_subtype == 'mortality'){
      year_list <- 1980:2020
    }else{
      year_list <- 1995:2018
    }

    job_names <- c()
    for(year in year_list){
      job_names <- c(job_names,
                     launch_imputation_script(year, run_subtype, archive_date))

    }
    hold_job(job_names)
    check_imputation(year_list, data_out_dir)
  }
} else if (run_type == "births") {
  year_list <- 1989:2020
  job_names <- c()
  data_out_dir <- "FILEPATH"
  for(year in year_list){
    job_names <- c(job_names,
                   launch_imputation_script(year, run_type, archive_date))

  }
  hold_job(job_names)
  check_imputation(year_list, data_out_dir)
}

# if running births, also launch interpolation and backcasting to eliminate all missing education
if (run_type == "births") {
  interp_job <- launch_births_edu_interpolation(archive_date)
  hold_job(interp_job)

  # run backcasting
  apply_backcasting_props(archive_date)
}
