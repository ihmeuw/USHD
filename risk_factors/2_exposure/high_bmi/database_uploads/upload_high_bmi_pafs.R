####################################################################################################
## Description: Upload PAF models to USHD database
####################################################################################################

library(R.utils)
library(data.table)
library(yaml)

## load database central functions
library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                      R.version$major,
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
ushd_client$save_covariate_population  # running this line ensures successful database connection and prevents interference from other packages

# this might change as pafs years run together in the future
paf_dirs <- paste0(list.dirs(path = "FILEPATH", full.names = T, recursive = FALSE), "/")
# remove "metab_bmi_adult/" from each paf_dirs (code below assumes it is not present)
paf_dirs <- gsub("metab_bmi_adult/", "", paf_dirs)
paf_compile_dir <- "FILEPATH"

# ID values. Fill these in to skip duplicative work
# alternatively set to None to re-do work
# For re-run
model_exp_sd_id <- get_risk_exp_model_run(model_output_type = "exp_sd", get_best = TRUE)$model_exp_run_id
paf_model_metadata_id <- NULL # 34
paf_model_id <- NULL 
paf_draws_saved <- FALSE # TRUE

paf_compile_model_metadata_id <- NULL
paf_compile_model_id <- NULL
paf_compile_draws_saved <- FALSE

is_best_paf <- TRUE
prev_issues_paf <- "Based on older models/raked versions"

is_best_paf_compile <- TRUE
prev_issues_paf_compile <- "Only used first 100 draws (out of 1000) from PAF"

if(is.null(paf_model_metadata_id)){
  # only upload one metadata file for all dirs
  paf_model_metadata_id <- save_paf_metadata(paste0(paf_dirs[1], 'metab_bmi_adult/paf_model_metadata.csv'))
  message("paf_model_metadata_id ", paf_model_metadata_id)
}

if(is.null(paf_model_id)){
  paf_model_id <- save_paf_run(model_exp_run_id = as.integer(model_exp_sd_id),
                               paf_metadata_id = paf_model_metadata_id,
                               rei_id = 370,
                               is_best = is_best_paf,
                               prev_issues = prev_issues_paf)
  message("paf_model_id ", paf_model_id)
}

if(!paf_draws_saved){
  paf_draws <- c()
  for(dir in paf_dirs){
    paf_draws <- c(paf_draws,
                   list.files(paste0(dir, '/metab_bmi_adult/'),
                              "_[123].csv$",
                              full.names = TRUE))
    
  }
  message("Saving ", length(paf_draws), " paf draws files")
  for(draw_file in paf_draws){
    draw_file_name <- gsub('.csv', '', basename(draw_file))
    draw_info <- strsplit(draw_file_name,'_')[[1]]
    
    save_paf_draw_file(paf_run_id = paf_model_id,
                       path = draw_file,
                       location_id = draw_info[1],
                       sex_id = draw_info[2])
  }
}


# COMPILE PAFS
if(is.null(paf_compile_model_metadata_id)){
  paf_compile_model_metadata_id <- save_paf_compile_metadata(compile_root = paf_compile_dir,
                                                             cause_set_version_id = 596,
                                                             rei_set_version_id = 283)
  message("paf_compile_model_metadata_id ", paf_compile_model_metadata_id)
}

if(is.null(paf_compile_model_id)){
  paf_compile_model_id <- save_paf_compile_model_run(paf_compile_metadata_id = paf_compile_model_metadata_id,
                                                     paf_run_ids = as.list(paf_model_id),
                                                     is_best = is_best_paf_compile,
                                                     prev_issues = prev_issues_paf_compile)
  message("paf_compile_model_id ", paf_compile_model_id)
}

if(!paf_compile_draws_saved){
  paf_compile_draws <- list.files(paf_compile_dir,
                                  "[0123456789].csv.gz$",
                                  full.names = TRUE)
  message("Saving ", length(paf_compile_draws), " paf_compile_draws files")
  
  for(draw_file in paf_compile_draws){
    draw_file_name <- gsub('.csv.gz', '', basename(draw_file))
    draw_info <- strsplit(draw_file_name,'_')[[1]]
    
    save_paf_compile_draw_file(paf_compile_run_id = paf_compile_model_id,
                               path = draw_file,
                               location_id = draw_info[1],
                               year_id = draw_info[2])
  }
  message("Completed saving ", length(paf_compile_draws), " paf_compile_draws files")
  
}
