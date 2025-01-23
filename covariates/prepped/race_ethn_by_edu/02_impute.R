###################################################################################################
## Description: Perform imputation for each race/ethnicity by educational attainment separately.
##              Launched by:
##                  .../covariates/counties/prepped/race_ethn_by_edu.r
##              after 01_prep_input_data.R script is finished running.
##              
## Output:      One imputed dataset per r/e group ready for raking and database upload
## 
###################################################################################################

# Setup -------------------------------------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")

# get arguments from launch script
args <- commandArgs(trailingOnly = TRUE)
time_stamp <- args[1]
re <- args[2]

job_start <- Sys.time()
print(paste0("Impute ", re, " job START: ", job_start))

# read in relevant dataset
parent_dir <- paste0(cov_dir, "prepped/", ifelse(re == "hisp", "ethn_", "race_"), re, "_by_edu/")
temp <- readRDS(paste0(parent_dir, "01_prepped_input_data/", time_stamp, ".rds"))
setnames(temp, paste0(re, "_moe"), "moe")  # imputation script expects "moe"

# Perform imputation --------------------------------------------------------------------------------
source("_functions/covariate_imputation_functions.R") # Source imputation functions

output_loc_to_pass = "FILEPATH"
message(output_loc_to_pass)
imputed <- impute_covariate(covar = temp, covariate_name = paste0(re, "_by_edu"), outcome_name = re,
                            family = "binomial", save_plots = T, output_loc = output_loc_to_pass,
                            lib_loc = rlibs_loc, INLA_loc = "FILEPATH", load_saved_intermediate = FALSE,
                            years_to_model = 2000:max(interp_years), stratify_by = "edu",
                            edu_groups = c("Less than HS", "HS graduate", "Some college", "College graduate"))

# Format and save ---------------------------------------------------------------------------------
# subset and rename columns, standardize edu IDs and labels
imputed <- imputed[, mget(c("mcnty", "year", "edu", paste0("raw_", re), "raw_pop", "mean_unscaled"))]
setnames(imputed, c("mean_unscaled", "raw_pop"), c(re, "pop"))

# save output
saveRDS(imputed, paste0(parent_dir, "02_imputed_data/", time_stamp, ".rds"))

job_end <- Sys.time()
print(paste0("Impute ", re, " job END: ", job_end))
print(job_end-job_start)