###################################################################################################
## Description: Prepare percent foreign-born population by education using ACS 2009-2021 5-year
##              series, imputing to fill in intervening years.
##
## Note: no decennial data appear to exist for this tabulation, so we are using ACS only.
###################################################################################################

# Set working directory to us_counties/covariates if not already set
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")

# Prepare ACS data --------------------------------------------------------------------------------
acs_meta <- get_covariate_metadata("acs_foreign_born_by_edu")
data <- readRDS(paste0(cov_dir, "FILEPATH", unique(acs_meta[, file])))[, -"nid"]
data <- merge(data, loc, by = "fips", all.x = T)
data[, var := (moe / 1.645)^2]  # calculate variance from MOE (for collapsing to merged counties)
data <- data[, list(foreign_born = sum(foreign_born), var = sum(var), pop = sum(pop)), by = "mcnty,year,edu"]
data[, c("moe", "var") := list(1.645*sqrt(var), NULL)]  # convert back to MOE
data[is.nan(foreign_born), foreign_born := NA]  # assign NA to NaNs that occur when data is missing across all counties in an mcnty
data[is.nan(moe), moe := NA]

# Perform imputation ------------------------------------------------------------------------------
source("_functions/covariate_imputation_functions.R") # Source imputation functions

output_loc = "FILEPATH"
dir.create(output_loc)
message(output_loc)
pardiso_path <- NULL  # change this to your PARDISO license location, if you have one and want to use it
imputed <- impute_covariate(covar = data, covariate_name = "foreign_born_by_edu", outcome_name = "foreign_born",
                            family = "binomial", save_plots = TRUE, output_loc = output_loc, pardiso_path = pardiso_path,
                            INLA_loc = rlibs_loc, lib_loc = rlibs_loc, load_saved_intermediate = FALSE,
                            years_to_model = 1990:max(interp_years), stratify_by = "edu")

# subset and rename columns, standardize edu IDs and labels
imputed <- imputed[, c("uid", "mcnty", "year", "edu", "raw_foreign_born", "raw_pop", "mean_unscaled", "sd",
                       "median_unscaled", "lower_unscaled", "upper_unscaled")]
setnames(imputed, c("mean_unscaled", "median_unscaled", "lower_unscaled", "upper_unscaled"),
         c("foreign_born", "median", "lower", "upper"))

data <- copy(imputed)
data[, edu := car::recode(edu, "'< HS'=101; 'HS or equivalent'=102; 'Some college'=103; 'BA or higher'=104; else=100")]
setcolorder(data, c("mcnty", "year", "edu", "foreign_born"))

# Format and save output --------------------------------------------------------------------------
# run checks
check_missingness(data)
check_var(data, "year")

# save RDS file
setkeyv(data, c("mcnty", "year"))
out_dir <- paste0(cov_dir, "FILEPATH")
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))  # name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
saveRDS(data, file = paste0(out_dir, date_time_stamp, ".rds"))