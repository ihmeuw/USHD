####################################################################################################
## Description: Prepare poverty rate by educational attainment using census 1990 and 2000 data and
##              ACS 2010-2021 5-year series, imputing to fill in intervening years.
####################################################################################################

# Set working directory to us_counties/covariates if not already set
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")
source("FILEPATH")

# Prepare Census data ------------------------------------------------------------------------------
# load data and collapse to merged counties
census_meta <- get_covariate_metadata("census_poverty_by_edu")
census_data <- readRDS(paste0(cov_dir, "FILEPATH", unique(census_meta[, file])))[, -"nid"]
census_data <- merge(census_data, loc, by = "fips", all.x = T)
census_data <- census_data[, list(poverty = sum(poverty), pop = sum(pop)), by = "mcnty,year,edu"]
census_data[is.nan(poverty), poverty := NA]  # assign NA to NaNs that occur when data is missing across all counties in an mcnty
census_data[, moe := NA]  # for rbinding with acs data later

# Prepare ACS data ---------------------------------------------------------------------------------
# load data and collapse to merged counties
acs_meta <- get_covariate_metadata("acs_poverty_by_edu")
acs_data <- readRDS(paste0(cov_dir, "FILEPATH", unique(acs_meta[, file])))[, -"nid"]
acs_data <- merge(acs_data, loc, by = "fips", all.x = T)
acs_data[, var := (moe / 1.645)^2]  # calculate variance from MOE
acs_data <- acs_data[, list(poverty = sum(poverty), var = sum(var), pop = sum(pop)), by = "mcnty,year,edu"]
acs_data[, c("moe", "var") := list(1.645*sqrt(var), NULL)]  # convert back to MOE
acs_data[is.nan(poverty), poverty := NA]  # assign NA to NaNs that occur when data is missing across all counties in an mcnty
acs_data[is.nan(moe), moe := NA]

# Combine Census and ACS data -----------------------------------------------------------------------
data <- rbindlist(list(census_data, acs_data), use.names = TRUE); rm(census_data, acs_data)

# Perform imputation --------------------------------------------------------------------------------
source("_functions/covariate_imputation_functions.R") # Source imputation functions
output_loc = "FILEPATH"
message(output_loc)
pardiso_path <- NULL  # change this to your PARDISO license location, if you have one and want to use it
imputed <- impute_covariate(covar = data, covariate_name = "poverty_by_edu", outcome_name = "poverty",
                            family = "binomial", save_plots = TRUE, output_loc = output_loc, pardiso_path = pardiso_path,
                            INLA_loc = rlibs_loc, lib_loc = rlibs_loc, load_saved_intermediate = FALSE,
                            years_to_model = 1990:max(interp_years), stratify_by = "edu")

# subset and rename columns, standardize edu IDs and labels
imputed <- imputed[, c("uid", "mcnty", "year", "edu", "raw_poverty", "raw_pop", "mean_unscaled", "sd",
                       "median_unscaled", "lower_unscaled", "upper_unscaled")]
setnames(imputed, c("mean_unscaled", "median_unscaled", "lower_unscaled", "upper_unscaled"),
         c("poverty", "median", "lower", "upper"))

data <- copy(imputed)
data[, edu := car::recode(edu, "'< HS'=101; 'HS or equivalent'=102; 'Some college'=103; 'BA or higher'=104; else=100")]
setcolorder(data, c("mcnty", "year", "edu", "poverty"))

# Format and save output ----------------------------------------------------------------------------
# run checks
check_missingness(data)
check_var(data, "year")

# save RDS file
setkeyv(data, c("mcnty", "year"))
out_dir <- paste0(cov_dir, "FILEPATH")
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))  # name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
saveRDS(data, file = paste0(out_dir, date_time_stamp, ".rds"))