####################################################################################################
## Description: Prepare the "edu_hs" and "edu_ba" covariates (proportion of the population that have
##              at least a high school diploma/GED or bachelors degree, respectively) using
##              decennial census and ACS data, interpolating to fill in the intervening years.
####################################################################################################

user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")
source("_functions/_prep_workspace.r")

# Load source metadata and extractions -------------------------------------------------------------
census_meta <- get_covariate_metadata("census_education")
acs_meta <- get_covariate_metadata("acs_education")
covar <- rbind(readRDS(paste0(cov_dir, "raw/census_education/", unique(census_meta[, file])))[, -"nid"],
               readRDS(paste0(cov_dir, "raw/acs_education/", unique(acs_meta[, file])))[, -"nid"], fill = T)

# Collapse to merged counties ----------------------------------------------------------------------
covar <- merge(covar, loc, by = "fips", all.x = T)
covar <- covar[, list(edu_ba = weighted.mean(edu_ba / 100, pop),
                      edu_hs = weighted.mean(edu_hs / 100, pop)),
               by = "mcnty,year"]
stopifnot(sum(is.na(covar)) == 0)

# Interpolate --------------------------------------------------------------------------------------
covar <- forecast_backcast_interpolate(covar, interp_years, covar_name = "edu", geog_id = "mcnty",
                                       logit_transform_list = c("edu_hs", "edu_ba"))

# Format and save output ---------------------------------------------------------------------------
# run checks
check_missingness(covar)
check_var(covar, "year")

# save RDS files
setkeyv(covar, c("mcnty", "year"))
out_dir_hs <- paste0(cov_dir, "prepped/edu_hs/")
out_dir_ba <- paste0(cov_dir, "prepped/edu_ba/")
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))  # name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
saveRDS(covar[, -"edu_ba"], file = paste0(out_dir_hs, date_time_stamp, ".rds"))
saveRDS(covar[, -"edu_hs"], file = paste0(out_dir_ba, date_time_stamp, ".rds"))