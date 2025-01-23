####################################################################################################
## Description: Prepare the "income_median" covariate (median household income) using census 1980
##              data and the SAIPE series, adjusting for inflation, and interpolating to fill in
##              intervening years.
####################################################################################################

user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")
interp_years <- 1980:2021  # this has newer data than most covariates

# Load source metadata and extractions -------------------------------------------------------------
census_meta <- get_covariate_metadata("census_income_median")
saipe_meta <- get_covariate_metadata("saipe_income_and_poverty")
bls_meta <- get_covariate_metadata("bls_cpi")

covar_census <- readRDS(paste0(cov_dir, "FILEPATH", unique(census_meta[, file])))[year == 1980, -"nid"]
covar_saipe <- readRDS(paste0(cov_dir, "FILEPATH", unique(saipe_meta[, file])))[, -"nid"]
covar_saipe <- covar_saipe[, list(fips, year, income_median, infl_year = year)]

covar <- rbind(covar_census, covar_saipe)
rm(covar_census, covar_saipe)

# Collapse to merged counties ----------------------------------------------------------------------
covar <- merge(covar, loc, by = "fips", all.x = T)
covar <- covar[, list(income_median = mean(income_median, na.rm = T)), by = "mcnty,year,infl_year"]

# Load CPI and adjust for inflation ----------------------------------------------------------------
cpi <- readRDS(paste0(cov_dir, "FILEPATH", unique(bls_meta[, file])))[, -"nid"]
cpi$infl <- cpi[year == max(interp_years), cpi] / cpi$cpi
cpi <- cpi[year %in% interp_years, list(year, infl)]
setnames(cpi, "year", "infl_year")

covar <- merge(covar, cpi, by = "infl_year", all.x = T)
covar[, income_median := income_median * infl]
covar[, c("infl", "infl_year") := NULL]

# Interpolate --------------------------------------------------------------------------------------
covar <- standard_interp(covar, interp_years, id_vars = c("mcnty", "year"), covar_name = "income_median")
stopifnot(sum(is.na(covar)) == 0)

# Format and save output ---------------------------------------------------------------------------
# run checks
check_missingness(covar)
check_var(covar, "year")

# save RDS file
setkeyv(covar, c("mcnty", "year"))
out_dir <- paste0(cov_dir, "FILEPATH")
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))  # name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
saveRDS(covar, file = paste0(out_dir, date_time_stamp, ".rds"))