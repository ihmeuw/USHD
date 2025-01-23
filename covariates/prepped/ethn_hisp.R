####################################################################################################
## Description: Prepare the "ethn_hisp" covariate (proportion of the population that identifies as
##              Hispanic) using the NCHS bridged race population estimate series from 2000-2020 and
##              Census population estimates in 2021+.
####################################################################################################

user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")
source("FILEPATH")

# Load source metadata and extractions -------------------------------------------------------------
nchs_meta <- get_covariate_metadata("nchs_ethn")
census_meta <- get_population_metadata("census_ethn")
covar <- rbindlist(list(readRDS(paste0(cov_dir, "FILEPATH", unique(nchs_meta[, file])))[, -"nid"],
                        readRDS(paste0(cov_dir, "FILEPATH", unique(census_meta[, file])))[year >= 2021, -"nid"]),
                   use.names = T, fill = T)

# Collapse to merged counties ----------------------------------------------------------------------
covar <- merge(covar, loc, by = "fips", all.x = T)
covar <- covar[, list(hisp = sum(hisp) / sum(pop)), by = "mcnty,year"]
stopifnot(sum(is.na(covar)) == 0)

# Interpolate --------------------------------------------------------------------------------------
covar <- standard_interp(covar, interp_years, id_vars = c("mcnty", "year"), covar_name = "hisp")
stopifnot(sum(is.na(covar)) == 0)

# Format and save output ---------------------------------------------------------------------------
setnames(covar, "hisp", "ethn_hisp")

# run checks
check_missingness(covar)
check_var(covar, "year")

# save RDS file
setkeyv(covar, c("mcnty", "year"))
out_dir <- paste0(cov_dir, "FILEPATH")
date_time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")  # name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
saveRDS(covar, file = paste0(out_dir, date_time_stamp, ".rds"))
