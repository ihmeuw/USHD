# *********************************************************************************
# Purpose:  Create clean versions of the following variables:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *Name*              *Definition*
# income_median       Median Household Income (universe: households)
#
# Raw Sources:
# Census (used to also have ACS, but that has been replaced with estimates from SAIPE)
#
# Processing Steps:
# -Extract median income
# -Designate year that the NHGIS adjusted the inflation t
# *********************************************************************************

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")
source("FILEPATH")

cov_dir <- "FILEPATH"
in_dir <- "FILEPATH"
out_dir <- "FILEPATH"

# specify NIDs and file paths for raw data
nids <- data.table(year = c(1980, 1990, 2000), nid = c(240910, 195803, 214962))
file_paths <- c("FILEPATH",
                "FILEPATH",
                "FILEPATH")

# Loop through years and load data -----------------------------------------------------------------
covar <- rbindlist(lapply(c(1980, 1990, 2000), function(yr) {
  income_var <- ifelse(yr == 1980, "DIE001", ifelse(yr == 1990, "E4U001", "GMY001"))
  data <- fread(file_paths[grep(yr, file_paths)])  # read file path according to year
  data[, fips := (1000*STATEA) + COUNTYA]  # assign full FIPS code
  data[, c("year", "infl_year") := yr]  # assign data year and inflation year (for the census, same as data year)
  setnames(data, income_var, "income_median")
  data <- data[, list(fips, year, income_median, infl_year)]
}), use.names = T)

# Merge in NIDs and format median income ----------------------------------------------------------
covar <- merge(covar, nids, by = "year")
covar[, income_median := as.numeric(income_median)]

# Save outputs ------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- list(list(file_paths[1]), list(file_paths[2]), list(file_paths[3]))
names(nid_path_list) = nids[, nid]

# save data
saveRDS(covar, file = paste0(out_dir, date_time_stamp, ".rds"))