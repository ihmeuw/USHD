####################################################################################################
## Description: Extract Census 2000 data on income per capita by race/ethnicity. The groups include
##              non-Hispanic white alone, black alone, AIAN alone, Asian alone, NHOPI alone, and
##              Hispanic.
##
## Get your Census API key here: https://api.census.gov/data/key_signup.html
## Add key to .Renviron: census_api_key(key, overwrite = TRUE, install = TRUE)
## Reload .Renviron: readRenviron("~/.Renviron")
##
## Use load_variables(year, dataset = "acs5", cache = TRUE) to find variable ids
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")
library(tidycensus)

in_dir <- "FILEPATH"
raw_file_dir <- "FILEPATH"
out_dir <- "FILEPATH"
nids <- data.table(year = 2000, nid = 13303)

# Extract using Census API ------------------------------------------------------------------------
# first define variable names for income pc for each race/ethn group
vars_inc <- c(white_nh = 'P157I001', black = 'P157B001', aian = 'P157C001',
              asian = 'P157D001', nhopi = 'P157E001', hisp = 'P157H001')

# get datasets with desired variables
dt <- setDT(get_decennial(geography = "county",
                              variables = vars_inc,
                              year = 2000,
                              sumfile = "sf3",
                              key = Sys.getenv('CENSUS_API_KEY')))
setnames(dt, old = c("variable", "value"), new = c("race_group", "income_pc"))
stopifnot(nrow(unique(dt)) == nrow(dt))  # all rows should be unique strata

# get file path
file_path <- "FILEPATH"

# Combine and format data -------------------------------------------------------------------------
dt[, c("fips", "GEOID", "NAME") := list(as.numeric(GEOID), NULL, NULL)]
dt <- dt[fips < 60000]  # filter out territories
dt[, c("year", "infl_year") := 2000]
setkeyv(dt, c("fips", "year"))
dt <- merge(dt, nids, by = "year")  # add nid
setcolorder(dt, c("year", "fips", "race_group", "income_pc", "infl_year", "nid"))
stopifnot(nrow(dt[is.na(income_pc)]) == 0)  # should be no missing values

# Save output -------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- list(list(file_path))  # get list of lists of file paths (1 list per data year)
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths

# save data
saveRDS(dt, paste0(out_dir, date_time_stamp, ".rds"))