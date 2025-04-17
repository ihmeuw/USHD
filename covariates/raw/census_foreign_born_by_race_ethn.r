###################################################################################################
## Description: Extract Census 2000 data on nativity status by race/ethnicity. The groups include
##              non-Hispanic white alone, black alone, AIAN alone, Asian alone, NHOPI alone, and
##              Hispanic.
##
## Get your Census API key here: https://api.census.gov/data/key_signup.html
## Add key to .Renviron: census_api_key(key, overwrite = TRUE, install = TRUE)
## Reload .Renviron: readRenviron("~/.Renviron")
##
## Use load_variables(year, dataset = "acs5", cache = TRUE) to find variable ids
###################################################################################################

# Load settings file and directories --------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")
library(tidycensus)

in_dir <- "FILEPATH"
raw_file_dir <- "FILEPATH"
out_dir <- "FILEPATH"
nids <- data.table(year = 2000, nid = 13303)

# Extract using Census API ------------------------------------------------------------------------
# first define variable names for foreign born and population totals for each race/ethn group
vars_fb <- c(white_nh = 'PCT063I013', black = 'PCT063B013', aian = 'PCT063C013', asian = 'PCT063D013',
             nhopi = 'PCT063E013', hisp = 'PCT063H013')
vars_pop <- c(white_nh = 'PCT063I001', black = 'PCT063B001', aian = 'PCT063C001', asian = 'PCT063D001',
              nhopi = 'PCT063E001', hisp = 'PCT063H001')

# get datasets with desired variables
dt_fb <- setDT(get_decennial(geography = "county",
                             variables = vars_fb,
                             year = 2000,
                             sumfile = "sf3",
                             key = Sys.getenv('CENSUS_API_KEY')))
setnames(dt_fb, old = c("variable", "value"), new = c("race_group", "foreign_born"))
dt_fb <- dt_fb[, list(foreign_born = sum(foreign_born)), by = "GEOID,NAME,race_group"]

dt_pop <- setDT(get_decennial(geography = "county",
                              variables = vars_pop,
                              year = 2000,
                              sumfile = "sf3",
                              key = Sys.getenv('CENSUS_API_KEY')))
setnames(dt_pop, old = c("variable", "value"), new = c("race_group", "pop"))
dt_pop <- dt_pop[, list(pop = sum(pop)), by = "GEOID,NAME,race_group"]

# get file path
file_path <- "FILEPATH"

# Combine and format data -------------------------------------------------------------------------
dt <- Reduce(function(...) merge(..., all=T), list(dt_fb, dt_pop))
dt[, c("fips", "GEOID", "NAME") := list(as.numeric(GEOID), NULL, NULL)]
dt <- dt[fips < 60000]  # filter out territories
dt[, year := 2000]
setkeyv(dt, c("fips", "year"))
dt <- merge(dt, nids, by = "year")  # add nid
setcolorder(dt, c("year", "fips", "race_group", "foreign_born", "pop", "nid"))
stopifnot(nrow(dt[pop < foreign_born]) == 0)  # should be no cases where total pop is less than people in foreign_born
stopifnot(nrow(dt[pop < 0 | foreign_born < 0]) == 0)  # should be no negatives
stopifnot(nrow(dt[is.na(pop) | is.na(foreign_born)]) == 0)  # should be no missing values

# Save output -------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- list(list(file_path))  # get list of lists of file paths (1 list per data year)
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths

# save data
saveRDS(dt, paste0(out_dir, date_time_stamp, ".rds"))