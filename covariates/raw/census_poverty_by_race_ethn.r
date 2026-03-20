###################################################################################################
## Description: Extract Census 2000 data on poverty status by race/ethnicity. The groups include
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
# first define variable names for poverty and population totals for each race/ethn group
vars_pov <- c(white_nh = 'P159I002', black = 'P159B002', aian = 'P159C002', asian = 'P159D002',
              nhopi = 'P159E002', hisp = 'P159H002')
vars_pop <- c(white_nh = 'P159I001', black = 'P159B001', aian = 'P159C001', asian = 'P159D001',
              nhopi = 'P159E001', hisp = 'P159H001')

# get datasets with desired variables
dt_pov <- setDT(get_decennial(geography = "county",
                              variables = vars_pov,
                              year = 2000,
                              sumfile = "sf3",
                              key = Sys.getenv('CENSUS_API_KEY')))
setnames(dt_pov, old = c("variable", "value"), new = c("race_group", "poverty"))
dt_pov <- dt_pov[, list(poverty = sum(poverty)), by = "GEOID,NAME,race_group"]

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
dt <- Reduce(function(...) merge(..., all=T), list(dt_pov, dt_pop))
dt[, c("fips", "GEOID", "NAME") := list(as.numeric(GEOID), NULL, NULL)]
dt <- dt[fips < 60000]  # filter out territories
dt[, year := 2000]
setkeyv(dt, c("fips", "year"))
dt <- merge(dt, nids, by = "year")  # add nid
setcolorder(dt, c("year", "fips", "race_group", "poverty", "pop", "nid"))
stopifnot(nrow(dt[pop < poverty]) == 0)  # should be no cases where total pop is less than people in poverty
stopifnot(nrow(dt[pop < 0 | poverty < 0]) == 0)  # should be no negatives
stopifnot(nrow(dt[is.na(pop) | is.na(poverty)]) == 0)  # should be no missing values

# Save output -------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- list(list(file_path))  # get list of lists of file paths (1 list per data year)
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths

# save data
saveRDS(dt, paste0(out_dir, date_time_stamp, ".rds"))