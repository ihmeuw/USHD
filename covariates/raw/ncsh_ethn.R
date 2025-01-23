####################################################################################################
## Description: Prep Hispanic ethnicity using bridged-race population data from NCHS
##
## Output:      A data.table with the following variables: 'fips', 'year', 'hisp', 'pop'
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")

in_dir <- "FILEPATH"
out_dir <- "FILEPATH"

# Load data (from NCHS population extractions) -----------------------------------------------------
pop_metadata <- get_population_metadata("nchs_pop_est_by_race_ethn")
pop <- readRDS(paste0(in_dir, "nchs_pop_est_by_race_ethn/", unique(pop_metadata[, file])))

# re-create "hisp" variable as the population that is Hispanic
data <- pop[, list(hisp = sum(hisp * pop), pop = sum(pop)), by = "fips,year,nid"]

# set key variables
setkey(data, fips, year)

# Save output --------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))

# make list of file paths named for corresponding NID to pass as extraction metadata 
nids <- unique(pop_metadata[, nid])
nid_path_list <- lapply(nids, function(i) {as.list(pop_metadata[nid == i, raw_data_filepaths])})  # get list of lists of file paths (1 list per NID)
names(nid_path_list) = nids  # map each NID to its respective list of file paths

# save data
saveRDS(data, paste0(out_dir, date_time_stamp, ".rds"))