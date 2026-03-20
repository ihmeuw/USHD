####################################################################################################
## Description: Prep ACS 5-year poverty rate estimates by race and ethnicity. There is
##              one file per race/ethnicity, including white alone, non-Hispanic white, black alone,
##              AIAN alone, Asian alone, NHOPI alone, some other race alone, two or more races, and
##              Hispanic. This script cleans the raw data but does not estimate poverty for
##              the non-Hispanic portion of each race - that happens in the prepped stage.
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")
library(tidycensus)

in_dir <- "FILEPATH"
in_dir2 <- "FILEPATH"
temp_dir <- "FILEPATH"
out_dir <- "FILEPATH"
nids <- data.table(year = 2008:2020, nid = c(91310:91313, 290843, 290854, 369465, 399302, 459162,
                                             476546, 503429, 520867, 548126))

convert_to_variance <- function(x) {return((x/1.645)^2)}
convert_to_moes <- function(x) {return(1.645*sqrt(x))}

# Load and format data -----------------------------------------------------------------------------
races <- c("white", "black", "aian", "asian", "nhopi", "other", "multi", "white_nh", "hisp")  # indicated by A-I in data
est_names <- as.vector(vapply(races, function(race) { paste0(race, c("_pop", "_pov")) }, character(2)))  # order variable names
moe_names <- paste0(est_names, "_moe")

# In census data for 2022+, CT has new planning regions instead of counties. However, sub-county
# geography did not change, so we can map census tracts to their old county FIPS and aggregate
# 2022+ estimates to those old FIPS, at least in the short term.
agg_ct_tracts <- function(yr, include_moe = F) {
  message("Aggregating CT census tracts in ", yr)
  files <- list.files("FILEPATH", pattern = "FILEPATH", full.names = T)
  files <- grep("FILEPATH", files, invert = T, value = T)
  
  data <- lapply(files, function(file){fread(file, header = T)})
  data <-  Reduce(merge, data)  # merge all files into one
  data <- data[!GEO_ID %in% c("id", "Geography")]
  data[, c("year", "tract_fips") := list(yr-2, as.numeric(substr(GEO_ID, 15, 20)))]  # assign data year, tract fips
  
  # map CT census tracts to old county FIPS by loading a CT tract-level dataset and isolating tract
  # and county fips from GEOID
  ct_2020_tract <- as.data.table(get_acs(geography = "tract", variables = c("B17001_001"),
                                         year = 2020, state = "CT", survey = "acs5"))
  ct_2020_tract[, c("tract_fips", "fips") := list(as.numeric(substr(GEOID, 6, 11)), as.numeric(substr(GEOID, 1, 5)))]
  ct_2020_tract[tract_fips == 990000, fips := 9001]  # manually assign FIPS to single county for water tracts (estimates are 0)
  ct_2020_tract[tract_fips == 990100, fips := 9007]
  tract_fips_map <- unique(ct_2020_tract[, list(tract_fips, fips)]); rm(ct_2020_tract)
  data[tract_fips_map, on = "tract_fips", fips := i.fips]  # add county fips to data
  
  # aggregate estimates to county fips
  # if we want to keep margins of error, compile with that info. otherwise drop MOE columns
  est_vars <- grep("_001E$|_002E$", names(data), value = T)  # define total pop and pop in poverty value columns
  if (include_moe) {
    moe_vars <- grep("_001M$|_002M$", names(data), value = T)  # define moe value columns
    data <- data[, lapply(.SD, as.numeric), .SDcols = c(est_vars, moe_vars), by = .(year, fips, tract_fips)]  # convert columns to numeric
    setnames(data, old = c(sort(est_vars), sort(moe_vars)), new = c(est_names, moe_names))
    data[, eval(moe_names) := lapply(.SD, convert_to_variance), .SDcols = moe_names, by = .(year, fips, tract_fips)]  # convert MOEs to variances for combining
    data <- data[, lapply(.SD, sum), .SDcols = c(est_names, moe_names), by = .(year, fips)]  # aggregate to fips
    data[, eval(moe_names) := lapply(.SD, convert_to_moes), .SDcols = moe_names, by = .(year, fips)]  # convert variances back to MOEs
  } else {
    data <- data[, lapply(.SD, as.numeric), .SDcols = est_vars, by = .(year, fips, tract_fips)]  # convert columns to numeric
    data <- data[, lapply(.SD, sum), .SDcols = est_vars, by = .(year, fips)]
    setnames(data, old = sort(est_vars), new = est_names)
  }
  
  return(list("ct_data" = data, "ct_file_paths" = files))
}

extract_data <- function(yr, include_moe = F) {
  message("Formatting ", yr, " 5-year series")
  if (yr == 2019) {
    files <- list.files("FILEPATH", pattern = "FILEPATH", full.names = T)
    files <- grep("FILEPATH", files, invert = T, value = T)
  } else {
    if (yr >= 2020) in_dir <- "FILEPATH"
    files <- list.files(paste0(in_dir, yr), pattern = "FILEPATH", recursive = T, full.names = T)
    files <- grep("FILEPATH", files, invert = T, value = T)
  }
  data <- lapply(files, function(file){fread(file, header = T)})
  data <-  Reduce(merge, data)  # merge all files into one
  data <- data[!GEO_ID %in% c("id", "Geography")]
  data[, c("year", "fips") := list(yr-2, as.numeric(substr(GEO_ID, 10, 14)))]  # assign data year, fips
  data[data == "null"] <- NA_real_  # 2021 has "null" instead of blank cells
  
  # if we want to keep margins of error, compile with that info. otherwise drop MOE columns
  est_vars <- grep("_001E$|_002E$", names(data), value = T)  # define total pop and pop in poverty value columns
  if (include_moe) {
    moe_vars <- grep("_001M$|_002M$", names(data), value = T)  # define moe value columns
    data <- data[, lapply(.SD, as.numeric), .SDcols = c(est_vars, moe_vars), by = .(year, fips)]
    setnames(data, old = c(sort(est_vars), sort(moe_vars)), new = c(est_names, moe_names))
  } else {
    data <- data[, lapply(.SD, as.numeric), .SDcols = est_vars, by = .(year, fips)]  # convert value columns to numeric
    setnames(data, old = sort(est_vars), new = est_names)
  }
  
  return(list("data" = data, "file_paths" = files))
}
extractions <- lapply(2010:2022, extract_data, include_moe = T)
data <- rbindlist(lapply(1:length(extractions), function(i) {extractions[[i]]$data}))  # rbind all data objects from extractions
ct_extractions <- lapply(2022, agg_ct_tracts, include_moe = T)
ct_data <- rbindlist(lapply(1:length(ct_extractions), function(i) {ct_extractions[[i]]$ct_data}))

# Combine and format data -------------------------------------------------------------------------
# replace CT planning region data with county data
nrow_start <- nrow(data)
data <- data[!(year >= 2020 & fips %in% 9000:9999)]  # drop planning region data
data <- rbindlist(list(data, ct_data), use.names = T, fill = T)
stopifnot(nrow_start - nrow(data) == max(data$year)-2019)  # difference in total rows should be 1 per data year after 2019 (9 planning regions -> 8 counties)

# format and sort columns, make dataset long
moe <- TRUE %in% grepl("moe", names(data))  # detect whether margins of error were kept
data <- data[fips < 60000]
setcolorder(data, c("year", "fips", est_names, if (moe) moe_names))  # make sure column order is correct
data <- melt.data.table(data, id.vars = c("year", "fips"),
                        measure.vars = patterns("_pov$", if (moe) "pov_moe", "_pop$", if (moe) "pop_moe"),
                        variable.name = "race_group", value.name = c("poverty", if (moe) "moe", "pop", if (moe) "pop_moe"))
data[, race_group := races[race_group]]  # re-assign race groups (becomes numeric values during melt)
data <- merge(data, nids, by = "year")  # add nids

# Save output --------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- lapply(1:length(extractions), function(i) {as.list(extractions[[i]]$file_paths)})  # get list of lists of file paths (1 list per data year)
ct_nid_path_list <- lapply(1:length(ct_extractions), function(i) {as.list(ct_extractions[[i]]$ct_file_paths)})
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths
names(ct_nid_path_list) = nids[year >= 2020, nid]
nid_path_list <- list(nid_path_list, ct_nid_path_list)
keys <- unique(unlist(lapply(nid_path_list, names)))  # make 1 entry per NID (a simple list concatenation will create a separate entry for CT file paths)
nid_path_list <- setNames(do.call(mapply, c(FUN=c, lapply(nid_path_list, `[`, keys))), keys)

# save data
saveRDS(data, paste0(out_dir, date_time_stamp, ".rds"))