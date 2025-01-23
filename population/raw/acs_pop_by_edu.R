###################################################################################################
## Description: Format population counts by education status, age, and sex from the ACS.
###################################################################################################

# Load settings file and directories --------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("population$", getwd())) setwd("FILEPATH")
library(tidycensus)

out_dir <- "FILEPATH"
temp_dir <- "FILEPATH"
nids <- data.table(year = 2007:2021, nid = c(236693:236698, 280080, 399019, 399014, 448152,
                                             476546, 503429, 520867, 548126, 569747))

# Load and prep ACS data --------------------------------------------------------------------------
# In census data for 2022+, CT has new planning regions instead of counties. However, sub-county
# geography did not change, so we can map census tracts to their old county FIPS and aggregate
# 2022+ estimates to those old FIPS, at least in the short term.
agg_ct_tracts <- function(yr) {
  message("Aggregating CT census tracts in ", yr)
  in_dir <- "FILEPATH"
  
  # load metadata to find target variables
  fp_meta <- list.files(paste0(in_dir, "ACS_5YR_EST_", yr), pattern = "B15001_COLUMN_METADATA", full.names = T)
  meta <- fread(fp_meta)
  setnames(meta, c("var", "label"))
  meta[, label := gsub(":!!", "!!", label)]
  meta <- meta[grepl("Estimate", label) & grepl("years", label) & !grepl("Annotation", label),]
  meta <- data.table(meta$var, do.call("rbind", strsplit(meta$label, split = "!!")))
  meta <- meta[, -c(2:3)]
  setnames(meta, c("variable", "sex", "age", "status"))
  meta[, age := gsub(":", "", age)]  # remove any extra ":" left in age column
  
  # load tract-level estimates
  fp <- list.files(paste0(in_dir, "ACS_5YR_EST_", yr), pattern = "B15001_DATA_CT_TRACT", full.names = T)
  fp <- grep("_CB|readme|METADATA", fp, invert = T, value = T)
  
  data <- fread(fp, header = T)
  if (names(which.max(table(sapply(data, class)))) == "character") data <- data[-1, ]  # remove column labels if present
  data[, c("year", "tract_fips") := list(yr-2, as.numeric(substr(GEO_ID, 15, 20)))]  # assign data year, tract fips
  
  # map CT census tracts to old county FIPS by loading a CT tract-level dataset and isolating tract
  # and county fips from GEOID
  ct_2020_tract <- as.data.table(get_acs(geography = "tract", variables = c("B15001_001"),
                                         year = 2020, state = "CT", survey = "acs5"))
  ct_2020_tract[, c("tract_fips", "fips") := list(as.numeric(substr(GEOID, 6, 11)), as.numeric(substr(GEOID, 1, 5)))]
  ct_2020_tract[tract_fips == 990000, fips := 9001]  # manually assign FIPS to single county for water tracts (estimates are 0)
  ct_2020_tract[tract_fips == 990100, fips := 9007]
  tract_fips_map <- unique(ct_2020_tract[, list(tract_fips, fips)]); rm(ct_2020_tract)
  data[tract_fips_map, on = "tract_fips", fips := i.fips]  # add county fips to data
  
  # aggregate estimates to county fips
  data <- data[, lapply(.SD, as.numeric), .SDcols = setdiff(colnames(data), c("year", "fips", "tract_fips")),
               by = .(year, fips, tract_fips)]  # convert columns to numeric first
  data <- data[, lapply(.SD, sum), .SDcols = setdiff(colnames(data), c("year", "fips")), by = .(year, fips)]  # then aggregate to FIPS
  data[, c("tract_fips", "GEO_ID", "NAME", "V169") := NULL]  # drop extra/no longer needed columns
  
  # merge data with metadata
  data <- melt(data, id.vars = c("year", "fips"))  # convert data to long first
  data <- merge(data, meta, all.y = T, by = "variable")
  data <- data[, list(year, fips, value, sex, status, age)]
  
  # rename and format easy variables
  data[, sex := ifelse(grepl("Male", sex), 1L, 2L)]
  data[, age_start := as.integer(substr(age, 1, 2))]
  data[, age_end := as.integer(substr(age, 7, 8))]
  stopifnot(nrow(data[is.na(age_start)]) == 0)  # age start should never be missing
  stopifnot(nrow(data[is.na(age_end) & age_start != 65]) == 0)  # age end should only be missing at the terminal age group
  
  # group and encode education status, collapse to less granular groups
  data[status %in% c("Less than 9th grade", "9th to 12th grade, no diploma"), edu := "less than HS"]
  data[status %in% c("High school graduate, GED, or alternative", "High school graduate (includes equivalency)"), edu := "HS grad"]
  data[status %in% c("Associate's degree", "Some college, no degree"), edu := "some college"]
  data[status %in% c("Bachelor's degree", "Graduate or professional degree"), edu := "college grad"]
  data[, edu := factor(edu, levels = c("less than HS", "HS grad", "some college", "college grad"))]
  stopifnot(nrow(data[is.na(edu) & status != "Estimate"]) == 0)  # the only missing edu values should be in total population rows
  data <- data[!is.na(edu)]  # drop population totals
  data <- data[, list(pop = sum(value)), by = 'fips,year,sex,age_start,age_end,edu']
  
  return(list("ct_data" = data, "ct_file_path" = fp))
}

extract_data <- function(year) {
  cat(paste0(year, "\n")); flush.console()
  
  # set in_dir based on year
  in_dir <- "FILEPATH"
  
  # load meta data and find variables of interest
  files <- list.files(paste0(in_dir, ifelse(year == 2015, "FILEPATH", "FILEPATH")),
                      pattern = "B15001", full.names = T)
  files <- grep(".CSV$|.csv$", files, value = T)
  
  if (year < 2016) {  # get data file path (varies by year) for database upload
    file_path <- grep("FILEPATH", files, invert = T, value = T)
  } else {
    file_path <- grep("FILEPATH", files, value = T)
    file_path <- grep("FILEPATH", file_path, invert = T, value = T)
  }
  
  meta <- fread(grep("FILEPATH", files, value = T))[, 1:2]
  setnames(meta, c("var", "label"))
  if (year < 2018) meta[, label := gsub("(; )|(: - )|:", "!!", label)]
  if (year >= 2020) meta[, label := gsub(":!!", "!!", label)]
  meta <- meta[grepl("Estimate", label) & grepl("years", label) & !grepl("Annotation", label),]
  meta <- data.table(meta$var, do.call("rbind", strsplit(meta$label, split = "!!")))
  if (year < 2018) meta <- meta[, -c(2)] else meta <- meta[, -c(2:3)]
  setnames(meta, c("variable", "sex", "age", "status"))
  meta[, age := gsub(":", "", age)]  # couple extra ":" left in age col in 2020+
  
  # merge data with meta data
  data <- fread(file_path, header = T)
  if (names(which.max(table(sapply(data, class)))) == "character") data <- data[-1, ]  # remove column labels
  
  if (year < 2018) {
    data <- melt(data, id.vars = c("GEO.id", "GEO.id2", "GEO.display-label"))
  } else {
    data <- melt(data, id.vars = c("GEO_ID", "NAME"))
    setnames(data, "GEO_ID", "GEO.id2")
  }
  
  data <- merge(data, meta, all.y = T, by = "variable")
  data <- data[, list(GEO.id2, value, sex, status, age)]
  
  # rename and format easy variables
  setnames(data, "GEO.id2", "fips")
  data[, fips := as.integer(gsub("0500000US", "", as.character(fips)))]
  data[, value := as.numeric(as.character(value))]
  data[, sex := ifelse(grepl("Male", sex), 1L, 2L)]
  data[, age_start := as.integer(substr(age, 1, 2))]
  data[, age_end := as.integer(substr(age, 7, 8))]
  data$year <- as.integer(year - 2) # center the five-year file
  
  # group and encode education status, collapse to less granular groups
  data[status %in% c("Less than 9th grade", "9th to 12th grade, no diploma"), edu := "less than HS"]
  data[status %in% c("High school graduate, GED, or alternative", "High school graduate (includes equivalency)"), edu := "HS grad"]
  data[status %in% c("Associate's degree", "Some college, no degree"), edu := "some college"]
  data[status %in% c("Bachelor's degree", "Graduate or professional degree"), edu := "college grad"]
  data[, edu := factor(edu, levels = c("less than HS", "HS grad", "some college", "college grad"))]
  data <- data[!is.na(edu)]
  data <- data[, list(pop = sum(value)), by = 'fips,year,sex,age_start,age_end,edu']
  
  # drop territories
  data <- data[fips < 60000]
  return(list("data" = data, "file_path" = file_path))
}
extractions <- lapply(2009:2023, extract_data)  # get data extractions + file paths
data <- rbindlist(lapply(1:length(extractions), function(i) {extractions[[i]]$data}), use.names = T)  # rbind all data objects from extractions
data[nids, on = "year", nid := i.nid]  # add NID column to data
ct_extractions <- lapply(2022:2023, agg_ct_tracts)
ct_data <- rbindlist(lapply(1:length(ct_extractions), function(i) {ct_extractions[[i]]$ct_data}))
ct_data[nids, on = "year", nid := i.nid]

# Temporary fix for Prairie County, MT ------------------------------------------------------------
# problem: suspiciously low population totals for females aged 25-34, males aged
# 45-64 in 2018
# solution: replicate 2017 values for 2018 for entire county
data <- data[!(fips == 30079 & year == 2018)]
prairie_mt_fix <- copy(data[fips == 30079 & year == 2017])
prairie_mt_fix[, year := 2018]
prairie_mt_fix[, nid := 503429]
data <- rbindlist(list(data, prairie_mt_fix), use.names = T, fill = T)
data <- data[order(fips, year, nid, sex, age_start, age_end, edu, pop)]

# Replace CT planning region data with county data ------------------------------------------------
# 9 planning regions -> 8 counties
# difference in total rows should be (# of data years after 2019) * edu groups (4) * age groups (5) * sexes (2)
nrow_start <- nrow(data)
data <- data[!(year >= 2020 & fips %in% 9000:9999)]  # drop planning region data
data <- rbindlist(list(data, ct_data), use.names = T, fill = T)
stopifnot(nrow_start - nrow(data) ==
            (max(data$year)-2019)*length(unique(data$edu))*length(unique(data$age_start))*length(unique(data$sex)))

# Save output -------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- lapply(1:length(extractions), function(i) {as.list(extractions[[i]]$file_path)})  # get list of lists of file paths (1 list per data year)
ct_nid_path_list <- lapply(1:length(ct_extractions), function(i) {as.list(ct_extractions[[i]]$ct_file_path)})
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths
names(ct_nid_path_list) = nids[year >= 2020, nid]
nid_path_list <- list(nid_path_list, ct_nid_path_list)
keys <- unique(unlist(lapply(nid_path_list, names)))  # make 1 entry per NID (a simple list concatenation will create a separate entry for CT file paths)
nid_path_list <- setNames(do.call(mapply, c(FUN=c, lapply(nid_path_list, `[`, keys))), keys)

# save output
saveRDS(data, paste0(out_dir, date_time_stamp, ".rds"))