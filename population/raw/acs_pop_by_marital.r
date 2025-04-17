###################################################################################################
## Description: Format population counts by marital status, age, and sex from the ACS.
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
  fp_meta <- list.files("FILEPATH", pattern = "FILEPATH", full.names = T)
  
  meta <- fread(fp_meta)
  setnames(meta,  c("var", "label"))
  meta <- meta[grepl("years", label) & grepl("Estimate", label),]
  meta[, label := gsub("Estimate!!Total!!|Estimate!!Total:!!", "Estimate; ", label)]
  meta[, label := gsub(paste0("Now married", ":!!"), "Now married, ", label)]
  meta[, label := gsub(paste0("spouse absent", ":!!"), "spouse absent, ", label)]
  meta <- data.table(meta$var, do.call("rbind", strsplit(meta$label, split = ":!!")))
  setnames(meta, c("variable", "sex", "status", "age"))
  
  # load tract-level estimates
  fp <- list.files("FILEPATH", pattern = "FILEPATH", full.names = T)
  fp <- grep("FILEPATH", fp, invert = T, value = T)
  
  data <- fread(fp, header = T)
  if (names(which.max(table(sapply(data, class)))) == "character") data <- data[-1, ]  # remove column labels if present
  data[, c("year", "tract_fips") := list(yr-2, as.numeric(substr(GEO_ID, 15, 20)))]  # assign data year, tract fips
  
  # map CT census tracts to old county FIPS by loading a CT tract-level dataset and isolating tract
  # and county fips from GEOID
  ct_2020_tract <- as.data.table(get_acs(geography = "tract", variables = c("B12002_001"),
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
  data[, c("tract_fips", "GEO_ID", "NAME", "V377") := NULL]  # drop extra/no longer needed columns
  
  # merge data with metadata
  data <- melt(data, id.vars = c("year", "fips"))  # convert data to long first
  data <- merge(data, meta, all.y = T, by = "variable")
  data <- data[, list(year, fips, value, sex, status, age)]
  
  # rename and format easy variables
  data[, sex := ifelse(sex == "Estimate; Male", 1L, 2L)]
  data[, age := gsub("and", "to", age)]
  data[, age_start := as.integer(substr(age, 1, 2))]
  data[, age_end := as.integer(substr(age, 7, 8))]
  
  # group and encode marital status, collapse to less granular groups
  data[status %in% c("Divorced", "Widowed", "Now married, Married, spouse absent, Separated"), marital := "former"]
  data[status %in% c("Now married, Married, spouse absent, Other", "Now married, Married, spouse present"), marital := "current"]
  data[status %in% c("Never married"), marital := "never"]
  data[, marital := factor(marital, levels = c("former", "current", "never"))]
  data <- data[, list(pop = sum(value)), by = 'fips,year,sex,age_start,age_end,marital']
  
  # drop territories
  data <- data[fips < 60000]
  
  return(list("ct_data" = data, "ct_file_path" = fp))
}

extract_data <- function(year) {
  cat(paste0(year, "\n")); flush.console()

  # set in_dir based on year
  if (year >= 2019) {
    in_dir <- "FILEPATH"
  }
  if (year <= 2018){
    in_dir <- "FILEPATH"
  }
  
  # load metadata and find variables of interest
  metadata_pattern <- ifelse(year >= 2018, "FILEPATH", "FILEPATH")
  split_string <- ifelse(year >= 2018, ifelse(year == 2018, "!!", ":!!"), ": - ")  # slightly different format in 2018+ data
  if(year >= 2018) {id_variables <- c("GEO_ID", "NAME")} else {id_variables <- c("GEO.id", "GEO.id2", "GEO.display-label")}
  
  files <- list.files(paste0(in_dir, ifelse(year == 2015, "ACS_5YR_EST_2011_2015", paste0("ACS_5YR_EST_", year))),
                      pattern = "B12002", full.names = T)
  files <- grep("FILEPATH", files, value = T)
  meta <- fread(grep(metadata_pattern, files, value = T))[, 1:2, with = F]  # blank third column shows up in 2018 so specify first two
  setnames(meta,  c("var", "label"))
  meta <- meta[grepl("years", label) & grepl("Estimate", label),]
  meta[, label := gsub("Estimate!!Total!!|Estimate!!Total:!!", "Estimate; ", label)]  # had to add this line for 2018+ data (formatted slightly differently)
  meta[, label := gsub(paste0("Now married", split_string), "Now married, ", label)]
  meta[, label := gsub(paste0("spouse absent", split_string), "spouse absent, ", label)]
  meta <- data.table(meta$var, do.call("rbind", strsplit(meta$label, split = split_string)))
  setnames(meta, c("variable", "sex", "status", "age"))

  # merge data with meta data
  file_path <- grep("FILEPATH", files, invert = T, value = T)
  data <- fread(file_path, header = T)

  if (names(which.max(table(sapply(data, class)))) == "character") data <- data[-1,]  # remove variable labels
  if (year >= 2022) {
    data[, V377 := NULL]  # remove extra empty column in newer data years
  }
  data <- melt(data, id.vars = id_variables)
  data <- merge(data, meta, all.y = T, by = "variable")
  if (year >= 2018) data[, GEO.id2 := substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID))]  # last 5 digits of GEO_ID = county fips
  data <- data[, list(GEO.id2, value, sex, status, age)]

  # rename and format easy variables
  setnames(data, "GEO.id2", "fips")
  data[, fips := as.integer(as.character(fips))]
  data[, value := as.numeric(as.character(value))]
  data <- data[!sex %like% 'Annotation']
  data[, sex := ifelse(sex == "Estimate; Male", 1L, 2L)]
  data[, age := gsub("and", "to", age)]
  data[, age_start := as.integer(substr(age, 1, 2))]
  data[, age_end := as.integer(substr(age, 7, 8))]
  data$year <- as.integer(year - 2) # center the five-year file

  # group and encode marital status, collapse to less granular groups
  data[status %in% c("Divorced", "Widowed", "Now married, Married, spouse absent, Separated"), marital := "former"]
  data[status %in% c("Now married, Married, spouse absent, Other", "Now married, Married, spouse present"), marital := "current"]
  data[status %in% c("Never married"), marital := "never"]
  data[, marital := factor(marital, levels = c("former", "current", "never"))]
  data <- data[, list(pop = sum(value)), by = 'fips,year,sex,age_start,age_end,marital']

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

# Replace CT planning region data with county data ------------------------------------------------
# 9 planning regions -> 8 counties
# difference in total rows should be (# of data years after 2019) * marital statuses (3) * age groups (14) * sexes (2)
nrow_start <- nrow(data)
data <- data[!(year >= 2020 & fips %in% 9000:9999)]  # drop planning region data
data <- rbindlist(list(data, ct_data), use.names = T, fill = T)
stopifnot(nrow_start - nrow(data) ==
            (max(data$year)-2019)*length(unique(data$marital))*length(unique(data$age_start))*length(unique(data$sex)))

# Format and save ---------------------------------------------------------------------------------
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
saveRDS(data, file = paste0(out_dir, date_time_stamp, ".rds"))