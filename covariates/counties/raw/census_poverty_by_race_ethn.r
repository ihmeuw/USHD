####################################################################################################
## Description: Prep percent of persons who are below the poverty line by race and ethnicity in
##              1980, 1990, and 2000 Census records
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
source("counties/raw/_load_settings.r")

root <- ifelse(Sys.info()[1] == "Windows", "[FILEPATH]", "[FILEPATH]")
in_dir <- paste0(root, "[FILEPATH]")
out_dir <- paste0(root, "[FILEPATH]")
if (!dir.exists(out_dir)) dir.create(out_dir)  # create new folder for outputs if one doesn't exist already
nids <- data.table(year = c(1980, 1990, 2000), nid = c(240910, 195803, 214962))

# Prep and combine data ----------------------------------------------------------------------------
years <- c(1980, 1990, 2000)
extract_data <- function(yr) {
  data_file <- list.files(paste0(in_dir, yr), pattern = "[FILEPATH]", full.names = T)
  data <- fread(data_file)
  data[, fips := as.numeric(paste0(STATEA, str_pad(as.character(COUNTYA), 3, pad = "0")))]
  poverty_pattern <- case_when(yr == 1980 ~ "DWWA.001|DWWA.002", yr == 1990 ~ paste(paste0("FDFA..0", 33:64), collapse = "|"),
                           yr == 2000 ~ "H3UAA..001")
  data <- data[, c("YEAR", "fips", grep(substr(poverty_pattern, 1, 4), names(data), value = T)), with = F]
  data <- melt.data.table(data, id.vars = c("YEAR", "fips"), measure.vars = patterns(substr(poverty_pattern, 1, 4)),
                          variable.name = "var", value.name = "pop")
  data[, race_group := case_when(grepl("AF|AAX|AAHV", var) ~ "Hispanic", grepl("WAA|ABR|AAIV", var) ~ "NH White",
                                 grepl("WAB|ABS|AAIX", var) ~ "NH Black", grepl("AC|ABT|AAIZ", var) ~ "NH AIAN",
                                 grepl("AD|ABU", var) ~ "NH API", grepl("AAJB", var) ~ "NH Asian", grepl("AAJD", var) ~ "NH NHOPI",
                                 grepl("AE|ABV|AAJF", var) ~ "NH Other race", grepl("AAJH", var) ~ "NH Multiracial")]
  
  # split below and at/above poverty
  data[, var := ifelse(grepl(poverty_pattern, var), "below_poverty", "at_or_above_poverty")]
  setnames(data, "YEAR", "year")
  data <- data[, list(pop = sum(pop, na.rm = T)), keyby = 'year,fips,var,race_group']
  data <- data[, list(var, pop, total_pop = sum(pop)), keyby = 'year,fips,race_group']
  
  # only keep below poverty observations and set column order
  data <- data[var == "below_poverty"]; data[, "var" := NULL]
  setnames(data, old = c("pop", "total_pop"), new = c("poverty", "pop"))
  setcolorder(data, c("year", "fips", "race_group", "poverty", "pop"))
  
  return(list("data" = data, "file_path" = data_file))
}
extractions <- lapply(years, extract_data)
data <- rbindlist(lapply(1:length(extractions), function(i) {extractions[[i]]$data}))  # rbind all data objects from extractions

# Format data --------------------------------------------------------------------------------------
data <- data[fips < 60000]
setkeyv(data, c("year", "fips"))
data <- merge(data, nids, by = "year")  # add nids

# Save output --------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- lapply(1:length(extractions), function(i) {list(extractions[[i]]$file_path)})  # get list of lists of file paths (1 list per data year)
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths

# save data
saveRDS(data, paste0(out_dir, date_time_stamp, ".rds"))

# save extraction metadata
cov_extract <- save_covariate_extraction(path = paste0(out_dir, date_time_stamp, ".rds"), 
                                         description = "Census poverty by race/ethnicity", 
                                         prev_issues = "none",
                                         raw_data_filepaths = nid_path_list)
