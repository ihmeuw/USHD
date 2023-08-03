####################################################################################################
## Description: Prep income and poverty estimates from SAIPE, 1989, 1993, 1995, 1997-2019
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
source("counties/raw/_load_settings.r")
library(readxl)

root <- ifelse(Sys.info()[1] == "Windows", "[FILEPATH]", "[FILEPATH]")
in_dir <- paste0(root, "[FILEPATH]")
out_dir <- paste0(root, "[FILEPATH]")
temp_dir <- paste0(root, "[FILEPATH]")
years <- c(1989, 1993, 1995, 1997:2019)  # skip 1996, it's state level estimates only
nids <- data.table(year = years, nid = c(57605, 57608:57609, 57611:57620, 57623, 57626, 57629, 81297, 81299, 132623,
                                         162649, 292282:292283, 400540, 400542, 452935, 476265))

# Load data ---------------------------------------------------------------------------------------
extract_data <- function(year) {
  message("extracting ", year)
  if (year <= 2011) {
    f <- paste0(in_dir, year, "[FILEPATH]", year, "[FILEPATH]")
    data <- data.table(read.csv(f, header = T, stringsAsFactors = F))
    if (year == 1999) {
      # there's a frame shift in the variables compared to the variable names in this year which
      # we have to manually correct
      data[["Poverty.Percent.All.Ages"]] <- data[["X90..CI.Upper.Bound"]]
      data[["Median.Household.Income"]] <- data[["X90..CI.Upper.Bound.5"]]
    }
    data <- data[, list(State.FIPS, County.FIPS, Poverty.Percent.All.Ages, Median.Household.Income)]

  } else {
    if (year %in% 2012:2013) {
      f <- grep("CB|DICT|READ_ME", paste0(in_dir, year, "/", dir(paste0(in_dir, year))), invert = T, value = T)
      data <- read.fwf(f, widths = c(3, 4, 9, 9, 9, 5, 5, 5, 9, 9, 9, 5, 5, 5, 9, 9, 9, 5, 5, 5, 7,
                                     7, 7, 8, 8, 8, 5, 5, 5, 46, 3, 22))
      data <- data.table(data[, c(1, 2, 6, 21)])
    } else if (year == 2018) {
      f <- list.files(paste0(in_dir, year), pattern = paste0("[FILEPATH]", year, "[FILEPATH]"), full.names = T)
      data <- fread(f, skip = 5, header = T)
      data <- data[, .(`State FIPS Code`, `County FIPS Code`, `Poverty Percent, All Ages`, `Median Household Income`)]
    } else if (year == 2019) {
      f <- list.files(paste0(in_dir, year), pattern = paste0("[FILEPATH]", year, "[FILEPATH]"), full.names = T)
      data <- as.data.table(read_xls(f, skip = 3))
      data <- data[, .(`State FIPS Code`, `County FIPS Code`, `Poverty Percent, All Ages`, `Median Household Income`)]
    } else {
      f <- list.files(paste0(in_dir, year), pattern = paste0("[FILEPATH]", year, "[FILEPATH]"), full.names = T)
      data <- read.fwf(f, widths = c(3, 4, 9, 9, 9, 5, 5, 5, 9, 9, 9, 5, 5, 5, 9, 9, 9, 5, 5, 5, 7,
                                     7, 7, 8, 8, 8, 5, 5, 5, 46, 3, 22))
      data <- data.table(data[, c(1, 2, 6, 21)])
    }
    
    setnames(data, c("State.FIPS", "County.FIPS", "Poverty.Percent.All.Ages", "Median.Household.Income"))
  }

  data[, State.FIPS := as.integer(State.FIPS)]
  data[, County.FIPS := as.integer(County.FIPS)]
  data <- data[!is.na(State.FIPS) & !is.na(County.FIPS) & County.FIPS > 0,]
  data <- data[!(data$State.FIPS == 15 & data$County.FIPS == 5),] # SAIPE doesn't report for Kalawao, HI

  # keep appropriate variables
  data[, fips := 1000 * State.FIPS + County.FIPS]
  data$year <- year
  data <- data[, list(fips, year, Poverty.Percent.All.Ages, Median.Household.Income)]
  setnames(data, 3:4, c("poverty", "income"))
  data[, income := as.numeric(gsub(",", "", as.character(income)))]
  data[, poverty := as.numeric(as.character(poverty))]
  
  return(list("data" = data, "file_path" = f))
}
extractions <- lapply(years, extract_data)
data <- rbindlist(lapply(1:length(extractions), function(i) {extractions[[i]]$data}))  # rbind all data objects from extractions

# Format data --------------------------------------------------------------------------------------
setnames(data, "income", "income_median")
data <- merge(data, nids, by = "year")  # add nids
setkeyv(data, c("fips", "year"))

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
                                         description = "SAIPE income and poverty rate estimates", 
                                         prev_issues = "none",
                                         raw_data_filepaths = nid_path_list)
