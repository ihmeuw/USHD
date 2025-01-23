###################################################################################################
## Description: Prep ACS 5-year poverty rate estimates by educational attainment
###################################################################################################

# Load settings file and directories --------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")

temp_dir <- "FILEPATH"
in_dir <- "FILEPATH"
out_dir <- "FILEPATH"
if (!dir.exists(out_dir)) dir.create(out_dir)  # create new folder for outputs if one doesn't exist already
nids <- data.table(year = 2007:2019, nid = c(241597, 241611:241614, 248849, 292127, 460848, 460853,
                                             460856, 472756, 501680, 527884))

# Load and format data ----------------------------------------------------------------------------
# functions for aggregating margins of error
convert_to_variance <- function(x) {return((x/1.645)^2)}
convert_to_moes <- function(x) {return(1.645*sqrt(x))}

extract_data <- function(yr, include_moe = F) {
  message("Formatting ", yr, " 5-year series")
  
  file_path <- list.files(paste0(in_dir, "ACS_5YR_EST_", yr), pattern = "B17003", full.names = T)
  
  file_path <- grep("codebook|_CB|\\.TXT", file_path, invert = T, value = T)  # drop metadata file
  data <- fread(file_path)
  data[, fips := as.numeric(paste0(STATEA, str_pad(as.character(COUNTYA), 3, pad = "0")))]
  data[, year := yr - 2]
  value_pattern <- substr(grep("E001", names(data), value = T), 1, ifelse(yr <= 2013, 4, 5))  # get defining poverty variable pattern (different each year)
  if (include_moe) moe_pattern <- substr(grep("M001", names(data), value = T), 1, ifelse(yr <= 2013, 4, 5))  # get defining poverty moe pattern
  data <- data[, c("year", "fips", grep(value_pattern, names(data), value = T), if (include_moe) grep(moe_pattern, names(data), value = T)),
               with = F]  # filter to relevant variables
  data <- melt.data.table(data, id.vars = c("year", "fips"), measure.vars = patterns(value_pattern, if (include_moe) moe_pattern),
                          variable.name = "var", value.name = c("pop", if (include_moe) "moe"))
  
  # assign education levels
  data[, "edu" := case_when(var %in% c(4, 9, 15, 20) ~ "< HS", var %in% c(5, 10, 16, 21) ~ "HS or equivalent",
                            var %in% c(6, 11, 17, 22) ~ "Some college", var %in% c(7, 12, 18, 23) ~ "BA or higher", TRUE ~ "remove")]
  
  # drop unnecessary totals (those not split by edu attainment)
  data <- data[edu != "remove"]
  
  # combine across sex and split below and at/above poverty
  data[, var := ifelse(var %in% 1:12, "below_poverty", "at_or_above_poverty")]
  data[, moe := convert_to_variance(moe)]  # convert MOEs to variances for combining
  data <- data[, list(pop = sum(pop, na.rm = T), moe = sum(moe, na.rm = T)), keyby = 'year,fips,var,edu']
  data[, moe := convert_to_moes(moe)]
  data <- data[, list(var, pop, moe, total_pop = sum(pop)), keyby = 'year,fips,edu']
  
  # only keep below poverty observations and set column order
  data <- data[var == "below_poverty"]; data[, "var" := NULL]
  setnames(data, old = c("pop", "total_pop"), new = c("poverty", "pop"))
  setcolorder(data, c("year", "fips", "edu", "poverty", "moe", "pop"))
  
  return(list("data" = data, "file_path" = file_path))
}
extractions <- lapply(2009:2021, extract_data, include_moe = T)
data <- rbindlist(lapply(1:length(extractions), function(i) {extractions[[i]]$data}))  # rbind all data objects from extractions

# Combine and format data -------------------------------------------------------------------------
data <- data[fips < 60000]
data[, edu := ordered(factor(edu, levels = c("< HS", "HS or equivalent", "Some college", "BA or higher")))]
data <- merge(data, nids, by = "year")
setkeyv(data, c("year", "fips"))
setorderv(data, c("year", "fips", "edu"))

# Save output -------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- lapply(1:length(extractions), function(i) {as.list(extractions[[i]]$file_path)})  # get list of lists of file paths (1 list per data year)
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths

# save data
saveRDS(data, paste0(out_dir, date_time_stamp, ".rds"))