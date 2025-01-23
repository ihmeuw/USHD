####################################################################################################
## Description: Prep population below the poverty line and total population by educational attainment
##              in 1990 and 2000 Census records
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")

temp_dir <- "FILEPATH"
in_dir <- "FILEPATH"
out_dir <- "FILEPATH"
nids <- data.table(year = c(1990, 2000), nid = c(195803, 214962))

# Prep and combine data ----------------------------------------------------------------------------
years <- c(1990, 2000)
extract_data <- function(yr) {
  data_file <- list.files(paste0(in_dir, yr), pattern = "POVERTY_RATE_BY_EDUCATION", full.names = T)
  data_file <- grep("_CB", data_file, invert = T, value = T)  # drop metadata file
  data <- fread(data_file)
  data[, fips := as.numeric(paste0(STATEA, str_pad(as.character(COUNTYA), 3, pad = "0")))]
  poverty_pattern <- case_when(yr == 1990 ~ paste(paste0("EK6", str_pad(61:120, 3, pad = "0")), collapse = "|"),
                               yr == 2000 ~ paste(paste0("H350", str_pad(7:18, 2, pad = "0")), collapse = "|"))
  data <- data[, c("YEAR", "fips", grep(substr(poverty_pattern, 1, 3), names(data), value = T)), with = F]  # filter to relevant variables
  data <- melt.data.table(data, id.vars = c("YEAR", "fips"), measure.vars = patterns(substr(poverty_pattern, 1, 3)),
                          variable.name = "var", value.name = "pop")
  
  # assign education levels
  if (yr == 1990) {
    data[, "edu" := case_when(grepl(paste(6016:6022, collapse = "|"), var) ~ "< HS", grepl(paste(6046:6052, collapse = "|"), var) ~ "< HS",
                              grepl(paste(6076:6082, collapse = "|"), var) ~ "< HS", grepl(paste(6106:6112, collapse = "|"), var) ~ "< HS",
                              grepl("6083|6113", var) ~ "HS or equivalent", grepl("6023|6053", var) ~ "HS or equivalent",
                              grepl(paste(6024:6026, collapse = "|"), var) ~ "Some college", grepl(paste(6084:6086, collapse = "|"), var) ~ "Some college",
                              grepl(paste(6054:6056, collapse = "|"), var) ~ "Some college", grepl(paste(6114:6116, collapse = "|"), var) ~ "Some college",
                              grepl(paste(6027:6030, collapse = "|"), var) ~ "BA or higher", grepl(paste(6087:6090, collapse = "|"), var) ~ "BA or higher",
                              grepl(paste(6057:6060, collapse = "|"), var) ~ "BA or higher", grepl(paste(6117:6120, collapse = "|"), var) ~ "BA or higher",
                              TRUE ~ "under_age_25")]
  } else {
    data[, "edu" := case_when(grepl("007|008|013|014|025|026|031|032|043|044|049|050|061|062|067|068|079|080|085|086", var) ~ "< HS",
                              grepl("009|015|027|033|045|051|063|069|081|087", var) ~ "HS or equivalent",
                              grepl("010|011|016|017|028|029|034|035|046|047|052|053|064|065|070|071|082|083|088|089", var) ~ "Some college",
                              grepl("012|018|030|036|048|054|066|072|084|090", var) ~ "BA or higher", TRUE ~ "under_age_25")]
  }
  
  
  # remove data for ages 18-24, since we only consider educational attainment for ages 25+
  data <- data[edu != "under_age_25"]
  
  # split below and at/above poverty
  data[, var := ifelse(grepl(poverty_pattern, var), "below_poverty", "at_or_above_poverty")]
  setnames(data, "YEAR", "year")
  data <- data[, list(pop = sum(pop, na.rm = T)), keyby = 'year,fips,var,edu']
  data <- data[, list(var, pop, total_pop = sum(pop)), keyby = 'year,fips,edu']
  
  # only keep below poverty observations and set column order
  data <- data[var == "below_poverty"]; data[, "var" := NULL]
  setnames(data, old = c("pop", "total_pop"), new = c("poverty", "pop"))
  setcolorder(data, c("year", "fips", "edu", "poverty", "pop"))
  
  return(list("data" = data, "file_path" = data_file))
}
extractions <- lapply(years, extract_data)
data <- rbindlist(lapply(1:length(extractions), function(i) {extractions[[i]]$data}))  # rbind all data objects from extractions

# Format data --------------------------------------------------------------------------------------
data <- data[fips < 60000]
data[, edu := ordered(factor(edu, levels = c("< HS", "HS or equivalent", "Some college", "BA or higher")))]
setkeyv(data, c("year", "fips"))
setorderv(data, c("year", "fips", "edu"))
data <- merge(data, nids, by = "year")  # add nids

# Save output --------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- lapply(1:length(extractions), function(i) {list(extractions[[i]]$file_path)})  # get list of lists of file paths (1 list per data year)
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths

# save data
saveRDS(data, paste0(out_dir, date_time_stamp, ".rds"))