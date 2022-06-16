####################################################################################################
## Description: Prep ACS 5-year foreign-born population estimates by race and ethnicity. There is
##              one file per race/ethnicity, including white alone, non-Hispanic white, black alone,
##              AIAN alone, Asian alone, NHOPI alone, some other race alone, two or more races, and
##              Hispanic. This script cleans the raw data but does not estimate foreign-born population
##              for the non-Hispanic portion of each race - that happens in the prepped stage.
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
source("counties/raw/_load_settings.r")

root <- ifelse(Sys.info()[1] == "Windows", "[FILEPATH]/", "[FILEPATH]")
in_dir <- paste0(root, "[FILEPATH]")
in_dir2 <- paste0(root, "[FILEPATH]")  # 2019 data are in here until further notice
temp_dir <- paste0(root, "[FILEPATH]")
out_dir <- paste0(root, "[FILEPATH]")
if (!dir.exists(out_dir)) dir.create(out_dir)  # create new folder for outputs if one doesn't exist already
nids <- data.table(year = 2008:2017, nid = c(91310:91313, 290843, 290854, 369465, 399302, 459162, 476546))

# Load and format data -----------------------------------------------------------------------------
races <- c("white", "black", "aian", "asian", "nhopi", "other", "multi", "white_nh", "hisp")  # indicated by A-I in data
foreign_pop_levels <- c("_pop", "_m1", "_m2", "_f1", "_f2")  # split by sex in raw data
raw_vars <- as.vector(vapply(races, function(race) { paste0(race, foreign_pop_levels) }, character(5)))  # order variable names
moe_names <- as.vector(vapply(grep("pop", raw_vars, invert = T, value = T), function(name) { paste0(name, "_moe")}, character(1)))

convert_to_variance <- function(x) {return((x/1.645)^2)}
convert_to_moes <- function(x) {return(1.645*sqrt(x))}

extract_data <- function(yr, include_moe = F) {
  message("Formatting ", yr, " 5-year series")
  if (yr == 2019) {
    files <- list.files(paste0(in_dir2, "ACS_5YR_EST_", yr), pattern = "B05003", full.names = T)
    files <- grep("_CB|readme", files, invert = T, value = T)
  } else {
    files <- list.files(paste0(in_dir, yr), pattern = "B05003", recursive = T, full.names = T)
    files <- grep("METADATA|readme", files, invert = T, value = T)
  }
  data <- lapply(files, function(file){fread(file)})
  data <-  Reduce(merge, data)  # merge all files into one
  data <- data[GEO_ID != "id"]
  data[, c("year", "fips") := list(yr-2, as.numeric(substr(GEO_ID, 10, 14)))]  # assign data year, fips
  
  # if we want to keep margins of error, compile with that info. otherwise drop MOE columns
  value_vars <- grep("_001E|_005E|_010E|_016E|_021E", names(data), value = T)  # define total and foreign-born pop columns
  if (include_moe) {
    moe_vars <- grep("_005M|_010M|_016M|_021M", names(data), value = T)  # define MOE value columns
    data <- data[, lapply(.SD, as.numeric), .SDcols = c(value_vars, moe_vars), by = .(year, fips)]
    setnames(data, old = c(sort(moe_vars)), new = moe_names)
    data[, eval(moe_names) := lapply(.SD, convert_to_variance), .SDcols = moe_names, by = .(year, fips)]  # convert MOEs to variances for combining
    data[, eval(paste0(races, "_moe")) := list(sum(white_m1_moe, white_m2_moe, white_f1_moe, white_f2_moe),
                                               sum(black_m1_moe, black_m2_moe, black_f1_moe, black_f2_moe),
                                               sum(aian_m1_moe, aian_m2_moe, aian_f1_moe, aian_f2_moe),
                                               sum(asian_m1_moe, asian_m2_moe, asian_f1_moe, asian_f2_moe),
                                               sum(nhopi_m1_moe, nhopi_m2_moe, nhopi_f1_moe, nhopi_f2_moe),
                                               sum(other_m1_moe, other_m2_moe, other_f1_moe, other_f2_moe),
                                               sum(multi_m1_moe, multi_m2_moe, multi_f1_moe, multi_f2_moe),
                                               sum(white_nh_m1_moe, white_nh_m2_moe, white_nh_f1_moe, white_nh_f2_moe),
                                               sum(hisp_m1_moe, hisp_m2_moe, hisp_f1_moe, hisp_f2_moe)), by = .(year, fips)]
    data[, eval(paste0(races, "_moe")) := lapply(.SD, convert_to_moes), .SDcols = paste0(races, "_moe"), by = .(year, fips)]  # convert back to MOEs
    } else {
    data <- data[, lapply(.SD, as.numeric), .SDcols = value_vars, by = .(year, fips)]  # convert value columns to numeric
  }
  
  setnames(data, old = sort(value_vars), new = raw_vars)
  
  # collapse by sex and age
  data[, eval(paste0(races, "_foreign")) := list(sum(white_m1, white_m2, white_f1, white_f2), sum(black_m1, black_m2, black_f1, black_f2),
                                                 sum(aian_m1, aian_m2, aian_f1, aian_f2), sum(asian_m1, asian_m2, asian_f1, asian_f2),
                                                 sum(nhopi_m1, nhopi_m2, nhopi_f1, nhopi_f2), sum(other_m1, other_m2, other_f1, other_f2),
                                                 sum(multi_m1, multi_m2, multi_f1, multi_f2), sum(white_nh_m1, white_nh_m2, white_nh_f1, white_nh_f2),
                                                 sum(hisp_m1, hisp_m2, hisp_f1, hisp_f2)), by = .(year, fips)]
  data <- data[, .SD, by = .(year, fips), .SDcols = c(paste0(races, "_foreign"), if (include_moe) paste0(races, "_moe"),
                                                      paste0(races, "_pop"))]
  
  return(list("data" = data, "file_paths" = files))
}
extractions <- lapply(2010:2019, extract_data, include_moe = T)
data <- rbindlist(lapply(1:length(extractions), function(i) {extractions[[i]]$data}))  # rbind all data objects from extractions

# Combine and format data -------------------------------------------------------------------------
moe <- TRUE %in% grepl("moe", names(data))  # detect whether margins of error were kept
data <- data[fips < 60000]
setcolorder(data, c("year", "fips", paste0(races, "_foreign"), if (moe) paste0(races, "_moe"), paste0(races, "_pop")))  # make sure column order is correct
data <- melt.data.table(data, id.vars = c("year", "fips"), measure.vars = patterns("_foreign", if (moe) "_moe", "_pop"),
                        variable.name = "race_group", value.name = c("foreign_born", if (moe) "moe", "pop"))
data[, race_group := races[race_group]]  # re-assign race groups (becomes numeric values during melt)
data <- merge(data, nids, by = "year")  # add nids

# Save output --------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- lapply(1:length(extractions), function(i) {list(extractions[[i]]$file_paths)})  # get list of lists of file paths (1 list per data year)
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths

# save data
saveRDS(data, paste0(out_dir, date_time_stamp, ".rds"))

# save extraction metadata
cov_extract <- save_covariate_extraction(path = paste0(out_dir, date_time_stamp, ".rds"), 
                                         description = "ACS foreign-born population by race/ethnicity", 
                                         prev_issues = "none",
                                         raw_data_filepaths = nid_path_list)
