####################################################################################################
## Description: Prep ACS 5-year race/ethnicity proportions in each edu group (less than HS, HS
##              graduate, Some college, and College graduate) by race and ethnicity. There is one
##              edu file per race/ethnicity, including white alone, non-Hispanic white, black alone,
##              AIAN alone, Asian alone, NHOPI alone, some other race alone, two or more races, and
##              Hispanic. This script cleans the raw data but does not estimate education for
##              the non-Hispanic portion of each race - that happens in the prepped stage.
##
## Note: though the underlying data are the same, this raw dataset is an inverse of the output from
##       acs_education_by_race_ethn and has more detailed education groups that match the ones in 
##       the NVSS mortality by education estimates.
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")

in_dir <- "FILEPATH"
in_dir2 <- "FILEPATH"
temp_dir <- "FILEPATH"
out_dir <- "FILEPATH"
if (!dir.exists(out_dir)) dir.create(out_dir)  # create new folder for outputs if one doesn't exist already
nids <- data.table(year = 2008:2019, nid = c(91310:91313, 290843, 290854, 369465, 399302, 459162,
                                             476546, 503429, 520867))

# Load and format data -----------------------------------------------------------------------------
races <- c("white", "black", "aian", "asian", "nhopi", "other", "multi", "white_nh", "hisp")  # indicated by A-I in data
edus <- c("lhs", "hs", "sc", "ba")
edu_levels <- c("pop_", "m_lhs_", "m_hs_", "m_sc_", "m_ba_", "f_lhs_", "f_hs_", "f_sc_", "f_ba_")  # split by sex in raw data
raw_vars <- as.vector(vapply(races, function(race) { paste0(edu_levels, race) }, character(9)))  # order variable names
edu_vars <- as.vector(vapply(races, function(race) { paste0(paste0(edus, "_"), race) }, character(4)))  # define prepped r/e vars by edu
pop_vars <- paste0("pop_", edus)
moe_names <- as.vector(vapply(grep("pop", raw_vars, invert = T, value = T), function(name) { paste0(name, "_moe")}, character(1)))

convert_to_variance <- function(x) {return((x/1.645)^2)}
convert_to_moes <- function(x) {return(1.645*sqrt(x))}

extract_data <- function(yr, include_moe = F){
  message("Formatting ", yr, " 5-year series")
  if (yr == 2019) {
    files <- list.files(paste0(in_dir2, "ACS_5YR_EST_", yr), pattern = "C15002", full.names = T)
    files <- grep("CB|readme", files, invert = T, value = T)
  } else {
    if (yr >= 2020) in_dir <- paste0(in_dir, "ACS_5YR_EST_")  # slightly different path for 2020+
    files <- list.files(paste0(in_dir, yr), pattern = paste0("5YR_EST_", yr, "_C15002"), recursive = T, full.names = T)
    files <- grep("METADATA|readme|intake", files, invert = T, value = T)
  }
  data <- lapply(files, function(file){fread(file, header = T)})
  data <-  Reduce(merge, data)  # merge all files into one
  data <- data[GEO_ID != "id"]
  data[, c("year", "fips") := list(yr-2, as.numeric(substr(GEO_ID, 10, 14)))]  # assign data year, fips
  
  # if we want to keep margins of error, compile with that info. otherwise drop MOE columns
  value_vars <- grep("_001E$|_003E$|_004E$|_005E$|_006E$|_008E$|_009E$|_010E$|_011E$", names(data), value = T)  # define relevant columns
  if (include_moe) {
    moe_vars <- grep("_003M$|_004M$|_005M$|_006M$|_008M$|_009M$|_010M$|_011M$", names(data), value = T)  # define moe value columns
    data <- data[, lapply(.SD, as.numeric), .SDcols = c(value_vars, moe_vars), by = .(year, fips)]
    setnames(data, old = sort(moe_vars), new = moe_names)
    data[, eval(moe_names) := lapply(.SD, convert_to_variance), .SDcols = moe_names, by = .(year, fips)]  # convert MOEs to variances for combining
    
    # collapse MOE variables by sex
    data[, eval(paste0(edu_vars, "_moe")) := list(sum(m_lhs_white_moe, f_lhs_white_moe), sum(m_hs_white_moe, f_hs_white_moe), sum(m_sc_white_moe, f_sc_white_moe),
                                                  sum(m_ba_white_moe, f_ba_white_moe), sum(m_lhs_black_moe, f_lhs_black_moe), sum(m_hs_black_moe, f_hs_black_moe),
                                                  sum(m_sc_black_moe, f_sc_black_moe), sum(m_ba_black_moe, f_ba_black_moe), sum(m_lhs_aian_moe, f_lhs_aian_moe),
                                                  sum(m_hs_aian_moe, f_hs_aian_moe), sum(m_sc_aian_moe, f_sc_aian_moe), sum(m_ba_aian_moe, f_ba_aian_moe),
                                                  sum(m_lhs_asian_moe, f_lhs_asian_moe), sum(m_hs_asian_moe, f_hs_asian_moe), sum(m_sc_asian_moe, f_sc_asian_moe),
                                                  sum(m_ba_asian_moe, f_ba_asian_moe), sum(m_lhs_nhopi_moe, f_lhs_nhopi_moe), sum(m_hs_nhopi_moe, f_hs_nhopi_moe),
                                                  sum(m_sc_nhopi_moe, f_sc_nhopi_moe), sum(m_ba_nhopi_moe, f_ba_nhopi_moe), sum(m_lhs_other_moe, f_lhs_other_moe),
                                                  sum(m_hs_other_moe, f_hs_other_moe), sum(m_sc_other_moe, f_sc_other_moe), sum(m_ba_other_moe, f_ba_other_moe),
                                                  sum(m_lhs_multi_moe, f_lhs_multi_moe), sum(m_hs_multi_moe, f_hs_multi_moe), sum(m_sc_multi_moe, f_sc_multi_moe),
                                                  sum(m_ba_multi_moe, f_ba_multi_moe), sum(m_lhs_white_nh_moe, f_lhs_white_nh_moe), sum(m_hs_white_nh_moe, f_hs_white_nh_moe),
                                                  sum(m_sc_white_nh_moe, f_sc_white_nh_moe), sum(m_ba_white_nh_moe, f_ba_white_nh_moe), sum(m_lhs_hisp_moe, f_lhs_hisp_moe),
                                                  sum(m_hs_hisp_moe, f_hs_hisp_moe), sum(m_sc_hisp_moe, f_sc_hisp_moe), sum(m_ba_hisp_moe, f_ba_hisp_moe)), by = .(year, fips)]
    
    data[, eval(paste0(edu_vars, "_moe")) := lapply(.SD, convert_to_moes), .SDcols = paste0(edu_vars, "_moe"), by = .(year, fips)]  # convert back to MOEs
  } else {
    data <- data[, lapply(.SD, as.numeric), .SDcols = value_vars, by = .(year, fips)]  # convert value columns to numeric
  }
  
  setnames(data, old = sort(value_vars), new = raw_vars)
  
  # collapse value variables by sex
  data[, eval(edu_vars) := list(sum(m_lhs_white, f_lhs_white), sum(m_hs_white, f_hs_white), sum(m_sc_white, f_sc_white),
                                sum(m_ba_white, f_ba_white), sum(m_lhs_black, f_lhs_black), sum(m_hs_black, f_hs_black),
                                sum(m_sc_black, f_sc_black), sum(m_ba_black, f_ba_black), sum(m_lhs_aian, f_lhs_aian),
                                sum(m_hs_aian, f_hs_aian), sum(m_sc_aian, f_sc_aian), sum(m_ba_aian, f_ba_aian),
                                sum(m_lhs_asian, f_lhs_asian), sum(m_hs_asian, f_hs_asian), sum(m_sc_asian, f_sc_asian),
                                sum(m_ba_asian, f_ba_asian), sum(m_lhs_nhopi, f_lhs_nhopi), sum(m_hs_nhopi, f_hs_nhopi),
                                sum(m_sc_nhopi, f_sc_nhopi), sum(m_ba_nhopi, f_ba_nhopi), sum(m_lhs_other, f_lhs_other),
                                sum(m_hs_other, f_hs_other), sum(m_sc_other, f_sc_other), sum(m_ba_other, f_ba_other),
                                sum(m_lhs_multi, f_lhs_multi), sum(m_hs_multi, f_hs_multi), sum(m_sc_multi, f_sc_multi),
                                sum(m_ba_multi, f_ba_multi), sum(m_lhs_white_nh, f_lhs_white_nh), sum(m_hs_white_nh, f_hs_white_nh),
                                sum(m_sc_white_nh, f_sc_white_nh), sum(m_ba_white_nh, f_ba_white_nh), sum(m_lhs_hisp, f_lhs_hisp),
                                sum(m_hs_hisp, f_hs_hisp), sum(m_sc_hisp, f_sc_hisp), sum(m_ba_hisp, f_ba_hisp)), by = .(year, fips)]
  
  # collapse pop variables across race to get total pop by edu group
  # does not include NH White or Hispanic, since they are accounted for by the other groups (white includes NH white, all groups include hisp)
  data[, eval(pop_vars) := list(sum(lhs_white, lhs_black, lhs_aian, lhs_asian, lhs_nhopi, lhs_other, lhs_multi),
                                sum(hs_white, hs_black, hs_aian, hs_asian, hs_nhopi, hs_other, hs_multi),
                                sum(sc_white, sc_black, sc_aian, sc_asian, sc_nhopi, sc_other, sc_multi),
                                sum(ba_white, ba_black, ba_aian, ba_asian, ba_nhopi, ba_other, ba_multi)),
       by = .(year, fips)]
  
  # filter down to only variables of interest
  data <- data[, .SD, by = .(year, fips), .SDcols = c(edu_vars, if (include_moe) paste0(edu_vars, "_moe"), pop_vars)]
  
  return(list("data" = data, "file_paths" = files))
}
extractions <- lapply(2010:2021, extract_data, include_moe = T)
data <- rbindlist(lapply(1:length(extractions), function(i) {extractions[[i]]$data}))  # rbind all data objects from extractions

# Combine and format data -------------------------------------------------------------------------
moe <- TRUE %in% grepl("moe", names(data))  # detect whether margins of error were kept
data <- data[fips < 60000]
data <- melt.data.table(data, id.vars = c("year", "fips"),
                        measure.vars = patterns("white$", "black$", "aian$", "asian$", "nhopi$", "other$", "multi$", "white_nh$", "hisp$",
                                                if(moe) "white_moe", if(moe) "black_moe", if(moe) "aian_moe", if(moe) "asian_moe",
                                                if(moe) "nhopi_moe", if(moe) "other_moe", if(moe) "multi_moe", if(moe) "white_nh_moe",
                                                if(moe) "hisp_moe", "pop_"),
                        variable.name = "edu_label", value.name = c(races, if (moe) paste0(races, "_moe"), "pop"))
data[, edu_label := edus[edu_label]]
data <- merge(data, nids, by = "year")  # add nids

# Save output --------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- lapply(1:length(extractions), function(i) {as.list(extractions[[i]]$file_paths)})  # get list of lists of file paths (1 list per data year)
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths

# save data
saveRDS(data, paste0(out_dir, date_time_stamp, ".rds"))