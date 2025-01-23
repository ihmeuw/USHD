####################################################################################################
## Description: Prep ACS 5-year foreign-born population estimates by educational attainment
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")

temp_dir <- "FILEPATH"
in_dir <- "FILEPATH"
out_dir <- "FILEPATH"
if (!dir.exists(out_dir)) dir.create(out_dir)  # create new folder for outputs if one doesn't exist already
nids <- data.table(year = 2007:2020, nid = c(241597, 241611:241614, 248849, 292127, 460848, 460853,
                                             460856, 472756, 501680, 527884, 548126))

# Load and format data -----------------------------------------------------------------------------
edus <- c("less_hs", "hs_ged", "some_college", "ba", "grad_prof")
edus_final <- c("less_hs", "hs_ged", "some_college", "ba_plus")  # we will end up combining BA and grad/prof to get BA+
fb_levels <- c("_pop", "_fb")
raw_vars <- as.vector(outer(edus, fb_levels, FUN = 'paste0'))  # need pops first, then foreign-born to match variable order
moe_names <- paste0(edus, "_moe")
moe_names_final <- paste0(edus_final, "_moe")

convert_to_variance <- function(x) {return((x/1.645)^2)}
convert_to_moes <- function(x) {return(1.645*sqrt(x))}

extract_data <- function(yr, include_moe = F){
  message("Formatting ", yr, " 5-year series")
  
  if(yr < 2022){
    files <- list.files(paste0(in_dir, "ACS_5YR_EST_", yr), pattern = "B06009", full.names = T)
    file <- grep("_CB", files, invert = T, value = T)  # drop metadata file
  }else{
    files <- list.files("FILEPATH", pattern = "B06009", full.names = T)
    files <- grep("2022", files, invert = F, value = T)
    file <- grep("_METADATA", files, invert = T, value = T)
  }
  
  if (yr == 2010) file <- grep("EDUCATIOAL", file, invert = T, value = T)  # drop extra misnamed file in 2010
  if(yr < 2022){
    data <- fread(file)
    data[, c("year", "fips") := list(yr-2, as.numeric(1000*STATEA + COUNTYA))]  # assign data year, fips
  }else{
    data <- fread(file, header = T)
    data[, c("year", "fips") := list(yr-2, as.numeric(substr(GEO_ID, (nchar(GEO_ID) - 4), nchar(GEO_ID))))]  # assign data year, fips
    
  }
  # if we want to keep margins of error, compile with that info. otherwise drop MOE columns
  value_vars <- grep("E002|E026|E003|E027|E004|E028|E005|E029|E006|E030", names(data), value = T)  # define relevant columns
  if (include_moe) {
    moe_vars <- grep("M026|M027|M028|M029|M030", names(data), value = T)  # define moe value columns
    data <- data[, lapply(.SD, as.numeric), .SDcols = c(value_vars, moe_vars), by = .(year, fips)]  # convert to numeric
    setnames(data, old = sort(moe_vars), new = moe_names)
    data[, eval(moe_names) := lapply(.SD, convert_to_variance), .SDcols = moe_names, by = .(year, fips)]  # convert MOEs to variances for combining BA and grad/prof
    data[, ba_plus_moe := ba_moe + grad_prof_moe, by = .(year, fips)]
    data[, eval(moe_names_final) := lapply(.SD, convert_to_moes), .SDcols = moe_names_final, by = .(year, fips)]  # convert back to MOEs
  } else {
    data <- data[, lapply(.SD, as.numeric), .SDcols = value_vars, by = .(year, fips)]  # convert value columns to numeric
  }
  
  setnames(data, old = sort(value_vars), new = raw_vars)
  
  # collapse BA and grad/prof into BA+
  data[, c("ba_plus_pop", "ba_plus_fb") := list(sum(ba_pop, grad_prof_pop), sum(ba_fb, grad_prof_fb)), by = .(year, fips)]
  
  data <- data[, .SD, by = .(year, fips), .SDcols = c(paste0(edus_final, "_fb"), if (include_moe) moe_names_final,
                                                      paste0(edus_final, "_pop"))]
  
  return(list("data" = data, "file_path" = file))
}
extractions <- lapply(2009:2022, extract_data, include_moe = T)
data <- rbindlist(lapply(1:length(extractions), function(i) {extractions[[i]]$data}))  # rbind all data objects from extractions

# Combine and format data -------------------------------------------------------------------------
moe <- TRUE %in% grepl("moe", names(data))  # detect whether margins of error were kept
data <- data[fips < 60000]
data <- melt.data.table(data, id.vars = c("year", "fips"), measure.vars = patterns("_fb", if (moe) "_moe", "_pop"),
                        variable.name = "edu", value.name = c("foreign_born", if (moe) "moe", "pop"))
data[, edu := case_when(edu == 1 ~ "< HS", edu == 2 ~ "HS or equivalent", edu == 3 ~ "Some college", edu == 4 ~ "BA or higher")]
data[, edu := ordered(factor(edu, levels = c("< HS", "HS or equivalent", "Some college", "BA or higher")))]
data <- merge(data, nids, by = "year")  # add nids
setkeyv(data, c("year", "fips"))
setorderv(data, c("year", "fips", "edu"))

# Save output --------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- lapply(1:length(extractions), function(i) {as.list(extractions[[i]]$file_path)})
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths

# save output
saveRDS(data, file = paste0(out_dir, date_time_stamp, ".rds"))