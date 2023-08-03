####################################################################################################
## Description: Prep Bureau of Labor Statistics consumer price index to use in adjusting other
##              covariates for inflation
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
source("counties/raw/_load_settings.r")

root <- ifelse(Sys.info()[1] == "Windows", "[FILEPATH]", "[FILEPATH]")
in_dir <- paste0(root, "[FILEPATH]")
out_dir <- paste0(root, "[FILEPATH]")
if (!dir.exists(out_dir)) dir.create(out_dir)  # create new folder for outputs if one doesn't exist already
nid <- 111469

# Load and format data ----------------------------------------------------------------------------
file_path <- paste0(in_dir, "[FILEPATH]")
data <- fread(file_path, skip = 10)
data <- data[, list(Year, Annual)]
setnames(data, c("year", "cpi"))

data <- data[!is.na(cpi), list(year, cpi)]
setkey(data, "year")
data[, nid := nid]  # add nid

# Save output --------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- list(list(file_path))
names(nid_path_list) = nid  # set NID as name of file path list

# save data
saveRDS(data, paste0(out_dir, date_time_stamp, ".rds"))

# save extraction metadata
cov_extract <- save_covariate_extraction(path = paste0(out_dir, date_time_stamp, ".rds"), 
                                         description = "BLS consumer price index", 
                                         prev_issues = "none",
                                         raw_data_filepaths = nid_path_list)
