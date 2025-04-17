####################################################################################################
## Description: Prep Bureau of Labor Statistics consumer price index to use in adjusting other
##              covariates for inflation
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")
library(readxl)

in_dir <- "FILEPATH"
out_dir <- "FILEPATH"
temp_dir <- "FILEPATH"
nid <- 111469

# Load and format data ----------------------------------------------------------------------------
file_path <- "FILEPATH"
data <- setDT(read_xlsx(file_path, skip = 11))
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