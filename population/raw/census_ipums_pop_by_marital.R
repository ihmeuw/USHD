############################################################################################
## Description: Format population counts by marital status, age, and sex from
##              the 2000 census (5% sample) using IPUMS microdata.
############################################################################################

# Load settings file and directories -------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("population$", getwd())) setwd("FILEPATH")
source("counties/raw/_load_settings.R")
# library(readstata13)
library(haven)
library(survey)

in_dir <- "FILEPATH"
out_dir <- "FILEPATH"
nid <- 43298

# Load and prep data -----------------------------------------------------------------------
file_path <- "FILEPATH"
cols <- c("year", "geo2_us2000", "perwt", "age", "sex", "marst")  # only read in columns of interest (otherwise reading in data will take forever and tons of memory)
dt <- as.data.table(read_dta(file_path, col_select = all_of(cols)))
setnames(dt, "geo2_us2000", "puma")

dt <- dt[age > 19, ]
dt[, year := as.integer(year)]
dt[, puma := as.integer(as.character(puma))]
dt[, sex := as.integer(sex)]
dt[, agegp := cut(age, breaks = c(seq(20, 85, 5), 105), right = FALSE)]
dt[, marital := case_when(marst %in% 3:4 ~ 2,  # former
                          marst %in% 2 ~ 1,  # current
                          marst == 1 ~ 3,  # never
                          TRUE ~ NA_real_)]

dt <- dt[!is.na(marital), .(pop = sum(perwt)), .(year, marital, sex, agegp, puma)]  # drop observations with missing marital status
dt[, nid := nid]

# Save output ------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- list("43298" = list(file_path))

# save output
saveRDS(dt, paste0(out_dir, date_time_stamp, ".rds"))