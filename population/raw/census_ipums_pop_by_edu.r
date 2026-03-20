############################################################################################
## Description: Format population counts by education status, age, and sex from
##              the 2000 census using IPUMS microdata.
############################################################################################

# Load settings file and directories -------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("population$", getwd())) setwd("FILEPATH")
library(readstata13)
library(haven)
library(survey)

in_dir <- "FILEPATH"
out_dir <- "FILEPATH"
nid <- 43298

# Load and prep data -----------------------------------------------------------------------
file_path <- "FILEPATH"
data_00 <- read_dta(file_path)
data <- data.table(data_00)
setnames(data, "geo2_us2000", "puma")
data <- data[, .(puma, serial, pernum, perwt, strata, age, sex, educus)]

data[, puma := as.integer(as.character(puma))]
data[, sex := ifelse(sex == "1", 1L, 2L)]
data[, year := 2000L]
data[, agegp := cut(age, breaks = c(seq(15,85,5),105), right = FALSE)]

# group and encode education status
data[educus < 702, edu := "less than HS"]
data[educus == 702 | educus == 703, edu := "HS grad"]
data[educus == 821 | educus == 822, edu := "some college"]
data[educus > 824, edu := "college grad"]
data[, edu := factor(edu, levels = c("less than HS", "HS grad", "some college", "college grad"))]
data <- data[, list(pop = sum(perwt)), by = 'puma,year,sex,agegp,edu']

# drop children and adolescents, add NID
data <- data[!is.na(agegp),]
data[, nid := nid]

# Save output ------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- list("43298" = list(file_path))

# save output
saveRDS(data, paste0(out_dir, date_time_stamp, ".rds"))