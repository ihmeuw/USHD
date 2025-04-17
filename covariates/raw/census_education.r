####################################################################################################
## Description: Prep Census (1980, 1990, and 2000) data on educational attainment for adults
##              aged 25+
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")
library(foreign)

in_dir1 <- "FILEPATH"
in_dir2 <- "FILEPATH"
in_dir3 <- "FILEPATH"
out_dir <- "FILEPATH"
nids <- data.table(year = c(1980, 1990, 2000), nid = c(106683, 13302, 105266))

# Load and prep 1980 data --------------------------------------------------------------------------
file_path_80 <- "FILEPATH"
data <- fread(file_path_80)
data <- data[!Fipco %in% c("", " ")]  # get rid of state-level data and row 2, which is for documentation
data <- data[, c('Fipco', 'LessThan9th', 'SomeHighSchool', 'HighSchool',
                 'SomeCollege', 'GradProf', 'Over25'), with = F]

# convert values to numeric
data <- data[, lapply(.SD, function(x) {gsub(",", "", x)}), .SDcols = names(data)]  # remove commas from data values (e.g., "8,303" becomes "8303")
data <- data[, lapply(.SD, as.numeric), .SDcols = names(data)]  # convert all values from character to numeric (e.g., "8303" becomes 8303)

# make percentages of those who completed high school and those who completed college.
# 'Over25' refers to the count of all people over 25, which is equal to the sum of all the
# other columns, so we use this as a denominator
data[, edu_hs := 100 * (HighSchool + SomeCollege + GradProf)/Over25]
data[, edu_ba := 100 * GradProf/Over25]
data1980 <- data[, list(fips = Fipco, year = 1980, edu_hs, edu_ba, pop = Over25)]; rm(data)

# Load and prep 1990 data (table P057, SF3 file) --------------------------------------------------
vars <- data.table(read.dbf("FILEPATH"))
vars <- vars[TABLE == "P057"]
# output below shows which fields we want to filter our data on
# vars
#     TABLE    FIELD                                        TEXT SEGMENT
#1313  P057     <NA>                      EDUCATIONAL ATTAINMENT  STF310
#1314  P057     <NA>         Universe: Persons 25 years and over    <NA>
#1315  P057 P0570001                         Less than 9th grade  STF310
#1316  P057 P0570002               9th to 12th grade, no diploma  STF310
#1317  P057 P0570003 High school graduate (includes equivalency)  STF310
#1318  P057 P0570004                     Some college, no degree  STF310
#1319  P057 P0570005                            Associate degree  STF310
#1320  P057 P0570006                           Bachelor's degree  STF310
#1321  P057 P0570007             Graduate or professional degree  STF310

file_path_90 <- "FILEPATH"
data <- data.table(read.dbf(file_path_90, as.is = T))
data <- data[SUMLEV == "050", c("STATEFP", "CNTY", paste0("P057000", 1:7)), with = F]
data[, fips := 1000 * as.integer(STATEFP) + as.integer(CNTY)]
setnames(data, names(data), gsub("P057000", "P", names(data)))
data[, edu_hs := 100 * (P3 + P4 + P5 + P6 + P7) / (P1 + P2 + P3 + P4 + P5 + P6 + P7)]
data[, edu_ba := 100 * (P6 + P7) / (P1 + P2 + P3 + P4 + P5 + P6 + P7)]
data[, pop := P1 + P2 + P3 + P4 + P5 + P6 + P7]
data1990 <- data[, list(fips, year = 1990, edu_hs, edu_ba, pop)]; rm(data)

# Load and prep 2000 data (table DP2, from SF3 file, from AFF) ------------------------------------
file_path_00 <- "FILEPATH"
data <- fread(file_path_00)
data <- data[, c("GEO.id2", "HC01_VC17", "HC01_VC18","HC01_VC09"), with = F]
setnames(data, c("fips", "edu_hs", "edu_ba","pop"))
data2000 <- data[, list(fips, year = 2000, edu_hs, edu_ba,pop)]; rm(data)

# Combine and format ------------------------------------------------------------------------------
data <- rbind(data1980, data1990, data2000, use.names = T)
setkeyv(data, c("fips", "year"))
data <- merge(data, nids, by = "year")  # add nids

# Save output --------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- list(list(file_path_80), list(file_path_90), list(file_path_00))  # get list of lists of file paths (1 list per data year)
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths

# save data
saveRDS(data, paste0(out_dir, date_time_stamp, ".rds"))