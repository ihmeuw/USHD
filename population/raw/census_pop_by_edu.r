################################################################################
## Description: Format population counts by education status, age, and sex from
##              the 1990 and 2000 census.
################################################################################

# Load settings file and directories -------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("population$", getwd())) setwd("FILEPATH")
library(foreign)

in_dir1 <- "FILEPATH"
in_dir2 <- "FILEPATH"
out_dir <- "FILEPATH"
nids <- data.table(year = c(1990, 2000), nid = c(195803, 107062),
                   file_path = c("FILEPATH",
                                 "FILEPATH"))

# Load and prep 1990 census ----------------------------------------------------
# load meta data and find variables of interest
raw_90 <- read.dta(nids[year == 1990, file_path])
var.labels <- attr(raw_90, "var.labels")
data.key <- data.frame(variable = names(raw_90),var.labels)
data.key <- data.key[grep("^ff", data.key$variable),]
data.key$sex <- lapply(strsplit(as.character(data.key$var.labels), ">>"), "[", 1)
data.key$age <- lapply(strsplit(as.character(data.key$var.labels), ">>"), "[", 2)
data.key$status <- lapply(strsplit(as.character(data.key$var.labels), ">>"), "[", 3)

trim <- function(x) gsub("^\\s+|\\s+$", "", x)
data.key$status <- trim(data.key$status)
data.key$age <- trim(data.key$age)
data.key$sex <- trim(data.key$sex)

# merge data with meta data
raw_90$fips <- paste(raw_90$statea, raw_90$countya, sep = '')
raw_90 <- raw_90[, colSums(raw_90 != "") != 0]
raw_90 <- subset(raw_90, select = -c(countya, divisiona, msa_cmsaa, pmsaa, regiona, state, statea, anpsadpi))
raw_90 <- melt(raw_90, id.vars = c("year", "county", "fips"))
raw_90$fips <- as.integer(raw_90$fips)
raw_90 <- merge(raw_90, data.key, by = "variable", all.y = T)
data_90 <- data.table(raw_90)

# rename and format easy variables
data_90[, sex := ifelse(sex == "Male", 1L, 2L)]
data_90[, year := as.integer(year)]
data_90[, age_start := as.integer(substr(age, 1, 2))]
data_90[, age_end := as.integer(substr(age, 7, 8))]

# group and encode education status
data_90[status %in% c("Less than 9th grade", "9th to 12th grade, no diploma"), edu := "less than HS"]
data_90[status %in% c("High school graduate (includes equivalency)"), edu := "HS grad"]
data_90[status %in% c("Some college, no degree or associate degree"), edu := "some college"]
data_90[status %in% c("Bachelor's degree", "Graduate or professional degree"), edu := "college grad"]
data_90[, edu := factor(edu, levels = c("less than HS", "HS grad", "some college", "college grad"))]
data_90 <- data_90[, list(pop = sum(value)), by = 'fips,year,sex,age_start,age_end,edu']
rm(data.key, var.labels, raw_90)

# Load and prep 2000 census ----------------------------------------------------
# load meta data and find variables of interest
meta <- fread("FILEPATH", header = F)
setnames(meta, c("var", "label"))
meta <- meta[grepl("years: -|years and over: -", label),]
meta <- data.table(meta$var, do.call("rbind", strsplit(meta$label, split = ": - ")))
setnames(meta, c("variable", "sex", "age", "status"))

# merge data with meta data
data_00 <- fread(nids[year == 2000, file_path], header = T)
data_00 <- melt(data_00, id.vars = c("GEO.id", "GEO.id2", "GEO.display-label"))
data_00 <- merge(data_00, meta, by = "variable", all.y = T)
data_00 <- data_00[, list(GEO.id2, value, sex, status, age)]

# rename and format easy variables
setnames(data_00, "GEO.id2", "fips")
data_00[, fips := as.integer(as.character(fips))]
data_00[, value := as.numeric(as.character(value))]
data_00[, sex := ifelse(sex == "Male", 1L, 2L)]
data_00[, year := 2000L]
data_00[, age_start := as.integer(substr(age, 1, 2))]
data_00[, age_end := as.integer(substr(age, 7, 8))]

# group and encode education status
data_00[status %in% c("Less than 9th grade", "9th to 12th grade, no diploma"), edu := "less than HS"]
data_00[status %in% c("High school graduate (includes equivalency)"), edu := "HS grad"]
data_00[status %in% c("Associate degree", "Some college, no degree"), edu := "some college"]
data_00[status %in% c("Bachelor's degree", "Graduate or professional degree"), edu := "college grad"]
data_00[, edu := factor(edu, levels = c("less than HS", "HS grad", "some college", "college grad"))]
data_00 <- data_00[, list(pop = sum(value)), by = 'fips,year,sex,age_start,age_end,edu']

# drop territories
data_00 <- data_00[fips < 60000]
rm(meta)

# Combine, format, and save ----------------------------------------------------
table_list = list(data_90, data_00)
data <- rbindlist(table_list, use.names = T); rm(data_90, data_00, table_list)
data <- merge(data, nids, by = "year")  # add nids
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))  # name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- lapply(1:nrow(nids), function(i) {list(nids[i]$file_path)})
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths

# save output
saveRDS(data, file = paste0(out_dir, date_time_stamp, ".rds"))