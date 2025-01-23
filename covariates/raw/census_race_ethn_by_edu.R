####################################################################################################
## Description: Prep Census (1980, 1990, and 2000) data on race/ethnicity for adults aged 25+ by
##              detailed educational attainment group (less than HS, HS graduate, some college,
##              college graduate). This raw covariate data is the inverse, with extra edu levels,
##              of the output from census_education_by_race_ethn.r.
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")

in_dir <- "FILEPATH"
out_dir <- "FILEPATH"
nids <- data.table(year = c(1980, 1990, 2000), nid = c(240910, 195803, 214962))
races <- c("nh_white", "nh_black", "nh_aian", "nh_api", "nh_other", "hisp")

# 1980 ---------------------------------------------------------------------------------------------
# read and format metadata
meta_1980 <- fread("FILEPATH",
                   sep = "\t", header = F, skip = 70)
meta_1980 <- meta_1980[grep("DX4A", V1)]
meta_1980[, V1 := gsub(":    Male|:    Female", "", V1)]  # don't need split by sex
meta_1980 <- data.table(do.call("rbind", strsplit(meta_1980$V1, split = " >> ")))
setnames(meta_1980, c("var", "age", "education"))
meta_1980[, race_group := case_when(grepl("AA", var) ~ "nh_white", grepl("AB", var) ~ "nh_black", grepl("AC", var) ~ "nh_aian",
                                    grepl("AD", var) ~ "nh_api", grepl("AE", var) ~ "nh_other", grepl("AF", var) ~ "hisp")]
meta_1980[, edu_label := case_when(grepl("Elementary|High School: 1", education) ~ "lhs", grepl("High School: 4", education) ~ "hs",
                                   grepl("College: 1", education) ~ "sc", grepl("College: 4", education) ~ "ba")]

# get data and merge with metadata
file_path_80 <- list.files(paste0(in_dir, "1980"), pattern = "EDUCATIONAL_ATTAINMENT_Y", full.names = T)
data_1980 <- fread(file_path_80)
data_1980[, fips := 1000*STATEA + COUNTYA]
data_1980 <- data_1980[, c("YEAR", "fips", grep("DX4A", names(data_1980), value = T)), with = F]
data_1980 <- melt.data.table(data_1980, id.vars = c("YEAR", "fips"), measure.vars = patterns("DX4A"), variable.name = "var",
                             value.name = "pop")
data_1980 <- merge(data_1980, meta_1980, all.y = T, by = "var")
setnames(data_1980, "YEAR", "year")
data_1980 <- data_1980[, list(pop = sum(pop, na.rm = T)), keyby = 'year,fips,race_group,edu_label']
data_1980[, pop_over_25 := sum(pop), by = 'fips,edu_label']  # get total over-25 pop by edu
data_1980 <- dcast.data.table(data_1980, year + fips + edu_label + pop_over_25 ~ race_group, value.var = "pop")
setnames(data_1980, "pop_over_25", "pop")
setcolorder(data_1980, c("year", "fips", "edu_label", races, "pop"))

# 1990 ---------------------------------------------------------------------------------------------
# read and format metadata
meta_1990 <- fread("FILEPATH",
                   sep = "\t", header = F, skip = 72)
meta_1990 <- meta_1990[grep("FF5A", V1)]
meta_1990[, V1 := gsub(":   Male|:   Female", "", V1)]  # don't need split by sex
meta_1990 <- data.table(do.call("rbind", strsplit(meta_1990$V1, split = " >> ")))
setnames(meta_1990, c("var", "education"))
meta_1990[, race_group := case_when(grepl("AAX", var) ~ "hisp", grepl("ABR", var) ~ "nh_white", grepl("ABS", var) ~ "nh_black",
                                    grepl("ABT", var) ~ "nh_aian", grepl("ABU", var) ~ "nh_api", grepl("ABV", var) ~ "nh_other")]
meta_1990[, edu_label := case_when(grepl("graduate", education) ~ "hs",  # hs complete
                                   grepl("Some|Associate", education) ~ "sc", # some college
                                   grepl("Bachelor|Master|Professional|Doctorate", education) ~ "ba",  # college complete
                                   TRUE ~ "lhs")]  # everything else is less than HS

file_path_90 <- list.files(paste0(in_dir, "1990"), pattern = "NPB44_EDUCATIONAL_ATTAINMENT_Y", full.names = T)
data_1990 <- fread(file_path_90)
data_1990[, fips := 1000*STATEA + COUNTYA]
data_1990 <- data_1990[, c("YEAR", "fips", grep("FF5A", names(data_1990), value = T)), with = F]
data_1990 <- melt.data.table(data_1990, id.vars = c("YEAR", "fips"), measure.vars = patterns("FF5A"), variable.name = "var",
                             value.name = "pop")
data_1990 <- merge(data_1990, meta_1990, all.y = T, by = "var")
setnames(data_1990, "YEAR", "year")
data_1990 <- data_1990[, list(pop = sum(pop, na.rm = T)), keyby = 'year,fips,race_group,edu_label']
data_1990[, pop_over_25 := sum(pop), by = 'fips,edu_label']  # get total over-25 pop by edu
data_1990 <- dcast.data.table(data_1990, year + fips + edu_label + pop_over_25 ~ race_group, value.var = "pop")
setnames(data_1990, "pop_over_25", "pop")
setcolorder(data_1990, c("year", "fips", "edu_label", races, "pop"))

# 2000 ---------------------------------------------------------------------------------------------
meta_2000 <- fread("FILEPATH",
                   sep = "\t", header = F, skip = 84)
meta_2000 <- meta_2000[grep("HX8A", V1)]
meta_2000[, V1 := gsub(":  Male|:  Female", "", V1)]  # don't need split by sex
meta_2000 <- data.table(do.call("rbind", strsplit(meta_2000$V1, split = " >> ")))
setnames(meta_2000, c("var", "education"))
meta_2000[, race_group := case_when(grepl("AAHV", var) ~ "hisp", grepl("AAIV", var) ~ "nh_white", grepl("AAIX", var) ~ "nh_black",
                                    grepl("AAIZ", var) ~ "nh_aian", grepl("AAJB", var) ~ "nh_asian", grepl("AAJD", var) ~ "nh_nhopi",
                                    grepl("AAJF", var) ~ "nh_other", grepl("AAJH", var) ~ "nh_multi")]
meta_2000[, edu_label := case_when(grepl("graduate", education) ~ "hs",  # hs complete
                                   grepl("Some|Associate", education) ~ "sc", # some college
                                   grepl("Bachelor|Master|Professional|Doctorate", education) ~ "ba",  # college complete
                                   TRUE ~ "lhs")]  # everything else is less than HS

file_path_00 <- list.files(paste0(in_dir, "2000"), pattern = "FILEPATH", full.names = T)
data_2000 <- fread(file_path_00)
data_2000[, fips := 1000*STATEA + COUNTYA]
data_2000 <- data_2000[, c("YEAR", "fips", grep("HX8A", names(data_2000), value = T)), with = F]
data_2000 <- melt.data.table(data_2000, id.vars = c("YEAR", "fips"), measure.vars = patterns("HX8A"), variable.name = "var",
                             value.name = "pop")
data_2000 <- merge(data_2000, meta_2000, all.y = T, by = "var")
data_2000[race_group %in% c("nh_asian", "nh_nhopi"), race_group := "nh_api"]  # for now, map these to 1977 standard group of NH API to match 1980 & 1990
data_2000 <- data_2000[race_group != "nh_multi"]  # for now, drop NH Multiracial to match 1977 standard race groups
setnames(data_2000, "YEAR", "year")
data_2000 <- data_2000[, list(pop = sum(pop, na.rm = T)), keyby = 'year,fips,race_group,edu_label']
data_2000[, pop_over_25 := sum(pop), by = 'fips,edu_label']  # get total over-25 pop by edu
data_2000 <- dcast.data.table(data_2000, year + fips + edu_label + pop_over_25 ~ race_group, value.var = "pop")
setnames(data_2000, "pop_over_25", "pop")
setcolorder(data_2000, c("year", "fips", "edu_label", races, "pop"))

# Combine and format ---------------------------------------------------------------------------
data <- rbind(data_1980, data_1990, data_2000, use.names = T)
setkeyv(data, c("fips", "year"))
data <- merge(data, nids, by = "year")  # add nids

# Save output --------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- list(list(file_path_80), list(file_path_90), list(file_path_00))  # get list of lists of file paths (1 list per data year)
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths

# save data
saveRDS(data, paste0(out_dir, date_time_stamp, ".rds"))