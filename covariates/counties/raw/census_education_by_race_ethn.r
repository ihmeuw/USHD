####################################################################################################
## Description: Prep Census (1980, 1990, and 2000) data on educational attainment for adults
##              aged 25+ by race and ethnicity
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
source("counties/raw/_load_settings.r")

root <- ifelse(Sys.info()[1] == "Windows", "[FILEPATH]", "[FILEPATH]")
in_dir <- paste0(root, "[FILEPATH]")
out_dir <- paste0(root, "[]")
nids <- data.table(year = c(1980, 1990, 2000), nid = c(240910, 195803, 214962))

# 1980 ---------------------------------------------------------------------------------------------
# read and format metadata
meta_1980 <- fread(paste0(in_dir, "[FILEPATH]"), sep = "\t", header = F, skip = 70)
meta_1980 <- meta_1980[grep("DX4A", V1)]
meta_1980[, V1 := gsub(":    Male|:    Female", "", V1)]  # don't need split by sex
meta_1980 <- data.table(do.call("rbind", strsplit(meta_1980$V1, split = " >> ")))
setnames(meta_1980, c("var", "age", "education"))
meta_1980[, race_group := case_when(grepl("AA", var) ~ "NH White", grepl("AB", var) ~ "NH Black", grepl("AC", var) ~ "NH AIAN",
                                  grepl("AD", var) ~ "NH API", grepl("AE", var) ~ "NH Other Race", grepl("AF", var) ~ "Hispanic")]
meta_1980[, edu := case_when(grepl("Elementary|High School: 1", education) ~ "< HS", grepl("High School: 4", education) ~ "HS",
                           grepl("College: 1", education) ~ "Some college", grepl("College: 4", education) ~ "BA")]

# get data and merge with metadata
file_path_80 <- paste0(in_dir, "[FILEPATH]")
data_1980 <- fread(file_path_80)
data_1980[, fips := as.numeric(paste0(STATEA, str_pad(as.character(COUNTYA), 3, pad = "0")))]
data_1980 <- data_1980[, c("YEAR", "fips", grep("DX4A", names(data_1980), value = T)), with = F]
data_1980 <- melt.data.table(data_1980, id.vars = c("YEAR", "fips"), measure.vars = patterns("DX4A"), variable.name = "var",
                           value.name = "pop")
data_1980 <- merge(data_1980, meta_1980, all.y = T, by = "var")
setnames(data_1980, "YEAR", "year")
data_1980 <- data_1980[, list(pop = sum(pop, na.rm = T)), keyby = 'year,fips,race_group,edu']
data_1980[, pop_over_25 := sum(pop), by = 'fips,race_group']  # get total over-25 pop for calculating proportions
data_1980 <- data_1980[edu != "< HS"]  # drop less than HS
data_1980[, c("edu_hs", "edu_ba") := list(sum(pop), pop[edu == "BA"]), by = list(fips, race_group)]
data_1980[, c("pop", "edu") := NULL]
data_1980 <- unique(data_1980)
setnames(data_1980, "pop_over_25", "pop")
setcolorder(data_1980, c("year", "fips", "race_group", "edu_hs", "edu_ba", "pop"))

# 1990 ---------------------------------------------------------------------------------------------
# read and format metadata
meta_1990 <- fread(paste0(in_dir, "[FILEPATH]"), sep = "\t", header = F, skip = 72)
meta_1990 <- meta_1990[grep("FF5A", V1)]
meta_1990[, V1 := gsub(":   Male|:   Female", "", V1)]  # don't need split by sex
meta_1990 <- data.table(do.call("rbind", strsplit(meta_1990$V1, split = " >> ")))
setnames(meta_1990, c("var", "education"))
meta_1990[, race_group := case_when(grepl("AAX", var) ~ "Hispanic", grepl("ABR", var) ~ "NH White", grepl("ABS", var) ~ "NH Black",
                                    grepl("ABT", var) ~ "NH AIAN", grepl("ABU", var) ~ "NH API", grepl("ABV", var) ~ "NH Other race")]
meta_1990[, edu := case_when(grepl("graduate|Some|Associate", education) ~ "HS",  # hs complete
                             grepl("Bachelor|Master|Professional|Doctorate", education) ~ "BA",  # college complete
                             TRUE ~ "< HS")]  # everything else is less than HS

file_path_90 <- paste0(in_dir, "[FILEPATH]")
data_1990 <- fread(file_path_90)
data_1990[, fips := as.numeric(paste0(STATEA, str_pad(as.character(COUNTYA), 3, pad = "0")))]
data_1990 <- data_1990[, c("YEAR", "fips", grep("FF5A", names(data_1990), value = T)), with = F]
data_1990 <- melt.data.table(data_1990, id.vars = c("YEAR", "fips"), measure.vars = patterns("FF5A"), variable.name = "var",
                             value.name = "pop")
data_1990 <- merge(data_1990, meta_1990, all.y = T, by = "var")
setnames(data_1990, "YEAR", "year")
data_1990 <- data_1990[, list(pop = sum(pop, na.rm = T)), keyby = 'year,fips,race_group,edu']
data_1990[, pop_over_25 := sum(pop), by = 'fips,race_group']  # get total over-25 pop
data_1990 <- data_1990[edu != "< HS"]  # drop less than HS
data_1990[, c("edu_hs", "edu_ba") := list(sum(pop), pop[edu == "BA"]), by = list(fips, race_group)]
data_1990[, c("pop", "edu") := NULL]
data_1990 <- unique(data_1990)
setnames(data_1990, "pop_over_25", "pop")
setcolorder(data_1990, c("year", "fips", "race_group", "edu_hs", "edu_ba", "pop"))

# 2000 ---------------------------------------------------------------------------------------------
meta_2000 <- fread(paste0(in_dir, "[FILEPATH]"), sep = "\t", header = F, skip = 84)
meta_2000 <- meta_2000[grep("HX8A", V1)]
meta_2000[, V1 := gsub(":  Male|:  Female", "", V1)]  # don't need split by sex
meta_2000 <- data.table(do.call("rbind", strsplit(meta_2000$V1, split = " >> ")))
setnames(meta_2000, c("var", "education"))
meta_2000[, race_group := case_when(grepl("AAHV", var) ~ "Hispanic", grepl("AAIV", var) ~ "NH White", grepl("AAIX", var) ~ "NH Black",
                                    grepl("AAIZ", var) ~ "NH AIAN", grepl("AAJB", var) ~ "NH Asian", grepl("AAJD", var) ~ "NH NHOPI",
                                    grepl("AAJF", var) ~ "NH Some Other Race", grepl("AAJH", var) ~ "NH Multiracial")]
meta_2000[, edu := case_when(grepl("graduate|Some|Associate", education) ~ "HS",  # hs complete
                             grepl("Bachelor|Master|Professional|Doctorate", education) ~ "BA",  # college complete
                             TRUE ~ "< HS")]  # everything else is less than HS

file_path_00 <- paste0(in_dir, "[FILEPATH]")
data_2000 <- fread(file_path_00)
data_2000[, fips := as.numeric(paste0(STATEA, str_pad(as.character(COUNTYA), 3, pad = "0")))]
data_2000 <- data_2000[, c("YEAR", "fips", grep("HX8A", names(data_2000), value = T)), with = F]
data_2000 <- melt.data.table(data_2000, id.vars = c("YEAR", "fips"), measure.vars = patterns("HX8A"), variable.name = "var",
                             value.name = "pop")
data_2000 <- merge(data_2000, meta_2000, all.y = T, by = "var")
setnames(data_2000, "YEAR", "year")
data_2000 <- data_2000[, list(pop = sum(pop, na.rm = T)), keyby = 'year,fips,race_group,edu']
data_2000[, pop_over_25 := sum(pop), by = 'fips,race_group']  # get total over-25 pop for calculating proportions
data_2000 <- data_2000[edu != "< HS"]  # drop less than HS
data_2000[, c("edu_hs", "edu_ba") := list(sum(pop), pop[edu == "BA"]), by = list(fips, race_group)]
data_2000[, c("pop", "edu") := NULL]
data_2000 <- unique(data_2000)
setnames(data_2000, "pop_over_25", "pop")
setcolorder(data_2000, c("year", "fips", "race_group", "edu_hs", "edu_ba", "pop"))

# Combine and format data --------------------------------------------------------------------------
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

# save extraction metadata
cov_extract <- save_covariate_extraction(path = paste0(out_dir, date_time_stamp, ".rds"), 
                                         description = "Census education (HS+ and BA+) by race/ethnicity", 
                                         prev_issues = "none",
                                         raw_data_filepaths = nid_path_list)
