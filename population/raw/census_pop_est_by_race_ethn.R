###################################################################################################
## Description: Format census population estimates by race and ethnicity for
##              2000+.
##              
## Note: in 2021+, we need to temporarily approximate values for Connecticut FIPS codes
###################################################################################################

# Load settings file and directories --------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("population$", getwd())) setwd("FILEPATH")
library(tidycensus, lib.loc = "FILEPATH")
library(ggplot2)

in_dir  <- "FILEPATH"
nhgis_in_dir <- "FILEPATH"
out_dir <- "FILEPATH"
nids <- data.table(year = 2000:2022,
                   nid = c(rep(438685, 10), rep(512373, 10), rep(529461, 3)))

# Load data ---------------------------------------------------------------------------------------
# load and combine 2000-2010 intercensal files
f <- list.files(path = paste0(in_dir, "intercensal_2000_2010/"), pattern = "*.csv", full.names = T)
data_ic10 <- rbindlist(lapply(f, fread))
data_ic10[, type := "intercensal10"]

# load 2010-2020 postcensal (2010) files
f_pc10 <- "FILEPATH"
data_pc10 <- fread(f_pc10)
data_pc10[, type := "postcensal10"]

# load 2020-2022 postcensal (2020) files, combine with 2000-2019
f_pc20 <- "FILEPATH"
data_pc20 <- fread(f_pc20)
data_pc20[, type := "postcensal20"]
data <- rbind(data_ic10, data_pc10, data_pc20, fill = T); rm(data_ic10, data_pc10, data_pc20)

# Format ------------------------------------------------------------------------------------------
# reshape long
data <- melt(data, id.vars = c("SUMLEV", "STATE", "COUNTY", "STNAME", "CTYNAME", "YEAR", "AGEGRP", "type"),
             value.name = "pop")
data[, fips := as.integer(STATE * 1000 + COUNTY)]

# format race
data <- data[grepl("^(H|NH)[[:alpha:]]", variable), ] # drop combined races and combined ethnicity totals
data <- data[!grepl("AC_", variable), ] # drop "alone or in combination" counts (which overlap with each other)
data[grepl("^(H|NH)WA_", variable), race := "White alone"]
data[grepl("^(H|NH)BA_", variable), race := "Black alone"]
data[grepl("^(H|NH)IA_", variable), race := "AIAN alone"]
data[grepl("^(H|NH)AA_", variable), race := "Asian alone"]
data[grepl("^(H|NH)NA_", variable), race := "NHOPI alone"]
data[grepl("^(H|NH)TOM_", variable), race := "Two or More races"]
data[, race := factor(race, levels = c("White alone", "Black alone", "AIAN alone", "Asian alone", "NHOPI alone", "Two or More races"))]

# format Hispanic ethnicity
data[grepl("^H", variable), hisp := 1L]
data[grepl("^NH", variable), hisp := 0]

# format age
data <- data[!((type == "intercensal10" & AGEGRP == 99) | (type %in% c("postcensal10", "postcensal20") & AGEGRP == 0)), ] # drop all ages
data[type == "intercensal10", age_start := as.integer(c(0, 1, seq(5, 85, 5)))[AGEGRP + 1]]
data[type == "intercensal10", age_end := as.integer(c(0, 4, seq(9, 84, 5), NA))[AGEGRP + 1]]
data[type %in% c("postcensal10", "postcensal20"), age_start := as.integer(seq(0, 85, 5))[AGEGRP]]
data[type %in% c("postcensal10", "postcensal20"), age_end := as.integer(c(seq(4, 84, 5), NA))[AGEGRP]]

# format year
data[type == "intercensal10" & YEAR %in% 2:12, year := YEAR + 1998]  # 2-12 = 2000-2010
data[type == "postcensal10" & YEAR %in% 3:13, year := YEAR + 2007]  # 3-13 = 2010-2020
data[type == "postcensal20" & YEAR %in% 2:4, year := YEAR + 2018]  # 2-4 = 2020-2022
data[, year := as.integer(year)]

# format sex
data[grepl("_MALE", variable), sex := 1L]
data[grepl("_FEMALE", variable), sex := 2L]

# subset to desired observations
data <- data[!is.na(year), ] # drop census counts and bases (all in April; keeping July estimates)
data <- data[!(year == 2010 & type == "intercensal10")] # 2010 is in two files; use the version in the postcensal10 file
to_split <- copy(data[fips %in% 9000:9999 & year >= 2020 & type == "postcensal20"])  # isolate post-2020 census CT data to split; include 2020 for intercensal estimates calculation in prep code
data <- data[!(year == 2020 & type == "postcensal20")] # 2020 is in two files; use the version in the postcensal10 file to more closely match NCHS estimates when race bridging

# subset to formatted variables
data <- data[, list(fips, year, sex, age_start, age_end, race, hisp, pop)]
data[, pop := as.numeric(pop)]  # pop is type 'character', set as numeric
to_split <- to_split[, list(fips, year, sex, age_start, age_end, race, hisp, pop)]  # format CT pops
to_split[, pop := as.numeric(pop)]

# TEMP population adjustment for Males 85+ in North Slope Borough, AK
# duplicate 2021 estimates in 2022; check necessity of this with release of Vintage 2023
nrow_start <- nrow(data)
replacements_by_race <- data[fips == 2185 & sex == 1 & age_start == 85 & year == 2021]
replacements_by_race[, year := 2022]  # set year to 2022
data <- data[!(fips == 2185  & sex == 1 & age_start == 85 & year == 2022)]  # drop actual 2022 values from data
data <- rbindlist(list(data, replacements_by_race), use.names = T)  # add back 2021 values for 2022
stopifnot(nrow_start == nrow(data))

# Split CT 2020-2022 planning regions -------------------------------------------------------------
# load tract-level 2020 DHCa estimates from NHGIS and format to match PEP
f_tracts <- "FILEPATH"
ct_tracts <- fread(f_tracts, header = T)[STATEA == 9]  # read in tract-level estimates and filter to CT
raw_var_names <- paste(c(paste0("00", 3:9), paste0("0", c(10:25, 27:49))), collapse = "|")
est_vars <- grep(raw_var_names, names(ct_tracts), value = T)  # define columns of interest
setnames(ct_tracts, c("TRACTA"), c("tract_fips"))
ct_tracts[, county_fips := 1000*STATEA+COUNTYA]
ct_tracts <- melt.data.table(ct_tracts, id.vars = c("county_fips", "tract_fips"), measure.vars = est_vars,
                             variable.name = "sex_age_race", value.name = "pop")
ct_tracts <- ct_tracts[!grepl("U70|U76|U8D", sex_age_race)]  # drop some other race & Hispanic any race (not contained in PEP estimates)
ct_tracts[, hisp := ifelse(grepl("U78|U79|U8A|U8B|U8C|U8E", sex_age_race), 1, 0)]  # set Hispanic indicator
stopifnot(nrow(ct_tracts[hisp == 1]) == nrow(ct_tracts[hisp == 0]))  # should be same number of Hisp/non Hisp rows
ct_tracts[, race := case_when(grepl("U71|U78", sex_age_race) ~ "White alone",
                              grepl("U72|U79", sex_age_race) ~ "Black alone",
                              grepl("U73|U8A", sex_age_race) ~ "AIAN alone",
                              grepl("U74|U8B", sex_age_race) ~ "Asian alone",
                              grepl("U75|U8C", sex_age_race) ~ "NHOPI alone",
                              grepl("U77|U8E", sex_age_race) ~ "Two or More races",
                              TRUE ~ NA_character_)]
stopifnot(nrow(ct_tracts[is.na(race)]) == 0)  # all rows should have a race group
ct_tracts[, race := factor(race, levels = c("White alone", "Black alone", "AIAN alone", "Asian alone", "NHOPI alone", "Two or More races"))]  # match PEP format
ct_tracts[, sex := ifelse(grepl(paste(paste0("0", 27:49), collapse = "|"), sex_age_race), 2, 1)]
stopifnot(nrow(ct_tracts[sex == 1]) == nrow(ct_tracts[sex == 2]))  # should be same number of male/female rows
ct_tracts[, age := case_when(grepl("003|027", sex_age_race) ~ 0, grepl("004|028", sex_age_race) ~ 5,
                             grepl("005|029", sex_age_race) ~ 10, grepl("006|007|030|031", sex_age_race) ~ 15,
                             grepl("008|009|010|032|033|034", sex_age_race) ~ 20,
                             grepl("011|035", sex_age_race) ~ 25, grepl("012|036", sex_age_race) ~ 30,
                             grepl("013|037", sex_age_race) ~ 35, grepl("014|038", sex_age_race) ~ 40,
                             grepl("015|039", sex_age_race) ~ 45, grepl("016|040", sex_age_race) ~ 50,
                             grepl("017|041", sex_age_race) ~ 55, grepl("018|019|042|043", sex_age_race) ~ 60,
                             grepl("020|021|044|045", sex_age_race) ~ 65, grepl("022|046", sex_age_race) ~ 70,
                             grepl("023|047", sex_age_race) ~ 75, grepl("024|048", sex_age_race) ~ 80,
                             grepl("025|049", sex_age_race) ~ 85, TRUE ~ NA_real_)]
stopifnot(nrow(ct_tracts[is.na(age)]) == 0)  # all ages should have a value

# map CT census tracts to planning region FIPS, then merge onto tract dataset
ct_tract_to_pr <- as.data.table(get_acs(geography = "tract", variables = c("B01001_001"),
                                        year = 2022, state = "CT", survey = "acs5"))
ct_tract_to_pr[, c("tract_fips", "pr_fips") := list(as.numeric(substr(GEOID, 6, 11)), as.numeric(substr(GEOID, 1, 5)))]
tract_pr_map <- unique(ct_tract_to_pr[, list(tract_fips, pr_fips)]); rm(ct_tract_to_pr)
ct_tracts[tract_pr_map, on = "tract_fips", pr_fips := i.pr_fips]
ct_tracts <- ct_tracts[!tract_fips %in% c(990000, 990100)]  # remove water tracts since they have 0 population and cause issues with proportion calculations

# aggregate tracts to county-planning region pairs and calculate splitting proportions
ct_props <- copy(ct_tracts[, list(pop = sum(pop)), by = c("county_fips", "pr_fips", "sex", "age", "race", "hisp")])
ct_props[, prop := pop/sum(pop), by = "pr_fips,sex,age,race,hisp"]
ct_props[, pop2 := sum(pop), by = "county_fips,pr_fips,sex,race,hisp"]  # agg over age when total pop in a stratum is 0
ct_props[, prop2 := pop2/sum(pop), by = 'pr_fips,sex,race,hisp']
ct_props[, pop3 := sum(pop), by = "county_fips,pr_fips,race,hisp"]  # then agg over age and sex
ct_props[, prop3 := pop3/sum(pop), by = 'pr_fips,race,hisp']
stopifnot(nrow(ct_props[is.na(prop3)]) == 0)  # check that proportions exist everywhere
ct_props[, paste0("pop", 2:3) := NULL]

# merge with CT PEP estimates and calculate pops for county FIPS
setnames(ct_props, "age", "age_start")
setnames(to_split, "fips", "pr_fips")
ct_pop <- merge.data.table(to_split, ct_props[, -"pop"], by = c("pr_fips", "sex", "age_start", "race", "hisp"),
                           all.x = T, allow.cartesian = T)
start_pop <- sum(to_split$pop)  # get starting pop to check that totals align after splitting
ct_pop[, pop := ifelse(!is.na(prop), pop * prop,
                       ifelse(!is.na(prop2), pop * prop2,
                              ifelse(!is.na(prop3), pop * prop3, NA)))]
stopifnot(nrow(ct_pop[pop < 0 | is.na(pop)]) == 0)  # make sure new pop is within reasonable bounds and nonmissing

# aggregate to county FIPS
setnames(ct_pop, "county_fips", "fips")
ct_pop <- ct_pop[, list(pop = sum(pop)), by = "year,fips,sex,age_start,race,hisp"]
stopifnot(nrow(ct_pop[is.na(pop)]) == 0)  # make sure all strata have population values
stopifnot(sum(ct_pop$pop) == start_pop)  # check that pop totals did not change

# save copy of FIPS-level CT estimates for race-bridging in prepped code
saveRDS(ct_pop, paste0(out_dir, "ct_pop_by_county_v2022.rds"))
ct_pop <- ct_pop[year >= 2021]  # drop 2020 since we are using the post-2010

# Replace CT in PEP estimates with county-level estimates in 2020-22 ------------------------------
nrow_start <- nrow(data)
data <- data[!(year >= 2021 & fips %in% 9000:9999)]  # drop planning region-level rows in 2021+
data <- rbindlist(list(data, ct_pop), use.names = T, fill = T)

# difference in total rows should be:
# 1 per data year after 2020 (9 planning regions -> 8 counties) * unique combination of sex/age/race/hisp
expected_diff <- (max(data$year)-2020)*nrow(unique(data[year >= 2021, list(sex, age_start, race, hisp)]))
stopifnot(nrow_start - nrow(data) == expected_diff)

# Save data ---------------------------------------------------------------------------------------
data <- merge(data, nids, by = "year")  # add nids
stopifnot(nrow(data[is.na(fips)]) == 0)  # make sure there are no missing FIPS codes
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))  # name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- list("438685" = lapply(1:length(f), function(i) {f[i]}),
                      "512373" = list(f_pc10),
                      "529461" = list(f_pc20))
# "498360" = list(f_tracts))  # for now, leave this out of the metadata since it causes issues with upload to the ushd db

# save output
saveRDS(data, file = paste0(out_dir, date_time_stamp, ".rds"))