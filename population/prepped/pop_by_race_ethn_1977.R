##############################################################################################
## Description: Prep populations by mcnty-year-age-sex-race/ethnicity for the 1977 race
##              categories. Also produce populations by mcnty-year-age-sex by aggregating
##              across race, ensuring that raking versions are always aligned.
##############################################################################################

library(ggplot2)
library(ggforce)
library(RColorBrewer)

source(paste0("FILEPATH", "/_bridge_nchs_pop.R"))
source(paste0("FILEPATH", "/create_intercensal_2010_2020.R"))
loc2 <- fread("FILEPATH")
archive_date <- paste0(gsub("-", "_", Sys.Date()), "/")  # folder named for plot archive date (today)

## Load raw population data -----------------------------------------------------------------------
## nchs bridged race estimates
pop_meta_df <- get_population_metadata("nchs_pop_est_by_race_ethn")  # load metadata for best extraction
pop <- readRDS(paste0(pop_dir, "FILEPATH", unique(pop_meta_df[, file])))  # read in file specified in metadata

## census population estimates for bridging 2021+
census_meta_df <- get_population_metadata("census_pop_est_by_race_ethn")
census_pop <- readRDS(paste0(pop_dir, "FILEPATH", unique(census_meta_df[, file])))[year >= 2020][, -"nid"]
setnames(census_pop, "race", "race_label")  # assign race ids to census estimates for merging
census_pop[, race := dplyr::case_when(race_label == "White alone" ~ 1, race_label == "Black alone" ~ 2,
                                      race_label == "AIAN alone" ~ 3, race_label %in% c("NHOPI alone", "Asian alone") ~ 4,
                                      race_label == "Two or More races" ~ 9, TRUE ~ NA_real_)]
stopifnot(nrow(census_pop[is.na(race)]) == 0)  # should be no missing race info
census_pop[, race := ifelse(hisp == 1, 7, race)]  # assign Hispanic based on indicator variable
census_pop <- census_pop[, list(pop_alone = sum(pop)), by = 'fips,year,sex,age_start,race']

## for the bridged race estimates, Hispanic ethnicity is still a single group (7) and non-Hispanics
## are split by bridged race (groups 1-4 = white, black, AIAN, and API)
pop[, race := ifelse(hisp == 1, 7, race)]
pop <- pop[, list(pop = sum(pop)), by = 'fips,year,sex,age,race']

## Bridge 2021+ estimates to 1977 race/ethnicity groups --------------------------------------------
## calculate proportions from NCHS 2020 for splitting 0-4 age group
props_u5 <- copy(pop[age < 5 & year == 2020])
props_u5[, age := ifelse(age == 0, 0, 1)]
props_u5 <- props_u5[, list(pop = sum(pop)), by = 'year,age']
props_u5[, freq := pop/sum(pop), by = 'year']
prop0 <- props_u5[age == 0, freq]
prop1to4 <- props_u5[age == 1, freq]; rm(props_u5)

## calculate bridged-race populations
pop_bridged <- rbindlist(lapply(unique(census_pop[year >= 2021, year]), function(yr) {
  message("bridging ", yr)
  temp <- bridge_nchs_pop(census2020 = census_pop[year == 2020], dt_to_bridge = census_pop[year == yr],
                          nchs2020 = pop[year == 2020], data_year = yr)
  
  # split 0-4 age group into 0 and 1-4
  pop_u5 <- copy(temp[age_start == 0])
  start_pop <- sum(pop_u5$pop)
  temp <- temp[age_start > 0]
  pop_u5 <- rbind(copy(pop_u5[, age_start := 0]), copy(pop_u5[, age_start := 1]))
  pop_u5[, pop := ifelse(age_start == 0, pop * prop0, pop * prop1to4)]
  stopifnot(all.equal(sum(pop_u5$pop), start_pop))
  temp <- rbind(pop_u5, temp); rm(pop_u5, start_pop)
  temp
}), fill = T, use.names = T)

## classify all population into 5-year age groups
pop[, age_start := 5 * floor(age / 5)]
pop[age %between% c(1, 4), age_start := 1]
pop[, age := NULL]
pop <- rbindlist(list(pop, pop_bridged), use.names = T, fill = T); rm(pop_bridged)
pop <- pop[, list(pop = sum(pop)), by = 'fips,year,sex,age_start,race']

## Estimate intercensal population values for 2010-2020 -------------------------------------------
# load raw postcensal 2020 file (update with each new vintage)
pc20 <- fread("FILEPATH")

pc20 <- melt(pc20, id.vars = c("SUMLEV", "STATE", "COUNTY", "STNAME", "CTYNAME", "YEAR", "AGEGRP"),
             value.name = "pop")
pc20[, fips := as.integer(STATE * 1000 + COUNTY)]

# format race for bridging
pc20 <- pc20[grepl("^(H|NH)[[:alpha:]]", variable), ] # drop combined races and combined ethnicity totals
pc20 <- pc20[!grepl("AC_", variable), ] # drop "alone or in combination" counts (which overlap with each other)
pc20[grepl("^(H|NH)WA_", variable), race := 1]  # White alone
pc20[grepl("^(H|NH)BA_", variable), race := 2]  # Black alone
pc20[grepl("^(H|NH)IA_", variable), race := 3]  # AIAN alone
pc20[grepl("^(H|NH)AA_", variable), race := 4]  # Asian alone
pc20[grepl("^(H|NH)NA_", variable), race := 4]  # NHOPI alone
pc20[grepl("^(H|NH)TOM_", variable), race := 9]  # Two or more races
stopifnot(nrow(pc20[is.na(race)]) == 0)  # should be no missing race info

# format Hispanic ethnicity
pc20[grepl("^H", variable), hisp := 1L]
pc20[grepl("^NH", variable), hisp := 0]
pc20[, race := ifelse(hisp == 1, 7, race)]  # assign Hispanic based on indicator variable

# format age
pc20 <- pc20[!AGEGRP == 0, ] # drop all ages
pc20[, age_start := as.integer(seq(0, 85, 5))[AGEGRP]]

# format year
pc20[YEAR %in% 2:4, year := YEAR + 2018]  # 2+ = 2020+
pc20[, year := as.integer(year)]
pc20 <- pc20[year == 2020]

# format sex
pc20[grepl("_MALE", variable), sex := 1L]
pc20[grepl("_FEMALE", variable), sex := 2L]

# load county-level CT pops in 2020 and replace planning region-level pops in pc20
ct_pop <- readRDS(paste0(pop_dir, "FILEPATH"))[year == 2020]
ct_pop[, race := dplyr::case_when(race == "White alone" ~ 1, race == "Black alone" ~ 2,
                                  race == "AIAN alone" ~ 3, race %in% c("NHOPI alone", "Asian alone") ~ 4,
                                  race == "Two or More races" ~ 9, TRUE ~ NA_real_)]
stopifnot(nrow(ct_pop[is.na(race)]) == 0)  # should be no missing race info
ct_pop[, race := ifelse(hisp == 1, 7, race)]  # assign Hispanic based on indicator variable
pc20 <- pc20[!fips %in% 9000:9999]
pc20 <- rbindlist(list(pc20, ct_pop), use.names = T, fill = T)
pc20 <- pc20[, list(pop_alone = sum(pop)), by = 'fips,year,sex,age_start,race']

# bridge to 1977 race/ethnicity groups
pc20_bridged <- bridge_nchs_pop(census2020 = census_pop[year == 2020], dt_to_bridge = pc20,
                                nchs2020 = pop[year == 2020], data_year = 2020)

# split ages 0-4 using NCHS 2020 proportions
pop_u5 <- copy(pc20_bridged[age_start == 0])  # then apply to bridged post-2020 census estimates
start_pop <- sum(pop_u5$pop)
pc20_bridged <- pc20_bridged[age_start > 0]
pop_u5 <- rbind(copy(pop_u5[, age_start := 0]), copy(pop_u5[, age_start := 1]))
pop_u5[, pop := ifelse(age_start == 0, pop * prop0, pop * prop1to4)]
stopifnot(all.equal(sum(pop_u5$pop), start_pop))
pc20_bridged <- rbindlist(list(pop_u5, pc20_bridged), use.names = T, fill = T); rm(pop_u5, start_pop, prop0, prop1to4)

# estimate 2010-2020 intercensal population
pc20_bridged[, pop := as.numeric(pop)]  # convert pop values to numeric from character
pop_ic <- create_intercensal(pc10 = pop[year %in% 2010:2020], pc20_2020 = pc20_bridged)
pc20_22 <- rbind(pc20_bridged, pop[year >= 2021])  # save for vetting plots

## Replace 2010-2020 with intercensal estimates and format for raking -----------------------------
nchs_00_20 <- copy(pop[year %in% 2000:2020])  # save for vetting plots
pop <- pop[!year %in% 2010:2020]
pop <- rbindlist(list(pop, pop_ic), use.names = T, fill = T); rm(pop_ic, pc20, pc20_bridged, census_pop)

## collapse to mcnty, rename age_start to age, add state
pop <- merge(pop, loc[, list(fips, mcnty, state)], by = "fips", all.x = T)
setnames(pop, "age_start", "age")
pop <- pop[, list(pop = sum(pop)), by = 'state,mcnty,year,sex,age,race']
stopifnot(nrow(pop[is.na(mcnty)]) == 0)  # make sure merged counties are nonmissing

## Scale to GBD population ------------------------------------------------------------------------

# get GBD age_group_ids
source("FILEPATH/get_age_metadata.R")
age_ids <- get_age_metadata(age_group_set_id = 19, gbd_round_id = 7)  # GBD 2021
age_ids <- age_ids[, list(age_group_id, age = age_group_years_start)]
age_ids[age < 1, age := 0]
age_ids[age >= 85, age := 85]
age_ids[age == 2, age := 1]

# get GBD population
source("FILEPATH/get_population.R")
gbd <- get_population(run_id = 377, release_id = 16, age_group_id = age_ids$age_group_id, sex_id = 1:2,
                      year_id = unique(pop$year), location_id = loc2[fips < 60, location_id])

gbd <- merge(gbd, age_ids, by = "age_group_id", all = T)

stopifnot(nrow(gbd[is.na(population)]) == 0)
stopifnot(nrow(gbd[is.na(age)]) == 0)
gbd <- gbd[, list(gbd_pop = sum(population)), by = 'location_id,year_id,sex_id,age']

# map to states
gbd <- merge(gbd, loc2[, list(location_id, state = fips)], by = "location_id", all.x = T)
gbd <- gbd[, list(state, year = year_id, sex = sex_id, age, gbd_pop)]

# TEMP load and format gbd 2023 run id 390 final estimates for plots
gbd_390 <- get_population(run_id = 390, release_id = 16, age_group_id = age_ids$age_group_id, sex_id = 1:2,
                          year_id = unique(pop$year), location_id = loc2[fips < 60, location_id])

gbd_390 <- merge(gbd_390, age_ids, by = "age_group_id", all = T)
stopifnot(nrow(gbd_390[is.na(population)]) == 0)
stopifnot(nrow(gbd_390[is.na(age)]) == 0)
gbd_390 <- gbd_390[, list(gbd_pop = sum(population)), by = 'location_id,year_id,sex_id,age']
gbd_390 <- merge(gbd_390, loc2[, list(location_id, state = fips)], by = "location_id", all.x = T)
gbd_390 <- gbd_390[, list(state, year = year_id, sex = sex_id, age, gbd_pop390 = gbd_pop)]

# rake population
pop <- merge(pop, gbd, by = c("state", "year", "sex", "age"), all = T)
pop[, rf := gbd_pop / sum(pop), by = 'state,year,sex,age']
pop[, pop_raked := pop * rf]

## Create pop by age and sex ----------------------------------------------------------------------
pop[, c("gbd_pop", "rf") := NULL]
setnames(pop, c("pop", "pop_raked"), c("pop_orig", "pop"))  # keep unraked pop for plots
setcolorder(pop, c("mcnty", "year", "sex", "age", "race", "pop"))
setkeyv(pop, c("mcnty", "year", "sex", "age", "race", "pop"))

pop_age_sex <- copy(pop)
pop_age_sex <- pop_age_sex[, list(pop = sum(pop), pop_orig = sum(pop_orig)), by = 'mcnty,state,year,sex,age']

## Save the output --------------------------------------------------------------------------------

# recode race IDs to match those in shared DB
pop[, race := car::recode(race, "1=5; 2=4; 3=6; 4=7; 7=2; else=NA")]
stopifnot(nrow(pop[is.na(race)]) == 0)

# assign race set ID from ushd_shared.population_group_set table
pop[, race_set := pop_group_sets[population_group_set_name == "omb_1977", population_group_set_id]]

# name output file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
saveRDS(pop, paste0(pop_dir, "FILEPATH", date_time_stamp, ".rds"))
saveRDS(pop_age_sex, paste0(pop_dir, "FILEPATH", date_time_stamp, ".rds"))