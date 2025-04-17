###########################################################################################################################################
## Description: Prepare combined PUMA/merged county units
##
## Input:      2000 and 2010 census PUMA:county crosswalk files.
## Output:     New composite geographic units representing the smallest possible units that correspond to both PUMA and mcnty boundaries.
##
###########################################################################################################################################

######## 1. Setup
#### Load needed packages
pacman::p_load(data.table, ggplot2, rgdal, rgeos, raster)

###### Source functions
repo <- paste0("FILEPATH")
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

## load database central functions
library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                      R.version$major,
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
ushd_client$save_covariate_population  # running this line ensures successful database connection and prevents interference from other packages


#### Set output folder
(data_version <- make_time_stamp())
output_dir <- paste0("FILEPATH")

dir.create(output_dir)

#### Read in data
pumas <- fread("FILEPATH")
locs <- fread("FILEPATH")
locs[, cnty := sprintf("%05d", cnty)]
ps_path <- "FILEPATH"
ps_path_under20 <- "FILEPATH"
ps_frame <- rbindlist(list(readRDS(ps_path), readRDS(ps_path_under20)), use.names = TRUE, fill = TRUE)


######## 2. Data prep for 2010 PUMAs
#### Summarize PUMAs
pumas <- unique(pumas[, list(STATEFP, COUNTYFP, PUMA5CE)])
pumas[, FIPS := paste0(sprintf("%02d", STATEFP), sprintf("%03d", COUNTYFP))]
pumas[, PUMA_ID := paste0(sprintf("%02d", STATEFP), sprintf("%05d", PUMA5CE))]

#### Merge mcnty
pumas <- merge(pumas, locs, by.x = c("FIPS"), by.y = c("cnty"), all = TRUE)

#### Drop territories
pumas <- pumas[!is.na(state)]

#### Deal with special cases
### See https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html
## Valdez-Cordova Census Area: Decomposed into Chugach Census Area and Copper River Census Area (2 January, 2019)
pumas[FIPS %in% c("02063", "02066"), c("STATEFP", "COUNTYFP", "PUMA5CE", "PUMA_ID") := list(2, 261, 300, "0200300")]
## Wade Hampton Census Area: Changed to Kusilvak Census Area (1 July, 2015)
pumas[FIPS %in% c("02158"), c("STATEFP", "COUNTYFP", "PUMA5CE", "PUMA_ID") := list(2, 270, 400, "0200400")]
## Shannon County: Changed to Oglala Lakota County (1 May, 2015)
pumas[FIPS %in% c("46102"), c("STATEFP", "COUNTYFP", "PUMA5CE", "PUMA_ID") := list(46, 113, 200, "4600200")]

#### Drop rows without PUMA data
pumas <- pumas[!(current == 0 & is.na(PUMA_ID))]

#### Check file for completeness
nrow(pumas[is.na(PUMA_ID)]) # Should be 0
length(unique(pumas$mcnty)) # Should be 3110
pumas[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:ncol(pumas)] # Check NAs; should all be zero

#### Create summary tables for data exploration
county_count <- pumas[, .N, by = PUMA_ID][order(PUMA_ID)]
puma_count <- pumas[, .N, by = mcnty][order(mcnty)]


######## 3. Identify PUMA-mcnty overlaps for 2010 PUMAs
loc_index <- 0
pumas$puma_version <- "2012_2019"
pumas$puma_mcnty <- NA_integer_
for (i in 1:nrow(county_count)) {
  current_puma <- county_count[i, PUMA_ID]

  if (nrow(pumas[mcnty %in% pumas[PUMA_ID %in% pumas[mcnty %in% pumas[PUMA_ID %in% pumas[mcnty %in% pumas[PUMA_ID %in% pumas[mcnty %in% pumas[PUMA_ID %in% current_puma, mcnty], PUMA_ID], mcnty], PUMA_ID], mcnty], PUMA_ID], mcnty],]) > 0) {
    pumas[mcnty %in% pumas[PUMA_ID %in% pumas[mcnty %in% pumas[PUMA_ID %in% pumas[mcnty %in% pumas[PUMA_ID %in% pumas[mcnty %in% pumas[PUMA_ID %in% current_puma, mcnty], PUMA_ID], mcnty], PUMA_ID], mcnty], PUMA_ID], mcnty], puma_mcnty := loc_index]
    loc_index <- loc_index + 1
  }
}

#### Check outputs
nrow(pumas[is.na(puma_mcnty)]) # Should be 0
length(unique(pumas$puma_mcnty)) # 976
length(unique(pumas$mcnty)) # Should be 3110

#### Create simplified table
pumas_simplified_1 <- unique(pumas[, list(puma_mcnty, mcnty)])
pumas_simplified_2 <- unique(pumas[, list(PUMA_ID, mcnty)])
pumas_simplified_3 <- unique(pumas[, list(PUMA_ID, puma_mcnty)])
pumas_simplified_county_count_1 <- pumas_simplified_1[, .N, by = list(puma_mcnty)][order(puma_mcnty)] # Values > 1 are okay
pumas_simplified_puma_count_1 <- pumas_simplified_1[, .N, by = mcnty][order(mcnty)] # Values > 1 are NOT okay [there aren't any]
stopifnot(nrow(pumas_simplified_puma_count_1[N > 1]) == 0)
pumas_simplified_county_count_2 <- pumas_simplified_2[, .N, by = list(PUMA_ID)][order(PUMA_ID)] # Values > 1 are okay
pumas_simplified_puma_count_2 <- pumas_simplified_2[, .N, by = mcnty][order(mcnty)] # Values > 1 are okay
pumas_simplified_county_count_3 <- pumas_simplified_3[, .N, by = list(PUMA_ID)][order(PUMA_ID)] # Values > 1 are NOT okay [there aren't any]
stopifnot(nrow(pumas_simplified_county_count_3[N > 1]) == 0)
pumas_simplified_puma_count_3 <- pumas_simplified_3[, .N, by = puma_mcnty][order(puma_mcnty)] # Values > 1 are okay

#### Load shapefile
mcnty_shape <- readRDS("FILEPATH")
puma_mcnty_shape <- copy(mcnty_shape)
puma_mcnty_shape@data <- merge(puma_mcnty_shape@data, pumas_simplified_1, by = "mcnty", all = TRUE)

#### Create puma_mcnty shapefile
puma_mcnty_shape <- aggregate(puma_mcnty_shape, "puma_mcnty")


######## 4. Repeat process for 2000 PUMAs
#### Load 2000 PUMA file
pumas_2000_raw <- fread("FILEPATH")

#### Drop Puerto Rico
pumas_2000_raw <- pumas_2000_raw[FIPS_state_code != 72]

#### Summarize PUMAs
pumas_2000 <- unique(pumas_2000_raw[, list(summary_level_code, FIPS_state_code, PUMA_code, FIPS_county_code, census_2000_pop)])
pumas_2000[, FIPS := paste0(sprintf("%02d", FIPS_state_code), sprintf("%03d", FIPS_county_code))]
pumas_2000[, PUMA_ID := paste0(sprintf("%02d", FIPS_state_code), sprintf("%05d", PUMA_code))]

#### Restrict to county-level rows
pumas_2000 <- pumas_2000[summary_level_code == 781]

#### Compare with raw data set (the following lines should both be TRUE)
(nrow(unique(pumas_2000_raw[, list(FIPS_state_code, PUMA_code)])) == nrow(unique(pumas_2000[, list(FIPS_state_code, PUMA_code)])))
all.equal(unique(pumas_2000_raw[, list(FIPS_state_code, PUMA_code)]), unique(pumas_2000[, list(FIPS_state_code, PUMA_code)]))

#### Compare population sums
# For Census 2000 total population, see https://www.census.gov/programs-surveys/decennial-census/decade.2000.html
(pumas_2000[, list(pop = sum(census_2000_pop))] == 281421906) # Should be TRUE

#### Create unique PUMA ID
pumas_2000[, PUMA_ID := paste0(sprintf("%02d", FIPS_state_code), sprintf("%05d", PUMA_code))]

#### Merge mcnty
pumas_2000 <- merge(pumas_2000, locs, by.x = c("FIPS"), by.y = c("cnty"), all = TRUE)

#### Deal with special cases
### See https://usa.ipums.org/usa-action/variables/PUMA#comparability_section
# "The boundaries and PUMA codes are the same for the 2000 census and the 2005-2011 ACS/PRCS samples, with one notable exception in Louisiana:
# due to population displacement following Hurricane Katrina, three PUMA's (01801, 01802, and 01905) are combined into code 77777
# for the 2006-onward ACS and for all cases in the 2005-2007 ACS 3-Year file. They no longer had sufficient population to be included as
# separate entities." We will create separate PUMA-mcnty sets for 2000-2005 and 2006-2011 (the 2010 PUMAs are used in ACS starting in 2012)
# to account for these changes.
pumas_2000_2005 <- copy(pumas_2000)
pumas_2006_2011 <- copy(pumas_2000)
pumas_2006_2011[PUMA_ID %in% c(2201801, 2201802, 2201905), c("PUMA_code", "PUMA_ID") := list(77777, 2277777)]

#### Drop rows without PUMA data
pumas_2000_2005 <- pumas_2000_2005[!is.na(PUMA_ID)]
pumas_2006_2011 <- pumas_2006_2011[!is.na(PUMA_ID)]

#### Compare population sums again
(pumas_2000_2005[, list(pop = sum(census_2000_pop, na.rm = TRUE))] == 281421906) # Should be TRUE
(pumas_2006_2011[, list(pop = sum(census_2000_pop, na.rm = TRUE))] == 281421906) # Should be TRUE

#### Check file for completeness
nrow(pumas_2000_2005[is.na(PUMA_ID)]) # Should be 0
nrow(pumas_2000_2005[is.na(PUMA_ID)]) # Should be 0
length(unique(pumas_2000_2005$mcnty)) # Should be 3110
length(unique(pumas_2006_2011$mcnty)) # Should be 3110
pumas_2000_2005[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:ncol(pumas_2000_2005)] # Check NAs; should all be zero
pumas_2006_2011[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:ncol(pumas_2006_2011)] # Check NAs; should all be zero

#### Create summary tables for data exploration
county_count_2000_2005 <- pumas_2000_2005[, .N, by = PUMA_ID][order(PUMA_ID)]
county_count_2006_2011 <- pumas_2006_2011[, .N, by = PUMA_ID][order(PUMA_ID)]
puma_count_2000_2005 <- pumas_2000_2005[, .N, by = mcnty][order(mcnty)]
puma_count_2006_2011 <- pumas_2006_2011[, .N, by = mcnty][order(mcnty)]

#### Identify PUMA-mcnty overlaps
pumas_2000_2005$puma_version <- "2000_2005"
pumas_2006_2011$puma_version <- "2006_2011"
pumas_2000_2005$puma_mcnty <- NA_integer_
pumas_2006_2011$puma_mcnty <- NA_integer_

for (i in 1:nrow(county_count_2000_2005)) {
  current_puma_2000_2005 <- county_count_2000_2005[i, PUMA_ID]

  if (nrow(pumas_2000_2005[mcnty %in% pumas_2000_2005[PUMA_ID %in% pumas_2000_2005[mcnty %in% pumas_2000_2005[PUMA_ID %in% pumas_2000_2005[mcnty %in% pumas_2000_2005[PUMA_ID %in% pumas_2000_2005[mcnty %in% pumas_2000_2005[PUMA_ID %in% current_puma_2000_2005, mcnty], PUMA_ID], mcnty], PUMA_ID], mcnty], PUMA_ID], mcnty],]) > 0) {
    pumas_2000_2005[mcnty %in% pumas_2000_2005[PUMA_ID %in% pumas_2000_2005[mcnty %in% pumas_2000_2005[PUMA_ID %in% pumas_2000_2005[mcnty %in% pumas_2000_2005[PUMA_ID %in% pumas_2000_2005[mcnty %in% pumas_2000_2005[PUMA_ID %in% current_puma_2000_2005, mcnty], PUMA_ID], mcnty], PUMA_ID], mcnty], PUMA_ID], mcnty], puma_mcnty := loc_index]
    loc_index <- loc_index + 1
  }
}

for (i in 1:nrow(county_count_2006_2011)) {
  current_puma_2006_2011 <- county_count_2006_2011[i, PUMA_ID]

  if (nrow(pumas_2006_2011[mcnty %in% pumas_2006_2011[PUMA_ID %in% pumas_2006_2011[mcnty %in% pumas_2006_2011[PUMA_ID %in% pumas_2006_2011[mcnty %in% pumas_2006_2011[PUMA_ID %in% pumas_2006_2011[mcnty %in% pumas_2006_2011[PUMA_ID %in% current_puma_2006_2011, mcnty], PUMA_ID], mcnty], PUMA_ID], mcnty], PUMA_ID], mcnty],]) > 0) {
    pumas_2006_2011[mcnty %in% pumas_2006_2011[PUMA_ID %in% pumas_2006_2011[mcnty %in% pumas_2006_2011[PUMA_ID %in% pumas_2006_2011[mcnty %in% pumas_2006_2011[PUMA_ID %in% pumas_2006_2011[mcnty %in% pumas_2006_2011[PUMA_ID %in% current_puma_2006_2011, mcnty], PUMA_ID], mcnty], PUMA_ID], mcnty], PUMA_ID], mcnty], puma_mcnty := loc_index]
    loc_index <- loc_index + 1
  }
}

#### Check outputs
nrow(pumas_2000_2005[is.na(puma_mcnty)]) # Should be 0
nrow(pumas_2006_2011[is.na(puma_mcnty)]) # Should be 0
length(unique(pumas_2000_2005$puma_mcnty)) # 944
length(unique(pumas_2006_2011$puma_mcnty)) # 943
length(unique(pumas_2000_2005$mcnty)) # Should be 3110
length(unique(pumas_2006_2011$mcnty)) # Should be 3110

#### Create simplified table
pumas_simplified_2000_2005_1 <- unique(pumas_2000_2005[, list(puma_mcnty, mcnty)])
pumas_simplified_2000_2005_2 <- unique(pumas_2000_2005[, list(PUMA_ID, mcnty)])
pumas_simplified_2000_2005_3 <- unique(pumas_2000_2005[, list(PUMA_ID, puma_mcnty)])
pumas_simplified_2000_2005_county_count_1 <- pumas_simplified_2000_2005_1[, .N, by = list(puma_mcnty)][order(puma_mcnty)] # Values > 1 are okay
pumas_simplified_2000_2005_puma_count_1 <- pumas_simplified_2000_2005_1[, .N, by = mcnty][order(mcnty)] # Values > 1 are NOT okay [there aren't any]
stopifnot(nrow(pumas_simplified_2000_2005_puma_count_1[N > 1]) == 0)
pumas_simplified_2000_2005_county_count_2 <- pumas_simplified_2000_2005_2[, .N, by = list(PUMA_ID)][order(PUMA_ID)] # Values > 1 are okay
pumas_simplified_2000_2005_puma_count_2 <- pumas_simplified_2000_2005_2[, .N, by = mcnty][order(mcnty)] # Values > 1 are okay
pumas_simplified_2000_2005_county_count_3 <- pumas_simplified_2000_2005_3[, .N, by = list(PUMA_ID)][order(PUMA_ID)] # Values > 1 are NOT okay [there aren't any]
stopifnot(nrow(pumas_simplified_2000_2005_county_count_3[N > 1]) == 0)
pumas_simplified_2000_2005_puma_count_3 <- pumas_simplified_2000_2005_3[, .N, by = puma_mcnty][order(puma_mcnty)] # Values > 1 are okay

pumas_simplified_2006_2011_1 <- unique(pumas_2006_2011[, list(puma_mcnty, mcnty)])
pumas_simplified_2006_2011_2 <- unique(pumas_2006_2011[, list(PUMA_ID, mcnty)])
pumas_simplified_2006_2011_3 <- unique(pumas_2006_2011[, list(PUMA_ID, puma_mcnty)])
pumas_simplified_2006_2011_county_count_1 <- pumas_simplified_2006_2011_1[, .N, by = list(puma_mcnty)][order(puma_mcnty)] # Values > 1 are okay
pumas_simplified_2006_2011_puma_count_1 <- pumas_simplified_2006_2011_1[, .N, by = mcnty][order(mcnty)] # Values > 1 are NOT okay [there aren't any]
stopifnot(nrow(pumas_simplified_2006_2011_puma_count_1[N > 1]) == 0)
pumas_simplified_2006_2011_county_count_2 <- pumas_simplified_2006_2011_2[, .N, by = list(PUMA_ID)][order(PUMA_ID)] # Values > 1 are okay
pumas_simplified_2006_2011_puma_count_2 <- pumas_simplified_2006_2011_2[, .N, by = mcnty][order(mcnty)] # Values > 1 are okay
pumas_simplified_2006_2011_county_count_3 <- pumas_simplified_2006_2011_3[, .N, by = list(PUMA_ID)][order(PUMA_ID)] # Values > 1 are NOT okay [there aren't any]
stopifnot(nrow(pumas_simplified_2006_2011_county_count_3[N > 1]) == 0)
pumas_simplified_2006_2011_puma_count_3 <- pumas_simplified_2006_2011_3[, .N, by = puma_mcnty][order(puma_mcnty)] # Values > 1 are okay

#### Load shapefile
mcnty_shape <- readRDS("FILEPATH")
puma_mcnty_shape_2000_2005 <- copy(mcnty_shape)
puma_mcnty_shape_2006_2011 <- copy(mcnty_shape)
puma_mcnty_shape_2000_2005@data <- merge(puma_mcnty_shape_2000_2005@data, pumas_simplified_2000_2005_1, by = "mcnty", all = TRUE)
puma_mcnty_shape_2006_2011@data <- merge(puma_mcnty_shape_2006_2011@data, pumas_simplified_2006_2011_1, by = "mcnty", all = TRUE)

#### Create puma_mcnty shapefile
puma_mcnty_shape_2000_2005 <- aggregate(puma_mcnty_shape_2000_2005, "puma_mcnty")
puma_mcnty_shape_2006_2011 <- aggregate(puma_mcnty_shape_2006_2011, "puma_mcnty")

#### Create mapping
pumas_mapping <- merge(unique(pumas[, c("PUMA_ID", "mcnty", "puma_mcnty", "puma_version", "state")]), unique(ps_frame[year %in% 2012:2019, c("year", "mcnty")]), by = "mcnty", all = TRUE, allow.cartesian = TRUE)
pumas_mapping_2000_2005 <- merge(unique(pumas_2000_2005[, c("PUMA_ID", "mcnty", "puma_mcnty", "puma_version", "state")]), unique(ps_frame[year %in% 2000:2005, c("year", "mcnty")]), by = "mcnty", all = TRUE, allow.cartesian = TRUE)
pumas_mapping_2006_2011 <- merge(unique(pumas_2006_2011[, c("PUMA_ID", "mcnty", "puma_mcnty", "puma_version", "state")]), unique(ps_frame[year %in% 2006:2011, c("year", "mcnty")]), by = "mcnty", all = TRUE, allow.cartesian = TRUE)

pumas_combined_mapping <- rbindlist(list(pumas_mapping, pumas_mapping_2000_2005, pumas_mapping_2006_2011), use.names = TRUE, fill = TRUE)


######## 5. Add population data
#### Merge post-stratification frame onto puma objects
## Use allow.cartesian = TRUE to replicate rows by year, sex, age, race, edu, and marital
pumas_pops <- merge(unique(pumas[, c("mcnty", "puma_mcnty", "puma_version", "state")]), ps_frame[year %in% 2012:2019, c("marital", "edu", "race", "age", "sex", "year", "mcnty", "value", "value_age_pooled")], by = "mcnty", all = TRUE, allow.cartesian = TRUE)
pumas_pops_2000_2005 <- merge(unique(pumas_2000_2005[, c("mcnty", "puma_mcnty", "puma_version", "state")]), ps_frame[year %in% 2000:2005, c("marital", "edu", "race", "age", "sex", "year", "mcnty", "value", "value_age_pooled")], by = "mcnty", all = TRUE, allow.cartesian = TRUE)
pumas_pops_2006_2011 <- merge(unique(pumas_2006_2011[, c("mcnty", "puma_mcnty", "puma_version", "state")]), ps_frame[year %in% 2006:2011, c("marital", "edu", "race", "age", "sex", "year", "mcnty", "value", "value_age_pooled")], by = "mcnty", all = TRUE, allow.cartesian = TRUE)

#### Combine versions
pumas_combined <- rbindlist(list(pumas_pops, pumas_pops_2000_2005, pumas_pops_2006_2011), use.names = TRUE, fill = TRUE)
pumas_combined <- pumas_combined[year %in% 2000:2019] # Restrict to years with PS frame
setnames(pumas_combined, c("value", "value_age_pooled"), c("pop", "pop_age_pooled"))

#### Calculate total pops and population proportions (mcnties within puma-mcnties)
pumas_combined[, c("total_pop", "total_pop_age_pooled") := list(sum(pop), sum(pop_age_pooled)), by = list(puma_mcnty, puma_version, year, sex, age, race, edu, marital)]

## Calculate aggregation weights
pumas_combined[, agg_wt := pop / total_pop]

pumas_combined_simplified <- unique(pumas_combined[, list(puma_version, puma_mcnty, mcnty, year, sex, age, race, edu, marital, pop, total_pop, pop_age_pooled, agg_wt)])

stopifnot(pumas_combined_simplified[, sum(agg_wt), by = c("puma_mcnty", "puma_version", "year", "sex", "age", "race", "edu", "marital")][, max(abs(V1 - 1), na.rm = TRUE)] < 1e-10)


#### Set post-stratification weights for strata with zero aggregation weight, using cascade strategy
# Set raw weights
pumas_combined_simplified[, c("wt0", "wt") := list(agg_wt, agg_wt)]

# Second level of the cascade (drop year)
pumas_combined_simplified[, pop2 := sum(pop), by = c("mcnty", "puma_mcnty", "puma_version", "age", "race", "sex", "edu", "marital")]
pumas_combined_simplified[, puma_mcnty_pop2 := sum(pop), by = c("puma_mcnty", "puma_version", "age", "race", "sex", "edu", "marital")]
pumas_combined_simplified[, wt2 := pop2 / puma_mcnty_pop2]

# Third level of the cascade (drop year, and use pooled age groups)
# Pooled age groups are created as part of the PS frame creation.
pumas_combined_simplified[, pop3 := sum(pop_age_pooled), by = c("mcnty", "puma_mcnty", "puma_version", "race", "sex", "edu", "marital", "age")]
pumas_combined_simplified[, puma_mcnty_pop3 := sum(pop_age_pooled), by = c("puma_mcnty", "puma_version", "sex", "race", "edu", "marital", "age")]
pumas_combined_simplified[, wt3 := pop3 / puma_mcnty_pop3]

# Fourth level of the cascade (drop year and race, and use pooled age groups)
pumas_combined_simplified[, pop4 := sum(pop_age_pooled), by = c("mcnty", "puma_mcnty", "puma_version", "sex", "edu", "marital", "age")]
pumas_combined_simplified[, puma_mcnty_pop4 := sum(pop_age_pooled), by = c("puma_mcnty", "puma_version", "sex", "edu", "marital", "age")]
pumas_combined_simplified[, wt4 := pop4 / puma_mcnty_pop4]

# Fifth level of the cascade (drop year, race, and sex)
pumas_combined_simplified[, pop5 := sum(pop_age_pooled), by = c("mcnty", "puma_mcnty", "puma_version", "age", "edu", "marital")]
pumas_combined_simplified[, puma_mcnty_pop5 := sum(pop_age_pooled), by = c("puma_mcnty", "puma_version", "age", "edu", "marital")]
pumas_combined_simplified[, wt5 := pop5 / puma_mcnty_pop5]

# Sixth level of the cascade (drop year, race, sex, and age)
pumas_combined_simplified[, pop6 := sum(pop_age_pooled), by = c("mcnty", "puma_mcnty", "puma_version", "edu", "marital")]
pumas_combined_simplified[, puma_mcnty_pop6 := sum(pop_age_pooled), by = c("puma_mcnty", "puma_version", "edu", "marital")]
pumas_combined_simplified[, wt6 := pop6 / puma_mcnty_pop6]

pumas_combined_simplified[total_pop >= 20, "use_version" := 1]
pumas_combined_simplified[total_pop < 20, c("agg_wt", "use_version") := list(wt2, 2)]
pumas_combined_simplified[puma_mcnty_pop2 < 20, c("agg_wt", "use_version") := list(wt3, 3)]
pumas_combined_simplified[puma_mcnty_pop3 < 20, c("agg_wt", "use_version") := list(wt4, 4)]
pumas_combined_simplified[puma_mcnty_pop4 < 20, c("agg_wt", "use_version") := list(wt5, 5)]
pumas_combined_simplified[puma_mcnty_pop5 < 20, c("agg_wt", "use_version") := list(wt6, 6)]

stopifnot(nrow(pumas_combined_simplified[is.na(agg_wt)]) == 0)
stopifnot(nrow(pumas_combined_simplified[puma_mcnty_pop6 < 20]) == 0)
stopifnot(pumas_combined_simplified[, sum(agg_wt), by = c("puma_mcnty", "puma_version", "year", "sex", "age", "race", "edu", "marital")][, max(abs(V1 - 1))] < 1e-10)

#### Cleanup
pumas_combined_simplified[, c("use_version", "wt", "wt0", "pop_age_pooled", "pop2", "puma_mcnty_pop2", "wt2", "pop3", "puma_mcnty_pop3", "wt3", "pop4", "puma_mcnty_pop4", "wt4", "pop5", "puma_mcnty_pop5", "wt5", "pop6", "puma_mcnty_pop6", "wt6") := NULL]

#### Set total_pop to tiny value (1e-12) where this is zero
pumas_combined_simplified[total_pop == 0, total_pop := 1e-12]

#### Set population
pumas_combined_simplified[, pop := total_pop * agg_wt]

#### Check that weights sum to 1
stopifnot(pumas_combined_simplified[, sum(agg_wt), by = c("puma_mcnty", "puma_version", "year", "sex", "age", "race", "edu", "marital")][, max(abs(V1 - 1))] < 1e-10)

#### Check for any missingness
stopifnot(nrow(pumas_combined_simplified) == nrow(pumas_combined_simplified[complete.cases(pumas_combined_simplified)]))



######## 6. Save outputs
saveRDS(pumas_combined, paste0(output_dir, "puma_mcnty_full.rds"))
saveRDS(pumas_combined_simplified, paste0(output_dir, "puma_mcnty_crosswalk.rds"))
saveRDS(pumas_combined_mapping, paste0(output_dir, "puma_mcnty_mapping.rds"))

saveRDS(puma_mcnty_shape, paste0(output_dir, "puma_mcnty_shape_2010.rds"))
saveRDS(puma_mcnty_shape_2000_2005, paste0(output_dir, "puma_mcnty_shape_2000_2005.rds"))
saveRDS(puma_mcnty_shape_2006_2011, paste0(output_dir, "puma_mcnty_shape_2006_2011.rds"))

sink(paste0(output_dir, "/input_versions.txt"))
print(paste("PS frame: ", ps_path))
print(paste("PS frame under-20: ", ps_path_under20))
sink()
