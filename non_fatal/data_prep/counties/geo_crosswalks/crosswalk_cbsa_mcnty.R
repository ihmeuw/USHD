###########################################################################################################################################
## Description: Prepare combined CBSA/merged county units
##
## Output:     New composite geographic units representing the smallest possible units that correspond to both CBSA and mcnty boundaries.
##
###########################################################################################################################################

######## 1. Setup
#### Load needed packages
pacman::p_load(data.table, ggplot2, rgdal, rgeos, raster, zoo)

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
(timestamp <- make_time_stamp())
output_dir <- paste0("FILEPATH")

dir.create(output_dir)

#### Read in data
locs <- fread("FILEPATH")
locs[, cnty := sprintf("%05d", cnty)]

pops_fips <- readRDS("FILEPATH")

cbsa_mcnty <- readRDS("FILEPATH")
ps_path <- "FILEPATH"
ps_path_under20 <- "FILEPATH"

####  Load and process microdata
## Set paths
data_version <- "NUMBER"
if (data_version != sub(".*/", "", system("readlink -f FILEPATH", intern = TRUE))) {
  warning("The requested data version is not the most recent version.")
}
input_dir <- paste0("FILEPATH")
data_combined <- readRDS(file = paste0(input_dir, "brfss_microdata.rds"))

## Merge state names
data_combined <- merge(data_combined, unique(locs[, list(state, state_name)]), by = "state", all.x = TRUE)

## Restrict to 2000-2019
data_combined <- data_combined[year %in% 2000:2019]


######## 2. Data prep
#### Output list of years, start_year, etc.
unique(cbsa_mcnty[order(year), list(year, start_year, start_month, end_year, end_month)])

#### Set a version field
cbsa_mcnty[year == 2018 & start_year == 2018 & start_month == 4, version := "2018a"]
cbsa_mcnty[year == 2018 & start_year == 2018 & start_month == 9, version := "2018b"]
cbsa_mcnty[is.na(version), version := as.character(year)]

#### Restrict to pre-2019
cbsa_mcnty <- cbsa_mcnty[!(version %in% c("2020"))]

#### Output list of years, start_year, etc., with version
unique(cbsa_mcnty[order(version), list(version, year, start_year, start_month, end_year, end_month)])

#### Fix missing mcnty and state for cnty 51515
cbsa_mcnty[cnty == 51515, c("mcnty", "state") := list(2916, 51)]

# check for mismatches between county code and state in CBSA cw
mismatch <- cbsa_mcnty[as.numeric(substr(cnty, 1, ifelse(floor(log10(cnty)) + 1 == 5, 2, 1))) != state]
if (nrow(mismatch) > 0) warning("some county and state codes in CBSA cw are inconsistent")

#### Check for mismatches between state in data_combined and CBSA cw
## Merge state from cbsa_mcnty onto data_combined
data_combined$cbsa <- as.integer(data_combined$cbsa)
setkeyv(data_combined, c("state", "cbsa"))
data_combined$uid <- 1:nrow(data_combined)
state_check <- data_combined[unique(cbsa_mcnty[, list(state, cbsa)])]
missing <- data_combined[!(uid %in% state_check$uid) & !is.na(cbsa)]

## Drop CBSA identifiers for these rows
data_combined[uid %in% missing$uid, cbsa := NA]

## Check again
setkeyv(data_combined, c("state", "cbsa"))
state_check <- data_combined[unique(cbsa_mcnty[, list(state, cbsa)])]
missing <- data_combined[!(uid %in% state_check$uid) & !is.na(cbsa)]

stopifnot(nrow(missing) == 0)

#### Make combos for generating square data
## All mcntys should have a row for each version, so we make the data square
combos <- as.data.table(expand.grid(mcnty = unique(locs$mcnty), version = c("pre-2013", "2013", "2015", "2017", "2018a", "2018b")))
combos <- merge(combos, unique(locs[, c("mcnty", "state")]), by = "mcnty", all = TRUE)
cbsa_mcnty <- merge(cbsa_mcnty, combos, by = c("mcnty", "version"), all = TRUE)
cbsa_mcnty[is.na(state.x) & !is.na(state.y), state.x := state.y]
cbsa_mcnty[, c("state", "state.x", "state.y") := list(state.x, NULL, NULL)]
rm(combos)

#### Deal with missingness in year variable
## First drop rows with NA versions (these are the original rows for mcnties that aren't ever present in CBSAs)
cbsa_mcnty <- cbsa_mcnty[!is.na(version)]

## Now set missing year by version
cbsa_mcnty[is.na(year) & version == "2013", year := 2013]
cbsa_mcnty[is.na(year) & version == "2015", year := 2015]
cbsa_mcnty[is.na(year) & version == "2017", year := 2017]
cbsa_mcnty[is.na(year) & version %in% c("2018a", "2018b"), year := 2018]
cbsa_mcnty[is.na(year) & version == "pre-2013", year := 2012]

# Determine which years are entirely missing and replicate those rows
missing_years <- unique(data_combined$year)[which(!(unique(data_combined$year) %in% unique(cbsa_mcnty$year)))]
append_rows_2014 <- cbsa_mcnty[version == "2013"]
append_rows_2014[, year := 2014]
append_rows_2016 <- cbsa_mcnty[version == "2015"]
append_rows_2016[, year := 2016]
append_rows_2019 <- cbsa_mcnty[version == "2018b"]
append_rows_2019[, year := 2019]

# Now also replicate partial year-versions
append_rows_2015_version_2013 <- cbsa_mcnty[version == "2013"]
append_rows_2015_version_2013[, year := 2015]
append_rows_2017_version_2015 <- cbsa_mcnty[version == "2015"]
append_rows_2017_version_2015[, year := 2017]
append_rows_2018_version_2017 <- cbsa_mcnty[version == "2017"]
append_rows_2018_version_2017[, year := 2018]

# Append to existing data set
cbsa_mcnty <- rbindlist(list(cbsa_mcnty, append_rows_2014, append_rows_2016, append_rows_2019, append_rows_2015_version_2013, append_rows_2017_version_2015, append_rows_2018_version_2017), use.names = TRUE, fill = TRUE)

# Now replicate pre-2013 rows, starting with year 2000
for (yr in 2000:2011) {
  temp <- cbsa_mcnty[year == 2012]
  temp[, year := yr]
  cbsa_mcnty <- rbindlist(list(cbsa_mcnty, temp), use.names = TRUE, fill = TRUE)
}

#### Output list of years, start_year, etc., with version
unique(cbsa_mcnty[order(version), list(version, year, start_year, start_month, end_year, end_month)])

#### Set CBSA version in microdata
# Set according to year and month
data_combined[(year %in% 2013:2014) | (year == 2015 & month <= 6), version := "2013"]
data_combined[(year == 2015 & month >= 7) | (year == 2016) | (year == 2017 & month <= 7), version := "2015"]
## All CBSA data for 2018 were recoded by BRFSS to use the September 2018 version of the CBSA codes
data_combined[(year == 2017 & month >= 8), version := "2017"]
data_combined[(year %in% 2018:2019), version := "2018b"]
## Set version for pre-2013 data
data_combined[(year < 2013), version := "pre-2013"]

#### Identify CBSA-mcnty-years present in microdata
cbsa_mcnty_years_present <- unique(data_combined[, list(cbsa, mcnty, state, year, version)])
cbsa_cnty_years_present <- unique(data_combined[, list(cbsa, cnty, mcnty, state, year, version)])

## Set a flag for presence in data
cbsa_mcnty_years_present$present_in_data <- 1
cbsa_mcnty_years_present[, cbsa := as.integer(cbsa)]
cbsa_cnty_years_present$present_in_data <- 1
cbsa_cnty_years_present[, cbsa := as.integer(cbsa)]

## Check for mcnties for which we only have data from some child counties
pops_fips[, fips_pop := sum(pop), by = c("fips", "year")]
pops_fips[, mcnty_pop := sum(pop), by = c("mcnty", "year")]
pops_fips[, fips_mcnty_pop_prop := fips_pop / mcnty_pop]
merged_fips <- merge(unique(pops_fips[year >= 2000, c("fips", "year", "mcnty", "mcnty_pop", "fips_pop", "fips_mcnty_pop_prop")]), unique(cbsa_cnty_years_present[!is.na(cnty) & !is.na(mcnty), c("cnty", "year", "present_in_data")]), by.x = c("fips", "year"), by.y = c("cnty", "year"), all = TRUE)
merged_fips[is.na(present_in_data), present_in_data := 0]
merged_fips[, cnty_count := .N, by = c("mcnty", "year")]
merged_fips[, data_present_count := sum(present_in_data), by = c("mcnty", "year")]
partials <- unique(merged_fips[data_present_count < cnty_count & data_present_count > 0])
setkeyv(partials, c("fips", "year"))
setkeyv(cbsa_mcnty, c("cnty", "year"))
setkeyv(merged_fips, c("fips", "year"))
locs$cnty_int <- as.integer(locs$cnty)
merged_fips2 <- merge(merged_fips[partials], unique(locs[, c("cnty_int", "cnty_name")]), by.x = c("fips"), by.y = c("cnty_int"), all.x = TRUE)

## Dissolve most partial mcnties into state remainders by dropping county identifiers
# This is based on assessment of the counties identified above.
# First, remove mcnties 292 and 534, as they are fine to leave as is.
merged_fips2 <- merged_fips2[!(mcnty %in% c(292, 534))]
merged_fips2_unique <- unique(merged_fips2[, list(mcnty, year)])
for (i in 1:nrow(merged_fips2_unique)) {
  cbsa_mcnty_years_present[year == merged_fips2_unique[i, year] & mcnty == merged_fips2_unique[i, mcnty], present_in_data := 0]  
}

## Merge onto cbsa_mcnty
cbsa_mcnty <- merge(cbsa_mcnty, unique(cbsa_mcnty_years_present[!is.na(mcnty), -c("cbsa")]), by = c("mcnty", "year", "version"), all.x = TRUE)
setnames(cbsa_mcnty, "present_in_data", "present_in_data_mcnty")
cbsa_mcnty[is.na(present_in_data_mcnty), present_in_data_mcnty := 0]
cbsa_mcnty[, c("state", "state.x", "state.y") := list(state.x, NULL, NULL)]
cbsa_mcnty <- merge(cbsa_mcnty, unique(cbsa_mcnty_years_present[!is.na(cbsa) & is.na(mcnty), -c("mcnty")]), by = c("cbsa", "state", "year", "version"), all.x = TRUE)
setnames(cbsa_mcnty, "present_in_data", "present_in_data_cbsa")
cbsa_mcnty[is.na(present_in_data_cbsa), present_in_data_cbsa := 0]

## Check for strata where data are present at multiple levels
multiple_levels <- cbsa_mcnty[present_in_data_mcnty + present_in_data_cbsa > 1]
warning(paste0(nrow(multiple_levels), " rows in cbsa_mcnty indicate data present at multiple levels. Check these."))

## Set an overall present_in_data flag
cbsa_mcnty[, present_in_data := as.integer(present_in_data_mcnty + present_in_data_cbsa > 0)]

#### Drop 2018 data for versions other than 2018b
cbsa_mcnty <- cbsa_mcnty[!(year == 2018 & version %in% c("2017", "2018a"))]

#### Check for mcnties that fall in multiple CBSAs, only some of which we have data from
## Summarize CBSA-mcnty coverage
coverage <- cbsa_mcnty[, list("present_in_data_mcnty" = sum(present_in_data_mcnty), "present_in_data_cbsa" = sum(present_in_data_cbsa), "present_in_data" = sum(present_in_data), N = .N), by = c("mcnty", "year", "version", "is_metropolitan_division")]

#### Establish fake codes for state remainder "CBSAs"; these are represented as {STATEFIP}{999}
## First, check that no existing CBSA codes contain 999
nrow(cbsa_mcnty[grepl("999", cbsa)])

## Copy original CBSA code
cbsa_mcnty[, cbsa_original := cbsa]

## Now set fake CBSA codes for state remainders
cbsa_mcnty[present_in_data == 0, cbsa := as.integer(paste0(sprintf("%02d", state), "999"))]

## Now set fake CBSA codes for mcnty-only-level rows; these fake CBSA codes are set to be the negative of the mcnty code
cbsa_mcnty[is.na(cbsa) & present_in_data_mcnty == 1 & present_in_data_cbsa == 0, cbsa := -mcnty]
nrow(cbsa_mcnty[is.na(cbsa)]) # Should be zero


######## 3. Collapse CBSAs and mcntys to a set of spatial units in which CBSAs and mcntys nest
#### Drop rows with NA version or NA mcnty
cbsa_mcnty <- cbsa_mcnty[!is.na(version) & !is.na(mcnty)]

#### Set NA metropolitan division to 0
cbsa_mcnty[is.na(is_metropolitan_division), is_metropolitan_division := 0]

#### Simplify table to only needed columns
cbsa_mcnty_original <- copy(cbsa_mcnty)
cbsa_mcnty <- unique(cbsa_mcnty[, list(mcnty, state, cbsa, version, year, is_metropolitan_division)])

#### Loop through CBSAs and set cbsa_mcnty_code
## First process metropolitan divisions
cbsa_mcnty_temp <- cbsa_mcnty[is_metropolitan_division == 1]

# Create summary table
county_count <- cbsa_mcnty_temp[, .N, by = c("cbsa", "state", "version", "year")][order(cbsa, state, version, year)]

message("Processing metropolitan divisions...")
loc_index <- 0
for (i in 1:nrow(county_count)) {
  current_cbsa <- county_count[i, cbsa]
  current_version <- county_count[i, version]
  current_year <- county_count[i, year]
  current_state <- county_count[i, state]
  
  print(paste0(i, " of ", nrow(county_count), ": CBSA ", current_cbsa, ", state ", current_state, ", version ", current_version, ", year ", current_year))
  # for a CBSA version and year (row in county_count), identify mcnties in the CBSA, CBSAs that overlap with those mcnties, and mcnties that over lap with those CBSAs...i.e., find overlapping layers
  cbsa_mcnty_temp[version == current_version & 
                    year == current_year & 
                    state == current_state & 
                    mcnty %in% cbsa_mcnty_temp[version == current_version & 
                                                 year == current_year & 
                                                 state == current_state & 
                                                 cbsa %in% cbsa_mcnty_temp[version == current_version & 
                                                                           year == current_year & 
                                                                           state == current_state & 
                                                                           mcnty %in% cbsa_mcnty_temp[version == current_version & 
                                                                                                        year == current_year & 
                                                                                                        state == current_state & 
                                                                                                        cbsa %in% current_cbsa, 
                                                                                                      mcnty],
                                                                           cbsa], 
                                               mcnty], 
                  cbsa_mcnty_code := loc_index]
  loc_index <- loc_index + 1
}

## Then process non-metropolitan divisions
cbsa_mcnty_temp2 <- cbsa_mcnty[is_metropolitan_division == 0]

# Create summary table
county_count <- cbsa_mcnty_temp2[, .N, by = c("cbsa", "state", "version", "year")][order(cbsa, state, version, year)]

message("Processing non-metropolitan division data...")
for (i in 1:nrow(county_count)) {
  current_cbsa <- county_count[i, cbsa]
  current_version <- county_count[i, version]
  current_year <- county_count[i, year]
  current_state <- county_count[i, state]
  
  print(paste0(i, " of ", nrow(county_count), ": CBSA ", current_cbsa, ", state ", current_state, ", version ", current_version, ", year ", current_year))
  
  cbsa_mcnty_temp2[version == current_version & year == current_year & state == current_state & mcnty %in% cbsa_mcnty_temp2[version == current_version & year == current_year & state == current_state & cbsa %in% cbsa_mcnty_temp2[version == current_version & year == current_year & state == current_state & mcnty %in% cbsa_mcnty_temp2[version == current_version & year == current_year & state == current_state & cbsa %in% current_cbsa, mcnty], cbsa], mcnty], cbsa_mcnty_code := loc_index]
  loc_index <- loc_index + 1
}

#### Combine metro divisions and remaining data
cbsa_mcnty <- rbindlist(list(cbsa_mcnty_temp, cbsa_mcnty_temp2), use.names = TRUE, fill = TRUE)

#### Check outputs
stopifnot(cbsa_mcnty[is.na(cbsa_mcnty_code)] == 0) # Should be 0
uniqueN(cbsa_mcnty$cbsa_mcnty_code) # 44642
uniqueN(cbsa_mcnty$mcnty) # Should be 3110

#### Evaluate outputs
cbsas_simplified_1 <- unique(cbsa_mcnty[, list(cbsa_mcnty_code, mcnty, state, version, year, is_metropolitan_division)])
cbsas_simplified_county_count_1 <- cbsas_simplified_1[, .N, by = list(cbsa_mcnty_code, state, version, year, is_metropolitan_division)][order(cbsa_mcnty_code)] # Values > 1 are okay
cbsas_simplified_cbsa_count_1 <- cbsas_simplified_1[, .N, by = list(mcnty, state, version, year, is_metropolitan_division)][order(mcnty)] # Values > 1 are NOT okay [there aren't any]
stopifnot(nrow(cbsas_simplified_cbsa_count_1[N > 1]) == 0)

cbsas_simplified_2 <- unique(cbsa_mcnty[, list(cbsa, mcnty, state, version, year, is_metropolitan_division)])
cbsas_simplified_county_count_2 <- cbsas_simplified_2[, .N, by = list(cbsa, state, version, year, is_metropolitan_division)][order(version)] # Values > 1 are okay
cbsas_simplified_cbsa_count_2 <- cbsas_simplified_2[, .N, by = list(mcnty, state, version, year, is_metropolitan_division)][order(mcnty)] # Values > 1 are okay

cbsas_simplified_3 <- unique(cbsa_mcnty[, list(cbsa, state, cbsa_mcnty_code, version, year, is_metropolitan_division)])
cbsas_simplified_cbsa_count_3 <- cbsas_simplified_3[, .N, by = list(cbsa_mcnty_code, state, version, year, is_metropolitan_division)][order(cbsa_mcnty_code)] # Values > 1 are okay
cbsas_simplified_county_count_3 <- cbsas_simplified_3[, .N, by = list(cbsa, state, version, year, is_metropolitan_division)][order(cbsa)] # Values > 1 are NOT okay [there aren't any]
stopifnot(nrow(cbsas_simplified_county_count_3[N > 1]) == 0)

#### Merge back onto original data set
cbsa_mcnty <- merge(cbsa_mcnty_original, cbsa_mcnty, by = c("mcnty", "cbsa", "state", "version", "year", "is_metropolitan_division"), all = TRUE)

stopifnot(nrow(cbsa_mcnty[is.na(cbsa_mcnty_code)]) == 0)

## Check existing combinations
unique(cbsa_mcnty[order(version), list(year, version)])

#### Collapse cbsa_mcnty_codes with identical geographic (mcnty) coverage
cbsa_mcnty[, "mcnty_string" := paste0(sort(unique(mcnty)), collapse = "_"), by = "cbsa_mcnty_code"]
cbsa_mcnty[, mcnty_string := paste0(mcnty_string, "_version_", version, "_is_metropolitan_division_", is_metropolitan_division)]
unique_mcnty_combos <- data.table("mcnty_string" = unique(cbsa_mcnty$mcnty_string))
unique_mcnty_combos$new_code <- 0:(nrow(unique_mcnty_combos) - 1)
cbsa_mcnty <- merge(cbsa_mcnty, unique_mcnty_combos, by = "mcnty_string", all = TRUE)
cbsa_mcnty[, c("cbsa_mcnty_code", "cbsa_mcnty_code_old", "new_code") := list(new_code, cbsa_mcnty_code, NULL)]

#### Save full data set
cbsa_mcnty_full <- copy(cbsa_mcnty)
saveRDS(cbsa_mcnty_full, paste0(output_dir, "cbsa_mcnty_full.rds"))


######## 4. Add population data
#### Simplify data set to required fields
cbsa_mcnty <- unique(cbsa_mcnty[, list(state, mcnty, cbsa, cbsa_mcnty_code, year, version, is_metropolitan_division)])

#### Load post-stratification frame
ps_frame <- rbindlist(list(readRDS(ps_path_under20), readRDS(ps_path)), use.names = TRUE, fill = TRUE)

#### Merge post-stratification frame onto cbsa objects
## Use allow.cartesian = TRUE to replicate rows by sex, age, race, edu, and marital
cbsa_pops <- merge(cbsa_mcnty, ps_frame[year %in% unique(cbsa_mcnty$year), c("marital", "edu", "race", "age", "sex", "year", "mcnty", "value", "value_age_pooled", "state_name")], by = c("mcnty", "year"), all = TRUE, allow.cartesian = TRUE)

#### Check cbsa_pops
setnames(cbsa_pops, "value", "pop")
nrow(cbsa_pops[is.na(cbsa_mcnty_code)]) # 0
nrow(cbsa_pops[is.na(pop)]) # 0

#### Calculate total pops and population proportions
## Sum pops across mcntys in a given cbsa_mcnty, by year/version/sex/age/race
cbsa_pops[, total_pop := sum(pop), by = list(cbsa_mcnty_code, state, year, version, sex, age, race, edu, marital)]

## Calculate aggregation weights
cbsa_pops[, agg_wt := pop / total_pop]

#### Create unique ID
cbsa_pops[, id := .I]

#### Check that there are no duplicated rows
first <- cbsa_pops[, head(.SD, 1), by = list(state, mcnty, cbsa, cbsa_mcnty_code, year, version, is_metropolitan_division, sex, age, race, edu, marital, pop, total_pop, agg_wt)]
stopifnot(nrow(cbsa_pops[!(id %in% first$id)]) == 0)

## Check existing combinations, again
unique(cbsa_pops[order(year), list(year, version)])

## Save pre-imputation cbsa_pops and basic mapping
saveRDS(unique(cbsa_pops[, list(state, mcnty, cbsa, cbsa_mcnty_code, year, version, is_metropolitan_division)]), paste0(output_dir, "cbsa_mcnty_mapping.rds"))
saveRDS(cbsa_pops, paste0(output_dir, "cbsa_mcnty_pre_wt_imputation.rds"))


######## 6. Final clean-up
###### Reduce geo crosswalk to unique rows, after dropping CBSA
cbsa_pops <- unique(cbsa_pops[, -c("cbsa", "id")])

###### Recalculate total_pop and agg_wt
cbsa_pops[, total_pop := sum(pop), by = c("cbsa_mcnty_code", "year", "sex", "race", "age", "edu", "marital")]
cbsa_pops[, agg_wt := pop / total_pop]

#### Set post-stratification weights for strata with zero aggregation weight, using cascade strategy
# Set raw weights
cbsa_pops[, c("pop_total", "wt0", "wt") := list(total_pop, agg_wt, agg_wt)]

# Second level of the cascade (drop year)
cbsa_pops[, pop2 := sum(pop), by = c("mcnty", "cbsa_mcnty_code", "age", "race", "sex", "edu", "marital")]
cbsa_pops[, pop_total2 := sum(pop), by = c("cbsa_mcnty_code", "age", "race", "sex", "edu", "marital")]
cbsa_pops[, wt2 := pop2 / pop_total2]

# Third level of the cascade (drop year, and use pooled age groups)
cbsa_pops[, pop3 := sum(value_age_pooled), by = c("mcnty", "cbsa_mcnty_code", "race", "sex", "edu", "marital", "age")]
cbsa_pops[, pop_total3 := sum(value_age_pooled), by = c("cbsa_mcnty_code", "sex", "race", "edu", "marital", "age")]
cbsa_pops[, wt3 := pop3 / pop_total3]

# Fourth level of the cascade (drop year and race, and use pooled age groups)
cbsa_pops[, pop4 := sum(value_age_pooled), by = c("mcnty", "cbsa_mcnty_code", "sex", "edu", "marital", "age")]
cbsa_pops[, pop_total4 := sum(value_age_pooled), by = c("cbsa_mcnty_code", "sex", "edu", "marital", "age")]
cbsa_pops[, wt4 := pop4 / pop_total4]

# Fifth level of the cascade (drop year, race, and sex)
cbsa_pops[, pop5 := sum(value_age_pooled), by = c("mcnty", "cbsa_mcnty_code", "age", "edu", "marital")]
cbsa_pops[, pop_total5 := sum(value_age_pooled), by = c("cbsa_mcnty_code", "age", "edu", "marital")]
cbsa_pops[, wt5 := pop5 / pop_total5]

# Sixth level of the cascade (drop year, race, sex, and age)
cbsa_pops[, pop6 := sum(value_age_pooled), by = c("mcnty", "cbsa_mcnty_code", "edu", "marital")]
cbsa_pops[, pop_total6 := sum(value_age_pooled), by = c("cbsa_mcnty_code", "edu", "marital")]
cbsa_pops[, wt6 := pop6 / pop_total6]

cbsa_pops[pop_total >= 20, "use_version" := 1]
cbsa_pops[pop_total < 20, c("agg_wt", "use_version") := list(wt2, 2)]
cbsa_pops[pop_total2 < 20, c("agg_wt", "use_version") := list(wt3, 3)]
cbsa_pops[pop_total3 < 20, c("agg_wt", "use_version") := list(wt4, 4)]
cbsa_pops[pop_total4 < 20, c("agg_wt", "use_version") := list(wt5, 5)]
cbsa_pops[pop_total5 < 20, c("agg_wt", "use_version") := list(wt6, 6)]

stopifnot(nrow(cbsa_pops[is.na(agg_wt)]) == 0)
stopifnot(nrow(cbsa_pops[pop_total6 == 0]) == 0)
stopifnot(cbsa_pops[, sum(agg_wt), by = c("cbsa_mcnty_code", "year", "sex", "age", "race", "edu", "marital")][, max(abs(V1 - 1))] < 1e-10)

#### Cleanup
cbsa_pops[, c("wt", "wt0", "pop_total", "value_age_pooled", "pop2", "pop_total2", "wt2", "pop3", "pop_total3", "wt3", "pop4", "pop_total4", "wt4", "pop5", "pop_total5", "wt5", "pop6", "pop_total6", "wt6") := NULL]

#### Set total_pop to tiny value (1e-12) where this is zero
cbsa_pops[total_pop == 0, total_pop := 1e-12]

#### Set population
cbsa_pops[, pop := total_pop * agg_wt]

#### Check that weights sum to 1
stopifnot(cbsa_pops[, sum(agg_wt), by = c("cbsa_mcnty_code", "year", "sex", "age", "race", "edu", "marital")][, max(abs(V1 - 1))] < 1e-10)

#### Check for any missingness
stopifnot(nrow(cbsa_pops) == nrow(cbsa_pops[complete.cases(cbsa_pops)]))


######## 7. Save outputs
saveRDS(cbsa_pops, paste0(output_dir, "cbsa_mcnty_crosswalk.rds"))

sink(paste0(output_dir, "/input_versions.txt"))
# print(paste("Population input: ", pops_loc))
print(paste("PS frame: ", ps_path))
print(paste("BRFSS data set: ", data_version))
sink()


######## 9. Produce shapefiles
#### Load shapefile
mcnty_shape <- readRDS("FILEPATH")
cbsa_mcnty_shape <- copy(mcnty_shape)

dir.create(paste0(output_dir, "/shapefiles/"))
dir.create(paste0(output_dir, "/plots/"))

#### Create cbsa_mcnty shapefiles by year and version
year_versions <- unique(cbsa_pops[, c("year", "version", "is_metropolitan_division")])[order(year)]
for (i in 1:nrow(year_versions)) {
  current <- year_versions[i]
  print(current)
  
  current_shape <- copy(cbsa_mcnty_shape)
  current_shape@data <- merge(current_shape@data, unique(cbsa_pops[year == current$year & version == current$version & is_metropolitan_division == current$is_metropolitan_division, c("mcnty", "year", "cbsa_mcnty_code", "version", "is_metropolitan_division")]), by = "mcnty", all = TRUE)
  current_shape <- aggregate(current_shape[!is.na(current_shape@data$cbsa_mcnty_code),], "cbsa_mcnty_code")
  
  saveRDS(current_shape, paste0(output_dir, "/shapefiles/cbsa_mcnty_shape_", current$year, "_version_", current$version, "_metro_", (current$is_metropolitan_division == 1), ".rds"))
  
  pdf(paste0(output_dir, "/plots/cbsa_mcnty_shape_", current$year, "_version_", current$version, "_metro_", (current$is_metropolitan_division == 1), ".pdf"), width = 11, height = 8.5)
  plot(current_shape)
  plot(mcnty_shape)
  dev.off()
}
