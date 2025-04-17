###########################################################################################################################################
## Description: Create county-year-age-sex-race/ethnicity aggregates of self-reported health data from BRFSS
##
## Input:       None
## Output:      Combined microdata files for BRFSS, with requisite demographic and general health data.
##
###########################################################################################################################################

######## 1. Setup
#### Load required packages installed in Singularity image
pacman::p_load(data.table, haven, ggplot2, stringr, car, R.utils)
pacman::p_load(rgdal, ggthemes, cowplot, mapproj, maptools, rgeos)

#### Set paths
data_version <- "VERSION"
input_dir <- paste0("FILEPATH")

cbsa_mcnty_version <- "VERSION"

## load database central functions
library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                      R.version$major,
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
ushd_client$save_covariate_population  # running this line ensures successful database connection and prevents interference from other packages

pops <- get_population_data(population_name = ..., covariate_dataset_id = ...)

###### Source functions
repo <- paste0("FILEPATH")
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

.libPaths(c("FILEPATH")) # Switch lib path to user folder (assumes R 4.1.3)
pacman::p_load(psych, digest) # Load custom-installed packages

###### Generate run date and create model output folder
run_date <- make_time_stamp()
output_dir <- paste0("FILEPATH")
message(run_date)
message(output_dir)
dir.create(output_dir)


######## 2. Load individual data set
data_combined <- readRDS(file = paste0(input_dir, "brfss_microdata.rds"))

#### Limit to 2009-2019
data_combined <- data_combined[year %in% 2009:2019]

# Now summarize missingness
#### Define function for summarizing presence of NAs in passed data set for requested variables
check_missingness <- function(dt, by_vars = NULL, sum_vars = NULL) {
  #### Count NAs
  if (is.null(sum_vars)) { # Summarize for all variables other than by_var
    if (is.null(by_vars)) { # Don't stratify results
      missing <- dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = colnames(dt)]
      prop_missing <- dt[, lapply(.SD, function(x) round(sum(is.na(x)) / .N, 3)), .SDcols = colnames(dt)]  
    } else {
      missing <- dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = which(!(colnames(dt) %in% by_vars)), by = by_vars]
      prop_missing <- dt[, lapply(.SD, function(x) round(sum(is.na(x)) / .N, 3)), .SDcols = which(!(colnames(dt) %in% by_vars)), by = by_vars] 
    }
  } else { # Summarize only for requested variables
    if (is.null(by_vars)) { # Don't stratify results
      missing <- dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = which(colnames(dt) %in% sum_vars)]
      prop_missing <- dt[, lapply(.SD, function(x) round(sum(is.na(x)) / .N, 3)), .SDcols = which(colnames(dt) %in% sum_vars)]  
    } else {
      missing <- dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = which(colnames(dt) %in% sum_vars), by = by_vars]
      prop_missing <- dt[, lapply(.SD, function(x) round(sum(is.na(x)) / .N, 3)), .SDcols = which(colnames(dt) %in% sum_vars), by = by_vars]
    }
  }
  
  #### Calculate and merge row counts
  counts <- dt[, list(N = .N), by = by_vars]
  
  if (is.null(by_vars)) {
    missing <- cbind(missing, counts)
  } else {
    missing <- merge(missing, counts, by = by_vars, all.x = TRUE)
  }
  
  #### Return missingess tabs
  return(list("count" = missing, "prop" = prop_missing))
}

#### Process race/ethnicity
setnames(data_combined, "race77", "race")

# For now, drop NH Other and NH MR w/o main individuals
data_combined[race %in% c("NH Other", "NH MR w/o main"), race := NA]

data_combined[race == "NH White", race_ushd := 5] # NH White
data_combined[race == "NH Black", race_ushd := 4] # NH Black
data_combined[race == "NH AIAN", race_ushd := 6] # NH AIAN
data_combined[race == "NH API", race_ushd := 7] # NH API
data_combined[race == "Hispanic", race_ushd := 2] # Hispanic
data_combined[, race := NULL]
setnames(data_combined, "race_ushd", "race")

#### Process education
# Set education codes
education_map <- data.table("edu_code" = 1:4, "edu" = c("less than HS", "HS grad", "some college", "college grad"))
data_combined <- merge(data_combined, education_map, by = "edu", all.x = TRUE)
data_combined[, edu := NULL]

#### Process age
## Create USHD age variables
# First, set age_continuous to imp_age_continuous where age_continuous is missing
data_combined[!is.na(age_continuous), age_bin := floor(age_continuous / 5) * 5]
# Age 85+ is collapsed into 80+ from 2014 onward, except for some individuals in some states in 2013 who were reported with age_continuous > 80; 
# we will collapse all such individuals in 2013 into 80+.
data_combined[!is.na(age_continuous) & age_continuous >= 85 & year < 2013, age_bin := 85]
data_combined[!is.na(age_continuous) & age_continuous >= 80 & year >= 2013, age_bin := 80]
data_combined <- data_combined[is.na(age_bin) | age_bin != 15] # Drop 18-19 year-olds (for nonfatal work, specifically)
data_combined[, c("age", "age_continuous") := NULL]

#### Rename weights variable
setnames(data_combined, "wt", "weights")

#### Define a severe tooth loss variable (combining categories of 6+ teeth removed and all teeth removed)
data_combined[, tooth_loss := as.integer(edentulism %in% 2:3)]
data_combined[is.na(edentulism), tooth_loss := NA]

#### Load state-cnty-mcnty crosswalk file
locs <- fread("FILEPATH")

#### Merge state name
data_combined <- merge(data_combined, unique(locs[, list(state, state_name)]), by = "state", all.x = TRUE)

#### Process marital
data_combined[marital == "current", marital_ushd := 1] # Currently married
data_combined[marital == "former", marital_ushd := 2] # Formerly married
data_combined[marital == "never", marital_ushd := 3] # Never married


######## 2.5 Add CBSA-mcnty information
#### Load CBSA-mcnty crosswalk
cbsa_mcnty <- readRDS(paste0("FILEPATH"))
cbsa_mcnty_full <- readRDS(paste0("FILEPATH"))
setnames(cbsa_mcnty, "age", "age_bin")
cbsa_mcnty$age_bin <- as.integer(as.character(cbsa_mcnty$age_bin))

#### Check for mismatches between state in data_combined and CBSA cw
## Merge state from cbsa_mcnty onto data_combined
data_combined$cbsa <- as.integer(data_combined$cbsa)
setkeyv(data_combined, c("state", "cbsa"))
data_combined$uid <- 1:nrow(data_combined)
state_check <- data_combined[unique(cbsa_mcnty_full[cbsa >= 0, list(state, cbsa)])]
missing <- data_combined[!(uid %in% state_check$uid) & !is.na(cbsa)]

## Drop CBSA identifiers for these rows
data_combined[uid %in% missing$uid, cbsa := NA]
data_combined[, uid := NULL]

#### Check that state, mcnty and CBSA are concordant
temp <- copy(data_combined)
temp_mcnty <- merge(temp[!is.na(mcnty)], unique(locs[, c("mcnty", "state")]), by = "mcnty", all.x = TRUE)
stopifnot(nrow(temp_mcnty[state.x != state.y]) == 0) # States should match between BRFSS coding and locs
stopifnot(nrow(temp_mcnty[is.na(state.x)]) == 0 & nrow(temp_mcnty[is.na(state.y)]) == 0) # State identifiers should be present for all rows
temp_cbsa <- merge(temp[is.na(mcnty) & !is.na(cbsa)], unique(cbsa_mcnty_full[, c("cbsa", "state")]), by = c("cbsa", "state"), all.x = TRUE)
stopifnot(nrow(temp_cbsa[is.na(state)]) == 0) # State identifiers should be present for all rows
rm(temp, temp_cbsa)

#### Set CBSA version
# Set according to year and month
data_combined[(year %in% 2013:2014) | (year == 2015 & month <= 6), version := "2013"]
data_combined[(year == 2015 & month >= 7) | (year == 2016) | (year == 2017 & month <= 7), version := "2015"]
## All CBSA data for 2018 were recoded by BRFSS to use the September 2018 version of the CBSA codes
data_combined[(year == 2017 & month >= 8), version := "2017"]
data_combined[(year %in% 2018:2019), version := "2018b"]
data_combined[year < 2013, version := "pre-2013"]
stopifnot(nrow(data_combined[is.na(version)]) == 0)

#### Rename unique ID
setnames(data_combined, "id", "uid")

#### Check for state-years where cbsa and mcnty are entirely missing
state_only <- merge(data_combined[is.na(cbsa) & is.na(mcnty), list(no_cbsa_no_mcnty = .N), by = c("state", "year")], data_combined[!is.na(cbsa) | !is.na(mcnty), list(cbsa_or_mcnty = .N), by = c("state", "year")], all = TRUE)
state_only <- state_only[!is.na(no_cbsa_no_mcnty) & is.na(cbsa_or_mcnty)]

#### Subset data according to geographic level
data_combined_original <- copy(data_combined)
data_state_only <- data_combined[state_only, on = c("state", "year")]
data_cbsa_only <- data_combined[!(uid %in% data_state_only$uid)]
data_cbsa_only_original <- copy(data_cbsa_only)
stopifnot(nrow(data_state_only) + nrow(data_cbsa_only) == nrow(data_combined))

#### Merge CBSA-mcnty crosswalk onto mcnty and CBSA data subsets
## Process metro and non-metro data separately
data_cbsa_only_metro_mcnty <- merge(data_cbsa_only[!is.na(mcnty)], unique(cbsa_mcnty_full[is_metropolitan_division == 1, c("mcnty", "cbsa_mcnty_code", "state", "year", "version")]), by = c("mcnty", "state", "year", "version"))
data_cbsa_only_non_metro_mcnty <- merge(data_cbsa_only[!(uid %in% data_cbsa_only_metro_mcnty$uid)], unique(cbsa_mcnty_full[is_metropolitan_division == 0, c("mcnty", "cbsa_mcnty_code", "state", "year", "version")]), by = c("mcnty", "state", "year", "version"))
remaining <- data_cbsa_only[!(uid %in% c(data_cbsa_only_metro_mcnty$uid, data_cbsa_only_non_metro_mcnty$uid))]
data_cbsa_only_metro_cbsa <- merge(remaining[!is.na(cbsa)], unique(cbsa_mcnty_full[is_metropolitan_division == 1, c("cbsa", "cbsa_mcnty_code", "state", "year", "version")]), by = c("cbsa", "state", "year", "version"))
data_cbsa_only_non_metro_cbsa <- merge(remaining[!(uid %in% data_cbsa_only_metro_cbsa$uid)], unique(cbsa_mcnty_full[is_metropolitan_division == 0, c("cbsa", "cbsa_mcnty_code", "state", "year", "version")]), by = c("cbsa", "state", "year", "version"))
remaining <- data_cbsa_only[!(uid %in% c(data_cbsa_only_metro_mcnty$uid, data_cbsa_only_non_metro_mcnty$uid, data_cbsa_only_metro_cbsa$uid, data_cbsa_only_non_metro_cbsa$uid))]

#### Establish fake codes for state remainder "CBSAs"; these are represented as {STATEFIP}{999}
## Set fake CBSA codes (only for relevant years)
remaining[, cbsa := as.integer(paste0(sprintf("%02d", state), "999"))]
remaining_metro_cbsa <- merge(remaining[!is.na(cbsa)], unique(cbsa_mcnty_full[is_metropolitan_division == 1, c("cbsa", "cbsa_mcnty_code", "state", "year", "version")]), by = c("cbsa", "state", "year", "version"))
remaining_non_metro_cbsa <- merge(remaining[!(uid %in% remaining_metro_cbsa$uid)], unique(cbsa_mcnty_full[is_metropolitan_division == 0, c("cbsa", "cbsa_mcnty_code", "state", "year", "version")]), by = c("cbsa", "state", "year", "version"))
remaining <- remaining[!(uid %in% c(remaining_metro_cbsa$uid, remaining_non_metro_cbsa$uid))]
unmatched_n <- nrow(remaining)

## Recombine data sets
# Retain rows with unknown cbsa_mcnty codes
data_cbsa_only <- rbindlist(list(data_cbsa_only_metro_mcnty, data_cbsa_only_non_metro_mcnty, data_cbsa_only_metro_cbsa, data_cbsa_only_non_metro_cbsa, remaining_metro_cbsa, remaining_non_metro_cbsa, remaining), use.names = TRUE, fill = TRUE)
stopifnot(nrow(data_cbsa_only_original) == nrow(data_cbsa_only))

#### Summarize CBSA-mcnty coverage
cbsa_mcnty_coverage <- data_cbsa_only[is.na(mcnty), list(.N), by = c("cbsa", "cbsa_mcnty_code", "state", "year", "version")]

#### Merge onto cbsa_mcnty
cbsa_mcnty_coverage_merged <- merge(cbsa_mcnty_coverage, unique(cbsa_mcnty_full[, c("cbsa", "cbsa_mcnty_code", "state", "year", "version")]), by = c("cbsa", "cbsa_mcnty_code", "state", "year", "version"), all = TRUE)

#### Set indicator for rows with zero observations
cbsa_mcnty_coverage_merged[is.na(N), missingness_indicator := 1]

#### Merge missingness indicators onto cbsa_mcnty rows with non-zero observations
coverage <- merge(cbsa_mcnty_coverage_merged[, -c("missingness_indicator")], unique(cbsa_mcnty_coverage_merged[is.na(N), c("cbsa_mcnty_code", "state", "year", "version", "missingness_indicator")]), by = c("cbsa_mcnty_code", "state", "year", "version"), all = TRUE)
problematic_cbsa_mcntys <- coverage[!is.na(N) & !is.na(cbsa) & missingness_indicator == 1]

#### Set data_combined
data_combined <- rbindlist(list(data_cbsa_only, data_state_only), use.names = TRUE, fill = TRUE)
stopifnot(nrow(data_combined_original) == nrow(data_combined))


#### Create a binary indicator indicating >= 14 days of poor health
setnames(data_combined, "poorhlth", "activity_limit_days")
data_combined[!is.na(activity_limit_days), frequent_activity_limitations := as.integer(activity_limit_days >= 14)]

#### Update conditional treatment variables (asthma)
data_combined[asthma == 0 & year %in% unique(data_combined[!is.na(asthma_now), year]), asthma_now := 0]

#### Process back or neck pain variable
data_combined[, back_neck := car::recode(hlthprb, "1 = 1; 2 = 0; else = NA")]

#### Save full data set to disk
saveRDS(data_combined, file = paste0(output_dir, "/data_full.rds"))


######## 3. Create a data set for analyzing activity_limit_days
#### Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_act_limit_race <- data_combined[(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(activity_limit_days) | is.na(edu_code) | is.na(marital_ushd) | is.na(weights)]
data_combined_act_limit_race <- data_combined[!(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(activity_limit_days) & !is.na(edu_code) & !is.na(marital_ushd) & !is.na(weights)]
if (nrow(data_missing_act_limit_race) + nrow(data_combined_act_limit_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_act_limit_race) + nrow(data_combined_act_limit_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 4. Collapse activity_limit_days by county-year-age-sex and either race/ethnicity or education
data_combined_agg_act_limit_race <- data_combined_act_limit_race[, list(frequent_activity_limitations_count = sum(frequent_activity_limitations), frequent_activity_limitations_weighted_count = weighted.mean(frequent_activity_limitations, weights) * .N, weights = sum(weights), sample_size = .N), by = list(mcnty, cbsa_mcnty_code, state, year, sex, race, age_bin, edu_code, marital_ushd)]
stopifnot(nrow(data_combined_agg_act_limit_race[is.na(frequent_activity_limitations_count)]) == 0 & nrow(data_combined_agg_act_limit_race[is.na(frequent_activity_limitations_weighted_count)]) == 0)

#### Save collapsed data sets
saveRDS(data_missing_act_limit_race, file = paste0(output_dir, "/brfss_agg_missing_data_act_limit_race.rds"))
saveRDS(data_combined_agg_act_limit_race, file = paste0(output_dir, "/brfss_agg_act_limit_race.rds"))
rm(data_combined_act_limit_race, data_missing_act_limit_race, data_combined_agg_act_limit_race)
gc()


######## 5. Create a data set for analyzing asthma
#### Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_asthma_race <- data_combined[(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(asthma) | is.na(edu_code) | is.na(marital_ushd) | is.na(weights)]
data_combined_asthma_race <- data_combined[!(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(asthma) & !is.na(edu_code) & !is.na(marital_ushd) & !is.na(weights)]
if (nrow(data_missing_asthma_race) + nrow(data_combined_asthma_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_asthma_race) + nrow(data_combined_asthma_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}
data_missing_asthma_now_race <- data_combined[(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(asthma_now) | is.na(edu_code) | is.na(marital_ushd) | is.na(weights)]
data_combined_asthma_now_race <- data_combined[!(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(asthma_now) & !is.na(edu_code) & !is.na(marital_ushd) & !is.na(weights)]
if (nrow(data_missing_asthma_now_race) + nrow(data_combined_asthma_now_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_asthma_now_race) + nrow(data_combined_asthma_now_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 6. Collapse asthma by county-year-age-sex and either race/ethnicity or education
data_combined_agg_asthma_race <- data_combined_asthma_race[, list(asthma_count = sum(asthma), asthma_weighted_count = weighted.mean(asthma, weights) * .N, weights = sum(weights), sample_size = .N), by = list(mcnty, cbsa_mcnty_code, state, year, sex, race, age_bin, edu_code, marital_ushd)]
stopifnot(nrow(data_combined_agg_asthma_race[is.na(asthma_count)]) == 0 & nrow(data_combined_agg_asthma_race[is.na(asthma_weighted_count)]) == 0)

data_combined_agg_asthma_now_race <- data_combined_asthma_now_race[, list(asthma_now_count = sum(asthma_now), asthma_now_weighted_count = weighted.mean(asthma_now, weights) * .N, weights = sum(weights), sample_size = .N), by = list(mcnty, cbsa_mcnty_code, state, year, sex, race, age_bin, edu_code, marital_ushd)]
stopifnot(nrow(data_combined_agg_asthma_now_race[is.na(asthma_now_count)]) == 0 & nrow(data_combined_agg_asthma_now_race[is.na(asthma_now_weighted_count)]) == 0)

#### Save collapsed data sets
saveRDS(data_missing_asthma_race, file = paste0(output_dir, "/brfss_agg_missing_data_asthma_race.rds"))
saveRDS(data_combined_agg_asthma_race, file = paste0(output_dir, "/brfss_agg_asthma_race.rds"))
saveRDS(data_missing_asthma_now_race, file = paste0(output_dir, "/brfss_agg_missing_data_asthma_now_race.rds"))
saveRDS(data_combined_agg_asthma_now_race, file = paste0(output_dir, "/brfss_agg_asthma_now_race.rds"))
rm(data_combined_asthma_race, data_combined_asthma_now_race, data_missing_asthma_race, data_combined_agg_asthma_race, data_missing_asthma_now_race, data_combined_agg_asthma_now_race)
gc()


######## 7. Create a data set for analyzing COPD
#### Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_copd_race <- data_combined[(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(copd) | is.na(edu_code) | is.na(marital_ushd) | is.na(weights)]
data_combined_copd_race <- data_combined[!(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(copd) & !is.na(edu_code) & !is.na(marital_ushd) & !is.na(weights)]
if (nrow(data_missing_copd_race) + nrow(data_combined_copd_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_copd_race) + nrow(data_combined_copd_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 8. Collapse COPD by county-year-age-sex and either race/ethnicity or education
data_combined_agg_copd_race <- data_combined_copd_race[, list(copd_count = sum(copd), copd_weighted_count = weighted.mean(copd, weights) * .N, weights = sum(weights), sample_size = .N), by = list(mcnty, cbsa_mcnty_code, state, year, sex, race, age_bin, edu_code, marital_ushd)]
stopifnot(nrow(data_combined_agg_copd_race[is.na(copd_count)]) == 0 & nrow(data_combined_agg_copd_race[is.na(copd_weighted_count)]) == 0)

#### Save collapsed data sets
saveRDS(data_missing_copd_race, file = paste0(output_dir, "/brfss_agg_missing_data_copd_race.rds"))
saveRDS(data_combined_agg_copd_race, file = paste0(output_dir, "/brfss_agg_copd_race.rds"))
rm(data_combined_copd_race, data_missing_copd_race, data_combined_agg_copd_race)
gc()


######## 9. Create a data set for analyzing arthritis
#### Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_arthritis_race <- data_combined[(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(arthritis) | is.na(edu_code) | is.na(marital_ushd) | is.na(weights)]
data_combined_arthritis_race <- data_combined[!(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(arthritis) & !is.na(edu_code) & !is.na(marital_ushd) & !is.na(weights)]

if (nrow(data_missing_arthritis_race) + nrow(data_combined_arthritis_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_arthritis_race) + nrow(data_combined_arthritis_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 10. Collapse arthritis by county-year-age-sex and either race/ethnicity or education
data_combined_agg_arthritis_race <- data_combined_arthritis_race[, list(arthritis_count = sum(arthritis), arthritis_weighted_count = weighted.mean(arthritis, weights) * .N, weights = sum(weights), sample_size = .N), by = list(mcnty, cbsa_mcnty_code, state, year, sex, race, age_bin, edu_code, marital_ushd)]
stopifnot(nrow(data_combined_agg_arthritis_race[is.na(arthritis_count)]) == 0 & nrow(data_combined_agg_arthritis_race[is.na(arthritis_weighted_count)]) == 0)

#### Save collapsed data sets
saveRDS(data_missing_arthritis_race, file = paste0(output_dir, "/brfss_agg_missing_data_arthritis_race.rds"))
saveRDS(data_combined_agg_arthritis_race, file = paste0(output_dir, "/brfss_agg_arthritis_race.rds"))
rm(data_combined_arthritis_race, data_missing_arthritis_race, data_combined_agg_arthritis_race)
gc()


######## 11. Create a data set for analyzing depression
#### Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_depression_race <- data_combined[(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(depression) | is.na(edu_code) | is.na(marital_ushd) | is.na(weights)]
data_combined_depression_race <- data_combined[!(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(depression) & !is.na(edu_code) & !is.na(marital_ushd) & !is.na(weights)]
if (nrow(data_missing_depression_race) + nrow(data_combined_depression_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_depression_race) + nrow(data_combined_depression_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 12. Collapse depression by county-year-age-sex and either race/ethnicity or education
data_combined_agg_depression_race <- data_combined_depression_race[, list(depression_count = sum(depression), depression_weighted_count = weighted.mean(depression, weights) * .N, weights = sum(weights), sample_size = .N), by = list(mcnty, cbsa_mcnty_code, state, year, sex, race, age_bin, edu_code, marital_ushd)]
stopifnot(nrow(data_combined_agg_depression_race[is.na(depression_count)]) == 0 & nrow(data_combined_agg_depression_race[is.na(depression_weighted_count)]) == 0)

#### Save collapsed data sets
saveRDS(data_missing_depression_race, file = paste0(output_dir, "/brfss_agg_missing_data_depression_race.rds"))
saveRDS(data_combined_agg_depression_race, file = paste0(output_dir, "/brfss_agg_depression_race.rds"))
rm(data_combined_depression_race, data_missing_depression_race, data_combined_agg_depression_race)
gc()


######## 13. Create a data set for analyzing tooth loss
#### Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_tooth_loss_race <- data_combined[(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(tooth_loss) | is.na(edu_code) | is.na(marital_ushd) | is.na(weights)]
data_combined_tooth_loss_race <- data_combined[!(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(tooth_loss) & !is.na(edu_code) & !is.na(marital_ushd) & !is.na(weights)]
if (nrow(data_missing_tooth_loss_race) + nrow(data_combined_tooth_loss_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_tooth_loss_race) + nrow(data_combined_tooth_loss_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 14. Collapse tooth loss by county-year-age-sex and either race/ethnicity or education
data_combined_agg_tooth_loss_race <- data_combined_tooth_loss_race[, list(tooth_loss_count = sum(tooth_loss), tooth_loss_weighted_count = weighted.mean(tooth_loss, weights) * .N, weights = sum(weights), sample_size = .N), by = list(mcnty, cbsa_mcnty_code, state, year, sex, race, age_bin, edu_code, marital_ushd)]
stopifnot(nrow(data_combined_agg_tooth_loss_race[is.na(tooth_loss_count)]) == 0 & nrow(data_combined_agg_tooth_loss_race[is.na(tooth_loss_weighted_count)]) == 0)

#### Save collapsed data sets
saveRDS(data_missing_tooth_loss_race, file = paste0(output_dir, "/brfss_agg_missing_data_tooth_loss_race.rds"))
saveRDS(data_combined_agg_tooth_loss_race, file = paste0(output_dir, "/brfss_agg_tooth_loss_race.rds"))
rm(data_combined_tooth_loss_race, data_missing_tooth_loss_race, data_combined_agg_tooth_loss_race)
gc()


######## 15. Create a data set for analyzing anxiety
#### Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_anxiety_race <- data_combined[(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(anxiety) | is.na(edu_code) | is.na(marital_ushd) | is.na(weights)]
data_combined_anxiety_race <- data_combined[!(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(anxiety) & !is.na(edu_code) & !is.na(marital_ushd) & !is.na(weights)]
if (nrow(data_missing_anxiety_race) + nrow(data_combined_anxiety_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_anxiety_race) + nrow(data_combined_anxiety_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 16. Collapse anxiety by county-year-age-sex and either race/ethnicity or education
data_combined_agg_anxiety_race <- data_combined_anxiety_race[, list(anxiety_count = sum(anxiety), anxiety_weighted_count = weighted.mean(anxiety, weights) * .N, weights = sum(weights), sample_size = .N), by = list(mcnty, cbsa_mcnty_code, state, year, sex, race, age_bin, edu_code, marital_ushd)]
stopifnot(nrow(data_combined_agg_anxiety_race[is.na(anxiety_count)]) == 0 & nrow(data_combined_agg_anxiety_race[is.na(anxiety_weighted_count)]) == 0)

#### Save collapsed data sets
saveRDS(data_missing_anxiety_race, file = paste0(output_dir, "/brfss_agg_missing_data_anxiety_race.rds"))
saveRDS(data_combined_agg_anxiety_race, file = paste0(output_dir, "/brfss_agg_anxiety_race.rds"))
rm(data_combined_anxiety_race, data_missing_anxiety_race, data_combined_agg_anxiety_race)
gc()


######## 17. Create a data set for analyzing diabetes
#### Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_diabetes_race <- data_combined[(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(diabetes) | is.na(edu_code) | is.na(marital_ushd) | is.na(weights)]
data_combined_diabetes_race <- data_combined[!(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(diabetes) & !is.na(edu_code) & !is.na(marital_ushd) & !is.na(weights)]
if (nrow(data_missing_diabetes_race) + nrow(data_combined_diabetes_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_diabetes_race) + nrow(data_combined_diabetes_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 18. Collapse diabetes by county-year-age-sex and either race/ethnicity or education
data_combined_agg_diabetes_race <- data_combined_diabetes_race[, list(diabetes_count = sum(diabetes), diabetes_weighted_count = weighted.mean(diabetes, weights) * .N, weights = sum(weights), sample_size = .N), by = list(mcnty, cbsa_mcnty_code, state, year, sex, race, age_bin, edu_code, marital_ushd)]
stopifnot(nrow(data_combined_agg_diabetes_race[is.na(diabetes_count)]) == 0 & nrow(data_combined_agg_diabetes_race[is.na(diabetes_weighted_count)]) == 0)

#### Save collapsed data sets
saveRDS(data_missing_diabetes_race, file = paste0(output_dir, "/brfss_agg_missing_data_diabetes_race.rds"))
saveRDS(data_combined_agg_diabetes_race, file = paste0(output_dir, "/brfss_agg_diabetes_race.rds"))
rm(data_combined_diabetes_race, data_missing_diabetes_race, data_combined_agg_diabetes_race)
gc()


#### Save input paths
sink(paste0(output_dir, "/input_versions.txt"))
print(paste("BRFSS microdata: ", input_dir))
print(paste("CBSA-mcnty crosswalk: ", cbsa_mcnty_version))
sink()


######## 19. Produce direct estimates by race/ethnicity
#### Drop rows with missingness in demographic variables
data_combined_backup <- copy(data_combined)

data_combined <- data_combined[!is.na(weights)]

#### Produce mcnty-level direct estimates
message("Produce mcnty estimates")
mcnty_estimates <- data_combined[, list(level = "mcnty", gen_health_45 = weighted.mean(gen_health_45, weights, na.rm = TRUE), 
                                        frequent_activity_limitations = weighted.mean(frequent_activity_limitations, weights, na.rm = TRUE),
                                        asthma = weighted.mean(asthma, weights, na.rm = TRUE), copd = weighted.mean(copd, weights, na.rm = TRUE), 
                                        arthritis = weighted.mean(arthritis, weights, na.rm = TRUE), depression = weighted.mean(depression, weights, na.rm = TRUE),
                                        tooth_loss = weighted.mean(tooth_loss, weights, na.rm = TRUE),
                                        asthma_now = weighted.mean(asthma_now, weights, na.rm = TRUE), anxiety = weighted.mean(anxiety, weights, na.rm = TRUE),
                                        fallinj = weighted.mean(fallinj / fall_person_years, weights, na.rm = TRUE),
                                        dementia = weighted.mean(dementia, weights, na.rm = TRUE), prostate = weighted.mean(prostate, weights, na.rm = TRUE),
                                        diabetes = weighted.mean(diabetes, weights, na.rm = TRUE), 
                                        survive_prostate = weighted.mean(survive_prostate, weights, na.rm = TRUE), survive_breast = weighted.mean(survive_breast, weights, na.rm = TRUE),
                                        weights = sum(weights, na.rm = TRUE), sample_size = .N, fall_person_years = sum(fall_person_years, na.rm = TRUE)), by = list(mcnty, state, state_name, year, sex, race, age_bin, edu_code, marital_ushd)]

#### Produce CBSA-mcnty-level direct estimates
message("Produce CBSA-mcnty estimates")
cbsa_mcnty_estimates <- data_combined[, list(level = "cbsa_mcnty", gen_health_45 = weighted.mean(gen_health_45, weights, na.rm = TRUE), 
                                        frequent_activity_limitations = weighted.mean(frequent_activity_limitations, weights, na.rm = TRUE),
                                        asthma = weighted.mean(asthma, weights, na.rm = TRUE), copd = weighted.mean(copd, weights, na.rm = TRUE), 
                                        arthritis = weighted.mean(arthritis, weights, na.rm = TRUE), depression = weighted.mean(depression, weights, na.rm = TRUE),
                                        tooth_loss = weighted.mean(tooth_loss, weights, na.rm = TRUE),
                                        asthma_now = weighted.mean(asthma_now, weights, na.rm = TRUE), anxiety = weighted.mean(anxiety, weights, na.rm = TRUE),
                                        fallinj = weighted.mean(fallinj / fall_person_years, weights, na.rm = TRUE),
                                        dementia = weighted.mean(dementia, weights, na.rm = TRUE), prostate = weighted.mean(prostate, weights, na.rm = TRUE),
                                        diabetes = weighted.mean(diabetes, weights, na.rm = TRUE), 
                                        survive_prostate = weighted.mean(survive_prostate, weights, na.rm = TRUE), survive_breast = weighted.mean(survive_breast, weights, na.rm = TRUE),
                                        weights = sum(weights, na.rm = TRUE), sample_size = .N, fall_person_years = sum(fall_person_years, na.rm = TRUE)), by = list(cbsa_mcnty_code, state, state_name, year, sex, race, age_bin, edu_code, marital_ushd)]

#### Produce state-level direct estimates
message("Produce state estimates")
state_estimates <- data_combined[, list(level = "state", gen_health_45 = weighted.mean(gen_health_45, weights, na.rm = TRUE), 
                                        frequent_activity_limitations = weighted.mean(frequent_activity_limitations, weights, na.rm = TRUE),
                                        asthma = weighted.mean(asthma, weights, na.rm = TRUE), copd = weighted.mean(copd, weights, na.rm = TRUE), 
                                        arthritis = weighted.mean(arthritis, weights, na.rm = TRUE), depression = weighted.mean(depression, weights, na.rm = TRUE),
                                        tooth_loss = weighted.mean(tooth_loss, weights, na.rm = TRUE),
                                        asthma_now = weighted.mean(asthma_now, weights, na.rm = TRUE), anxiety = weighted.mean(anxiety, weights, na.rm = TRUE),
                                        fallinj = weighted.mean(fallinj / fall_person_years, weights, na.rm = TRUE),
                                        dementia = weighted.mean(dementia, weights, na.rm = TRUE), prostate = weighted.mean(prostate, weights, na.rm = TRUE),
                                        diabetes = weighted.mean(diabetes, weights, na.rm = TRUE),
                                        survive_prostate = weighted.mean(survive_prostate, weights, na.rm = TRUE), survive_breast = weighted.mean(survive_breast, weights, na.rm = TRUE),
                                        weights = sum(weights, na.rm = TRUE), sample_size = .N, fall_person_years = sum(fall_person_years, na.rm = TRUE)), by = list(state, state_name, year, sex, race, age_bin, edu_code, marital_ushd)]

#### Load and merge population file
message("Process subnational estimates...")
pop <- copy(pops)[, -c("edu", "edu_label", "race_label")]
pop_state <- pop[, list(pop = sum(pop)), by = c("year", "sex", "age", "race", "state")]

message("Merge mcnty_estimates with pop...")
mcnty_estimates <- merge(mcnty_estimates, pop[, -c("state")], by.x = c("mcnty", "year", "sex", "race", "age_bin"), by.y = c("mcnty", "year", "sex", "race", "age"), all.x = TRUE)

message("Merge cbsa_mcnty_estimates with pop...")
cbsa_mcnty_collapsed <- cbsa_mcnty[, list(total_pop = sum(pop)), by = c("year", "cbsa_mcnty_code", "sex", "age_bin", "race")]
cbsa_mcnty_estimates <- merge(cbsa_mcnty_estimates, cbsa_mcnty_collapsed, by = c("year", "cbsa_mcnty_code", "sex", "age_bin", "race"), all.x = TRUE)
setnames(cbsa_mcnty_estimates, "total_pop", "pop")

message("Merge state_estimates with pop...")
state_estimates <- merge(state_estimates, pop_state, by.x = c("state", "year", "sex", "race", "age_bin"), by.y = c("state", "year", "sex", "race", "age"), all.x = TRUE)

#### Derive BRFSS weights proportions to scale population estimates for stratified estimates
mcnty_estimates[, pop_scaled := pop * weights / sum(weights, na.rm = TRUE), by = c("mcnty", "year", "sex", "race", "age_bin")]
cbsa_mcnty_estimates[, pop_scaled := pop * weights / sum(weights, na.rm = TRUE), by = c("cbsa_mcnty_code", "year", "sex", "race", "age_bin")]
state_estimates[, pop_scaled := pop * weights / sum(weights, na.rm = TRUE), by = c("state", "year", "sex", "race", "age_bin")]
mcnty_estimates[, c("pop", "pop_scaled") := list(pop_scaled, NULL)]
cbsa_mcnty_estimates[, c("pop", "pop_scaled") := list(pop_scaled, NULL)]
state_estimates[, c("pop", "pop_scaled") := list(pop_scaled, NULL)]

#### Produce national-level direct estimates
message("Produce national estimates")
# BRFSS weights are not nationally-representative, so we weight state-level direct estimates by population
national_estimates <- state_estimates[, list(level = "natl", gen_health_45 = weighted.mean(gen_health_45, pop, na.rm = TRUE), 
                                        frequent_activity_limitations = weighted.mean(frequent_activity_limitations, pop, na.rm = TRUE),
                                        asthma = weighted.mean(asthma, pop, na.rm = TRUE), copd = weighted.mean(copd, pop, na.rm = TRUE), 
                                        arthritis = weighted.mean(arthritis, pop, na.rm = TRUE), depression = weighted.mean(depression, pop, na.rm = TRUE),
                                        tooth_loss = weighted.mean(tooth_loss, pop, na.rm = TRUE),
                                        asthma_now = weighted.mean(asthma_now, pop, na.rm = TRUE), anxiety = weighted.mean(anxiety, pop, na.rm = TRUE),
                                        fallinj = weighted.mean(fallinj / fall_person_years, pop, na.rm = TRUE),
                                        dementia = weighted.mean(dementia, pop, na.rm = TRUE), prostate = weighted.mean(prostate, pop, na.rm = TRUE),
                                        diabetes = weighted.mean(diabetes, pop, na.rm = TRUE),
                                        survive_prostate = weighted.mean(survive_prostate, pop, na.rm = TRUE), survive_breast = weighted.mean(survive_breast, pop, na.rm = TRUE),
                                        weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE), 
                                        fall_person_years = sum(fall_person_years, na.rm = TRUE)), by = list(year, sex, race, age_bin, edu_code, marital_ushd)]

#### Combine direct estimates across levels
direct_estimates <- rbindlist(list(mcnty_estimates, cbsa_mcnty_estimates, state_estimates, national_estimates), use.names = TRUE, fill = TRUE)

#### Harmonize area identifiers
direct_estimates[level == "mcnty", area := mcnty]
direct_estimates[level == "cbsa_mcnty", area := cbsa_mcnty_code]
direct_estimates[level == "state", area := state]
direct_estimates[level == "natl", area := 1L]

#### Set weights equal to pop for national estimates
direct_estimates[level == "natl", final_weight := pop]
direct_estimates[level != "natl", final_weight := weights]

#### Aggregate across sexes
message("Aggregate across sex")
sex_agg <- direct_estimates[, list(sex = 3, gen_health_45 = weighted.mean(gen_health_45, final_weight, na.rm = TRUE), 
                                                     frequent_activity_limitations = weighted.mean(frequent_activity_limitations, final_weight, na.rm = TRUE),
                                                     asthma = weighted.mean(asthma, final_weight, na.rm = TRUE), copd = weighted.mean(copd, final_weight, na.rm = TRUE), 
                                                     arthritis = weighted.mean(arthritis, final_weight, na.rm = TRUE), depression = weighted.mean(depression, final_weight, na.rm = TRUE),
                                                     tooth_loss = weighted.mean(tooth_loss, final_weight, na.rm = TRUE),
                                                     asthma_now = weighted.mean(asthma_now, final_weight, na.rm = TRUE), anxiety = weighted.mean(anxiety, final_weight, na.rm = TRUE),
                                                     fallinj = weighted.mean(fallinj / fall_person_years, final_weight, na.rm = TRUE),
                                                     dementia = weighted.mean(dementia, final_weight, na.rm = TRUE), prostate = weighted.mean(prostate, final_weight, na.rm = TRUE),
                                                     diabetes = weighted.mean(diabetes, final_weight, na.rm = TRUE),
                                                     survive_prostate = weighted.mean(survive_prostate, final_weight, na.rm = TRUE), survive_breast = weighted.mean(survive_breast, final_weight, na.rm = TRUE),
                                                     weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE),
                                                     fall_person_years = sum(fall_person_years, na.rm = TRUE)), by = list(area, level, year, race, age_bin, edu_code, marital_ushd)]
direct_estimates <- rbindlist(list(direct_estimates, sex_agg), use.names = TRUE, fill = TRUE)
rm(sex_agg); gc()

#### Aggregate across races
message("Aggregate across race")
race_agg <- direct_estimates[, list(race = 1, gen_health_45 = weighted.mean(gen_health_45, final_weight, na.rm = TRUE), 
                                             frequent_activity_limitations = weighted.mean(frequent_activity_limitations, final_weight, na.rm = TRUE),
                                             asthma = weighted.mean(asthma, final_weight, na.rm = TRUE), copd = weighted.mean(copd, final_weight, na.rm = TRUE), 
                                             arthritis = weighted.mean(arthritis, final_weight, na.rm = TRUE), depression = weighted.mean(depression, final_weight, na.rm = TRUE),
                                             tooth_loss = weighted.mean(tooth_loss, final_weight, na.rm = TRUE),
                                             asthma_now = weighted.mean(asthma_now, final_weight, na.rm = TRUE), anxiety = weighted.mean(anxiety, final_weight, na.rm = TRUE),
                                             fallinj = weighted.mean(fallinj / fall_person_years, final_weight, na.rm = TRUE),
                                             dementia = weighted.mean(dementia, final_weight, na.rm = TRUE), prostate = weighted.mean(prostate, final_weight, na.rm = TRUE),
                                             diabetes = weighted.mean(diabetes, final_weight, na.rm = TRUE),
                                             survive_prostate = weighted.mean(survive_prostate, final_weight, na.rm = TRUE), survive_breast = weighted.mean(survive_breast, final_weight, na.rm = TRUE),
                                             weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE),
                                             fall_person_years = sum(fall_person_years, na.rm = TRUE)), by = list(area, level, year, sex, age_bin, edu_code, marital_ushd)]
direct_estimates <- rbindlist(list(direct_estimates, race_agg), use.names = TRUE, fill = TRUE)
rm(race_agg); gc()

#### Aggregate across ages (note that age "98" actually represents 20+, since those are the ages present in the BRFSS data set [after excluding 18-19])
message("Aggregate across age")
age_agg <- direct_estimates[, list(age_bin = 98, gen_health_45 = weighted.mean(gen_health_45, final_weight, na.rm = TRUE), 
                                            frequent_activity_limitations = weighted.mean(frequent_activity_limitations, final_weight, na.rm = TRUE),
                                            asthma = weighted.mean(asthma, final_weight, na.rm = TRUE), copd = weighted.mean(copd, final_weight, na.rm = TRUE), 
                                            arthritis = weighted.mean(arthritis, final_weight, na.rm = TRUE), depression = weighted.mean(depression, final_weight, na.rm = TRUE),
                                            tooth_loss = weighted.mean(tooth_loss, final_weight, na.rm = TRUE),
                                            asthma_now = weighted.mean(asthma_now, final_weight, na.rm = TRUE), anxiety = weighted.mean(anxiety, final_weight, na.rm = TRUE),
                                            fallinj = weighted.mean(fallinj / fall_person_years, final_weight, na.rm = TRUE),
                                            dementia = weighted.mean(dementia, final_weight, na.rm = TRUE), prostate = weighted.mean(prostate, final_weight, na.rm = TRUE),
                                            diabetes = weighted.mean(diabetes, final_weight, na.rm = TRUE),
                                            survive_prostate = weighted.mean(survive_prostate, final_weight, na.rm = TRUE), survive_breast = weighted.mean(survive_breast, final_weight, na.rm = TRUE),
                                            weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE),
                                            fall_person_years = sum(fall_person_years, na.rm = TRUE)), by = list(area, level, year, sex, race, edu_code, marital_ushd)]
direct_estimates <- rbindlist(list(direct_estimates, age_agg), use.names = TRUE, fill = TRUE)
rm(age_agg); gc()

#### Aggregate across edu
message("Aggregate across edu")
edu_agg <- direct_estimates[, list(edu_code = 9, gen_health_45 = weighted.mean(gen_health_45, final_weight, na.rm = TRUE), 
                                   frequent_activity_limitations = weighted.mean(frequent_activity_limitations, final_weight, na.rm = TRUE),
                                   asthma = weighted.mean(asthma, final_weight, na.rm = TRUE), copd = weighted.mean(copd, final_weight, na.rm = TRUE), 
                                   arthritis = weighted.mean(arthritis, final_weight, na.rm = TRUE), depression = weighted.mean(depression, final_weight, na.rm = TRUE),
                                   tooth_loss = weighted.mean(tooth_loss, final_weight, na.rm = TRUE),
                                   asthma_now = weighted.mean(asthma_now, final_weight, na.rm = TRUE), anxiety = weighted.mean(anxiety, final_weight, na.rm = TRUE),
                                   fallinj = weighted.mean(fallinj / fall_person_years, final_weight, na.rm = TRUE),
                                   dementia = weighted.mean(dementia, final_weight, na.rm = TRUE), prostate = weighted.mean(prostate, final_weight, na.rm = TRUE),
                                   diabetes = weighted.mean(diabetes, final_weight, na.rm = TRUE),
                                   survive_prostate = weighted.mean(survive_prostate, final_weight, na.rm = TRUE), survive_breast = weighted.mean(survive_breast, final_weight, na.rm = TRUE),
                                   weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE),
                                   fall_person_years = sum(fall_person_years, na.rm = TRUE)), by = list(area, level, age_bin, sex, race, year, marital_ushd)]
direct_estimates <- rbindlist(list(direct_estimates, edu_agg), use.names = TRUE, fill = TRUE)

#### Aggregate across marital
message("Aggregate across marital")
marital_agg <- direct_estimates[, list(marital_ushd = 9, gen_health_45 = weighted.mean(gen_health_45, final_weight, na.rm = TRUE), 
                                       frequent_activity_limitations = weighted.mean(frequent_activity_limitations, final_weight, na.rm = TRUE),
                                       asthma = weighted.mean(asthma, final_weight, na.rm = TRUE), copd = weighted.mean(copd, final_weight, na.rm = TRUE), 
                                       arthritis = weighted.mean(arthritis, final_weight, na.rm = TRUE), depression = weighted.mean(depression, final_weight, na.rm = TRUE),
                                       tooth_loss = weighted.mean(tooth_loss, final_weight, na.rm = TRUE),
                                       asthma_now = weighted.mean(asthma_now, final_weight, na.rm = TRUE), anxiety = weighted.mean(anxiety, final_weight, na.rm = TRUE),
                                       fallinj = weighted.mean(fallinj / fall_person_years, final_weight, na.rm = TRUE),
                                       dementia = weighted.mean(dementia, final_weight, na.rm = TRUE), prostate = weighted.mean(prostate, final_weight, na.rm = TRUE),
                                       diabetes = weighted.mean(diabetes, final_weight, na.rm = TRUE),
                                       survive_prostate = weighted.mean(survive_prostate, final_weight, na.rm = TRUE), survive_breast = weighted.mean(survive_breast, final_weight, na.rm = TRUE),
                                       weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE),
                                       fall_person_years = sum(fall_person_years, na.rm = TRUE)), by = list(area, level, age_bin, sex, race, year, edu_code)]
direct_estimates <- rbindlist(list(direct_estimates, marital_agg), use.names = TRUE, fill = TRUE)


#### Rename age column
setnames(direct_estimates, "age_bin", "age")

#### Save direct estimates
saveRDS(direct_estimates, file = paste0(output_dir, "/brfss_2000_2019_direct_estimates.rds"))
