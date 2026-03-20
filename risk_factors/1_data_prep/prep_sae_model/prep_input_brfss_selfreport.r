###########################################################################################################################################
## Description: Create county-year-age-sex-race/ethnicity aggregates of self-reported risk factors data from BRFSS
##
## Input:       None
## Output:      Combined microdata files for BRFSS, with requisite demographic and general health data
##
###########################################################################################################################################

rm(list=ls())
pacman::p_load(data.table, haven, ggplot2, stringr, survey, car, R.utils,doParallel, RColorBrewer, Rcpp)

# Race        OLD    NEW
# Hispanic    7       2
# NH Black    2       4
# NH White    1       5
# NH AIAN     3       6
# NH API      4       7
# all_race    9       1
race_code_set <- 'old' #'db
stopifnot(race_code_set %in% c("old", "db"))

#### Set paths
data_version <- "FILEPATH"
input_dir <- paste0('FILEPATH')
cbsa_mcnty_version <- "FILEPATH"

if(race_code_set == "old"){
  pop_file <- "FILEPATH"  
} else{
  pop_file <- "FILEPATH"  
}

###### Source functions
repo <- paste0("FILEPATH") 
funcs <- list.files(paste0(repo, "functions/"))
for (func in funcs) {
  source(paste0(repo, "functions/", func))
}

###### Generate run date and create model output folder
run_date <- make_time_stamp()
output_dir <- paste0("FILEPATH", run_date)
message(run_date)
message(output_dir)
dir.create(output_dir)

######## 2. Load individual data set
data_combined <- readRDS(file = paste0(input_dir, "pred_bridged_race.rds"))
colnames(data_combined)
table(data_combined$race97, exclude=NULL)
table(data_combined[year>1999]$race97, exclude=NULL)

#### Limit to 2000-2019
data_combined <- data_combined[year %in% 2000:2019 & survey=='brfss']

# drop unused columns
drop_cols <- c(grep("percentile|quantile|matched_val|quant|outlier_t10|logit_diff", names(data_combined), value = T), 'race', 'race2')
data_combined[, (drop_cols) := NULL ]

#### Process race/ethnicity
setnames(data_combined, "race77", "race")

# drop NH Other and NH MR w/o main individuals
data_combined[race %in% c("NH Other", "NH MR w/o main"), race := NA]

# Race        OLD    NEW
# Hispanic    7       2
# NH Black    2       4
# NH White    1       5
# NH AIAN     3       6
# NH API      4       7
# all_race    9       1
if (race_code_set=='old'){
  race_map <- list(race_id = c(7,2,1,3,4),
                   race = c("Hispanic", "NH Black", "NH White", "NH AIAN", "NH API"))
  race_all <- 9
  education_map <- data.table("edu_code" = 1:4, "edu" = c("less than HS", "HS grad", "some college", "college grad"))
  edu_all <- 9
} else {
  race_map <- list(race_id = c(2,4,5,6,7),
                   race = c("Hispanic", "NH Black", "NH White", "NH AIAN", "NH API"))
  race_all <- 1
  education_map <- data.table("edu_code" = 100+1:4, "edu" = c("less than HS", "HS grad", "some college", "college grad"))
  edu_all <- 1
}


data_combined <- merge(data_combined, race_map, by = "race", all.x = T)
stopifnot(data_combined[is.na(race_id) & !is.na(race), .N] == 0)
data_combined[, race := NULL]
setnames(data_combined, "race_id", "race")

# Set education codes
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

########## Process data for mean BMI
data_combined[, obese := ifelse(bmi_report >= 30, 1, 0)]
data_combined[, overwt := ifelse(bmi_report >= 25, 1, 0)]
data_combined[, underwt := ifelse(bmi_report < 18.5, 1, 0)]

#### Rename weights variable
setnames(data_combined, "wt", "weights")

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
setnames(data_combined, "brfss_index", "uid")

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
# For now we retain rows with unknown cbsa_mcnty codes
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

#### Save full data set to disk
saveRDS(data_combined, file = paste0(output_dir, "/data_full.rds"))

## Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_mean <- data_combined[(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin)  | is.na(edu_code) | is.na(marital_ushd)]
data_combined_mean <- na.omit(data_combined, cols = c("mcnty", "state", "year", "sex", "race", "age_bin", "edu_code", "marital_ushd",  "cbsa", "cbsa_mcnty_code"))
if (nrow(data_missing_mean) + nrow(data_combined_mean) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_mean) + nrow(data_combined_mean), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

#### Collapse overweight by county-year-age-sex and either race/ethnicity or education
data_combined_agg_mean <- data_combined_mean[, list(pred_bmi = mean(bmi_report, na.rm = TRUE), pred_bmi_weighted = weighted.mean(bmi_report, weights, na.rm = TRUE) * .N, 
                                                  weights = sum(weights, na.rm = TRUE), sample_size = .N), by = list(mcnty, cbsa_mcnty_code, state, year, sex, race, age_bin, edu_code, marital_ushd)]
#### Save collapsed data sets
saveRDS(data_missing_mean, file = paste0(output_dir, "/brfss_agg_missing_data_pred_bmi.rds"))
saveRDS(data_combined_agg_mean, file = paste0(output_dir, "/brfss_agg_pred_bmi.rds"))

########## Process data for overweight
## Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_overwt <- data_combined[(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin)  | is.na(edu_code) | is.na(marital_ushd) ]
data_combined_overwt <- data_combined[!(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(edu_code) & !is.na(marital_ushd)  ]
if (nrow(data_missing_overwt) + nrow(data_combined_overwt) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_overwt) + nrow(data_combined_overwt), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

#### Collapse overweight by county-year-age-sex and either race/ethnicity or education
data_combined_agg_overwt <- data_combined_overwt[, list(overwt = sum(overwt, na.rm = TRUE), overwt_weighted_count = weighted.mean(overwt, weights, na.rm = TRUE) * .N, 
                                                        weights = sum(weights, na.rm = TRUE), sample_size = .N), by = list(mcnty, cbsa_mcnty_code, state, year, sex, race, age_bin, edu_code, marital_ushd)]
#### Save collapsed data sets
saveRDS(data_missing_overwt, file = paste0(output_dir, "/brfss_agg_missing_data_overweight.rds"))
saveRDS(data_combined_agg_overwt, file = paste0(output_dir, "/brfss_agg_overweight.rds"))

########## Process data for underweight
data_missing_underwt <- data_combined[(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(edu_code) | is.na(marital_ushd) ]
data_combined_underwt <- data_combined[!(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(edu_code) & !is.na(marital_ushd) ]
if (nrow(data_missing_underwt) + nrow(data_combined_underwt) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_underwt) + nrow(data_combined_underwt), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

######## Collapse underweight by county-year-age-sex and either race/ethnicity or education
data_combined_agg_underwt <- data_combined_underwt[, list(underwt = sum(underwt, na.rm = TRUE), underwt_weighted_count = weighted.mean(underwt, weights, na.rm = TRUE) * .N, 
                                                          weights = sum(weights, na.rm = TRUE), sample_size = .N), by = list(mcnty, cbsa_mcnty_code, state, year, sex, race, age_bin, edu_code, marital_ushd)]

#### Save collapsed data sets
saveRDS(data_missing_underwt, file = paste0(output_dir, "/brfss_agg_missing_data_underweight.rds"))
saveRDS(data_combined_agg_underwt, file = paste0(output_dir, "/brfss_agg_underweight.rds"))

########## Process data for obesity conditional on overweight
data_missing_obese <- data_combined[overwt==1 & (is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(edu_code) | is.na(marital_ushd) ]
data_combined_obese <- data_combined[overwt==1 & !(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(edu_code) & !is.na(marital_ushd) ]#& !is.na(phone_ushd)
data_combined_agg_obese <- data_combined_obese[, list(obese = sum(obese, na.rm = TRUE), 
                                                        obese_weighted_count = weighted.mean(obese, weights, na.rm = TRUE) * .N, 
                                                        weights = sum(weights, na.rm = TRUE), 
                                                        sample_size = .N), 
                                                 by = list(mcnty, cbsa_mcnty_code, state, year, sex, race, age_bin, edu_code, marital_ushd)]

#### Save collapsed data sets
saveRDS(data_missing_obese, file = paste0(output_dir, "/brfss_agg_missing_data_obese.rds"))
saveRDS(data_combined_agg_obese, file = paste0(output_dir, "/brfss_agg_obese.rds"))

######## 17. Produce direct estimates by race/ethnicity
#### Drop rows with missingness in demographic variables
data_combined_backup <- copy(data_combined)

#### Produce mcnty-level direct estimates
mcnty_estimates <- data_combined[, list(level = "mcnty", obese = weighted.mean(obese, weights, na.rm = TRUE), 
                                        underwt = weighted.mean(underwt, weights, na.rm = TRUE),
                                        overwt = weighted.mean(overwt, weights, na.rm = TRUE),
                                        pred_bmi = weighted.mean(bmi_report, weights, na.rm = TRUE),
                                        weights = sum(weights, na.rm = TRUE), sample_size = .N), by = list(mcnty, state, state_name, year, sex, race, age_bin, edu_code, marital_ushd)]

mcnty_estimates_no_strat <- data_combined[, list(level = "mcnty", obese = weighted.mean(obese, weights, na.rm = TRUE), 
                                                 underwt = weighted.mean(underwt, weights, na.rm = TRUE),
                                                 overwt = weighted.mean(overwt, weights, na.rm = TRUE),
                                                 pred_bmi = weighted.mean(bmi_report, weights, na.rm = TRUE),
                                                 weights = sum(weights, na.rm = TRUE), sample_size = .N), by = list(mcnty, state, state_name, year, sex, race, age_bin)]

#### Produce CBSA-mcnty-level direct estimates
cbsa_mcnty_estimates <- data_combined[, list(level = "cbsa_mcnty", obese = weighted.mean(obese, weights, na.rm = TRUE), 
                                             underwt = weighted.mean(underwt, weights, na.rm = TRUE),
                                             overwt = weighted.mean(overwt, weights, na.rm = TRUE),
                                             pred_bmi = weighted.mean(bmi_report, weights, na.rm = TRUE),
                                             weights = sum(weights, na.rm = TRUE), sample_size = .N), by = list(cbsa_mcnty_code, state, state_name, year, sex, race, age_bin, edu_code, marital_ushd)]

cbsa_mcnty_estimates_no_strat <- data_combined[, list(level = "cbsa_mcnty", obese = weighted.mean(obese, weights, na.rm = TRUE), 
                                                      underwt = weighted.mean(underwt, weights, na.rm = TRUE),
                                                      overwt = weighted.mean(overwt, weights, na.rm = TRUE),
                                                      pred_bmi = weighted.mean(bmi_report, weights, na.rm = TRUE),
                                                      weights = sum(weights, na.rm = TRUE), sample_size = .N), by = list(cbsa_mcnty_code, state, state_name, year, sex, race, age_bin)]

#### Produce state-level direct estimates
state_estimates <- data_combined[, list(level = "state", obese = weighted.mean(obese, weights, na.rm = TRUE), 
                                        underwt = weighted.mean(underwt, weights, na.rm = TRUE),
                                        overwt = weighted.mean(overwt, weights, na.rm = TRUE),
                                        pred_bmi = weighted.mean(bmi_report, weights, na.rm = TRUE),
                                        weights = sum(weights, na.rm = TRUE), sample_size = .N), by = list(state, state_name, year, sex, race, age_bin, edu_code, marital_ushd)]

state_estimates_no_strat <- data_combined[, list(level = "state", obese = weighted.mean(obese, weights, na.rm = TRUE), 
                                                 underwt = weighted.mean(underwt, weights, na.rm = TRUE),
                                                 overwt = weighted.mean(overwt, weights, na.rm = TRUE),
                                                 pred_bmi = weighted.mean(bmi_report, weights, na.rm = TRUE),
                                                 weights = sum(weights, na.rm = TRUE), sample_size = .N), by = list(state, state_name, year, sex, race, age_bin)]

#### Load and merge population file
pop <- readRDS("FILEPATH")
pop_state <- pop[, list(pop = sum(pop)), by = c("year", "sex", "age", "race", "state")]

mcnty_estimates <- merge(mcnty_estimates, pop[, -c("state")], by.x = c("mcnty", "year", "sex", "race", "age_bin"), by.y = c("mcnty", "year", "sex", "race", "age"), all.x = TRUE)
mcnty_estimates_no_strat <- merge(mcnty_estimates_no_strat, pop[, -c("state")], by.x = c("mcnty", "year", "sex", "race", "age_bin"), by.y = c("mcnty", "year", "sex", "race", "age"), all.x = TRUE)

cbsa_mcnty_estimates <- merge(cbsa_mcnty_estimates, unique(cbsa_mcnty[, c("year", "cbsa_mcnty_code", "sex", "age_bin", "race", "total_pop")]), by.x = c("year", "cbsa_mcnty_code", "sex", "age_bin", "race"), by.y = c("year", "cbsa_mcnty_code", "sex", "age_bin", "race"), all.x = TRUE)
setnames(cbsa_mcnty_estimates, "total_pop", "pop")
cbsa_mcnty_estimates_no_strat <- merge(cbsa_mcnty_estimates_no_strat, unique(cbsa_mcnty[, c("year", "cbsa_mcnty_code", "sex", "age_bin", "race", "total_pop")]), by.x = c("year", "cbsa_mcnty_code", "sex", "age_bin", "race"), by.y = c("year", "cbsa_mcnty_code", "sex", "age_bin", "race"), all.x = TRUE)
setnames(cbsa_mcnty_estimates_no_strat, "total_pop", "pop")

state_estimates <- merge(state_estimates, pop_state, by.x = c("state", "year", "sex", "race", "age_bin"), by.y = c("state", "year", "sex", "race", "age"), all.x = TRUE)
state_estimates_no_strat <- merge(state_estimates_no_strat, pop_state, by.x = c("state", "year", "sex", "race", "age_bin"), by.y = c("state", "year", "sex", "race", "age"), all.x = TRUE)

#### Derive BRFSS weights proportions to scale population estimates for stratified estimates
mcnty_estimates[, pop_scaled := pop * weights / sum(weights, na.rm = TRUE), by = c("mcnty", "year", "sex", "race", "age_bin")]
cbsa_mcnty_estimates[, pop_scaled := pop * weights / sum(weights, na.rm = TRUE), by = c("cbsa_mcnty_code", "year", "sex", "race", "age_bin")]
state_estimates[, pop_scaled := pop * weights / sum(weights, na.rm = TRUE), by = c("state", "year", "sex", "race", "age_bin")]
mcnty_estimates[, c("pop", "pop_scaled") := list(pop_scaled, NULL)]
cbsa_mcnty_estimates[, c("pop", "pop_scaled") := list(pop_scaled, NULL)]
state_estimates[, c("pop", "pop_scaled") := list(pop_scaled, NULL)]

#### Produce national-level direct estimates
# BRFSS weights are not nationally-representative, so we weight state-level direct estimates by population
national_estimates <- state_estimates[, list(level = "natl", obese = weighted.mean(obese, weights, na.rm = TRUE), 
                                             underwt = weighted.mean(underwt, weights, na.rm = TRUE),
                                             overwt = weighted.mean(overwt, weights, na.rm = TRUE),
                                             pred_bmi = weighted.mean(pred_bmi, weights, na.rm = TRUE),
                                             weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE)), by = list(year, sex, race, age_bin, edu_code, marital_ushd)]

national_estimates_no_strat <- state_estimates_no_strat[, list(level = "natl", obese = weighted.mean(obese, pop, na.rm = TRUE), 
                                                               underwt = weighted.mean(underwt, pop, na.rm = TRUE),
                                                               overwt = weighted.mean(overwt, pop, na.rm = TRUE),
                                                               pred_bmi = weighted.mean(pred_bmi, pop, na.rm = TRUE),
                                                               weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE)), by = list(year, sex, race, age_bin)]

#### Combine direct estimates across levels
direct_estimates <- rbindlist(list(mcnty_estimates, cbsa_mcnty_estimates, state_estimates, national_estimates), use.names = TRUE, fill = TRUE)
direct_estimates_no_strat <- rbindlist(list(mcnty_estimates_no_strat, cbsa_mcnty_estimates_no_strat, state_estimates_no_strat, national_estimates_no_strat), use.names = TRUE, fill = TRUE)

#### Harmonize area identifiers
direct_estimates[level == "mcnty", area := mcnty]
direct_estimates[level == "cbsa_mcnty", area := cbsa_mcnty_code]
direct_estimates[level == "state", area := state]
direct_estimates[level == "natl", area := 1L]

direct_estimates_no_strat[level == "mcnty", area := mcnty]
direct_estimates_no_strat[level == "cbsa_mcnty", area := cbsa_mcnty_code]
direct_estimates_no_strat[level == "state", area := state]
direct_estimates_no_strat[level == "natl", area := 1L]

#### Set weights equal to pop for national estimates
direct_estimates[level == "natl", final_weight := pop]
direct_estimates[level != "natl", final_weight := weights]
direct_estimates_no_strat[level == "natl", final_weight := pop]
direct_estimates_no_strat[level != "natl", final_weight := weights]

#### Aggregate across sexes
sex_agg <- direct_estimates[, list(sex = 3, obese = weighted.mean(obese, final_weight, na.rm = TRUE), 
                                   underwt = weighted.mean(underwt, final_weight, na.rm = TRUE),
                                   overwt = weighted.mean(overwt, final_weight, na.rm = TRUE),
                                   pred_bmi = weighted.mean(pred_bmi, final_weight, na.rm = TRUE),
                                   weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE)), by = list(area, level, year, race, age_bin, edu_code, marital_ushd)]
direct_estimates <- rbindlist(list(direct_estimates, sex_agg), use.names = TRUE, fill = TRUE)

sex_agg_no_strat <- direct_estimates_no_strat[, list(sex = 3, obese = weighted.mean(obese, final_weight, na.rm = TRUE), 
                                                     underwt = weighted.mean(underwt, final_weight, na.rm = TRUE),
                                                     overwt = weighted.mean(overwt, final_weight, na.rm = TRUE),
                                                     pred_bmi = weighted.mean(pred_bmi, final_weight, na.rm = TRUE),
                                                     weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE)), by = list(area, level, year, race, age_bin)]
direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, sex_agg_no_strat), use.names = TRUE, fill = TRUE)


#### Aggregate across races
race_agg <- direct_estimates[, list(race = 9, obese = weighted.mean(obese, final_weight, na.rm = TRUE), 
                                    underwt = weighted.mean(underwt, final_weight, na.rm = TRUE),
                                    overwt = weighted.mean(overwt, final_weight, na.rm = TRUE),
                                    pred_bmi = weighted.mean(pred_bmi, final_weight, na.rm = TRUE),
                                    weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE)), by = list(area, level, year, sex, age_bin, edu_code, marital_ushd)]
direct_estimates <- rbindlist(list(direct_estimates, race_agg), use.names = TRUE, fill = TRUE)

race_agg_no_strat <- direct_estimates_no_strat[, list(race = 9, obese = weighted.mean(obese, final_weight, na.rm = TRUE), 
                                                      underwt = weighted.mean(underwt, final_weight, na.rm = TRUE),
                                                      overwt = weighted.mean(overwt, final_weight, na.rm = TRUE),
                                                      pred_bmi = weighted.mean(pred_bmi, final_weight, na.rm = TRUE),
                                                      weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE)), by = list(area, level, year, sex, age_bin)]
direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, race_agg_no_strat), use.names = TRUE, fill = TRUE)

#### Aggregate across ages
age_agg <- direct_estimates[, list(age_bin = 98, obese = weighted.mean(obese, final_weight, na.rm = TRUE), 
                                   underwt = weighted.mean(underwt, final_weight, na.rm = TRUE),
                                   overwt = weighted.mean(overwt, final_weight, na.rm = TRUE),
                                   pred_bmi = weighted.mean(pred_bmi, final_weight, na.rm = TRUE),
                                   weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE)), by = list(area, level, year, sex, race, edu_code, marital_ushd)]
direct_estimates <- rbindlist(list(direct_estimates, age_agg), use.names = TRUE, fill = TRUE)

age_agg_no_strat <- direct_estimates_no_strat[, list(age_bin = 98, obese = weighted.mean(obese, final_weight, na.rm = TRUE), 
                                                     underwt = weighted.mean(underwt, final_weight, na.rm = TRUE),
                                                     overwt = weighted.mean(overwt, final_weight, na.rm = TRUE),
                                                     pred_bmi = weighted.mean(pred_bmi, final_weight, na.rm = TRUE),
                                                     weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE)), by = list(area, level, year, sex, race)]
direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, age_agg_no_strat), use.names = TRUE, fill = TRUE)

#### Aggregate across years
year_agg <- direct_estimates[, list(year = -1, obese = weighted.mean(obese, final_weight, na.rm = TRUE), 
                                    underwt = weighted.mean(underwt, final_weight, na.rm = TRUE),
                                    overwt = weighted.mean(overwt, final_weight, na.rm = TRUE),
                                    pred_bmi = weighted.mean(pred_bmi, final_weight, na.rm = TRUE),
                                    weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE)), by = list(area, level, age_bin, sex, race, edu_code, marital_ushd)]
direct_estimates <- rbindlist(list(direct_estimates, year_agg), use.names = TRUE, fill = TRUE)

year_agg_no_strat <- direct_estimates_no_strat[, list(year = -1, obese = weighted.mean(obese, final_weight, na.rm = TRUE), 
                                                      underwt = weighted.mean(underwt, final_weight, na.rm = TRUE),
                                                      overwt = weighted.mean(overwt, final_weight, na.rm = TRUE),
                                                      pred_bmi = weighted.mean(pred_bmi, final_weight, na.rm = TRUE),
                                                      weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE)), by = list(area, level, age_bin, sex, race)]
direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, year_agg_no_strat), use.names = TRUE, fill = TRUE)

#### Rename age column
setnames(direct_estimates, "age_bin", "age")
setnames(direct_estimates_no_strat, "age_bin", "age")

#### Save direct estimates
saveRDS(direct_estimates, file = paste0(output_dir, "/brfss_2000_2019_direct_estimates.rds"))
saveRDS(direct_estimates_no_strat, file = paste0(output_dir, "/brfss_2000_2019_direct_estimates_no_strat.rds"))

saveRDS(state_estimates, file = paste0(output_dir, "/brfss_state_level.rds"))
saveRDS(state_estimates_no_strat, file = paste0(output_dir, "/brfss_state_level_no_strat.rds"))

## Track data/crosswalk versions
sink(paste0(output_dir, "/input_versions.txt"))
print(paste('BRFSS input directory:', input_dir))
print(paste('BRFSS data version:', data_version))
print(paste('CBSA-mcnty crosswalk version:', cbsa_mcnty_version))
sink()
