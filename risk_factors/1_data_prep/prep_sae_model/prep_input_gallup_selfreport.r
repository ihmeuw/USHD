###########################################################################################################################################
## Description: Create county-year-age-sex-race/ethnicity aggregates of self-reported risk factors data from gallup
##
## Input:       None
## Output:      Combined microdata files for gallup, with requisite demographic and general health data.
##
###########################################################################################################################################
######## 1. Setup
#### Load required packages installed in Singularity image
rm(list=ls())
pacman::p_load(data.table, haven, ggplot2, stringr, survey, car, R.utils,doParallel, RColorBrewer)

#### Set paths
# Race        OLD    NEW
# Hispanic    7       2
# NH Black    2       4
# NH White    1       5
# NH AIAN     3       6
# NH API      4       7
# all_race    9       1
race_code_set <- 'old' #'db
stopifnot(race_code_set %in% c("old", "db"))

data_version <- "FILEPATH"
input_dir <- paste0('FILEPATH')

###### Source functions
repo <- paste0("FILEPATH") 
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

###### Generate run date and create model output folder
run_date <- 'FILEPATH'
output_dir <- paste0("FILEPATH", run_date)
message(run_date)
message(output_dir)
dir.create(output_dir)


######## 2. Load individual data set
data_combined <- readRDS(file = paste0(input_dir, "pred_bridged_race.rds"))

#### Limit to 2000-2019
data_combined <- data_combined[survey=='gallup']

#### Drop 2018-2019 data (different methodology from previous years)
data_combined <- data_combined[!(year %in% 2018:2019)]

data_combined[,c('race','race2','race77','race97'):=NULL]
#### Process race/ethnicity
setnames(data_combined, "gallup_race77", "race")
data_combined[race %in% c("NH Other", "NH Multiracial"), race := NA]
data_combined[race %in% c("NH Asian", "NH NHOPI"), race := 'NH API']

# Set race codes
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

#### Process education
# Set education codes
data_combined <- merge(data_combined, education_map, by = "edu", all.x = TRUE)
data_combined[, edu := NULL]

#### Process age
# Create USHD age variables
data_combined[!is.na(age_continuous), age_bin := floor(age_continuous / 5) * 5]
data_combined[!is.na(age_continuous) & age_continuous >= 80, age_bin := 80]
data_combined <- data_combined[is.na(age_bin) | age_bin != 15] # Drop 18-19 year-olds (for nonfatal work, specifically)
data_combined[, c("age", "age_continuous") := NULL]


#### Process output
########## Process data for mean BMI
data_combined[, obese := ifelse(bmi_report >= 30, 1, 0)]
data_combined[, overwt := ifelse(bmi_report >= 25, 1, 0)]
data_combined[, underwt := ifelse(bmi_report < 18.5, 1, 0)]

#### Rename weights variable
setnames(data_combined, "wt", "weights")

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

#### Load state-cnty-mcnty crosswalk file
locs <- fread("FILEPATH")

#### Merge state name
data_combined <- merge(data_combined, unique(locs[, list(state, state_name)]), by = "state", all.x = TRUE)

#### Process marital
data_combined[marital == "current", marital_ushd := 1] # Currently married
data_combined[marital == "former", marital_ushd := 2] # Formerly married
data_combined[marital == "never", marital_ushd := 3] # Never married

#### Save full data set to disk
saveRDS(data_combined, file = paste0(output_dir, "/data_full.rds"))

########## Process data for mean BMI
## Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_mean <- data_combined[(is.na(mcnty) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin)  | is.na(edu_code) | is.na(marital_ushd)]
data_combined_mean <- na.omit(data_combined, cols = c("mcnty", "state", "year", "sex", "race", "age_bin", "edu_code", "marital_ushd"))
if (nrow(data_missing_mean) + nrow(data_combined_mean) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_overwt) + nrow(data_combined_overwt), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

#### Collapse overweight by county-year-age-sex and either race/ethnicity or education
data_combined_agg_mean <- data_combined_mean[, list(pred_bmi = mean(bmi, na.rm = TRUE), pred_bmi_weighted = weighted.mean(bmi, weights * re_weight) * sum(re_weight), 
                                                    weights = sum(weights * re_weight), sample_size = sum(re_weight)), 
                                             by = list(mcnty, state, year, sex, race, age_bin, edu_code, marital_ushd)]

#### Save collapsed data sets
saveRDS(data_missing_mean, file = paste0(output_dir, "/gallup_agg_missing_data_meanbmi.rds"))
saveRDS(data_combined_agg_mean, file = paste0(output_dir, "/gallup_agg_meanbmi.rds"))


########## Process data for overweight
## Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_overwt <- data_combined[(is.na(mcnty) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin)  | is.na(edu_code) | is.na(marital_ushd)]
data_combined_overwt <- na.omit(data_combined, cols = c("mcnty", "state", "year", "sex", "race", "age_bin", "edu_code", "marital_ushd"))
if (nrow(data_missing_overwt) + nrow(data_combined_overwt) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_overwt) + nrow(data_combined_overwt), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

#### Collapse overweight by county-year-age-sex and either race/ethnicity or education
data_combined_agg_overwt <- data_combined_overwt[, list(overwt = sum(overwt * re_weight, na.rm = TRUE), overwt_weighted_count = weighted.mean(overwt, weights * re_weight) * sum(re_weight), 
                                                        weights = sum(weights * re_weight), sample_size = sum(re_weight)), 
                                                 by = list(mcnty, state, year, sex, race, age_bin, edu_code, marital_ushd)]

########## Process data for underweight
data_missing_underwt <- data_combined[(is.na(mcnty) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_underwt <- na.omit(data_combined, cols = c("mcnty", "state", "year", "sex", "race", "age_bin", "edu_code", "marital_ushd"))
if (nrow(data_missing_underwt) + nrow(data_combined_underwt) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_underwt) + nrow(data_combined_underwt), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

######## Collapse underweight by county-year-age-sex and either race/ethnicity or education
data_combined_agg_underwt <- data_combined_underwt[, list(underwt = sum(underwt * re_weight, na.rm = TRUE), underwt_weighted_count = weighted.mean(underwt, weights * re_weight) * sum(re_weight), 
                                                          weights = sum(weights * re_weight), sample_size = sum(re_weight)), 
                                                   by = list(mcnty, state, year, sex, race, age_bin, edu_code, marital_ushd)]

#### Save collapsed data sets
saveRDS(data_missing_underwt, file = paste0(output_dir, "/gallup_agg_missing_data_underweight.rds"))
saveRDS(data_combined_agg_underwt, file = paste0(output_dir, "/gallup_agg_underweight.rds"))

########## Process data for obesity conditional on overweight
data_missing_obese <- data_combined[overwt==1 & (is.na(mcnty) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_obese <- data_combined[overwt==1 & !(is.na(mcnty) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(edu_code) & !is.na(marital_ushd) ]

data_combined_agg_obese <- data_combined_obese[, list(obese = sum(obese * re_weight, na.rm = TRUE), 
                                                      obese_weighted_count = weighted.mean(obese, weights * re_weight) * sum(re_weight), 
                                                      weights = sum(weights * re_weight), sample_size = sum(re_weight)), 
                                               by = list(mcnty, state, year, sex, race, age_bin, edu_code, marital_ushd)]

#### Save collapsed data sets
saveRDS(data_missing_obese, file = paste0(output_dir, "/gallup_agg_missing_data_obese.rds"))
saveRDS(data_combined_agg_obese, file = paste0(output_dir, "/gallup_agg_obese.rds"))

######## 11. Produce direct estimates by race/ethnicity
##### Note: Phone has been dropped from these aggregations
#### Produce mcnty-level direct estimates
mcnty_estimates <- data_combined[, list(level = "mcnty", obese = weighted.mean(obese, re_weight * weights, na.rm = TRUE), 
                                        underwt = weighted.mean(underwt, re_weight * weights, na.rm = TRUE),
                                        overwt = weighted.mean(overwt, re_weight * weights, na.rm = TRUE),
                                        pred_bmi = weighted.mean(bmi, re_weight * weights, na.rm = TRUE),
                                        weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(mcnty, state, state_name, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

mcnty_estimates_no_strat <- data_combined[, list(level = "mcnty", obese = weighted.mean(obese, re_weight * weights, na.rm = TRUE), 
                                                 underwt = weighted.mean(underwt, re_weight * weights, na.rm = TRUE),
                                                 overwt = weighted.mean(overwt, re_weight * weights, na.rm = TRUE),
                                                 pred_bmi = weighted.mean(bmi, re_weight * weights, na.rm = TRUE),
                                                 weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(mcnty, state, state_name, year, sex, race, age_bin, nid)]

#### Produce state-level direct estimates
state_estimates <- data_combined[, list(level = "state", obese = weighted.mean(obese, re_weight * weights, na.rm = TRUE), 
                                        underwt = weighted.mean(underwt, re_weight * weights, na.rm = TRUE),
                                        overwt = weighted.mean(overwt, re_weight * weights, na.rm = TRUE),
                                        pred_bmi = weighted.mean(bmi, re_weight * weights, na.rm = TRUE),
                                        weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(state, state_name, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

state_estimates_no_strat <- data_combined[, list(level = "state", obese = weighted.mean(obese, re_weight * weights, na.rm = TRUE), 
                                                 underwt = weighted.mean(underwt, re_weight * weights, na.rm = TRUE),
                                                 overwt = weighted.mean(overwt, re_weight * weights, na.rm = TRUE),
                                                 pred_bmi = weighted.mean(bmi, re_weight * weights, na.rm = TRUE),
                                                 weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(state, state_name, year, sex, race, age_bin, nid)]


#### Produce national-level direct estimates
# The weight object here should be nationally-representative
national_estimates <- data_combined[, list(level = "natl", obese = weighted.mean(obese, re_weight * weights, na.rm = TRUE), 
                                           underwt = weighted.mean(underwt, re_weight * weights, na.rm = TRUE),
                                           overwt = weighted.mean(overwt, re_weight * weights, na.rm = TRUE),
                                           pred_bmi = weighted.mean(bmi, re_weight * weights, na.rm = TRUE),
                                           weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(year, sex, race, age_bin, edu_code, marital_ushd, nid)]

national_estimates_no_strat <- data_combined[, list(level = "natl", obese = weighted.mean(obese, re_weight * weights, na.rm = TRUE), 
                                                    underwt = weighted.mean(underwt, re_weight * weights, na.rm = TRUE),
                                                    overwt = weighted.mean(overwt, re_weight * weights, na.rm = TRUE),
                                                    pred_bmi = weighted.mean(bmi, re_weight * weights, na.rm = TRUE),
                                                    weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(year, sex, race, age_bin, nid)]

#### Combine direct estimates across levels
direct_estimates <- rbindlist(list(mcnty_estimates, state_estimates, national_estimates), use.names = TRUE, fill = TRUE)
direct_estimates_no_strat <- rbindlist(list(mcnty_estimates_no_strat, state_estimates_no_strat, national_estimates_no_strat), use.names = TRUE, fill = TRUE)

#### Harmonize area identifiers
direct_estimates[level == "mcnty", area := mcnty]
direct_estimates[level == "state", area := state]
direct_estimates[level == "natl", area := 1]

direct_estimates_no_strat[level == "mcnty", area := mcnty]
direct_estimates_no_strat[level == "state", area := state]
direct_estimates_no_strat[level == "natl", area := 1]

#### Aggregate across sexes
sex_agg <- direct_estimates[, list(sex = 3, obese = weighted.mean(obese, weights, na.rm = TRUE), 
                                   underwt = weighted.mean(underwt, weights, na.rm = TRUE),
                                   overwt = weighted.mean(overwt, weights, na.rm = TRUE),
                                   pred_bmi = weighted.mean(pred_bmi, weights, na.rm = TRUE),
                                   weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, year, race, age_bin, edu_code, marital_ushd, nid)]
direct_estimates <- rbindlist(list(direct_estimates, sex_agg), use.names = TRUE, fill = TRUE)

sex_agg_no_strat <- direct_estimates_no_strat[, list(sex = 3, obese = weighted.mean(obese, weights, na.rm = TRUE), 
                                                     underwt = weighted.mean(underwt, weights, na.rm = TRUE),
                                                     overwt = weighted.mean(overwt, weights, na.rm = TRUE),
                                                     pred_bmi = weighted.mean(pred_bmi, weights, na.rm = TRUE),
                                                     weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, year, race, age_bin, nid)]
direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, sex_agg_no_strat), use.names = TRUE, fill = TRUE)

#### Aggregate across races
race_agg <- direct_estimates[, list(race = 9, obese = weighted.mean(obese, weights, na.rm = TRUE), 
                                    underwt = weighted.mean(underwt, weights, na.rm = TRUE),
                                    overwt = weighted.mean(overwt, weights, na.rm = TRUE),
                                    pred_bmi = weighted.mean(pred_bmi, weights, na.rm = TRUE),
                                    weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, year, sex, age_bin, edu_code, marital_ushd, nid)]
direct_estimates <- rbindlist(list(direct_estimates, race_agg), use.names = TRUE, fill = TRUE)

race_agg_no_strat <- direct_estimates_no_strat[, list(race = 9, obese = weighted.mean(obese, weights, na.rm = TRUE), 
                                                      underwt = weighted.mean(underwt, weights, na.rm = TRUE),
                                                      overwt = weighted.mean(overwt, weights, na.rm = TRUE),
                                                      pred_bmi = weighted.mean(pred_bmi, weights, na.rm = TRUE),
                                                      weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, year, sex, age_bin, nid)]
direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, race_agg_no_strat), use.names = TRUE, fill = TRUE)

#### Aggregate across ages
age_agg <- direct_estimates[, list(age_bin = 98, obese = weighted.mean(obese, weights, na.rm = TRUE), 
                                   underwt = weighted.mean(underwt, weights, na.rm = TRUE),
                                   overwt = weighted.mean(overwt, weights, na.rm = TRUE),
                                   pred_bmi = weighted.mean(pred_bmi, weights, na.rm = TRUE),
                                   weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, year, sex, race, edu_code, marital_ushd, nid)]
direct_estimates <- rbindlist(list(direct_estimates, age_agg), use.names = TRUE, fill = TRUE)

age_agg_no_strat <- direct_estimates_no_strat[, list(age_bin = 98, obese = weighted.mean(obese, weights, na.rm = TRUE), 
                                                     underwt = weighted.mean(underwt, weights, na.rm = TRUE),
                                                     overwt = weighted.mean(overwt, weights, na.rm = TRUE),
                                                     pred_bmi = weighted.mean(pred_bmi, weights, na.rm = TRUE),
                                                     weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, year, sex, race, nid)]
direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, age_agg_no_strat), use.names = TRUE, fill = TRUE)

#### Aggregate across years
year_agg <- direct_estimates[, list(year = -1, obese = weighted.mean(obese, weights, na.rm = TRUE), 
                                    underwt = weighted.mean(underwt, weights, na.rm = TRUE),
                                    overwt = weighted.mean(overwt, weights, na.rm = TRUE),
                                    pred_bmi = weighted.mean(pred_bmi, weights, na.rm = TRUE),
                                    weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, age_bin, sex, race, edu_code, marital_ushd)]
direct_estimates <- rbindlist(list(direct_estimates, year_agg), use.names = TRUE, fill = TRUE)

year_agg_no_strat <- direct_estimates_no_strat[, list(year = -1, obese = weighted.mean(obese, weights, na.rm = TRUE), 
                                                      underwt = weighted.mean(underwt, weights, na.rm = TRUE),
                                                      overwt = weighted.mean(overwt, weights, na.rm = TRUE),
                                                      pred_bmi = weighted.mean(pred_bmi, weights, na.rm = TRUE),
                                                      weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, age_bin, sex, race)]
direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, year_agg_no_strat), use.names = TRUE, fill = TRUE)

#### Rename age column
setnames(direct_estimates, "age_bin", "age")
setnames(direct_estimates_no_strat, "age_bin", "age")

#### Save direct estimates
saveRDS(direct_estimates, file = paste0(output_dir, "/gallup_2000_2019_direct_estimates.rds"))
saveRDS(direct_estimates_no_strat, file = paste0(output_dir, "/gallup_2000_2019_direct_estimates_no_strat.rds"))

saveRDS(state_estimates, file = paste0(output_dir, "/gallup_daily_2008_2019_state_level.rds"))
saveRDS(state_estimates_no_strat, file = paste0(output_dir, "/gallup_daily_2008_2019_state_level_no_strat.rds"))

sink(paste0(output_dir, "/input_versions.txt"))
print(paste('Gallup input directory:', input_dir))
print(paste('Gallup data version:', data_version))
sink()