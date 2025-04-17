###########################################################################################################################################
## Description: Create county-year-age-sex-race/ethnicity aggregates of self-reported health data from Gallup Daily, using previously-
##              extracted individual-level data.
##
## Input:       None
## Output:      Combined data files for Gallup Daily, with requisite demographic and general health data
##
###########################################################################################################################################

######## 1. Setup
#### Load required packages installed in Singularity image
pacman::p_load(data.table, haven, ggplot2, stringr, car, R.utils, doParallel, boot)
pacman::p_load(rgdal, ggthemes, cowplot, mapproj, maptools, rgeos, RColorBrewer)

.libPaths(c("FILEPATH", .libPaths())) # Switch lib path to user folder (assumes R 4.1.3)
pacman::p_load(psych) # Load custom-installed packages

#### Set paths
data_version <- "NUM"
data_version_append <- "_additional_strat_vars"
input_dir <- paste0("FILEPATH")

###### Source functions
repo <- paste0("FILEPATH")
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs[funcs != "load_sae_shared.R"]) {
  print(func)
  source(paste0(repo, "/functions/", func))
}

###### Generate run date and create model output folder
run_date <- make_time_stamp()
output_dir <- paste0("FILEPATH")
message(run_date)
message(output_dir)
dir.create(output_dir)


######## 2. Load individual data set
data_combined <- readRDS(file = paste0(input_dir, "gallup_microdata_race77.rds"))

#### Rename some variables
setnames(data_combined, c("race77", "educ", "genhealth", "wt_nalt_wb"), c("race", "education", "gen_health", "weights"))

#### Drop 2018-2019 data (different methodology from previous years)
data_combined <- data_combined[!(year %in% 2018:2019)]

#### Load state-cnty-mcnty crosswalk file
locs <- fread("FILEPATH")

#### Merge state codes and names
data_combined <- merge(data_combined[, -c("state_name")], unique(locs[, list(state, state_name)]), by = "state", all.x = TRUE)

#### Recode education (1 = < HS, 2 = HS or equivalent, 3 = Some college, 4 = BA or higher)
education_map <- data.table("edu_code" = 1:4, "education" = c("less than HS", "HS grad", "some college", "college grad"))
data_combined <- merge(data_combined, education_map, by = "education", all.x = TRUE)
data_combined[, education := NULL] # Drop old education variable
data_combined[, weight := NULL] # Also drop weight variable, to avoid confusion (this is body weight)

#### Create USHD age variables
data_combined <- data_combined[!(age %in% 18:19)] # Drop 18-19 year-olds (for nonfatal work, specifically)
data_combined[age %in% 18:84, age_bin := floor(age / 5) * 5]
data_combined[age >= 85, age_bin := 85]

# Set race codes
# USHD (1977 OMB classification)
# 2: Hispanic
# 4: NH Black
# 5: NH White
# 6: NH AIAN
# 7: NH API

#### Set race codes
#### Convert race to USHD race codes (1977 OMB version)
data_combined[race == "NH White", race_ushd := 5] # NH White
data_combined[race == "NH Black", race_ushd := 4] # NH Black
data_combined[race == "NH AIAN", race_ushd := 6] # NH AIAN
data_combined[race == "NH API", race_ushd := 7] # NH API
data_combined[race == "Hispanic", race_ushd := 2] # Hispanic
data_combined[race == "NH Asian", race_ushd := NA] # NH Asian
data_combined[race == "NH Other", race_ushd := NA] # Other

data_combined[, race := NULL]
setnames(data_combined, "race_ushd", "race")

#### Clean marital status
data_combined[marital == "current", marital_ushd := 1] # Currently married
data_combined[marital == "former", marital_ushd := 2] # Formerly married
data_combined[marital == "never", marital_ushd := 3] # Never married

#### Create a binary indicator indicating >= 14 days of poor health
setnames(data_combined, "activity_limited_days", "activity_limit_days")
data_combined[!is.na(activity_limit_days), frequent_activity_limitations := as.integer(activity_limit_days >= 14)]

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

#### Save full data set to disk
saveRDS(data_combined, file = paste0(output_dir, "/data_full.rds"))


######## 5. Create a data set for analyzing activity_limit_days
#### Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_act_limit_race <- data_combined[(is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(edu_code) | is.na(marital_ushd) | is.na(activity_limit_days))]
data_combined_act_limit_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(edu_code) & !is.na(marital_ushd) & !is.na(activity_limit_days)]
if (nrow(data_missing_act_limit_race) + nrow(data_combined_act_limit_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_act_limit_race) + nrow(data_combined_act_limit_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 6. Collapse activity_limit_days by county-year-age-sex and either race/ethnicity or education
data_combined_agg_act_limit_race_weighted <- data_combined_act_limit_race[, list(frequent_activity_limitations_count = sum(frequent_activity_limitations * re_weight), frequent_activity_limitations_weighted_count = weighted.mean(frequent_activity_limitations, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

#### Save collapsed data sets
saveRDS(data_missing_act_limit_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_act_limit_race.rds"))
saveRDS(data_combined_agg_act_limit_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_act_limit_race.rds"))


######## 7. Prepare data for asthma and depression
#### Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
## First for asthma
data_missing_asthma_race <- data_combined[(is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(asthma) | is.na(edu_code) | is.na(marital_ushd))]
data_combined_asthma_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(asthma) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_asthma_race) + nrow(data_combined_asthma_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_asthma_race) + nrow(data_combined_asthma_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Then for depression
data_missing_depression_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(depression) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_depression_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(depression) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_depression_race) + nrow(data_combined_depression_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_depression_race) + nrow(data_combined_depression_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 8. Collapse asthma and depression by county-year-age-sex and either race/ethnicity or education
## First for asthma
data_combined_asthma_race_weighted <- data_combined_asthma_race[, list(asthma_count = sum(asthma * re_weight), asthma_weighted_count = weighted.mean(asthma, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Then for depression
data_combined_depression_race_weighted <- data_combined_depression_race[, list(depression_count = sum(depression * re_weight), depression_weighted_count = weighted.mean(depression, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

#### Save collapsed data sets
saveRDS(data_missing_asthma_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_asthma_race.rds"))
saveRDS(data_combined_asthma_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_asthma_race.rds"))

saveRDS(data_missing_treated_asthma_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_treated_asthma_race.rds"))
saveRDS(data_combined_treated_asthma_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_treated_asthma_race.rds"))

saveRDS(data_missing_depression_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_depression_race.rds"))
saveRDS(data_combined_depression_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_depression_race.rds"))

saveRDS(data_missing_treated_depression_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_treated_depression_race.rds"))
saveRDS(data_combined_treated_depression_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_treated_depression_race.rds"))


######## 9. Prepare data for worry, stress, and pain
#### Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
## First for physical pain
data_missing_pain_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(exp_physical_pain) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_pain_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(exp_physical_pain) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_pain_race) + nrow(data_combined_pain_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_pain_race) + nrow(data_combined_pain_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Then for stress
data_missing_stress_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(exp_stress) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_stress_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(exp_stress) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_stress_race) + nrow(data_combined_stress_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_stress_race) + nrow(data_combined_stress_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Then for worry
data_missing_worry_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(exp_worry) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_worry_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(exp_worry) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_worry_race) + nrow(data_combined_worry_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_worry_race) + nrow(data_combined_worry_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 10. Collapse worry, stress and pain by county-year-age-sex and either race/ethnicity or education
## First for pain
data_combined_pain_race_weighted <- data_combined_pain_race[, list(exp_physical_pain_count = sum(exp_physical_pain * re_weight), exp_physical_pain_weighted_count = weighted.mean(exp_physical_pain, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Then for stress
data_combined_stress_race_weighted <- data_combined_stress_race[, list(exp_stress_count = sum(exp_stress * re_weight), exp_stress_weighted_count = weighted.mean(exp_stress, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Then for worry
data_combined_worry_race_weighted <- data_combined_worry_race[, list(exp_worry_count = sum(exp_worry * re_weight), exp_worry_weighted_count = weighted.mean(exp_worry, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

#### Save collapsed data sets
saveRDS(data_missing_pain_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_pain_race.rds"))
saveRDS(data_combined_pain_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_pain_race.rds"))

saveRDS(data_missing_stress_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_stress_race.rds"))
saveRDS(data_combined_stress_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_stress_race.rds"))

saveRDS(data_missing_worry_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_worry_race.rds"))
saveRDS(data_combined_worry_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_worry_race.rds"))


######## 11. Prepare data for diabetes
#### Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
## First for diabetes
data_missing_diabetes_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(diabetes) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_diabetes_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(diabetes) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_diabetes_race) + nrow(data_combined_diabetes_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_diabetes_race) + nrow(data_combined_diabetes_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 12. Collapse diabetes by county-year-age-sex and either race/ethnicity or education
## First for diabetes
data_combined_diabetes_race_weighted <- data_combined_diabetes_race[, list(diabetes_count = sum(diabetes * re_weight), diabetes_weighted_count = weighted.mean(diabetes, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

#### Save collapsed data sets
saveRDS(data_missing_diabetes_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_diabetes_race.rds"))
saveRDS(data_combined_diabetes_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_diabetes_race.rds"))


######## 13. Produce direct estimates by race/ethnicity
#### Produce mcnty-level direct estimates
mcnty_estimates <- data_combined[, list(level = "mcnty", gen_health_45 = weighted.mean(gen_health_45, re_weight * weights, na.rm = TRUE), 
                                        frequent_activity_limitations = weighted.mean(frequent_activity_limitations, re_weight * weights, na.rm = TRUE),
                                        asthma = weighted.mean(asthma, re_weight * weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, re_weight * weights, na.rm = TRUE),
                                        depression = weighted.mean(depression, re_weight * weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, re_weight * weights, na.rm = TRUE),
                                        pain = weighted.mean(exp_physical_pain, re_weight * weights, na.rm = TRUE), worry = weighted.mean(exp_worry, re_weight * weights, na.rm = TRUE),
                                        stress = weighted.mean(exp_stress, re_weight * weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, re_weight * weights, na.rm = TRUE),
                                        cancer = weighted.mean(cancer, re_weight * weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, re_weight * weights, na.rm = TRUE),
                                        weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(mcnty, state, state_name, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

mcnty_estimates_no_strat <- data_combined[, list(level = "mcnty", gen_health_45 = weighted.mean(gen_health_45, re_weight * weights, na.rm = TRUE), 
                                                 frequent_activity_limitations = weighted.mean(frequent_activity_limitations, re_weight * weights, na.rm = TRUE),
                                                 asthma = weighted.mean(asthma, re_weight * weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, re_weight * weights, na.rm = TRUE),
                                                 depression = weighted.mean(depression, re_weight * weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, re_weight * weights, na.rm = TRUE),
                                                 pain = weighted.mean(exp_physical_pain, re_weight * weights, na.rm = TRUE), worry = weighted.mean(exp_worry, re_weight * weights, na.rm = TRUE),
                                                 stress = weighted.mean(exp_stress, re_weight * weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, re_weight * weights, na.rm = TRUE),
                                                 cancer = weighted.mean(cancer, re_weight * weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, re_weight * weights, na.rm = TRUE),
                                                 weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(mcnty, state, state_name, year, sex, race, age_bin, nid)]

#### Produce state-level direct estimates
state_estimates <- data_combined[, list(level = "state", gen_health_45 = weighted.mean(gen_health_45, re_weight * weights, na.rm = TRUE), 
                                        frequent_activity_limitations = weighted.mean(frequent_activity_limitations, re_weight * weights, na.rm = TRUE),
                                        asthma = weighted.mean(asthma, re_weight * weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, re_weight * weights, na.rm = TRUE),
                                        depression = weighted.mean(depression, re_weight * weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, re_weight * weights, na.rm = TRUE),
                                        pain = weighted.mean(exp_physical_pain, re_weight * weights, na.rm = TRUE), worry = weighted.mean(exp_worry, re_weight * weights, na.rm = TRUE),
                                        stress = weighted.mean(exp_stress, re_weight * weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, re_weight * weights, na.rm = TRUE),
                                        cancer = weighted.mean(cancer, re_weight * weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, re_weight * weights, na.rm = TRUE),
                                        weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(state, state_name, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

state_estimates_no_strat <- data_combined[, list(level = "state", gen_health_45 = weighted.mean(gen_health_45, re_weight * weights, na.rm = TRUE),
                                                 frequent_activity_limitations = weighted.mean(frequent_activity_limitations, re_weight * weights, na.rm = TRUE),
                                                 asthma = weighted.mean(asthma, re_weight * weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, re_weight * weights, na.rm = TRUE),
                                                 depression = weighted.mean(depression, re_weight * weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, re_weight * weights, na.rm = TRUE),
                                                 pain = weighted.mean(exp_physical_pain, re_weight * weights, na.rm = TRUE), worry = weighted.mean(exp_worry, re_weight * weights, na.rm = TRUE),
                                                 stress = weighted.mean(exp_stress, re_weight * weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, re_weight * weights, na.rm = TRUE),
                                                 cancer = weighted.mean(cancer, re_weight * weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, re_weight * weights, na.rm = TRUE),
                                                 weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(state, state_name, year, sex, race, age_bin, nid)]

#### Produce national-level direct estimates
# The weight object here should be nationally-representative
national_estimates <- data_combined[, list(level = "natl", gen_health_45 = weighted.mean(gen_health_45, re_weight * weights, na.rm = TRUE), 
                                           frequent_activity_limitations = weighted.mean(frequent_activity_limitations, re_weight * weights, na.rm = TRUE),
                                           asthma = weighted.mean(asthma, re_weight * weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, re_weight * weights, na.rm = TRUE),
                                           depression = weighted.mean(depression, re_weight * weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, re_weight * weights, na.rm = TRUE),
                                           pain = weighted.mean(exp_physical_pain, re_weight * weights, na.rm = TRUE), worry = weighted.mean(exp_worry, re_weight * weights, na.rm = TRUE),
                                           stress = weighted.mean(exp_stress, re_weight * weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, re_weight * weights, na.rm = TRUE),
                                           cancer = weighted.mean(cancer, re_weight * weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, re_weight * weights, na.rm = TRUE),
                                           weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(year, sex, race, age_bin, edu_code, marital_ushd, nid)]

national_estimates_no_strat <- data_combined[, list(level = "natl", gen_health_45 = weighted.mean(gen_health_45, re_weight * weights, na.rm = TRUE), 
                                                    frequent_activity_limitations = weighted.mean(frequent_activity_limitations, re_weight * weights, na.rm = TRUE),
                                                    asthma = weighted.mean(asthma, re_weight * weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, re_weight * weights, na.rm = TRUE),
                                                    depression = weighted.mean(depression, re_weight * weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, re_weight * weights, na.rm = TRUE),
                                                    pain = weighted.mean(exp_physical_pain, re_weight * weights, na.rm = TRUE), worry = weighted.mean(exp_worry, re_weight * weights, na.rm = TRUE),
                                                    stress = weighted.mean(exp_stress, re_weight * weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, re_weight * weights, na.rm = TRUE),
                                                    cancer = weighted.mean(cancer, re_weight * weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, re_weight * weights, na.rm = TRUE),
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
sex_agg <- direct_estimates[, list(sex = 3, gen_health_45 = weighted.mean(gen_health_45, weights, na.rm = TRUE), 
                                   frequent_activity_limitations = weighted.mean(frequent_activity_limitations, weights, na.rm = TRUE),
                                   asthma = weighted.mean(asthma, weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, weights, na.rm = TRUE),
                                   depression = weighted.mean(depression, weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, weights, na.rm = TRUE),
                                   pain = weighted.mean(pain, weights, na.rm = TRUE), worry = weighted.mean(worry, weights, na.rm = TRUE),
                                   stress = weighted.mean(stress, weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, weights, na.rm = TRUE),
                                   cancer = weighted.mean(cancer, weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, weights, na.rm = TRUE),
                                   weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, year, race, age_bin, edu_code, marital_ushd, nid)]
direct_estimates <- rbindlist(list(direct_estimates, sex_agg), use.names = TRUE, fill = TRUE)

sex_agg_no_strat <- direct_estimates_no_strat[, list(sex = 3, gen_health_45 = weighted.mean(gen_health_45, weights, na.rm = TRUE), 
                                                     frequent_activity_limitations = weighted.mean(frequent_activity_limitations, weights, na.rm = TRUE),
                                                     asthma = weighted.mean(asthma, weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, weights, na.rm = TRUE),
                                                     depression = weighted.mean(depression, weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, weights, na.rm = TRUE),
                                                     pain = weighted.mean(pain, weights, na.rm = TRUE), worry = weighted.mean(worry, weights, na.rm = TRUE),
                                                     stress = weighted.mean(stress, weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, weights, na.rm = TRUE),
                                                     cancer = weighted.mean(cancer, weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, weights, na.rm = TRUE),
                                                     weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, year, race, age_bin, nid)]
direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, sex_agg_no_strat), use.names = TRUE, fill = TRUE)

#### Aggregate across races
race_agg <- direct_estimates[, list(race = 1, gen_health_45 = weighted.mean(gen_health_45, weights, na.rm = TRUE), 
                                    frequent_activity_limitations = weighted.mean(frequent_activity_limitations, weights, na.rm = TRUE),
                                    asthma = weighted.mean(asthma, weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, weights, na.rm = TRUE),
                                    depression = weighted.mean(depression, weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, weights, na.rm = TRUE),
                                    pain = weighted.mean(pain, weights, na.rm = TRUE), worry = weighted.mean(worry, weights, na.rm = TRUE),
                                    stress = weighted.mean(stress, weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, weights, na.rm = TRUE),
                                    cancer = weighted.mean(cancer, weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, weights, na.rm = TRUE),
                                    weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, year, sex, age_bin, edu_code, marital_ushd, nid)]
direct_estimates <- rbindlist(list(direct_estimates, race_agg), use.names = TRUE, fill = TRUE)

race_agg_no_strat <- direct_estimates_no_strat[, list(race = 1, gen_health_45 = weighted.mean(gen_health_45, weights, na.rm = TRUE), 
                                                      frequent_activity_limitations = weighted.mean(frequent_activity_limitations, weights, na.rm = TRUE),
                                                      asthma = weighted.mean(asthma, weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, weights, na.rm = TRUE),
                                                      depression = weighted.mean(depression, weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, weights, na.rm = TRUE),
                                                      pain = weighted.mean(pain, weights, na.rm = TRUE), worry = weighted.mean(worry, weights, na.rm = TRUE),
                                                      stress = weighted.mean(stress, weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, weights, na.rm = TRUE),
                                                      cancer = weighted.mean(cancer, weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, weights, na.rm = TRUE),
                                                      weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, year, sex, age_bin, nid)]
direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, race_agg_no_strat), use.names = TRUE, fill = TRUE)

#### Aggregate across ages
age_agg <- direct_estimates[, list(age_bin = 98, gen_health_45 = weighted.mean(gen_health_45, weights, na.rm = TRUE), 
                                   frequent_activity_limitations = weighted.mean(frequent_activity_limitations, weights, na.rm = TRUE),
                                   asthma = weighted.mean(asthma, weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, weights, na.rm = TRUE),
                                   depression = weighted.mean(depression, weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, weights, na.rm = TRUE),
                                   pain = weighted.mean(pain, weights, na.rm = TRUE), worry = weighted.mean(worry, weights, na.rm = TRUE),
                                   stress = weighted.mean(stress, weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, weights, na.rm = TRUE),
                                   cancer = weighted.mean(cancer, weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, weights, na.rm = TRUE),
                                   weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, year, sex, race, edu_code, marital_ushd, nid)]
direct_estimates <- rbindlist(list(direct_estimates, age_agg), use.names = TRUE, fill = TRUE)

age_agg_no_strat <- direct_estimates_no_strat[, list(age_bin = 98, gen_health_45 = weighted.mean(gen_health_45, weights, na.rm = TRUE), 
                                                     frequent_activity_limitations = weighted.mean(frequent_activity_limitations, weights, na.rm = TRUE),
                                                     asthma = weighted.mean(asthma, weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, weights, na.rm = TRUE),
                                                     depression = weighted.mean(depression, weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, weights, na.rm = TRUE),
                                                     pain = weighted.mean(pain, weights, na.rm = TRUE), worry = weighted.mean(worry, weights, na.rm = TRUE),
                                                     stress = weighted.mean(stress, weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, weights, na.rm = TRUE),
                                                     cancer = weighted.mean(cancer, weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, weights, na.rm = TRUE),
                                                     weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, year, sex, race, nid)]
direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, age_agg_no_strat), use.names = TRUE, fill = TRUE)

#### Aggregate across edu
message("Aggregate across edu")
edu_agg <- direct_estimates[, list(edu_code = 9, gen_health_45 = weighted.mean(gen_health_45, weights, na.rm = TRUE), 
                                   frequent_activity_limitations = weighted.mean(frequent_activity_limitations, weights, na.rm = TRUE),
                                   asthma = weighted.mean(asthma, weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, weights, na.rm = TRUE),
                                   depression = weighted.mean(depression, weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, weights, na.rm = TRUE),
                                   pain = weighted.mean(pain, weights, na.rm = TRUE), worry = weighted.mean(worry, weights, na.rm = TRUE),
                                   stress = weighted.mean(stress, weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, weights, na.rm = TRUE),
                                   cancer = weighted.mean(cancer, weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, weights, na.rm = TRUE),
                                   weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, age_bin, sex, race, year, marital_ushd)]
direct_estimates <- rbindlist(list(direct_estimates, edu_agg), use.names = TRUE, fill = TRUE)

#### Aggregate across marital
message("Aggregate across marital")
marital_agg <- direct_estimates[, list(marital_ushd = 9, gen_health_45 = weighted.mean(gen_health_45, weights, na.rm = TRUE), 
                                       frequent_activity_limitations = weighted.mean(frequent_activity_limitations, weights, na.rm = TRUE),
                                       asthma = weighted.mean(asthma, weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, weights, na.rm = TRUE),
                                       depression = weighted.mean(depression, weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, weights, na.rm = TRUE),
                                       pain = weighted.mean(pain, weights, na.rm = TRUE), worry = weighted.mean(worry, weights, na.rm = TRUE),
                                       stress = weighted.mean(stress, weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, weights, na.rm = TRUE),
                                       cancer = weighted.mean(cancer, weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, weights, na.rm = TRUE),
                                       weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, age_bin, sex, race, year, edu_code)]
direct_estimates <- rbindlist(list(direct_estimates, marital_agg), use.names = TRUE, fill = TRUE)

#### Rename age column
setnames(direct_estimates, "age_bin", "age")
setnames(direct_estimates_no_strat, "age_bin", "age")

#### Save direct estimates
saveRDS(direct_estimates, file = paste0(output_dir, "/gallup_2000_2019_direct_estimates.rds"))
saveRDS(direct_estimates_no_strat, file = paste0(output_dir, "/gallup_2000_2019_direct_estimates_no_strat.rds"))
