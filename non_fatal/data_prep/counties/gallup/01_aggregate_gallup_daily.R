###########################################################################################################################################
## Description: Create county-year-age-sex-race/ethnicity aggregates of self-reported health data from Gallup Daily, using previously-
##              extracted individual-level data.
##
## Input:       None
## Output:      Combined data files for Gallup Daily, with requisite demographic and general health data. Data are saved to the Limited Use
##              folder in FILEPATH.
##
###########################################################################################################################################

######## 1. Setup
#### Load required packages installed in Singularity image
pacman::p_load(data.table, haven, ggplot2, stringr, car, R.utils, doParallel, boot)
pacman::p_load(rgdal, ggthemes, cowplot, mapproj, maptools, rgeos, RColorBrewer)

.libPaths(c("FILEPATH", .libPaths())) # Switch lib path to user folder
pacman::p_load(psych) # Load custom-installed packages

#### Set paths
data_version <- ""
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

#### Clean phone type
data_combined[sample == "landline", sample_type := 1] # Landline
data_combined[sample == "cellphone", sample_type := 2] # Cellphone

#### Update conditional treatment variables (asthma, cancer, depression)
data_combined[asthma == 0 & year %in% unique(data_combined[!is.na(treated_asthma), year]), treated_asthma := 0]
data_combined[cancer == 0 & year %in% unique(data_combined[!is.na(treated_cancer), year]), treated_cancer := 0]
data_combined[depression == 0 & year %in% unique(data_combined[!is.na(treated_depression), year]), treated_depression := 0]

#### Create dummy codes for general health
## Dummy code values of general health variable
data_combined[!(gen_health %in% 1:5), gen_health := NA]
gen_health_dummy <- as.data.table(dummy.code(data_combined$gen_health))
colnames(gen_health_dummy) <- paste0("gen_health_", colnames(gen_health_dummy))
data_combined <- cbind(data_combined, gen_health_dummy)

#### Drop existing genhealth_45 and genhealth_12 indicators
data_combined[, c("genhealth_45", "genhealth_12") := NULL]

#### Create indicator variables for fair/poor health and for excellent/very good health
data_combined[, gen_health_45 := as.integer(gen_health %in% 4:5)]
data_combined[is.na(gen_health), gen_health_45 := NA]
data_combined[, gen_health_12 := as.integer(gen_health %in% 1:2)]
data_combined[is.na(gen_health), gen_health_12 := NA]

#### Create a binary indicator indicating >= 14 days of poor health
setnames(data_combined, "activity_limited_days", "activity_limit_days")
data_combined[!is.na(activity_limit_days), frequent_activity_limitations := as.integer(activity_limit_days >= 14)]

## Dummy code insurance type variables
data_combined[!(prim_insur_type %in% 1:7), prim_insur_type := NA]
prim_insur_type_dummy <- as.data.table(dummy.code(data_combined$prim_insur_type))
colnames(prim_insur_type_dummy) <- paste0("prim_insur_type_", colnames(prim_insur_type_dummy))
data_combined <- cbind(data_combined, prim_insur_type_dummy)

data_combined[!(sec_insur_type %in% 1:7), sec_insur_type := NA]
sec_insur_type_dummy <- as.data.table(dummy.code(data_combined$sec_insur_type))
colnames(sec_insur_type_dummy) <- paste0("sec_insur_type_", colnames(sec_insur_type_dummy))
data_combined <- cbind(data_combined, sec_insur_type_dummy)

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


######## 3. Prepare data for general health analysis
## Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_gen_health_race <- data_combined[(is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(edu_code) | is.na(age_bin) | is.na(marital_ushd) | is.na(gen_health))]
data_combined_gen_health_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(edu_code) & !is.na(age_bin) & !is.na(marital_ushd) & !is.na(gen_health)]
if (nrow(data_missing_gen_health_race) + nrow(data_combined_gen_health_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_gen_health_race) + nrow(data_combined_gen_health_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 4. Collapse general health by county-year-age-sex, race/ethnicity, education and other post-stratification vars
data_combined_agg_gen_health_race_weighted <- data_combined_gen_health_race[, list(gen_health_45_count = sum(gen_health_45 * re_weight), gen_health_45_weighted_count = weighted.mean(gen_health_45, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight, na.rm = TRUE)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

#### Save collapsed data sets
saveRDS(data_missing_gen_health_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_gen_health_race.rds"))
saveRDS(data_combined_agg_gen_health_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_gen_health_race.rds"))


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

## Then for treated_asthma
data_missing_treated_asthma_race <- data_combined[(is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(treated_asthma) | is.na(edu_code) | is.na(marital_ushd))]
data_combined_treated_asthma_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(treated_asthma) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_treated_asthma_race) + nrow(data_combined_treated_asthma_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_treated_asthma_race) + nrow(data_combined_treated_asthma_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Then for depression
data_missing_depression_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(depression) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_depression_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(depression) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_depression_race) + nrow(data_combined_depression_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_depression_race) + nrow(data_combined_depression_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Then for treated depression
data_missing_treated_depression_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(treated_depression) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_treated_depression_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(treated_depression) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_treated_depression_race) + nrow(data_combined_treated_depression_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_treated_depression_race) + nrow(data_combined_treated_depression_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 8. Collapse asthma and depression by county-year-age-sex and either race/ethnicity or education
## First for asthma
data_combined_asthma_race_weighted <- data_combined_asthma_race[, list(asthma_count = sum(asthma * re_weight), asthma_weighted_count = weighted.mean(asthma, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Then for treated_asthma
data_combined_treated_asthma_race_weighted <- data_combined_treated_asthma_race[, list(treated_asthma_count = sum(treated_asthma * re_weight), treated_asthma_weighted_count = weighted.mean(treated_asthma, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Then for depression
data_combined_depression_race_weighted <- data_combined_depression_race[, list(depression_count = sum(depression * re_weight), depression_weighted_count = weighted.mean(depression, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Then for treated_depression
data_combined_treated_depression_race_weighted <- data_combined_treated_depression_race[, list(treated_depression_count = sum(treated_depression * re_weight), treated_depression_weighted_count = weighted.mean(treated_depression, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

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


######## 11. Prepare data for diabetes, cancer, treated_cancer
#### Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
## First for diabetes
data_missing_diabetes_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(diabetes) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_diabetes_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(diabetes) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_diabetes_race) + nrow(data_combined_diabetes_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_diabetes_race) + nrow(data_combined_diabetes_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Then for cancer
data_missing_cancer_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(cancer) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_cancer_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(cancer) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_cancer_race) + nrow(data_combined_cancer_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_cancer_race) + nrow(data_combined_cancer_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Then for treated_cancer
data_missing_treated_cancer_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(treated_cancer) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_treated_cancer_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(treated_cancer) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_treated_cancer_race) + nrow(data_combined_treated_cancer_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_treated_cancer_race) + nrow(data_combined_treated_cancer_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 12. Collapse diabetes, cancer, treated_cancer by county-year-age-sex and either race/ethnicity or education
## First for diabetes
data_combined_diabetes_race_weighted <- data_combined_diabetes_race[, list(diabetes_count = sum(diabetes * re_weight), diabetes_weighted_count = weighted.mean(diabetes, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Then for cancer
data_combined_cancer_race_weighted <- data_combined_cancer_race[, list(cancer_count = sum(cancer * re_weight), cancer_weighted_count = weighted.mean(cancer, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Then for treated_cancer
data_combined_treated_cancer_race_weighted <- data_combined_treated_cancer_race[, list(treated_cancer_count = sum(treated_cancer * re_weight), treated_cancer_weighted_count = weighted.mean(treated_cancer, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

#### Save collapsed data sets
saveRDS(data_missing_diabetes_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_diabetes_race.rds"))
saveRDS(data_combined_diabetes_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_diabetes_race.rds"))

saveRDS(data_missing_cancer_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_cancer_race.rds"))
saveRDS(data_combined_cancer_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_cancer_race.rds"))

saveRDS(data_missing_treated_cancer_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_treated_cancer_race.rds"))
saveRDS(data_combined_treated_cancer_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_treated_cancer_race.rds"))


######## 13. Prepare data for health care access and utilization variables
#### Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
## Dentist
data_missing_dentist_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(dentist) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_dentist_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(dentist) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_dentist_race) + nrow(data_combined_dentist_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_dentist_race) + nrow(data_combined_dentist_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Personal doctor
data_missing_personal_doc_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(personal_doc) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_personal_doc_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(personal_doc) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_personal_doc_race) + nrow(data_combined_personal_doc_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_personal_doc_race) + nrow(data_combined_personal_doc_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Health plan
data_missing_hlthplan_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(hlthplan) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_hlthplan_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(hlthplan) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_hlthplan_race) + nrow(data_combined_hlthplan_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_hlthplan_race) + nrow(data_combined_hlthplan_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Secondary insurance
data_missing_sec_insur_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(sec_insur) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_sec_insur_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(sec_insur) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_sec_insur_race) + nrow(data_combined_sec_insur_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_sec_insur_race) + nrow(data_combined_sec_insur_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Primary insurance type: Employer
data_missing_prim_insur_type_1_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(prim_insur_type_1) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_prim_insur_type_1_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(prim_insur_type_1) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_prim_insur_type_1_race) + nrow(data_combined_prim_insur_type_1_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_prim_insur_type_1_race) + nrow(data_combined_prim_insur_type_1_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Primary insurance type: Union
data_missing_prim_insur_type_2_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(prim_insur_type_2) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_prim_insur_type_2_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(prim_insur_type_2) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_prim_insur_type_2_race) + nrow(data_combined_prim_insur_type_2_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_prim_insur_type_2_race) + nrow(data_combined_prim_insur_type_2_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Primary insurance type: Medicare
data_missing_prim_insur_type_3_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(prim_insur_type_3) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_prim_insur_type_3_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(prim_insur_type_3) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_prim_insur_type_3_race) + nrow(data_combined_prim_insur_type_3_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_prim_insur_type_3_race) + nrow(data_combined_prim_insur_type_3_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Primary insurance type: Medicaid
data_missing_prim_insur_type_4_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(prim_insur_type_4) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_prim_insur_type_4_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(prim_insur_type_4) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_prim_insur_type_4_race) + nrow(data_combined_prim_insur_type_4_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_prim_insur_type_4_race) + nrow(data_combined_prim_insur_type_4_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Primary insurance type: Military/Veteran's
data_missing_prim_insur_type_5_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(prim_insur_type_5) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_prim_insur_type_5_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(prim_insur_type_5) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_prim_insur_type_5_race) + nrow(data_combined_prim_insur_type_5_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_prim_insur_type_5_race) + nrow(data_combined_prim_insur_type_5_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Primary insurance type: Personally paid
data_missing_prim_insur_type_6_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(prim_insur_type_6) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_prim_insur_type_6_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(prim_insur_type_6) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_prim_insur_type_6_race) + nrow(data_combined_prim_insur_type_6_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_prim_insur_type_6_race) + nrow(data_combined_prim_insur_type_6_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Primary insurance type: Other
data_missing_prim_insur_type_7_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(prim_insur_type_7) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_prim_insur_type_7_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(prim_insur_type_7) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_prim_insur_type_7_race) + nrow(data_combined_prim_insur_type_7_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_prim_insur_type_7_race) + nrow(data_combined_prim_insur_type_7_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Secondary insurance type: Employer
data_missing_sec_insur_type_1_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(sec_insur_type_1) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_sec_insur_type_1_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(sec_insur_type_1) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_sec_insur_type_1_race) + nrow(data_combined_sec_insur_type_1_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_sec_insur_type_1_race) + nrow(data_combined_sec_insur_type_1_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Secondary insurance type: Union
data_missing_sec_insur_type_2_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(sec_insur_type_2) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_sec_insur_type_2_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(sec_insur_type_2) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_sec_insur_type_2_race) + nrow(data_combined_sec_insur_type_2_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_sec_insur_type_2_race) + nrow(data_combined_sec_insur_type_2_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Secondary insurance type: Medicare
data_missing_sec_insur_type_3_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(sec_insur_type_3) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_sec_insur_type_3_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(sec_insur_type_3) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_sec_insur_type_3_race) + nrow(data_combined_sec_insur_type_3_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_sec_insur_type_3_race) + nrow(data_combined_sec_insur_type_3_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Secondary insurance type: Medicaid
data_missing_sec_insur_type_4_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(sec_insur_type_4) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_sec_insur_type_4_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(sec_insur_type_4) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_sec_insur_type_4_race) + nrow(data_combined_sec_insur_type_4_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_sec_insur_type_4_race) + nrow(data_combined_sec_insur_type_4_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Secondary insurance type: Military/Veteran's
data_missing_sec_insur_type_5_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(sec_insur_type_5) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_sec_insur_type_5_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(sec_insur_type_5) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_sec_insur_type_5_race) + nrow(data_combined_sec_insur_type_5_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_sec_insur_type_5_race) + nrow(data_combined_sec_insur_type_5_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Secondary insurance type: Personally paid
data_missing_sec_insur_type_6_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(sec_insur_type_6) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_sec_insur_type_6_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(sec_insur_type_6) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_sec_insur_type_6_race) + nrow(data_combined_sec_insur_type_6_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_sec_insur_type_6_race) + nrow(data_combined_sec_insur_type_6_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

## Secondary insurance type: Other
data_missing_sec_insur_type_7_race <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(sec_insur_type_7) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_sec_insur_type_7_race <- data_combined[!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(sec_insur_type_7) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_sec_insur_type_7_race) + nrow(data_combined_sec_insur_type_7_race) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_sec_insur_type_7_race) + nrow(data_combined_sec_insur_type_7_race), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}


######## 14. Collapse health care access and utilization variables
## Dentist
data_combined_dentist_race_weighted <- data_combined_dentist_race[, list(dentist_count = sum(dentist * re_weight), dentist_weighted_count = weighted.mean(dentist, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Personal doctor
data_combined_personal_doc_race_weighted <- data_combined_personal_doc_race[, list(personal_doc_count = sum(personal_doc * re_weight), personal_doc_weighted_count = weighted.mean(personal_doc, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Health plan
data_combined_hlthplan_race_weighted <- data_combined_hlthplan_race[, list(hlthplan_count = sum(hlthplan * re_weight), hlthplan_weighted_count = weighted.mean(hlthplan, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Secondary insurance
data_combined_sec_insur_race_weighted <- data_combined_sec_insur_race[, list(sec_insur_count = sum(sec_insur * re_weight), sec_insur_weighted_count = weighted.mean(sec_insur, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Primary insurance type: Employer
data_combined_prim_insur_type_1_race_weighted <- data_combined_prim_insur_type_1_race[, list(prim_insur_type_1_count = sum(prim_insur_type_1 * re_weight), prim_insur_type_1_weighted_count = weighted.mean(prim_insur_type_1, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Primary insurance type: Union
data_combined_prim_insur_type_2_race_weighted <- data_combined_prim_insur_type_2_race[, list(prim_insur_type_2_count = sum(prim_insur_type_2 * re_weight), prim_insur_type_2_weighted_count = weighted.mean(prim_insur_type_2, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Primary insurance type: Medicare
data_combined_prim_insur_type_3_race_weighted <- data_combined_prim_insur_type_3_race[, list(prim_insur_type_3_count = sum(prim_insur_type_3 * re_weight), prim_insur_type_3_weighted_count = weighted.mean(prim_insur_type_3, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Primary insurance type: Medicaid
data_combined_prim_insur_type_4_race_weighted <- data_combined_prim_insur_type_4_race[, list(prim_insur_type_4_count = sum(prim_insur_type_4 * re_weight), prim_insur_type_4_weighted_count = weighted.mean(prim_insur_type_4, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Primary insurance type: Military/Veteran's
data_combined_prim_insur_type_5_race_weighted <- data_combined_prim_insur_type_5_race[, list(prim_insur_type_5_count = sum(prim_insur_type_5 * re_weight), prim_insur_type_5_weighted_count = weighted.mean(prim_insur_type_5, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Primary insurance type: Self-paid
data_combined_prim_insur_type_6_race_weighted <- data_combined_prim_insur_type_6_race[, list(prim_insur_type_6_count = sum(prim_insur_type_6 * re_weight), prim_insur_type_6_weighted_count = weighted.mean(prim_insur_type_6, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Primary insurance type: Other
data_combined_prim_insur_type_7_race_weighted <- data_combined_prim_insur_type_7_race[, list(prim_insur_type_7_count = sum(prim_insur_type_7 * re_weight), prim_insur_type_7_weighted_count = weighted.mean(prim_insur_type_7, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Secondary insurance type: Employer
data_combined_sec_insur_type_1_race_weighted <- data_combined_sec_insur_type_1_race[, list(sec_insur_type_1_count = sum(sec_insur_type_1 * re_weight), sec_insur_type_1_weighted_count = weighted.mean(sec_insur_type_1, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Secondary insurance type: Union
data_combined_sec_insur_type_2_race_weighted <- data_combined_sec_insur_type_2_race[, list(sec_insur_type_2_count = sum(sec_insur_type_2 * re_weight), sec_insur_type_2_weighted_count = weighted.mean(sec_insur_type_2, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Secondary insurance type: Medicare
data_combined_sec_insur_type_3_race_weighted <- data_combined_sec_insur_type_3_race[, list(sec_insur_type_3_count = sum(sec_insur_type_3 * re_weight), sec_insur_type_3_weighted_count = weighted.mean(sec_insur_type_3, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Secondary insurance type: Medicaid
data_combined_sec_insur_type_4_race_weighted <- data_combined_sec_insur_type_4_race[, list(sec_insur_type_4_count = sum(sec_insur_type_4 * re_weight), sec_insur_type_4_weighted_count = weighted.mean(sec_insur_type_4, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Secondary insurance type: Military/Veteran's
data_combined_sec_insur_type_5_race_weighted <- data_combined_sec_insur_type_5_race[, list(sec_insur_type_5_count = sum(sec_insur_type_5 * re_weight), sec_insur_type_5_weighted_count = weighted.mean(sec_insur_type_5, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Secondary insurance type: Self-paid
data_combined_sec_insur_type_6_race_weighted <- data_combined_sec_insur_type_6_race[, list(sec_insur_type_6_count = sum(sec_insur_type_6 * re_weight), sec_insur_type_6_weighted_count = weighted.mean(sec_insur_type_6, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

## Secondary insurance type: Other
data_combined_sec_insur_type_7_race_weighted <- data_combined_sec_insur_type_7_race[, list(sec_insur_type_7_count = sum(sec_insur_type_7 * re_weight), sec_insur_type_7_weighted_count = weighted.mean(sec_insur_type_7, weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = list(mcnty, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

#### Save collapsed data sets
saveRDS(data_missing_dentist_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_dentist_race.rds"))
saveRDS(data_combined_dentist_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_dentist_race.rds"))

saveRDS(data_missing_personal_doc_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_personal_doc_race.rds"))
saveRDS(data_combined_personal_doc_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_personal_doc_race.rds"))

saveRDS(data_missing_hlthplan_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_hlthplan_race.rds"))
saveRDS(data_combined_hlthplan_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_hlthplan_race.rds"))

saveRDS(data_missing_sec_insur_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_sec_insur_race.rds"))
saveRDS(data_combined_sec_insur_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_sec_insur_race.rds"))

saveRDS(data_missing_prim_insur_type_1_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_prim_insur_type_1_race.rds"))
saveRDS(data_combined_prim_insur_type_1_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_prim_insur_type_1_race.rds"))

saveRDS(data_missing_prim_insur_type_2_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_prim_insur_type_2_race.rds"))
saveRDS(data_combined_prim_insur_type_2_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_prim_insur_type_2_race.rds"))

saveRDS(data_missing_prim_insur_type_3_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_prim_insur_type_3_race.rds"))
saveRDS(data_combined_prim_insur_type_3_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_prim_insur_type_3_race.rds"))

saveRDS(data_missing_prim_insur_type_4_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_prim_insur_type_4_race.rds"))
saveRDS(data_combined_prim_insur_type_4_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_prim_insur_type_4_race.rds"))

saveRDS(data_missing_prim_insur_type_5_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_prim_insur_type_5_race.rds"))
saveRDS(data_combined_prim_insur_type_5_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_prim_insur_type_5_race.rds"))

saveRDS(data_missing_prim_insur_type_6_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_prim_insur_type_6_race.rds"))
saveRDS(data_combined_prim_insur_type_6_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_prim_insur_type_6_race.rds"))

saveRDS(data_missing_prim_insur_type_7_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_prim_insur_type_7_race.rds"))
saveRDS(data_combined_prim_insur_type_7_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_prim_insur_type_7_race.rds"))

saveRDS(data_missing_sec_insur_type_1_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_sec_insur_type_1_race.rds"))
saveRDS(data_combined_sec_insur_type_1_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_sec_insur_type_1_race.rds"))

saveRDS(data_missing_sec_insur_type_2_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_sec_insur_type_2_race.rds"))
saveRDS(data_combined_sec_insur_type_2_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_sec_insur_type_2_race.rds"))

saveRDS(data_missing_sec_insur_type_3_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_sec_insur_type_3_race.rds"))
saveRDS(data_combined_sec_insur_type_3_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_sec_insur_type_3_race.rds"))

saveRDS(data_missing_sec_insur_type_4_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_sec_insur_type_4_race.rds"))
saveRDS(data_combined_sec_insur_type_4_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_sec_insur_type_4_race.rds"))

saveRDS(data_missing_sec_insur_type_5_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_sec_insur_type_5_race.rds"))
saveRDS(data_combined_sec_insur_type_5_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_sec_insur_type_5_race.rds"))

saveRDS(data_missing_sec_insur_type_6_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_sec_insur_type_6_race.rds"))
saveRDS(data_combined_sec_insur_type_6_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_sec_insur_type_6_race.rds"))

saveRDS(data_missing_sec_insur_type_7_race, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_missing_data_sec_insur_type_7_race.rds"))
saveRDS(data_combined_sec_insur_type_7_race_weighted, file = paste0(output_dir, "/gallup_daily_2000_2019_agg_sec_insur_type_7_race.rds"))


######## 13. Produce direct estimates by race/ethnicity
##### Note: Phone has been dropped from these aggregations
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


######## 13. Produce direct estimates by race/ethnicity for health care access and utilization variables
#### Produce mcnty-level direct estimates
mcnty_estimates_access <- data_combined[, list(level = "mcnty", 
                                               dentist = weighted.mean(dentist, re_weight * weights, na.rm = TRUE),
                                               personal_doc = weighted.mean(personal_doc, re_weight * weights, na.rm = TRUE),
                                               hlthplan = weighted.mean(hlthplan, re_weight * weights, na.rm = TRUE),
                                               sec_insur = weighted.mean(sec_insur, re_weight * weights, na.rm = TRUE),
                                               prim_insur_type_1 = weighted.mean(prim_insur_type_1, re_weight * weights, na.rm = TRUE),
                                               prim_insur_type_2 = weighted.mean(prim_insur_type_2, re_weight * weights, na.rm = TRUE),
                                               prim_insur_type_3 = weighted.mean(prim_insur_type_3, re_weight * weights, na.rm = TRUE),
                                               prim_insur_type_4 = weighted.mean(prim_insur_type_4, re_weight * weights, na.rm = TRUE),
                                               prim_insur_type_5 = weighted.mean(prim_insur_type_5, re_weight * weights, na.rm = TRUE),
                                               prim_insur_type_6 = weighted.mean(prim_insur_type_6, re_weight * weights, na.rm = TRUE),
                                               prim_insur_type_7 = weighted.mean(prim_insur_type_7, re_weight * weights, na.rm = TRUE),
                                               sec_insur_type_1 = weighted.mean(sec_insur_type_1, re_weight * weights, na.rm = TRUE),
                                               sec_insur_type_2 = weighted.mean(sec_insur_type_2, re_weight * weights, na.rm = TRUE),
                                               sec_insur_type_3 = weighted.mean(sec_insur_type_3, re_weight * weights, na.rm = TRUE),
                                               sec_insur_type_4 = weighted.mean(sec_insur_type_4, re_weight * weights, na.rm = TRUE),
                                               sec_insur_type_5 = weighted.mean(sec_insur_type_5, re_weight * weights, na.rm = TRUE),
                                               sec_insur_type_6 = weighted.mean(sec_insur_type_6, re_weight * weights, na.rm = TRUE),
                                               sec_insur_type_7 = weighted.mean(sec_insur_type_7, re_weight * weights, na.rm = TRUE),
                                        weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(mcnty, state, state_name, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

#### Produce state-level direct estimates
state_estimates_access <- data_combined[, list(level = "state", 
                                               dentist = weighted.mean(dentist, re_weight * weights, na.rm = TRUE),
                                               personal_doc = weighted.mean(personal_doc, re_weight * weights, na.rm = TRUE),
                                               hlthplan = weighted.mean(hlthplan, re_weight * weights, na.rm = TRUE),
                                               sec_insur = weighted.mean(sec_insur, re_weight * weights, na.rm = TRUE),
                                               prim_insur_type_1 = weighted.mean(prim_insur_type_1, re_weight * weights, na.rm = TRUE),
                                               prim_insur_type_2 = weighted.mean(prim_insur_type_2, re_weight * weights, na.rm = TRUE),
                                               prim_insur_type_3 = weighted.mean(prim_insur_type_3, re_weight * weights, na.rm = TRUE),
                                               prim_insur_type_4 = weighted.mean(prim_insur_type_4, re_weight * weights, na.rm = TRUE),
                                               prim_insur_type_5 = weighted.mean(prim_insur_type_5, re_weight * weights, na.rm = TRUE),
                                               prim_insur_type_6 = weighted.mean(prim_insur_type_6, re_weight * weights, na.rm = TRUE),
                                               prim_insur_type_7 = weighted.mean(prim_insur_type_7, re_weight * weights, na.rm = TRUE),
                                               sec_insur_type_1 = weighted.mean(sec_insur_type_1, re_weight * weights, na.rm = TRUE),
                                               sec_insur_type_2 = weighted.mean(sec_insur_type_2, re_weight * weights, na.rm = TRUE),
                                               sec_insur_type_3 = weighted.mean(sec_insur_type_3, re_weight * weights, na.rm = TRUE),
                                               sec_insur_type_4 = weighted.mean(sec_insur_type_4, re_weight * weights, na.rm = TRUE),
                                               sec_insur_type_5 = weighted.mean(sec_insur_type_5, re_weight * weights, na.rm = TRUE),
                                               sec_insur_type_6 = weighted.mean(sec_insur_type_6, re_weight * weights, na.rm = TRUE),
                                               sec_insur_type_7 = weighted.mean(sec_insur_type_7, re_weight * weights, na.rm = TRUE),
                                        weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(state, state_name, year, sex, race, age_bin, edu_code, marital_ushd, nid)]

#### Produce national-level direct estimates
# The weight object here should be nationally-representative
national_estimates_access <- data_combined[, list(level = "natl", 
                                                  dentist = weighted.mean(dentist, re_weight * weights, na.rm = TRUE),
                                                  personal_doc = weighted.mean(personal_doc, re_weight * weights, na.rm = TRUE),
                                                  hlthplan = weighted.mean(hlthplan, re_weight * weights, na.rm = TRUE),
                                                  sec_insur = weighted.mean(sec_insur, re_weight * weights, na.rm = TRUE),
                                                  prim_insur_type_1 = weighted.mean(prim_insur_type_1, re_weight * weights, na.rm = TRUE),
                                                  prim_insur_type_2 = weighted.mean(prim_insur_type_2, re_weight * weights, na.rm = TRUE),
                                                  prim_insur_type_3 = weighted.mean(prim_insur_type_3, re_weight * weights, na.rm = TRUE),
                                                  prim_insur_type_4 = weighted.mean(prim_insur_type_4, re_weight * weights, na.rm = TRUE),
                                                  prim_insur_type_5 = weighted.mean(prim_insur_type_5, re_weight * weights, na.rm = TRUE),
                                                  prim_insur_type_6 = weighted.mean(prim_insur_type_6, re_weight * weights, na.rm = TRUE),
                                                  prim_insur_type_7 = weighted.mean(prim_insur_type_7, re_weight * weights, na.rm = TRUE),
                                                  sec_insur_type_1 = weighted.mean(sec_insur_type_1, re_weight * weights, na.rm = TRUE),
                                                  sec_insur_type_2 = weighted.mean(sec_insur_type_2, re_weight * weights, na.rm = TRUE),
                                                  sec_insur_type_3 = weighted.mean(sec_insur_type_3, re_weight * weights, na.rm = TRUE),
                                                  sec_insur_type_4 = weighted.mean(sec_insur_type_4, re_weight * weights, na.rm = TRUE),
                                                  sec_insur_type_5 = weighted.mean(sec_insur_type_5, re_weight * weights, na.rm = TRUE),
                                                  sec_insur_type_6 = weighted.mean(sec_insur_type_6, re_weight * weights, na.rm = TRUE),
                                                  sec_insur_type_7 = weighted.mean(sec_insur_type_7, re_weight * weights, na.rm = TRUE),
                                           weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(year, sex, race, age_bin, edu_code, marital_ushd, nid)]

#### Combine direct estimates across levels
direct_estimates_access <- rbindlist(list(mcnty_estimates_access, state_estimates_access, national_estimates_access), use.names = TRUE, fill = TRUE)

#### Harmonize area identifiers
direct_estimates_access[level == "mcnty", area := mcnty]
direct_estimates_access[level == "state", area := state]
direct_estimates_access[level == "natl", area := 1]

#### Aggregate across sexes
sex_agg_access <- direct_estimates_access[, list(sex = 3, 
                                                 dentist = weighted.mean(dentist, weights, na.rm = TRUE),
                                                 personal_doc = weighted.mean(personal_doc, weights, na.rm = TRUE),
                                                 hlthplan = weighted.mean(hlthplan, weights, na.rm = TRUE),
                                                 sec_insur = weighted.mean(sec_insur, weights, na.rm = TRUE),
                                                 prim_insur_type_1 = weighted.mean(prim_insur_type_1, weights, na.rm = TRUE),
                                                 prim_insur_type_2 = weighted.mean(prim_insur_type_2, weights, na.rm = TRUE),
                                                 prim_insur_type_3 = weighted.mean(prim_insur_type_3, weights, na.rm = TRUE),
                                                 prim_insur_type_4 = weighted.mean(prim_insur_type_4, weights, na.rm = TRUE),
                                                 prim_insur_type_5 = weighted.mean(prim_insur_type_5, weights, na.rm = TRUE),
                                                 prim_insur_type_6 = weighted.mean(prim_insur_type_6, weights, na.rm = TRUE),
                                                 prim_insur_type_7 = weighted.mean(prim_insur_type_7, weights, na.rm = TRUE),
                                                 sec_insur_type_1 = weighted.mean(sec_insur_type_1, weights, na.rm = TRUE),
                                                 sec_insur_type_2 = weighted.mean(sec_insur_type_2, weights, na.rm = TRUE),
                                                 sec_insur_type_3 = weighted.mean(sec_insur_type_3, weights, na.rm = TRUE),
                                                 sec_insur_type_4 = weighted.mean(sec_insur_type_4, weights, na.rm = TRUE),
                                                 sec_insur_type_5 = weighted.mean(sec_insur_type_5, weights, na.rm = TRUE),
                                                 sec_insur_type_6 = weighted.mean(sec_insur_type_6, weights, na.rm = TRUE),
                                                 sec_insur_type_7 = weighted.mean(sec_insur_type_7, weights, na.rm = TRUE),
                                   weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, year, race, age_bin, edu_code, marital_ushd, nid)]
direct_estimates_access <- rbindlist(list(direct_estimates_access, sex_agg_access), use.names = TRUE, fill = TRUE)

#### Aggregate across races
race_agg_access <- direct_estimates_access[, list(race = 1, 
                                                  dentist = weighted.mean(dentist, weights, na.rm = TRUE),
                                                  personal_doc = weighted.mean(personal_doc, weights, na.rm = TRUE),
                                                  hlthplan = weighted.mean(hlthplan, weights, na.rm = TRUE),
                                                  sec_insur = weighted.mean(sec_insur, weights, na.rm = TRUE),
                                                  prim_insur_type_1 = weighted.mean(prim_insur_type_1, weights, na.rm = TRUE),
                                                  prim_insur_type_2 = weighted.mean(prim_insur_type_2, weights, na.rm = TRUE),
                                                  prim_insur_type_3 = weighted.mean(prim_insur_type_3, weights, na.rm = TRUE),
                                                  prim_insur_type_4 = weighted.mean(prim_insur_type_4, weights, na.rm = TRUE),
                                                  prim_insur_type_5 = weighted.mean(prim_insur_type_5, weights, na.rm = TRUE),
                                                  prim_insur_type_6 = weighted.mean(prim_insur_type_6, weights, na.rm = TRUE),
                                                  prim_insur_type_7 = weighted.mean(prim_insur_type_7, weights, na.rm = TRUE),
                                                  sec_insur_type_1 = weighted.mean(sec_insur_type_1, weights, na.rm = TRUE),
                                                  sec_insur_type_2 = weighted.mean(sec_insur_type_2, weights, na.rm = TRUE),
                                                  sec_insur_type_3 = weighted.mean(sec_insur_type_3, weights, na.rm = TRUE),
                                                  sec_insur_type_4 = weighted.mean(sec_insur_type_4, weights, na.rm = TRUE),
                                                  sec_insur_type_5 = weighted.mean(sec_insur_type_5, weights, na.rm = TRUE),
                                                  sec_insur_type_6 = weighted.mean(sec_insur_type_6, weights, na.rm = TRUE),
                                                  sec_insur_type_7 = weighted.mean(sec_insur_type_7, weights, na.rm = TRUE),
                                    weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, year, sex, age_bin, edu_code, marital_ushd, nid)]
direct_estimates_access <- rbindlist(list(direct_estimates_access, race_agg_access), use.names = TRUE, fill = TRUE)

#### Aggregate across ages
age_agg_access <- direct_estimates_access[, list(age_bin = 98, 
                                                 dentist = weighted.mean(dentist, weights, na.rm = TRUE),
                                                 personal_doc = weighted.mean(personal_doc, weights, na.rm = TRUE),
                                                 hlthplan = weighted.mean(hlthplan, weights, na.rm = TRUE),
                                                 sec_insur = weighted.mean(sec_insur, weights, na.rm = TRUE),
                                                 prim_insur_type_1 = weighted.mean(prim_insur_type_1, weights, na.rm = TRUE),
                                                 prim_insur_type_2 = weighted.mean(prim_insur_type_2, weights, na.rm = TRUE),
                                                 prim_insur_type_3 = weighted.mean(prim_insur_type_3, weights, na.rm = TRUE),
                                                 prim_insur_type_4 = weighted.mean(prim_insur_type_4, weights, na.rm = TRUE),
                                                 prim_insur_type_5 = weighted.mean(prim_insur_type_5, weights, na.rm = TRUE),
                                                 prim_insur_type_6 = weighted.mean(prim_insur_type_6, weights, na.rm = TRUE),
                                                 prim_insur_type_7 = weighted.mean(prim_insur_type_7, weights, na.rm = TRUE),
                                                 sec_insur_type_1 = weighted.mean(sec_insur_type_1, weights, na.rm = TRUE),
                                                 sec_insur_type_2 = weighted.mean(sec_insur_type_2, weights, na.rm = TRUE),
                                                 sec_insur_type_3 = weighted.mean(sec_insur_type_3, weights, na.rm = TRUE),
                                                 sec_insur_type_4 = weighted.mean(sec_insur_type_4, weights, na.rm = TRUE),
                                                 sec_insur_type_5 = weighted.mean(sec_insur_type_5, weights, na.rm = TRUE),
                                                 sec_insur_type_6 = weighted.mean(sec_insur_type_6, weights, na.rm = TRUE),
                                                 sec_insur_type_7 = weighted.mean(sec_insur_type_7, weights, na.rm = TRUE),
                                   weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, year, sex, race, edu_code, marital_ushd, nid)]
direct_estimates_access <- rbindlist(list(direct_estimates_access, age_agg_access), use.names = TRUE, fill = TRUE)

#### Aggregate across edu
message("Aggregate across edu")
edu_agg_access <- direct_estimates_access[, list(edu_code = 9, 
                                                 dentist = weighted.mean(dentist, weights, na.rm = TRUE),
                                                 personal_doc = weighted.mean(personal_doc, weights, na.rm = TRUE),
                                                 hlthplan = weighted.mean(hlthplan, weights, na.rm = TRUE),
                                                 sec_insur = weighted.mean(sec_insur, weights, na.rm = TRUE),
                                                 prim_insur_type_1 = weighted.mean(prim_insur_type_1, weights, na.rm = TRUE),
                                                 prim_insur_type_2 = weighted.mean(prim_insur_type_2, weights, na.rm = TRUE),
                                                 prim_insur_type_3 = weighted.mean(prim_insur_type_3, weights, na.rm = TRUE),
                                                 prim_insur_type_4 = weighted.mean(prim_insur_type_4, weights, na.rm = TRUE),
                                                 prim_insur_type_5 = weighted.mean(prim_insur_type_5, weights, na.rm = TRUE),
                                                 prim_insur_type_6 = weighted.mean(prim_insur_type_6, weights, na.rm = TRUE),
                                                 prim_insur_type_7 = weighted.mean(prim_insur_type_7, weights, na.rm = TRUE),
                                                 sec_insur_type_1 = weighted.mean(sec_insur_type_1, weights, na.rm = TRUE),
                                                 sec_insur_type_2 = weighted.mean(sec_insur_type_2, weights, na.rm = TRUE),
                                                 sec_insur_type_3 = weighted.mean(sec_insur_type_3, weights, na.rm = TRUE),
                                                 sec_insur_type_4 = weighted.mean(sec_insur_type_4, weights, na.rm = TRUE),
                                                 sec_insur_type_5 = weighted.mean(sec_insur_type_5, weights, na.rm = TRUE),
                                                 sec_insur_type_6 = weighted.mean(sec_insur_type_6, weights, na.rm = TRUE),
                                                 sec_insur_type_7 = weighted.mean(sec_insur_type_7, weights, na.rm = TRUE),
                                   weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, age_bin, sex, race, year, marital_ushd)]
direct_estimates_access <- rbindlist(list(direct_estimates_access, edu_agg_access), use.names = TRUE, fill = TRUE)

#### Aggregate across marital
message("Aggregate across marital")
marital_agg_access <- direct_estimates_access[, list(marital_ushd = 9, 
                                                     dentist = weighted.mean(dentist, weights, na.rm = TRUE),
                                                     personal_doc = weighted.mean(personal_doc, weights, na.rm = TRUE),
                                                     hlthplan = weighted.mean(hlthplan, weights, na.rm = TRUE),
                                                     sec_insur = weighted.mean(sec_insur, weights, na.rm = TRUE),
                                                     prim_insur_type_1 = weighted.mean(prim_insur_type_1, weights, na.rm = TRUE),
                                                     prim_insur_type_2 = weighted.mean(prim_insur_type_2, weights, na.rm = TRUE),
                                                     prim_insur_type_3 = weighted.mean(prim_insur_type_3, weights, na.rm = TRUE),
                                                     prim_insur_type_4 = weighted.mean(prim_insur_type_4, weights, na.rm = TRUE),
                                                     prim_insur_type_5 = weighted.mean(prim_insur_type_5, weights, na.rm = TRUE),
                                                     prim_insur_type_6 = weighted.mean(prim_insur_type_6, weights, na.rm = TRUE),
                                                     prim_insur_type_7 = weighted.mean(prim_insur_type_7, weights, na.rm = TRUE),
                                                     sec_insur_type_1 = weighted.mean(sec_insur_type_1, weights, na.rm = TRUE),
                                                     sec_insur_type_2 = weighted.mean(sec_insur_type_2, weights, na.rm = TRUE),
                                                     sec_insur_type_3 = weighted.mean(sec_insur_type_3, weights, na.rm = TRUE),
                                                     sec_insur_type_4 = weighted.mean(sec_insur_type_4, weights, na.rm = TRUE),
                                                     sec_insur_type_5 = weighted.mean(sec_insur_type_5, weights, na.rm = TRUE),
                                                     sec_insur_type_6 = weighted.mean(sec_insur_type_6, weights, na.rm = TRUE),
                                                     sec_insur_type_7 = weighted.mean(sec_insur_type_7, weights, na.rm = TRUE),
                                       weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), by = list(area, level, age_bin, sex, race, year, edu_code)]
direct_estimates_access <- rbindlist(list(direct_estimates_access, marital_agg_access), use.names = TRUE, fill = TRUE)

#### Rename age column
setnames(direct_estimates_access, "age_bin", "age")

#### Save direct estimates
saveRDS(direct_estimates_access, file = paste0(output_dir, "/gallup_2000_2019_direct_estimates_access.rds"))


####### Plot diagnostics for direct estimates
#### Load shapefiles
shape <- readRDS("FILEPATH")
shape <- spTransform(shape, "+init=epsg:4326") # Project shapefile
shape_fortified <- fortify(shape, region = "mcnty") # Fortify SpatialPolygonsDataFrame to facilitate plotting

state_shape <- readRDS(paste0("FILEPATH"))
state_shape <- spTransform(state_shape, "+init=epsg:4326")
states <- fortify(state_shape, region = "state") # Fortify SpatialPolygonsDataFrame to facilitate plotting

#### Create directory for plots
dir.create(path = paste0(output_dir, "/plots"))

#### Set plotting labels
sex_labels <- c("Males", "Females", "Both")
names(sex_labels) <- c(1:3)
race_labels <- c("NH White", "NH Black", "NH AIAN", "NH API", "Hispanic", "All Races/Ethnicities")
names(race_labels) <- c(5, 4, 6, 7, 2, 1)
edu_labels <- c("Less than HS", "HS", "Some College", "BA or Higher", "All Education Categories")
names(edu_labels) <- c(1:4, 9)
marital_labels <- c("Currently Married", "Formerly Married", "Never Married", "All Marital Status Categories")
names(marital_labels) <- c(1:3, 9)
post_2010_labels <- c("Pre-2011", "2011+")
names(post_2010_labels) <- c(0, 1)

#### List outcomes
outcomes <- c("gen_health_45", "frequent_activity_limitations", "asthma", "treated_asthma", "depression", "treated_depression", "pain", "worry", "stress", "diabetes", "cancer", "treated_cancer")

#### Rename some columns
setnames(direct_estimates, c("edu_code", "marital_ushd"), c("edu", "marital"))

#### Set age 98 to age -1
direct_estimates[age == 98, age := -1]

#### Loop through outcomes and plot
for (outcome in outcomes) {
  print(paste0(outcome, "..."))
  pdf(paste0(output_dir, "/plots/plots_", outcome, ".pdf"), width = 11, height = 8.5)
  
  #### Plot national estimates by year, prevalence by age-sex-race
  current <- direct_estimates[marital == 9 & edu == 9 & age == -1 & level == "natl" & year != -1 & sex != 3]
  if (nrow(current) == 0) {
    next
  }
  print(ggplot(data = current) + theme_bw() +
          geom_line(data = current, aes(x = year, y = get(outcome), color = factor(race), linetype = factor(sex))) +
          geom_point(data = current, aes(x = year, y = get(outcome), color = factor(race), size = sample_size), alpha = 0.25) +
          labs(x = "Year", y = "Prevalence", title=paste0("Prevalence of ", outcome, " (direct estimates from Gallup), National"), subtitle = paste0("Ages 20+.\nAll education and marital status categories.")) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) +
          scale_color_discrete(name = "Race", labels = race_labels) + scale_linetype_discrete(name = "Sex", labels = sex_labels) + scale_size_continuous(name = "Sample Size"))
  
  ## Now for both sexes combined
  current <- direct_estimates[marital == 9 & edu == 9 & age == -1 & level == "natl" & year != -1 & sex == 3]
  if (nrow(current) == 0) {
    next
  }
  print(ggplot(data = current) + theme_bw() +
          geom_line(data = current, aes(x = year, y = get(outcome), color = factor(race), linetype = factor(sex))) +
          geom_point(data = current, aes(x = year, y = get(outcome), color = factor(race), size = sample_size), alpha = 0.25) +
          labs(x = "Year", y = "Prevalence", title=paste0("Prevalence of ", outcome, " (direct estimates from Gallup), National"), subtitle = paste0("Ages 20+.\nAll education and marital status categories.")) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) +
          scale_color_discrete(name = "Race", labels = race_labels) + scale_linetype_discrete(name = "Sex", labels = sex_labels) + scale_size_continuous(name = "Sample Size"))
  
  #### Plot national estimates by age, prevalence by age-sex-race
  current <- direct_estimates[marital == 9 & edu == 9 & !(age %in% c(-1, 98)) & level == "natl" & year == 2015 & sex != 3]
  if (nrow(current) == 0) {
    next
  }
  print(ggplot(data = current) + theme_bw() +
          geom_line(data = current, aes(x = age, y = get(outcome), color = factor(race), linetype = factor(sex))) +
          geom_point(data = current, aes(x = age, y = get(outcome), color = factor(race), size = sample_size), alpha = 0.25) +
          labs(x = "Age", y = "Prevalence", title=paste0("Prevalence of ", outcome, " (direct estimates from Gallup), National"), subtitle = paste0("2015\nAll education and marital status categories.")) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) +
          scale_color_discrete(name = "Race", labels = race_labels) + scale_linetype_discrete(name = "Sex", labels = sex_labels) + scale_size_continuous(name = "Sample Size"))
  
  ## Now for both sexes combined
  current <- direct_estimates[marital == 9 & edu == 9 & !(age %in% c(-1, 98)) & level == "natl" & year == 2015 & sex == 3]
  if (nrow(current) == 0) {
    next
  }
  print(ggplot(data = current) + theme_bw() +
          geom_line(data = current, aes(x = age, y = get(outcome), color = factor(race), linetype = factor(sex))) +
          geom_point(data = current, aes(x = age, y = get(outcome), color = factor(race), size = sample_size), alpha = 0.25) +
          labs(x = "Age", y = "Prevalence", title=paste0("Prevalence of ", outcome, " (direct estimates from Gallup), National"), subtitle = paste0("2015\nAll education and marital status categories.")) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) +
          scale_color_discrete(name = "Race", labels = race_labels) + scale_linetype_discrete(name = "Sex", labels = sex_labels) + scale_size_continuous(name = "Sample Size"))
  
  #### Plot county-level maps of prevalence by year
  current_map <- shape_fortified
  current_shape <- shape
  
  years <- unique(direct_estimates$year)
  years <- sort(years[years != -1])
  limits <- quantile(direct_estimates[marital == 9 & edu == 9 & age %in% -1 & sex != 3 & race == 1 & level == "mcnty", get(outcome)], probs = c(0, 1), na.rm = TRUE)
  
  for (y in years) {
    current <- direct_estimates[marital == 9 & edu == 9 & age %in% -1 & level == "mcnty" & year == y]
    if (nrow(current) == 0) {
      next
    }
    
    print(ggplot() + theme_map() + geom_map(data = current[sex != 3 & race == 1], aes(map_id = area, fill = get(outcome)), map = current_map) +
            facet_wrap(~ sex, labeller = labeller(sex = sex_labels)) +
            geom_polygon(data = current_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.05) +
            geom_polygon(data = state_shape, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA, size = 0.2) +
            expand_limits(x = state_shape$long, y = state_shape$lat) +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") + coord_map(projection = "albers", lat0 = 29.5, lat1 = 45.5) +
            labs(title=paste0("Prevalence of ", outcome, " (direct estimates from Gallup), Mcnty-Level, ", y), subtitle = paste0("All ages (represented in data set for this indicator), races, education and marital status categories."), fill = "Prevalence") +
            scale_fill_gradientn(colours = rev(brewer.pal(5, "Spectral")), n.breaks = 5, limits = limits))
  }
  
  #### Plot all-year estimates
  current <- direct_estimates[marital == 9 & edu == 9 & age %in% -1 & level == "mcnty" & year == 2015]
  
  print(ggplot() + theme_map() + geom_map(data = current[sex != 3 & race == 1], aes(map_id = area, fill = get(outcome)), map = current_map) +
          facet_wrap(~ sex, labeller = labeller(sex = sex_labels)) +
          geom_polygon(data = current_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.05) +
          geom_polygon(data = state_shape, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA, size = 0.2) +
          expand_limits(x = state_shape$long, y = state_shape$lat) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") + coord_map(projection = "albers", lat0 = 29.5, lat1 = 45.5) +
          labs(title=paste0("Prevalence of ", outcome, " (direct estimates from Gallup), Mcnty-Level, 2015"), subtitle = paste0("All ages (represented in data set for this indicator), races, education and marital status categories."), fill = "Prevalence") +
          scale_fill_gradientn(colours = rev(brewer.pal(5, "Spectral")), n.breaks = 5, limits = limits))
  
  #### Plot county-by-race
  limits <- quantile(direct_estimates[marital == 9 & edu == 9 & age %in% -1 & year != -1 & sex == 3 & race != 1 & level == "mcnty", get(outcome)], probs = c(0, 1), na.rm = TRUE)
  for (y in years) {
    current <- direct_estimates[marital == 9 & edu == 9 & age %in% -1 & level == "mcnty" & year == y]
    if (nrow(current) == 0) {
      next
    }
    
    print(ggplot() + theme_map() + geom_map(data = current[sex == 3], aes(map_id = area, fill = get(outcome)), map = current_map) +
            facet_wrap(~ race, labeller = labeller(sex = sex_labels, race = race_labels)) +
            geom_polygon(data = current_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.05) +
            geom_polygon(data = state_shape, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA, size = 0.2) +
            expand_limits(x = state_shape$long, y = state_shape$lat) +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") + coord_map(projection = "albers", lat0 = 29.5, lat1 = 45.5) +
            labs(title=paste0("Prevalence of ", outcome, " (direct estimates from Gallup), Mcnty-Level, ", y), subtitle = paste0("All ages (represented in data set for this indicator), sexes, and education and marital status categories."), fill = "Prevalence") +
            scale_fill_gradientn(colours = rev(brewer.pal(5, "Spectral")), n.breaks = 5, limits = limits))
  }
  
  #### Plot all-year estimates
  limits <- quantile(direct_estimates[marital == 9 & edu == 9 & age %in% -1 & year == 2015 & sex == 3 & race != 1 & level == "mcnty", get(outcome)], probs = c(0, 1), na.rm = TRUE)
  current <- direct_estimates[marital == 9 & edu == 9 & age %in% -1 & level == "mcnty" & year == 2015]
  print(ggplot() + theme_map() + geom_map(data = current[sex == 3], aes(map_id = area, fill = get(outcome)), map = current_map) +
          facet_wrap(~ race, labeller = labeller(sex = sex_labels, race = race_labels)) +
          geom_polygon(data = current_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.05) +
          geom_polygon(data = state_shape, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA, size = 0.2) +
          expand_limits(x = state_shape$long, y = state_shape$lat) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") + coord_map(projection = "albers", lat0 = 29.5, lat1 = 45.5) +
          labs(title=paste0("Prevalence of ", outcome, " (direct estimates from Gallup), Mcnty-Level, 2015"), subtitle = paste0("All ages (represented in data set for this indicator), sexes, and education and marital status categories."), fill = "Prevalence") +
          scale_fill_gradientn(colours = rev(brewer.pal(5, "Spectral")), n.breaks = 5, limits = limits))
  
  #### Plot state-level maps of prevalence by year
  limits <- quantile(direct_estimates[marital == 9 & edu == 9 & age %in% -1 & sex != 3 & race == 1 & level == "state", get(outcome)], probs = c(0, 1), na.rm = TRUE)
  for (y in years) {
    current <- direct_estimates[marital == 9 & edu == 9 & age %in% -1 & race == 1 & level == "state" & year == y]
    if (nrow(current) == 0) {
      next
    }
    
    print(ggplot() + theme_map() + geom_map(data = current[sex != 3 & race == 1], aes(map_id = area, fill = get(outcome)), map = states) +
            facet_wrap(~ sex, labeller = labeller(sex = sex_labels)) +
            geom_polygon(data = state_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.1) +
            expand_limits(x = current_map$long, y = current_map$lat) +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") + coord_map(projection = "albers", lat0 = 29.5, lat1 = 45.5) +
            labs(title=paste0("Prevalence of ", outcome, " (direct estimates from Gallup), State-Level, ", y), subtitle = paste0("All ages (represented in data set for this indicator), races, education and marital status categories."), fill = "Prevalence") +
            scale_fill_gradientn(colours = rev(brewer.pal(5, "Spectral")), n.breaks = 5, limits = limits))
  }
  
  #### Plot all-year estimates
  limits <- quantile(direct_estimates[marital == 9 & edu == 9 & age %in% -1 & sex != 3 & race == 1 & year == 2015 & level == "state", get(outcome)], probs = c(0, 1), na.rm = TRUE)
  current <- direct_estimates[marital == 9 & edu == 9 & age %in% -1 & race == 1 & level == "state" & year == 2015]
  print(ggplot() + theme_map() + geom_map(data = current[sex != 3 & race == 1], aes(map_id = area, fill = get(outcome)), map = states) +
          facet_wrap(~ sex, labeller = labeller(sex = sex_labels)) +
          geom_polygon(data = state_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.1) +
          expand_limits(x = current_map$long, y = current_map$lat) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") + coord_map(projection = "albers", lat0 = 29.5, lat1 = 45.5) +
          labs(title=paste0("Prevalence of ", outcome, " (direct estimates from Gallup), State-Level, 2015"), subtitle = paste0("All ages (represented in data set for this indicator), races, education and marital status categories."), fill = "Prevalence") +
          scale_fill_gradientn(colours = rev(brewer.pal(5, "Spectral")), n.breaks = 5, limits = limits))
  
  limits <- quantile(direct_estimates[marital == 9 & edu == 9 & age %in% -1 & sex == 3 & race != 1 & level == "state", get(outcome)], probs = c(0, 1), na.rm = TRUE)
  for (y in years) {
    current <- direct_estimates[marital == 9 & edu == 9 & age %in% -1 & level == "state" & year == y]
    if (nrow(current) == 0) {
      next
    }
    
    print(ggplot() + theme_map() + geom_map(data = current[sex == 3], aes(map_id = area, fill = get(outcome)), map = states) +
            facet_wrap(~ race, labeller = labeller(sex = sex_labels, race = race_labels)) +
            geom_polygon(data = state_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.1) +
            expand_limits(x = current_map$long, y = current_map$lat) +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") + coord_map(projection = "albers", lat0 = 29.5, lat1 = 45.5) +
            labs(title=paste0("Prevalence of ", outcome, " (direct estimates from Gallup), State-Level, ", y), subtitle = paste0("All ages (represented in data set for this indicator), sexes, and education and marital status categories."), fill = "Prevalence") +
            scale_fill_gradientn(colours = rev(brewer.pal(5, "Spectral")), n.breaks = 5, limits = limits))
  }
  
  #### Plot all-year estimates
  limits <- quantile(direct_estimates[marital == 9 & edu == 9 & age %in% -1 & sex == 3 & race != 1 & year == 2015 & level == "state", get(outcome)], probs = c(0, 1), na.rm = TRUE)
  current <- direct_estimates[marital == 9 & edu == 9 & age %in% -1 & level == "state" & year == 2015]
  print(ggplot() + theme_map() + geom_map(data = current[sex == 3], aes(map_id = area, fill = get(outcome)), map = states) +
          facet_wrap(~ race, labeller = labeller(sex = sex_labels, race = race_labels)) +
          geom_polygon(data = state_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.1) +
          expand_limits(x = current_map$long, y = current_map$lat) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") + coord_map(projection = "albers", lat0 = 29.5, lat1 = 45.5) +
          labs(title=paste0("Prevalence of ", outcome, " (direct estimates from Gallup), State-Level, 2015"), subtitle = paste0("All ages (represented in data set for this indicator), sexes, and education and marital status categories."), fill = "Prevalence") +
          scale_fill_gradientn(colours = rev(brewer.pal(5, "Spectral")), n.breaks = 5, limits = limits))
  
  dev.off()
}


########## Analyze monthly coverage
data_combined[, month := format(idate,"%m")]
table(data_combined[, list(year, month)])

data_combined[, year_month := as.Date(paste0(year, "-", month, "-01"))]
data_combined[, month_int := as.integer(month)]

national_estimates_no_strat <- data_combined[, list(level = "natl", gen_health_45 = weighted.mean(gen_health_45, re_weight * weights, na.rm = TRUE), 
                                                    frequent_activity_limitations = weighted.mean(frequent_activity_limitations, re_weight * weights, na.rm = TRUE),
                                                    asthma = weighted.mean(asthma, re_weight * weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, re_weight * weights, na.rm = TRUE),
                                                    depression = weighted.mean(depression, re_weight * weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, re_weight * weights, na.rm = TRUE),
                                                    pain = weighted.mean(exp_physical_pain, re_weight * weights, na.rm = TRUE), worry = weighted.mean(exp_worry, re_weight * weights, na.rm = TRUE),
                                                    stress = weighted.mean(exp_stress, re_weight * weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, re_weight * weights, na.rm = TRUE),
                                                    cancer = weighted.mean(cancer, re_weight * weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, re_weight * weights, na.rm = TRUE),
                                                    weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(year, month, month_int)]
national_estimates_no_strat[, year_month := as.Date(paste0(year, "-", month, "-01"))]

national_estimates_year <- data_combined[, list(level = "natl", gen_health_45 = weighted.mean(gen_health_45, re_weight * weights, na.rm = TRUE), 
                                                                  frequent_activity_limitations = weighted.mean(frequent_activity_limitations, re_weight * weights, na.rm = TRUE),
                                                                  asthma = weighted.mean(asthma, re_weight * weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, re_weight * weights, na.rm = TRUE),
                                                                  depression = weighted.mean(depression, re_weight * weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, re_weight * weights, na.rm = TRUE),
                                                                  pain = weighted.mean(exp_physical_pain, re_weight * weights, na.rm = TRUE), worry = weighted.mean(exp_worry, re_weight * weights, na.rm = TRUE),
                                                                  stress = weighted.mean(exp_stress, re_weight * weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, re_weight * weights, na.rm = TRUE),
                                                                  cancer = weighted.mean(cancer, re_weight * weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, re_weight * weights, na.rm = TRUE),
                                                                  weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(year)]

national_estimates_year_1_9 <- data_combined[month_int < 10, list(level = "natl", gen_health_45 = weighted.mean(gen_health_45, re_weight * weights, na.rm = TRUE), 
                                                    frequent_activity_limitations = weighted.mean(frequent_activity_limitations, re_weight * weights, na.rm = TRUE),
                                                    asthma = weighted.mean(asthma, re_weight * weights, na.rm = TRUE), treated_asthma = weighted.mean(treated_asthma, re_weight * weights, na.rm = TRUE),
                                                    depression = weighted.mean(depression, re_weight * weights, na.rm = TRUE), treated_depression = weighted.mean(treated_depression, re_weight * weights, na.rm = TRUE),
                                                    pain = weighted.mean(exp_physical_pain, re_weight * weights, na.rm = TRUE), worry = weighted.mean(exp_worry, re_weight * weights, na.rm = TRUE),
                                                    stress = weighted.mean(exp_stress, re_weight * weights, na.rm = TRUE), diabetes = weighted.mean(diabetes, re_weight * weights, na.rm = TRUE),
                                                    cancer = weighted.mean(cancer, re_weight * weights, na.rm = TRUE), treated_cancer = weighted.mean(treated_cancer, re_weight * weights, na.rm = TRUE),
                                                    weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(year)]

pdf(paste0(output_dir, "/plots/plots_month_examination.pdf"), width = 11, height = 8.5)
ggplot(national_estimates_no_strat[year != 2008]) + theme_bw() + 
  geom_point(aes(x = month, y = gen_health_45, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(aes(x = month, y = gen_health_45, group = as.factor(year), color = as.factor(year)), se = FALSE, size = 0.5) +
  geom_smooth(aes(x = month_int, y = gen_health_45), color = 1, se = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Month", y = "Prevalence", title=paste0("Prevalence of gen_health_45", " (direct estimates from Gallup), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot(national_estimates_no_strat[year != 2008]) + theme_bw() + 
  geom_point(aes(x = year_month, y = gen_health_45, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(aes(x = year_month, y = gen_health_45), color = 1, se = FALSE, size = 0.5, span = 0.1) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of gen_health_45", " (direct estimates from Gallup), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot() + theme_bw() + 
  geom_point(data = national_estimates_year[year != 2008], aes(x = year, y = gen_health_45, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(data = national_estimates_year[year != 2008], aes(x = year, y = gen_health_45), color = 1, se = FALSE, size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of gen_health_45", " (direct estimates from Gallup, using all months), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot() + theme_bw() + 
  geom_point(data = national_estimates_year_1_9[year != 2008], aes(x = year, y = gen_health_45, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(data = national_estimates_year_1_9[year != 2008], aes(x = year, y = gen_health_45), color = 1, se = FALSE, size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of gen_health_45", " (direct estimates from Gallup, using months 1-9), National, All Ages, Sexes, and Races (weighted)"), color = "Year")


ggplot(national_estimates_no_strat[year != 2008]) + theme_bw() + 
  geom_point(aes(x = month, y = frequent_activity_limitations, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(aes(x = month, y = frequent_activity_limitations, group = as.factor(year), color = as.factor(year)), se = FALSE, size = 0.5) +
  geom_smooth(aes(x = month_int, y = frequent_activity_limitations), color = 1, se = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Month", y = "Prevalence", title=paste0("Prevalence of frequent_activity_limitations", " (direct estimates from Gallup), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot(national_estimates_no_strat[year != 2008]) + theme_bw() + 
  geom_point(aes(x = year_month, y = frequent_activity_limitations, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(aes(x = year_month, y = frequent_activity_limitations), color = 1, se = FALSE, size = 0.5, span = 0.1) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of frequent_activity_limitations", " (direct estimates from Gallup), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot() + theme_bw() + 
  geom_point(data = national_estimates_year[year != 2008], aes(x = year, y = frequent_activity_limitations, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(data = national_estimates_year[year != 2008], aes(x = year, y = frequent_activity_limitations), color = 1, se = FALSE, size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of frequent_activity_limitations", " (direct estimates from Gallup, using all months), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot() + theme_bw() + 
  geom_point(data = national_estimates_year_1_9[year != 2008], aes(x = year, y = frequent_activity_limitations, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(data = national_estimates_year_1_9[year != 2008], aes(x = year, y = frequent_activity_limitations), color = 1, se = FALSE, size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of frequent_activity_limitations", " (direct estimates from Gallup, using months 1-9), National, All Ages, Sexes, and Races (weighted)"), color = "Year")


ggplot(national_estimates_no_strat[year != 2008]) + theme_bw() + 
  geom_point(aes(x = month, y = asthma, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(aes(x = month, y = asthma, group = as.factor(year), color = as.factor(year)), se = FALSE, size = 0.5) +
  geom_smooth(aes(x = month_int, y = asthma), color = 1, se = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Month", y = "Prevalence", title=paste0("Prevalence of asthma", " (direct estimates from Gallup), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot(national_estimates_no_strat[year != 2008]) + theme_bw() + 
  geom_point(aes(x = year_month, y = asthma, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(aes(x = year_month, y = asthma), color = 1, se = FALSE, size = 0.5, span = 0.1) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of asthma", " (direct estimates from Gallup), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot() + theme_bw() + 
  geom_point(data = national_estimates_year[year != 2008], aes(x = year, y = asthma, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(data = national_estimates_year[year != 2008], aes(x = year, y = asthma), color = 1, se = FALSE, size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of asthma", " (direct estimates from Gallup, using all months), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot() + theme_bw() + 
  geom_point(data = national_estimates_year_1_9[year != 2008], aes(x = year, y = asthma, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(data = national_estimates_year_1_9[year != 2008], aes(x = year, y = asthma), color = 1, se = FALSE, size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of asthma", " (direct estimates from Gallup, using months 1-9), National, All Ages, Sexes, and Races (weighted)"), color = "Year")


ggplot(national_estimates_no_strat[year != 2008]) + theme_bw() + 
  geom_point(aes(x = month, y = depression, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(aes(x = month, y = depression, group = as.factor(year), color = as.factor(year)), se = FALSE, size = 0.5) +
  geom_smooth(aes(x = month_int, y = depression), color = 1, se = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Month", y = "Prevalence", title=paste0("Prevalence of depression", " (direct estimates from Gallup), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot(national_estimates_no_strat[year != 2008]) + theme_bw() + 
  geom_point(aes(x = year_month, y = depression, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(aes(x = year_month, y = depression), color = 1, se = FALSE, size = 0.5, span = 0.1) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of depression", " (direct estimates from Gallup), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot() + theme_bw() + 
  geom_point(data = national_estimates_year[year != 2008], aes(x = year, y = depression, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(data = national_estimates_year[year != 2008], aes(x = year, y = depression), color = 1, se = FALSE, size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of depression", " (direct estimates from Gallup, using all months), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot() + theme_bw() + 
  geom_point(data = national_estimates_year_1_9[year != 2008], aes(x = year, y = depression, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(data = national_estimates_year_1_9[year != 2008], aes(x = year, y = depression), color = 1, se = FALSE, size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of depression", " (direct estimates from Gallup, using months 1-9), National, All Ages, Sexes, and Races (weighted)"), color = "Year")


ggplot(national_estimates_no_strat[year != 2008]) + theme_bw() + 
  geom_point(aes(x = month, y = pain, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(aes(x = month, y = pain, group = as.factor(year), color = as.factor(year)), se = FALSE, size = 0.5) +
  geom_smooth(aes(x = month_int, y = pain), color = 1, se = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Month", y = "Prevalence", title=paste0("Prevalence of pain", " (direct estimates from Gallup), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot(national_estimates_no_strat[year != 2008]) + theme_bw() + 
  geom_point(aes(x = year_month, y = pain, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(aes(x = year_month, y = pain), color = 1, se = FALSE, size = 0.5, span = 0.1) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of pain", " (direct estimates from Gallup), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot() + theme_bw() + 
  geom_point(data = national_estimates_year[year != 2008], aes(x = year, y = pain, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(data = national_estimates_year[year != 2008], aes(x = year, y = pain), color = 1, se = FALSE, size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of pain", " (direct estimates from Gallup, using all months), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot() + theme_bw() + 
  geom_point(data = national_estimates_year_1_9[year != 2008], aes(x = year, y = pain, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(data = national_estimates_year_1_9[year != 2008], aes(x = year, y = pain), color = 1, se = FALSE, size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of pain", " (direct estimates from Gallup, using months 1-9), National, All Ages, Sexes, and Races (weighted)"), color = "Year")


ggplot(national_estimates_no_strat[year != 2008]) + theme_bw() + 
  geom_point(aes(x = month, y = worry, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(aes(x = month, y = worry, group = as.factor(year), color = as.factor(year)), se = FALSE, size = 0.5) +
  geom_smooth(aes(x = month_int, y = worry), color = 1, se = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Month", y = "Prevalence", title=paste0("Prevalence of worry", " (direct estimates from Gallup), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot(national_estimates_no_strat[year != 2008]) + theme_bw() + 
  geom_point(aes(x = year_month, y = worry, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(aes(x = year_month, y = worry), color = 1, se = FALSE, size = 0.5, span = 0.1) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of worry", " (direct estimates from Gallup), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot() + theme_bw() + 
  geom_point(data = national_estimates_year[year != 2008], aes(x = year, y = worry, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(data = national_estimates_year[year != 2008], aes(x = year, y = worry), color = 1, se = FALSE, size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of worry", " (direct estimates from Gallup, using all months), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot() + theme_bw() + 
  geom_point(data = national_estimates_year_1_9[year != 2008], aes(x = year, y = worry, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(data = national_estimates_year_1_9[year != 2008], aes(x = year, y = worry), color = 1, se = FALSE, size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of worry", " (direct estimates from Gallup, using months 1-9), National, All Ages, Sexes, and Races (weighted)"), color = "Year")


ggplot(national_estimates_no_strat[year != 2008]) + theme_bw() + 
  geom_point(aes(x = month, y = stress, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(aes(x = month, y = stress, group = as.factor(year), color = as.factor(year)), se = FALSE, size = 0.5) +
  geom_smooth(aes(x = month_int, y = stress), color = 1, se = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Month", y = "Prevalence", title=paste0("Prevalence of stress", " (direct estimates from Gallup), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot(national_estimates_no_strat[year != 2008]) + theme_bw() + 
  geom_point(aes(x = year_month, y = stress, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(aes(x = year_month, y = stress), color = 1, se = FALSE, size = 0.5, span = 0.1) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of stress", " (direct estimates from Gallup), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot() + theme_bw() + 
  geom_point(data = national_estimates_year[year != 2008], aes(x = year, y = stress, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(data = national_estimates_year[year != 2008], aes(x = year, y = stress), color = 1, se = FALSE, size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of stress", " (direct estimates from Gallup, using all months), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

ggplot() + theme_bw() + 
  geom_point(data = national_estimates_year_1_9[year != 2008], aes(x = year, y = stress, group = as.factor(year), color = as.factor(year), size = weights), alpha = 0.25) +
  geom_smooth(data = national_estimates_year_1_9[year != 2008], aes(x = year, y = stress), color = 1, se = FALSE, size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  labs(x = "Date", y = "Prevalence", title=paste0("Prevalence of stress", " (direct estimates from Gallup, using months 1-9), National, All Ages, Sexes, and Races (weighted)"), color = "Year")

dev.off()
