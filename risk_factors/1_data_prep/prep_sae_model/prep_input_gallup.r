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
source(paste0("FILEPATH"))  # load USHD database
pacman::p_load(data.table, haven, ggplot2, stringr, survey, car, R.utils,doParallel, RColorBrewer)

#### Set paths
# Race        OLD    NEW
# Hispanic    7       2
# NH Black    2       4
# NH White    1       5
# NH AIAN     3       6
# NH API      4       7
# all_race    9       1
race_code_set <- 'db' #'db
stopifnot(race_code_set %in% c("old", "db"))


cw_meta <- get_crosswalk_version(rei_id = 370, get_best = T)
data_version <- basename(cw_meta$output_file_path)
input_dir <- cw_meta$output_file_path

cbsa_mcnty_meta <- get_mcnty_mapping_crosswalk(mcnty_mapping_type_id = 1, get_best = T)
cbsa_mcnty_meta <- cbsa_mcnty_meta[mcnty_mapping_crosswalk_id  == max(mcnty_mapping_crosswalk_id )]
cbsa_mcnty_version_id <- cbsa_mcnty_meta$mcnty_mapping_crosswalk_id

###### Source functions
repo <- paste0("FILEPATH") 
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

###### Generate run date and create model output folder
run_date <- make_time_stamp()#
output_dir <- paste0("FILEPATH", run_date)
message(run_date)
message(output_dir)
dir.create(output_dir)

age_std_file <- 'FILEPATH'
ages <- c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)
std_wt <- readRDS(age_std_file)[age %in% ages, list(age, wt = wt / sum(wt))]

######## 2. Load individual data set
data_combined <- readRDS(file = paste0(input_dir, "pred_bridged_race.rds"))

#### Limit to 2000-2019
data_combined <- data_combined[survey=='gallup']

#### Drop 2018-2019 data (different methodology from previous years)
data_combined <- data_combined[!(year %in% 2018:2019)]

#FIXME temporary hack: drop multiracial because crosswalk was done based on 1997 OMB
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
  education_map <- data.table("edu_code" = 1:4, "edu" = c("less than HS", "HS grad", "some college", "college grad"))
  edu_all <- 9 # note that this is not a valid education code in the database
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
data_combined[!is.na(age_continuous) & age_continuous >= 85, age_bin := 85]
data_combined <- data_combined[is.na(age_bin) | age_bin != 15] # Drop 18-19 year-olds (for nonfatal work, specifically)
data_combined[, c("age", "age_continuous") := NULL]

#### Process output
# calculate BMI continuous
# get number of BMI draws (not including the point estimate)
setnames(data_combined, "bmi_report", "pred_bmi_report") # rename for consistency
# take out "pred_bmi_pnt" b/c that's not actually the point estimates for 
#   bmi predictions -- it's related to the point estimate of logit percentile for bmi...
#   not needed here.
bmi_draws <- setdiff(grep("pred_bmi", names(data_combined), value = T), "pred_bmi_pnt")
# reorder bmi_draws so that "pred_bmi_report" is first
bmi_draws <- c("pred_bmi_report", bmi_draws[!bmi_draws %in% "pred_bmi_report"])
stopifnot(setequal(bmi_draws, unique(bmi_draws)))
# make vector of column names for overweight indicator for each draw
overwt_draws <- c("overwt_report", paste("overwt", 1:(length(bmi_draws)-1), sep = "_"))
underwt_draws <- c("underwt_report", paste("underwt", 1:(length(bmi_draws)-1), sep = "_"))
obese_draws <- c("obese_report", paste("obese", 1:(length(bmi_draws)-1), sep = "_"))
# based on each BMI value, calc overwt indicator
data_combined[, (overwt_draws) := lapply(.SD, function(x) as.numeric(x >= 25)), .SDcols = bmi_draws]
data_combined[, (underwt_draws) := lapply(.SD, function(x) as.numeric(x < 18.5)), .SDcols = bmi_draws]
data_combined[, (obese_draws) := lapply(.SD, function(x) as.numeric(x >= 30)), .SDcols = bmi_draws]

#### Rename weights variable
# NOTE: The variable "weight" in the crosswalked data is rescaled so that it sums to the 20+ population for each survey year. 
# All of the weights within a year are adjusted by the same factor (should not matter since we're using weighted.mean below)
setnames(data_combined, "wt", "weights")

#### Load state-cnty-mcnty crosswalk file
locs <- fread("FILEPATH")

#### Merge state name
data_combined <- merge(data_combined, unique(locs[, list(state, state_name)]), by = "state", all.x = TRUE)

#### Process marital
data_combined[marital == "current", marital_ushd := 1] # Currently married
data_combined[marital == "former", marital_ushd := 2] # Formerly married
data_combined[marital == "never", marital_ushd := 3] # Never married

#### Process phone
data_combined[, phone_ushd := factor(phone, levels = c("cellphone", "landline"), 1:2)]

#### Save full data set to disk
saveRDS(data_combined, file = paste0(output_dir, "/data_full.rds"))

######## 3. Prepare data for analysis
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

########## Process data for mean BMI
## Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_mean <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin)  | is.na(edu_code) | is.na(marital_ushd)]
data_combined_mean <- na.omit(data_combined, cols = c("mcnty", "year", "sex", "race", "age_bin", "edu_code", "marital_ushd"))
if (nrow(data_missing_mean) + nrow(data_combined_mean) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_overwt) + nrow(data_combined_overwt), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

#### Collapse overweight by county-year-age-sex and either race/ethnicity or education
data_combined_agg_mean <- data_combined_mean[, list(pred_bmi_report = weighted.mean(pred_bmi_report, weights * re_weight, na.rm = TRUE),
                                                    pred_bmi_1 = weighted.mean(pred_bmi_1, weights * re_weight, na.rm = TRUE), 
                                                    pred_bmi_2 = weighted.mean(pred_bmi_2, weights * re_weight, na.rm = TRUE), 
                                                    pred_bmi_3 = weighted.mean(pred_bmi_3, weights * re_weight, na.rm = TRUE), 
                                                    pred_bmi_4 = weighted.mean(pred_bmi_4, weights * re_weight, na.rm = TRUE), 
                                                    pred_bmi_5 = weighted.mean(pred_bmi_5, weights * re_weight, na.rm = TRUE), 
                                                    pred_bmi_6 = weighted.mean(pred_bmi_6, weights * re_weight, na.rm = TRUE), 
                                                    pred_bmi_7 = weighted.mean(pred_bmi_7, weights * re_weight, na.rm = TRUE), 
                                                    pred_bmi_8 = weighted.mean(pred_bmi_8, weights * re_weight, na.rm = TRUE), 
                                                    pred_bmi_9 = weighted.mean(pred_bmi_9, weights * re_weight, na.rm = TRUE), 
                                                    pred_bmi_10 = weighted.mean(pred_bmi_10, weights * re_weight, na.rm = TRUE),
                                                    weights = sum(weights * re_weight), sample_size = sum(re_weight)), 
                                             by = list(mcnty, state, year, sex, race, age_bin, edu_code, marital_ushd)]
#### Save collapsed data sets
saveRDS(data_missing_mean, file = paste0(output_dir, "/gallup_agg_missing_data_meanbmi.rds"))
saveRDS(data_combined_agg_mean, file = paste0(output_dir, "/gallup_agg_meanbmi.rds"))


########## Process data for overweight
## Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_overwt <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin)  | is.na(edu_code) | is.na(marital_ushd)]
data_combined_overwt <- na.omit(data_combined, cols = c("mcnty", "year", "sex", "race", "age_bin", "edu_code", "marital_ushd"))
if (nrow(data_missing_overwt) + nrow(data_combined_overwt) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_overwt) + nrow(data_combined_overwt), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

#### Collapse overweight by county-year-age-sex and either race/ethnicity or education
data_combined_agg_overwt <- data_combined_overwt[, list(overwt_report = sum(overwt_report * re_weight), overwt_weighted_count_report = weighted.mean(overwt_report, weights * re_weight) * sum(re_weight),
                                                        overwt_1 = sum(overwt_1 * re_weight), overwt_weighted_count_1 = weighted.mean(overwt_1, weights * re_weight) * sum(re_weight), 
                                                        overwt_2 = sum(overwt_2 * re_weight), overwt_weighted_count_2 = weighted.mean(overwt_2, weights * re_weight) * sum(re_weight), 
                                                        overwt_3 = sum(overwt_3 * re_weight), overwt_weighted_count_3 = weighted.mean(overwt_3, weights * re_weight) * sum(re_weight), 
                                                        overwt_4 = sum(overwt_4 * re_weight), overwt_weighted_count_4 = weighted.mean(overwt_4, weights * re_weight) * sum(re_weight), 
                                                        overwt_5 = sum(overwt_5 * re_weight), overwt_weighted_count_5 = weighted.mean(overwt_5, weights * re_weight) * sum(re_weight), 
                                                        overwt_6 = sum(overwt_6 * re_weight), overwt_weighted_count_6 = weighted.mean(overwt_6, weights * re_weight) * sum(re_weight), 
                                                        overwt_7 = sum(overwt_7 * re_weight), overwt_weighted_count_7 = weighted.mean(overwt_7, weights * re_weight) * sum(re_weight), 
                                                        overwt_8 = sum(overwt_8 * re_weight), overwt_weighted_count_8 = weighted.mean(overwt_8, weights * re_weight) * sum(re_weight), 
                                                        overwt_9 = sum(overwt_9 * re_weight), overwt_weighted_count_9 = weighted.mean(overwt_9, weights * re_weight) * sum(re_weight), 
                                                        overwt_10 = sum(overwt_10 * re_weight), overwt_weighted_count_10 = weighted.mean(overwt_10, weights * re_weight) * sum(re_weight), 
                                                        weights = sum(weights * re_weight), sample_size = sum(re_weight)), 
                                                 by = list(mcnty, state, year, sex, race, age_bin, edu_code, marital_ushd)]
#### Save collapsed data sets
saveRDS(data_missing_overwt, file = paste0(output_dir, "/gallup_agg_missing_data_overweight.rds"))
saveRDS(data_combined_agg_overwt, file = paste0(output_dir, "/gallup_agg_overweight.rds"))

########## Process data for underweight
data_missing_underwt <- data_combined[is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_underwt <- na.omit(data_combined, cols = c("mcnty", "year", "sex", "race", "age_bin", "edu_code", "marital_ushd"))
if (nrow(data_missing_underwt) + nrow(data_combined_underwt) != nrow(data_combined)) {
  warning(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_underwt) + nrow(data_combined_underwt), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

######## Collapse underweight by county-year-age-sex and either race/ethnicity or education
data_combined_agg_underwt <- data_combined_underwt[, list(underwt_report = sum(underwt_report * re_weight), underwt_weighted_count_report = weighted.mean(underwt_report, weights * re_weight) * sum(re_weight),
                                                          underwt_1 = sum(underwt_1 * re_weight), underwt_weighted_count_1 = weighted.mean(underwt_1, weights * re_weight) * sum(re_weight), 
                                                          underwt_2 = sum(underwt_2 * re_weight), underwt_weighted_count_2 = weighted.mean(underwt_2, weights * re_weight) * sum(re_weight), 
                                                          underwt_3 = sum(underwt_3 * re_weight), underwt_weighted_count_3 = weighted.mean(underwt_3, weights * re_weight) * sum(re_weight), 
                                                          underwt_4 = sum(underwt_4 * re_weight), underwt_weighted_count_4 = weighted.mean(underwt_4, weights * re_weight) * sum(re_weight), 
                                                          underwt_5 = sum(underwt_5 * re_weight), underwt_weighted_count_5 = weighted.mean(underwt_5, weights * re_weight) * sum(re_weight), 
                                                          underwt_6 = sum(underwt_6 * re_weight), underwt_weighted_count_6 = weighted.mean(underwt_6, weights * re_weight) * sum(re_weight), 
                                                          underwt_7 = sum(underwt_7 * re_weight), underwt_weighted_count_7 = weighted.mean(underwt_7, weights * re_weight) * sum(re_weight), 
                                                          underwt_8 = sum(underwt_8 * re_weight), underwt_weighted_count_8 = weighted.mean(underwt_8, weights * re_weight) * sum(re_weight), 
                                                          underwt_9 = sum(underwt_9 * re_weight), underwt_weighted_count_9 = weighted.mean(underwt_9, weights * re_weight) * sum(re_weight), 
                                                          underwt_10 = sum(underwt_10 * re_weight), underwt_weighted_count_10 = weighted.mean(underwt_10, weights * re_weight) * sum(re_weight),
                                                     weights = sum(weights * re_weight), sample_size = sum(re_weight)), 
                                                   by = list(mcnty, state, year, sex, race, age_bin, edu_code, marital_ushd)]

#### Save collapsed data sets
saveRDS(data_missing_underwt, file = paste0(output_dir, "/gallup_agg_missing_data_underweight.rds"))
saveRDS(data_combined_agg_underwt, file = paste0(output_dir, "/gallup_agg_underweight.rds"))

########## Process data for obesity conditional on overweight
# exclude "report" from overwt_draws, otherwise the indexing of data_combined_agg_obese
# is changed in ways downstream processes can't use
overwt_draws <- overwt_draws[!grepl("report", overwt_draws)]
obese_draws <- obese_draws[!grepl("report", obese_draws)]
bmi_draws <- bmi_draws[!grepl("report", bmi_draws)]
for (var in overwt_draws){
  n <- match(var, overwt_draws)
  print(n)
  # get the suffix for the draw (either a number, or "report")
  suffix <- gsub("overwt_", "", var)
  data_missing_obese <- data_combined[get(var)==1 & (is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(edu_code) | is.na(marital_ushd))]
  data_combined_obese <- data_combined[get(var)==1 & (!is.na(mcnty) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(edu_code) & !is.na(marital_ushd))] 
  obese <- obese_draws[n]

  data_combined_agg_obese <- data_combined_obese[, list(obese = sum(get(obese) * re_weight), 
                                                        obese_weighted_count = weighted.mean(get(obese), weights * re_weight) * sum(re_weight), 
                                                        weights = sum(weights * re_weight), 
                                                        sample_size = sum(re_weight)), 
                                                 by = list(mcnty, state, year, sex, race, age_bin, edu_code, marital_ushd)]
  setnames(data_combined_agg_obese, c('obese','obese_weighted_count','sample_size','weights'), c(obese, paste0('obese_weighted_count_', suffix), paste0('sample_size_', suffix),paste0('weights_', suffix)))
assign(paste0('data_missing_obese_', n), data_missing_obese)
assign(paste0('data_combined_obese_', n), data_combined_obese)
assign(paste0('data_combined_agg_obese_', n), data_combined_agg_obese)
}


######## 4. Collapse general health by county-year-age-sex and either race/ethnicity or education
data_combined_agg_obese_n <- paste("data_combined_agg_obese", 1:length(bmi_draws), sep = "_")
data_combined_agg_obese <- lapply(data_combined_agg_obese_n, function(x) get(x))

data_missing_obese_n <- paste("data_missing_obese", 1:length(bmi_draws), sep = "_")
data_missing_obese <- lapply(data_missing_obese_n, function(x) get(x))
#### Save collapsed data sets
saveRDS(data_missing_obese, file = paste0(output_dir, "/gallup_agg_missing_data_obese.rds"))
saveRDS(data_combined_agg_obese, file = paste0(output_dir, "/gallup_agg_obese.rds"))

######## 17. Produce direct estimates by race/ethnicity
#### Drop rows with missingness in demographic variables
data_combined_backup <- copy(data_combined)
data_combined_no_strat <- data_combined[!(is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(edu_code) | is.na(marital_ushd))]
data_combined <- data_combined[!(is.na(mcnty) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(edu_code) | is.na(marital_ushd))]
### Without phone
data_combined[,sex := as.integer(sex)]

##############################
mcnty_estimates <- data_combined[, list(level = "mcnty", obese_1 = weighted.mean(obese_1, weights * re_weight, na.rm = TRUE), 
                                        obese_2 = weighted.mean(obese_2, weights * re_weight, na.rm = TRUE), 
                                        obese_3 = weighted.mean(obese_3, weights * re_weight, na.rm = TRUE), 
                                        obese_4 = weighted.mean(obese_4, weights * re_weight, na.rm = TRUE), 
                                        obese_5 = weighted.mean(obese_5, weights * re_weight, na.rm = TRUE), 
                                        obese_6 = weighted.mean(obese_6, weights * re_weight, na.rm = TRUE), 
                                        obese_7 = weighted.mean(obese_7, weights * re_weight, na.rm = TRUE), 
                                        obese_8 = weighted.mean(obese_8, weights * re_weight, na.rm = TRUE), 
                                        obese_9 = weighted.mean(obese_9, weights * re_weight, na.rm = TRUE), 
                                        obese_10 = weighted.mean(obese_10, weights * re_weight, na.rm = TRUE), 
                                        underwt_1 = weighted.mean(underwt_1, weights * re_weight, na.rm = TRUE),
                                        underwt_2 = weighted.mean(underwt_2, weights * re_weight, na.rm = TRUE),
                                        underwt_3 = weighted.mean(underwt_3, weights * re_weight, na.rm = TRUE),
                                        underwt_4 = weighted.mean(underwt_4, weights * re_weight, na.rm = TRUE),
                                        underwt_5 = weighted.mean(underwt_5, weights * re_weight, na.rm = TRUE),
                                        underwt_6 = weighted.mean(underwt_6, weights * re_weight, na.rm = TRUE),
                                        underwt_7 = weighted.mean(underwt_7, weights * re_weight, na.rm = TRUE),
                                        underwt_8 = weighted.mean(underwt_8, weights * re_weight, na.rm = TRUE),
                                        underwt_9 = weighted.mean(underwt_9, weights * re_weight, na.rm = TRUE),
                                        underwt_10 = weighted.mean(underwt_10, weights * re_weight, na.rm = TRUE),
                                        overwt_1 = weighted.mean(overwt_1, weights * re_weight, na.rm = TRUE),
                                        overwt_2 = weighted.mean(overwt_2, weights * re_weight, na.rm = TRUE),
                                        overwt_3 = weighted.mean(overwt_3, weights * re_weight, na.rm = TRUE),
                                        overwt_4 = weighted.mean(overwt_4, weights * re_weight, na.rm = TRUE),
                                        overwt_5 = weighted.mean(overwt_5, weights * re_weight, na.rm = TRUE),
                                        overwt_6 = weighted.mean(overwt_6, weights * re_weight, na.rm = TRUE),
                                        overwt_7 = weighted.mean(overwt_7, weights * re_weight, na.rm = TRUE),
                                        overwt_8 = weighted.mean(overwt_8, weights * re_weight, na.rm = TRUE),
                                        overwt_9 = weighted.mean(overwt_9, weights * re_weight, na.rm = TRUE),
                                        overwt_10 = weighted.mean(overwt_10, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_1 = weighted.mean(pred_bmi_1, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_2 = weighted.mean(pred_bmi_2, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_3 = weighted.mean(pred_bmi_3, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_4 = weighted.mean(pred_bmi_4, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_5 = weighted.mean(pred_bmi_5, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_6 = weighted.mean(pred_bmi_6, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_7 = weighted.mean(pred_bmi_7, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_8 = weighted.mean(pred_bmi_8, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_9 = weighted.mean(pred_bmi_9, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_10 = weighted.mean(pred_bmi_10, weights * re_weight, na.rm = TRUE),
                                        weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), 
                                 by = list(mcnty, state, state_name, year, sex, race, age_bin, edu_code, marital_ushd)]

mcnty_estimates_no_strat <- data_combined[, list(level = "mcnty", obese_1 = weighted.mean(obese_1, weights * re_weight, na.rm = TRUE), 
                                                 obese_2 = weighted.mean(obese_2, weights * re_weight, na.rm = TRUE), 
                                                 obese_3 = weighted.mean(obese_3, weights * re_weight, na.rm = TRUE), 
                                                 obese_4 = weighted.mean(obese_4, weights * re_weight, na.rm = TRUE), 
                                                 obese_5 = weighted.mean(obese_5, weights * re_weight, na.rm = TRUE), 
                                                 obese_6 = weighted.mean(obese_6, weights * re_weight, na.rm = TRUE), 
                                                 obese_7 = weighted.mean(obese_7, weights * re_weight, na.rm = TRUE), 
                                                 obese_8 = weighted.mean(obese_8, weights * re_weight, na.rm = TRUE), 
                                                 obese_9 = weighted.mean(obese_9, weights * re_weight, na.rm = TRUE), 
                                                 obese_10 = weighted.mean(obese_10, weights * re_weight, na.rm = TRUE), 
                                                 underwt_1 = weighted.mean(underwt_1, weights * re_weight, na.rm = TRUE),
                                                 underwt_2 = weighted.mean(underwt_2, weights * re_weight, na.rm = TRUE),
                                                 underwt_3 = weighted.mean(underwt_3, weights * re_weight, na.rm = TRUE),
                                                 underwt_4 = weighted.mean(underwt_4, weights * re_weight, na.rm = TRUE),
                                                 underwt_5 = weighted.mean(underwt_5, weights * re_weight, na.rm = TRUE),
                                                 underwt_6 = weighted.mean(underwt_6, weights * re_weight, na.rm = TRUE),
                                                 underwt_7 = weighted.mean(underwt_7, weights * re_weight, na.rm = TRUE),
                                                 underwt_8 = weighted.mean(underwt_8, weights * re_weight, na.rm = TRUE),
                                                 underwt_9 = weighted.mean(underwt_9, weights * re_weight, na.rm = TRUE),
                                                 underwt_10 = weighted.mean(underwt_10, weights * re_weight, na.rm = TRUE),
                                                 overwt_1 = weighted.mean(overwt_1, weights * re_weight, na.rm = TRUE),
                                                 overwt_2 = weighted.mean(overwt_2, weights * re_weight, na.rm = TRUE),
                                                 overwt_3 = weighted.mean(overwt_3, weights * re_weight, na.rm = TRUE),
                                                 overwt_4 = weighted.mean(overwt_4, weights * re_weight, na.rm = TRUE),
                                                 overwt_5 = weighted.mean(overwt_5, weights * re_weight, na.rm = TRUE),
                                                 overwt_6 = weighted.mean(overwt_6, weights * re_weight, na.rm = TRUE),
                                                 overwt_7 = weighted.mean(overwt_7, weights * re_weight, na.rm = TRUE),
                                                 overwt_8 = weighted.mean(overwt_8, weights * re_weight, na.rm = TRUE),
                                                 overwt_9 = weighted.mean(overwt_9, weights * re_weight, na.rm = TRUE),
                                                 overwt_10 = weighted.mean(overwt_10, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_1 = weighted.mean(pred_bmi_1, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_2 = weighted.mean(pred_bmi_2, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_3 = weighted.mean(pred_bmi_3, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_4 = weighted.mean(pred_bmi_4, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_5 = weighted.mean(pred_bmi_5, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_6 = weighted.mean(pred_bmi_6, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_7 = weighted.mean(pred_bmi_7, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_8 = weighted.mean(pred_bmi_8, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_9 = weighted.mean(pred_bmi_9, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_10 = weighted.mean(pred_bmi_10, weights * re_weight, na.rm = TRUE),
                                                 weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)),
                                          by = list(mcnty, state, state_name, year, sex, race, age_bin)]

#### Produce state-level direct estimates
state_estimates <- data_combined[, list(level = "state", obese_1 = weighted.mean(obese_1, weights * re_weight, na.rm = TRUE), 
                                        obese_2 = weighted.mean(obese_2, weights * re_weight, na.rm = TRUE), 
                                        obese_3 = weighted.mean(obese_3, weights * re_weight, na.rm = TRUE), 
                                        obese_4 = weighted.mean(obese_4, weights * re_weight, na.rm = TRUE), 
                                        obese_5 = weighted.mean(obese_5, weights * re_weight, na.rm = TRUE), 
                                        obese_6 = weighted.mean(obese_6, weights * re_weight, na.rm = TRUE), 
                                        obese_7 = weighted.mean(obese_7, weights * re_weight, na.rm = TRUE), 
                                        obese_8 = weighted.mean(obese_8, weights * re_weight, na.rm = TRUE), 
                                        obese_9 = weighted.mean(obese_9, weights * re_weight, na.rm = TRUE), 
                                        obese_10 = weighted.mean(obese_10, weights * re_weight, na.rm = TRUE), 
                                        underwt_1 = weighted.mean(underwt_1, weights * re_weight, na.rm = TRUE),
                                        underwt_2 = weighted.mean(underwt_2, weights * re_weight, na.rm = TRUE),
                                        underwt_3 = weighted.mean(underwt_3, weights * re_weight, na.rm = TRUE),
                                        underwt_4 = weighted.mean(underwt_4, weights * re_weight, na.rm = TRUE),
                                        underwt_5 = weighted.mean(underwt_5, weights * re_weight, na.rm = TRUE),
                                        underwt_6 = weighted.mean(underwt_6, weights * re_weight, na.rm = TRUE),
                                        underwt_7 = weighted.mean(underwt_7, weights * re_weight, na.rm = TRUE),
                                        underwt_8 = weighted.mean(underwt_8, weights * re_weight, na.rm = TRUE),
                                        underwt_9 = weighted.mean(underwt_9, weights * re_weight, na.rm = TRUE),
                                        underwt_10 = weighted.mean(underwt_10, weights * re_weight, na.rm = TRUE),
                                        overwt_1 = weighted.mean(overwt_1, weights * re_weight, na.rm = TRUE),
                                        overwt_2 = weighted.mean(overwt_2, weights * re_weight, na.rm = TRUE),
                                        overwt_3 = weighted.mean(overwt_3, weights * re_weight, na.rm = TRUE),
                                        overwt_4 = weighted.mean(overwt_4, weights * re_weight, na.rm = TRUE),
                                        overwt_5 = weighted.mean(overwt_5, weights * re_weight, na.rm = TRUE),
                                        overwt_6 = weighted.mean(overwt_6, weights * re_weight, na.rm = TRUE),
                                        overwt_7 = weighted.mean(overwt_7, weights * re_weight, na.rm = TRUE),
                                        overwt_8 = weighted.mean(overwt_8, weights * re_weight, na.rm = TRUE),
                                        overwt_9 = weighted.mean(overwt_9, weights * re_weight, na.rm = TRUE),
                                        overwt_10 = weighted.mean(overwt_10, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_1 = weighted.mean(pred_bmi_1, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_2 = weighted.mean(pred_bmi_2, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_3 = weighted.mean(pred_bmi_3, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_4 = weighted.mean(pred_bmi_4, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_5 = weighted.mean(pred_bmi_5, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_6 = weighted.mean(pred_bmi_6, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_7 = weighted.mean(pred_bmi_7, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_8 = weighted.mean(pred_bmi_8, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_9 = weighted.mean(pred_bmi_9, weights * re_weight, na.rm = TRUE),
                                        pred_bmi_10 = weighted.mean(pred_bmi_10, weights * re_weight, na.rm = TRUE),
                                        weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), 
                                 by = list(state, state_name, year, sex, race, age_bin, edu_code, marital_ushd)]

state_estimates_no_strat <- data_combined[, list(level = "state", obese_1 = weighted.mean(obese_1, weights * re_weight, na.rm = TRUE), 
                                                 obese_2 = weighted.mean(obese_2, weights * re_weight, na.rm = TRUE), 
                                                 obese_3 = weighted.mean(obese_3, weights * re_weight, na.rm = TRUE), 
                                                 obese_4 = weighted.mean(obese_4, weights * re_weight, na.rm = TRUE), 
                                                 obese_5 = weighted.mean(obese_5, weights * re_weight, na.rm = TRUE), 
                                                 obese_6 = weighted.mean(obese_6, weights * re_weight, na.rm = TRUE), 
                                                 obese_7 = weighted.mean(obese_7, weights * re_weight, na.rm = TRUE), 
                                                 obese_8 = weighted.mean(obese_8, weights * re_weight, na.rm = TRUE), 
                                                 obese_9 = weighted.mean(obese_9, weights * re_weight, na.rm = TRUE), 
                                                 obese_10 = weighted.mean(obese_10, weights * re_weight, na.rm = TRUE), 
                                                 underwt_1 = weighted.mean(underwt_1, weights * re_weight, na.rm = TRUE),
                                                 underwt_2 = weighted.mean(underwt_2, weights * re_weight, na.rm = TRUE),
                                                 underwt_3 = weighted.mean(underwt_3, weights * re_weight, na.rm = TRUE),
                                                 underwt_4 = weighted.mean(underwt_4, weights * re_weight, na.rm = TRUE),
                                                 underwt_5 = weighted.mean(underwt_5, weights * re_weight, na.rm = TRUE),
                                                 underwt_6 = weighted.mean(underwt_6, weights * re_weight, na.rm = TRUE),
                                                 underwt_7 = weighted.mean(underwt_7, weights * re_weight, na.rm = TRUE),
                                                 underwt_8 = weighted.mean(underwt_8, weights * re_weight, na.rm = TRUE),
                                                 underwt_9 = weighted.mean(underwt_9, weights * re_weight, na.rm = TRUE),
                                                 underwt_10 = weighted.mean(underwt_10, weights * re_weight, na.rm = TRUE),
                                                 overwt_1 = weighted.mean(overwt_1, weights * re_weight, na.rm = TRUE),
                                                 overwt_2 = weighted.mean(overwt_2, weights * re_weight, na.rm = TRUE),
                                                 overwt_3 = weighted.mean(overwt_3, weights * re_weight, na.rm = TRUE),
                                                 overwt_4 = weighted.mean(overwt_4, weights * re_weight, na.rm = TRUE),
                                                 overwt_5 = weighted.mean(overwt_5, weights * re_weight, na.rm = TRUE),
                                                 overwt_6 = weighted.mean(overwt_6, weights * re_weight, na.rm = TRUE),
                                                 overwt_7 = weighted.mean(overwt_7, weights * re_weight, na.rm = TRUE),
                                                 overwt_8 = weighted.mean(overwt_8, weights * re_weight, na.rm = TRUE),
                                                 overwt_9 = weighted.mean(overwt_9, weights * re_weight, na.rm = TRUE),
                                                 overwt_10 = weighted.mean(overwt_10, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_1 = weighted.mean(pred_bmi_1, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_2 = weighted.mean(pred_bmi_2, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_3 = weighted.mean(pred_bmi_3, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_4 = weighted.mean(pred_bmi_4, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_5 = weighted.mean(pred_bmi_5, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_6 = weighted.mean(pred_bmi_6, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_7 = weighted.mean(pred_bmi_7, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_8 = weighted.mean(pred_bmi_8, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_9 = weighted.mean(pred_bmi_9, weights * re_weight, na.rm = TRUE),
                                                 pred_bmi_10 = weighted.mean(pred_bmi_10, weights * re_weight, na.rm = TRUE),
                                                 weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), 
                                          by = list(state, state_name, year, sex, race, age_bin)]

#### Produce national-level direct estimates
# gallup weights are not nationally-representative, so we weight state-level direct estimates by population
national_estimates <- data_combined[, list(level = "natl", obese_1 = weighted.mean(obese_1, weights * re_weight, na.rm = TRUE), 
                                             obese_2 = weighted.mean(obese_2, weights * re_weight, na.rm = TRUE), 
                                             obese_3 = weighted.mean(obese_3, weights * re_weight, na.rm = TRUE), 
                                             obese_4 = weighted.mean(obese_4, weights * re_weight, na.rm = TRUE), 
                                             obese_5 = weighted.mean(obese_5, weights * re_weight, na.rm = TRUE), 
                                             obese_6 = weighted.mean(obese_6, weights * re_weight, na.rm = TRUE), 
                                             obese_7 = weighted.mean(obese_7, weights * re_weight, na.rm = TRUE), 
                                             obese_8 = weighted.mean(obese_8, weights * re_weight, na.rm = TRUE), 
                                             obese_9 = weighted.mean(obese_9, weights * re_weight, na.rm = TRUE), 
                                             obese_10 = weighted.mean(obese_10, weights * re_weight, na.rm = TRUE), 
                                             underwt_1 = weighted.mean(underwt_1, weights * re_weight, na.rm = TRUE),
                                             underwt_2 = weighted.mean(underwt_2, weights * re_weight, na.rm = TRUE),
                                             underwt_3 = weighted.mean(underwt_3, weights * re_weight, na.rm = TRUE),
                                             underwt_4 = weighted.mean(underwt_4, weights * re_weight, na.rm = TRUE),
                                             underwt_5 = weighted.mean(underwt_5, weights * re_weight, na.rm = TRUE),
                                             underwt_6 = weighted.mean(underwt_6, weights * re_weight, na.rm = TRUE),
                                             underwt_7 = weighted.mean(underwt_7, weights * re_weight, na.rm = TRUE),
                                             underwt_8 = weighted.mean(underwt_8, weights * re_weight, na.rm = TRUE),
                                             underwt_9 = weighted.mean(underwt_9, weights * re_weight, na.rm = TRUE),
                                             underwt_10 = weighted.mean(underwt_10, weights * re_weight, na.rm = TRUE),
                                             overwt_1 = weighted.mean(overwt_1, weights * re_weight, na.rm = TRUE),
                                             overwt_2 = weighted.mean(overwt_2, weights * re_weight, na.rm = TRUE),
                                             overwt_3 = weighted.mean(overwt_3, weights * re_weight, na.rm = TRUE),
                                             overwt_4 = weighted.mean(overwt_4, weights * re_weight, na.rm = TRUE),
                                             overwt_5 = weighted.mean(overwt_5, weights * re_weight, na.rm = TRUE),
                                             overwt_6 = weighted.mean(overwt_6, weights * re_weight, na.rm = TRUE),
                                             overwt_7 = weighted.mean(overwt_7, weights * re_weight, na.rm = TRUE),
                                             overwt_8 = weighted.mean(overwt_8, weights * re_weight, na.rm = TRUE),
                                             overwt_9 = weighted.mean(overwt_9, weights * re_weight, na.rm = TRUE),
                                             overwt_10 = weighted.mean(overwt_10, weights * re_weight, na.rm = TRUE),
                                             pred_bmi_1 = weighted.mean(pred_bmi_1, weights * re_weight, na.rm = TRUE),
                                             pred_bmi_2 = weighted.mean(pred_bmi_2, weights * re_weight, na.rm = TRUE),
                                             pred_bmi_3 = weighted.mean(pred_bmi_3, weights * re_weight, na.rm = TRUE),
                                             pred_bmi_4 = weighted.mean(pred_bmi_4, weights * re_weight, na.rm = TRUE),
                                             pred_bmi_5 = weighted.mean(pred_bmi_5, weights * re_weight, na.rm = TRUE),
                                             pred_bmi_6 = weighted.mean(pred_bmi_6, weights * re_weight, na.rm = TRUE),
                                             pred_bmi_7 = weighted.mean(pred_bmi_7, weights * re_weight, na.rm = TRUE),
                                             pred_bmi_8 = weighted.mean(pred_bmi_8, weights * re_weight, na.rm = TRUE),
                                             pred_bmi_9 = weighted.mean(pred_bmi_9, weights * re_weight, na.rm = TRUE),
                                             pred_bmi_10 = weighted.mean(pred_bmi_10, weights * re_weight, na.rm = TRUE),
                                             weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), 
                                      by = list(year, sex, race, age_bin, edu_code, marital_ushd)]

national_estimates_no_strat <- data_combined[, list(level = "natl", obese_1 = weighted.mean(obese_1, weights * re_weight, na.rm = TRUE), 
                                                               obese_2 = weighted.mean(obese_2, weights * re_weight, na.rm = TRUE), 
                                                               obese_3 = weighted.mean(obese_3, weights * re_weight, na.rm = TRUE), 
                                                               obese_4 = weighted.mean(obese_4, weights * re_weight, na.rm = TRUE), 
                                                               obese_5 = weighted.mean(obese_5, weights * re_weight, na.rm = TRUE), 
                                                               obese_6 = weighted.mean(obese_6, weights * re_weight, na.rm = TRUE), 
                                                               obese_7 = weighted.mean(obese_7, weights * re_weight, na.rm = TRUE), 
                                                               obese_8 = weighted.mean(obese_8, weights * re_weight, na.rm = TRUE), 
                                                               obese_9 = weighted.mean(obese_9, weights * re_weight, na.rm = TRUE), 
                                                               obese_10 = weighted.mean(obese_10, weights * re_weight, na.rm = TRUE), 
                                                               underwt_1 = weighted.mean(underwt_1, weights * re_weight, na.rm = TRUE),
                                                               underwt_2 = weighted.mean(underwt_2, weights * re_weight, na.rm = TRUE),
                                                               underwt_3 = weighted.mean(underwt_3, weights * re_weight, na.rm = TRUE),
                                                               underwt_4 = weighted.mean(underwt_4, weights * re_weight, na.rm = TRUE),
                                                               underwt_5 = weighted.mean(underwt_5, weights * re_weight, na.rm = TRUE),
                                                               underwt_6 = weighted.mean(underwt_6, weights * re_weight, na.rm = TRUE),
                                                               underwt_7 = weighted.mean(underwt_7, weights * re_weight, na.rm = TRUE),
                                                               underwt_8 = weighted.mean(underwt_8, weights * re_weight, na.rm = TRUE),
                                                               underwt_9 = weighted.mean(underwt_9, weights * re_weight, na.rm = TRUE),
                                                               underwt_10 = weighted.mean(underwt_10, weights * re_weight, na.rm = TRUE),
                                                               overwt_1 = weighted.mean(overwt_1, weights * re_weight, na.rm = TRUE),
                                                               overwt_2 = weighted.mean(overwt_2, weights * re_weight, na.rm = TRUE),
                                                               overwt_3 = weighted.mean(overwt_3, weights * re_weight, na.rm = TRUE),
                                                               overwt_4 = weighted.mean(overwt_4, weights * re_weight, na.rm = TRUE),
                                                               overwt_5 = weighted.mean(overwt_5, weights * re_weight, na.rm = TRUE),
                                                               overwt_6 = weighted.mean(overwt_6, weights * re_weight, na.rm = TRUE),
                                                               overwt_7 = weighted.mean(overwt_7, weights * re_weight, na.rm = TRUE),
                                                               overwt_8 = weighted.mean(overwt_8, weights * re_weight, na.rm = TRUE),
                                                               overwt_9 = weighted.mean(overwt_9, weights * re_weight, na.rm = TRUE),
                                                               overwt_10 = weighted.mean(overwt_10, weights * re_weight, na.rm = TRUE),
                                                               pred_bmi_1 = weighted.mean(pred_bmi_1, weights * re_weight, na.rm = TRUE),
                                                               pred_bmi_2 = weighted.mean(pred_bmi_2, weights * re_weight, na.rm = TRUE),
                                                               pred_bmi_3 = weighted.mean(pred_bmi_3, weights * re_weight, na.rm = TRUE),
                                                               pred_bmi_4 = weighted.mean(pred_bmi_4, weights * re_weight, na.rm = TRUE),
                                                               pred_bmi_5 = weighted.mean(pred_bmi_5, weights * re_weight, na.rm = TRUE),
                                                               pred_bmi_6 = weighted.mean(pred_bmi_6, weights * re_weight, na.rm = TRUE),
                                                               pred_bmi_7 = weighted.mean(pred_bmi_7, weights * re_weight, na.rm = TRUE),
                                                               pred_bmi_8 = weighted.mean(pred_bmi_8, weights * re_weight, na.rm = TRUE),
                                                               pred_bmi_9 = weighted.mean(pred_bmi_9, weights * re_weight, na.rm = TRUE),
                                                               pred_bmi_10 = weighted.mean(pred_bmi_10, weights * re_weight, na.rm = TRUE),
                                                               weights = sum(weights * re_weight, na.rm = TRUE), sample_size = sum(re_weight, na.rm = TRUE)), by = list(year, sex, race, age_bin)]

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

# merge with age-standardized weights
setnames(std_wt, 'age', 'age_bin')
direct_estimates <- merge(direct_estimates, std_wt, by='age_bin')
direct_estimates_no_strat <- merge(direct_estimates_no_strat, std_wt, by='age_bin')

#### Aggregate across age standardized
agestd_agg <- direct_estimates[, list(age_bin = 99, wt = sum(wt, na.rm = T),
                                      obese_1 = weighted.mean(obese_1, wt, na.rm = TRUE), 
                                      obese_2 = weighted.mean(obese_2, wt, na.rm = TRUE), 
                                      obese_3 = weighted.mean(obese_3, wt, na.rm = TRUE), 
                                      obese_4 = weighted.mean(obese_4, wt, na.rm = TRUE), 
                                      obese_5 = weighted.mean(obese_5, wt, na.rm = TRUE), 
                                      obese_6 = weighted.mean(obese_6, wt, na.rm = TRUE), 
                                      obese_7 = weighted.mean(obese_7, wt, na.rm = TRUE), 
                                      obese_8 = weighted.mean(obese_8, wt, na.rm = TRUE), 
                                      obese_9 = weighted.mean(obese_9, wt, na.rm = TRUE), 
                                      obese_10 = weighted.mean(obese_10, wt, na.rm = TRUE), 
                                      underwt_1 = weighted.mean(underwt_1, wt, na.rm = TRUE),
                                      underwt_2 = weighted.mean(underwt_2, wt, na.rm = TRUE),
                                      underwt_3 = weighted.mean(underwt_3, wt, na.rm = TRUE),
                                      underwt_4 = weighted.mean(underwt_4, wt, na.rm = TRUE),
                                      underwt_5 = weighted.mean(underwt_5, wt, na.rm = TRUE),
                                      underwt_6 = weighted.mean(underwt_6, wt, na.rm = TRUE),
                                      underwt_7 = weighted.mean(underwt_7, wt, na.rm = TRUE),
                                      underwt_8 = weighted.mean(underwt_8, wt, na.rm = TRUE),
                                      underwt_9 = weighted.mean(underwt_9, wt, na.rm = TRUE),
                                      underwt_10 = weighted.mean(underwt_10, wt, na.rm = TRUE),
                                      overwt_1 = weighted.mean(overwt_1, wt, na.rm = TRUE),
                                      overwt_2 = weighted.mean(overwt_2, wt, na.rm = TRUE),
                                      overwt_3 = weighted.mean(overwt_3, wt, na.rm = TRUE),
                                      overwt_4 = weighted.mean(overwt_4, wt, na.rm = TRUE),
                                      overwt_5 = weighted.mean(overwt_5, wt, na.rm = TRUE),
                                      overwt_6 = weighted.mean(overwt_6, wt, na.rm = TRUE),
                                      overwt_7 = weighted.mean(overwt_7, wt, na.rm = TRUE),
                                      overwt_8 = weighted.mean(overwt_8, wt, na.rm = TRUE),
                                      overwt_9 = weighted.mean(overwt_9, wt, na.rm = TRUE),
                                      overwt_10 = weighted.mean(overwt_10, wt, na.rm = TRUE),
                                      pred_bmi_1 = weighted.mean(pred_bmi_1, wt, na.rm = TRUE),
                                      pred_bmi_2 = weighted.mean(pred_bmi_2, wt, na.rm = TRUE),
                                      pred_bmi_3 = weighted.mean(pred_bmi_3, wt, na.rm = TRUE),
                                      pred_bmi_4 = weighted.mean(pred_bmi_4, wt, na.rm = TRUE),
                                      pred_bmi_5 = weighted.mean(pred_bmi_5, wt, na.rm = TRUE),
                                      pred_bmi_6 = weighted.mean(pred_bmi_6, wt, na.rm = TRUE),
                                      pred_bmi_7 = weighted.mean(pred_bmi_7, wt, na.rm = TRUE),
                                      pred_bmi_8 = weighted.mean(pred_bmi_8, wt, na.rm = TRUE),
                                      pred_bmi_9 = weighted.mean(pred_bmi_9, wt, na.rm = TRUE),
                                      pred_bmi_10 = weighted.mean(pred_bmi_10, wt, na.rm = TRUE),
                                      weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), 
                               by = list(area, level, year, sex, race, edu_code, marital_ushd)]
direct_estimates <- rbindlist(list(direct_estimates, agestd_agg), use.names = TRUE, fill = TRUE)

agestd_agg_no_strat <- direct_estimates_no_strat[, list(age_bin = 99, wt = sum(wt, na.rm = T),
                                                        obese_1 = weighted.mean(obese_1, wt, na.rm = TRUE), 
                                                        obese_2 = weighted.mean(obese_2, wt, na.rm = TRUE), 
                                                        obese_3 = weighted.mean(obese_3, wt, na.rm = TRUE), 
                                                        obese_4 = weighted.mean(obese_4, wt, na.rm = TRUE), 
                                                        obese_5 = weighted.mean(obese_5, wt, na.rm = TRUE), 
                                                        obese_6 = weighted.mean(obese_6, wt, na.rm = TRUE), 
                                                        obese_7 = weighted.mean(obese_7, wt, na.rm = TRUE), 
                                                        obese_8 = weighted.mean(obese_8, wt, na.rm = TRUE), 
                                                        obese_9 = weighted.mean(obese_9, wt, na.rm = TRUE), 
                                                        obese_10 = weighted.mean(obese_10, wt, na.rm = TRUE), 
                                                        underwt_1 = weighted.mean(underwt_1, wt, na.rm = TRUE),
                                                        underwt_2 = weighted.mean(underwt_2, wt, na.rm = TRUE),
                                                        underwt_3 = weighted.mean(underwt_3, wt, na.rm = TRUE),
                                                        underwt_4 = weighted.mean(underwt_4, wt, na.rm = TRUE),
                                                        underwt_5 = weighted.mean(underwt_5, wt, na.rm = TRUE),
                                                        underwt_6 = weighted.mean(underwt_6, wt, na.rm = TRUE),
                                                        underwt_7 = weighted.mean(underwt_7, wt, na.rm = TRUE),
                                                        underwt_8 = weighted.mean(underwt_8, wt, na.rm = TRUE),
                                                        underwt_9 = weighted.mean(underwt_9, wt, na.rm = TRUE),
                                                        underwt_10 = weighted.mean(underwt_10, wt, na.rm = TRUE),
                                                        overwt_1 = weighted.mean(overwt_1, wt, na.rm = TRUE),
                                                        overwt_2 = weighted.mean(overwt_2, wt, na.rm = TRUE),
                                                        overwt_3 = weighted.mean(overwt_3, wt, na.rm = TRUE),
                                                        overwt_4 = weighted.mean(overwt_4, wt, na.rm = TRUE),
                                                        overwt_5 = weighted.mean(overwt_5, wt, na.rm = TRUE),
                                                        overwt_6 = weighted.mean(overwt_6, wt, na.rm = TRUE),
                                                        overwt_7 = weighted.mean(overwt_7, wt, na.rm = TRUE),
                                                        overwt_8 = weighted.mean(overwt_8, wt, na.rm = TRUE),
                                                        overwt_9 = weighted.mean(overwt_9, wt, na.rm = TRUE),
                                                        overwt_10 = weighted.mean(overwt_10, wt, na.rm = TRUE),
                                                        pred_bmi_1 = weighted.mean(pred_bmi_1, wt, na.rm = TRUE),
                                                        pred_bmi_2 = weighted.mean(pred_bmi_2, wt, na.rm = TRUE),
                                                        pred_bmi_3 = weighted.mean(pred_bmi_3, wt, na.rm = TRUE),
                                                        pred_bmi_4 = weighted.mean(pred_bmi_4, wt, na.rm = TRUE),
                                                        pred_bmi_5 = weighted.mean(pred_bmi_5, wt, na.rm = TRUE),
                                                        pred_bmi_6 = weighted.mean(pred_bmi_6, wt, na.rm = TRUE),
                                                        pred_bmi_7 = weighted.mean(pred_bmi_7, wt, na.rm = TRUE),
                                                        pred_bmi_8 = weighted.mean(pred_bmi_8, wt, na.rm = TRUE),
                                                        pred_bmi_9 = weighted.mean(pred_bmi_9, wt, na.rm = TRUE),
                                                        pred_bmi_10 = weighted.mean(pred_bmi_10, wt, na.rm = TRUE),
                                                        weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), 
                                                 by = list(area, level, year, sex, race)]
direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, agestd_agg_no_strat), use.names = TRUE, fill = TRUE)

#### Aggregate across sexes
sex_agg <- direct_estimates[, list(sex = 3, 
                                   obese_1 = weighted.mean(obese_1, weights, na.rm = TRUE), 
                                   obese_2 = weighted.mean(obese_2, weights, na.rm = TRUE), 
                                   obese_3 = weighted.mean(obese_3, weights, na.rm = TRUE), 
                                   obese_4 = weighted.mean(obese_4, weights, na.rm = TRUE), 
                                   obese_5 = weighted.mean(obese_5, weights, na.rm = TRUE), 
                                   obese_6 = weighted.mean(obese_6, weights, na.rm = TRUE), 
                                   obese_7 = weighted.mean(obese_7, weights, na.rm = TRUE), 
                                   obese_8 = weighted.mean(obese_8, weights, na.rm = TRUE), 
                                   obese_9 = weighted.mean(obese_9, weights, na.rm = TRUE), 
                                   obese_10 = weighted.mean(obese_10, weights, na.rm = TRUE), 
                                   underwt_1 = weighted.mean(underwt_1, weights, na.rm = TRUE),
                                   underwt_2 = weighted.mean(underwt_2, weights, na.rm = TRUE),
                                   underwt_3 = weighted.mean(underwt_3, weights, na.rm = TRUE),
                                   underwt_4 = weighted.mean(underwt_4, weights, na.rm = TRUE),
                                   underwt_5 = weighted.mean(underwt_5, weights, na.rm = TRUE),
                                   underwt_6 = weighted.mean(underwt_6, weights, na.rm = TRUE),
                                   underwt_7 = weighted.mean(underwt_7, weights, na.rm = TRUE),
                                   underwt_8 = weighted.mean(underwt_8, weights, na.rm = TRUE),
                                   underwt_9 = weighted.mean(underwt_9, weights, na.rm = TRUE),
                                   underwt_10 = weighted.mean(underwt_10, weights, na.rm = TRUE),
                                   overwt_1 = weighted.mean(overwt_1, weights, na.rm = TRUE),
                                   overwt_2 = weighted.mean(overwt_2, weights, na.rm = TRUE),
                                   overwt_3 = weighted.mean(overwt_3, weights, na.rm = TRUE),
                                   overwt_4 = weighted.mean(overwt_4, weights, na.rm = TRUE),
                                   overwt_5 = weighted.mean(overwt_5, weights, na.rm = TRUE),
                                   overwt_6 = weighted.mean(overwt_6, weights, na.rm = TRUE),
                                   overwt_7 = weighted.mean(overwt_7, weights, na.rm = TRUE),
                                   overwt_8 = weighted.mean(overwt_8, weights, na.rm = TRUE),
                                   overwt_9 = weighted.mean(overwt_9, weights, na.rm = TRUE),
                                   overwt_10 = weighted.mean(overwt_10, weights, na.rm = TRUE),
                                   pred_bmi_1 = weighted.mean(pred_bmi_1, weights, na.rm = TRUE),
                                   pred_bmi_2 = weighted.mean(pred_bmi_2, weights, na.rm = TRUE),
                                   pred_bmi_3 = weighted.mean(pred_bmi_3, weights, na.rm = TRUE),
                                   pred_bmi_4 = weighted.mean(pred_bmi_4, weights, na.rm = TRUE),
                                   pred_bmi_5 = weighted.mean(pred_bmi_5, weights, na.rm = TRUE),
                                   pred_bmi_6 = weighted.mean(pred_bmi_6, weights, na.rm = TRUE),
                                   pred_bmi_7 = weighted.mean(pred_bmi_7, weights, na.rm = TRUE),
                                   pred_bmi_8 = weighted.mean(pred_bmi_8, weights, na.rm = TRUE),
                                   pred_bmi_9 = weighted.mean(pred_bmi_9, weights, na.rm = TRUE),
                                   pred_bmi_10 = weighted.mean(pred_bmi_10, weights, na.rm = TRUE),
                                   weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), 
                            by = list(area, level, year, race, age_bin, edu_code, marital_ushd)]
direct_estimates <- rbindlist(list(direct_estimates, sex_agg), use.names = TRUE, fill = TRUE)

sex_agg_no_strat <- direct_estimates_no_strat[, list(sex = 3, 
                                                     obese_1 = weighted.mean(obese_1, weights, na.rm = TRUE), 
                                                     obese_2 = weighted.mean(obese_2, weights, na.rm = TRUE), 
                                                     obese_3 = weighted.mean(obese_3, weights, na.rm = TRUE), 
                                                     obese_4 = weighted.mean(obese_4, weights, na.rm = TRUE), 
                                                     obese_5 = weighted.mean(obese_5, weights, na.rm = TRUE), 
                                                     obese_6 = weighted.mean(obese_6, weights, na.rm = TRUE), 
                                                     obese_7 = weighted.mean(obese_7, weights, na.rm = TRUE), 
                                                     obese_8 = weighted.mean(obese_8, weights, na.rm = TRUE), 
                                                     obese_9 = weighted.mean(obese_9, weights, na.rm = TRUE), 
                                                     obese_10 = weighted.mean(obese_10, weights, na.rm = TRUE), 
                                                     underwt_1 = weighted.mean(underwt_1, weights, na.rm = TRUE),
                                                     underwt_2 = weighted.mean(underwt_2, weights, na.rm = TRUE),
                                                     underwt_3 = weighted.mean(underwt_3, weights, na.rm = TRUE),
                                                     underwt_4 = weighted.mean(underwt_4, weights, na.rm = TRUE),
                                                     underwt_5 = weighted.mean(underwt_5, weights, na.rm = TRUE),
                                                     underwt_6 = weighted.mean(underwt_6, weights, na.rm = TRUE),
                                                     underwt_7 = weighted.mean(underwt_7, weights, na.rm = TRUE),
                                                     underwt_8 = weighted.mean(underwt_8, weights, na.rm = TRUE),
                                                     underwt_9 = weighted.mean(underwt_9, weights, na.rm = TRUE),
                                                     underwt_10 = weighted.mean(underwt_10, weights, na.rm = TRUE),
                                                     overwt_1 = weighted.mean(overwt_1, weights, na.rm = TRUE),
                                                     overwt_2 = weighted.mean(overwt_2, weights, na.rm = TRUE),
                                                     overwt_3 = weighted.mean(overwt_3, weights, na.rm = TRUE),
                                                     overwt_4 = weighted.mean(overwt_4, weights, na.rm = TRUE),
                                                     overwt_5 = weighted.mean(overwt_5, weights, na.rm = TRUE),
                                                     overwt_6 = weighted.mean(overwt_6, weights, na.rm = TRUE),
                                                     overwt_7 = weighted.mean(overwt_7, weights, na.rm = TRUE),
                                                     overwt_8 = weighted.mean(overwt_8, weights, na.rm = TRUE),
                                                     overwt_9 = weighted.mean(overwt_9, weights, na.rm = TRUE),
                                                     overwt_10 = weighted.mean(overwt_10, weights, na.rm = TRUE),
                                                     pred_bmi_1 = weighted.mean(pred_bmi_1, weights, na.rm = TRUE),
                                                     pred_bmi_2 = weighted.mean(pred_bmi_2, weights, na.rm = TRUE),
                                                     pred_bmi_3 = weighted.mean(pred_bmi_3, weights, na.rm = TRUE),
                                                     pred_bmi_4 = weighted.mean(pred_bmi_4, weights, na.rm = TRUE),
                                                     pred_bmi_5 = weighted.mean(pred_bmi_5, weights, na.rm = TRUE),
                                                     pred_bmi_6 = weighted.mean(pred_bmi_6, weights, na.rm = TRUE),
                                                     pred_bmi_7 = weighted.mean(pred_bmi_7, weights, na.rm = TRUE),
                                                     pred_bmi_8 = weighted.mean(pred_bmi_8, weights, na.rm = TRUE),
                                                     pred_bmi_9 = weighted.mean(pred_bmi_9, weights, na.rm = TRUE),
                                                     pred_bmi_10 = weighted.mean(pred_bmi_10, weights, na.rm = TRUE),
                                                     weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), 
                                              by = list(area, level, year, race, age_bin)]
direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, sex_agg_no_strat), use.names = TRUE, fill = TRUE)


#### Aggregate across races
race_agg <- direct_estimates[, list(race = race_all,
                                    obese_1 = weighted.mean(obese_1, weights, na.rm = TRUE), 
                                    obese_2 = weighted.mean(obese_2, weights, na.rm = TRUE), 
                                    obese_3 = weighted.mean(obese_3, weights, na.rm = TRUE), 
                                    obese_4 = weighted.mean(obese_4, weights, na.rm = TRUE), 
                                    obese_5 = weighted.mean(obese_5, weights, na.rm = TRUE), 
                                    obese_6 = weighted.mean(obese_6, weights, na.rm = TRUE), 
                                    obese_7 = weighted.mean(obese_7, weights, na.rm = TRUE), 
                                    obese_8 = weighted.mean(obese_8, weights, na.rm = TRUE), 
                                    obese_9 = weighted.mean(obese_9, weights, na.rm = TRUE), 
                                    obese_10 = weighted.mean(obese_10, weights, na.rm = TRUE), 
                                    underwt_1 = weighted.mean(underwt_1, weights, na.rm = TRUE),
                                    underwt_2 = weighted.mean(underwt_2, weights, na.rm = TRUE),
                                    underwt_3 = weighted.mean(underwt_3, weights, na.rm = TRUE),
                                    underwt_4 = weighted.mean(underwt_4, weights, na.rm = TRUE),
                                    underwt_5 = weighted.mean(underwt_5, weights, na.rm = TRUE),
                                    underwt_6 = weighted.mean(underwt_6, weights, na.rm = TRUE),
                                    underwt_7 = weighted.mean(underwt_7, weights, na.rm = TRUE),
                                    underwt_8 = weighted.mean(underwt_8, weights, na.rm = TRUE),
                                    underwt_9 = weighted.mean(underwt_9, weights, na.rm = TRUE),
                                    underwt_10 = weighted.mean(underwt_10, weights, na.rm = TRUE),
                                    overwt_1 = weighted.mean(overwt_1, weights, na.rm = TRUE),
                                    overwt_2 = weighted.mean(overwt_2, weights, na.rm = TRUE),
                                    overwt_3 = weighted.mean(overwt_3, weights, na.rm = TRUE),
                                    overwt_4 = weighted.mean(overwt_4, weights, na.rm = TRUE),
                                    overwt_5 = weighted.mean(overwt_5, weights, na.rm = TRUE),
                                    overwt_6 = weighted.mean(overwt_6, weights, na.rm = TRUE),
                                    overwt_7 = weighted.mean(overwt_7, weights, na.rm = TRUE),
                                    overwt_8 = weighted.mean(overwt_8, weights, na.rm = TRUE),
                                    overwt_9 = weighted.mean(overwt_9, weights, na.rm = TRUE),
                                    overwt_10 = weighted.mean(overwt_10, weights, na.rm = TRUE),
                                    pred_bmi_1 = weighted.mean(pred_bmi_1, weights, na.rm = TRUE),
                                    pred_bmi_2 = weighted.mean(pred_bmi_2, weights, na.rm = TRUE),
                                    pred_bmi_3 = weighted.mean(pred_bmi_3, weights, na.rm = TRUE),
                                    pred_bmi_4 = weighted.mean(pred_bmi_4, weights, na.rm = TRUE),
                                    pred_bmi_5 = weighted.mean(pred_bmi_5, weights, na.rm = TRUE),
                                    pred_bmi_6 = weighted.mean(pred_bmi_6, weights, na.rm = TRUE),
                                    pred_bmi_7 = weighted.mean(pred_bmi_7, weights, na.rm = TRUE),
                                    pred_bmi_8 = weighted.mean(pred_bmi_8, weights, na.rm = TRUE),
                                    pred_bmi_9 = weighted.mean(pred_bmi_9, weights, na.rm = TRUE),
                                    pred_bmi_10 = weighted.mean(pred_bmi_10, weights, na.rm = TRUE),
                                    weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), 
                             by = list(area, level, year, sex, age_bin, edu_code, marital_ushd)]
direct_estimates <- rbindlist(list(direct_estimates, race_agg), use.names = TRUE, fill = TRUE)

race_agg_no_strat <- direct_estimates_no_strat[, list(race = race_all, 
                                                      obese_1 = weighted.mean(obese_1, weights, na.rm = TRUE), 
                                                      obese_2 = weighted.mean(obese_2, weights, na.rm = TRUE), 
                                                      obese_3 = weighted.mean(obese_3, weights, na.rm = TRUE), 
                                                      obese_4 = weighted.mean(obese_4, weights, na.rm = TRUE), 
                                                      obese_5 = weighted.mean(obese_5, weights, na.rm = TRUE), 
                                                      obese_6 = weighted.mean(obese_6, weights, na.rm = TRUE), 
                                                      obese_7 = weighted.mean(obese_7, weights, na.rm = TRUE), 
                                                      obese_8 = weighted.mean(obese_8, weights, na.rm = TRUE), 
                                                      obese_9 = weighted.mean(obese_9, weights, na.rm = TRUE), 
                                                      obese_10 = weighted.mean(obese_10, weights, na.rm = TRUE), 
                                                      underwt_1 = weighted.mean(underwt_1, weights, na.rm = TRUE),
                                                      underwt_2 = weighted.mean(underwt_2, weights, na.rm = TRUE),
                                                      underwt_3 = weighted.mean(underwt_3, weights, na.rm = TRUE),
                                                      underwt_4 = weighted.mean(underwt_4, weights, na.rm = TRUE),
                                                      underwt_5 = weighted.mean(underwt_5, weights, na.rm = TRUE),
                                                      underwt_6 = weighted.mean(underwt_6, weights, na.rm = TRUE),
                                                      underwt_7 = weighted.mean(underwt_7, weights, na.rm = TRUE),
                                                      underwt_8 = weighted.mean(underwt_8, weights, na.rm = TRUE),
                                                      underwt_9 = weighted.mean(underwt_9, weights, na.rm = TRUE),
                                                      underwt_10 = weighted.mean(underwt_10, weights, na.rm = TRUE),
                                                      overwt_1 = weighted.mean(overwt_1, weights, na.rm = TRUE),
                                                      overwt_2 = weighted.mean(overwt_2, weights, na.rm = TRUE),
                                                      overwt_3 = weighted.mean(overwt_3, weights, na.rm = TRUE),
                                                      overwt_4 = weighted.mean(overwt_4, weights, na.rm = TRUE),
                                                      overwt_5 = weighted.mean(overwt_5, weights, na.rm = TRUE),
                                                      overwt_6 = weighted.mean(overwt_6, weights, na.rm = TRUE),
                                                      overwt_7 = weighted.mean(overwt_7, weights, na.rm = TRUE),
                                                      overwt_8 = weighted.mean(overwt_8, weights, na.rm = TRUE),
                                                      overwt_9 = weighted.mean(overwt_9, weights, na.rm = TRUE),
                                                      overwt_10 = weighted.mean(overwt_10, weights, na.rm = TRUE),
                                                      pred_bmi_1 = weighted.mean(pred_bmi_1, weights, na.rm = TRUE),
                                                      pred_bmi_2 = weighted.mean(pred_bmi_2, weights, na.rm = TRUE),
                                                      pred_bmi_3 = weighted.mean(pred_bmi_3, weights, na.rm = TRUE),
                                                      pred_bmi_4 = weighted.mean(pred_bmi_4, weights, na.rm = TRUE),
                                                      pred_bmi_5 = weighted.mean(pred_bmi_5, weights, na.rm = TRUE),
                                                      pred_bmi_6 = weighted.mean(pred_bmi_6, weights, na.rm = TRUE),
                                                      pred_bmi_7 = weighted.mean(pred_bmi_7, weights, na.rm = TRUE),
                                                      pred_bmi_8 = weighted.mean(pred_bmi_8, weights, na.rm = TRUE),
                                                      pred_bmi_9 = weighted.mean(pred_bmi_9, weights, na.rm = TRUE),
                                                      pred_bmi_10 = weighted.mean(pred_bmi_10, weights, na.rm = TRUE),
                                                      weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), 
                                               by = list(area, level, year, sex, age_bin)]
direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, race_agg_no_strat), use.names = TRUE, fill = TRUE)

#### Aggregate across ages
age_agg <- direct_estimates[, list(age_bin = 98,
                                   obese_1 = weighted.mean(obese_1, weights, na.rm = TRUE), 
                                   obese_2 = weighted.mean(obese_2, weights, na.rm = TRUE), 
                                   obese_3 = weighted.mean(obese_3, weights, na.rm = TRUE), 
                                   obese_4 = weighted.mean(obese_4, weights, na.rm = TRUE), 
                                   obese_5 = weighted.mean(obese_5, weights, na.rm = TRUE), 
                                   obese_6 = weighted.mean(obese_6, weights, na.rm = TRUE), 
                                   obese_7 = weighted.mean(obese_7, weights, na.rm = TRUE), 
                                   obese_8 = weighted.mean(obese_8, weights, na.rm = TRUE), 
                                   obese_9 = weighted.mean(obese_9, weights, na.rm = TRUE), 
                                   obese_10 = weighted.mean(obese_10, weights, na.rm = TRUE), 
                                   underwt_1 = weighted.mean(underwt_1, weights, na.rm = TRUE),
                                   underwt_2 = weighted.mean(underwt_2, weights, na.rm = TRUE),
                                   underwt_3 = weighted.mean(underwt_3, weights, na.rm = TRUE),
                                   underwt_4 = weighted.mean(underwt_4, weights, na.rm = TRUE),
                                   underwt_5 = weighted.mean(underwt_5, weights, na.rm = TRUE),
                                   underwt_6 = weighted.mean(underwt_6, weights, na.rm = TRUE),
                                   underwt_7 = weighted.mean(underwt_7, weights, na.rm = TRUE),
                                   underwt_8 = weighted.mean(underwt_8, weights, na.rm = TRUE),
                                   underwt_9 = weighted.mean(underwt_9, weights, na.rm = TRUE),
                                   underwt_10 = weighted.mean(underwt_10, weights, na.rm = TRUE),
                                   overwt_1 = weighted.mean(overwt_1, weights, na.rm = TRUE),
                                   overwt_2 = weighted.mean(overwt_2, weights, na.rm = TRUE),
                                   overwt_3 = weighted.mean(overwt_3, weights, na.rm = TRUE),
                                   overwt_4 = weighted.mean(overwt_4, weights, na.rm = TRUE),
                                   overwt_5 = weighted.mean(overwt_5, weights, na.rm = TRUE),
                                   overwt_6 = weighted.mean(overwt_6, weights, na.rm = TRUE),
                                   overwt_7 = weighted.mean(overwt_7, weights, na.rm = TRUE),
                                   overwt_8 = weighted.mean(overwt_8, weights, na.rm = TRUE),
                                   overwt_9 = weighted.mean(overwt_9, weights, na.rm = TRUE),
                                   overwt_10 = weighted.mean(overwt_10, weights, na.rm = TRUE),
                                   pred_bmi_1 = weighted.mean(pred_bmi_1, weights, na.rm = TRUE),
                                   pred_bmi_2 = weighted.mean(pred_bmi_2, weights, na.rm = TRUE),
                                   pred_bmi_3 = weighted.mean(pred_bmi_3, weights, na.rm = TRUE),
                                   pred_bmi_4 = weighted.mean(pred_bmi_4, weights, na.rm = TRUE),
                                   pred_bmi_5 = weighted.mean(pred_bmi_5, weights, na.rm = TRUE),
                                   pred_bmi_6 = weighted.mean(pred_bmi_6, weights, na.rm = TRUE),
                                   pred_bmi_7 = weighted.mean(pred_bmi_7, weights, na.rm = TRUE),
                                   pred_bmi_8 = weighted.mean(pred_bmi_8, weights, na.rm = TRUE),
                                   pred_bmi_9 = weighted.mean(pred_bmi_9, weights, na.rm = TRUE),
                                   pred_bmi_10 = weighted.mean(pred_bmi_10, weights, na.rm = TRUE),
                                   weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), 
                            by = list(area, level, year, sex, race, edu_code, marital_ushd)]
direct_estimates <- rbindlist(list(direct_estimates, age_agg), use.names = TRUE, fill = TRUE)

age_agg_no_strat <- direct_estimates_no_strat[, list(age_bin = 98, 
                                                     obese_1 = weighted.mean(obese_1, weights, na.rm = TRUE), 
                                                     obese_2 = weighted.mean(obese_2, weights, na.rm = TRUE), 
                                                     obese_3 = weighted.mean(obese_3, weights, na.rm = TRUE), 
                                                     obese_4 = weighted.mean(obese_4, weights, na.rm = TRUE), 
                                                     obese_5 = weighted.mean(obese_5, weights, na.rm = TRUE), 
                                                     obese_6 = weighted.mean(obese_6, weights, na.rm = TRUE), 
                                                     obese_7 = weighted.mean(obese_7, weights, na.rm = TRUE), 
                                                     obese_8 = weighted.mean(obese_8, weights, na.rm = TRUE), 
                                                     obese_9 = weighted.mean(obese_9, weights, na.rm = TRUE), 
                                                     obese_10 = weighted.mean(obese_10, weights, na.rm = TRUE), 
                                                     underwt_1 = weighted.mean(underwt_1, weights, na.rm = TRUE),
                                                     underwt_2 = weighted.mean(underwt_2, weights, na.rm = TRUE),
                                                     underwt_3 = weighted.mean(underwt_3, weights, na.rm = TRUE),
                                                     underwt_4 = weighted.mean(underwt_4, weights, na.rm = TRUE),
                                                     underwt_5 = weighted.mean(underwt_5, weights, na.rm = TRUE),
                                                     underwt_6 = weighted.mean(underwt_6, weights, na.rm = TRUE),
                                                     underwt_7 = weighted.mean(underwt_7, weights, na.rm = TRUE),
                                                     underwt_8 = weighted.mean(underwt_8, weights, na.rm = TRUE),
                                                     underwt_9 = weighted.mean(underwt_9, weights, na.rm = TRUE),
                                                     underwt_10 = weighted.mean(underwt_10, weights, na.rm = TRUE),
                                                     overwt_1 = weighted.mean(overwt_1, weights, na.rm = TRUE),
                                                     overwt_2 = weighted.mean(overwt_2, weights, na.rm = TRUE),
                                                     overwt_3 = weighted.mean(overwt_3, weights, na.rm = TRUE),
                                                     overwt_4 = weighted.mean(overwt_4, weights, na.rm = TRUE),
                                                     overwt_5 = weighted.mean(overwt_5, weights, na.rm = TRUE),
                                                     overwt_6 = weighted.mean(overwt_6, weights, na.rm = TRUE),
                                                     overwt_7 = weighted.mean(overwt_7, weights, na.rm = TRUE),
                                                     overwt_8 = weighted.mean(overwt_8, weights, na.rm = TRUE),
                                                     overwt_9 = weighted.mean(overwt_9, weights, na.rm = TRUE),
                                                     overwt_10 = weighted.mean(overwt_10, weights, na.rm = TRUE),
                                                     pred_bmi_1 = weighted.mean(pred_bmi_1, weights, na.rm = TRUE),
                                                     pred_bmi_2 = weighted.mean(pred_bmi_2, weights, na.rm = TRUE),
                                                     pred_bmi_3 = weighted.mean(pred_bmi_3, weights, na.rm = TRUE),
                                                     pred_bmi_4 = weighted.mean(pred_bmi_4, weights, na.rm = TRUE),
                                                     pred_bmi_5 = weighted.mean(pred_bmi_5, weights, na.rm = TRUE),
                                                     pred_bmi_6 = weighted.mean(pred_bmi_6, weights, na.rm = TRUE),
                                                     pred_bmi_7 = weighted.mean(pred_bmi_7, weights, na.rm = TRUE),
                                                     pred_bmi_8 = weighted.mean(pred_bmi_8, weights, na.rm = TRUE),
                                                     pred_bmi_9 = weighted.mean(pred_bmi_9, weights, na.rm = TRUE),
                                                     pred_bmi_10 = weighted.mean(pred_bmi_10, weights, na.rm = TRUE),
                                                     weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), 
                                              by = list(area, level, year, sex, race)]
direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, age_agg_no_strat), use.names = TRUE, fill = TRUE)

### save output
setnames(direct_estimates_no_strat, "age_bin", "age")
saveRDS(direct_estimates_no_strat, file = paste0(output_dir, "/gallup_direct_estimates_no_strat.rds"))
saveRDS(state_estimates_no_strat, file = paste0(output_dir, "/gallup_state_level_no_strat.rds"))

#### Aggregate across years
edu_agg <- direct_estimates[, list(edu_code = edu_all, obese_1 = weighted.mean(obese_1, weights, na.rm = TRUE), 
                                    obese_2 = weighted.mean(obese_2, weights, na.rm = TRUE), 
                                    obese_3 = weighted.mean(obese_3, weights, na.rm = TRUE), 
                                    obese_4 = weighted.mean(obese_4, weights, na.rm = TRUE), 
                                    obese_5 = weighted.mean(obese_5, weights, na.rm = TRUE), 
                                    obese_6 = weighted.mean(obese_6, weights, na.rm = TRUE), 
                                    obese_7 = weighted.mean(obese_7, weights, na.rm = TRUE), 
                                    obese_8 = weighted.mean(obese_8, weights, na.rm = TRUE), 
                                    obese_9 = weighted.mean(obese_9, weights, na.rm = TRUE), 
                                    obese_10 = weighted.mean(obese_10, weights, na.rm = TRUE), 
                                    underwt_1 = weighted.mean(underwt_1, weights, na.rm = TRUE),
                                    underwt_2 = weighted.mean(underwt_2, weights, na.rm = TRUE),
                                    underwt_3 = weighted.mean(underwt_3, weights, na.rm = TRUE),
                                    underwt_4 = weighted.mean(underwt_4, weights, na.rm = TRUE),
                                    underwt_5 = weighted.mean(underwt_5, weights, na.rm = TRUE),
                                    underwt_6 = weighted.mean(underwt_6, weights, na.rm = TRUE),
                                    underwt_7 = weighted.mean(underwt_7, weights, na.rm = TRUE),
                                    underwt_8 = weighted.mean(underwt_8, weights, na.rm = TRUE),
                                    underwt_9 = weighted.mean(underwt_9, weights, na.rm = TRUE),
                                    underwt_10 = weighted.mean(underwt_10, weights, na.rm = TRUE),
                                    overwt_1 = weighted.mean(overwt_1, weights, na.rm = TRUE),
                                    overwt_2 = weighted.mean(overwt_2, weights, na.rm = TRUE),
                                    overwt_3 = weighted.mean(overwt_3, weights, na.rm = TRUE),
                                    overwt_4 = weighted.mean(overwt_4, weights, na.rm = TRUE),
                                    overwt_5 = weighted.mean(overwt_5, weights, na.rm = TRUE),
                                    overwt_6 = weighted.mean(overwt_6, weights, na.rm = TRUE),
                                    overwt_7 = weighted.mean(overwt_7, weights, na.rm = TRUE),
                                    overwt_8 = weighted.mean(overwt_8, weights, na.rm = TRUE),
                                    overwt_9 = weighted.mean(overwt_9, weights, na.rm = TRUE),
                                    overwt_10 = weighted.mean(overwt_10, weights, na.rm = TRUE),
                                    pred_bmi_1 = weighted.mean(pred_bmi_1, weights, na.rm = TRUE),
                                    pred_bmi_2 = weighted.mean(pred_bmi_2, weights, na.rm = TRUE),
                                    pred_bmi_3 = weighted.mean(pred_bmi_3, weights, na.rm = TRUE),
                                    pred_bmi_4 = weighted.mean(pred_bmi_4, weights, na.rm = TRUE),
                                    pred_bmi_5 = weighted.mean(pred_bmi_5, weights, na.rm = TRUE),
                                    pred_bmi_6 = weighted.mean(pred_bmi_6, weights, na.rm = TRUE),
                                    pred_bmi_7 = weighted.mean(pred_bmi_7, weights, na.rm = TRUE),
                                    pred_bmi_8 = weighted.mean(pred_bmi_8, weights, na.rm = TRUE),
                                    pred_bmi_9 = weighted.mean(pred_bmi_9, weights, na.rm = TRUE),
                                    pred_bmi_10 = weighted.mean(pred_bmi_10, weights, na.rm = TRUE),
                                    weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), 
                             by = list(area, level, age_bin, sex, race, year, marital_ushd)]
direct_estimates <- rbindlist(list(direct_estimates, edu_agg), use.names = TRUE, fill = TRUE)

#### Aggregate across years
marital_agg <- direct_estimates[, list(marital_ushd = 9, obese_1 = weighted.mean(obese_1, weights, na.rm = TRUE), 
                                    obese_2 = weighted.mean(obese_2, weights, na.rm = TRUE), 
                                    obese_3 = weighted.mean(obese_3, weights, na.rm = TRUE), 
                                    obese_4 = weighted.mean(obese_4, weights, na.rm = TRUE), 
                                    obese_5 = weighted.mean(obese_5, weights, na.rm = TRUE), 
                                    obese_6 = weighted.mean(obese_6, weights, na.rm = TRUE), 
                                    obese_7 = weighted.mean(obese_7, weights, na.rm = TRUE), 
                                    obese_8 = weighted.mean(obese_8, weights, na.rm = TRUE), 
                                    obese_9 = weighted.mean(obese_9, weights, na.rm = TRUE), 
                                    obese_10 = weighted.mean(obese_10, weights, na.rm = TRUE), 
                                    underwt_1 = weighted.mean(underwt_1, weights, na.rm = TRUE),
                                    underwt_2 = weighted.mean(underwt_2, weights, na.rm = TRUE),
                                    underwt_3 = weighted.mean(underwt_3, weights, na.rm = TRUE),
                                    underwt_4 = weighted.mean(underwt_4, weights, na.rm = TRUE),
                                    underwt_5 = weighted.mean(underwt_5, weights, na.rm = TRUE),
                                    underwt_6 = weighted.mean(underwt_6, weights, na.rm = TRUE),
                                    underwt_7 = weighted.mean(underwt_7, weights, na.rm = TRUE),
                                    underwt_8 = weighted.mean(underwt_8, weights, na.rm = TRUE),
                                    underwt_9 = weighted.mean(underwt_9, weights, na.rm = TRUE),
                                    underwt_10 = weighted.mean(underwt_10, weights, na.rm = TRUE),
                                    overwt_1 = weighted.mean(overwt_1, weights, na.rm = TRUE),
                                    overwt_2 = weighted.mean(overwt_2, weights, na.rm = TRUE),
                                    overwt_3 = weighted.mean(overwt_3, weights, na.rm = TRUE),
                                    overwt_4 = weighted.mean(overwt_4, weights, na.rm = TRUE),
                                    overwt_5 = weighted.mean(overwt_5, weights, na.rm = TRUE),
                                    overwt_6 = weighted.mean(overwt_6, weights, na.rm = TRUE),
                                    overwt_7 = weighted.mean(overwt_7, weights, na.rm = TRUE),
                                    overwt_8 = weighted.mean(overwt_8, weights, na.rm = TRUE),
                                    overwt_9 = weighted.mean(overwt_9, weights, na.rm = TRUE),
                                    overwt_10 = weighted.mean(overwt_10, weights, na.rm = TRUE),
                                    pred_bmi_1 = weighted.mean(pred_bmi_1, weights, na.rm = TRUE),
                                    pred_bmi_2 = weighted.mean(pred_bmi_2, weights, na.rm = TRUE),
                                    pred_bmi_3 = weighted.mean(pred_bmi_3, weights, na.rm = TRUE),
                                    pred_bmi_4 = weighted.mean(pred_bmi_4, weights, na.rm = TRUE),
                                    pred_bmi_5 = weighted.mean(pred_bmi_5, weights, na.rm = TRUE),
                                    pred_bmi_6 = weighted.mean(pred_bmi_6, weights, na.rm = TRUE),
                                    pred_bmi_7 = weighted.mean(pred_bmi_7, weights, na.rm = TRUE),
                                    pred_bmi_8 = weighted.mean(pred_bmi_8, weights, na.rm = TRUE),
                                    pred_bmi_9 = weighted.mean(pred_bmi_9, weights, na.rm = TRUE),
                                    pred_bmi_10 = weighted.mean(pred_bmi_10, weights, na.rm = TRUE),
                                    weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE)), 
                             by = list(area, level, age_bin, sex, race, edu_code, year)]
direct_estimates <- rbindlist(list(direct_estimates, marital_agg), use.names = TRUE, fill = TRUE)
#### Rename age column
setnames(direct_estimates, "age_bin", "age")

#### Save direct estimates
saveRDS(direct_estimates, file = paste0(output_dir, "/gallup_direct_estimates.rds"))
saveRDS(state_estimates, file = paste0(output_dir, "/gallup_state_level.rds"))

# Upload to database ------------------------------------------------------

survey_direct_estimate_collapse_microdata_run_id <-
  save_survey_direct_estimate_collapse_microdata_run(
    survey_source_id = 2, # 2 = Gallup
    survey_crosswalk_version_id = cw_meta$survey_crosswalk_version_id,
    mcnty_mapping_crosswalk_id = cbsa_mcnty_version_id, # id of cbsa_mcnty CW
    direct_estimate_file_path = paste0(output_dir, "/gallup_direct_estimates.rds"),
    collapse_microdata_file_path = paste0(output_dir, "/gallup_agg_overweight.rds"),
    description = "Uses newest CW (INLA-based BMI crosswalk)",
    is_best = TRUE, # setting default to TRUE b/c typically only run this when there are new input versions.
    prev_issues = "Based on older CW"
  )
# use update_survey_direct_estimate_collapse_microdata_run to change "best" status if needed

sink(paste0(output_dir, "/input_versions.txt"))
print(paste('Direct estimates ID', survey_direct_estimate_collapse_microdata_run_id))
print(paste('Gallup input directory (BMI crosswalk):', input_dir))
print(paste('Gallup data version (BMI crosswalk version):', data_version))
print(paste('race/edu code:', race_code_set))
print(paste0('age std version:', age_std_file))
sink()

print("Done!")
