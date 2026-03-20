###########################################################################################################################################
## Description: Create county-year-age-sex-race/ethnicity aggregates of self-reported risk factors data from BRFSS
##
## Input:       None
## Output:      Combined microdata files for BRFSS, with requisite demographic and general health data.
##
###########################################################################################################################################

######## 1. Setup
#### Load required packages installed in Singularity image
rm(list=ls())
source(paste0("FILEPATH"))  # load USHD database
pacman::p_load(data.table, haven, ggplot2, stringr, survey, car, R.utils,doParallel, RColorBrewer, Rcpp)

# Race        OLD    NEW
# Hispanic    7       2
# NH Black    2       4
# NH White    1       5
# NH AIAN     3       6
# NH API      4       7
# all_race    9       1
race_code_set <- 'db' #'db
stopifnot(race_code_set %in% c("old", "db"))

#### Set paths
cw_meta <- ushd.dbr::get_crosswalk_version(rei_id = 370, get_best = T)
data_version <- basename(cw_meta$output_file_path)
input_dir <- cw_meta$output_file_path

cbsa_mcnty_meta <- get_mcnty_mapping_crosswalk(mcnty_mapping_type_id = 1, get_best = T)
cbsa_mcnty_meta <- cbsa_mcnty_meta[mcnty_mapping_crosswalk_id  == max(mcnty_mapping_crosswalk_id )]
cbsa_mcnty_version_id <- cbsa_mcnty_meta$mcnty_mapping_crosswalk_id
cbsa_mcnty_version <- basename(dirname(cbsa_mcnty_meta$mcnty_mapping_filepath))

ps_frame_meta <- get_post_stratification_frame(post_stratification_frame_id = cbsa_mcnty_meta$post_stratification_frame_id)
# Turn that string into a vector of numbers
ps_covariates <- as.integer(unlist(strsplit(ps_frame_meta$covariate_dataset_id, ", ")))
all_pop_versions <- get_covariate_version("pop_by_race_ethn_1977")
# figure out which of the ps_covariates is the population version
pop_version_id <- ps_covariates[ps_covariates %in% all_pop_versions$covariate_dataset_id]
stopifnot(length(pop_version_id) == 1)

if(race_code_set == "old"){
  stop("Script no longer supports old race codes")
  pop_file <- "FILEPATH"  
} else{
  pop_file <- paste0("FILEPATH", all_pop_versions[covariate_dataset_id == pop_version_id, dataset_filename])
  stopifnot(file.exists(pop_file))
}

age_std_file <- 'FILEPATH'
ages <- c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)
std_wt <- readRDS(age_std_file)[age %in% ages, list(age, wt = wt / sum(wt))]
if(sum(std_wt$wt)!=1){
 stop(message('age standardization weights do not sum to one!'))  
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
data_combined <- readRDS(file = paste0(input_dir, "pred.rds"))
colnames(data_combined)
table(data_combined$race77, exclude=NULL)
table(data_combined[year>1999]$race97, exclude=NULL)

#### Limit to 2000-2019
data_combined <- data_combined[year %in% 2000:2019 & survey=='brfss']

# drop unused columns
drop_cols <- c(grep("percentile|quantile|matched_val|quant|outlier_t10|logit_diff", names(data_combined), value = T), 'race', 'race2')
data_combined[, (drop_cols) := NULL ]

#### Process race/ethnicity
setnames(data_combined, "race77", "race")

# For now, drop NH Other and NH MR w/o main individuals
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
  education_map <- data.table("edu_code" = 1:4, "edu" = c("less than HS", "HS grad", "some college", "college grad"))
  edu_all <- 9 # note that this is not a valid education code in the database
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

#### Process output
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
data_combined[, phone_ushd := factor(phone, levels = c("cell_only", "landline_only", "dual_phone"), 1:3)]

######## 2.5 Add CBSA-mcnty information
#### Load CBSA-mcnty crosswalk
cbsa_mcnty <- readRDS(paste0("FILEPATH"))
cbsa_mcnty_full <- readRDS(paste0("FILEPATH"))
setnames(cbsa_mcnty, "age", "age_bin")
cbsa_mcnty$age_bin <- as.integer(as.character(cbsa_mcnty$age_bin))

# map old race to new race codes.
# Race        OLD    NEW
# Hispanic    7       2
# NH Black    2       4
# NH White    1       5
# NH AIAN     3       6
# NH API      4       7
# all_race    9       1
cbsa_mcnty <- cbsa_mcnty[age_bin>19]

#remap race if old
if(setequal(sort(unique(cbsa_mcnty$race)),  c(1,2,3,4,7)) & race_code_set == "db"){
  race_mapper <- data.table(old = c(7,2,1,3,4), new = c(2,4,5,6,7))
  cbsa_mcnty$race <- plyr::mapvalues(cbsa_mcnty$race, from = race_mapper$old, to = race_mapper$new)
}

if(race_code_set == "old"){
  stopifnot(setequal(sort(unique(cbsa_mcnty$race)), c(1,2,3,4,7))) # make sure we're now using old codes
} else{
  stopifnot(setequal(sort(unique(cbsa_mcnty$race)), c(2,4,5,6,7))) # make sure we're now using new race codes
}


## Note that there are some additional checks in nonfatal/data_prep/counties/brfss/01_aggregate_brfss.R
#### Check for mismatches between state in data_combined and CBSA cw
## Merge state from cbsa_mcnty onto data_combined
data_combined$cbsa <- as.integer(data_combined$cbsa)
setkeyv(data_combined, c("state", "cbsa"))
data_combined$uid <- 1:nrow(data_combined)
state_check <- data_combined[unique(cbsa_mcnty_full[cbsa >= 0, list(state, cbsa)])]
missing <- data_combined[!(uid %in% state_check$uid) & !is.na(cbsa)]
# e.g., there is one row with state == 35 (New Mexico) but CBSA == 15540 (which is in Vermont)
## Drop CBSA identifiers for these rows
data_combined[uid %in% missing$uid, cbsa := NA]
data_combined[, uid := NULL]

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
state_only <- merge(data_combined[is.na(cbsa) & is.na(mcnty), list(no_cbsa_no_mcnty = .N), by = c("state", "year")], 
                    data_combined[!is.na(cbsa) | !is.na(mcnty), list(cbsa_or_mcnty = .N), by = c("state", "year")], 
                    all = TRUE)
state_only <- state_only[!is.na(no_cbsa_no_mcnty) & is.na(cbsa_or_mcnty)]

#### Subset data according to geographic level
data_combined_original <- copy(data_combined)
data_state_only <- data_combined[state_only, on = c("state", "year")]
data_cbsa_only <- data_combined[!(uid %in% data_state_only$uid)]
data_cbsa_only_original <- copy(data_cbsa_only)
stopifnot(nrow(data_state_only) + nrow(data_cbsa_only) == nrow(data_combined))

#### Merge CBSA-mcnty crosswalk onto mcnty and CBSA data subsets
## Process metro and non-metro data separately
data_cbsa_only_metro_mcnty <- merge(data_cbsa_only[!is.na(mcnty)], 
                                    unique(cbsa_mcnty_full[is_metropolitan_division == 1, c("mcnty", "cbsa_mcnty_code", "state", "year", "version")]), 
                                    by = c("mcnty", "state", "year", "version"))
data_cbsa_only_non_metro_mcnty <- merge(data_cbsa_only[!(uid %in% data_cbsa_only_metro_mcnty$uid)], 
                                        unique(cbsa_mcnty_full[is_metropolitan_division == 0, c("mcnty", "cbsa_mcnty_code", "state", "year", "version")]), 
                                        by = c("mcnty", "state", "year", "version"))
remaining <- data_cbsa_only[!(uid %in% c(data_cbsa_only_metro_mcnty$uid, data_cbsa_only_non_metro_mcnty$uid))]
data_cbsa_only_metro_cbsa <- merge(remaining[!is.na(cbsa)], 
                                   unique(cbsa_mcnty_full[is_metropolitan_division == 1, c("cbsa", "cbsa_mcnty_code", "state", "year", "version")]), 
                                   by = c("cbsa", "state", "year", "version"))
data_cbsa_only_non_metro_cbsa <- merge(remaining[!(uid %in% data_cbsa_only_metro_cbsa$uid)], 
                                       unique(cbsa_mcnty_full[is_metropolitan_division == 0, c("cbsa", "cbsa_mcnty_code", "state", "year", "version")]), 
                                       by = c("cbsa", "state", "year", "version"))
remaining <- data_cbsa_only[!(uid %in% c(data_cbsa_only_metro_mcnty$uid, data_cbsa_only_non_metro_mcnty$uid, data_cbsa_only_metro_cbsa$uid, data_cbsa_only_non_metro_cbsa$uid))]

#### Establish fake codes for state remainder "CBSAs"; these are represented as {STATEFIP}{999}
## Set fake CBSA codes (only for relevant years)
# nrow(remaining[!is.na(cbsa)]) # 0
remaining[, cbsa := as.integer(paste0(sprintf("%02d", state), "999"))]
remaining_metro_cbsa <- merge(remaining[!is.na(cbsa)], unique(cbsa_mcnty_full[is_metropolitan_division == 1, c("cbsa", "cbsa_mcnty_code", "state", "year", "version")]), by = c("cbsa", "state", "year", "version"))
remaining_non_metro_cbsa <- merge(remaining[!(uid %in% remaining_metro_cbsa$uid)], unique(cbsa_mcnty_full[is_metropolitan_division == 0, c("cbsa", "cbsa_mcnty_code", "state", "year", "version")]), by = c("cbsa", "state", "year", "version"))
remaining <- remaining[!(uid %in% c(remaining_metro_cbsa$uid, remaining_non_metro_cbsa$uid))]
unmatched_n <- nrow(remaining)

## Recombine data sets
# For now we retain rows with unknown cbsa_mcnty codes
data_cbsa_only <- rbindlist(list(data_cbsa_only_metro_mcnty, 
                                 data_cbsa_only_non_metro_mcnty, 
                                 data_cbsa_only_metro_cbsa, 
                                 data_cbsa_only_non_metro_cbsa, 
                                 remaining_metro_cbsa, 
                                 remaining_non_metro_cbsa, 
                                 remaining), 
                            use.names = TRUE, fill = TRUE)
stopifnot(nrow(data_cbsa_only_original) == nrow(data_cbsa_only))

#### Summarize CBSA-mcnty coverage
cbsa_mcnty_coverage <- data_cbsa_only[is.na(mcnty), list(.N), by = c("cbsa", "cbsa_mcnty_code", "state", "year", "version")]

#### Merge onto cbsa_mcnty
cbsa_mcnty_coverage_merged <- merge(cbsa_mcnty_coverage, 
                                    unique(cbsa_mcnty_full[, c("cbsa", "cbsa_mcnty_code", "state", "year", "version")]), 
                                    by = c("cbsa", "cbsa_mcnty_code", "state", "year", "version"), 
                                    all = TRUE)

#### Set indicator for rows with zero observations
cbsa_mcnty_coverage_merged[is.na(N), missingness_indicator := 1]

#### Merge missingness indicators onto cbsa_mcnty rows with non-zero observations
coverage <- merge(cbsa_mcnty_coverage_merged[, -c("missingness_indicator")], 
                  unique(cbsa_mcnty_coverage_merged[is.na(N), c("cbsa_mcnty_code", "state", "year", "version", "missingness_indicator")]), by = c("cbsa_mcnty_code", "state", "year", "version"), 
                  all = TRUE)
#   catch problematic_cbsa_mcntys if there are some
problematic_cbsa_mcntys <- coverage[!is.na(N) & !is.na(cbsa) & missingness_indicator == 1]

#### Set data_combined
data_combined <- rbindlist(list(data_cbsa_only, data_state_only), use.names = TRUE, fill = TRUE)
stopifnot(nrow(data_combined_original) == nrow(data_combined))

######## 3. Prepare data for general health analysis
#### Check missingness
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

missing_by_year <- check_missingness(data_combined, by_vars = "year")
# Phone is completely missing until 2009

########## Process data for mean BMI
## Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_mean <- data_combined[(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin)  | is.na(edu_code) | is.na(marital_ushd)]
data_combined_mean <- data_combined[!(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_mean) + nrow(data_combined_mean) != nrow(data_combined)) {
  stop(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_mean) + nrow(data_combined_mean), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

#### Collapse mean BMI by county-year-age-sex and either race/ethnicity or education

collapse_bmi <- function(DT, 
                         cols, 
                         weighted_cols_name,
                         by_col,
                         is_prev){
  if(is_prev){
    # agg of cases
    temp1 <- DT[, lapply(.SD, sum, na.rm =T), 
                .SDcols = cols,
                by = by_col]
    temp2 <- DT[, lapply(.SD, function(c) weighted.mean(c, w = weights, na.rm = T)*.N),
                .SDcols = cols,
                by = by_col]
  } else{
    # agg of mean BMI
    temp1 <- DT[, lapply(.SD, mean, na.rm =T), 
                .SDcols = cols,
                by = by_col]
    temp2 <- DT[, lapply(.SD, function(c) weighted.mean(c, w = weights, na.rm = T)),  # removed *N
                .SDcols = cols,
                by = by_col]
  }
  setnames(temp2, cols, weighted_cols_name)
  temp3 <- DT[, .(weights = sum(weights, na.rm = T), sample_size = .N),
              by = by_col]
  invisible(lapply(list(temp1, temp2, temp3), setkeyv, cols = by_col))
  
  temp <- temp1[temp2][temp3]
  return(temp)
}

data_combined_agg_mean <- collapse_bmi(data_combined_mean,
                                       cols = paste0("pred_bmi_", c("report", 1:10)),
                                       weighted_cols_name = paste0("pred_bmi_weighted_", c("report", 1:10)),
                                       by_col = c("mcnty", "cbsa_mcnty_code", "state", "year", "sex", "race", "age_bin", "edu_code", "marital_ushd"),
                                       is_prev = F)

#### Save collapsed data sets
saveRDS(data_missing_mean, file = paste0(output_dir, "/brfss_agg_missing_data_pred_bmi.rds"))
saveRDS(data_combined_agg_mean, file = paste0(output_dir, "/brfss_agg_pred_bmi.rds"))

########## Process data for overweight
## Drop observations without mcnty, year, sex, age, race and/or education; save these to separate files for follow-up
data_missing_overwt <- data_combined[(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin)  | is.na(edu_code) | is.na(marital_ushd)]#excluded !is.na(overwt since not relevant for crosswaked)
data_combined_overwt <- data_combined[!(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(edu_code) & !is.na(marital_ushd)]
if (nrow(data_missing_overwt) + nrow(data_combined_overwt) != nrow(data_combined)) {
  stop(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_overwt) + nrow(data_combined_overwt), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

#### Collapse overweight by county-year-age-sex and either race/ethnicity or education
data_combined_agg_overwt <- collapse_bmi(data_combined_overwt,
                                         cols = paste0("overwt_", c("report", 1:10)),
                                         weighted_cols_name = paste0("overwt_weighted_count_", c("report", 1:10)),
                                         by_col = c("mcnty", "cbsa_mcnty_code", "state", "year", "sex", "race", "age_bin", "edu_code", "marital_ushd"),
                                         is_prev = T)

#### Save collapsed data sets
saveRDS(data_missing_overwt, file = paste0(output_dir, "/brfss_agg_missing_data_overweight.rds"))
saveRDS(data_combined_agg_overwt, file = paste0(output_dir, "/brfss_agg_overweight.rds"))

########## Process data for underweight
data_missing_underwt <- data_combined[(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(edu_code) | is.na(marital_ushd)]
data_combined_underwt <- data_combined[!(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(edu_code) & !is.na(marital_ushd)] #& !is.na(phone_ushd)]
if (nrow(data_missing_underwt) + nrow(data_combined_underwt) != nrow(data_combined)) {
  stop(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_underwt) + nrow(data_combined_underwt), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
}

######## Collapse underweight by county-year-age-sex and either race/ethnicity or education
data_combined_agg_underwt <- collapse_bmi(data_combined_underwt,
                                          cols = paste0("underwt_", c("report", 1:10)),
                                          weighted_cols_name = paste0("underwt_weighted_count_", c("report", 1:10)),
                                          by_col = c("mcnty", "cbsa_mcnty_code", "state", "year", "sex", "race", "age_bin", "edu_code", "marital_ushd"),
                                          is_prev = T)

#### Save collapsed data sets
saveRDS(data_missing_underwt, file = paste0(output_dir, "/brfss_agg_missing_data_underweight.rds"))
saveRDS(data_combined_agg_underwt, file = paste0(output_dir, "/brfss_agg_underweight.rds"))

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

  data_missing_obese <- data_combined[get(var)==1 & ((is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(edu_code) | is.na(marital_ushd))]
  data_combined_obese <- data_combined[get(var)==1 & !(is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) & !is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age_bin) & !is.na(edu_code) & !is.na(marital_ushd)]
  if (nrow(data_missing_obese) + nrow(data_combined_obese) != nrow(data_combined[get(var)==1])) {
    stop(paste0("The total number of rows in the missingness and complete-case data sets for race/ethnicity (", nrow(data_missing_obese) + nrow(data_combined_obese), ") does not equal the number of rows in the original data set (", nrow(data_combined), "). Please check this." ))
  }
  obese <- obese_draws[n]
  
  data_combined_agg_obese <- collapse_bmi(data_combined_obese,
                                          cols = obese,
                                          weighted_cols_name = gsub("obese_", "obese_weighted_count_", obese),
                                          by_col = c("mcnty", "cbsa_mcnty_code", "state", "year", "sex", "race", "age_bin", "edu_code", "marital_ushd"),
                                          is_prev = T)
  setnames(data_combined_agg_obese, c("sample_size", "weights"), c(paste0("sample_size_", suffix), paste0("weights_", suffix)))
  
  assign(paste0('data_missing_obese_', n), data_missing_obese)
  assign(paste0('data_combined_obese_', n), data_combined_obese)
  assign(paste0('data_combined_agg_obese_', n), data_combined_agg_obese)
}

data_combined_agg_obese_n <- paste("data_combined_agg_obese", 1:length(bmi_draws), sep = "_")
data_combined_agg_obese <- lapply(data_combined_agg_obese_n, function(x) get(x))

data_missing_obese_n <- paste("data_missing_obese", 1:length(bmi_draws), sep = "_")
data_missing_obese <- lapply(data_missing_obese_n, function(x) get(x))

#### Save collapsed data sets
saveRDS(data_missing_obese, file = paste0(output_dir, "/brfss_agg_missing_data_obese.rds"))
saveRDS(data_combined_agg_obese, file = paste0(output_dir, "/brfss_agg_obese.rds"))

######## 17. Produce direct estimates by race/ethnicity
#### Drop rows with missingness in demographic variables
data_combined_backup <- copy(data_combined)
data_combined_no_strat <- data_combined[!((is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(edu_code) | is.na(marital_ushd))]
data_combined <- data_combined[!((is.na(mcnty) & is.na(cbsa) & is.na(cbsa_mcnty_code) & is.na(state)) | is.na(year) | is.na(sex) | is.na(race) | is.na(age_bin) | is.na(edu_code) | is.na(marital_ushd))]

### Without phone
data_combined[,sex := as.integer(sex)]

##############################
measure_vars <- c(paste0("obese_", c("report", 1:10)), paste0("underwt_", c("report", 1:10)), paste0("overwt_", c("report", 1:10)), paste0("pred_bmi_", c("report", 1:10)))
# check that all measure_vars are in the data
stopifnot(all(measure_vars %in% names(data_combined)))
mcnty_estimates <- data_combined[, c(list(level = "mcnty",
                                          weights = sum(weights, na.rm = T),
                                          sample_size = .N),
                                     lapply(.SD, weighted.mean, w = weights, na.rm = T)),
                                 .SDcols = measure_vars,
                                 by = list(mcnty, state, state_name, year, sex, race, age_bin, edu_code, marital_ushd)]

mcnty_estimates_no_strat <- data_combined[, c(list(level = "mcnty",
                                                   weights = sum(weights, na.rm = T),
                                                   sample_size = .N),
                                              lapply(.SD, weighted.mean, w = weights, na.rm = T)),
                                          .SDcols = measure_vars,
                                          by = list(mcnty, state, state_name, year, sex, race, age_bin)]

#### Produce CBSA-mcnty-level direct estimates

cbsa_mcnty_estimates <- data_combined[, c(list(level = "cbsa_mcnty",
                                               weights = sum(weights, na.rm = T),
                                               sample_size = .N),
                                          lapply(.SD, weighted.mean, w = weights, na.rm = T)),
                                      .SDcols = measure_vars,
                                      by = list(cbsa_mcnty_code, state, state_name, year, sex, race, age_bin, edu_code, marital_ushd)]

cbsa_mcnty_estimates_no_strat <- data_combined[, c(list(level = "cbsa_mcnty",
                                                        weights = sum(weights, na.rm = T),
                                                        sample_size = .N),
                                                   lapply(.SD, weighted.mean, w = weights, na.rm = T)),
                                               .SDcols = measure_vars,
                                               by = list(cbsa_mcnty_code, state, state_name, year, sex, race, age_bin)]

#### Produce state-level direct estimates
state_estimates <- data_combined[, c(list(level = "state",
                                          weights = sum(weights, na.rm = T),
                                          sample_size = .N),
                                     lapply(.SD, weighted.mean, w = weights, na.rm = T)),
                                 .SDcols = measure_vars,
                                 by = list(state, state_name, year, sex, race, age_bin, edu_code, marital_ushd)]

state_estimates_no_strat <- data_combined[, c(list(level = "state",
                                                   weights = sum(weights, na.rm = T),
                                                   sample_size = .N),
                                              lapply(.SD, weighted.mean, w = weights, na.rm = T)),
                                          .SDcols = measure_vars,
                                          by = list(state, state_name, year, sex, race, age_bin)]

#### Load and merge population file
pop <- readRDS(pop_file)
pop[,sex := as.integer(sex)]
pop_state <- pop[, list(pop = sum(pop)), by = c("year", "sex", "age", "race", "state")]
# check that race codes in pop match those in the ests
stopifnot(setequal(mcnty_estimates[!is.na(race), sort(unique(race))], pop[, sort(unique(race))]))

mcnty_estimates <- merge(mcnty_estimates, pop[, -c("state")], 
                         by.x = c("mcnty", "year", "sex", "race", "age_bin"), 
                         by.y = c("mcnty", "year", "sex", "race", "age"), all.x = TRUE)

mcnty_estimates_no_strat <- merge(mcnty_estimates_no_strat, pop[, -c("state")], 
                                  by.x = c("mcnty", "year", "sex", "race", "age_bin"), 
                                  by.y = c("mcnty", "year", "sex", "race", "age"), all.x = TRUE)

message("Merge cbsa_mcnty_estimates with pop...")
cbsa_mcnty_collapsed <- cbsa_mcnty[, list(total_pop = sum(pop)), by = c("year", "cbsa_mcnty_code", "sex", "age_bin", "race")]
cbsa_mcnty_estimates <- merge(cbsa_mcnty_estimates, cbsa_mcnty_collapsed, 
                              by = c("year", "cbsa_mcnty_code", "sex", "age_bin", "race"), all.x = TRUE)
setnames(cbsa_mcnty_estimates, "total_pop", "pop")

message("Merge cbsa_mcnty_estimates_no_strat with pop...")
cbsa_mcnty_estimates_no_strat <- merge(cbsa_mcnty_estimates_no_strat, cbsa_mcnty_collapsed, 
                                       by = c("year", "cbsa_mcnty_code", "sex", "age_bin", "race"), all.x = TRUE)
setnames(cbsa_mcnty_estimates_no_strat, "total_pop", "pop")

state_estimates <- merge(state_estimates, pop_state, 
                         by.x = c("state", "year", "sex", "race", "age_bin"), 
                         by.y = c("state", "year", "sex", "race", "age"), all.x = TRUE)

state_estimates_no_strat <- merge(state_estimates_no_strat, pop_state, 
                                  by.x = c("state", "year", "sex", "race", "age_bin"), 
                                  by.y = c("state", "year", "sex", "race", "age"), all.x = TRUE)

#### Derive BRFSS weights proportions to scale population estimates for stratified estimates
mcnty_estimates[, pop_scaled := pop * weights / sum(weights, na.rm = TRUE), by = c("mcnty", "year", "sex", "race", "age_bin")]
cbsa_mcnty_estimates[, pop_scaled := pop * weights / sum(weights, na.rm = TRUE), by = c("cbsa_mcnty_code", "year", "sex", "race", "age_bin")]
state_estimates[, pop_scaled := pop * weights / sum(weights, na.rm = TRUE), by = c("state", "year", "sex", "race", "age_bin")]
mcnty_estimates[, c("pop", "pop_scaled") := list(pop_scaled, NULL)]
cbsa_mcnty_estimates[, c("pop", "pop_scaled") := list(pop_scaled, NULL)]
state_estimates[, c("pop", "pop_scaled") := list(pop_scaled, NULL)]

#### Produce national-level direct estimates
# BRFSS weights are not nationally-representative, so we weight state-level direct estimates by population

national_estimates <- state_estimates[, c(list(level = "natl",
                                               weights = sum(weights, na.rm = T),
                                               sample_size = sum(sample_size, na.rm = T),
                                               pop = sum(pop, na.rm = T)),
                                          lapply(.SD, weighted.mean, w = pop, na.rm = T)),
                                      .SDcols = measure_vars,
                                      by = list(year, sex, race, age_bin, edu_code, marital_ushd)]

national_estimates_no_strat <- state_estimates[, c(list(level = "natl",
                                                        weights = sum(weights, na.rm = T),
                                                        sample_size = sum(sample_size, na.rm = T),
                                                        pop = sum(pop, na.rm = T)),
                                                   lapply(.SD, weighted.mean, w = pop, na.rm = T)),
                                               .SDcols = measure_vars,
                                               by = list(year, sex, race, age_bin)]

#### Combine direct estimates across levels
direct_estimates <- rbindlist(list(mcnty_estimates, cbsa_mcnty_estimates, state_estimates, national_estimates), use.names = TRUE, fill = TRUE)
direct_estimates_no_strat <- rbindlist(list(mcnty_estimates_no_strat, cbsa_mcnty_estimates_no_strat, state_estimates_no_strat, national_estimates_no_strat), use.names = TRUE, fill = TRUE)

saveRDS(state_estimates, file = paste0(output_dir, "/brfss_state_level.rds"))
saveRDS(state_estimates_no_strat, file = paste0(output_dir, "/brfss_state_level_no_strat.rds"))

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

# merge with age-standardized weights
setnames(std_wt, 'age', 'age_bin')
direct_estimates <- merge(direct_estimates, std_wt, by='age_bin')
direct_estimates_no_strat <- merge(direct_estimates_no_strat, std_wt, by='age_bin')

#### Aggregate across ages
# wt refers to age standardization weights (from standardized population distribution by age)
# weights refers to teh sample weights from BRFSS
# final_weights refers to encompasses (1) weights for aggregation by age/sex/year/race/edu/marital and mcnty/cbsa to state, 
# and (2) population weights for aggregation from state to national
agestd_agg <- direct_estimates[, c(list(age_bin = 99,
                                        wt = sum(wt, na.rm = T),
                                        weights = sum(weights, na.rm = T),
                                        sample_size = sum(sample_size, na.rm = T),
                                        pop = sum(pop, na.rm = T),
                                        final_weight = sum(final_weight, na.rm = TRUE)),
                                   lapply(.SD, weighted.mean, w = wt, na.rm = T)),
                               .SDcols = measure_vars,
                               by = list(area, level, year, sex, race, edu_code, marital_ushd)]

direct_estimates <- rbindlist(list(direct_estimates, agestd_agg), use.names = TRUE, fill = TRUE)

agestd_agg_no_strat <- direct_estimates_no_strat[, c(list(age_bin = 99,
                                                          wt = sum(wt, na.rm = T),
                                                          weights = sum(weights, na.rm = T),
                                                          sample_size = sum(sample_size, na.rm = T),
                                                          pop = sum(pop, na.rm = T),
                                                          final_weight = sum(final_weight, na.rm = TRUE)),
                                                     lapply(.SD, weighted.mean, w = wt, na.rm = T)),
                                                 .SDcols = measure_vars,
                                                 by = list(area, level, year, sex, race)]

direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, agestd_agg_no_strat), use.names = TRUE, fill = TRUE)

#### Aggregate across ages
age_agg <- direct_estimates[, c(list(age_bin = 98,
                                     weights = sum(weights, na.rm = T),
                                     sample_size = sum(sample_size, na.rm = T),
                                     pop = sum(pop, na.rm = T),
                                     final_weight = sum(final_weight, na.rm = TRUE)),
                                lapply(.SD, weighted.mean, w = final_weight, na.rm = T)),
                            .SDcols = measure_vars,
                            by = list(area, level, year, sex, race, edu_code, marital_ushd)]

direct_estimates <- rbindlist(list(direct_estimates, age_agg), use.names = TRUE, fill = TRUE)

age_agg_no_strat <- direct_estimates_no_strat[, c(list(age_bin = 98,
                                                       weights = sum(weights, na.rm = T),
                                                       sample_size = sum(sample_size, na.rm = T),
                                                       pop = sum(pop, na.rm = T),
                                                       final_weight = sum(final_weight, na.rm = TRUE)),
                                                  lapply(.SD, weighted.mean, w = final_weight, na.rm = T)),
                                              .SDcols = measure_vars,
                                              by = list(area, level, year, sex, race)]

direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, age_agg_no_strat), use.names = TRUE, fill = TRUE)

#### Aggregate across sexes

sex_agg <- direct_estimates[, c(list(sex = 3,
                                     weights = sum(weights, na.rm = T),
                                     sample_size = sum(sample_size, na.rm = T),
                                     pop = sum(pop, na.rm = T),
                                     final_weight = sum(final_weight, na.rm = TRUE)),
                                lapply(.SD, weighted.mean, w = final_weight, na.rm = T)),
                            .SDcols = measure_vars,
                            by = list(area, level, year, race, age_bin, edu_code, marital_ushd)]

direct_estimates <- rbindlist(list(direct_estimates, sex_agg), use.names = TRUE, fill = TRUE)

sex_agg_no_strat <- direct_estimates_no_strat[, c(list(sex = 3,
                                                       weights = sum(weights, na.rm = T),
                                                       sample_size = sum(sample_size, na.rm = T),
                                                       pop = sum(pop, na.rm = T),
                                                       final_weight = sum(final_weight, na.rm = TRUE)),
                                                  lapply(.SD, weighted.mean, w = final_weight, na.rm = T)),
                                              .SDcols = measure_vars,
                                              by = list(area, level, year, race, age_bin)]

direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, sex_agg_no_strat), use.names = TRUE, fill = TRUE)


#### Aggregate across races
race_agg <- direct_estimates[, c(list(race = race_all, 
                                      weights = sum(weights, na.rm = T),
                                      sample_size = sum(sample_size, na.rm = T),
                                      pop = sum(pop, na.rm = T),
                                      final_weight = sum(final_weight, na.rm = TRUE)),
                                 lapply(.SD, weighted.mean, w = final_weight, na.rm = T)),
                             .SDcols = measure_vars,
                             by = list(area, level, year, sex, age_bin, edu_code, marital_ushd)]

direct_estimates <- rbindlist(list(direct_estimates, race_agg), use.names = TRUE, fill = TRUE)

race_agg_no_strat <- direct_estimates_no_strat[, c(list(race = race_all,
                                                        weights = sum(weights, na.rm = T),
                                                        sample_size = sum(sample_size, na.rm = T),
                                                        pop = sum(pop, na.rm = T),
                                                        final_weight = sum(final_weight, na.rm = TRUE)),
                                                   lapply(.SD, weighted.mean, w = final_weight, na.rm = T)),
                                               .SDcols = measure_vars,
                                               by = list(area, level, year, sex, age_bin)]

direct_estimates_no_strat <- rbindlist(list(direct_estimates_no_strat, race_agg_no_strat), use.names = TRUE, fill = TRUE)

setnames(direct_estimates_no_strat, "age_bin", "age")
saveRDS(direct_estimates_no_strat, file = paste0(output_dir, "/brfss_direct_estimates_no_strat.rds"))

#### Aggregate across edu
message("Aggregate across edu")
edu_agg <- direct_estimates[, c(list(edu_code = edu_all,
                                     weights = sum(weights, na.rm = T),
                                     sample_size = sum(sample_size, na.rm = T),
                                     pop = sum(pop, na.rm = T),
                                     final_weight = sum(final_weight, na.rm = TRUE)),
                                lapply(.SD, weighted.mean, w = final_weight, na.rm = T)),
                            .SDcols = measure_vars,
                            by = list(area, level, age_bin, sex, race, year, marital_ushd)]

direct_estimates <- rbindlist(list(direct_estimates, edu_agg), use.names = TRUE, fill = TRUE)

#### Aggregate across marital status
message("Aggregate across marital status")
marital_agg <- direct_estimates[, c(list(marital_ushd = 9, 
                                         weights = sum(weights, na.rm = T),
                                         sample_size = sum(sample_size, na.rm = T),
                                         pop = sum(pop, na.rm = T),
                                         final_weight = sum(final_weight, na.rm = TRUE)),
                                    lapply(.SD, weighted.mean, w = final_weight, na.rm = T)),
                                .SDcols = measure_vars,
                                by = list(area, level, age_bin, sex, race, year, edu_code)]

direct_estimates <- rbindlist(list(direct_estimates, marital_agg), use.names = TRUE, fill = TRUE)

#### Rename age column
setnames(direct_estimates, "age_bin", "age")

#### Save direct estimates
saveRDS(direct_estimates, file = paste0(output_dir, "/brfss_direct_estimates.rds"))


# Upload to database ------------------------------------------------------

survey_direct_estimate_collapse_microdata_run_id <-
  save_survey_direct_estimate_collapse_microdata_run(
    survey_source_id = 1, # 1 = BRFSS
    survey_crosswalk_version_id = cw_meta$survey_crosswalk_version_id,
    mcnty_mapping_crosswalk_id = cbsa_mcnty_version_id, # id of cbsa_mcnty CW
    direct_estimate_file_path = paste0(output_dir, "/brfss_direct_estimates.rds"),
    collapse_microdata_file_path = paste0(output_dir, "/brfss_agg_overweight.rds"),
    description = sprintf("New direct estimates version -- based on updated INLA-based BMI crosswalk. Uses cbsa_mcnty CW version %s (cbsa_mcnty_version_id %s) and pop version %s (covariate dataset ID %s)", cbsa_mcnty_version, cbsa_mcnty_version_id, pop_file, pop_version_id),
    is_best = TRUE, # setting default to TRUE b/c typically only run this when there are new input versions.
    prev_issues = "Based on older crosswalk" # REMEMBER TO UPDATE ME
)
# use update_survey_direct_estimate_collapse_microdata_run to change "best" status if needed


## Track data/crosswalk versions
sink(paste0(output_dir, "/input_versions.txt"))
print(paste('Direct estimates ID', survey_direct_estimate_collapse_microdata_run_id))
print(paste('BRFSS input directory (BMI CW version):', input_dir))
print(paste('BRFSS data version (BMI CW version):', data_version))
print(paste('CBSA-mcnty crosswalk version:', cbsa_mcnty_version))
print(paste('race/edu code:', race_code_set))
print(paste0('age std version:', age_std_file))
sink()

print("Done!")