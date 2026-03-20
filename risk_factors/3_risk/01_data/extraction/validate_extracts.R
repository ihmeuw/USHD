####################################################################################################
## Description: Performs validation of extractions used on GBD. Additionally, checks
##                USHD-specific aspects:
##              
##              * Makes a sequence column
##              * Check  GBD_keep and USHD_keep 
##              * rows marked as "effectively single race:" the majority race should
##                  make up at least 90% of the cohort
##              * turn the % race columns to decimals
##              * create warnings if the issue flag == 1 (not implemented)
##              * ushd_keep should equal 1 if we have excluded the NID
##            
##            This script validates and uploads the risk bundle to the ELMO database
##            This should be as close to the original extractions as possible. 
##            Calculated variables should be made later so that we preserve a record
##            of the extractions!
##                
## Input: Cleaned extraction sheet 
##          FILEPATH
##          
## Output: Validated extraction sheets
##             FILEPATH
##             FILEPATH
## 
####################################################################################################

## Load dependencies
if(!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, assertable, magrittr, readxl, openxlsx, stringr, ggplot2)
source("FILEPATH")

bundle_id <- 9963

## LOAD RELEVANT DATA
data_dir = "FILEPATH"
file <- "FILEPATH"
message(sprintf("Validating %s", basename(file)))
df <- read_xlsx(file) %>% as.data.table
df <- df[-1]
warning("Currently skipping the first row (which is description of column")

df <- df[in_bundle_current_40010 == 1] # we don't want to upload data not in GBD's current bundle

# Validate that no column names are duplicated

# Validate that necessary columns are present and properly named
req_cols <- c('bundle_id',
              'seq',
              'nid',
              'field_citation_value',
              'risk',
              'exposure_category',
              'outcome',
              'outcome_type',
              'location_id',
              'location_name',
              'age_start',
              'age_end',
              'sex',
              'year_start_study',
              'year_end_study',
              'outcome_type',
              'effect_size_measure',
              'effect_size',
              'lower',
              'upper',
              'cohort_exposed_low_limit',
              'cohort_exposed_upper_limit',
              'cohort_unexposed_low_limit',
              'cohort_unexposed_upper_limit',
              'washout_years',
              'representative_score',
              'exposure_idv_score',
              'exposure_obj_score',
              'exposure_mult_score',
              'outcomes_record_score',
              'outcomes_blind_score',
              'control_rct_other_score',
              'control_for_confounders_score',
              'reverse_causality_score',
              'gbd_duplicate',
              'gbd_keep',
              'ushd_keep',
              'include_nid',
              'how_race_incorported',
              'race_1', 
              'race_2', 
              'race_3', 
              'race_4', 
              'race_5', 
              'race_6', 
              'percent_race_1', 
              'percent_race_2', 
              'percent_race_3', 
              'percent_race_4', 
              'percent_race_5', 
              'percent_race_6', 
              'race_memo', 
              'race_issue', 
              'race_note',
              'subgroup_analysis',
              'USHD_determination',
              'USHD_exclusion_reason'
)
assert_colnames(df, req_cols, only_colnames = F)

# Convert necessary columns to numeric
for(numeric_col in c('bundle_id', 'seq', 'nid', 'location_id', 'age_start', 'age_end', "age_mean", "age_sd", 'year_start_study', 'year_end_study', 'effect_size', 'lower', 'upper', 'cohort_exposed_low_limit', 'cohort_exposed_upper_limit',
                     'cohort_unexposed_low_limit', 'cohort_unexposed_upper_limit', 'washout_years', grep('score', names(df), value = T), 'gbd_duplicate', 'gbd_keep', 'ushd_keep', "include_nid",
                     'percent_race_1', 'percent_race_2', 'percent_race_3', 'percent_race_4', 'percent_race_5', 'percent_race_6',"subgroup_analysis")){
  message(sprintf("Attempting to convert %s to numeric type", numeric_col))
  df[, (numeric_col) := as.numeric(get(numeric_col))]
} 

# ushd_keep should be 1 (don't keep) if include_nid = 0 (don't keep)
stopifnot(df[ushd_keep == 0 & include_nid == 0, .N] == 0)
# remove rows for NIDs we are excluding 
df <- df[include_nid ==1]


# Populate columns that have contingencies (e.g. where missing values can be approximated by values in other columns)

# Create start/end ages (because cannot be blank when uploading to ELMO)
df[is.na(age_start) & is.na(age_end) & !is.na(age_mean) & !is.na(age_sd), c('age_start', 'age_end', 'note_sr') := .(age_mean - 1.96*age_sd, age_mean + 1.96*age_sd, paste(note_sr, "constructed age start/end from age mean/SD assuming normally distributed age: age_mean - 1.96*age_sd, age_mean + 1.96*age_sd"))]

# check that the race-information variables are appropriately handled
stopifnot(df[ushd_keep == 0 & (how_race_incorported == "[Blank]" | is.na(how_race_incorported)), .N] == 0)

### check that USHD_keep makes sense

# ushd_keep should not be missing
stopifnot(df[is.na(ushd_keep), .N] == 0) 
# if the analysis is not stratified by race/ethnicity, ushd_keep and gbd_keep should be the same (after excluding articles that don't meet USHD inclusion criteria)
stopifnot(df[how_race_incorported != "Stratified" & include_nid == 1, mean(ushd_keep == gbd_keep, na.rm = T)] == 1)

# USHD keep should be 1 (don't keep) when the analysis is stratified but the row does not correspond to subgroup analysis
stopifnot(df[how_race_incorported == "Stratified" & subgroup_analysis == 1, mean(ushd_keep)] == 1)
# USHD keep should generally be 0 (keep) when the analysis is stratified AND the row corresponds to subgroup analysis -- however, this will not be the case if the subgroup analysis is not related to race, so check

# Validate that duplicates do not exist across expected dimensions
if(nrow(df) != nrow(unique(df))){
  print(df[duplicated(df)])
  stop(sprintf("Duplicates exist in rows across dataset. Check extraction sheet"))
} 

# Validate that column values fall within plausible ranges

# Age start and age end positive
df[, age_diff := age_end - age_start]
assert_values(df[!is.na(age_diff)], c('age_start', 'age_end', 'age_diff'), test = 'gte', test_val = 0)
df[, age_diff := NULL]

# Sex male female both
df[, sex := str_to_title(sex)]
assert_values(df[ushd_keep == 0], 'sex', test = 'in', c("Male", "Female", "Both"))

# Effect size, lower, upper positive
df[, effect_diff := upper - lower]
if(df[is.na(effect_diff), .N] > 0) warning("Effect diff NA in ", df[is.na(effect_diff), .N], " rows")
assert_values(df[!is.na(effect_diff)], c('effect_size', 'lower', 'upper', 'effect_diff'), test = 'gte', test_val = 0)
df[, effect_diff := NULL]

# race percentages must be between 0 and 100 (missing is okay, except for first row)
assert_values(df[ushd_keep == 0], c("percent_race_1", "race_1"), test = "not_na")
assert_values(df, colnames = c('percent_race_1', 'percent_race_2', 'percent_race_3', 'percent_race_4', 'percent_race_5', 'percent_race_6'), test = 'gte', test_val = 0, na.rm = T) 
assert_values(df, colnames = c('percent_race_1', 'percent_race_2', 'percent_race_3', 'percent_race_4', 'percent_race_5', 'percent_race_6'), test = 'lte', test_val = 100, na.rm = T) 
# sum of races should be around 100 (within a whole number due to rounding)
df[, race_perc_sum := sum(percent_race_1, percent_race_2, percent_race_3, percent_race_4, percent_race_5, percent_race_6, na.rm = T), by = seq][, perc_diff := abs(100 - race_perc_sum)]
assert_values(df[ushd_keep == 0], colnames = "perc_diff", test = 'lte', test_val = 0.11)
df[, c("perc_diff", "race_perc_sum") := NULL]
# turn percentages into decimals
df[, c('percent_race_1', 'percent_race_2', 'percent_race_3', 'percent_race_4', 'percent_race_5', 'percent_race_6') := lapply(.SD, function(x) round(x/100, digits = 3)),
   .SDcols = c('percent_race_1', 'percent_race_2', 'percent_race_3', 'percent_race_4', 'percent_race_5', 'percent_race_6')]


# Score columns 0 1 or ranges
assert_values(df, c('representative_score', 
                    'exposure_idv_score', 
                    'exposure_obj_score', 
                    'exposure_multi_score', 
                    'outcomes_record_score', 
                    'outcomes_blind_score', 
                    'control_rct_other_score', 
                    'reverse_causality_score'),
              test = 'in', test_val = c(0,1))

# BMI categories plausible

# Checking whether lower bmi limits capped at lower biological limit
assert_values(df[ushd_keep == 0], c('cohort_exposed_low_limit',
                    'cohort_unexposed_low_limit'), test = 'gte', test_val = 10, na.rm = T) # NAs okay

# Checking whether upper bmi limits capped at upper biological limit
assert_values(df[ushd_keep == 0], c('cohort_exposed_upper_limit',
                    'cohort_unexposed_upper_limit'), test = 'lte', test_val = 70, na.rm = T)

df[, exposed_diff := cohort_exposed_upper_limit - cohort_exposed_low_limit][, unexposed_diff := cohort_unexposed_upper_limit - cohort_unexposed_low_limit]

assert_values(df, c('exposed_diff', 'unexposed_diff'), test = 'gte', test_val = 0, , na.rm = T)
df[, c('exposed_diff', 'unexposed_diff') := NULL]

# merge on REI/CAUSE IDs
reis <- get_ids("rei")
causes <- get_ids("cause")

df <- merge(df, reis[, .(rei_id, rei_name)], by.x = "risk", by.y = "rei_name", all.x = T)[, c(names(df), "rei_id"), with = F] # avoid reordering
df <- merge(df, causes[, .(cause_id, cause_name)], by.x = "outcome", by.y = "cause_name", all.x = T)[, c(names(df), "cause_id"), with = F]
assert_values(df, "rei_id", test = "equal", 370) # REI ID should be 370
assert_values(df, "rei_id", test = "not_na")
assert_values(df, "cause_id", test = "not_na")
setkey(df, seq)

# Clean to match ELMO -----------------------------------------------------

setnames(df, old = c("year_start_study", "year_end_study"), new = c("year_start", "year_end"))
df[effect_size_measure %in% c("Relative risk (RR)", "Hazard ratio (HR)", "Odds ratio (OR)"), measure := "relrisk"]

# save --------------------------------------------------------------------

# if passes validations, save:
file_out <- gsub("CLEANED", "CLEANED_VALIDATED", file)
file_out_formated <- gsub("VALIDATED", "VALIDATED_formated", file_out) 
# get the full workbook for extractions and add the validated version as the first page
workbook <- loadWorkbook(file)
# overwrite the extraction sheet with validated data
writeData(wb = workbook, sheet = "extraction", x = df, colNames = T, startRow = 2)
saveWorkbook(workbook, file_out_formated, overwrite = T) # this version preserves formating and has a second header, which cannot be uploaded to DB

# create a version without formating so that it can be uploaded to DB, but it messes up the formating/validation in the spreadsheet
# # remove the second header row, which cannot be uploaded
# deleteData(workbook, "extraction", rows = 2, cols = 1:ncol(df), gridExpand = T)
removeWorksheet(wb = workbook, sheet = "extraction")
addWorksheet(wb = workbook, sheetName = "extraction")
worksheetOrder(workbook) <- c(length(names(workbook)), 1:(length(names(workbook)) -1 )) # make the extraction sheet first
# now that we aren't worried about formating with excel, drop some unused columns
# need to escape instances of '\n' (line breaks are converted to \n when xlsx is read into R)
#    because it caused errors with the database functions
df[, names(df) := lapply(.SD, function(x) gsub("\n", "\\\\n", x))] # need four slashes so that  "hello\nworld" turns into "hello\\nworld"
df[, names(df) := lapply(.SD, function(x) gsub("%", "%%", x))] # escape percent signs
df[, names(df) := lapply(.SD, function(x) gsub("\\\\", "", x))] # remove trailing backslash


writeData(wb = workbook, sheet = "extraction", x = df, colNames = T, startRow = 1)
activeSheet(workbook) <- "extraction"
saveWorkbook(workbook, file_out, overwrite = T) # this version does not preserve formating and removes first row