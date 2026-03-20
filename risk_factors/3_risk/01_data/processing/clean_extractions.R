####################################################################################################
## Description: Performs validations and calculations needed to model RR curves 
##                for BMI (on USHD)
##            
## Input: Latest bundle version of RR extractions (bundle ID 9963) (extractions)
##        Raw extractions FILEPATH
##          
## Output: csv that can be used for MR-BRT analysis
##            FILEPATH
## 
####################################################################################################

# set up ------------------------------------------------------------------

library(data.table)
library(openxlsx)
library(assertable)
source("FILEPATH")
source("FILEPATH")

bundle_id <- 9963
rei_id = 370

out_raw_root <- "FILEPATH"
out_prepped_root <- "FILEPATH"

# load lookup table
race_vars <- as.data.table(read.xlsx("FILEPATH"))
cause_ids <- get_ids("cause")

# Set bundle version ------------------------------------------------------
versions <- as.data.table(read.xlsx("FILEPATH"))
best_version <- versions[current_best == 1, bundle_version_id]

# create outdirs based on bundle
in_raw <- sprintf('%s/bundle%i/v%i', out_raw_root, bundle_id, best_version)
out_prepped <- sprintf('%s/bundle%i/v%i', out_prepped_root, bundle_id, best_version)
if(!dir.exists(out_prepped)) dir.create(out_prepped, recursive = T)

clean_mr_brt <- function(c_id){ # only argument is cause ID -- everything else in global environment
  cat("\n######### processing ", c_id, "#########\n")
  
  ## LOAD RELEVANT DATA
  file <- list.files(in_raw, pattern = sprintf('_%d.xlsx', c_id), full.names = T)
  # IMPT only keep rows specified as keep during extraction
  # ***EXTRACTOR WILL NEED TO ENSURE THAT THIS VARIABLE IS ACCURATE***
  df <- as.data.table(read.xlsx(file, na.strings = ""))[ushd_keep == 0]
  stopifnot(df[include_nid == 0 | ushd_keep == 1, .N] == 0)
  # Incidence column (may need mortality column as well)
  unique(df$outcome_type) # See if there are just 2 outcome types or also observations of both incidence and  mortality
  df[, custom_incidence := ifelse(outcome_type == "Incidence", 1, 0)]
  df[, custom_mortality := ifelse(outcome_type == "Mortality", 1, 0)]
  df[outcome_type == "Incidence & Mortality", `:=` (custom_incidence = 0, custom_mortality = 0)]
  
  if(c_id == 429){
    # making premenopausal indicator (so can adjust for that later and predict for postmenopausal)
    df[, custom_premeno := ifelse(X167 %like% "premenopausal", 1, 0)]
  }
  if(c_id == 465){
    df[, custom_premeno := ifelse(X167 %like% "premenopausal", 1, 0)]
    df[, custom_postmeno := ifelse(X167 %like% "postmenopausal", 1, 0)]
  }
  # Make indicator for total stroke
  if(c_id == 495){
    df[, custom_total_stroke := ifelse(outcome == "Ischemic stroke", 0, 1)]
  }
  # Make indicator for mid life bmi or late life bmi
  if(c_id == 543){
    warning("If modeling 543, look into which studies were eventually included in GBD.")
    # GBD-specific cleaning saved below (commented out) for reference
    # df[, custom_midlife_bmi := ifelse(bmi_exposure == "mid life", 1, 0)]
    # df[, custom_latelife_bmi := ifelse(bmi_exposure == "late life", 1, 0)]
    # if(mr_brt_version_id == 3) {
    #   # For studies that report mid life and late life effect sizes, only keep the mid life BMI
    #   mid_late_nids <- unique(df[,.(nid, custom_midlife_bmi)]) %>%
    #     filter(duplicated(nid)) %>%
    #     select(nid) 
    #   
    #   df <- df[(nid %in% mid_late_nids$nid & custom_midlife_bmi == 1) | !nid %in% mid_late_nids$nid, ]
    # }
    # 
    # if(mr_brt_version_id == 4) {
    #   # Version only looking at mid life bmi
    #   df <- df[custom_midlife_bmi == 1]
    # }
    # 
    # if(mr_brt_version_id == 5) {
    #   # Version only looking at late life BMI
    #   df <- df[custom_midlife_bmi == 0]
    # }
    # 
    # if(mr_brt_version_id == 6){
    #   # Version only looking at studies reporting washout period
    #   df <- df[reverse_causality_score == 0]
    # }
    # 
    # if(mr_brt_version_id == 7) {
    #   # Version only looking at mid life bmi (without updated studies)
    #   # Excluding studies that recalled BMI (Fitzpatrick) and duplicate cohort (Kivipelto)
    #   df <- df[custom_midlife_bmi == 1 & nid < 491951 & ! nid %in% c(454513, 309547)]
    # }
    # 
    # if(mr_brt_version_id == 8) {
    #   # Version only looking at mid life bmi (with updated studies)
    #   # Excluding studies that recalled BMI (Fitzpatrick) and duplicate cohort (Kivipelto)
    #   df <- df[custom_midlife_bmi == 1 & ! nid %in% c(454513, 309547)]
    # }
  }
  # Sex indicators
  # Note that observations labeled as "Both" for sex wil have 0s in custom_male and custom_female
  unique(df$sex)
  df[, custom_male := ifelse(sex %in% c("male", "Male", "Males"), 1, 0)]
  df[, custom_female := ifelse(sex %in% c("female", "Female", "Females"), 1, 0)]
  
  # Make all col names lowercase
  setnames(df, names(df), tolower(names(df)))
  
  # Some exposure columns are named custom* instead of cohort*; fixing that here
  sum(names(df) %in% c("custom_exp_level_lower", "custom_exp_level_upper", "custom_unexp_level_lower", "custom_unexp_level_upper"))
  if(sum(names(df) %in% c('cohort_exposed_low_limit', 'cohort_exposed_upper_limit', 'cohort_unexposed_low_limit', 'cohort_unexposed_upper_limit')) != 4){
    message("Relying on custom_exp etc. columns")
    
    df[, c("custom_exp_level_lower_sign", "custom_exp_level_upper_sign", "custom_unexp_level_lower_sign", "custom_unexp_level_upper_sign") := NULL]
    
    setnames(df, c("custom_exp_level_lower", "custom_exp_level_upper", "custom_unexp_level_lower", "custom_unexp_level_upper"),
             c('cohort_exposed_low_limit', 'cohort_exposed_upper_limit', 'cohort_unexposed_low_limit', 'cohort_unexposed_upper_limit'))
  } else {
    df[, c("custom_exp_level_lower_sign", "custom_exp_level_upper_sign", "custom_unexp_level_lower_sign", "custom_unexp_level_upper_sign") := NULL]
  }
  
  # Removed column 'exposure_category' for IHD
  keep_cols <- c('underlying_nid', 'nid', 'field_citation_value', 'effect_size_measure', 'effect_size', 'lower', 'upper', 
                 'sex', 'age_mean', 'cohort_exposed_low_limit', 'cohort_exposed_upper_limit', 'cohort_unexposed_low_limit', 'cohort_unexposed_upper_limit', 
                 'washout_years', 'value_of_duration_fup', grep('_score', names(df), value = T), grep("custom_", names(df), value = T),
                 'how_race_incorported', 'race_1', 'race_2', 'race_3','race_4', 'race_5', 'race_6', 
                 'percent_race_1', 'percent_race_2', 'percent_race_3', 'percent_race_4', 'percent_race_5','percent_race_6')
  keep_cols <- keep_cols[!keep_cols %in% c('notes_score', 'custom_PMID', 'custom_washout_period_notes')]
  
  # check if there are any columns expected that are missing in the data
  assert_colnames(df, keep_cols, only_colnames = F)
  
  ## BODY
  
  # Filter to cols want to keep
  df <- df[, keep_cols, with = F]
  stopifnot(df[is.na(control_for_confounders_score), .N] ==0)
  # df <- df[!is.na(control_for_confounders_score)]
  
  # rename columns for applied math folks
  setnames(df, c('cohort_unexposed_low_limit', 'cohort_unexposed_upper_limit', 
                 'cohort_exposed_low_limit', 'cohort_exposed_upper_limit'),
           c('a_0', 'a_1', 'b_0', 'b_1'))
  
  # convert category bounds to numerics
  df[, c('a_0', 'a_1', 'b_0', 'b_1') := lapply(.SD, as.numeric), .SDcols = c('a_0', 'a_1', 'b_0', 'b_1')]
  
  # set bounds on ranges based on empirical limits of BMI if info had not been extracted
  df[is.na(a_0), a_0 := 10][is.na(b_0), b_0 := 10]
  df[is.na(a_1), a_1 := 50][is.na(b_1), b_1 := 50]
  
  # If min reference bound is less than max alternative upper bound is greater than 50
  df[a_0 < 10, a_0 := 10][b_1 > 50, b_1 := 50]
  
  # print number of observations where 0 and add constant to mean/lower/upper if 0
  # Ideally there shouldn't be any rows where these columns have a value of 0
  # Would double check extractions if that is the case though
  print("number of observations where mean,lower, or upper is 0")
  nrow(df[lower == 0 | effect_size == 0 | upper == 0])
  const <- 1e-2
  df[lower == 0, lower := lower + const]
  df[upper == 0, upper := upper + const]
  df[effect_size == 0, effect_size := effect_size + const]
  
  # Transform mean and sd
  df[, ln_effect := log(effect_size)]
  df[, ln_se := (log(upper) - log(lower))/(2*qnorm(0.975))]
  
  # Create unique id
  df[, seq := 1:.N]
  
  # Add rei and gbd cause_id
  df[, rei_id := get('rei_id', .GlobalEnv)][, cause_id := c_id]
  
  # final keep columns
  keep_cols_final <- c('rei_id', 'cause_id','nid', 'underlying_nid', 'field_citation_value', 'seq', 
                       'sex', 'age_mean', 'effect_size', 'lower', 'upper',
                       'ln_effect', 'ln_se', 'a_0', 'a_1', 'b_0', 'b_1', 'washout_years', 'value_of_duration_fup',
                       grep("_score", names(df), value = T), grep("custom_", names(df), value = T),
                       grep("race", names(df), value = T))
  df[, grep(paste(sprintf('^%s$', keep_cols_final), collapse = '|'), names(df), invert = T, value = T) := NULL]
  setcolorder(df, keep_cols_final)
  
  ### make modelable race categories
  df[, num_race_groups := Reduce(`+`, lapply(.SD,function(x) !is.na(x))),
     .SDcols = c('race_1',
                 'race_2',
                 'race_3',
                 'race_4',
                 'race_5',
                 'race_6'),
     by = seq]
  stopifnot(df[num_race_groups == 1, mean(percent_race_1 == 1)] == 1 | df[num_race_groups == 1, .N] == 0) # race %  should be 1 if single race (skip check if not rows)
  
  # incorporate the effectively single race var
  assert_values(df[how_race_incorported == "Effectively single race"], "percent_race_1", test = 'gte', test_val= 0.9,
                warn_only=T, # generaly, % race should be > 90% if considering effectively single race. Slightly smaller percents are allowable if it's only for some BMI cgroups
                display_rows = TRUE)
  df[, treat_single_race := as.numeric(how_race_incorported == "Effectively single race")]
  
  # merge df and look up table
  df <- merge(df, race_vars, by.x = "race_1", by.y = "value", all.x = T)
  # if single race or effectively single race, use the target name (unless target is missing, in which case use the name in extraction sheet and flag); otherwise, use "combined"
  df[, race := ifelse(num_race_groups == 1 | treat_single_race == 1, ifelse(!is.na(target), target, sprintf("FIXME_%s", race_1)), "Combined")][, target := NULL]
  df[race %like% "FIXME", race_id := 99]
  
  # make sure there is only one race ID per race
  stopifnot(mean(df[, uniqueN(race_id), race][, V1 == 1]) ==1)
  
  # ## SAVE OUTPUTS
  c_name <- cause_ids[cause_id == c_id, acause]
  c_name_long <- cause_ids[cause_id == c_id, cause_name]
  df[, outcome := c_name_long]
  
  write.csv(df, sprintf('%s/ushd_high_bmi_%s_%d.csv', out_prepped, c_name, c_id), row.names = F)
  
  cat("\n######### Successfully processed ", c_id, "#########\n")
  return(df)
}

# cause files in raw
ff <- list.files(in_raw)
# get the cause IDs associated with each
all_causes <- sort(as.numeric(stringr::str_extract(ff, "(?<=_)[:digit:]*(?=\\.)")))

results <- sapply(all_causes, clean_mr_brt)

# Tabulate number of studies and number of observations (can add other tabulations later)

results <- rbindlist(results, fill = T)
# make of summary of data available by race

t1 <- results[, .(`NIDs (total)` = uniqueN(nid)),
   by = .(outcome)]
t2 <- results[, .(`NIDs per race group` = uniqueN(nid)),
         by = .(outcome, race)]
t3 <- results[, .(`Observations (total)` = .N),
         by = .(outcome)]
t4 <- results[, .(`Observations per race group` = .N),
         by = .(outcome, race)]

invisible(lapply(list(t1, t2, t3, t4), setkey, "outcome"))

tt <- t1[t3[t2[t4, on = c("outcome", "race")]]] # merge together

tt <- dcast(tt, outcome + `NIDs (total)` + `Observations (total)`~ race, fill = 0)

write.xlsx(tt, file=sprintf('%s/_ushd_high_bmi_summary.xlsx', out_prepped), asTable = T, tableStyle = "TableStyleMedium2")
