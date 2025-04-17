###########################################################################################################################################
## Description: Create county-year-age-sex-race/ethnicity aggregates of self-reported health data from ACS
##
## Input:      Combined microdata files from ACS.
## Output:     Aggregated data.
##
###########################################################################################################################################

######## 1. Setup
#### Load required packages installed in Singularity image
pacman::p_load(data.table, ggplot2, haven, bit64, stringr, car)
pacman::p_load(rgdal, ggthemes, cowplot, mapproj, maptools, rgeos)

###### Source functions
repo <- "FILEPATH"
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

.libPaths(c("FILEPATH"))
pacman::p_load(psych, digest) # Load custom-installed packages

#### Set paths
(data_version <- make_time_stamp())
input_file <- "FILEPATH"
output_dir <- paste0("FILEPATH")

dir.create(output_dir)

puma_mcnty_path <- "FILEPATH"

######## 2. Load individual data set
data_combined <- fread(input_file)

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

#### First check missingness of all variables across samples
missingness <- check_missingness(data_combined, "YEAR")

#### Process disability variables
data_combined[, DIFFREM := car::recode(DIFFREM, "1=0; 2=1; else = NA")] # Process DIFFREM (difficulty remembering)
data_combined[, DIFFPHYS := car::recode(DIFFPHYS, "1=0; 2=1; else = NA")] # Process DIFFPHYS (physical limitations)
data_combined[, DIFFMOB := car::recode(DIFFMOB, "1=0; 2=1; else = NA")] # Process DIFFMOB (independent living limitations)
data_combined[, DIFFCARE := car::recode(DIFFCARE, "1=0; 2=1; else = NA")] # Process DIFFCARE (self care limitations)
data_combined[, DIFFSENS := car::recode(DIFFSENS, "1=0; 2=1; else = NA")] # Process DIFFSENS (hearing or vision difficulty)
data_combined[, DIFFEYE := car::recode(DIFFEYE, "1=0; 2=1; else = NA")] # Process DIFFEYE (vision difficulty)
data_combined[, DIFFHEAR := car::recode(DIFFHEAR, "1=0; 2=1; else = NA")] # Process DIFFHEAR (hearing difficulty)

#### Recheck missingness in disability variables
# DIFFMOB is only asked of individuals 16+. DIFFREM, DIFFPHYS, DIFFCARE, DIFFSENS are only asked of individuals 5+,
# except that DIFFSENS (actually DIFFEYE and DIFFHEAR) was asked of all ages starting in 2008.

# https://censusreporter.org/topics/disability/: "Disability status is simply defined as having one or more of the difficulties listed above. 
# However, for children under five, only hearing and vision difficulties are considered. Independent living difficulty is also only applied to adults."
# Based on the pattern of missingness, DIFFMOB is recorded for individuals 16+

#### Examine COUNTYFIP variable
## From the documentation: "Counties are not identified in public-use microdata from 1950 onwards, so IPUMS instead identifies counties, where possible, from other low-level geographic identifiers. These include State Economic Areas (SEA) in 1950; county groups in 1970 (CNTYGP97) and 1980 (CNTYGP98); and Public Use Microdata Areas (PUMA) from 1998 onwards, including Super-PUMAs (PUMASUPR) in 2000.
# COUNTYFIP identifies a county if and only if:
#   it was coterminous with a single SEA, county group, or PUMA; or
#   it contained multiple SEAs, county groups, or PUMAs, none of which extended into other counties."
data_combined[COUNTYFIP == 0, COUNTYFIP := NA]

#### Combine state and county FIPS codes
data_combined[!is.na(COUNTYFIP), FIPS := paste0(sprintf("%02d", STATEFIP), sprintf("%03d", COUNTYFIP))]

#### Load state-cnty-mcnty crosswalk file
locs <- fread("FILEPATH")
locs[, cnty := sprintf("%05d", cnty)]

#### Merge mcnty
data_combined <- merge(data_combined, locs, by.x = c("FIPS"), by.y = c("cnty"), all.x = TRUE)

#### Merge on state and state_name for rows without COUNTYFIP
no_countyfip <- merge(data_combined[is.na(COUNTYFIP), -c("state", "state_name")], unique(locs[, c("state", "state_name")]), by.x = "STATEFIP", by.y = "state", all.x = TRUE)
data_combined <- rbindlist(list(data_combined[!is.na(COUNTYFIP)], no_countyfip), use.names = TRUE, fill = TRUE)
data_combined[, state := STATEFIP]

#### Create USHD age variables
data_combined[!is.na(AGE), age_bin := floor(AGE / 5) * 5]
data_combined[!is.na(AGE) & AGE >= 85, age_bin := 85]
data_combined[!is.na(AGE) & AGE >= 1 & AGE < 5, age_bin := 1]

#### Process race/ethnicity
## Recode race, ignoring Other and Multiracial
## Race codes:
# 2: Hispanic
# 4: NH Black
# 5: NH White
# 6: NH AIAN
# 7: NH API

# ACS (RACE variable)
# Value	Label
# 1	White
# 2	Black/African American/Negro
# 3	American Indian or Alaska Native
# 4	Chinese
# 5	Japanese
# 6	Other Asian or Pacific Islander
# 7	Other race, nec
# 8	Two major races
# 9	Three or more major races

#### Use the new PREDAI, PREDAPI, PREDBLK, PREDWHIT, and PREDHISP variables. This requires replication of individuals for each race group, with re_weights set as with Gallup data.
#### Add a temporary unique ID
data_combined$temp_uid <- 1:nrow(data_combined)

#### Check that race predictions sum to 1 for all individuals
stopifnot(data_combined[, PREDWHT + PREDBLK + PREDAI + PREDAPI + PREDHISP, by = temp_uid][, max(abs(V1 - 1))] < 1e-6)

replication <- vector(mode = "list", length = 5)
categories <- data.table(code = c(2, 4:7), variable = c("PREDHISP", "PREDBLK", "PREDWHT", "PREDAI", "PREDAPI"))
for (i in 1:nrow(categories)) {
  replication[[i]] <- data_combined
  replication[[i]][, race_ushd := categories[i, code]]
  replication[[i]][, re_weight := .SD, .SDcols = categories[i, variable]]
  replication[[i]] <- replication[[i]][re_weight != 0]
}

replication <- rbindlist(replication, use.names = TRUE, fill = TRUE)

#### Check that re_weight sums to 1 for each individual
stopifnot(replication[, sum(re_weight), by = temp_uid][, max(abs(V1 - 1))] < 1e-6)

#### Check that temp_uids are equal between data_combined and replication
stopifnot(all.equal(sort(unique(data_combined$temp_uid)), sort(unique(replication$temp_uid))))

#### Set data_combined to replication
data_combined <- replication
rm(replication)

#### Process education
# Use EDUCD to provide more nuance in classifications. See https://usa.ipums.org/usa-action/variables/EDUC#codes_section for coding.
# USHD classifications: 1 = < HS, 2 = HS or equivalent, 3 = Some college, 4 = BA or higher
data_combined[, edu_ushd := car::recode(EDUCD, "c(2, 10, 11, 12, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 30, 40, 50, 61)=1; c(62, 63, 64)=2; c(65, 71, 81)=3; c(101, 114, 115, 116)=4; else = NA")]
# There is steadily decreasing missingness in the education variable over time.

#### Create a composite "any disability" variable, conditional on age
data_combined[AGE %in% 0:4, DIFFANY := as.integer(rowSums(.SD, na.rm = TRUE) > 0), .SDcols = c("DIFFSENS")]
data_combined[AGE %in% 0:4 & is.na(DIFFSENS) & DIFFANY == 0, DIFFANY := NA]
data_combined[AGE %in% 5:15, DIFFANY := as.integer(rowSums(.SD, na.rm = TRUE) > 0), .SDcols = c("DIFFREM", "DIFFPHYS", "DIFFCARE", "DIFFSENS")]
data_combined[AGE %in% 5:15 & (is.na(DIFFREM) || is.na(DIFFPHYS) || is.na(DIFFCARE) || is.na(DIFFSENS)) & DIFFANY == 0, DIFFANY := NA]
data_combined[AGE >= 16, DIFFANY := as.integer(rowSums(.SD, na.rm = TRUE) > 0), .SDcols = c("DIFFREM", "DIFFPHYS", "DIFFMOB", "DIFFCARE", "DIFFSENS")]
data_combined[AGE >= 16 & (is.na(DIFFREM) || is.na(DIFFPHYS) || is.na(DIFFMOB) || is.na(DIFFCARE) || is.na(DIFFSENS)) & DIFFANY == 0, DIFFANY := NA]

#### Process marital status
# ACS coding for marital status:
# MARST               Marital status
# 1                   Married, spouse present
# 2                   Married, spouse absent
# 3                   Separated
# 4                   Divorced
# 5                   Widowed
# 6                   Never married/single

# Coding to conform to correct coding in IPUMS-based post-stratification frame
# 1: Currently married
# 2: Formerly married
# 3: Never married

data_combined[, marital_ushd := car::recode(MARST, "6=3; c(1, 2)=1; c(3, 4, 5)=2; else = NA")]

#### Rename some fields
setnames(data_combined, c("SEX", "PERWT", "YEAR", "race_ushd", "edu_ushd", "marital_ushd"), c("sex", "weights", "year", "race", "edu", "marital"))

# For proper variance estimation, see:
# https://usa.ipums.org/usa/complex_survey_vars/userNotes_variance.shtml
# https://usa.ipums.org/usa/resources/complex_survey_vars/UserNote_Variance.pdf

#### Retrieve composite PUMA-mcnty identifiers
puma_mcnty <- readRDS(paste0(puma_mcnty_path, "/puma_mcnty_mapping.rds"))

#### Merge PUMA identifiers onto data_combined, according to survey year
# Note on comparability, from https://usa.ipums.org/usa-action/variables/PUMA#comparability_section:
# "This variable is not comparable across years. PUMAs did not necessarily retain the same boundaries or PUMA codes between 1990 and 2000. 
# The boundaries and PUMA codes are the same for the 2000 census and the 2005-2011 ACS/PRCS samples, with one notable exception in Louisiana:
# due to population displacement following Hurricane Katrina, three PUMA's (01801, 01802, and 01905) are combined into code 77777 for the
# 2006-onward ACS and for all cases in the 2005-2007 ACS 3-Year file. They no longer had sufficient population to be included as separate
# entities. PUMA boundaries based on the 2010 Decennial Census are used for 2012-onward ACS samples."
data_combined[, PUMA_ID := paste0(sprintf("%02d", STATEFIP), sprintf("%05d", PUMA))]

data_combined <- merge(data_combined, unique(puma_mcnty[, c("PUMA_ID", "puma_mcnty", "puma_version", "year")]), by = c("PUMA_ID", "year"), all.x = TRUE)

#### Restrict to 2009-2019
data_combined <- data_combined[year %in% 2009:2019]

#### Check that all mcntys with county-level data are in at most only a single puma_mcnty for each PUMA version
stopifnot(nrow(data_combined[!is.na(mcnty), .(count = length(unique(puma_mcnty))), by = list(mcnty, puma_version)][count != 1]) == 0)

#### Add a unique ID
data_combined$uid <- 1:nrow(data_combined)

#### Drop state-level data for all years except 2001-2004 (for which only state-level data were available)
data_combined <- data_combined[(year %in% 2001:2004 & !is.na(STATEFIP)) | (year %in% c(2000, 2005:2019) & !is.na(puma_mcnty))]

#### Set mcnty-level identifiers to NA (only collapse by puma-mcnty)
data_combined[, mcnty := NA]

#### Save full data set to disk
saveRDS(data_combined, file = paste0(output_dir, "/data_full.rds"))


######## 3. Process and save disability data
# Establish a function for processing data for each type of disability
process_disability <- function(dt, outcome_var, strata_vars, output_dir) {
  message(paste0("Processing ", outcome_var))
  original_count <- length(unique(dt$uid))
  
  ## Establish columns that must be present. Note: STATEFIP is the coarsest geographic scale considered here.
  check_cols <- c(outcome_var, strata_vars, "STATEFIP")
  
  ## Subset to rows with missingness and rows without missingness in outcome variable, respectively
  message("Subsetting to missing and complete cases...")
  dt_missing <- dt[!complete.cases(dt[, ..check_cols])]
  dt <- dt[!is.na(get(outcome_var))] # Drop rows missing a value for the outcome
  
  ## Update list of vars for stratification
  strata_vars <- c(strata_vars, "puma_version", "puma_mcnty")
  
  ## Collapse by strata
  message("Aggregating data by requested strata...")
  dt <- dt[, list(count = sum(get(outcome_var) * re_weight), weighted_count = weighted.mean(get(outcome_var), weights * re_weight) * sum(re_weight), weights = sum(weights * re_weight), sample_size = sum(re_weight)), by = strata_vars]
  
  ## Save outputs
  message(paste0("Saving outputs to ", output_dir))
  saveRDS(dt_missing, file = paste0(output_dir, "/acs_missing_data_", outcome_var, ".rds"))
  saveRDS(dt, file = paste0(output_dir, "/acs_agg_data_", outcome_var, ".rds"))
}

#### Prepare data for each disability type
# Cognitive disability (DIFFREM)
process_disability(dt = data_combined, outcome_var = "DIFFREM", strata_vars = c("year", "sex", "race", "age_bin", "edu", "marital"), output_dir = output_dir)

# Physical disability (DIFFPHYS)
process_disability(dt = data_combined, outcome_var = "DIFFPHYS", strata_vars = c("year", "sex", "race", "age_bin", "edu", "marital"), output_dir = output_dir)

# Independent living disability (DIFFMOB)
process_disability(dt = data_combined, outcome_var = "DIFFMOB", strata_vars = c("year", "sex", "race", "age_bin", "edu", "marital"), output_dir = output_dir)

# Self-care disability (DIFFCARE)
process_disability(dt = data_combined, outcome_var = "DIFFCARE", strata_vars = c("year", "sex", "race", "age_bin", "edu", "marital"), output_dir = output_dir)

# Any disability (DIFFSENS)
process_disability(dt = data_combined, outcome_var = "DIFFSENS", strata_vars = c("year", "sex", "race", "age_bin", "edu", "marital"), output_dir = output_dir)

# Any disability (DIFFANY)
process_disability(dt = data_combined, outcome_var = "DIFFANY", strata_vars = c("year", "sex", "race", "age_bin", "edu", "marital"), output_dir = output_dir)

#### Save input paths
sink(paste0(output_dir, "/input_versions.txt"))
print(paste("ACS microdata: ", input_file))
print(paste("PUMA-mcnty crosswalk: ", puma_mcnty_path))
sink()


######## Produce direct estimates by race/ethnicity
#### Restrict to modeling years to save computation time
model_years <- 2009:2019
data_combined <- data_combined[year %in% model_years]

#### Produce puma_mcnty-level direct estimates
message("Produce puma_mcnty-level direct estimates")

puma_mcnty_estimates <- data_combined[, list(level = "puma_mcnty", DIFFREM = weighted.mean(DIFFREM, re_weight * weights), DIFFPHYS = weighted.mean(DIFFPHYS, re_weight * weights), DIFFMOB = weighted.mean(DIFFMOB, re_weight * weights),
                                             DIFFCARE = weighted.mean(DIFFCARE, re_weight * weights), DIFFSENS = weighted.mean(DIFFSENS, re_weight * weights),
                                             DIFFANY = weighted.mean(DIFFANY, re_weight * weights), weights = sum(re_weight * weights), sample_size = sum(re_weight)), by = list(puma_mcnty, puma_version, state, state_name, year, sex, race, age_bin, edu, marital)]

#### Produce state-level direct estimates
message("Produce state-level direct estimates")
state_estimates <- data_combined[, list(level = "state", DIFFREM = weighted.mean(DIFFREM, re_weight * weights), DIFFPHYS = weighted.mean(DIFFPHYS, re_weight * weights), DIFFMOB = weighted.mean(DIFFMOB, re_weight * weights),
                                        DIFFCARE = weighted.mean(DIFFCARE, re_weight * weights), DIFFSENS = weighted.mean(DIFFSENS, re_weight * weights),
                                        DIFFANY = weighted.mean(DIFFANY, re_weight * weights), weights = sum(re_weight * weights), sample_size = sum(re_weight)), by = list(state, state_name, year, sex, race, age_bin, edu, marital)]

#### Load and merge population file
puma_mcnty <- readRDS(paste0(puma_mcnty_path, "/puma_mcnty_full.rds"))
puma_mcnty$age <- as.integer(as.character(puma_mcnty$age))
puma_mcnty_state <- puma_mcnty[, list(pop = sum(pop)), by = c("year", "sex", "age", "race", "state", "edu", "marital")]

puma_mcnty_estimates <- merge(puma_mcnty_estimates, unique(puma_mcnty[, c("puma_mcnty", "puma_version", "year", "sex", "race", "age", "total_pop", "edu", "marital")]), by.x = c("puma_mcnty", "puma_version", "year", "sex", "race", "age_bin", "edu", "marital"), by.y = c("puma_mcnty", "puma_version", "year", "sex", "race", "age", "edu", "marital"), all.x = TRUE)

setnames(puma_mcnty_estimates, "total_pop", "pop")

state_estimates <- merge(state_estimates, puma_mcnty_state, by.x = c("state", "year", "sex", "race", "age_bin", "edu", "marital"), by.y = c("state", "year", "sex", "race", "age", "edu", "marital"), all.x = TRUE)

#### Produce national-level direct estimates
message("Produce national-level direct estimates")
national_estimates <- puma_mcnty_estimates[, list(level = "natl", DIFFREM = weighted.mean(DIFFREM, weights), DIFFPHYS = weighted.mean(DIFFPHYS, weights), DIFFMOB = weighted.mean(DIFFMOB, weights),
                                                  DIFFCARE = weighted.mean(DIFFCARE, weights), DIFFSENS = weighted.mean(DIFFSENS, weights),
                                                  DIFFANY = weighted.mean(DIFFANY, weights), pop = sum(pop), weights = sum(weights), sample_size = sum(sample_size)), by = list(year, sex, race, age_bin, edu, marital)]

stopifnot(all.equal(nrow(data_combined), sum(puma_mcnty_estimates$sample_size), sum(state_estimates$sample_size), sum(national_estimates$sample_size)))

#### Combine direct estimates across levels
direct_estimates <- rbindlist(list(puma_mcnty_estimates, state_estimates, national_estimates), use.names = TRUE, fill = TRUE)

#### Harmonize area identifiers
direct_estimates[level == "puma_mcnty", area := puma_mcnty]
direct_estimates[level == "state", area := state]
direct_estimates[level == "natl", area := 1]

#### Set weights
direct_estimates[, final_weight := weights]

#### Aggregate across sexes
message("Aggregate across sexes")
sex_agg <- direct_estimates[, list(sex = 3, DIFFREM = weighted.mean(DIFFREM, final_weight), DIFFPHYS = weighted.mean(DIFFPHYS, final_weight), DIFFMOB = weighted.mean(DIFFMOB, final_weight),
                                   DIFFCARE = weighted.mean(DIFFCARE, final_weight), DIFFSENS = weighted.mean(DIFFSENS, final_weight),
                                   DIFFANY = weighted.mean(DIFFANY, final_weight),
                                   weights = sum(weights), sample_size = sum(sample_size), pop = sum(pop), final_weight = sum(final_weight)), by = list(area, level, year, race, age_bin, edu, marital)]
direct_estimates <- rbindlist(list(direct_estimates, sex_agg), use.names = TRUE, fill = TRUE)

#### Aggregate across races
message("Aggregate across races")
race_agg <- direct_estimates[, list(race = 1, DIFFREM = weighted.mean(DIFFREM, final_weight), DIFFPHYS = weighted.mean(DIFFPHYS, final_weight), DIFFMOB = weighted.mean(DIFFMOB, final_weight),
                                    DIFFCARE = weighted.mean(DIFFCARE, final_weight), DIFFSENS = weighted.mean(DIFFSENS, final_weight),
                                    DIFFANY = weighted.mean(DIFFANY, final_weight), weights = sum(weights), sample_size = sum(sample_size), pop = sum(pop), final_weight = sum(final_weight)), by = list(area, level, year, sex, age_bin, edu, marital)]
direct_estimates <- rbindlist(list(direct_estimates, race_agg), use.names = TRUE, fill = TRUE)

#### Aggregate across ages
message("Aggregate across ages")
age_agg <- direct_estimates[, list(age_bin = 98, DIFFREM = weighted.mean(DIFFREM, final_weight), DIFFPHYS = weighted.mean(DIFFPHYS, final_weight), DIFFMOB = weighted.mean(DIFFMOB, final_weight),
                                   DIFFCARE = weighted.mean(DIFFCARE, final_weight), DIFFSENS = weighted.mean(DIFFSENS, final_weight),
                                   DIFFANY = weighted.mean(DIFFANY, final_weight), weights = sum(weights), sample_size = sum(sample_size), pop = sum(pop), final_weight = sum(final_weight)), by = list(area, level, year, sex, race, edu, marital)]
direct_estimates <- rbindlist(list(direct_estimates, age_agg), use.names = TRUE, fill = TRUE)

#### Aggregate across ages present for a given indicator (call this age group -1)
age_agg <- direct_estimates[age_bin != 98, list(age_bin = -1, DIFFREM = weighted.mean(DIFFREM, final_weight, na.rm = TRUE), DIFFPHYS = weighted.mean(DIFFPHYS, final_weight, na.rm = TRUE), DIFFMOB = weighted.mean(DIFFMOB, final_weight, na.rm = TRUE),
                                                DIFFCARE = weighted.mean(DIFFCARE, final_weight, na.rm = TRUE), DIFFSENS = weighted.mean(DIFFSENS, final_weight, na.rm = TRUE),
                                                DIFFANY = weighted.mean(DIFFANY, final_weight, na.rm = TRUE), weights = sum(weights), sample_size = sum(sample_size), pop = sum(pop), final_weight = sum(final_weight)), by = list(area, level, year, sex, race, edu, marital)]
direct_estimates <- rbindlist(list(direct_estimates, age_agg), use.names = TRUE, fill = TRUE)

#### Rename age column
setnames(direct_estimates, "age_bin", "age")
setnames(direct_estimates_no_strat, "age_bin", "age")

#### Aggregate across edu
message("Aggregate across edu")
edu_agg <- direct_estimates[, list(edu = 9, DIFFREM = weighted.mean(DIFFREM, final_weight, na.rm = TRUE), DIFFPHYS = weighted.mean(DIFFPHYS, final_weight, na.rm = TRUE), DIFFMOB = weighted.mean(DIFFMOB, final_weight, na.rm = TRUE),
                                   DIFFCARE = weighted.mean(DIFFCARE, final_weight, na.rm = TRUE), DIFFSENS = weighted.mean(DIFFSENS, final_weight, na.rm = TRUE),
                                   DIFFANY = weighted.mean(DIFFANY, final_weight, na.rm = TRUE), weights = sum(weights), sample_size = sum(sample_size), pop = sum(pop), final_weight = sum(final_weight)), by = list(area, level, year, sex, age, race, marital)]
direct_estimates <- rbindlist(list(direct_estimates, edu_agg), use.names = TRUE, fill = TRUE)

#### Aggregate across marital
message("Aggregate across marital")
marital_agg <- direct_estimates[, list(marital = 9, DIFFREM = weighted.mean(DIFFREM, final_weight, na.rm = TRUE), DIFFPHYS = weighted.mean(DIFFPHYS, final_weight, na.rm = TRUE), DIFFMOB = weighted.mean(DIFFMOB, final_weight, na.rm = TRUE),
                                       DIFFCARE = weighted.mean(DIFFCARE, final_weight, na.rm = TRUE), DIFFSENS = weighted.mean(DIFFSENS, final_weight, na.rm = TRUE),
                                       DIFFANY = weighted.mean(DIFFANY, final_weight, na.rm = TRUE), weights = sum(weights), sample_size = sum(sample_size), pop = sum(pop), final_weight = sum(final_weight)), by = list(area, level, year, sex, age, race, edu)]
direct_estimates <- rbindlist(list(direct_estimates, marital_agg), use.names = TRUE, fill = TRUE)


#### Save direct estimates
message("Save direct estimates")
saveRDS(direct_estimates, file = paste0(output_dir, "/acs_2009_2019_direct_estimates.rds"))