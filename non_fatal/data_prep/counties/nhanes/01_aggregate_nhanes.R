###########################################################################################################################################
## Description: Create county-year-age-sex-race/ethnicity aggregates of data from NHANES
##
## Input:       None
## Output:      Combined microdata files for NHANES, with requisite demographic and health data. Data are saved to the Limited Use
##              folder in FILEPATH.
##
###########################################################################################################################################

###### Load required libraries
pacman::p_load(data.table, ggplot2, car, R.utils)

#### Set paths
data_version <- ""
input_dir <- paste0("FILEPATH")

natl_mcnty_version <- ""
pop_loc <- "FILEPATH"

###### Source functions
repo <- "FILEPATH"
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

###### Generate run date and create model output folder
run_date <- make_time_stamp()
output_dir <- paste0("FILEPATH", run_date)
message(run_date)
message(output_dir)
dir.create(output_dir)


######## 2. Load individual data set and summarize missingness
data_combined <- readRDS(file = paste0(input_dir, "nhanes_microdata.rds"))

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

# missing <- check_missingness(data_combined)
missing_by_year <- check_missingness(data_combined, by_vars = "svyyear")
missing_by_age <- check_missingness(data_combined, by_vars = "age")

######## 3. Prep variables
###### Process diabetes variables
### DIQ010 (diq): Has a doctor ever told you that you have diabetes? (ages 1+)
# table(data_combined[, list(age, diq)], useNA = "always")
## Set diq values of 2 ("borderline") to 0 ("no")
data_combined[diq == 2, diq := 0]

#### Fit simple model to crosswalk A1c to FPG
mod <- lm(lbxglu ~ lbxgh + race:sex, data = data_combined[age >= 10])
preds <- cbind(data_combined[age >= 10], "lbxglu_pred" = predict(mod, newdata = data_combined[age >= 10]))
data_combined <- rbindlist(list(data_combined[age < 10], preds), use.names = TRUE, fill = TRUE)

## Use predicted FPG where only A1c is available
data_combined[, lbxglu_original := lbxglu] # Save a copy of the original values
data_combined[is.na(lbxglu) & !is.na(lbxglu_pred), lbxglu := lbxglu_pred]

### LBXGLU (lbxglu): Fasting Glucose (mg/dL) (ages 12+)
# The GBD case definition for diabetes includes individuals with FPG >= 126 mg/dL
data_combined[!is.na(lbxglu) & lbxglu >= 126, has_diabetes := 1]
data_combined[!is.na(lbxglu) & lbxglu < 126, has_diabetes := 0]

### DIQ050 (db_insulin): Taking insulin now
# Asked of all individuals
data_combined[!is.na(db_insulin) & db_insulin == 1, has_diabetes := 1]

### DIQ070 (db_pill): Take diabetic pills to lower blood sugar
# BOX 0. CHECK ITEM DIQ.065: IF DIQ.010 = 1 (YES) OR DIQ.010 = 3 (BORDERLINE OR PREDIABETES) OR DIQ.160 = 1 (YES) CONTINUE. OTHERWISE, GO TO END OF SECTION.
# Only asked of individuals who answered YES or BORDERLINE to DIQ010, or YES to DIQ160 (told that you have prediabetes)
data_combined[!is.na(db_pill) & db_pill == 1, has_diabetes := 1]

###### Update age field
data_combined[!is.na(age), age_bin := floor(age / 5) * 5]
data_combined[!is.na(age) & age >= 85, age_bin := 85]
data_combined[!is.na(age) & age >= 1 & age < 5, age_bin := 1]
data_combined[, c("age", "age_bin") := list(age_bin, NULL)]

###### Update race field
# For modeling using 1977 OMB categories, we'll use the race variable, which collapses NH Asian Only into "other". Leave it coded as text in this script; it will be processed in the pre-SAE data prep script.

#### Process marital
data_combined[marital == "current", marital_ushd := 1] # Currently married
data_combined[marital == "former", marital_ushd := 2] # Formerly married
data_combined[marital == "never", marital_ushd := 3] # Never married

#### Process edu
## USHD classifications: 1 = < HS, 2 = HS or equivalent, 3 = Some college, 4 = BA or higher
data_combined[edu == "less than HS", edu_ushd := 1] # Less than HS
data_combined[edu == "HS grad", edu_ushd := 2] # HS or equivalent
data_combined[edu == "some college", edu_ushd := 3] # Some college
data_combined[edu == "college grad", edu_ushd := 4] # BA or higher

###### Drop data from 2017_2020prp
data_combined <- data_combined[svyyear != "2017_2020prp"]

###### Final NHANES cleanup
data_combined$source <- "NHANES"

###### Collapse and save NHANES data (NHANES data are national-level)
has_diabetes <- data_combined[!is.na(has_diabetes) & !is.na(mec_wt), list(count = sum(has_diabetes), weighted_count = weighted.mean(has_diabetes, mec_wt) * .N, weights = sum(mec_wt), sample_size = .N), by = c("svyyear", "sex", "age", "race", "edu_ushd", "marital_ushd")]

#### Save collapsed and full data set to disk
saveRDS(has_diabetes, file = paste0(output_dir, "/has_diabetes.rds"))
saveRDS(data_combined, file = paste0(output_dir, "/data_full.rds"))


######## 4. Produce direct estimates
#### Save input paths
sink(paste0(output_dir, "/input_versions.txt"))
print(paste("Population input: ", pop_loc))
print(paste("NHANES microdata: ", input_dir))
print(paste("natl-mcnty crosswalk: ", natl_mcnty_version))
sink()

#### Drop rows with missing outcome
direct_estimates <- data_combined[!is.na(has_diabetes) & !is.na(mec_wt)]
setnames(direct_estimates, "mec_wt", "final_weight")

direct_estimates <- direct_estimates[, list(has_diabetes = weighted.mean(has_diabetes, final_weight), final_weight = sum(final_weight), sample_size = .N), by = c("svyyear", "sex", "age", "race", "marital_ushd", "edu_ushd", "source", "NID")]

#### Create area identifiers
direct_estimates[, c("level", "area") := list("natl", 1L)]

#### Aggregate across sexes
message("Aggregate across sex")
sex_agg <- direct_estimates[, list(sex = 3, has_diabetes = weighted.mean(has_diabetes, final_weight, na.rm = TRUE), sample_size = sum(sample_size), final_weight = sum(final_weight)),
                            by = list(area, level, svyyear, race, age, edu_ushd, marital_ushd, NID)]
direct_estimates <- rbindlist(list(direct_estimates, sex_agg), use.names = TRUE, fill = TRUE)
rm(sex_agg); gc()

#### Aggregate across races
message("Aggregate across race")
race_agg <- direct_estimates[, list(race = "all races", has_diabetes = weighted.mean(has_diabetes, final_weight, na.rm = TRUE), sample_size = sum(sample_size), final_weight = sum(final_weight)),
                             by = list(area, level, svyyear, sex, age, edu_ushd, marital_ushd, NID)]

direct_estimates <- rbindlist(list(direct_estimates, race_agg), use.names = TRUE, fill = TRUE)
rm(race_agg); gc()

#### Aggregate across ages
message("Aggregate across age")
age_agg <- direct_estimates[, list(age = 98, has_diabetes = weighted.mean(has_diabetes, final_weight, na.rm = TRUE), sample_size = sum(sample_size), final_weight = sum(final_weight)),
                            by = list(area, level, svyyear, sex, race, edu_ushd, marital_ushd, NID)]

direct_estimates <- rbindlist(list(direct_estimates, age_agg), use.names = TRUE, fill = TRUE)
rm(age_agg); gc()

#### Aggregate across edu
message("Aggregate across edu")
edu_agg <- direct_estimates[, list(edu_ushd = 9, has_diabetes = weighted.mean(has_diabetes, final_weight, na.rm = TRUE), sample_size = sum(sample_size), final_weight = sum(final_weight)),
                            by = list(area, level, svyyear, sex, race, age, marital_ushd, NID)]
direct_estimates <- rbindlist(list(direct_estimates, edu_agg), use.names = TRUE, fill = TRUE)
rm(edu_agg); gc()

#### Aggregate across marital
message("Aggregate across marital")
marital_agg <- direct_estimates[, list(marital_ushd = 9, has_diabetes = weighted.mean(has_diabetes, final_weight, na.rm = TRUE), sample_size = sum(sample_size), final_weight = sum(final_weight)),
                                by = list(area, level, svyyear, sex, race, age, edu_ushd, NID)]
direct_estimates <- rbindlist(list(direct_estimates, marital_agg), use.names = TRUE, fill = TRUE)
rm(marital_agg); gc()

direct_estimates$source <- "NHANES"

#### Save direct estimates
saveRDS(direct_estimates, file = paste0(output_dir, "/NHANES_2009_2018_direct_estimates.rds"))


####### Plot diagnostics for direct estimates
#### Create directory for plots
dir.create(path = paste0(output_dir, "/plots"))

#### Set plotting labels
sex_labels <- c("Males", "Females", "Both")
names(sex_labels) <- c(1:3)
race_labels <- c("NH White", "NH Black", "NH AIAN", "NH API", "Hispanic", "All Races/Ethnicities")
names(race_labels) <- c(1:4, 7, 9)
edu_labels <- c("Less than HS", "HS", "Some College", "BA or Higher", "All Education Categories")
names(edu_labels) <- c(1:4, 9)
marital_labels <- c("Currently Married", "Formerly Married", "Never Married", "All Marital Status Categories")
names(marital_labels) <- c(1:3, 9)

#### List outcomes
outcomes <- c("has_diabetes")

#### Rename some columns
setnames(direct_estimates, c("edu_ushd", "marital_ushd"), c("edu", "marital"))

#### Loop through outcomes and plot
for (outcome in outcomes) {
  print(paste0(outcome, "..."))
  pdf(paste0(output_dir, "/plots/plots_", outcome, ".pdf"), width = 11, height = 8.5)
  
  #### Plot national estimates by year, prevalence by age-sex-race
  current <- direct_estimates[marital == 9 & edu == 9 & age == 98 & level == "natl"]
  print(ggplot(data = current) + theme_bw() +
          geom_smooth(aes(x = as.factor(svyyear), y = get(outcome), group = factor(race), color = factor(race)), se = FALSE, size = 0.5) +
          geom_point(aes(x = as.factor(svyyear), y = get(outcome), color = factor(race), size = sample_size), alpha = 0.25) +
          labs(x = "Year", y = "Prevalence", title=paste0("Prevalence of ", outcome, " (Direct Estimates from NHANES), National"), subtitle = paste0("All ages, by sex.\nAll education and marital status categories.")) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) +
          scale_color_discrete(name = "Race") + scale_linetype_discrete(name = "Sex", labels = sex_labels) + scale_size_continuous(name = "Sample Size") +
          facet_grid(~ sex))
  
  #### Plot national estimates by age, prevalence by age-sex-race
  current <- direct_estimates[marital == 9 & edu == 9 & !(age %in% c(98)) & level == "natl" & svyyear == max(svyyear)]
  print(ggplot(data = current) + theme_bw() +
          geom_smooth(aes(x = age, y = get(outcome), group = factor(race), color = factor(race)), se = FALSE, size = 0.5) +
          geom_point(aes(x = age, y = get(outcome), color = factor(race), size = sample_size), alpha = 0.25) +
          labs(x = "Age", y = "Prevalence", title=paste0("Prevalence of ", outcome, " (Direct Estimates from NHANES), National"), subtitle = paste0("All years (weighted by population).\nAll education and marital status categories.")) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) +
          scale_color_discrete(name = "Race", labels = race_labels) + scale_linetype_discrete(name = "Sex", labels = sex_labels) + scale_size_continuous(name = "Sample Size") +
          facet_grid(~ sex))
  dev.off()
}
