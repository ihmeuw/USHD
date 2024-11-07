##############################################################################################################################
## Description: Re-format the dataset we get back from the CoD pipeline to be prepped for both all-cause
##              and cause-specific models.
##Input:
##      --data: raw aggregation data for the indicated year/nid
## Output:
##      --deaths: a cleaned data.table
##############################################################################################################################



clean_rdp <- function(data, by_race = F){
  
  library(data.table)
  library(stringr)
  
  # Load shared functions
  shared_function_dir <- "FILEPATH"
  
  source(paste0(shared_function_dir, "get_age_metadata.R"))
  source(paste0(shared_function_dir, "get_cause_metadata.R"))
  
  # Get cause and age data
  message("Getting cause and age data")
  ages <- get_age_metadata(age_group_set_id = 12, gbd_round_id = 6)
  causes <- get_cause_metadata(cause_set_id = 4, gbd_round_id = 6)
  reporting_causes <- get_cause_metadata(cause_set_id = 3, gbd_round_id = 6)
  secret_causes <- setdiff(causes$cause_id, reporting_causes$cause_id)
  causes[, secret_cause := ifelse(cause_id %in% secret_causes, 1, 0)]     # add secret cause indicator
  causes <- causes[, list(acause, cause_id, level, parent_id, secret_cause, yld_only)]

  # Make sure all the causes in the dataset are also in the cause list
  data[causes, on = "cause_id", acause := i.acause]         # raw data only has cause ids, so need to add acause
  cause_diff <- setdiff(data$acause, causes$acause)
  if (length(cause_diff) != 0){
    warning(paste("The following causes exist in the deaths data but not the cause list:", cause_diff, "\n"))
  }

  # Map cause information to data
  message("Adding cause information to data")
  setkeyv(data, c("acause", "cause_id"))
  setkeyv(causes, c("acause", "cause_id"))
  data <- merge(data, causes, all.x = T)

  # Drop secret and YLD only causes
  message("Dropping secret and YLD only causes")
  data <- data[secret_cause == 0 & is.na(yld_only)]
  
  # Add level 1 deaths up to get all-cause counts (not included in aggregation output*)
  # * technically included but with a negligible total death count (e.g., 0.07 all-cause deaths in 2010)
  allcause <- data[level %in% c(0, 1), list(acause = "_all", cause_id = 294, level = 0, deaths = sum(deaths)), 
                   by = c("age_group_id", "location_id", "nid", "sex_id", "year_id", "parent_id", 
                          "secret_cause", "yld_only")]
  data <- data[level != 0]  # we will have duplicates if we keep the all-cause deaths originally included
  data <- rbind(allcause, data); rm(allcause)
  
  # Get complete death count, for checking later
  full_count <- sum(data$deaths)

  # Rename column identifying race/ethnicity
  if (by_race) {
    setnames(data, "subdiv", "race")
    data[, race := factor(race, levels = c("non_hispanic_white", "non_hispanic_black", "non_hispanic_other", "hispanic"))]
    data[, race := as.integer(factor(race))] # convert to integer 1:4
  }

  message("Eliminating zeroes")
  message("There are ", nrow(data[deaths == 0]), " instances where deaths = 0")
  data <- data[deaths > 0]
  
  # Merge age values into data
  message("Adding age values")
  data[ages, on = "age_group_id", age := round(i.age_group_years_start, 2)]       # get first number in each age group range

  
  # ICD Corrections: We're doing this because CoD makes this switch to their own data
  # after the CoD pipeline process, so we're doing it to align with them. These corrections
  # were updated for GBD2019. We need to check for changes to these corrections for GBD2020+.
  # There are also similar corrections in `02_reallocate_parent_deaths` to make sure deaths
  # are not reallocated to these acause-ages.
  # Note from most recent update: only drug and opioid corrections were made across all years, so that's all we are leaving for now.
  message("Making post-pipeline deaths corrections")
  
  # drug deaths into "unintentional poisoning" for age < 15, except mental_opioids where you allow deaths in age 0
  num_poison <- sum(data[acause == "inj_poisoning", deaths])
  data[(acause == "mental_alcohol" | acause == "mental_drug" | (parent_id == 561 & acause != "mental_drug_opioids")) & age < 15, 
       c("acause", "cause_id", "level") := list("inj_poisoning", causes[acause == "inj_poisoning", cause_id], causes[acause == "inj_poisoning", level])]
  if (sum(data[acause == "inj_poisoning", deaths]) > num_poison) {
    message(round(sum(data[acause == "inj_poisoning", deaths]) - num_poison, 2), " drug deaths were recoded to unintentional poisoning")
  }
  
  num_opioids <- sum(data[acause == "mental_drug_opioids", deaths])
  data[acause == "mental_drug_opioids" & age >= 0.02 & age < 15, 
       c("acause", "cause_id", "level") := list("inj_poisoning", causes[acause == "inj_poisoning", cause_id], causes[acause == "inj_poisoning", level])]
  if (sum(data[acause == "mental_drug_opioids", deaths]) < num_opioids) {
    message(round(num_opioids - sum(data[acause == "mental_drug_opioids", deaths]), 2), " opioid deaths were recoded to unintentional poisoning")
  }
  
  
  # Collapse NN to single 0-1 age group and 85-95+ to just 85+
  message("Collapsing neonatal ages to 0-1 and 85-95+ to 85+")
  data[, age := ifelse(age < 1, 0, age)]
  data[, age := ifelse(age >= 85, 85, age)]
  data[age == 0, age_group_id := 28]      # age group id for 0-1 is 28
  data[age == 85, age_group_id := 160]    # age group id for 85+ is 160

  # Aggregate after collapsing age groups
  message("Re-aggregating after collapsing age groups")
  data <- data[, list(deaths = sum(deaths)), by = c("year_id", "nid", "location_id", "sex_id", "age_group_id", "age", "level", "cause_id",
                                                    "acause", if (by_race) "race")]

  # Check to make sure we didn't lose any deaths
  message("Checking death counts")
  new_count <- sum(data$deaths)
  if (abs(full_count - new_count) > 0.005){
    stop(paste("Deaths mismatch of ", abs(full_count-new_count), "!"))
  }

  # Final formatting
  message("Finishing formatting")
  setorderv(data, c("year_id", "nid", "level", "cause_id", "acause", "location_id", "sex_id", "age_group_id", "age", if (by_race) "race"))
  setkeyv(data, "cause_id")
  return(data)

}

