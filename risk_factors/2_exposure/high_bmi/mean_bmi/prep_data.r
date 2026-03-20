####################################################################################################
## Description: Prep data for INLA model and diagnostics
##              
##              Uses settings code from launch_risks.R
##                
## Input: Cross-walked data with mean BMI estimate aggregated to the state level (BRFSS)
##  
## Output: "/combined_dt.RDS": state-level model input for INLA model stratified by age, sex, race
##
####################################################################################################
# set up ------------------------------------------------------------------
library(data.table)

###### Source functions
repo <- paste0("FILEPATH")
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(repo, "/functions/", func)))
}

if (!interactive()) {
  (output_dir <- commandArgs(TRUE)[[1]])
  (settings_file <- commandArgs(TRUE)[[2]])
} else {
  settings_file <- 'mean_bmi_model_84_v6'
}

###### Get mcnty:state mapping
locs <- fread("FILEPATH")

##### Set data location and settings locations, and read in settings
settings_path <- "FILEPATH"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc)


############ load state-level BRFSS direct estimates -- for FITTING MODEL
message("load BRFSS data")
get_settings(paste0(in_overweight_est, "/settings.csv"))
brfss_data_file_overwt <- direct_estimates_file[name='BRFSS']

if (isTRUE(include_underwt)) {
  get_settings(paste0(in_underweight_est, "/settings.csv"))
  brfss_data_file_underwt <- direct_estimates_file[name='BRFSS']
}

get_settings(paste0(in_obese_est, "/settings.csv"))
brfss_data_file_obese <- direct_estimates_file[name='BRFSS']

if (isTRUE(include_underwt)) {
  if (brfss_data_file_overwt != brfss_data_file_underwt) {
    stop('direct estimate files different in overwt and underwt prevalence models!!')
  }
}

if (brfss_data_file_overwt != brfss_data_file_obese) {
  stop('direct estimate files different in overwt and obese prevalence models!!')
} 

message('load mean_bmi model settings again')
get_settings(settings_loc)

combined_dt <- readRDS(direct_estimates_file[name='BRFSS'])

# subset to desired strata
combined_dt <- combined_dt[!is.na(race) & !is.na(age) & !is.na(sex) & !is.na(year) & age %in% ages & race != -1 & sex != -1 & race != -1 & age != -1 & year != -1]
combined_dt <- combined_dt[race != 1 & age < 98 & sex != 3 & level == "state"]

#### Set certain variables to factors
combined_dt$sex <- as.factor(combined_dt$sex)
combined_dt$race <- as.factor(combined_dt$race)
combined_dt$age <- as.factor(combined_dt$age)

message('combine ages 80+ in BRFSS')
combined_dt_80 <- combined_dt[age %in% 80:85]

combined_dt_80 <- combined_dt_80[, list(age = 80, obese_1 = weighted.mean(obese_1, weights, na.rm = TRUE),
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
                                        weights = sum(weights, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE),
                                        pop = sum(pop, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE)),
                                 by = list(state, state_name, year, sex, race, level)]

combined_dt <- rbindlist(list(combined_dt[!(age %in% 80:85),], combined_dt_80), use.names = T, fill = TRUE)

# race_all should be specified in settings file.
if (race_code_set =='old') {
  stopifnot(race_all == 9)
} else if (race_code_set =='db') {
  stopifnot(race_all == 1)
}

###### Resort again
setkeyv(combined_dt, c("year", "sex", "race", "age", "state"))

message('save recodes')
recodes <- list()

#### Recode vars to start from 1
# Race
combined_dt[, race_recode := as.integer(factor(race, levels = c(races)))] # I think this makes race_recode NA for the all-race category
recodes$race <- unique(combined_dt[, .(var = "race", race, race_recode)])
combined_dt[, c("race", "race_recode") := list(race_recode, NULL)]

# Sex
combined_dt[, sex_recode := ifelse(sex != 3, as.integer(factor(sex, levels = c(sexes))), 3)]
recodes$sex <- unique(combined_dt[, .(var = "sex", sex, sex_recode)])
combined_dt[, c("sex", "sex_recode") := list(sex_recode, NULL)]

# Year
if ("year" %in% colnames(combined_dt)) {
  combined_dt[, year_recode := ifelse(year != -1, as.integer(factor(year, levels = c(years))), -1)]
  recodes$year <- unique(combined_dt[, .(var = "year", year, year_recode)])
  combined_dt[, c("year", "year_recode") := list(year_recode, NULL)]
}

#### Create a state-race variable
state_races <- data.table(expand.grid(state = unique(locs$state), race = recodes$race$race_recode))
state_races[, state_race_string := paste0(state, "_", race)]
state_races[, state_race_recode := .I]
combined_dt[, state_race_string := paste0(state, "_", race)]
combined_dt <- merge(combined_dt, state_races[, c("state_race_string", "state_race_recode")], by = "state_race_string", all.x = TRUE)
setnames(combined_dt, "state_race_recode", "state_race")
recodes$state_race <- state_races[, var := "state_race"]

# Age
combined_dt[, age_recode := ifelse(age != 98, ifelse(age != 99, as.integer(factor(age, levels = c(ages))), 99), 98)]
recodes$age <- data.table(cbind.data.frame(var = "age", "age" = c(as.integer(ages)), "age_recode" = c(as.integer(factor(c(ages), levels = c(ages))))))
combined_dt[, c("age", "age_recode") := list(age_recode, NULL)]

###### Area (mcnty) and state
if (exists("states")) {
  if ((length(states) > 0) & states[1] != "all") { # Specific states were requested
    area_levels <- unique(locs[state_name %in% states, mcnty])
    state_levels <- unique(locs[state_name %in% states, state])
  } else {
    state_levels <- unique(locs$state)
  }
}

if (!exists("area_levels")) {
  area_levels <- unique(locs$mcnty)
}

if (!exists("state_levels")) {
  state_levels <- unique(locs$state)
}

recodes$area <- data.table(cbind.data.frame(var = "mcnty", "area" = as.integer(area_levels), "area_recode" = as.integer(factor(area_levels, levels = area_levels))))
setnames(recodes$area, "area", "mcnty")
recodes$state <- data.table(cbind.data.frame(var = "state", "state" = as.integer(state_levels), "state_recode" = as.integer(factor(state_levels, levels = state_levels))))

###### Save recoding file
saveRDS(recodes, file = paste0(output_dir, "/recode.rds"))

if ("state" %in% colnames(combined_dt)) {
  combined_dt <- merge(combined_dt, recodes$state[, .(state, state_recode)], by = "state", all.x = T)
  combined_dt[, c("state", "state_recode") := list(state_recode, NULL)]
}

###### Create obesity proportion variable
for (i in 1:n.imp) {
  col0 <- paste0("obese_prop_", i)
  col1 <- paste0("obese_", i)
  col2 <- paste0("overwt_", i)
  combined_dt[, eval(col0) := get(col1) / get(col2)]
}

#### Create vars for overwt_not_obese
for (i in 1:n.imp) {
  col0 <- paste0("overwt_not_obese_", i)
  col1 <- paste0("obese_", i)
  col2 <- paste0("overwt_", i)
  combined_dt[, eval(col0) := get(col2) - get(col1)]
}

###### Discretize obese and overwt
library(INLA)
obese_discretized <- inla.group(unlist(combined_dt[!(obese_1 %in% c(0, 1)), obese_1]), n = 23, method = "quantile")
obese_categories <- c(0, sort(unique(obese_discretized)), 1)
obese_categories_cut <- sort(unique(cut(unlist(combined_dt[!(obese_1 %in% c(0, 1)), obese_1]), breaks = obese_categories, include.lowest = TRUE, ordered_result = TRUE)))
obese_categories_cut <- data.table(label = obese_categories_cut, cat = 1:24)
for (i in 1:10) {
  combined_dt[, paste0("obese_category_", i) := cut(unlist(combined_dt[, paste0("obese_", i), with = FALSE]), breaks = obese_categories, include.lowest = TRUE, labels = FALSE)]
}

overwt_discretized <- inla.group(unlist(combined_dt[!(overwt_1 %in% c(0, 1)), overwt_1]), n = 23, method = "quantile")
overwt_categories <- c(0, sort(unique(overwt_discretized)), 1)
overwt_categories_cut <- sort(unique(cut(unlist(combined_dt[!(overwt_1 %in% c(0, 1)), overwt_1]), breaks = overwt_categories, include.lowest = TRUE, ordered_result = TRUE)))
overwt_categories_cut <- data.table(label = overwt_categories_cut, cat = 1:24)
for (i in 1:10) {
  combined_dt[, paste0("overwt_category_", i) := cut(unlist(combined_dt[, paste0("overwt_", i), with = FALSE]), breaks = overwt_categories, include.lowest = TRUE, labels = FALSE)]
}

obese_prop_discretized <- inla.group(unlist(combined_dt[!(obese_prop_1 %in% c(0, 1)), obese_prop_1]), n = 23, method = "quantile")
obese_prop_categories <- c(0, sort(unique(obese_prop_discretized)), 1)
obese_prop_categories_cut <- sort(unique(cut(unlist(combined_dt[!(obese_prop_1 %in% c(0, 1)), obese_prop_1]), breaks = obese_prop_categories, include.lowest = TRUE, ordered_result = TRUE)))
obese_prop_categories_cut <- data.table(label = obese_prop_categories_cut, cat = 1:24)
for (i in 1:10) {
  combined_dt[, paste0("obese_prop_category_", i) := cut(unlist(combined_dt[, paste0("obese_prop_", i), with = FALSE]), breaks = obese_prop_categories, include.lowest = TRUE, labels = FALSE)]
}

overwt_not_obese_discretized <- inla.group(unlist(combined_dt[!(overwt_not_obese_1 %in% c(0, 1)), overwt_not_obese_1]), n = 23, method = "quantile")
overwt_not_obese_categories <- c(0, sort(unique(overwt_not_obese_discretized)), 1)
overwt_not_obese_categories_cut <- sort(unique(cut(unlist(combined_dt[!(overwt_not_obese_1 %in% c(0, 1)), overwt_not_obese_1]), breaks = overwt_not_obese_categories, include.lowest = TRUE, ordered_result = TRUE)))
overwt_not_obese_categories_cut <- data.table(label = overwt_not_obese_categories_cut, cat = 1:24)
for (i in 1:10) {
  combined_dt[, paste0("overwt_not_obese_category_", i) := cut(unlist(combined_dt[, paste0("overwt_not_obese_", i), with = FALSE]), breaks = overwt_not_obese_categories, include.lowest = TRUE, labels = FALSE)]
}

save(obese_categories, obese_categories_cut, overwt_categories, overwt_categories_cut, obese_prop_categories, obese_prop_categories_cut, overwt_not_obese_categories, overwt_not_obese_categories_cut, file = paste0(output_dir, "/weight_categories.RData"))

saveRDS(combined_dt, paste0(output_dir, "/combined_dt.RDS"))

message('SCRIPT COMPLETED')
