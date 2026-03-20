####################################################################################################
## Description: Generate out-of-sample predictions from INLA model (fit at the state level) 
##                Apply parameters to mcnty-level prevalence of overweight and obesity 
##                estimates from SAE model to predict mean BMI at the mcnty-level 
##              
##              Uses settings code from launch_risks.R
####################################################################################################

# Maybe I can pull the  model matrix from the model fit, and compare to what we created (with the exception 
#   that the model fit from INLA does not include age 14
#     #maybe this would be helpful: https://becarioprecario.bitbucket.io/inla-gitbook/ch-INLAfeatures.html
#     
# I haven't found a simple way to get the prediction/model matrix from INLA,
#   but I think it could try to compare the column names of predictors to 
#   column names 

if (!interactive()) {
  (output_dir_draws_est <- commandArgs(TRUE)[[1]])
  (settings_file <- commandArgs(TRUE)[[2]])
  (s <- commandArgs(TRUE)[[3]])
  (r <- commandArgs(TRUE)[[4]])
  (e <- commandArgs(TRUE)[[5]])
  (yr <- commandArgs(TRUE)[[6]])
  (output_dir <- commandArgs(TRUE)[[7]])
} else {
  s <- 2
  r <- 6
  e <- 1
  yr <- 2006
}

# set up ------------------------------------------------------------------
library(data.table)
library(mvtnorm)

###### Source functions
nonfatal_repo <- paste0("FILEPATH")
funcs <- list.files(paste0(nonfatal_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(nonfatal_repo, "/functions/", func)))
}

start.time <- Sys.time()
##### Set data location and settings locations, and read in settings
settings_path <- "FILEPATH"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc)

if (!exists("imp_for_preds")) {
  imp_for_preds <- 0
} else if (imp_for_preds == "NULL" | is.null(imp_for_preds) | is.na(imp_for_preds)) {
  imp_for_preds <- 0
}

imp_pred_string <- paste0("imputation", imp_for_preds)

testing <- FALSE

#### Load FIPS:merged county reference table
locs <- fread("FILEPATH")
models <- setDT(read.csv('FILEPATH'))

combined_dt <- readRDS(paste0(output_dir, "/combined_dt.RDS"))

####################################################
message('Import sae model draws')
draws_file <- paste0("draws_mcnty_", yr, "_", s, "_", r, "_", e, ".rds")
draws_file_raked <- paste0("draws_mcnty_", yr, "_", s, "_", r, "_", e, "_raked.rds")

if (raked_overweight == 0) {
  draws_overwt <- readRDS(paste0(in_overweight_est, "/", imp_pred_string, "/draws/", draws_file))
} else if (raked_overweight == 1) {
  draws_overwt <- readRDS(paste0(in_overweight_est, "/", imp_pred_string, "/draws/", draws_file_raked))[, -c("edu", "acause")]
} else {
  message('specify raking status - overweight!')
}

draws_overwt <- draws_overwt[age < 98]
setnames(draws_overwt, c('area', 'pred'), c('mcnty', 'overwt'))
draws_overwt[, c('level') := NULL]

if (raked_obesity == 0) {
  draws_obese <- readRDS(paste0(in_obese_est, "/", imp_pred_string, "/draws/", draws_file))
} else if (raked_obesity == 1) {
  draws_obese <- readRDS(paste0(in_obese_est, "/", imp_pred_string, "/draws/", draws_file_raked))[, -c("edu", "acause")]
} else {
  message('specify raking status - obesity!')
}

setnames(draws_obese, c('area', 'pred'), c('mcnty', 'obese'))
draws_obese <- draws_obese[age < 98]
draws_obese <- merge(draws_obese, unique(locs[,c('mcnty', 'state')]), by = 'mcnty')
draws_obese[, c('level') := NULL]

message('Merge sae model draws')
if (exists("gold_standard_source")) {
  if (!is.null(draws_obese$source_v2)) {
    draws <- merge(draws_obese[source_v2 == gold_standard_source, -c("source_v2")], 
                   draws_overwt[source_v2 == gold_standard_source, -c("source_v2")], 
                   by = c('year', 'mcnty', 'age', 'sex', 'race', 'sim'))  
  } else {
    draws <- merge(draws_obese, draws_overwt, by = c('year', 'mcnty', 'age', 'sex', 'race', 'sim'))
  }
} else {
  draws <- merge(draws_obese, draws_overwt, by = c('year', 'mcnty', 'age', 'sex', 'race', 'sim'))
}

if (isTRUE(include_underwt)) {
  draws_underwt <- readRDS(paste0(in_underweight_est, "/draws/", draws_file))
  draws_underwt <- draws_underwt[age < 98]
  setnames(draws_underwt, c('area', 'pred'), c('mcnty', 'underwt'))
  draws_underwt[, c('level') := NULL]
  draws <- merge(draws, draws_underwt, by = c('year', 'mcnty', 'age', 'sex', 'race', 'sim'))
  rm(draws_underwt)
} else {
  'underwt not included'
}

setnames(draws, 'sim', 'sample')
rm(draws_overwt, draws_obese)

if (isTRUE(testing)) {
  draws <- draws[sample == 1]
}

recodes <- readRDS(paste0(output_dir, "/recode.rds"))

if (models[model == mean_model]$formula %like% "state_race") {
  draws[, state_race_string := paste0(state, "_", as.integer(factor(race, levels = races)))]
  draws <- merge(draws, recodes$state_race[, c("state_race_string", "state_race_recode")], by = "state_race_string", all.x = TRUE)
  setnames(draws, "state_race_recode", "state_race")
}

####################################################
message('Save copy of draws')

###############################################################################
### Note: use mod$summary.random to check ordering of variables in interaction terms
###############################################################################

#### Set order
setkeyv(draws, c("mcnty", "age", "sample"))
copy_draws <- copy(draws) #to use later to reverse dummy coding
setkeyv(copy_draws, c("mcnty", "age", "sample"))

# Reindex vars
races <- factor(races)
message('Reformat draws')
draws[, c('state', 'age') := list(as.integer(as.factor(state)), as.integer(as.factor(age)))]
draws[, year := as.integer(factor(year, levels = years))]
draws[, race := as.integer(factor(race, levels = races))]

copy_draws[, c('state_recode', 'age_recode') := list(as.integer(as.factor(state)), as.integer(as.factor(age)))]
copy_draws[, year_recode := as.integer(factor(year, levels = years))]
copy_draws[, race_recode := as.integer(factor(race, levels = races))]

formula <- models[model == mean_model]$formula

if (!use_gam) {
  #### Rename columns to match effect_draws
  char_cols <- NULL
  
  ## Year
  if ((formula %like% "year, model = 'ar', order = 2") & !((formula %like% "year, model = 'ar', order = 2, replicate = race") | (formula %like% "year, model = 'ar', order = 1, group = race") | (formula %like% "year, model = 'ar', order = 1, replicate = race") | (formula %like% "year, model = 'ar', order = 2, replicate = state_race"))) {
    char_cols <- c('year', char_cols)
  }
  
  if (formula %like% "race," & !(formula %like% "group = race,")) {
    char_cols <- c('race', char_cols)
  }
  
  load(paste0(output_dir, "/weight_categories.RData"))
  
  if (formula %like% "obese_category, model = 'rw1', group = race2" | formula %like% "obese_category, model = 'rw2', replicate = race2") {
    draws[, paste0("obese_cat_data") := cut(unlist(draws[, paste0("obese"), with = FALSE]), breaks = obese_categories, include.lowest = TRUE, labels = FALSE)]
    char_cols <- c("obese_cat_data", char_cols)
  }
  
  if (formula %like% "overwt_category, model = 'rw1', group = race3" | formula %like% "overwt_category, model = 'rw2', replicate = race3") {
    draws[, paste0("overwt_cat_data") := cut(unlist(draws[, paste0("overwt"), with = FALSE]), breaks = overwt_categories, include.lowest = TRUE, labels = FALSE)]
    char_cols <- c("overwt_cat_data", char_cols)
  }
  
  if (formula %like% "obese_prop_category, model = 'rw2', replicate = race2") {
    #### Calculate proportion obese
    draws[, obese_prop := obese / overwt]
    
    draws[, paste0("obese_prop_cat_data") := cut(unlist(draws[, paste0("obese_prop"), with = FALSE]), breaks = obese_prop_categories, include.lowest = TRUE, labels = FALSE)]
    char_cols <- c("obese_prop_cat_data", char_cols)
    
    draws[, obese_prop := NULL]
  }
  
  if (formula %like% "overwt_not_obese_category, model = 'rw2', replicate = race3") {
    #### Calculate overweight not obese
    draws[, overwt_not_obese := overwt - obese]
    
    draws[, paste0("overwt_not_obese_cat_data") := cut(unlist(draws[, paste0("overwt_not_obese"), with = FALSE]), breaks = overwt_not_obese_categories, include.lowest = TRUE, labels = FALSE)]
    char_cols <- c("overwt_not_obese_cat_data", char_cols)
    
    draws[, overwt_not_obese := NULL]
  }
  
  message('Dummy coding')
  
  # dummy code age and state columns (to match effect draws)
  for (col_name in char_cols) {
    if (col_name == "obese_cat_data") {
      unique_vals <- 1:(length(obese_categories) - 1)
    } else if (col_name == "overwt_cat_data") {
      unique_vals <- 1:(length(overwt_categories) - 1)
    } else if (col_name == "obese_prop_cat_data") {
      unique_vals <- 1:(length(obese_prop_categories) - 1)
    } else if (col_name == "overwt_not_obese_cat_data") {
      unique_vals <- 1:(length(overwt_not_obese_categories) - 1)
    } else {
      unique_vals <- unique(draws[[col_name]])
    }
    unique_vals <- stringr::str_sort(unique_vals, na_last = TRUE, locale = "en_US", numeric = TRUE)
    unique_vals <- as.character(unique_vals)
    #create dummy vars - all 0s
    draws[, paste0(col_name, ":", unique_vals)] <- 0L
    #fill-in dummy vars
    for (unique_value in unique_vals) {
      message(col_name, unique_value)
      data.table::set(draws, i = which(data.table::chmatch(as.character(draws[[col_name]]), unique_value, nomatch = 0) == 1L),
                      j = paste0(col_name, ":", unique_value),
                      value = 1L)
    }
  }
  
  #### Generate interactions
  if ((formula %like% "as.integer\\(year\\), model = 'ar1', group = race") | (formula %like% "year, model = 'ar', order = 2, replicate = race") | (formula %like% "year, model = 'ar', order = 1, group = race") | (formula %like% "year, model = 'ar', order = 1, replicate = race")) {
    #subset to specific year
    unique_vals <- 1:(length(years)*length(races))
    vars <- paste0("year:", c(split(unique_vals, ceiling(seq_along(unique_vals)/ length(years)))[[which(races %in% r)]]))
    vars <- vars[which(years %in% yr)]
    draws[, `:=`((vars), 1L)]
    yrs_col <- paste0("year:", unlist(split(unique_vals, ceiling(seq_along(unique_vals)/ length(years)))))
    yrs_col <- yrs_col[!(yrs_col %in% vars)]
  }
  
  if (formula %like% 'race, overwt') {
    draws[, paste0('race:', which(races %in% r)) := overwt]
  }
  
  if (formula %like% 'race3, overwt') {
    draws[, paste0('race3:', which(races %in% r)) := overwt]
  }
  
  if (formula %like% 'overwt +') {
    setnames(draws, 'overwt', 'overwt:1', skip_absent = TRUE)
  }
  draws[, overwt := NULL]
  
  if (formula %like% 'race2, obese') {
    draws[, paste0('race2:', which(races %in% r)) := obese]
  }
  
  if (formula %like% 'obese +' | like(formula, '+ obese', fixed = TRUE)) {
    setnames(draws, 'obese', 'obese:1', skip_absent = TRUE)
  }
  draws[, obese := NULL]
  
  # rename remaining columns to match effects_draws
  if (!formula %like% 'race,' & (formula %like% 'f\\(race,')) {
    setnames(draws, c('race'), c(paste0('race:', which(races %in% r))), skip_absent = TRUE)
  }
  
  if (formula %like% "race2, model = 'iid', group = obese_category") {
    #subset to specific race
    unique_vals <- 1:(length(races)*nrow(obese_categories_cut))
    vars <- paste0("race2:", c(split(unique_vals, ceiling(seq_along(unique_vals)/ nrow(obese_categories_cut)))[[which(races %in% r)]]))
    draws[, `:=`((vars), 1L)]
    races2_col <- paste0("race2:", 1:length(unique_vals))
    races2_col <- races2_col[!(races2_col %in% vars)]
  }
  
  if (formula %like% "race3, model = 'iid', group = overwt_category") {
    #subset to specific race
    unique_vals <- 1:(length(races)*nrow(overwt_categories_cut))
    vars <- paste0("race3:", c(split(unique_vals, ceiling(seq_along(unique_vals)/ nrow(overwt_categories_cut)))[[which(races %in% r)]]))
    draws[, `:=`((vars), 1L)]
    races3_col <- paste0("race3:", 1:length(unique_vals))
    races3_col <- races3_col[!(races3_col %in% vars)]
  }
  
  if (formula %like% "obese_category, model = 'rw1', group = race2" | formula %like% "obese_category, model = 'rw2', group = race2" | formula %like% "obese_category, model = 'rw2', replicate = race2") {
    #subset to specific race
    unique_vals <- 1:(length(races)*nrow(obese_categories_cut))
    vars <- paste0("obese_category:", c(split(unique_vals, ceiling(seq_along(unique_vals)/nrow(obese_categories_cut)))[[which(races %in% r)]]))
    setnames(draws, c(paste0("obese_cat_data:", obese_categories_cut$cat)), vars)
    races2_col <- paste0("obese_category:", 1:length(unique_vals))
    races2_col <- races2_col[!(races2_col %in% vars)]
  }
  
  if (formula %like% "overwt_category, model = 'rw1', group = race3" | formula %like% "overwt_category, model = 'rw2', replicate = race3") {
    #subset to specific race
    unique_vals <- 1:(length(races)*nrow(overwt_categories_cut))
    vars <- paste0("overwt_category:", c(split(unique_vals, ceiling(seq_along(unique_vals)/nrow(overwt_categories_cut)))[[which(races %in% r)]]))
    setnames(draws, c(paste0("overwt_cat_data:", overwt_categories_cut$cat)), vars)
    races3_col <- paste0("overwt_category:", 1:length(unique_vals))
    races3_col <- races3_col[!(races3_col %in% vars)]
  }
  
  if (formula %like% "obese_prop_category, model = 'rw2', replicate = race2") {
    #subset to specific race
    unique_vals <- 1:(length(races)*nrow(obese_prop_categories_cut))
    vars <- paste0("obese_prop_category:", c(split(unique_vals, ceiling(seq_along(unique_vals)/nrow(obese_prop_categories_cut)))[[which(races %in% r)]]))
    setnames(draws, c(paste0("obese_prop_cat_data:", obese_prop_categories_cut$cat)), vars)
    races2_col <- paste0("obese_prop_category:", 1:length(unique_vals))
    races2_col <- races2_col[!(races2_col %in% vars)]
  }
  
  if (formula %like% "overwt_not_obese_category, model = 'rw2', replicate = race3") {
    #subset to specific race
    unique_vals <- 1:(length(races)*nrow(overwt_not_obese_categories_cut))
    vars <- paste0("overwt_not_obese_category:", c(split(unique_vals, ceiling(seq_along(unique_vals)/nrow(overwt_not_obese_categories_cut)))[[which(races %in% r)]]))
    setnames(draws, c(paste0("overwt_not_obese_cat_data:", overwt_not_obese_categories_cut$cat)), vars)
    races3_col <- paste0("overwt_not_obese_category:", 1:length(unique_vals))
    races3_col <- races3_col[!(races3_col %in% vars)]
  }
  
  draws[, c('(Intercept):1') := c(1)]
  
  # drop original columns after dummy coding
  if (!is.null(char_cols)) {
    draws[, c(char_cols[!char_cols %in% c("race", "year")]) := NULL]
  }
  
  message('------------------------------')
  message('Import effect_draws')
  effects_draws <- rbindlist(lapply(1:n.imp, function(imp) { 
    message(imp)
    if (by_sex) {
      effects_imp <- readRDS(paste0(output_dir_draws_est, "/effects_draws", imp, "sex", s, ".RDS"))  
    } else {
      effects_imp <- readRDS(paste0(output_dir_draws_est, "/effects_draws", imp, "sex", 0, ".RDS"))  
    }
    
    effects_imp <- as.data.table(t(effects_imp))
    return(effects_imp)
  }), fill = TRUE)
  
  message('Reformat effect_draws')
  effects_draws[, sample := 1:nrow(effects_draws)]
  
  if (isTRUE(testing)) {
    effects_draws <- effects_draws[sample %in% c(1)]
  }
  
  # # exclude irrelevant betas - vectors will be empty when not needed
  # # these betas correspond to age/sex/year/race columns that draws are subsetted by 
  races_col <- grep('race:', colnames(effects_draws), value = T)
  races_col <- races_col[races_col != paste0('race:', which(races %in% r))]

  if (!exists("yrs_col")) {
    yrs_col <- grep('year:', colnames(effects_draws), value = T)
    yrs_col <- yrs_col[yrs_col != paste0('year:', which(years %in% yr))]
  }
  
  effects_draws[, c(yrs_col, races_col, races2_col, races3_col) := NULL]
  
  #### Drop source effects, if present
  if ("ll_only:1" %in% colnames(effects_draws)) {
    effects_draws[, "ll_only:1" := NULL]
  }
  
  combined_dt <- combined_dt[!is.na(race) & !is.na(sex) & !is.na(age)] #exclude age 98/99. Do NOT include race != all_race b/c all_race is 1, but race now refers to the race variables indexed to 1 (so all_race is now NA) 
  combined_dt <- combined_dt[sex == s]
  
  # Check that have same columns in parameter and model matrices 
  # Note that this check does NOT ensure that the columns are actually equivalent, although it 
  #   will flag major issues
  
  stopifnot(setequal(str_sort(ls(effects_draws), numeric = TRUE), str_sort(ls(draws[, -c("mcnty", "sex", "state", "year", "age", "race")]), numeric = TRUE)))
  
  #set same column order
  setcolorder(effects_draws, colnames(draws[, -c("mcnty", "sex", "state", "year", "age", "race")]))
  
  draws <- draws[sample %in% 1:(n.sims * n.imp)]
  
  message('Multiply draws * effect_draws')
  cols <- colnames(effects_draws[, !'sample'])
  if (isTRUE(testing)) {
    i <- 1
    draws_all <- as.data.table(mapply("*", draws[sample == i, ..cols], effects_draws[sample == i, ..cols]))
  } else {
    draws_all <- rbindlist(lapply(1:1000, function(i) {
      multiply <- cbind(as.data.table(mapply("*", draws[sample == i, ..cols], effects_draws[sample == i, ..cols])), draws[sample == i, which(!(colnames(draws) %in% cols)), with = FALSE])
      return(multiply)
    }))
  }
  
  message('calculate predictions')
  if (formula %like% "log\\(pred_bmi\\)") {
    draws_all[, pred := exp(rowSums(draws_all[, which(colnames(draws_all) %in% cols), with = FALSE]))] 
  } else {
    draws_all[, pred := rowSums(draws_all[, which(colnames(draws_all) %in% cols), with = FALSE])]
  }
} else if (use_gam) {
  message('Import models')
  draws_all <- rbindlist(lapply(1:n.imp, function(imp) {
    library(mgcv)
    message(imp)
    if (by_sex) {
      mod <- readRDS(paste0(output_dir_draws_est, "/mod", imp, "sex", s, ".RDS"))  
    } else {
      mod <- readRDS(paste0(output_dir_draws_est, "/mod", imp, "sex", 0, ".RDS"))  
    }
    
    preds <- cbind(draws[sample %in% ((imp - 1) * n.sims + 1):(imp * n.sims)], pred = exp(predict.gam(mod, draws[sample %in% ((imp - 1) * n.sims + 1):(imp * n.sims)])))
    
    return(preds)
  }), fill = TRUE)
}

message('Reverse coding of stratum indicators')
draws_all <- merge(draws_all, unique(copy_draws[, c("state", "state_recode")]), by.x = "state", by.y = "state_recode", all.x = TRUE)
draws_all <- merge(draws_all, unique(copy_draws[, c("age", "age_recode")]), by.x = "age", by.y = "age_recode", all.x = TRUE)
draws_all <- merge(draws_all, unique(copy_draws[, c("year", "year_recode")]), by.x = "year", by.y = "year_recode", all.x = TRUE)
draws_all <- merge(draws_all, unique(copy_draws[, c("race", "race_recode")]), by.x = "race", by.y = "race_recode", all.x = TRUE)
draws_all[, c("year", "age", "state", "race", "year.y", "age.y", "state.y", "race.y") := list(year.y, age.y, state.y, race.y, NULL, NULL, NULL, NULL)]

setnames(draws_all, c('mcnty', 'sample'), c('area', 'sim'))
draws_all[, level := 'mcnty']

if (isTRUE(testing)) {
  summary(draws_all$pred)
}

if (isFALSE(testing)) {
  message(paste('Aggregate ages', yr))
  # load the age standard
  std_wt <- readRDS(age_std_file)[, list(age, wt = wt / sum(wt))]
  
  # load and subset the population file, then merge onto draws
  pop <- readRDS(pop_file)
  
  setnames(pop, area_var, "area")
  draws_dt <- merge(draws_all, pop[, !'state'], by = c("area", "year", "sex", "race", "age"))
  
  # add crude and age-standardized rates
  message(paste('Save draws', yr))
  draws_dt <- calc_all_ages(draws_dt, std_wt, "pred", c("area", "year", "sex", "race", "sim", "level"))
  
  saveRDS(draws_dt, file = paste0(output_dir_draws_est, "/draws/draws_mcnty_", yr, "_", s,"_", r, "_", e, ".rds"))
  
  message(paste('Save estimates', yr))
  est_dt <- collapse_draws(draws_dt, "pred", id_vars = c("level", "area", "year", "sex", "age", "race"))
  
  saveRDS(est_dt, file = paste0(output_dir_draws_est, "/est/est_mcnty_", yr, "_", s, "_", r, "_", edu, ".rds"))
  
  end.time <- Sys.time()
  print(second_round <- end.time - start.time)
  message('------------------------------')
  message('SCRIPT COMPLETED')
}
