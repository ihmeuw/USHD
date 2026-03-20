####################################################################################################
## Description: Compile point estimates, CI, and standard errors for all
##              geographic levels, years, sexes, races, and ages. This code also runs basic
##              consistency checks on output -- if any of these checks fail, a report about the
##              identified issues is saved instead of the final output file.
##
## Passed args: repo [character] -- directory where the repo is stored
##              output_dir [character] -- directory where the final output will be saved  (LU path)
##              settings_loc [character] -- location of the settings file
##              output_dir_draws_est [character] -- directory where the predictions will be saved (share drive)
##
## Requires:    point estimates for all level-year-sex-race combos:
##                "[dir]/est_[level]_[year]_[sex]_[race].rds"
##                (or the corresponding raked files if raked == T)
##
## Outputs:     If all checks pass, compiled estimates:
##                "[dir]/est_all.rds"
##                (or the corresponding raked files if raked == T)
##
##              Otherwise, text files with a list of flagged issues:
##                "[dir]/est_all_issues.txt"
##                (or the corresponding raked files if raked == T)
##
####################################################################################################

###### Load required libraries
pacman::p_load(R.utils, data.table)

###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (settings_loc <- commandArgs(TRUE)[[3]])
  (output_dir_draws_est <- commandArgs(TRUE)[[4]])
  (var_name <- commandArgs(TRUE)[[5]])
} else {
  if (!exists("repo")) {
    repo <- nonfatal_repo
  }
  var_name <- "pred"
}

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

###### Assign settings from settings file
get_settings(settings_loc)

if (n.imp == 0) {
  imp <- 0
}

if (!exists("by_source")) {
  by_source <- FALSE
}

var <- var_name

if (!exists("outcome")) {
  outcome <- "blank"
}

if ("_all" %in% outcome) {
  var <- "yld"
}

## Compile estimates -------------------------------------------------------------------
# Function to check estimates
check <- function(est, var, eval_vars) {
  issues <- NULL
  var_cols <- grep(var, names(est), value = TRUE) # original value column names
  setnames(est, var_cols, gsub(var, "value", var_cols))
  setcolorder(est, c(key(est), grep("value_", names(est), value = TRUE)))
  
  # check: no value estimates are NA
  if (nrow(est[is.na(value_mean) | is.na(value_lb) | is.na(value_ub)]) > 0) {
    issues <- c(issues, paste("Some estimates are NA"))
  }
  
  # check: all value estimates are >= 0
  if (est[, sum(value_mean < 0 | value_lb < 0 | value_ub < 0, na.rm = TRUE)] > 0) {
    issues <- c(issues, paste("Not all", var, "estimates are positive"))
  }
  
  # check: point estimates within confidence intervals
  if (est[, sum(!(value_mean > value_lb & value_mean < value_ub), na.rm = TRUE)] > 0) {
    issues <- c(issues, paste("Point estimates of", var, "are outside confidence bounds"))
  }
  
  # check: there are no duplicates
  if (nrow(est) != nrow(unique(est[, (eval_vars), with=F]))) {
    issues <- c(issues, "There are duplicated rows")
  }
  
  # check: all expected level/area/year/sex/race/age combinations are present
  if ("adjusted" %in% names(est)) {
    all <- CJ(level = c(area_var, names(geoagg_files)), year = years, sex = c(sexes, 3),
              race = unique(c(races, 1)), age = c(ages, 98, 99),
              adjusted = c(0,1))
  } else if ("_all" %in% outcome || var_name == "yld" || exists("cause_id")){
    all <- CJ(level = c(area_var, names(geoagg_files)), year = years, sex = c(sexes, 3),
              race = unique(c(races, 1)), age = c(ages, 98, 99))
  } else {
    if (by_source) {
      # determine whether source or source_v2 is used:
      # Source V2 is like the source column, but it separates BRFSS into 
      # two sources:
      # BRFSS_LLCP (combined landline & cellphone sample, used in years >= 2011)
      # BRFSS_LANDLINE_ONLY (BRFSS only sampled landlines prior to 2011)
      source_var <- grep("source", names(est), value = T)
      # make sure that it's only length 1
      if (length(source_var) > 1) {
        stop("More than one column in ests matches source: ", paste(source_var, collapse = ", "))
      }
      # if source_var is source_v2, we can't get the expected levels from the settings file.
      # Instead, just get those from the ests (so it's not actually going to check that all levels are there)
      if(source_var == "source_v2"){
        check_source_levels <- est[, unique(source_v2)]
        all <- CJ(level = c(area_var, names(geoagg_files)), year = years, sex = c(sexes, 3),
                  race = unique(c(races, 1)), age = c(ages, 98, 99), source_v2 = check_source_levels)
      } else{
        # otherwise, get the expected levels from the settings file
        all <- CJ(level = c(area_var, names(geoagg_files)), year = years, sex = c(sexes, 3),
                  race = unique(c(races, 1)), age = c(ages, 98, 99), source = source_levels)
      }      
    } else {
      all <- CJ(level = c(area_var, names(geoagg_files)), year = years, sex = c(sexes, 3),
                race = unique(c(races, 1)), age = c(ages, 98, 99))
    }
  }
  
  # including year because some geographic units appear in only some years rather than in all years (like CBSA-mcnties or PUMA-mcnties)
  if (by_source) {
    # determine whether source or source_v2 is used:
    source_var <- grep("source", names(est), value = T)
    # make sure that it's only length 1
    if (length(source_var) > 1) {
      stop("More than one column in ests matches source: ", paste(source_var, collapse = ", "))
    }
    all <- merge(all, unique(est[, .SD, .SDcols = c("level", "area", "year", source_var)]), by = c("level", "year", source_var), allow.cartesian = TRUE)
  } else {
    all <- merge(all, unique(est[, list(level, area, year)]), by = c("level", "year"), allow.cartesian = TRUE)  
  }
  
  if (!is.null(geoagg_files)) {
    for (this_level in names(geoagg_files)) {
      wts <- readRDS(geoagg_files[this_level])
      if ("year" %in% names(wts)) {
        all <- all[level != this_level | year %in% unique(wts$year),]
      }
      rm(wts)
    }
  }
  
  setkeyv(all, key(est))
  if (!isTRUE(all.equal(all, est[, names(all), with = FALSE]))) {
    issues <- c(issues, paste("There are missing level/area/year/sex/race/age/(adjusted) combinations for", var))
  }
  rm(all)
  
  # check: value for both sexes combined is between value for males and females
  if (length(sexes) == 2) {
    temp <- est[age < 98, c((eval_vars), "value_mean"), with = FALSE]
    func <- paste0(paste0(eval_vars[!(eval_vars == "sex")], collapse=" + "), " ~ sex")
    
    temp <- dcast.data.table(temp, formula = func, value.var = "value_mean")
    
    setnames(temp, as.character(1:3), c("m", "f", "b"))
    temp <- na.omit(temp)
    if (temp[, mean(b > 0.99999 * pmin(m, f) & b < 1.00001 * pmax(m, f)) < 1]) {
      issues <- c(issues, paste(var, "estimate for both sexes is not always between male and female estimates"))
    }
    rm(temp)
  }
  setnames(est, gsub(var, "value", var_cols), var_cols)
  issues
}

if ("_all" %in% outcome || var == "yld" || exists("cause_id")) {
  files <- expand_file_template(
    paste0(output_dir_draws_est, "/est/est_", level, "_", year, "_", sex, "_", race, "_1.rds"),
    level = c(area_var, names(geoagg_files)),
    year = years,
    sex = c(sexes, 3), # include aggregated sex
    race = unique(c(races, 1)) # aggregated all-race (1) included in this vector currently in settings
  )
} else if (var != "pred") {
  files <- expand_file_template(
    paste0(output_dir_draws_est, "/est/est_", level, "_", year, "_", sex, "_", race, "_", var, ".rds"),
    level = c(area_var, names(geoagg_files)), 
    year = years,
    sex = c(sexes, 3), # include aggregated sex
    race = unique(c(races, 1))
  )
} else {
  files <- expand_file_template(
    paste0(output_dir_draws_est, "/est/est_", level, "_", year, "_", sex, "_", race, "_", edu, ".rds"),
    level = c(area_var, names(geoagg_files)), 
    year = years,
    sex = c(sexes, 3), # include aggregated sex
    race = unique(c(races, 1))
  )
}

present <- file.exists(files)
if (any(!present)) {
  n.missing <- sum(!present)
  message(sprintf("(%s) %s files expected but not present: ", var, n.missing))
  stop_or_quit(
    msg = sprintf("\t%s\n", paste(files[!present], collapse = "\n\t")),
    status = 51)
}
files <- files[file.exists(files)]

est <- rbindlist(lapply(files, readRDS), use.names = TRUE)

if (by_source) {
  # determine whether source or source_v2 is used:
  source_var <- grep("source", names(est), value = T)
  # make sure that it's only length 1
  if (length(source_var) > 1) {
    stop("More than one column in ests matches source: ", paste(source_var, collapse = ", "))
  }
  eval_vars <- c("level", "area", "year", "sex", "race", "age", source_var)
} else {
  eval_vars <- c("level", "area", "year", "sex", "race", "age")
}

# format
setkeyv(est, eval_vars)
setcolorder(est, c(key(est), grep(paste0(var, "_"), names(est), value = T)))

# check for issues with var
if (!validate) {
  issues <- check(est, var, eval_vars)
} else {
  issues <- NULL
}

# save the estimates, regardless of whether there were issues (to assist with troubleshooting); also save the list of issues
saveRDS(est, file = paste0(output_dir_draws_est, "/est/", var, "_est_all.rds"))
write.table(issues, file = paste0(output_dir_draws_est, "/est/", var, "_est_all_issues.csv"),
            row.names = FALSE, col.names = FALSE)

#### Save estimates to covariate directory, if this is a covariate model
if (exists("model")) {
  if (model %like% "covariate_mod") {
    base_dir <- paste0("FILEPATH")
    est_save <- est[level == "mcnty" & sex == 3 & race != 1 & age < 98, list("mcnty" = area, outcome = pred_mean), by = c("year", "sex", "race", "age")]
    setnames(est_save, "outcome", outcome)
    race_table <- data.table(race = c(2, 4:7), race_label = c("Hispanic", "NH Black", "NH White", "NH AIAN", "NH API"))
    setkeyv(est_save, "race")
    est_save <- est_save[race_table]
    est_save[, race_set := 1]
    
    saveRDS(est_save, paste0(base_dir, run_date, ".rds"))
  }
}

rm(est, issues); gc()
