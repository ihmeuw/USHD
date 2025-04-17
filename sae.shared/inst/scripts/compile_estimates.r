####################################################################################################
## Description: Compile mx, yll, yld, pred (bmi) and (optionally) lt point estimates, CI, and standard errors for all
##              geographic levels, years, sexes, races, education groups, and ages. This code also runs basic
##              consistency checks on output -- if any of these checks fail, a report about the
##              identified issues is saved instead of the final output file.
##
## Passed args: dir [character] -- home directory for settings and final output
##              raked [logical] -- should raked (T) or unraked (F) estimates be compiled?
##              lt [logical] -- should life table estimates be compiled in addition to mx estimates?
##              validate [logical] -- is this a validation model? If so, testing is skipped.
##
## Requires:    mx, yll, yld, pred, and lt [if lt == T] point estimates for all level-year-sex-race-edu combos:
##                "[dir]/mx_est_[level]_[year]_[sex]_[race]_[edu].rds"
##                "[dir]/yll_est_[level]_[year]_[sex]_[race]_[edu].rds"
##                "[dir]/yld_est_[level]_[year]_[sex]_[race].rds"
##                "[dir]/pred_est_[level]_[year]_[sex]_[race].rds"
##                "[dir]/lt_est_[level]_[year]_[sex]_[race]_[edu].rds"
##                (or the corresponding raked files if raked == T)
##
## Outputs:     If all checks pass, compiled mx, yll, yld, pred, and lt [if lt == T] estimates:
##                "[dir]/mx_est_all.rds"
##                "[dir]/yll_est_all.rds"
##                "[dir]/yld_est_all.rds"
##                "[dir]/pred_est_all.rds"
##                "[dir]/lt_est_all.rds"
##                (or the corresponding raked files if raked == T)
##
##              Otherwise, text files with a list of flagged issues:
##                "[dir]/mx_est_all_issues.txt"
##                "[dir]/yll_est_all_issues.txt"
##                "[dir]/yld_est_all_issues.txt"
##                "[dir]/pred_est_all_issues.txt"
##                "[dir]/lt_est_all_issues.txt"
##                (or the corresponding raked files if raked == T)
##
####################################################################################################
library(lbd.loader,
        lib.loc = sprintf("FILEPATH",
                          R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
suppressMessages(lbd.loader::load.containing.package())

## Get settings ------------------------------------------------------------------------------------
if (interactive()) {
  dir <- "FILEPATH"
  raked <- FALSE
  run_lt <- TRUE
  validate <- TRUE
  write_estimates_with_issues <- FALSE
  skip_mx_aggregation <- FALSE
  skip_yll_aggregation <- TRUE
  skip_yld_aggregation <- TRUE
  skip_pred_aggregation <- TRUE
  
} else {
  parser <- argparse::ArgumentParser()
  add_dir_argument(parser)
  add_raked_argument(parser)
  add_lifetable_argument(parser)
  parser$add_argument("validate", choices = c("TRUE", "FALSE"))
  parser$add_argument("write_estimates_with_issues", choices = c("TRUE", "FALSE"), default = "TRUE", nargs = "?")
  add_aggregation_skip_flags(parser)
  args <- parser$parse_args(get_args())
  
  dir <- args$dir
  raked <- as.logical(args$raked)
  run_lt <- as.logical(args$run_lt)
  validate <- as.logical(args$validate)
  write_estimates_with_issues <- as.logical(args$write_estimates_with_issues)
  
  skip_mx_aggregation <- args$skip_mx_aggregation
  skip_yll_aggregation <- args$skip_yll_aggregation
  skip_yld_aggregation <- args$skip_yld_aggregation
  skip_pred_aggregation <- args$skip_pred_aggregation
  
}

for (setting in names(args)){
  print(paste0(setting, ': ', get(setting)))
}

# BMI prevalence/exposure still uses old race codes, overwrite defaults
if (!skip_pred_aggregation) {
  acause <- basename(dirname(dir))
  settings <- ModelSettings$from_yaml(dir)
  # race_default <- 9
  if(setequal(settings$races, c(1,2,3,4,7))){
    message("Overwriting race_default to 9...")
    overwrite_constants(new_race_default = 9)
    # replace with OLD race and edu codes
    race_default <- 9
  } else{
    stopifnot(setequal(settings$races, c(2,4,5,6,7)))
  }
} else {
  acause <- basename(dir)
  settings <- ModelSettings$from_yaml(dirname(dir))$settings_for_cause(acause)
}

## Compile mx, yll or yld estimates -------------------------------------------------------------------
# Function to check mx, yll or yld estimates
check <- function(est, var, eval_vars, settings) {
  issues <- NULL
  var_cols <- grep(var, names(est), value = T) # original value column names
  setnames(est, var_cols, gsub(var, "value", var_cols))
  setcolorder(est, c(key(est), grep("value_", names(est), value = T)))
  
  # check: all value estimates are >= 0
  if (est[, sum(value_mean < 0 | value_lb < 0 | value_ub < 0)] > 0) {
    issues <- c(issues, paste("Not all", var, "estimates are positive"))
  }
  
  # Some causes fail the "between confidence intervals" check. Apply exception.  pred has no causes
  if (var != "pred") issues <- looser_confidence_intervals_exception(dir, settings$rake_to_gbd_version, var, est, issues)
  
  # check: there are no duplicates
  if (nrow(est) != nrow(unique(est[, (eval_vars), with=F]))) {
    issues <- c(issues, "There are duplicated rows")
  }
  
  # get ages for checking combinations. The way that settings are loaded in this script creates
  # variables for ages and by_edu settings in scope. If this is an edu model, it's okay for age to
  # only be age 0, and it's okay for ages to be 25-85. If it is bmi-model (pred), it is ok to have
  # age 20-85
  if (settings$by_edu && (setequal(settings$ages, 0)) || setequal(settings$ages, seq(25, 85, 5))) {
    age <- c(sort(settings$ages), 98, 99)
  } else {
    if (identical(var, "pred")) {
      age <- c(settings$ages, 98, 99)
    } else {
      age <- c(readRDS(settings$age_std_file)$age, 98, 99)
    }
  }
  
  # check: all expected level/area/year/sex/race/edu/age combinations are present
  if ("adjusted" %in% names(est)) {
    all <- CJ(
      level = c(settings$area_var, names(settings$geoagg_files)),
      year = settings$years,
      sex = c(settings$sexes, 3),
      race = unique(c(settings$races, race_default)),
      edu = unique(c(settings$edu_groups, edu_default)),
      age = age,
      adjusted = c(if (settings$misclassification_correction) 0, 1)
    )
  } else {
    all <- CJ(
      level = c(settings$area_var, names(settings$geoagg_files)),
      year = settings$years,
      sex = c(settings$sexes, 3),
      race = unique(c(settings$races, race_default)),
      edu = unique(c(settings$edu_groups, edu_default)),
      age = age
    )
  }
  
  all <- merge(all, unique(est[, list(level, area)]), by = "level", allow.cartesian = T)
  
  if (!is.null(settings$geoagg_files)) {
    for (this_level in names(settings$geoagg_files)) {
      wts <- readRDS(settings$geoagg_files[[this_level]])
      if ("year" %in% names(wts)) {
        all <- all[level != this_level | year %in% unique(wts$year), ]
      }
      rm(wts)
    }
  }
  setkeyv(all, key(est))
  if (!isTRUE(all.equal(all, est[, names(all), with = F]))) {
    message("This is the value of the all.equal() call:")
    print(all.equal(all, est[, names(all), with = F]))
    message(paste("There are missing level/area/year/sex/race/edu/age/(adjusted) combinations for", var))
    all[, unique_key := paste0(level, "_", year, "_", sex, "_", race, "_", edu, "_", age, "_", area)]
    est[, unique_key := paste0(level, "_", year, "_", sex, "_", race, "_", edu, "_", age, "_", area)]
    
    missing_keys = all$unique_key[!all$unique_key %in% est$unique_key]
    message("missing keys:\n\t", paste(missing_keys, collapse="\n\t"))
    issues <- c(issues, paste("There are missing level/area/year/sex/race/edu/age/(adjusted) combinations for", var))
  }
  
  rm(all)
  
  if (length(settings$sexes) == 2) {
    temp <- est[age < 98, c((eval_vars),"value_mean"), with=F]
    func <- paste0(paste0(eval_vars[!(eval_vars == "sex")],collapse=" + "), " ~ sex")
    
    temp <- dcast.data.table(temp, formula = func, value.var = "value_mean")
    
    setnames(temp, as.character(1:3), c("m", "f", "b"))
    temp <- na.omit(temp)
    if (nrow(temp[b < 0.99999 * pmin(m, f) | b > 1.00001 * pmax(m, f)])>0) {
      issues <- c(issues, paste(var, "estimate for both sexes is not always between male and female estimates"))
    }
    rm(temp)
  }
  setnames(est, gsub(var, "value", var_cols), var_cols)
  return(issues)
}

write_estimates <- function(issues, est, var) {
  # Save bmi (pred) estimates to "est" dir
  if (identical(var, "pred")) {
    dir <- file.path(dir, "est")
  }
  
  # if there are no problems, save the estimates, otherwise save the list of issues
  if (length(issues) == 0) {
    est[, acause := acause]
    saveRDS(est, file = paste0(dir, "/", var, "_est_all", ifelse(raked, "_raked", ""), ".rds"))
    
    message("compile_estimates.r finished successfully for var = ", var)
  } else {
    if (write_estimates_with_issues) {
      write.table(issues, file = paste0(dir, "/", var, "_est_all", ifelse(raked, "_raked", ""), "_issues.csv"),
                  row.names = F, col.names = F)
      
      est[, acause := acause]
      file <- paste0(dir, "/", var, "_est_all", ifelse(raked, "_raked", ""), "_with_issues.rds")
      saveRDS(est, file = file)
      
      message("compile_estimates.r finished with issues, for var = ", var, " wrote ", file)
    }
    else {
      write.table(issues, file = paste0(dir, "/", var, "_est_all", ifelse(raked, "_raked", ""), "_issues.csv"),
                  row.names = F, col.names = F)
      lsae.utils::stop_or_quit(msg = paste0("Issues for ", var, " were non-zero in compile estimates."), status=3) # This makes sure you see this on job report.
    }
  }
}

# load and combine estimates for all levels, years, sexes, and races
for (var in c("mx", "yll", "yld", "pred")) {
  # optionally skip combining estimates for MX or YLL
  if (get(sprintf("skip_%s_aggregation", var))) next
  
  if (identical(var, "pred")) {
    # bmi models (pred) do not have cause directories. Draws and estimates are saved to dir/draws and dir/est respectively.
    est_dir <- dir
  } else {
    est_dir <- dirname(dir)
  }
  
  # if this is for validation we expect a different set of files. No raked
  # estimates, only mcnty level, only males and females, only race/edu specific.
  if (validate) {
    raked_arg <- "unraked"
    level_arg <- c("mcnty")
    sex_arg <- c(settings$sexes)
    race_arg <- unique(settings$races)
    edu_arg <- unique(c(settings$edu_groups))
    
    settings_val <- read.csv(paste0(dir, "/settings.csv"), stringsAsFactors = F, header = F)
    gs <- readRDS(settings_val[settings_val$V1 == "gs_file", ]$V2)
    years_arg <- unique(gs$year)
    
  } else {
    raked_arg <- ifelse(raked, "raked", "unraked")
    level_arg <- c("mcnty", "state", "natl")
    sex_arg <- c(settings$sexes, 3)
    race_arg <- unique(c(settings$races, race_default))
    edu_arg <- unique(c(settings$edu_groups, edu_default))
    years_arg <- settings$years
  }
  
  files <- expand_file_template(
    cause_mx_draws_path(
      root = est_dir,
      acause = acause,
      measure = var,
      type = "est",
      area_var = level,
      year = year,
      sex = sex,
      race = race,
      edu = edu,
      raked = raked_arg
    ),
    level = level_arg,
    year = years_arg,
    sex = sex_arg,
    race = race_arg,
    edu = edu_arg
  )
  
  present <- file.exists(files)
  if (any(!present)) {
    n.missing <- sum(!present)
    message(sprintf("(%s) %s files expected but not present:", var, n.missing))
    lsae.utils::stop_or_quit(
      msg = sprintf("\t%s\n", paste(files[!present], collapse = "\n\t")),
      status = 51)
  }
  files <- files[file.exists(files)]
  
  # We have files with different schema - this is a hack to read them in correctly.
  
  est <- rbindlist(lapply(files, readRDS), use.names = T, fill = T)
  if (!settings$by_race) {
    est[, race := race_default]
  }
  if ('acause' %in% names(est)) {
    est[, acause := NULL]
  }
  if (!settings$by_edu) {
    est[, edu := edu_default]
    
  }
  
  # need to take into consideration the adjusted column
  eval_vars <- c("level","area","year","sex","race","edu","age")
  if("adjusted" %in% names(est)) {
    eval_vars <- c(eval_vars,"adjusted")
  }
  
  # format
  setkeyv(est, eval_vars)
  setcolorder(est, c(key(est), grep(paste0(var, "_"), names(est), value = T)))
  
  # check for issues with var
  if (!validate) {
    issues <- check(est, var, eval_vars, settings)
  } else {
    issues <- NULL
  }
  
  
  write_estimates(issues, est, var)
  rm(est, issues); gc()
}

## Compile lt estimates ----------------------------------------------------------------------------
if (run_lt) {
  
  est_dir <- dirname(dir)
  
  # if this is for validation we expect a different set of files. No raked
  # estimates, only mcnty level, only males and females, only race/edu specific.
  if (validate) {
    raked_arg <- "unraked"
    level_arg <- c("mcnty")
    sex_arg <- c(settings$sexes)
    race_arg <- unique(settings$races)
    edu_arg <- unique(c(settings$edu_groups))
    
    settings_val <- read.csv(paste0(dir, "/settings.csv"), stringsAsFactors = F, header = F)
    gs <- readRDS(settings_val[settings_val$V1 == "gs_file", ]$V2)
    years_arg <- unique(gs$year)
    
  } else {
    raked_arg <- ifelse(raked, "raked", "unraked")
    level_arg <- c("mcnty", "state", "natl")
    sex_arg <- c(settings$sexes, 3)
    race_arg <- unique(c(settings$races, race_default))
    edu_arg <- unique(c(settings$edu_groups, edu_default))
    years_arg <- settings$years
  }
  
  if(paste0("lt_est_all", ifelse(raked, "_raked", ""), "_with_issues.rds") %in% files) {
    files <- files[files != paste0("lt_est_all", ifelse(raked, "_raked", ""), "_with_issues.rds")]
  }
  
  files <- expand_file_template(
    cause_mx_draws_path(
      root = est_dir,
      acause = acause,
      measure = "lt",
      type = "est",
      area_var = level,
      year = year,
      sex = sex,
      race = race,
      edu = edu,
      raked = raked_arg
    ),
    level = level_arg,
    year = years_arg,
    sex = sex_arg,
    race = race_arg,
    edu = edu_arg
  )
  
  present <- file.exists(files)
  if (any(!present)) {
    n.missing <- sum(!present)
    message(sprintf("(lt) %s files expected but not present:", n.missing))
    lsae.utils::stop_or_quit(
      msg = sprintf("\t%s\n", paste(files[!present], collapse = "\n\t")),
      status = 51)
  }
  files <- files[file.exists(files)]
  
  # load and combine estimates for all levels, years, sexes, and races
  est <- rbindlist(lapply(files, readRDS), use.names=T, fill=T)
  if(!settings$by_race) {
    est[, race:=race_default]
  }
  if ('acause' %in% names(est)){
    est[, acause:=NULL]
  }
  if(!settings$by_edu) {
    est[, edu := edu_default]
  }
  if(settings$area_var %in% names(est)) {
    setnames(est, settings$area_var, "tmp")
    est[is.na(area), area := tmp]
    est[,tmp := NULL]
  }
  
  # need to take into consideration the adjusted column
  eval_vars <- c("level","area","year","sex","race","edu","age")
  if("adjusted" %in% names(est)) {
    eval_vars <- c(eval_vars,"adjusted")
  }
  
  # format
  setkeyv(est, eval_vars)
  setcolorder(est, c(key(est), grep("mx_|ax_|qx_|ex_", names(est), value = T)))
  
  ## Run checks on lt estimates and save -------------------------------------------------------------
  issues <- NULL
  if (!validate) {
    
    # check: all mx/ex estimates are positive
    if (est[, sum(mx_mean <= 0 | mx_lb <= 0 | mx_ub <= 0)] > 0) {
      issues <- c(issues, "Not all mx estimates are positive")
    }
    if (est[, sum(ex_mean <= 0 | ex_lb <= 0 | ex_ub <= 0)] > 0) {
      issues <- c(issues, "Not all ex estimates are positive")
    }
    
    # check: all qx values are between 0 and 1
    if (est[, sum(!data.table::between(qx_mean, 0, 1 + 10e-10))] > 0) {
      issues <- c(issues, "Not all mean qx are between 0 and 1")
    }
    if (est[, sum(!data.table::between(qx_lb, 0, 1 + 10e-10))] > 0) {
      issues <- c(issues, "Not all lower qx are between 0 and 1")
    }
    if (est[, sum(!data.table::between(qx_ub, 0, 1 + 10e-10))] > 0) {
      issues <- c(issues, "Not all upper qx are between 0 and 1")
    }
    
    # check: all ax values are between 0 and n
    if (est[age == 0, sum(!data.table::between(ax_mean, 0, 1 + 10e-10))] +
        est[age == 1, sum(!data.table::between(ax_mean, 0, 4 + 10e-10))] +
        est[age > 1 & age < max(age), sum(!data.table::between(ax_mean, 0, 5 + 10e-10))] > 0) {
      issues <- c(issues, "Not all mean ax are between 0 and n")
    }
    if (est[age == 0, sum(!data.table::between(ax_lb, 0, 1 + 10e-10))] +
        est[age == 1, sum(!data.table::between(ax_lb, 0, 4 + 10e-10))] +
        est[age > 1 & age < max(age), sum(!data.table::between(ax_lb, 0, 5 + 10e-10))] > 0) {
      issues <- c(issues, "Not all lower ax are between 0 and n")
    }
    if (est[age == 0, sum(!data.table::between(ax_ub, 0, 1 + 10e-10))] +
        est[age == 1, sum(!data.table::between(ax_ub, 0, 4 + 10e-10))] +
        est[age > 1 & age < max(age), sum(!data.table::between(ax_ub, 0, 5 + 10e-10))] > 0) {
      issues <- c(issues, "Not all upper ax are between 0 and n")
    }
    
    # check: point estimates within confidence intervals
    if (est[, sum(!data.table::between(mx_mean, mx_lb, mx_ub))] > 0) {
      issues <- c(issues, "mx point estimates are outside confidence bounds")
    }
    if (est[, sum(!data.table::between(ax_mean, ax_lb, ax_ub))] > 0) {
      issues <- c(issues, "ax point estimates are outside confidence bounds")
    }
    if (est[age < max(age), sum(!data.table::between(qx_mean, qx_lb, qx_ub))] > 0) {
      issues <- c(issues, "qx point estimates are outside confidence bounds")
    }
    if (est[, sum(!data.table::between(ex_mean, ex_lb, ex_ub))] > 0) {
      issues <- c(issues, "ex point estimates are outside confidence bounds")
    }
    
    # check: there are no duplicates
    if (nrow(est) != nrow(unique(est[, (eval_vars), with=F]))) {
      issues <- c(issues, "There are duplicated rows")
    }
    
    # get ages for checking combinations. The way that settings are loaded in this script creates
    # variables for ages and by_edu settings in scope. If this is an edu model, it's okay for ages to be 25-85
    if (settings$by_edu && setequal(settings$ages, seq(25, 85, 5))) {
      age <- sort(settings$ages)
    } else {
      age <- readRDS(settings$age_std_file)$age
    }
    
    if("adjusted" %in% names(est)) {
      all <-
        CJ(
          level = c(settings$area_var, names(settings$geoagg_files)),
          year = settings$years,
          sex = c(settings$sexes, 3),
          race = unique(c(settings$races, race_default)),
          edu = unique(c(settings$edu_groups, edu_default)),
          age = age,
          adjusted = c(if(settings$misclassification_correction) 0, 1)
        )
    } else {
      all <-
        CJ(
          level = c(settings$area_var, names(settings$geoagg_files)),
          year = settings$years,
          sex = c(settings$sexes, 3),
          race = unique(c(settings$races, race_default)),
          edu = unique(c(settings$edu_groups, edu_default)),
          age = age
        )
    }
    
    
    all <- merge(all, unique(est[, list(level, area)]), by = "level", allow.cartesian = T)
    if (!is.null(settings$geoagg_files)) {
      for (this_level in names(settings$geoagg_files)) {
        wts <- readRDS(settings$geoagg_files[[this_level]])
        if ("year" %in% names(wts)) {
          all <- all[level != this_level | year %in% unique(wts$year), ]
        }
        rm(wts)
      }
    }
    setkeyv(all, key(est))
    if (!isTRUE(all.equal(all, est[, names(all), with = F]))) {
      issues <- c(issues, "There are missing level/area/year/sex/age/(adjusted) combinations")
    }
    rm(all)
    
    if (length(settings$sexes) == 2) {
      temp <- est[, c((eval_vars),"mx_mean"), with=F]
      
      func <- paste0(paste0(eval_vars[!(eval_vars == "sex")],collapse=" + "), " ~ sex")
      temp <- dcast.data.table(temp, formula = func, value.var = "mx_mean")
      setnames(temp, as.character(1:3), c("m", "f", "b"))
      temp <- na.omit(temp)
      if (temp[, mean(data.table::between(b, 0.99999 * pmin(m, f), 1.00001 * pmax(m, f))) < 1]) {
        issues <- c(issues, "mx estimate for both sexes is not always between male and female estimates")
      }
      rm(temp)
    }
  }
  write_estimates(issues, est, var = "lt")
}
