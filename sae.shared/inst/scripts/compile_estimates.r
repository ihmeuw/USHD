####################################################################################################
## Description: Compile mx, yll, yld, and (optionally) lt point estimates, CI, and standard errors for all
##              geographic levels, years, sexes, races, education groups, and ages. This code also runs basic
##              consistency checks on output -- if any of these checks fail, a report about the
##              identified issues is saved instead of the final output file.
##
## Passed args: dir [character] -- home directory for settings and final output
##              raked [logical] -- should raked (T) or unraked (F) estimates be compiled?
##              lt [logical] -- should life table estimates be compiled in addition to mx estimates?
##              validate [logical] -- is this a validation model? If so, testing is skipped.
##
## Requires:    mx, yll, yld, and lt [if lt == T] point estimates for all level-year-sex-race-edu combos:
##                "[dir]/mx_est_[level]_[year]_[sex]_[race]_[edu].rds"
##                "[dir]/yll_est_[level]_[year]_[sex]_[race]_[edu].rds"
##                "[dir]/yld_est_[level]_[year]_[sex]_[race].rds"
##                "[dir]/lt_est_[level]_[year]_[sex]_[race]_[edu].rds"
##                (or the corresponding raked files if raked == T)
##
## Outputs:     If all checks pass, compiled mx, yll, yld, and lt [if lt == T] estimates:
##                "[dir]/mx_est_all.rds"
##                "[dir]/yll_est_all.rds"
##                "[dir]/yld_est_all.rds"
##                "[dir]/lt_est_all.rds"
##                (or the corresponding raked files if raked == T)
##
##              Otherwise, text files with a list of flagged issues:
##                "[dir]/mx_est_all_issues.txt"
##                "[dir]/yll_est_all_issues.txt"
##                "[dir]/yld_est_all_issues.txt"
##                "[dir]/lt_est_all_issues.txt"
##                (or the corresponding raked files if raked == T)
##
####################################################################################################

library(lbd.loader, lib.loc = sprintf("FILEPATH", R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
suppressMessages(lbd.loader::load.containing.package())

## Get settings ------------------------------------------------------------------------------------
parser <- argparse::ArgumentParser()
add_dir_argument(parser)
add_raked_argument(parser)
add_lifetable_argument(parser)
parser$add_argument("validate", choices = c("TRUE", "FALSE"))
add_aggregation_skip_flags(parser)

args <- parser$parse_args(get_args())

dir <- args$dir
raked <- as.logical(args$raked)
run_lt <- as.logical(args$run_lt)
validate <- as.logical(args$validate)
skip_mx_aggregation <- args$skip_mx_aggregation
skip_yll_aggregation <- args$skip_yll_aggregation
skip_yld_aggregation <- args$skip_yld_aggregation

for (setting in names(args)){
  print(paste0(setting, ': ', get(setting)))
}


acause <- basename(dir)

# load settings.yaml from parent dir, get settings for this cause, and then update .GlobalEnv
ModelSettings$from_dir(dirname(dir))$settings_for_cause(acause)$update_env(.GlobalEnv)

## Compile mx, yll or yld estimates -------------------------------------------------------------------
# Function to check mx, yll or yld estimates
check <- function(est, var, eval_vars) {
  issues <- NULL
  var_cols <- grep(var, names(est), value = T) # original value column names
  setnames(est, var_cols, gsub(var, "value", var_cols))
  setcolorder(est, c(key(est), grep("value_", names(est), value = T)))

  # check: all value estimates are >= 0
  if (est[, sum(value_mean < 0 | value_lb < 0 | value_ub < 0)] > 0) {
    issues <- c(issues, paste("Not all", var, "estimates are positive"))
  }

  # Some causes fail the "between confidence intervals" check. Apply exception.
  issues <- looser_confidence_intervals_exception(dir, rake_to_gbd_version, var, est, issues)

  # check: there are no duplicates
  if (nrow(est) != nrow(unique(est[, (eval_vars), with=F]))) {
    issues <- c(issues, "There are duplicated rows")
  }

  # check: all expected level/area/year/sex/race/edu/age combinations are present
  if("adjusted" %in% names(est)) {
    all <- CJ(
      level = c(area_var, names(geoagg_files)),
      year = years,
      sex = c(sexes, 3),
      race = unique(c(races, race_default)),
      edu = unique(c(edu_groups, edu_default)),
      age = c(readRDS(age_std_file)$age, 98, 99),
      adjusted = c(if (misclassification_correction) 0, 1)
    )

  } else {
    all <- CJ(
      level = c(area_var, names(geoagg_files)),
      year = years,
      sex = c(sexes, 3),
      race = unique(c(races, race_default)),
      edu = unique(c(edu_groups, edu_default)),
      age = c(readRDS(age_std_file)$age, 98, 99)
    )

  }

  all <- merge(all, unique(est[, list(level, area)]), by = "level", allow.cartesian = T)

  if (!is.null(geoagg_files)) {
    for (this_level in names(geoagg_files)) {
      wts <- readRDS(geoagg_files[[this_level]])
      if ("year" %in% names(wts)) {
        all <- all[level != this_level | year %in% unique(wts$year), ]
      }
      rm(wts)
    }
  }
  setkeyv(all, key(est))
  if (!isTRUE(all.equal(all, est[, names(all), with = F]))) {
    all[, unique_key:=paste0(level, "_", year, "_", sex, "_", race, "_", edu, "_", age, "_", area)]
    est[, unique_key:=paste0(level, "_", year, "_", sex, "_", race, "_", edu, "_", age, "_", area)]

    missing_keys = all$unique_key[!all$unique_key %in% est$unique_key]
    message("missing keys:\n\t", paste(missing_keys, collapse="\n\t"))
    issues <- c(issues, paste("There are missing level/area/year/sex/race/edu/age/(adjusted) combinations for", var))
  }

  rm(all)

  # check: value for both sexes combined is between value for males and females
  if (length(sexes) == 2) {
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

# load and combine estimates for all levels, years, sexes, and races
for (var in c("mx", "yll", "yld")) {
  # optionally skip combining estimates for MX or YLL
  if (get(sprintf("skip_%s_aggregation", var))) next

  files <- expand_file_template(
    cause_mx_draws_path(
      root = dirname(dir),
      acause = acause,
      measure = var, "est",
      area_var = level,
      year = year,
      sex = sex,
      race = race,
      edu = edu,
      raked = if (raked) "raked" else "unraked"
    ),
    level = c("mcnty", "state", "natl"),
    year = years,
    sex = c(sexes, 3), # include aggregated sex
    race = unique(c(races, race_default)),
    edu = unique(c(edu_groups, edu_default))
  )

  present <- file.exists(files)
  if (any(!present)) {
    n.missing <- sum(!present)
    message(sprintf("(%s) %s files expected but not present:", var, n.missing))
    stop_or_quit(
      msg = sprintf("\t%s\n", paste(files[!present], collapse = "\n\t")),
      status = 51)
  }
  files <- files[file.exists(files)]

  est <- data.table()
  for (i in 1:length(files)){
    subset = readRDS(paste0(files[i]))

    if ('acause' %in% names(subset)){
      subset[, acause:=NULL]
    }

    if (!'area' %in% names(subset)){
      setnames(subset, area_var, 'area')
    }

    if (!'race' %in% names(subset)){
      subset[, race:=race_default]
    }

    if (!'edu' %in% names(subset)){
      subset[, edu:=edu_default]
    }

    tryCatch({
      est <- rbind(est, subset, use.names=T)
    }, error=function(cond) {
      print(cond)
      print(sprintf("Iteration is %i and file is %s", i, files[[i]]))
    }
    )
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
    issues <- check(est, var, eval_vars)
  } else {
    issues <- NULL
  }

  # if there are no problems, save the estimates, otherwise save the list of issues
  if (length(issues) == 0) {
    est[, acause := acause]
    saveRDS(est, file = paste0(dir, "/", var, "_est_all", ifelse(raked, "_raked", ""), ".rds"))
  } else if (length(issues) > 0) {
    write.table(issues, file = paste0(dir, "/", var, "_est_all", ifelse(raked, "_raked", ""), "_issues.csv"),
                row.names = F, col.names = F)
    stop_or_quit("Issues for mortality rates were non-zero in compile estimates.", status=3) # This makes sure you see this on job report.
  }
  rm(est, issues); gc()
}

## Compile lt estimates ----------------------------------------------------------------------------
if (run_lt) {
  # load and combine estimates for all levels, years, sexes, and races
  files <- dir(dir, pattern = "lt_est_.*rds")
  files <- grep("_raked", files, value = T, invert = !raked)

  if (paste0("lt_est_all", ifelse(raked, "_raked", ""), ".rds") %in% files) {
    files <- files[files != paste0("lt_est_all", ifelse(raked, "_raked", ""), ".rds")]
  }

  est <- data.table()
  for (i in 1:length(files)){
    subset = readRDS(paste0(dir, "/", files[i]))

    if ('acause' %in% names(subset)){
      subset[, acause:=NULL]
    }

    if (! 'area' %in% names(subset)){
      setnames(subset, area_var, 'area')
    }

    if (!'race' %in% names(subset)){
      subset[, race:=race_default]
    }

    if (!'edu' %in% names(subset)){
      subset[, edu:=edu_default]
    }

    tryCatch({
      est <- rbind(est, subset, use.names=T)
    }, error=function(cond) {
      print(cond)
      print(sprintf("Iteration is %i and file is %s", i, files[[i]]))
    }
    )
  }

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
    if (est[, sum(!between(qx_mean, 0, 1 + 10e-10))] > 0) {
      issues <- c(issues, "Not all mean qx are between 0 and 1")
    }
    if (est[, sum(!between(qx_lb, 0, 1 + 10e-10))] > 0) {
      issues <- c(issues, "Not all lower qx are between 0 and 1")
    }
    if (est[, sum(!between(qx_ub, 0, 1 + 10e-10))] > 0) {
      issues <- c(issues, "Not all upper qx are between 0 and 1")
    }

    # check: all ax values are between 0 and n
    if (est[age == 0, sum(!between(ax_mean, 0, 1 + 10e-10))] +
        est[age == 1, sum(!between(ax_mean, 0, 4 + 10e-10))] +
        est[age > 1 & age < max(age), sum(!between(ax_mean, 0, 5 + 10e-10))] > 0) {
      issues <- c(issues, "Not all mean ax are between 0 and n")
    }
    if (est[age == 0, sum(!between(ax_lb, 0, 1 + 10e-10))] +
        est[age == 1, sum(!between(ax_lb, 0, 4 + 10e-10))] +
        est[age > 1 & age < max(age), sum(!between(ax_lb, 0, 5 + 10e-10))] > 0) {
      issues <- c(issues, "Not all lower ax are between 0 and n")
    }
    if (est[age == 0, sum(!between(ax_ub, 0, 1 + 10e-10))] +
        est[age == 1, sum(!between(ax_ub, 0, 4 + 10e-10))] +
        est[age > 1 & age < max(age), sum(!between(ax_ub, 0, 5 + 10e-10))] > 0) {
      issues <- c(issues, "Not all upper ax are between 0 and n")
    }

    # check: point estimates within confidence intervals
    if (est[, sum(!between(mx_mean, mx_lb, mx_ub))] > 0) {
      issues <- c(issues, "mx point estimates are outside confidence bounds")
    }
    if (est[, sum(!between(ax_mean, ax_lb, ax_ub))] > 0) {
      issues <- c(issues, "ax point estimates are outside confidence bounds")
    }
    if (est[age < max(age), sum(!between(qx_mean, qx_lb, qx_ub))] > 0) {
      issues <- c(issues, "qx point estimates are outside confidence bounds")
    }
    if (est[, sum(!between(ex_mean, ex_lb, ex_ub))] > 0) {
      issues <- c(issues, "ex point estimates are outside confidence bounds")
    }

    # check: there are no duplicates
    if (nrow(est) != nrow(unique(est[, (eval_vars), with=F]))) {
      issues <- c(issues, "There are duplicated rows")
    }

    # check: all expected level/area/year/sex/age combinations are present
    if("adjusted" %in% names(est)) {
      all <- CJ(level = c(area_var, names(geoagg_files)), 
                ear = years, 
                sex = c(sexes, 3),
                race = unique(c(races, race_default)), 
                edu = unique(c(edu_groups, edu_default)), 
                age = c(0, 1, seq(5, 85, 5)),
                adjusted = c(if (misclassification_correct) 0, 1))
    } else {
      all <- CJ(level = c(area_var, names(geoagg_files)), 
                year = years, 
                sex = c(sexes, 3),
                race = unique(c(races, race_default)), 
                edu = unique(c(edu_groups, edu_default)), 
                age = c(0, 1, seq(5, 85, 5)))
    }


    all <- merge(all, unique(est[, list(level, area)]), by = "level", allow.cartesian = T)
    if (!is.null(geoagg_files)) {
      for (this_level in names(geoagg_files)) {
        wts <- readRDS(geoagg_files[[this_level]])
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

    # check: mx for both sexes combined is between mx for males and females (allow some tolerance for
    # when the estimate for both is effectively equal to the male or female estimate)
    if (length(sexes) == 2) {
      temp <- est[, c((eval_vars),"mx_mean"), with=F]

      func <- paste0(paste0(eval_vars[!(eval_vars == "sex")],collapse=" + "), " ~ sex")
      temp <- dcast.data.table(temp, formula = func, value.var = "mx_mean")
      setnames(temp, as.character(1:3), c("m", "f", "b"))
      temp <- na.omit(temp)
      if (temp[, mean(between(b, 0.99999 * pmin(m, f), 1.00001 * pmax(m, f))) < 1]) {
        issues <- c(issues, "mx estimate for both sexes is not always between male and female estimates")
      }
      rm(temp)
    }

  }

  # if there are no problems, save the estimates, otherwise save the list of issues
  if (length(issues) == 0) {
    est[, acause := acause]
    saveRDS(est, file = paste0(dir, "/lt_est_all", ifelse(raked, "_raked", ""), ".rds"))
    message("compile_estimates.r finished successfully")
  } else if (length(issues) > 0) {
    write.table(issues, file = paste0(dir, "/lt_est_all", ifelse(raked, "_raked", ""), "_issues.csv"),
                row.names = F, col.names = F)
    stop_or_quit("Issues for lifetables were non-zero in compile estimates.", status=3) # This makes sure you see this on job report.
  }
}
