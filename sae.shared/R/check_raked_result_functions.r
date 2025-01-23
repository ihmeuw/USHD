#' @title recover_diff_files
#'
#' @description If needed, use this function to re-form geo_diff and cause_diff using the
#'              intermediate files saved.
#'
#' @param diff_dir - Directory where diff files are saved (usually check_raked_results/[geo or cause]_diff)
#' @param causes - list of cause names
#' @param measures - Vector of measures to use.  Usually run singly ['mx'], ['yll'] or ['yld']
#'
#' @return single data.table that is the concatenated set of individual diff files.
#'
#' @rdname recover_diff_files
#' @export
recover_diff_files <- function(diff_dir, causes, measures) {
  # match files specific to causes and measures
  cause_var <- tail(unlist(strsplit(diff_dir, "/")), n = 1)
  if(cause_var %in% c("race_diff","edu_diff")) {
    cause_var <- "diff"
  }

  pattern <- paste0("^(", paste(measures, collapse = "|"), ")_(", paste(causes, collapse = "|"), ")_", cause_var, ".rds$")

  files <- list.files(diff_dir, full.names = T, pattern = pattern)
  temp_diff <- lapply(files, function(file) {
    readRDS(file)
  })
  temp_diff <- rbindlist(temp_diff, use.names = T)

  return(temp_diff)
}
#' @title check_cause_diff
#'
#' @description Checks that differences in cause_diff data.table are not NaN, and that they fall within abs_tol and rel_tol
#' (These are set globally in the calling scripts).  Appends apprpriate information, warning and error messages
#' into returned list.
#'
#' @param cause_diff - [data.table] single data.table that is the concatenated set of individual cause diff files.

#'
#' @return list of messages generated by cause checks, causes which failed, and causes which passed
#'
#' @rdname check_cause_diff
#' @export
check_cause_diff <- function(cause_diff, results_dir) {
  # update abs_tol from global environment if it exists
  abs_tol <- get("abs_tol", envir = .GlobalEnv)
  msg_list <- list()
  failed_causes <- vector("character")
  if (nrow(cause_diff) > 0) {
    cause_diff[, na_mean_pass := ifelse(is.na(diff_cause_mean), FALSE, TRUE)]
    cause_diff[, na_percentage_pass := ifelse(is.na(perc_diff_cause_mean), FALSE, TRUE)]
    cause_diff[, tolerance_pass := ifelse(as.numeric(diff_cause_mean < abs_tol & perc_diff_cause_mean < rel_tol) & na_mean_pass & na_percentage_pass,
      TRUE, FALSE
    )]

    if (nrow(cause_diff[na_mean_pass == FALSE | na_percentage_pass == FALSE | tolerance_pass == FALSE]) > 0) {
      failed_causes <- unique(cause_diff[na_mean_pass == FALSE | na_percentage_pass == FALSE | tolerance_pass == FALSE, acause])
      msg_list <- append(msg_list, "\nSome rows failed the cause checks:")

      msg_list <- append(msg_list, "\nFailures because calculated mean was NA:")
      msg_list <- append(msg_list, capture.output(print(unique(cause_diff[na_mean_pass == FALSE, list(acause, measure, diff_cause_mean, perc_diff_cause_mean)]))))

      msg_list <- append(msg_list, "\nFailures because calculated percentage was NA:")
      msg_list <- append(msg_list, capture.output(print(unique(cause_diff[na_percentage_pass == FALSE, list(acause, measure, diff_cause_mean, perc_diff_cause_mean)]))))

      msg_list <- append(msg_list, "\nFailures because difference was above tolerance, or NAs were calculated for difference metrics:")
      msg_list <- append(msg_list, paste0("\nTolerance level for diff_cause_mean is ", abs_tol))
      msg_list <- append(msg_list, paste0("\nTolerance level for perc_diff_cause_mean is ", rel_tol))
      msg_list <- append(msg_list, capture.output(print(unique(cause_diff[tolerance_pass == FALSE, list(acause, measure, diff_cause_mean, perc_diff_cause_mean)]))))
    } else {
      msg_list <- append(msg_list, "all cause checks passed!")
    }
  } else {
    msg_list <- append(msg_list, "Cause-checks not done. Caution, diff-files missing. Check /check_raked_result/cause_status.rds")
  }
  result <- list(
    messages = msg_list,
    failed = failed_causes,
    passed = setdiff(unique(cause_diff$acause), failed_causes)
  )
  return(result)
}
#' @title check_geo_diff
#'
#' @description Checks that differences in cause_diff data.table are not NaN, and that they fall within abs_tol and rel_tol
#' (these are set globally in the calling scripts).  Appends apprpriate information, warning and error messages
#' into returned list.
#'
#' @param geo_diff - [data.table] single data.table that is the concatenated set of individual geo diff files.

#'
#' @return list of messages generated by geo checks, causes which failed, and causes which passed
#'
#' @rdname check_geo_diff
#' @export
check_geo_diff <- function(geo_diff, results_dir) {
  # update abs_tol from global environment if it exists
  abs_tol <- get("abs_tol", envir = .GlobalEnv)
  msg_list <- list()
  failed_causes <- vector("character")
  if (nrow(geo_diff) > 0) {
    geo_diff[, na_mean_pass := ifelse(is.na(diff_geo_mean), FALSE, TRUE)]
    geo_diff[, na_percentage_pass := ifelse(is.na(perc_diff_geo_mean), FALSE, TRUE)]
    geo_diff[, tolerance_pass := ifelse(as.numeric(diff_geo_mean < abs_tol & perc_diff_geo_mean < rel_tol) & na_mean_pass & na_percentage_pass,
      TRUE, FALSE
    )]

    if (nrow(geo_diff[na_mean_pass == FALSE | na_percentage_pass == FALSE | tolerance_pass == FALSE]) > 0) {
      failed_causes <- unique(geo_diff[na_mean_pass == FALSE | na_percentage_pass == FALSE | tolerance_pass == FALSE, acause])
      msg_list <- append(msg_list, "\nSome rows failed the geography checks:")

      msg_list <- append(msg_list, "\nFailures because calculated mean was NA:")
      msg_list <- append(msg_list, capture.output(print(unique(geo_diff[na_mean_pass == FALSE, list(acause, measure, diff_geo_mean, perc_diff_geo_mean)]))))

      msg_list <- append(msg_list, "\nFailures because calculated percentage was NA:")
      msg_list <- append(msg_list, capture.output(print(unique(geo_diff[na_percentage_pass == FALSE, list(acause, measure, diff_geo_mean, perc_diff_geo_mean)]))))

      msg_list <- append(msg_list, "\nFailures because difference was above tolerance, or NAs were calculated for difference metrics:")
      msg_list <- append(msg_list, paste0("\nTolerance level for diff_geo_mean is ", abs_tol))
      msg_list <- append(msg_list, paste0("\nTolerance level for perc_diff_geo_mean is ", rel_tol))
      msg_list <- append(msg_list, capture.output(print(unique(geo_diff[tolerance_pass == FALSE, list(acause, measure, diff_geo_mean, perc_diff_geo_mean)]))))
    } else {
      msg_list <- append(msg_list, "all geography checks passed!")
    }
  } else {
    msg_list <- append(msg_list, "Geography-checks not done. Caution, diff-files missing. Check /check_raked_result/geo_status.rds")
  }
  result <- list(
    messages = msg_list,
    failed = failed_causes,
    passed = setdiff(unique(geo_diff$acause), failed_causes)
  )
  return(result)
}
#' @title check_race_edu_diff
#'
#' @description Checks that differences in cause_diff data.table are not NaN, and that they fall within abs_tol and rel_tol
#' (I believe these are globally set in the calling scripts).  Appends appropriate information, warning and error messages
#' into returned list.
#'
#' @param race_edu_diff - [data.table] single data.table that is the concatenated set of individual race or edu diff files.

#'
#' @return list of messages generated by race checks, causes which failed, and causes which passed
#'
#' @rdname check_race_edu_diff
#' @export
check_race_edu_diff <- function(race_edu_diff, results_dir) {
  # update abs_tol from global environment if it exists
  abs_tol <- get("abs_tol", envir = .GlobalEnv)
  msg_list <- list()
  failed_causes <- vector("character")
  if (nrow(race_edu_diff) > 0) {
    race_edu_diff[, na_mean_pass := ifelse(is.na(diff_mean), FALSE, TRUE)]
    race_edu_diff[, na_percentage_pass := ifelse(is.na(perc_diff_mean), FALSE, TRUE)]
    race_edu_diff[, tolerance_pass := ifelse(as.numeric(diff_mean < abs_tol & perc_diff_mean < rel_tol) & na_mean_pass & na_percentage_pass,
      TRUE, FALSE
    )]

    if (nrow(race_edu_diff[na_mean_pass == FALSE | na_percentage_pass == FALSE | tolerance_pass == FALSE]) > 0) {
      failed_causes <- unique(race_edu_diff[na_mean_pass == FALSE | na_percentage_pass == FALSE | tolerance_pass == FALSE, acause])
      msg_list <- append(msg_list, "\nSome rows failed the race/edu checks:")

      msg_list <- append(msg_list, "\nFailures because calculated mean was NA:")
      msg_list <- append(msg_list, capture.output(print(unique(race_edu_diff[na_mean_pass == FALSE, list(acause, measure, diff_mean, perc_diff_mean)]))))

      msg_list <- append(msg_list, "\nFailures because calculated percentage was NA:")
      msg_list <- append(msg_list, capture.output(print(unique(race_edu_diff[na_percentage_pass == FALSE, list(acause, measure, diff_mean, perc_diff_mean)]))))

      msg_list <- append(msg_list, "\nFailures because difference was above tolerance, or NAs were calculated for difference metrics:")
      msg_list <- append(msg_list, paste0("\nTolerance level for diff_race_mean is ", abs_tol))
      msg_list <- append(msg_list, paste0("\nTolerance level for perc_diff_race_mean is ", rel_tol))
      msg_list <- append(msg_list, capture.output(print(unique(race_edu_diff[tolerance_pass == FALSE, list(acause, measure, diff_mean, perc_diff_mean)]))))
    } else {
      msg_list <- append(msg_list, "all race/edu checks passed!")
    }
  } else {
    msg_list <- append(msg_list, "FILEPATH")
  }
  result <- list(
    messages = msg_list,
    failed = failed_causes,
    passed = setdiff(unique(race_edu_diff$acause), failed_causes)
  )
  return(result)
}
