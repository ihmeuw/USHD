####################################################################################################
## Author:      INDIVIDUAL_NAME
##
## Description: Check that raking worked as it should have and then make diagnostic plots of the
##              impact of raking on the estimates. This script reads in final files, so it should
##              be ran after post_raking
##
##              Specifically check that:
##              1) Final estimates at the "area_var" level when aggregated to the "raking_area_var"
##                 level match the "raking_area_var"-level estimates from the standard. (i.e.,
##                 raking worked along the geography dimension).
##              2) Final estimates at all levels add up properly from child to parent cause
##                 (i.e., raking worked along the cause dimension)
##              3) Optional(if edu_race_comparison_dir is present): Final compile estimates for by_race or by_edu (model)
##                 when aggregated at "all race" or "all edu" level match aggregated compile estimates from the
##                 "edu_race_comparison" dir (model).
##
##              And then plot for each cause:
##              1) Line plots of the data, modeled estimates, and raked estimates (by area)
##              2) Line plots of the data, modeled estimates, and raked estimates (by age)
##              3) Maps of the area_var-level estimates in the first and most recent year,
##                  pre- and post-raking (also maps different in levels raked/unraked and ratio
##                  of raked/unraked levels)
##              4) Scatters of the pre- and post-raking estimates at the area_var-level
##              5) Scatters of the pre- and post-raking confidence interval widths for estimates at
##                 the area_var-level
##
## Inputs:      dir [character] -- main directory for settings and final output for all causes. If checking
##                  an r/e model, this will be the county_race_dir or the county_edu_dir
##              plot_level [character] -- level to display data and estimates plots at. Note that mcnty
##                  has not been implemented due to large numbers of plots, but could be by making
##                  a workaround to the geoagg_files logic
##              initial_level [integer] -- highest raking level to consider
##              terminal_level [integer] -- lowest raking level to consider
##              dont_recalc_validation [logical] -- if TRUE, then if the output already exists, that cause will
##                  be skipped in the loops.
##              threads [integer] -- number of threads to parallelize over
##              edu_race_comparison_dir [character] -- used for the edu or race check, this will be the county_dir.
##                  If not running an r/e model, this can be left out of the command line call.
##              children_of [character] -- Parent cause to subset to. If not NULL, causes will be subsetted to this
##                  parent and its immediate children.
##              measures [character] -- vector of measures ("mx" for Mortality rates, "yll" for Years of Life Lost,
##                   "yld" for Years Lived with Disablity, and "pred" for risk exposure) to validate. Note: yld and pred cannot be checked together
##                  with mx and yll
##              delete_temp [logical] -- If True, on passing checks for raked results, temporary files for the specified
##                  cause/level will be deleted.
##              dir_with_data [character] -- Data directory used for plotting.
##
####################################################################################################

# FILEPATH/projects/LSAE/repos/lbd.loader/browse
library(lbd.loader, lib.loc = sprintf("'FILEPATH'", R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
suppressMessages(lbd.loader::load.containing.package())

library(gridExtra)
library(sf)

## Get settings ------------------------------------------------------------------------------------

if (interactive()) {

  dir <- "FILEPATH"
  edu_race_comparison_dir <- NULL
  plot_level <- "state"
  initial_level <- 0
  terminal_level <- 0
  dont_recalc_validation <- FALSE
  children_of <- NULL#  "cvd" 
  measures <- "pred"
  delete_temp <- FALSE
  threads <- 4
  dir_with_data <- "FILEPATH"
  raking_est_dir <- NULL #"FILEPATH"
  measure_id <- NULL # 4 # means YLLs

} else {
  parser <- argparse::ArgumentParser()
  parser$add_argument("--dir", help = "Model run dir")
  parser$add_argument("--plot_level", choices = c("natl", "state"))
  parser$add_argument("--initial_level", type = "integer", choices = 0:4)
  parser$add_argument("--terminal_level", type = "integer", choices = 0:4)
  parser$add_argument("--dont_recalc_validation", choices = c("TRUE", "FALSE"))
  parser$add_argument("--edu_race_comparison_dir", help = "all-race or all-edu model run dir for dimension check", default = NULL)
  # `action = "append"` does not support `default = c("mx", "yll")`
  parser$add_argument("--measures", action = "append", help = "Vector of measures to validate.")
  parser$add_argument("--children_of", type = "character", default = NULL, help = "Parent cause to subset to.")
  parser$add_argument("--delete_temp",
                      default = FALSE,
                      choices = c("TRUE", "FALSE"),
                      help = paste0(
                        "If True, on passing checks for raked results, temporary files for the specified ",
                        "cause/level will be deleted."
                      )
  )
  default_threads <- as.integer(Sys.getenv("SGE_HGR_fthread", unset = "1"))
  default_threads <- ifelse(default_threads > 1L, default_threads - 1L, default_threads)
  parser$add_argument("--threads", type = "integer", default = default_threads)
  parser$add_argument("--dir_with_data", default = NULL, help = "Data directory - used for plotting")
  add_measure_id_argument(parser)


  args <- parser$parse_args(get_args())
  print(args)

  dir <- args$dir
  plot_level <- args$plot_level
  initial_level <- args$initial_level
  terminal_level <- args$terminal_level
  dont_recalc_validation <- as.logical(args$dont_recalc_validation)
  edu_race_comparison_dir <- args$edu_race_comparison_dir
  children_of <- args$children_of
  measures <- args$measures
  delete_temp <- args$delete_temp
  threads <- args$threads
  dir_with_data <- args$dir_with_data
  measure_id <- args$measure_id


  # If measures was not passed, set it to c("mx", "yll") for default behavior.
  if (length(measures) == 0) {
    measures <- c("mx", "yll")
  }
}

print("Settings passed are:")
for (setting in names(args)) {
  print(paste0(setting, ": ", get(setting)))
}

model_settings <- ModelSettings$from_dir(dir)
submitted_causes <- model_settings$submitted_causes
model_settings$update_env(.GlobalEnv) 

if(identical(measures, "pred")){
  # if the model is using the old RF race codes, the "race_default" (i.e., all race)
  # should be 9 instead of 1
  if(setequal(model_settings$races, c(1,2,3,4,7))){
    overwrite_constants(new_race_default = 9)
    race_default <- sae.shared::race_default
  } else{
    stopifnot(setequal(model_settings$races, c(2,4,5,6,7)))
  }
}

# if measure is "ab", increase abs_tol to 1e-8
if("ab" %in% measures){
  new_abs_tol <- 1e-8
  orig_abs_tol <- abs_tol
  abs_tol <- max(new_abs_tol, abs_tol)
  if(abs_tol != orig_abs_tol){
    warning('settings abs_tol to ', abs_tol, ' for measure "ab" (was ', orig_abs_tol, ')')
  }
  rm(new_abs_tol, orig_abs_tol)
}

# set years for AB
if("ab" %in% measures){
  # ModelSettings$from_dir(dir) sets the years as begin_year:end_year
  # Sometimes, we only want to run AB for a selection of years.
  # Check that the years in model_settings is limited to years run.
  # Construct based on metadata.json in dir
  json_settings <- fromJSON(txt = paste0(dir, "/metadata.json"))
  json_settings <- json_settings$`cli-args`
  years <- json_settings$year_ids
  model_settings$years <- years
}

shape_file = "FILEPATH/mcnty_mapping_shape_file.rds"

# "yld" cannot be checked with "yll" or "mx"
valid_measures <- c("mx", "yll", "yld", "pred", "ab")
invalid_measures <- setdiff(measures, valid_measures)
if (length(invalid_measures) > 0) {
  msg <- paste0(
    "Valid measures are: ", paste(valid_measures, collapse = ", "),
    ". Invalid measure/s provided: ", paste(invalid_measures, collapse = ", ")
  )
  lsae.utils::stop_or_quit(msg, status = 1)
} else if ("yld" %in% measures && !identical("yld", measures)) {
  msg <- paste0("Raked results for yld cannot be checked together with other measures, was given: ", paste(measures, collapse = ", "))
  lsae.utils::stop_or_quit(msg, status = 1)
}

# if ab is in measures, we need to get the measure ID (which refers to whether 
# attributable burden is YLL, mx, YLD, DALYs, etc.)
if("ab" %in% measures){
  # check that measure_id is not NULL
  if(is.null(measure_id)){
    lsae.utils::stop_or_quit("measure_id must be provided when 'ab' is in measures", status = 1)
  }
}

# get information necessary to save plots in the limited use folder
if (any(measures %in% c("mx", "yll"))) {
  if (is.null(dir_with_data)) {
    lsae.utils::stop_or_quit("dir_with_data not specified!", status = 1)
  } else if (!dir.exists(dir_with_data)) {
    lsae.utils::stop_or_quit("dir_with_data does not exist or you don't have permissions to access it.", status = 1)
  }
  data_folder <- tail(unlist(strsplit(dir_with_data, "/")), n = 1)
  lu_modeldir <- paste0("FILEPATH", LU_folder, "FILEPATH", data_folder)
}

if (by_race && (is.null(edu_race_comparison_dir) || !dir.exists(edu_race_comparison_dir))) {
  warnings("by_race is T but valid all-race directory not provided, race check will be skipped.")
  skip_all_race <- T
} else{
  skip_all_race <- F
}

if (by_edu && (is.null(edu_race_comparison_dir) || !dir.exists(edu_race_comparison_dir))) {
  warnings("by_edu is T but valid all-edu directory not provided, edu check will be skipped.")
}

if (n.sims < 1000) {
  warnings(paste0(
    "WARNING\nn.sims is set to a value less than 1000. Only the first ",
    n.sims,
    " draws raked results will not match GBD results."
  ))
}

# cause list
causes <- data.table()
for (cause in names(submitted_causes)) {
  subset <- submitted_causes[[cause]]
  subset <- subset[names(subset) %in% c(
    "level", "cause_id", "cause_outline",
    "parent_id", "path_to_top_parent", "run_model"
  )]
  causes <- rbind(causes, c("acause" = cause, subset), fill = TRUE)
}

causes <- annotate_post_model_workflow(causes)
# We don't want all-cause to be a child of itself, so make its parent cause 0.
causes[cause_id == 294, parent_id := 0]
causes <- causes[!cause_id %in% cause_id_exclusion_list] # get rid of weird reporting aggregates
# create an indicator of "leaf_cause", i.e., the lowest levels of the cause hierarchy.
# TRUE if cause_id is not in parent_id, FALSE otherwise.
causes[, leaf_cause := !cause_id %in% parent_id]
setkey(causes, acause)

if (nrow(causes) == 0) {
  lsae.utils::stop_or_quit(paste0("No causes to check!"))
}

# If 'children_of' is specified, subset down to just this parent
if (!is.null(children_of)) {
  if (!children_of %in% unique(causes$acause)) {
    lsae.utils::stop_or_quit(paste0("children_of argument is invalid! Was passed: ", children_of))
  }
  causes <- causes[(acause == children_of) | parent_id %in% causes[acause == children_of, cause_id]]
  # parent cause (children_of) overtakes initial/terminal level, so re/assign them
  initial_level <- terminal_level <- max(causes[, level])

  message(
    "children_of argument passed: ", children_of,
    " so re/setting initial_level: ", initial_level, " and terminal_level: ", terminal_level
  )
}

# dir to store results
results_dir <- paste0(dir, "/check_raked_results")
lsae.utils::make_group_writable_dir(results_dir)

# make dirs to store results
geo_diff_dir <- paste0(dir, "FILEPATH")
lsae.utils::make_group_writable_dir(geo_diff_dir)

cause_diff_dir <- paste0(dir, "FILEPATH")
lsae.utils::make_group_writable_dir(cause_diff_dir)

race_diff_dir <- paste0(dir, "FILEPATH")
lsae.utils::make_group_writable_dir(race_diff_dir)

edu_diff_dir <- paste0(dir, "FILEPATH")
lsae.utils::make_group_writable_dir(edu_diff_dir)

#' Get the vector of causes whose diff files already exist.
#'
#' @param dir [character]  model dir
#' @param diff_type [character] type of diff files to look for, e.g. "cause_diff", "race_diff", etc.
#'
#' @return vector of causes that have existing diff files.
finished_causes <- function(dir, diff_type) {
  if (!(diff_type) %in% c("geo_diff", "cause_diff", "race_diff", "edu_diff")) {
    stop("diff_type must be one of 'geo_diff', 'cause_diff', 'race_diff', or 'edu_diff'")
  }
  acauses <- sapply(list.files(dir), function(file) gsub(x = file, pattern = paste0("_", diff_type, ".rds"), replacement = ""))
  names(acauses) <- NULL
  return(acauses)
}

finished_geo_diff <- finished_causes(geo_diff_dir, "geo_diff")
finished_cause_diff <- finished_causes(cause_diff_dir, "cause_diff")
finished_race_diff <- finished_causes(race_diff_dir, "race_diff")
finished_edu_diff <- finished_causes(edu_diff_dir, "edu_diff")

# load exclusions for check_raked_results if present
exclusions <- get_check_rake_exclusions(dir)

#' Changes fail-messages in diff_results to pass-messages for the causes due to check_rake_exclusions.
#'
#' @param diff_results [list] list of messages generated by cause checks, causes which failed, and causes which passed
#' @param check_var [character] check types: cause_check, geo_check or race_check
#'
#' @return adjusted list of diff_results with pass/fail messages with respect to exclusions.
adjust_diff_results_by_exclusions <- function(diff_results, check_var) {
  for (cause in diff_results$failed) {
    for (measure in measures) {
      if (cause %in% names(exclusions) && measure %in% exclusions[[cause]]) {
        message(check_var, "_diff_results passed due to check_rake_exclusions for cause: ", cause, ", measure: ", measure)
        diff_results$failed <- diff_results$failed[diff_results$failed != cause]
        diff_results$passed <- append(diff_results$passed, cause)
      }
    }
  }
  if (length(diff_results$failed) > 0) {
    message("Causes that remain failed for ", check_var, " check are: ", diff_results$failed)
    message("Causes that passed ", check_var, " check are: ", diff_results$passed)
  }
  return(diff_results)
}

#' Prints messages from check_status and errors if the message contains "caution".
#'
#' @param check_status [list] list with '.message' and '.call' components. This is returned by foreach loop in
#'        cause_status, geo_status or race_status checks.
print_output_from_foreach <- function(check_status) {

  status_loop <- unlist(check_status)
  for (name in names(status_loop)) {
    if (str_detect(name, "call")) {
      next
    } else if (str_detect(tolower(status_loop[[name]]), "caution")) {
      lsae.utils::stop_or_quit(paste0(name, " ", status_loop[[name]]), status = 3)
    } else {
      message(name)
      message("  ", status_loop[[name]])
    }
  }
  message("\n")
}

#' Generate status report for children of parents in cause tree
#'
#' @param causes [data.table] the cause tree
#' @param parents [character] one or more acause values in the tree to report on the children of
#'
#' @return list with $check and $message values. $check indicates whether any children should be checked and $message provides a human-readable explanation.
child.check.status <- function(causes, parents) {
  children <- causes[parent_id %in% causes[acause %in% parents, cause_id], acause]

  if (length(children) == 0) {
    list(
      message = sprintf("%s has no children, skipping", paste(parents, collapse = "/")),
      check = FALSE
    )
  } else if (!any(causes[acause %in% children, run_model])) {
    list(
      message = sprintf("%s has all un-modeled children, skipping", paste(parents, collapse = "/")),
      check = FALSE
    )
  } else {
    list(
      message = sprintf("%s has children to check", paste(parents, collapse = "/")),
      check = TRUE
    )
  }
}

for (i in initial_level:terminal_level) {
  print(paste0("Running check at level ", i))

  # Grab the causes that should be checked at this level.
  # When "children_of" argument is provided, parent_causes_to_check are actually child-causes of "children_of".
  parent_causes_to_check <- causes[level == i, unique(acause)]

  # (acause == "_all") has no parent so, don't subset to its parent cause.
  if (i == 0) {
    parent_causes_to_check_cause <- parent_causes_to_check
  } else {
    parent_causes_to_check_cause <- causes[level == (i - 1), unique(acause)]
  }
  
  print("Parent causes that will be checked are: ")
  print(parent_causes_to_check_cause)

  # foreach settings
  capture.output(cl <- makeCluster(threads, type = "FORK", outfile = ""), file = "FILEPATH")
  registerDoParallel(cl)

  ## Check that raking worked along the cause dimension ----------------------------------------------
  cat("\n\nCheck for differences along the cause dimension \n")
  cause_status <- foreach(this_parent = parent_causes_to_check_cause, .packages = c("data.table"), .errorhandling = "pass") %dopar% {
    cause_status_sub <- foreach(measure = measures, .errorhandling = "pass") %do% {
      # cause check for "yld" not required
      if (identical("yld", measure)) {
        return(paste0("cause-check for ", measure, " ", this_parent, " not required, skipping."))
      }
      if (identical("pred", measure)) {
        return(paste0("cause-check for ", measure, " ", this_parent, " not required, skipping."))
      }
      if ((dont_recalc_validation) & (paste0(measure, "_", this_parent) %in% finished_cause_diff)) {
        return(paste0(measure, " ", this_parent, " exists, skipping."))
      }

      # load and sum up the results for the children causes
      message(measure, " ", this_parent, " - Loading children causes")
      children <- causes[parent_id == causes[acause == this_parent, cause_id], list(acause)]

      status <- child.check.status(causes, this_parent)

      # skip cause check for `_all` or if skipped by child.check.status()
      if (i == 0 & terminal_level == 0) {
        msg <- "No cause check because looking at _all (and _all has no parent)"
        return(msg)
      } else if (!status$check) {
        msg <- paste(measure, status$message)
        return(msg)
      }

      child_est <- rbindlist(lapply(children[, acause], function(this_child) {
        if(measure == "ab"){
          child_path <- get_compiled_estimates_path_ab(root = dir, cause = this_child, raked = TRUE, years = years, measure_id = measure_id)
        } else {
          child_path <- paste0(dir, "/", this_child, "/", measure, "_est_all_raked.rds")
        }
        
        message(paste0("Loading ", child_path))
        if (!causes[acause == this_child, compile_model]) {
          message(paste0("child cause ", this_child, " has compile_model set to FALSE, skipping"))
          return()
        } else if (!all(file.exists(child_path))) {
          message("caution, child cause file missing (", child_path, ")")
          message(paste0("caution, make sure the child cause: ", this_child, " at level: ", i + 1, " has been aggregated!"))
          return()
        }
        dt <- rbindlist(lapply(child_path, readRDS), use.names = T) # for ab, we have separate files by year
        dt[, child_cause := this_child]

        if ("value_mean" %in% names(dt)) {
          message(paste0("Correcting schema for file ", child_path))
          setnames(dt, c("value_mean", "value_lb", "value_ub", "value_se"), c(paste0(measure, c("_mean", "_lb", "_ub", "_se"))))
        }

        if ("id" %in% names(dt)) dt[, id := NULL]
        return(dt)
      }))


      # generalize column names to work for both yll and mx
      setnames(child_est, c(paste0(measure, c("_mean", "_lb", "_ub", "_se"))), c("value_mean", "value_lb", "value_ub", "value_se"))
      # if measure is ab, filter to metric_id 1 (count) and 3 (rates)
      if(measure == "ab"){
        child_est <- child_est[metric_id %in% c(1,3)]
      } else {
        # otherwise, check that metric_id is NOT present. 
        # Then fill in metric_id with NA
        if(!"metric_id" %in% names(child_est)){
          child_est[, metric_id := NA]
        }
      }
      child_est <- child_est[, list(child_value = sum(value_mean)), keyby = "level,area,year,sex,race,age,metric_id"]

      # load results for the parent cause
      message(measure, " ", this_parent, " - Loading parent cause")
      
      if(measure == "ab"){
        parent_path <- get_compiled_estimates_path_ab(root = dir, cause = this_parent, raked = TRUE, years = years, measure_id = measure_id)
      } else {
        parent_path <- paste0(dir, "/", this_parent, "/", measure, "_est_all_raked.rds")
      }
      
      message(paste0("Loading ", parent_path))
      if (!causes[acause == this_parent, aggregate_model]) {
        print(paste0("caution ", measure, " ", this_parent, " has aggregate_model set to FALSE, but at least one child is modeled"))
        stop(paste0("caution ", measure, " ", this_parent, " has aggregate_model set to FALSE, but at least one child is modeled"))
      } else if (!all(file.exists(parent_path))) {
        print(paste0(measure, " ", this_parent, " caution final file missing, skipping"))
        stop(paste0(measure, " ", this_parent, " caution final file missing, skipping"))
      }
      parent_est <- rbindlist(lapply(parent_path, readRDS), use.names = T)
      setnames(parent_est, paste0(measure, "_mean"), "value_mean")
      # if measure is ab, filter to metric_id 1 (count) and 3 (rate)
      if(measure == "ab"){
        parent_est <- parent_est[metric_id %in% c(1,3)]
      } else {
        # otherwise, check that metric_id is NOT present. 
        # Then fill in metric_id with NA
        if(!"metric_id" %in% names(parent_est)){
          parent_est[, metric_id := NA]
        }
      }
      parent_est <- parent_est[, list(level, area, year, sex, race, age, parent_value = value_mean, metric_id)]

      # merge and compare
      message(measure, " ", this_parent, " - Merging and comparing")
      est <- merge(child_est, parent_est, by = c("level", "area", "year", "sex", "race", "age", "metric_id"))

      # Create comparison statistics
      est[, diff_cause_mean := abs(parent_value - child_value)]
      est[, perc_diff_cause_mean := 100 * (abs(parent_value - child_value) / parent_value)]
      est[parent_value == 0 & child_value == 0, perc_diff_cause_mean := 0] # If both are zero, you get NAs in the percentage calculation

      if (nrow(est[is.na(diff_cause_mean) | is.na(perc_diff_cause_mean)]) != 0) {
        return("Caution, NAs not allowed in validation columns diff_cause_mean and perc_diff_cause_mean")
      }

      # Show the number of points that fail the tolerance criteria, as a number and a percentage,
      # the unique set of years, sexes, ages, and geographic regions that fail,
      # and show the worst example. Write all failing rows to a CSV.
      failures <- est[diff_cause_mean > abs_tol | perc_diff_cause_mean > rel_tol]
      if (nrow(failures) == 0) {
        est <- est[, list(
          acause = this_parent,
          diff_cause_mean = max(diff_cause_mean),
          perc_diff_cause_mean = max(perc_diff_cause_mean)
        ), by = metric_id]
        est[, measure := measure]
        saveRDS(est, file = paste0(cause_diff_dir, "/", measure, "_", this_parent, "_cause_diff.rds"))
        return("finished successfully")
      } else {
        num_failures <- nrow(failures)
        pct_failures <- 100 * nrow(failures) / nrow(est)
        year_failures <- paste0(unique(failures$year), collapse = ",")
        sex_failures <- paste0(unique(failures$sex), collapse = ",")
        race_failures <- paste0(unique(failures$race), collapse = ",")
        age_failures <- paste0(unique(failures$age), collapse = ",")

        status <- failures[, .(
          acause = this_parent, child_value, parent_value,
          diff_cause_mean, perc_diff_cause_mean
        )][order(-perc_diff_cause_mean)][1:5] # Start by capturing the five "worst offenders"
        for (status_var in c(
          "num_failures", "pct_failures", "year_failures",
          "sex_failures", "race_failures", "age_failures"
        )) {
          status[, (status_var) := get(status_var)]
        }

        saveRDS(status, file = paste0(cause_diff_dir, "/", measure, "_", this_parent, "_cause_diff.rds"))
        saveRDS(failures, file = paste0(cause_diff_dir, "/", measure, "_", this_parent, "_all_cause_failures.rds"))
        return(paste0(
          "Caution, some cause checks failed. See file: ",
          cause_diff_dir, "/", measure, "_", this_parent, "_all_cause_failures.rds"
        ))
      }

      rm(est, child_est, parent_est, failures)
    }

    # Since we often run check_raked_results() using children_of, or for a particular level, we should save
    # the cause status by parent cause
    saveRDS(cause_status_sub, file = paste0(results_dir, "FILEPATH", measure, "_cause_status_", this_parent, "_lvl_", i, ".rds"))

    names(cause_status_sub) <- paste0(measures, this_parent)
    return(cause_status_sub)
  }

  stopCluster(cl)
  gc()
  message("cause check complete \n")

  # print the output, error if output string has "caution"
  print_output_from_foreach(cause_status)

  saveRDS(cause_status, file = paste0(results_dir, "/cause_status.rds"))


  capture.output(cl <- makeCluster(threads, type = "FORK", outfile = ""), file = "FILEPATH")
  registerDoParallel(cl)

  ## Check that raking worked along the geography dimension ------------------------------------------
  cat("\n\nCheck for differences along the geography dimension\n")
  geo_status <- foreach(this_cause = parent_causes_to_check, .packages = c("data.table"), .errorhandling = "pass") %dopar% {
    geo_status_sub <- foreach(measure = measures, .errorhandling = "pass") %do% {
      if ((dont_recalc_validation) & (paste0(measure, "_", this_cause) %in% finished_geo_diff)) {
        return(paste0(measure, " ", this_cause, " already exists, skipping."))
      }

      if(measure == "ab" & causes[acause == this_cause, leaf_cause] == FALSE){
        return(paste0("Skipping geo_check for ", measure, " ", this_cause, " check because it is not the most detailed cause/leaf cause"))
      }

      if (identical(measure, "pred")) {
        # cause_name (e.g. overweight) is used for getting correct cause_id while loading GBD draws for BMI-prevalence.
        this_cause <- basename(dirname(dir))
        this_cause_hierarchy = "_all"
        this_cause_path = "est" # b/c the RF directory doesn't have cause names in the model dirs -- the ests live in "model_dir/est"
      } else{
        this_cause_path <- this_cause
        this_cause_hierarchy <- this_cause
      }

      # load lower results files
      message(measure, " ", this_cause, " - Loading lower results")

      if(identical(measure, "ab")){
        lower_path <- get_compiled_estimates_path_ab(root = dir, cause = this_cause_path, raked = TRUE, years = years, measure_id = measure_id)
      } else {
        lower_path <- paste0(dir, "/", this_cause_path, "/", measure, "_est_all_raked.rds")
      }
      

      if (!causes[acause == this_cause_hierarchy, aggregate_model]) {
        print(paste0(measure, " ", this_cause, " has aggregate_model set to FALSE, skipping"))
        stop(paste0(measure, " ", this_cause, " has aggregate_model set to FALSE, skipping"))
      } else if (!all(file.exists(lower_path))) {
        message(paste0("caution, lower files for ", this_cause, " missing, skipping"))
        stop(paste0("caution, lower files for ", this_cause, " missing, skipping"))
      }

      lower <- rbindlist(lapply(lower_path, readRDS), use.names = T)
      # generalize column names to work for both yll and mx
      setnames(lower, c(paste0(measure, c("_mean", "_lb", "_ub", "_se"))), c("value_mean", "value_lb", "value_ub", "value_se"))
      if(measure == "ab"){
        lower <- lower[metric_id %in% c(3)]
      } 
      lower <- lower[J(raking_area_var), on = "level", list(area, year, sex, race, edu, age, value_mean, value_lb, value_ub)] # can drop the metric_id here since we are only including rates

      # add location_id
      lower[, level := raking_area_var]
      lower <- add_gbd_location_ids(data = lower, natl_area = 1)
      lower[, level := NULL]

      # subset to all race
      lower <- lower[race == race_default]
      lower[, race := NULL]

      # subset to all edu
      lower <- lower[edu == edu_default]
      lower[, edu := NULL]

      # load upper results files
      message(measure, " ", this_cause, " - Loading upper results")
      if (rake_to_gbd) {
        # This upper geography draws loading takes a long time - so results are cached for repeat runs.
        cache_filename <- paste0(geo_diff_dir, "/", this_cause, "_", measure, "_gbd_results_cache.RDS")

        if (file.exists(cache_filename)) {
          upper <- readRDS(cache_filename)
          # error if all years are not present in cached file
          if (!all(years %in% unique(upper$year))) {
            stop(paste0("Caution, not all years present in cached file: ", cache_filename))
          }
        } else {
          # load GBD estimates

          to_settings <- list(
            to_geo = raking_area_var,
            rake_to_gbd_version = rake_to_gbd_version,
            ages = ages,
            n.sims = n.sims,
            area_var = raking_area_var
          )
          to_settings <- GBDSettings$new(gbd_settings = to_settings)

          ## get population because we need this for processing the GBD draws
          # get population so that mortality rates can be made
          message(measure, this_cause, " - Loading population")

          pop <- load_population(pop_file)

          pop <- pop[year %in% years & age %in% ages]

          # load GBD estimates
          if(identical(measure, "ab")){
            upper_draws <- to_settings$load_draws(
              acause = this_cause,
              measure = measure,
              year = years,
              sex = sexes,
              raked = "raked",
              population = pop,
              measure_id = measure_id,
              metric_id = 3, # rates  
              location_id = ushd_state_location_ids # defined in constants.r -- for measure AB, the location_id(s) need to be defined for get_gbd_upper_draws() (defined in load_save.r)
            )
          
            
          } else {
            upper_draws <- to_settings$load_draws(
              acause = this_cause,
              measure = measure,
              year = years,
              sex = sexes,
              raked = "raked",
              population = pop
            )
          }
          
          # collapse draws
          upper <- copy(upper_draws)
          setnames(upper, c(raking_area_var), c("area"))
          upper <- collapse_draws(upper, "value", c("year", "sex", "age", "area"))

          # put names back
          setnames(upper, c("year", "sex", "value_lb", "value_ub", "value_mean"), c("year_id", "sex_id", "lower", "upper", "value"))

          upper <- upper[, list(
            year = year_id, sex = sex_id,
            area = area,
            age = age,
            value_mean = value, value_lb = lower, value_ub = upper
          )]

          saveRDS(upper, file = cache_filename)
        }
      } else {
        message(measure, " ", this_cause, " - 'rake_to_gbd' is false, loading upper values from ", measure, "_est_all_raked file")
        upper <- readRDS(paste0(paste0(raking_est_dir, "/", this_cause_path), "/", measure, "_est_all_raked.rds"))
        setnames(upper, c(paste0(measure, c("_mean", "_lb", "_ub", "_se"))), c("value_mean", "value_lb", "value_ub", "value_se"))
        upper <- upper[J(raking_area_var, unique(lower$area)), list(area, year, sex, age, value_mean, value_lb, value_ub, value_se)]
      }

      # ignore confidence intervals for ages that have been collapsed (CI can't really be added)
      if (raking_area_var %in% c("natl", "state")) { # i.e., GBD, where age 0 has been collapsed from ENN, LNN, and PNN
        lower[age == 0, `:=`(value_lb = NA, value_ub = NA)]
        upper[age == 0, `:=`(value_lb = NA, value_ub = NA)]
      }

      # merge and compare
      message(measure, " ", this_cause, " - Merging and comparing")
      est <- merge(lower, upper, by = c("area", "year", "sex", "age"), suffixes = c("_lower", "_upper"))
      stopifnot(nrow(est) > 0)
      est <- est[age != 99, ] # this won't match if different age standards were used (e.g., GBD vs US standard)

      # Apply exception for males 1, 5, and 10 in the "_subs" tree
      est <- substance_abuse_males_gbd6_geo(rake_to_gbd_version, this_cause, est)

      # Create comparison statistics
      est[, diff_geo_mean := abs(value_mean_upper - value_mean_lower)]
      est[, perc_diff_geo_mean := 100 * abs(value_mean_upper - value_mean_lower) / value_mean_upper]
      est[, diff_geo_lb := abs(value_lb_upper - value_lb_lower)]
      est[, perc_diff_geo_lb := 100 * abs(value_lb_upper - value_lb_lower) / value_lb_upper]
      est[, diff_geo_ub := abs(value_ub_upper - value_ub_lower)]
      est[, perc_diff_geo_ub := 100 * abs(value_ub_upper - value_ub_lower) / value_ub_upper]
      est[, measure := measure]

      # Correct for divide-by-zero errors
      est[value_mean_upper == 0 & value_mean_lower == 0, perc_diff_geo_mean := 0]

      if (nrow(est[is.na(diff_geo_mean) | is.na(perc_diff_geo_mean)]) != 0) {
        return("caution, NAs not allowed in validation columns diff_geo_mean and perc_diff_geo_mean")
      }

      # Show the number of points that fail the tolerance criteria, as a number and a percentage,
      # the unique set of years, sexes, ages, and geographic regions that fail,
      # and show the worst example. Write all failing rows to a CSV.
      failures <- est[diff_geo_mean > abs_tol | perc_diff_geo_mean > rel_tol]
      if (nrow(failures) == 0) {
        est <- est[, list(
          acause = this_cause,
          diff_geo_mean = max(diff_geo_mean),
          perc_diff_geo_mean = max(perc_diff_geo_mean, na.rm = T),
          diff_geo_lb = max(diff_geo_lb, na.rm = T),
          perc_diff_geo_lb = max(perc_diff_geo_lb, na.rm = T),
          diff_geo_ub = max(diff_geo_ub, na.rm = T),
          perc_diff_geo_ub = max(perc_diff_geo_ub, na.rm = T)
        )]
        est[, measure := measure]
        saveRDS(est, file = paste0(geo_diff_dir, "/", measure, "_", this_cause, "_geo_diff.rds"))
        return("finished successfully")
      } else {
        num_failures <- nrow(failures)
        pct_failures <- 100 * nrow(failures) / nrow(est)
        year_failures <- paste0(unique(failures$year), collapse = ",")
        sex_failures <- paste0(unique(failures$sex), collapse = ",")
        
        age_failures <- paste0(unique(failures$age), collapse = ",")

        status <- failures[, .(
          acause = this_cause, value_mean_lower, value_mean_upper,
          diff_geo_mean, perc_diff_geo_mean, diff_geo_lb,
          perc_diff_geo_lb, diff_geo_ub, perc_diff_geo_ub
        )][order(-perc_diff_geo_mean)][1:5] # Start by capturing the five "worst offenders"
        for (status_var in c(
          "num_failures", "pct_failures", "year_failures",
          "sex_failures", "age_failures"
        )) {
          status[, (status_var) := get(status_var)]
        }

        saveRDS(status, file = paste0(geo_diff_dir, "/", measure, "_", this_cause, "_geo_diff.rds"))
        saveRDS(failures, file = paste0(geo_diff_dir, "/", measure, "_", this_cause, "_all_geo_failures.rds"))
        return(paste0(
          "Caution, some geo checks failed. See file: ",
          geo_diff_dir, "/", measure, "_", this_cause, "_all_geo_failures.rds", " and: ",
          geo_diff_dir, "/", measure, "_", this_cause, "_geo_diff.rds"
        ))
      }

      rm(est, lower, upper, failures)
    }

    saveRDS(geo_status_sub, file = paste0(results_dir, "FILEPATH", measure, "_geo_status_", this_cause, "_lvl_", i, ".rds"))

    names(geo_status_sub) <- paste0(measures, this_cause)
    return(geo_status_sub)
  }

  stopCluster(cl)
  gc()
  message("geo check complete \n")

  # print the output, error if output string has "caution"
  print_output_from_foreach(geo_status)

  saveRDS(geo_status, file = paste0(results_dir, "/geo_status.rds"))



  ## Create general check to validate race/education aggregates -------------------------------------------
  validate_race_edu_aggregates <- function(type, parent_causes_to_check, measures, causes, finished_diff, diff_dir) {

    if (!type %in% c("race", "edu")) {
      lsae.utils::stop_or_quit(sprintf("type must be one of 'race' or 'edu'. Was passed %s", type))
    }

    capture.output(cl <- makeCluster(threads, type = "FORK", outfile = ""), file = "FILEPATH")
    registerDoParallel(cl)

    # This check compares the aggregated model estimates (all-race or all-edu) with the aggregates
    # from the by-race or by-edu model.
    cat(sprintf("\n\nCheck for differences between all-%s and by-%s estimates\n", type, type))
    status <- foreach(this_cause = parent_causes_to_check, .packages = c("data.table"), .errorhandling = "pass") %dopar% {
      status_sub <- foreach(measure = measures, .errorhandling = "pass") %do% {
        if ((dont_recalc_validation) & (paste0(measure, "_", this_cause) %in% finished_diff)) {
          print(paste0(measure, " ", this_cause, " exists, skipping."))
          stop(paste0(measure, " ", this_cause, " exists, skipping."))
        }

        # If this_cause is not modeled all_edu_files are not created
        if (!causes[acause == this_cause, run_model]) {
          print(paste0(measure, " ", this_cause, " is not modeled, skipping."))
          stop(paste0(measure, " ", this_cause, " is not modeled, skipping."))
        } else {
          print(paste0("run_model is not zero for cause: ", this_cause))
        }

        message(measure, " ", this_cause, " - Loading estimates")
        # load in mcnty raked data from county_dir
        # _all files get saved out in rake_by_geography_re rather than rake_by_geography_and_cause_re and include
        # all age groups, so it has one less digit in naming schema (year_sex_edu[_age])
        if (!getOption("ushd.use_edu_paths")) {
          pattern <- sprintf(
            "%s_est_mcnty_[0-9]+_[0-9]+_[0-9]+_raked.rds",
            measure
          )
        } else {
          pattern <- sprintf(
            "%s_est_mcnty_[0-9]+_[0-9]+_[0-9]+_[0-9]+_raked.rds",
            measure
          )
        }

        all_files <- list.files(
          path = file.path(edu_race_comparison_dir, this_cause),
          pattern = pattern,
          full.names = T
        )

        all_est <- rbindlist(lapply(all_files, function(file_path) {
          if (!file.exists(file_path)) {
            message("caution, filepath missing (", file_path, ")")
            return()
          }
          readRDS(file_path)
        }), use.names = T, fill = T) # Had to change this to fill=T because some files have acause and edu

        if (nrow(all_est) == 0) {
          stop(paste0(measure, " ", this_cause, " caution, all-", type, " files missing"))
        }

        # load in compiled estimates from by-race or by-edu directory and subset to mcnty and all-race/edu
        by_est <- load_compiled_estimates(dir, this_cause, measure, raked = T, use_adjusted = 1)

        # find values where the type column has a value of {type}_default
        # e.g., race == race_default OR edu == edu_default
        # build our mask indirectly via substitute(). This will be unevaluated so we must eval() it
        mask <- substitute(
          column == desired_value,
          # substitute these values
          env = list(
            # column will be e.g., race OR edu. NOT the string "race" or "edu"
            column = as.symbol(type),
            # desired_value will be race_default OR edu_default
            desired_value = as.symbol(paste0(type, "_default"))
          )
        )
        # this effectively subsets by_est to the rows where edu or race is equal to the all-edu/all-race value
        by_est <- by_est[level == "mcnty" & eval(mask), ]

        setnames(all_est, paste0(measure, "_mean"), "all_mean")
        setnames(by_est, paste0(measure, "_mean"), "by_mean")

        all_est <- all_est[, list(level, area, year, sex, age, all_mean)]
        by_est <- by_est[, list(level, area, year, sex, age, by_mean)]

        # merge and compare
        message(measure, " ", this_cause, " - Merging and combining")
        
        est <- merge(by_est, all_est, by = c("level", "area", "year", "sex", "age"))

        # Create comparison statistics
        est[, diff_mean := abs(all_mean - by_mean)]
        est[, perc_diff_mean := 100 * (abs(all_mean - by_mean) / all_mean)]
        est[all_mean == 0 & by_mean == 0, perc_diff_mean := 0] # If both are zero, you get NAs in the percentage calculation

        if (nrow(est[is.na(diff_mean) | is.na(perc_diff_mean)]) != 0) {
          return("caution, NAs not allowed in validation columns diff_mean and perc_diff_mean")
        }



        # Show the number of points that fail the tolerance criteria, as a number and a percentage,
        # the unique set of years, sexes, ages, and geographic regions that fail,
        # and show the worst example. Write all failing rows to a CSV
        failures <- est[diff_mean > abs_tol | perc_diff_mean > rel_tol]

        # add exception for zero pop in some segments of the population that
        # lead to differences in raked estimates, originating during sex
        # aggregation. See function for full description.
        if (nrow(failures) > 0) { # don't need to run this if there aren't any failures
          if (type == "edu") {
            failures <- exception_zero_pop_sex_agg_raking_diff(failures)
          } else {
            failures[, zero_pop_sex_agg_exception := FALSE] # fill placeholder column if this run isn't by-edu
          }
        }

        # Rows are marked with TRUE when the exception is applied. By dropping
        # the rows, the exceptions won't be included in the check below for the
        # presence of failure rows
        failures <- failures[zero_pop_sex_agg_exception == FALSE]
        failures[, zero_pop_sex_agg_exception := NULL]

        if (nrow(failures) == 0) {
          est <- est[, list(
            acause = this_cause,
            diff_mean = max(diff_mean),
            perc_diff_mean = max(perc_diff_mean)
          )]
          est[, measure := measure]
          saveRDS(est, file = paste0(diff_dir, "/", measure, "_", this_cause, "_diff.rds"))
          return("finished successfully")
        } else {
          num_failures <- nrow(failures)
          pct_failures <- 100 * nrow(failures) / nrow(est)
          year_failures <- paste0(unique(failures$year), collapse = ",")
          sex_failures <- paste0(unique(failures$sex), collapse = ",")
          type_failures <- paste0(unique(failures[[type]]), collapse = ",")
          age_failures <- paste0(unique(failures$age), collapse = ",")

          status <- failures[, .(
            acause = this_cause,
            measure = measure,
            by_mean, all_mean,
            diff_mean, perc_diff_mean
          )][order(-perc_diff_mean)][1:5] # Start by capturing the five "worst offenders"
          for (status_var in c(
            "num_failures", "pct_failures", "year_failures",
            "sex_failures", "type_failures", "age_failures"
          )) {
            status[, (status_var) := get(status_var)]
          }

          saveRDS(status, file = paste0(diff_dir, "/", measure, "_", this_cause, "_diff.rds"))
          saveRDS(failures, file = paste0(diff_dir, "/", measure, "_", this_cause, "_all_failures.rds"))
          return(paste0(
            "Caution, some ", type, " checks failed.\nSee ",
            diff_dir, "/", measure, "_", this_cause, "_all_failures.rds"
          ))
        } # end else (there are failures)
      } # end loop over measures

      saveRDS(status_sub, file = paste0(
        results_dir, "/", type, "_diff/", type, "_", measure,
        "_status_", this_cause, "_lvl_", i, ".rds"
      ))

      names(status_sub) <- paste0(measures, this_cause)
      return(status_sub)
    } # end status loop (over causes)

    stopCluster(cl)
    gc()
    message(type, " check complete \n")

    # print the output, error if output string has "caution"
    print_output_from_foreach(status)

    saveRDS(status, file = paste0(results_dir, "/", type, "_status.rds"))

  } # end validate_race_edu_aggregates


  ## Check for differences between all-race and by-race estimates -------------------------------------
  if (!is.null(edu_race_comparison_dir) && dir.exists(edu_race_comparison_dir) & by_race) {
    validate_race_edu_aggregates("race", parent_causes_to_check, measures, causes, finished_race_diff, race_diff_dir)
  } else {
    message("No all-race directory provided, skipping race check")
  }

  ## Check for differences between all-edu and by-edu estimates -------------------------------------
  if (!is.null(edu_race_comparison_dir) && dir.exists(edu_race_comparison_dir) & by_edu) {
    # function(type, parent_causes_to_check, measures, causes, finished_diff, diff_dir) {
    validate_race_edu_aggregates("edu", parent_causes_to_check, measures, causes, finished_edu_diff, edu_diff_dir)
  } else {
    message("No all-edu directory provided, skipping edu check")
  }

  ## Combine and save verification results -----------------------------------------------------------
  # Load in intermediate files to get both mx and ylls
  # cause-check is skipped for yld/pred (exposure), as well as for all-cause when we are only looking at level 1
  if (!any(identical("pred", measures), identical("yld", measures), i == 0)) {
    cause_diff <- recover_diff_files(diff_dir = cause_diff_dir, parent_causes_to_check_cause, measures)
    if (nrow(cause_diff) > 0) {
      write_csv(cause_diff, paste0(results_dir, "/cause_raking_check", format(Sys.time(), "%Y_%m_%d_%H_%M"), ".csv"), row.names = F)
    }

    # skip cause_check if there are no children to compare to. This implies cause_diff is empty
    children.to.check <- child.check.status(causes, parent_causes_to_check)$check
    if (children.to.check) {
      cause_diff_results <- check_cause_diff(cause_diff, results_dir)
      for (msg in cause_diff_results$messages) {
        if (str_detect(tolower(msg), "caution") || str_detect(tolower(msg), "failure")) {
          lsae.utils::stop_or_quit(capture.output(print(cause_diff_results$messages)), status = 3)
        }
        message(msg)
      }
      cause_diff_results <- adjust_diff_results_by_exclusions(cause_diff_results, check_var = "cause")
    } else {
      cause_diff_results <- list()
    }
  } else {
    cause_diff_results <- list()
  }

  if(!identical("pred", measures)){
    if(identical("ab", measures) && all(causes[acause %in% parent_causes_to_check, leaf_cause] == FALSE)){
      # skip if all causes are NOT leaf causes
      # (these are skipped in geo_check)
      geo_diff_results <- list()
    } else {
      geo_diff <- recover_diff_files(diff_dir = geo_diff_dir, parent_causes_to_check, measures)
      if (nrow(geo_diff) > 0) {
        write.csv(geo_diff, paste0(results_dir, "/geo_raking_check", format(Sys.time(), "%Y_%m_%d_%H_%M"), ".csv"), row.names = F)
      }
      geo_diff_results <- check_geo_diff(geo_diff, results_dir)
      for (msg in geo_diff_results$messages) {
        if (str_detect(tolower(msg), "caution") || str_detect(tolower(msg), "failure")) {
          lsae.utils::stop_or_quit(capture.output(print(geo_diff_results$messages)), status = 3)
        }
        message(msg)
      }
      geo_diff_results <- adjust_diff_results_by_exclusions(geo_diff_results, check_var = "geo")
    }
  } else{
    geo_diff_results <- list()
  }

  
  race_diff_results <- edu_diff_results <- list()
  if (!is.null(edu_race_comparison_dir) && dir.exists(edu_race_comparison_dir)) {
    
    # rename race_diff_results to race_edu_diff_results *for consistencny*
    # this should really be named extra_dim_results though...
    if (by_race) {
      race_diff <- recover_diff_files(diff_dir = race_diff_dir, parent_causes_to_check, measures)
      if (nrow(race_diff) > 0) {
        write_csv(race_diff, paste0(results_dir, "/race_raking_check", format(Sys.time(), "%Y_%m_%d_%H_%M"), ".csv"), row.names = F)
      }
      race_diff_results <- check_race_edu_diff(race_diff, results_dir)
      for (msg in race_diff_results$messages) {
        if (str_detect(tolower(msg), "caution") || str_detect(tolower(msg), "failure")) {
          lsae.utils::stop_or_quit(capture.output(print(race_diff_results$messages)), status = 3)
        }
        message(msg)
      }
      race_diff_results <- adjust_diff_results_by_exclusions(race_diff_results, check_var = "race")
    } else if (by_edu) {
      edu_diff <- recover_diff_files(diff_dir = edu_diff_dir, parent_causes_to_check, measures)
      if (nrow(edu_diff) > 0) {
        write_csv(edu_diff, paste0(results_dir, "/edu_raking_check", format(Sys.time(), "%Y_%m_%d_%H_%M"), ".csv"), row.names = F)
      }
      edu_diff_results <- check_race_edu_diff(edu_diff, results_dir)
      for (msg in edu_diff_results$messages) {
        if (str_detect(tolower(msg), "caution") || str_detect(tolower(msg), "failure")) {
          lsae.utils::stop_or_quit(capture.output(print(edu_diff_results$messages)), status = 3)
        }
        message(msg)
      }
      edu_diff_results <- adjust_diff_results_by_exclusions(edu_diff_results, check_var = "edu")
    }
  }


  ## Make plots --------------------------------------------------------------------------------------
  
  if (!identical("yld", measures)){ 
    cat("\n\nMake plots\n")

    capture.output(cl <- makeCluster(threads, type = "FORK", outfile = ""), file = "FILEPATH")
    registerDoParallel(cl)

    # load shape files
    area_map <- readRDS(shape_file)
    if(any(measures %in% c("ab", "pred"))){ # RF code uses sf objects
      area_map <- as(area_map, "sf")
      area_map <- st_make_valid(area_map) # fix a self-intersection error that's causing problems for geom_sf 
      setDT(area_map)
      setnames(area_map, "mcnty", "area")
    } else {
      area_map <- data.table(fortify(area_map))
      area_map[, area := as.numeric(as.character(id))]
    }
    
    # load population data
    pop <- readRDS(pop_file)
    pop <- pop[year %in% years & age %in% ages]
    setnames(pop, "mcnty", "area")

    # loop over causes and make plots for each cause
    
    
    plot_status <- foreach(this_cause = parent_causes_to_check, .packages = c("data.table", "ggplot2", "RColorBrewer", "gridExtra"), .inorder = T, .errorhandling = "pass", .final = function(x) setNames(x, parent_causes_to_check)) %dopar% {
      cat(paste0("\nBeginning on ", this_cause))
      foreach(measure = measures, .errorhandling = "pass") %do% {
        if (identical(measure, "pred")){
          this_cause <- "est"
          measure_name <- basename(dirname(dir)) # e.g., 'overweight'
        } else{
          measure_name <- measure
        }
        if (measure == "mx") {
          ages <- eval(parse(text = as.character(as.data.table(read.csv(paste0(dir, "/", this_cause, "/settings.csv"), header = F))[V1 == "ages", V2])))
          sexes <- eval(parse(text = as.character(as.data.table(read.csv(paste0(dir, "/", this_cause, "/settings.csv"), header = F))[V1 == "sexes", V2])))
          years <- eval(parse(text = as.character(as.data.table(read.csv(paste0(dir, "/", this_cause, "/settings.csv"), header = F))[V1 == "years", V2])))
          # load data, collapse to "plot_level" and "raking_area_var"-level, and add all-ages and both sexes
          deaths <- readRDS(paste0(lu_modeldir, "/", this_cause, "/data.rds"))
          deaths[, year := as.integer(year + years[1])]
          deaths[, area := as.integer(area)]
          deaths[, sex := as.integer(sex)]
          deaths[, race := as.integer(race)]
          if("edu" %in% names(deaths)) {
            deaths[, edu := as.integer(edu)]
            deaths <- deaths[edu != unknown_edu, ] # drop unknown education
          }
          deaths[, age := as.integer(ages[1 + age])]
          deaths[, chsda := NULL]

          # bring in the missing ages so that we get the correct denominator
          miss_demo <- pop[(!(age %in% ages) | !(sex %in% sexes)) & year %in% years, .(area, year, sex, age, race, pop)]
          miss_demo[, deaths := 0]
          deaths <- unique(rbind(deaths, miss_demo, fill = T))

          # aggregate data over race
          deaths <- deaths[, list(area, year, sex, race, edu, age, pop, deaths)]
          if (by_race) {
            message("Aggregating over race...")
            # list(area, year, sex, race, edu, age, pop, deaths)
            temp_deaths <- deaths[, list(race = all_pop_id, pop = sum(pop), deaths = sum(deaths)),
                                  by = "area,year,age,sex,edu"]
            stopifnot(setequal(names(deaths), names(temp_deaths)))
            deaths <- rbind(deaths, temp_deaths)
            rm(temp_deaths); gc()
          }

          # aggregate data over edu
          if (by_edu) {
            message("Aggregating over edu...")
            temp_deaths <- deaths[, list(edu = all_pop_id, pop = sum(pop), deaths = sum(deaths)),
                                  by = "area,year,age,sex,race"]
            stopifnot(setequal(names(deaths), names(temp_deaths)))
            deaths <- rbind(deaths, temp_deaths)
            rm(temp_deaths); gc()
          }
          # if not by race and not by edu then it's at the county level, and all-race/all-edu is
          # already in the data.

          weights <- readRDS(geoagg_files[[plot_level]])
          stopifnot(area_var %in% names(weights))
          setnames(weights, area_var, "area")

          agg <- merge(deaths, weights, by = "area", all = T)

          agg[is.na(deaths), deaths := 0] 
          
          agg <- agg[, list(deaths = sum(deaths), pop = sum(pop)), by = c("year", "sex", "age", "race", "edu", plot_level)]
          setnames(agg, plot_level, "area")
          agg[, level := plot_level]

          if (plot_level != raking_area_var) {
            
            weights <- readRDS(geoagg_files[raking_area_var])
            stopifnot(area_var %in% names(weights))
            setnames(weights, area_var, "area")
            agg2 <- merge(deaths, weights, by = "area", all = T)
            agg2[is.na(deaths), deaths := 0]
            agg2 <- agg2[, list(deaths = sum(deaths), pop = sum(pop)), by = c("year", "sex", "age", "race", "edu", raking_area_var)]
            setnames(agg2, raking_area_var, "area")
            agg2[, level := raking_area_var]
            data <- rbindlist(list(agg, agg2), use.names = T)
            rm(agg, agg2, deaths, weights)
          } else {
            data <- copy(agg)
            rm(agg, deaths, weights)
          }

          data <- rbind(data, data[, list(age = 99L, deaths = sum(deaths), pop = sum(pop)), by = "level,area,year,sex,race,edu"])
          data <- rbind(data, data[, list(age = 98L, deaths = sum(deaths), pop = sum(pop)), by = "level,area,year,sex,race,edu"])
          data <- rbind(data, data[, list(sex = 3L, deaths = sum(deaths), pop = sum(pop)), by = "level,area,year,age,race,edu"])
          data <- data[, list(level, area, year, sex, age, race, edu, type = "Data", value = deaths / pop)]

        }

        # load pre- and post-raking estimates and combine
        if (identical(measure, "pred")){
          pre <- load_compiled_estimates(dir, "est", measure = measure, raked = F, use_adjusted = 1)
          # if this model is predicted by source, filter to the gold-standard source and remove the source column
          if(!is.null(model_settings$settings$by_source) && model_settings$settings$by_source == TRUE){
            pre <- pre[source_v2 == model_settings$settings$gold_standard]
            pre[, source_v2 := NULL]
          }
          post <- load_compiled_estimates(dir, this_cause, measure = measure, raked = T, use_adjusted = 1)
          if(by_race & !by_edu) pre[, edu := sae.shared::edu_default]
          if(is.null(key(pre))) setkeyv(pre, key(post))
        } else if(identical(measure, "ab")) {
          pre <- load_compile_estimates_ab(root = dir, cause = this_cause, raked = FALSE, years = years, measure_id = measure_id)
          post <- load_compile_estimates_ab(root = dir, cause = this_cause, raked = TRUE, years = years, measure_id = measure_id)
        } else {
          pre <- load_compiled_estimates(dir, this_cause, measure = measure, raked = F, use_adjusted = 1)
          post <- load_compiled_estimates(dir, this_cause, measure = measure, raked = T, use_adjusted = 1)

        }
        keep_levels <- unique(c(raking_area_var, plot_level, area_var))
        if(measure %in% c("pred", "ab")) keep_levels <- unique(c(keep_levels, "natl"))
        
        # If metric_id is not in pre/post, add that column (make it NA).
        # This gives flexibility to loop through metric_id for AB results
        if(measure != "ab") {
          if(!"metric_id" %in% names(pre)) pre[, metric_id := NA]
          if(!"metric_id" %in% names(post)) post[, metric_id := NA]
        }
        pre <- pre[
          J(keep_levels), on = "level",
          list(level, area, year, sex, age, race, edu, metric_id,
               type = "Unraked", value = get(paste0(measure, "_mean")),
               ci = get(paste0(measure, "_ub")) - get(paste0(measure, "_lb"))
          )
        ]

        post <- post[
          J(keep_levels), on = "level",
          list(level, area, year, sex, age, race, edu, metric_id,
               type = "Raked", value = get(paste0(measure, "_mean")),
               ci = get(paste0(measure, "_ub")) - get(paste0(measure, "_lb"))
          )
        ]

        est <- rbind(pre, post, use.names = T)
        rm(pre, post)

        # if(identical(measure, "pred")){
        #   est <- est[year >= 2009] # limited risk factor data to years modeled for paper7
        #   years <- years[years >= 2009]
        # }


        # Add the effect of raking on the all-race model (skip for risk factors b/c we don't have an all-race model)
        if (!measure %in% c("pred", "ab") && dir.exists(edu_race_comparison_dir)) {
          pre_county <- load_compiled_estimates(edu_race_comparison_dir, this_cause,
                                                measure = measure, raked = F, use_adjusted = 1
          )
          post_county <- load_compiled_estimates(edu_race_comparison_dir, this_cause,
                                                 measure = measure, raked = T, use_adjusted = 1
          )

          pre_county <- pre_county[
            J(unique(c(raking_area_var, plot_level, area_var))),
            list(level, area, year, sex, age, race, edu,
                 type = "Unraked", value = get(paste0(measure, "_mean")),
                 ci = get(paste0(measure, "_ub")) - get(paste0(measure, "_lb"))
            )
          ]
          post_county <- post_county[
            J(unique(c(raking_area_var, plot_level, area_var))),
            list(level, area, year, sex, age, race, edu,
                 type = "Raked", value = get(paste0(measure, "_mean")),
                 ci = get(paste0(measure, "_ub")) - get(paste0(measure, "_lb"))
            )
          ]

          est_county <- rbind(pre_county, post_county)
          rm(pre_county, post_county)
        }

        # start PDF - save in the limited use folder
        cat("\nstart PDF")
        
        full_est <- copy(est)
        for(mm in unique(est$metric_id)){ # only has an effect for ab as currently implemented
          if(identical(measure, "pred")){
            # pdf(paste0(lu_modeldir, "/raking_effect.pdf"), width = 15, height = 8)
            pdf(paste0(results_dir, "/raking_effect.pdf"), width = 15, height = 8) # RF plots only show modeled data so are not limited use
          } else if (measure == "ab") {
            pdf(paste0(results_dir, "/raking_effect", this_cause, "_metric_id_", mm, ".pdf"), width = 15, height = 8) # RF plots only show modeled data so are not limited use
          } else{
            pdf(paste0(lu_modeldir, "/", this_cause, "/raking_effect_",measure,".pdf"), width = 15, height = 8)
          }

          if(measure == "ab"){
            est <- full_est[metric_id == mm]
            metric_name <- fcase(mm == 1, "count", mm == 2, "PAF", mm == 3, "rate")
          } else {
            est <- full_est
            metric_name <- "rate"
          }

          if(identical(measure, "mx")) {
            race_iterations <- sort(unique(c(data$race, est$race)))
          } else if(identical(measure, "yll") | identical(measure, "pred") | identical(measure, "ab")) {
            race_iterations <- sort(unique(c(est$race)))
          } else {
            lsae.utils::stop_or_quit(paste0("race_iterations cannot be calculated for ", measure_name), status = 1)
          }

          if(identical(measure, "mx")) {
            edu_iterations <- sort(unique(c(data$edu, est$edu)))
          } else if(identical(measure, "yll") | identical(measure, "pred")) {
            edu_iterations <- sort(unique(c(est$edu)))
          } else if(identical(measure, "pred") | identical(measure, "ab")){
            edu_iterations <- sort(unique(c(est$edu)))
          } else {
            lsae.utils::stop_or_quit(paste0("edu_iterations cannot be calculated for ", measure_name), status = 1)
          }

          if(identical(measure, "pred") | identical(measure, "ab")) {
            fdata <- est[level == "natl" & age %in% c(98,99) & sex == 3]
            for(aa in c(98, 99)){
              fdata_a <- fdata[age == aa]
              # if no rows in fdata_a, skip (this can happen for age-standardized counts)
              if(nrow(fdata_a) == 0) next
              print(ggplot(fdata_a, aes(x = year, y = value)) +
                      geom_line(data = fdata[age == aa & type != "Data", ], aes(color = type), size = 0.75, alpha = 0.75) +
                      geom_point(data = fdata[age == aa & type == "Data", ], color = "black", size = 0.75, alpha = 0.5) +
                      facet_wrap(~race) +
                      labs(
                        title = paste0(ifelse(aa == 98, "Crude ", "Age-standardized "), measure_name, " ", metric_name, " by ", plot_level, ", ", this_cause, " (National)"),
                        x = "Year", y = measure
                      ) +
                      theme_bw(base_size = 12) +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1)))
            }
          }

          message(glue::glue("Plot type 1: crude/age-standardized {measure_name} {metric_name} by {plot_level}, and race/edu. One at the county level and one at the county-race/edu level."))

          for (edu_sub in edu_iterations) {
            for (race_sub in race_iterations) {

              message(glue::glue("edu: {edu_sub}"))
              message(glue::glue("race: {race_sub}"))
              if (!skip_all_race && (race_sub == race_default & edu_sub == edu_default)) { # selecting county level data
                # if dealing with all-race, first plot the effect of raking on the county level model
                if (measure == "mx") {
                  fdata_county <- rbind(data[level %in% c(raking_area_var, plot_level) & age %in% c(98, 99) & sex == 3 & race == race_sub & edu == edu_sub, ],
                                        est_county[level %in% c(raking_area_var, plot_level) & age %in% c(98, 99) & sex == 3 & race == race_sub & edu == edu_sub, ],
                                        fill = T
                  )
                } else {
                  fdata_county <- est_county[level %in% c(raking_area_var, plot_level) & age %in% c(98, 99) & sex == 3 & race == race_sub & edu == edu_sub, ]
                }

                fdata_county[, level_area := paste0(level, ": ", area)]
                fdata_county[, level_area := factor(level_area, levels = unique(fdata_county[order(level, area), level_area]))]

                # loop over ages 98 and 99 for county level plots
                for (aa in c(98, 99)) {
                  print(ggplot(fdata_county[age == aa, ], aes(x = year, y = value)) +
                          geom_line(data = fdata_county[age == aa & type != "Data", ], aes(color = type), size = 0.75, alpha = 0.75) +
                          geom_point(data = fdata_county[age == aa & type == "Data", ], color = "black", size = 0.75, alpha = 0.5) +
                          facet_wrap(~level_area, scales = "free_y") +
                          labs(
                            title = paste0(ifelse(aa == 98, "Crude ", "Age-standardized "), measure_name, " ", metric_name, " by ", plot_level, ", ", this_cause, ", race ", race_sub, ", edu", edu_sub, " (county model)"),
                            x = "Year", y = measure
                          ) +
                          theme_bw(base_size = 12) + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
                } # end loop over age 98 and 99 for county level plots
              } # end if that selected for county level data

              # make line plots by area for race/edu specific raking results

              
              # would not have edu or county specific estimates in it. I don't think this plot was
              # intended to show county level plots because the title doesn't say that, like the plot
              # above does.
              if (measure == "mx") {
                fdata <- rbind(data[level %in% c(raking_area_var, plot_level) & age %in% c(98, 99) & sex == 3 & race == race_sub & edu == edu_sub, ],
                              est[level %in% c(raking_area_var, plot_level) & age %in% c(98, 99) & sex == 3 & race == race_sub & edu == edu_sub, ],
                              fill = T
                )
              } else {
                fdata <- est[level %in% c(raking_area_var, plot_level) & age %in% c(98, 99) & sex == 3 & race == race_sub & edu == edu_sub, ]
              }
              fdata[, level_area := paste0(level, ": ", area)]
              fdata[, level_area := factor(level_area, levels = unique(fdata[order(level, area), level_area]))]

              # loop over Crude rates and Age-standardized rates for county-race/county-edu plots
              for(aa in c(98, 99)){
                if(fdata[age == aa, .N] == 0) next
                print(ggplot(fdata[age == aa], aes(x = year, y = value)) +
                        geom_line(data = fdata[age == aa & type != "Data", ], aes(color = type), size = 0.75, alpha = 0.75) +
                        geom_point(data = fdata[age == aa & type == "Data", ], color = "black", size = 0.75, alpha = 0.5) +
                        facet_wrap(~level_area, scales = "free_y") +
                        labs(
                          title = paste0(ifelse(aa == 98, "Crude ", "Age-standardized "), measure_name, " ", metric_name, " by ", plot_level, ", ", this_cause, ", race ", race_sub, ", edu", edu_sub),
                          x = "Year", y = measure
                        ) +
                        theme_bw(base_size = 12) +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1)))
              } # end loop over age 98 and 99
            } # end race loop
          } # end edu loop

          message(glue::glue("Plot type 2: both-sex, state-level, age-specific {measure_name} {metric_name} for each state, and race/edu, over time."))
          for (edu_sub in edu_iterations) {
            for (race_sub in race_iterations) {

              message(glue::glue("edu: {edu_sub}"))
              message(glue::glue("race: {race_sub}"))

              # make line plots by age
              
              if (identical(measure, "mx")) {
                fdata <-
                  rbind(data[level == raking_area_var &
                              age <= 99 & sex == 3 & race == race_sub & edu == edu_sub,],
                        est[level == raking_area_var &
                              age <= 99 & sex == 3 & race == race_sub & edu == edu_sub,],
                        fill = T)
              } else {
                fdata <-
                  est[level == raking_area_var &
                        age <= 99 & sex == 3 & race == race_sub & edu == edu_sub,]
              }
              # loop over every area available
              for (this_area in unique(fdata[, area])) {
                print(
                  ggplot(fdata[area == this_area,], aes(
                    x = year,
                    y = value,
                    color = type,
                    group = type
                  )) +
                    geom_line(data = fdata[area == this_area & type != "Data",],
                              aes(color = type), size = 1) +
                    geom_point(
                      data = fdata[area == this_area & type == "Data",],
                      color = "black",
                      size = 0.75
                    ) +
                    facet_wrap(~ age, scales = "free_y") +
                    labs(
                      title = paste0(
                        measure_name, " ", metric_name, " by age, ", this_cause, ", ", raking_area_var, " ", this_area, ", race ", race_sub, ", edu ", edu_sub),
                      x = "Year",
                      y = measure
                    ) +
                    theme_bw(base_size = 12) +
                    theme(axis.text.x = element_text(
                      angle = 45, hjust = 1
                    ))
                )
              } # end loop over area
            } # end race loop
          } # end edu loop

          # make maps
          if (!skip_all_race) {
            message(glue::glue("Plot type 3: Map of raked county-level model age-standardized {measure_name} {metric_name}, both-sex, {area_var}-level, for latest year."))

            fdata_mcnty <- est_county[level == area_var & year == max(years) & sex == 3 & age == 99 &
                                        race == race_default & edu == edu_default, ]

            fdata_mcnty[, type := factor(type, levels = c("Unraked", "Raked"))]
            fdata_mcnty <- merge(area_map, fdata_mcnty, by = "area", allow.cartesian = T)

            print(ggplot(fdata_mcnty) + facet_grid(. ~ type) +
                    geom_polygon(aes(x = long, y = lat, group = group, fill = value)) +
                    scale_x_continuous("", breaks = NULL, expand = c(0, 0)) +
                    scale_y_continuous("", breaks = NULL, expand = c(0, 0)) +
                    coord_fixed(ratio = 1) +
                    scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), guide = guide_colorbar(nbin = 50, barheight = 20)) +
                    theme_bw(base_size = 12) +
                    labs(title = paste0(
                      "county-level model, age-standardized ", measure_name," ", metric_name, "s, ", max(years),
                      ", both sexes"
                    ), caption = "All-race and All-edu"))
          } # end skip_all_race if statement

          message(glue::glue("Plot type 4: Map of raked and unraked model age-standardized {measure_name} {metric_name}, both-sex, {area_var}-level, for each year and race/edu. Also, map of differences (abs and rel) between raked and unraked."))

          for(yr in range(years)){
            for (edu_sub in c(edu_iterations, NA)) {
              if(measure %in% c("pred", "ab") & is.na(edu_iterations)) next # skip the edu loops for RF if the files are "all edu"
              for (race_sub in c(race_iterations, NA)) {

                message(glue::glue("year: {yr}"))
                message(glue::glue("edu: {edu_sub}"))
                message(glue::glue("race: {race_sub}"))

                # create a plot for all races / all edus, faceted by race/edu when race_sub/edu_sub is NA
                if(is.na(race_sub)) r_tmp <- race_iterations else r_tmp <- race_sub
                if(is.na(edu_sub)) e_tmp <- edu_iterations else e_tmp <- edu_sub

                fdata <- est[J(level = area_var, year = yr, sex =3, age = 99, race = r_tmp, edu = e_tmp),
                    on = c("level", "year", "sex", "age", "race", "edu")]
                # skip if only one row -- this happens for metric_id 1 (count) since we don't have age-standardized rates
                if(nrow(fdata) == 1) next
                # also skip if all values are NA & there are as many rows as race_iterations
                # when race_sub is NA -- this is also related to now age-std rates for
                # metric_id 1 -- but multiple rows b/c we are showing each race
                if(is.na(race_sub) & nrow(fdata) == length(race_iterations) & all(is.na(fdata$value))) next 

                fdata[, type := factor(type, levels = c("Unraked", "Raked"))]
                fdata <- merge(area_map, fdata, by = "area", allow.cartesian = T)

                # map raked and unraked levels
                print(ggplot(fdata) +
                        facet_grid(type~race) +
                        # geom_polygon(aes(x = long, y = lat, group = group, fill = value)) +
                        geom_sf(aes(geometry = geometry, fill = value)) +
                        scale_x_continuous("", breaks = NULL, expand = c(0, 0)) +
                        scale_y_continuous("", breaks = NULL, expand = c(0, 0)) +
                        # coord_fixed(ratio = 1) +
                        scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), guide = guide_colorbar(nbin = 50, barheight = 20)) +
                        theme_bw(base_size = 12) +
                        labs(
                          title = paste0("age-standardized ", measure_name, " ", metric_name, "s, ", yr, ", both sexes"),
                          subtitle = paste0("Race ", race_sub, ", Education, ", edu_sub)
                        ))


                fdata[, geometry := NULL] # can't dcast with geometry
                fdata <- dcast(fdata, ...~type, value.var = c("value", "ci")) 
                fdata[, diff := value_Raked - value_Unraked]
                fdata[, ratio := value_Raked / value_Unraked]
                # for AB, we have some very extreme diffs and ratios that obscure all other differences
                # Suppress these
                if(measure == "ab"){
                  fdata[abs(diff) > 1e9, diff := NA]
                  fdata[abs(ratio) > 1e9, ratio := NA]                  
                }
                suppress_diff <- fdata[is.na(diff), .N]
                suppress_ratio <- fdata[is.na(ratio), .N]
                if(suppress_diff > 0) {
                  caption_diff <- glue::glue("Suppressed {suppress_diff} rows with abs(diff) > 1e9")
                } else {
                  caption_diff <- NULL
                }
                if(suppress_ratio > 0) {
                  caption_ratio <- glue::glue("Suppressed {suppress_ratio} rows with abs(ratio) > 1e9")
                } else {
                  caption_ratio <- NULL
                }


                # add back geography
                fdata <- merge(area_map, fdata, by = "area", allow.cartesian = T)

                print(ggplot(fdata) +
                        # geom_polygon(aes(x = long, y = lat, group = group, fill = diff)) +
                        geom_sf(aes(geometry = geometry, fill = diff)) + 
                        scale_x_continuous("", breaks = NULL, expand = c(0, 0)) +
                        scale_y_continuous("", breaks = NULL, expand = c(0, 0)) +
                        # coord_fixed(ratio = 1) +
                        scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), guide = guide_colorbar(nbin = 50, barheight = 20),
                          limits = function(lim) return(c(-1,1)*max(abs(lim)))) + # make limits symetric
                        theme_bw(base_size = 12) +
                        facet_wrap(edu ~ race) +
                        labs(
                          title = paste0("Raked - Unraked: age-standardized ", measure_name, " ", metric_name, "s, ", yr, ", both sexes"),
                          subtitle = paste0("Race ", race_sub, ", Education", edu_sub),
                          caption = caption_diff
                        ))

                
                print(ggplot(fdata) +
                        # geom_polygon(aes(x = long, y = lat, group = group, fill = ratio)) +
                        geom_sf(aes(geometry = geometry, fill = ratio)) +
                        scale_x_continuous("", breaks = NULL, expand = c(0, 0)) +
                        scale_y_continuous("", breaks = NULL, expand = c(0, 0)) +
                        # coord_fixed(ratio = 1) +
                        # scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), guide = guide_colorbar(nbin = 50, barheight = 20)) +
                        # make ratio scale centered on 1
                        scale_fill_gradient2(low = "#642594", mid = "white", high = "#005800", midpoint = 1,
                          guide = guide_colorbar(nbin = 50, barheight = 20)) +
                        theme_bw(base_size = 12) +
                        facet_wrap(edu ~ race) +
                        labs(
                          title = paste0("Raked / Unraked: age-standardized ", measure_name, " ", metric_name, "s, ", yr, ", both sexes"),
                          subtitle = paste0("Race ", race_sub, ", Education", edu_sub),
                          caption = caption_ratio
                        ))
              } # end race loop
            } # end edu loop
          } # end year loop


          # make estimates scatters
          if(!skip_all_race){
            message(glue::glue("Plot type 5: Comparison scatters of raked vs unraked age-standardized {measure_name} {metric_name}, both-sex, {area_var}-level, for each year, all-race/all-edu."))

            
            fdata_mcnty <- est_county[
              level == area_var & sex == 3 & age == 99 & race == race_default & edu == edu_default,
              list(area, year, type, value)
            ]
            fdata_mcnty <- dcast.data.table(fdata_mcnty, area + year ~ type, value.var = "value")

            print(ggplot(fdata_mcnty, aes(x = Unraked, y = Raked)) + facet_wrap(~year, nrow = 4) +
                    geom_point(size = 0.1, alpha = 0.5) + geom_abline() + coord_equal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                    labs(title = paste0(
                      "County-level model, age-standardized ", measure_name, " ", metric_name, "s by\n",
                      area_var, ", unraked and raked, ", this_cause
                    )) +
                    theme_bw(base_size = 12) + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
          }

          message(glue::glue("Plot type 6: Comparison scatters of raked vs unraked age-standardized {measure_name} {metric_name}, both-sex, {area_var}-level, for each year, and race/edu."))

          for (edu_sub in edu_iterations) {
            for (race_sub in race_iterations) {

              message(glue::glue("edu: {edu_sub}"))
              message(glue::glue("race: {race_sub}"))

              fdata <- est[level == area_var & sex == 3 & age == 99 & race == race_sub & edu == edu_sub, list(area, year, type, value)]
              # skip if only one row -- this happens for metric_id 1 (count) since we don't have age-standardized rates
              if(nrow(fdata) == 0) next
              fdata <- dcast.data.table(fdata, area + year ~ type, value.var = "value")
              
              print(ggplot(fdata, aes(x = Unraked, y = Raked)) +
                      facet_wrap(~year, nrow = 4) +
                      geom_point(size = 0.1, alpha = 0.5) +
                      geom_abline() +
                      coord_equal() +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                      labs(
                        title = paste0("Age-standardized ", measure_name, "\n", metric_name, "s by ", area_var, ", unraked and raked, ", this_cause),
                        subtitle = paste("Race ", race_sub, "Education ", edu_sub)
                      ) +
                      theme_bw(base_size = 12) +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1)))
            }
          } # end edu loop

          # make CI scatters
          if(!skip_all_race){

            message(glue::glue("Plot type 7: Comparison scatters of raked vs unraked age-standardized {measure_name} Confidence Intervals, both-sex, {area_var}-level, for each year, all-race/all-edu."))


            fdata_mcnty <- est_county[
              level == area_var & sex == 3 & age == 99 & race == race_default & edu == edu_default,
              list(area, year, type, ci)
            ]
            fdata_mcnty <- dcast.data.table(fdata_mcnty, area + year ~ type, value.var = "ci")

            print(ggplot(fdata_mcnty, aes(x = Unraked, y = Raked)) + facet_wrap(~year, nrow = 4) +
                    geom_point(size = 0.1, alpha = 0.5) + geom_abline() + coord_equal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                    labs(title = paste0("County-level model, CI width for age-standardized ", measure_name, "\n", metric_name, "s by ", area_var, ", unraked and raked, ", this_cause)) +
                    theme_bw(base_size = 12) + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

          }


          message(glue::glue("Plot type 8: Comparison scatters of raked vs unraked age-standardized {measure_name} Confidence Intervals, both-sex, {area_var}-level, for each year, by-race/by-edu. Two y-axis scales: normal and log"))

          for (edu_sub in edu_iterations) {
            for (race_sub in race_iterations) {
              message(glue::glue("edu: {edu_sub}"))
              message(glue::glue("race: {race_sub}"))
              fdata <- est[level == area_var & sex == 3 & age == 99 & race == race_sub & edu == edu_sub, list(area, year, type, ci)]
              # skip if only one row -- this happens for metric_id 1 (count) since we don't have age-standardized rates
              if(nrow(fdata) == 0) next
              fdata <- dcast.data.table(fdata, area + year ~ type, value.var = "ci")

              p1 <- ggplot(fdata, aes(x = Unraked, y = Raked)) +
                facet_wrap(~year, nrow = 4) +
                geom_point(size = 0.1, alpha = 0.5) +
                geom_abline() +
                coord_equal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                labs(
                  title = "Normal space",
                  subtitle = paste0("Race ", race_sub, "Edu ", edu_sub)
                ) +
                theme_bw(base_size = 12) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

              p2 <- p1 + scale_y_log10() + scale_x_log10() +
                labs(
                  title = paste0("Log10 space"),
                  subtitle = paste0("Race ", race_sub, "Edu ", edu_sub)
                )

              print(grid.arrange(p1, p2, ncol = 2, top = paste0(
                "CI width for age-standardized ", measure, " ", metric_name, "s by ", area_var,
                ", unraked and raked, ", this_cause
              )))
            } # end race loop
          } # end edu loop

          dev.off()
          cat(paste0("\nDone with ", this_cause), "metric_id", mm)
        }
        
        return(paste0(this_cause, " finished successfully"))
      }
    }

    print_output_from_foreach(plot_status)

    stopCluster(cl)
    gc()
    message("plotting complete \n")

    plot_status_loop <- unlist(plot_status)
    for (name in names(plot_status_loop)) {
      if (str_detect(name, "call")) {
        next
      } else {
        message(name)
        message("  ", plot_status_loop[[name]])
      }
    }

    # If anything failed, exit early
    any_failures <- any(
      length(geo_diff_results$failed) > 0 |
        length(cause_diff_results$failed) > 0 |
        length(race_diff_results$failed) > 0 |
        length(edu_diff_results$failed) > 0
    )

    if (any_failures) {
      lsae.utils::stop_or_quit("Issues found in check_raked_result - exiting early", status = 3)
    }
    saveRDS(plot_status, file = paste0(results_dir, "/plot_status.rds"))

    cat("\nDONE\n\n")
  }
}

# check_raked_result has passed, so delete temporary files if delete_temp is set to TRUE.
if (delete_temp) {
  for (measure in measures) {
    if (measure == "mx") {
      file_pattern <- "*_TEMP.rds"
    } else {
      file_pattern <- "*_prelim_raked.rds"
    }

    if (!is.null(children_of)) {
      path <- paste0(dir, "/", children_of)
      print(paste0("Deleting files in ", path, " that match pattern ", file_pattern))
      cmd <- paste0("find ", path, " -type f -name ", file_pattern, " -delete")
      system(SYSTEM_COMMAND)
    } else {
      for (lvl in initial_level:terminal_level) {
        parent_causes <- causes[level == lvl, unique(acause)]
        for (parent_cause in parent_causes) {
          path <- paste0(dir, "/", parent_cause)
          print(paste0("Deleting files in ", path, " that match pattern ", file_pattern))
          cmd <- paste0("find ", path, " -type f -name ", file_pattern, " -delete")
          system(SYSTEM_COMMAND)
        }
      }
    }
  }
}


# change group ownership to 'Domain Users'
set_group_ownership(group_name = "Domain Users", dest_dir = dir)

# Send email notification that job is complete
lsae.utils::send_email("Validation step complete", "PROJECT", "QUEUE") 
