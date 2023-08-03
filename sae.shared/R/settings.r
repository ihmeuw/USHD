#' @title is.wholenumber
#'
#' @description check that input is whole number
#'
#' @export
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  if (class(x) == "integer") {
    return(TRUE)
  } else if (class(x) == "numeric" & max(abs(x - round(x))) < tol) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @title get_settings
#'
#' @description Define two functions related to model settings:
#'
#'              "get_settings":
#'                load settings from the "settings" CSV into the R global environment and evaluate
#'                to the appropriate data type.
#'
#'              "validate_settings_for_post_estimation":
#'                run checks on the model settings objenct. That is, check that everything that needs
#'                to be specified is there and in the right format. Similarly, check that all files
#'                listed in settings exist and are in the right format and that all files and
#'                settings are self-consistent.
#'
#'
#' @param       dir [character] -- the file path to the directory where "settings.csv" is located
#'              validation [logical] -- is this a validation model?
#'
#' @rdname get_settings
#' @export
get_settings <- function(dir, dest.env = .GlobalEnv) {
  # stop if "settings.file" does not exist.
  if (!file.exists(file.path(dir, "settings.csv"))) {
    lsae.utils::stop_or_quit(msg = paste0("settings.csv file is missing at: ", dir), status = 9)
  }

  # load settings
  settings <- read.csv(paste0(dir, "/settings.csv"), stringsAsFactors = F, header = F)

  # loop over settings
  for (var in 1:nrow(settings)) {
    arg_name <- settings[var, 1]
    arg_value <- settings[var, 2]

    # assign settings in the global environment
    exp <- try(assign(arg_name, eval(parse(text = arg_value)), envir = dest.env), silent = T)
    if (class(exp) == "try-error") assign(arg_name, arg_value, envir = dest.env)
  }

  # create (theoretically) optional variables if not provided
  optional <- c(
    "covars_subpop", "covar_subpop_versions", "covar_subpop_hyperpriors_settings",
    "covars", "covars_as", "covars_trans", "covar_versions", "covar_as_versions",
    "geoagg_files", "raking_area_var",
    "raking_draws_dir", "raking_est_dir", "subset_hisp_mr", "misclassification_correction",
    "subset_hisp_mr_path", "prior_type", "prior_list",
    "year_knots_num", "age_knots_spec","lt_hc",
    # population parameters - either pop_file or pop_version (to pull from DB) can be specified
    "pop_file", "pop_version",
    # to indicate shocks related deaths
    "indicator_file", "indicator_year",
    # NOTE: there are different kinds of offsets!
    "offset_file", "offset",
	# These are for the education format death certificate offset
    "covars_offset", "covar_offset_file"
  )

  # assign the all-race value so that we don't have to hardcode this
  assign("all_pop_id", race_default, envir = dest.env)

  for (var in setdiff(optional, ls(envir = dest.env))) assign(var, NULL, envir = dest.env)
  if (!"fixed_age_time" %in% ls(envir = dest.env)) assign("fixed_age_time", FALSE, envir = dest.env)
  if (!"by_race" %in% ls(envir = dest.env)) assign("by_race", FALSE, envir = dest.env)
  if (!"races" %in% ls(envir = dest.env)) assign("races", race_default, envir = dest.env)
  if (!"race_labels" %in% ls(envir = dest.env)) assign("race_labels", "Total", envir = dest.env)
  if (!"race_together" %in% ls(envir = dest.env)) assign("race_together", FALSE, envir = dest.env)

  if (!"by_edu" %in% ls(envir = dest.env)) assign("by_edu", FALSE, envir = dest.env)
  if (!"edu_groups" %in% ls(envir = dest.env)) assign("edu_groups", edu_default, envir = dest.env)
  if (!"edu_labels" %in% ls(envir = dest.env)) assign("edu_labels", "Total", envir = dest.env)
  if (!"edu_together" %in% ls(envir = dest.env)) assign("edu_together", FALSE, envir = dest.env)

  if (!"sex_together" %in% ls(envir = dest.env)) assign("sex_together", FALSE, envir = dest.env)

  if (!"zero_covar_offset" %in% ls(envir = dest.env)) assign("zero_covar_offset", TRUE, envir = dest.env) 

  if (!"lt_hc" %in% ls(envir = dest.env)) assign("lt_hc", FALSE, envir = dest.env)
  
  if(!"remove_multi_before_implementation" %in% ls(envir = dest.env)) assign("remove_multi_before_implementation", FALSE, envir = dest.env)

  return("Settings loaded")
}

#' @title validate_settings_for_post_estimation
#'
#' @description Validates model settings specific for use in post estimation
#'
#' @param settings [list] Named list of model settings.
#' @param validation [logical] Logical for additional validation from check_settings()
#' @param strict [logical] True will validate post_estimation specific checks
#'
#' @export
validate_settings_for_post_estimation <- function(settings, validation = FALSE, strict = FALSE) {
  if (strict) {
    # check that all necessary settings are present
    all_settings <- c(
      "ages", "area_var", "raking_area_var", "by_race", "n.sims", "races", "rake_to_gbd",
      "sexes", "years", "age_std_file"
    )
  } else {
    # check that all necessary settings are present
    all_settings <- c(
      "model_class", "model", "area_var", "raking_area_var", "years", "ages", "sexes", "by_race",
      "races", "race_labels", "covars", "covars_as", "covars_trans", "n.sims", "adjmat_file",
      "deaths_file", "covar_versions", "covar_as_versions", "geoagg_files", "shape_file",
      "age_std_file", "ref_lt_file", "raking_draws_dir", "raking_est_dir", "edu_groups"
    )
    if (validation) all_settings <- c(all_settings, "val_sizes", "val_iter", "val_types", "val_dir", "gs_file")
    if (settings$fixed_age_time) all_settings <- c(all_settings, "ref_dir", "ref_raked", "ref_level", "ref_area")
  }
  miss <- all_settings[!all_settings %in% names(settings)]
  if (length(miss)) {
    stop(paste("Settings missing:", paste(miss, collapse = "; ")))
  }

  # check the settings are of right type
  if (!is.wholenumber(settings$ages)) {
    stop("'ages' should be a whole number of type integer or numeric")
  }
  if (!is.character(settings$area_var)) {
    stop("'area_var' should be type character")
  }
  if (!is.null(settings$by_edu) && !is.logical(settings$by_edu)) {
    stop("'by_edu' should be a logical")
  }
  if (!is.logical(settings$by_race)) {
    stop("'by_race' should be a logical")
  }
  if (!is.null(settings$edu_groups) && !is.wholenumber(settings$edu_groups)) {
    stop("'edu_groups' should be a whole number of type integer or numeric")
  }
  if (!is.wholenumber(settings$n.sims)) {
    stop("'n.sims' should be a whole number of type integer or numeric")
  }
  if (!is.wholenumber(settings$races)) {
    stop("'races' should be a whole number of type integer or numeric")
  }
  if (strict) {
    if (!is.logical(settings$rake_to_gbd)) {
      stop("'rake_to_gbd' should be a logical")
    }
  }
  if (strict && is.null(settings$raking_area_var)) {
    stop("'raking_area_var' cannot be NULL")
  }
  if (!is.character(settings$raking_area_var)) {
    stop("'raking_area_var' should be type character")
  }
  if (!is.wholenumber(settings$sexes)) {
    stop("'sexes' should be a whole number of type integer or numeric")
  }
  if (!is.wholenumber(settings$years)) {
    stop("'years' should be a whole number of type integer or numeric")
  }
  if (!is.character(settings$age_std_file)) {
    stop("'age_std_file' should be type character")
  }

  # check that all settings are the right length
  if (length(settings$ages) < 1) {
    stop("'ages' should be length >= 1")
  }
  if (length(settings$area_var) != 1) {
    stop("'area_var' should be length 1")
  }
  if (!is.null(settings$by_edu) && length(settings$by_edu) != 1) {
    stop("'by_edu' should be length 1")
  }
  if (length(settings$by_race) != 1) {
    stop("'by_race' should be length 1")
  }
  if (length(settings$races) < 1) {
    stop("'races' should be length >= 1")
  }
  if (!is.null(settings$edu_groups) && length(settings$edu_groups) < 1) {
    stop("'edu_groups' should be length >= 1")
  }
  if (length(settings$n.sims) != 1) {
    stop("'n.sims' should be length 1")
  }
  if (strict) {
    if (length(settings$rake_to_gbd) != 1) {
      stop("'rake_to_gbd' should be length 1")
    }
  }
  if (length(settings$raking_area_var) != 1) {
    stop("'raking_area_var' should be length 1")
  }
  if (length(settings$sexes) < 1 || length(settings$sexes) > 3) {
    stop("'sexes' should be length 1 to 3")
  }
  if (length(settings$years) < 1) {
    stop("'years' should be length >= 1")
  }
  if (length(settings$age_std_file) != 1) {
    stop("'age_std_file' should be length 1")
  }

  # check that n.sims > 0 but not crazy big (which will cause memory problems)
  if (settings$n.sims <= 0) {
    stop("'n.sims' must be >= 1")
  } else if (settings$n.sims > 1e4) {
    stop("setting 'n.sims' too high which may cause memory problems")
  }

  # check that "by_race" and "races" are self-consistent
  if (!settings$by_race & length(settings$races) > 1) {
    stop("'by_race' is FALSE, but multiple races are specified")
  }
  if (!settings$by_race && !identical(settings$races, race_default)) {
    stop("'by_race' is FALSE, but races does not equal race_default")
  }
  if (settings$by_race & length(settings$races) < 2) {
    stop("'by_race' is TRUE, but only one race is specified")
  }

  # check that "by_edu" and "edu_groups" are consistent
  if (!is.null(settings$by_edu) && !is.null(settings$edu_groups)) {
    if (!settings$by_edu & length(settings$edu_groups) > 1) {
      stop("'by_edu' is FALSE, but multiple edu_groups are specified")
    }
    if (!settings$by_edu && !identical(settings$edu_groups, edu_default))
      stop("'by_edu' is FALSE, but edu_groups does not equal edu_default")
  }
  if (settings$by_edu & length(settings$edu_groups) < 2) {
    stop("'by_edu' is TRUE, but only one edu_group is specified")
  }


  # check that all input files exist
  if (!file.exists(settings$age_std_file)) {
    stop(paste0("Input file missing, age_std_file: ", settings$age_std_file))
  }

  # check that input files are the correct file type
  if (!grepl(".rds$", settings$age_std_file)) {
    stop(paste0("age_std_file should be of type rds but was passed: ", settings$age_std_file))
  }

}
