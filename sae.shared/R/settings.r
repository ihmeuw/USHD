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
#'                run checks on the model settings object. That is, check that everything that needs
#'                to be specified is there and in the right format. Similarly, check that all files
#'                listed in settings exist and are in the right format and that all files and
#'                settings are self-consistent.
#'
#' @param       dir [character] -- the file path to the directory where "settings.csv" is located
#'              validation [logical] -- is this a validation model?
#'
#' @rdname get_settings
#' @export
get_settings <- function(dir, dest.env = .GlobalEnv) {
  # stop if "settings.file" does not exist.
  if (!file.exists(file.path(dir, "settings.csv"))) {
    stop_or_quit(msg = paste0("settings.csv file is missing at: ", dir), status = 9)
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
    "covars_subpop", "covar_subpop_file", "covar_subpop_hyperpriors_settings", "covars", "covars_as", "covars_trans", "covar_file", "covar_as_file", "geoagg_files", "raking_area_var",
    "raking_draws_dir", "raking_est_dir", "subset_hisp_mr", "misclassification_correction", "coethnic_concen",
    "by_region", "qx_diff_extension", "subset_hisp_mr_path", "qx_diff_extension_coefs", "prior_type", "prior_list",
    "year_knots_num", "age_knots_spec", "combine_mc", "age_knots_spec_re3", "year_knots_num_re3", "pop_growth_file", "lt_hc", "offset_file", "offset"
  )

  # assign the all-race value so that we don't have to hardcode this
  assign("all_pop_id", race_default, envir = dest.env)

  for (var in setdiff(optional, ls(envir = dest.env))) assign(var, NULL, envir = dest.env)
  if (!"fixed_age_time" %in% ls(envir = dest.env)) assign("fixed_age_time", FALSE, envir = dest.env)
  if (!"by_race" %in% ls(envir = dest.env)) assign("by_race", FALSE, envir = dest.env)
  if (!"races" %in% ls(envir = dest.env)) assign("races", race_default, envir = dest.env)
  if (!"race_labels" %in% ls(envir = dest.env)) assign("race_labels", paste("Race/Ethnicity", dest.env[["races"]]), envir = dest.env)
  if (!"race_together" %in% ls(envir = dest.env)) assign("race_together", FALSE, envir = dest.env)

  if (!"by_edu" %in% ls(envir = dest.env)) assign("by_edu", FALSE, envir = dest.env)
  if (!"edu_groups" %in% ls(envir = dest.env)) assign("edu_groups", edu_default, envir = dest.env)
  if (!"edu_labels" %in% ls(envir = dest.env)) assign("edu_labels", paste("Education", dest.env[["edu_groups"]]), envir = dest.env)
  if (!"edu_together" %in% ls(envir = dest.env)) assign("edu_together", FALSE, envir = dest.env)
  
  if (!"sex_together" %in% ls(envir = dest.env)) assign("sex_together", FALSE, envir = dest.env)

  # for backwards compatibility, need to add this setting for older models. This only needs to be added if there are subpop covariates
  if ((!"covar_subpop_hyperpriors_settings" %in% ls(envir = dest.env)) & ("covars_subpop" %in% ls(envir = dest.env))) {
    assign("covar_subpop_hyperpriors_settings", list(re_cov_par1 = 5, re_cov_par2 = 0.05, re_cov_log_sigma = -5), envir = dest.env)
  }

  if (!"lt_hc" %in% ls(envir = dest.env)) assign("lt_hc", FALSE, envir = dest.env)

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
      "rake_to_gbd_version", "sexes", "years", "age_std_file"
    )
  } else {
    # check that all necessary settings are present
    all_settings <- c(
      "model_class", "model", "area_var", "raking_area_var", "years", "ages", "sexes", "by_race",
      "races", "race_labels", "covars", "covars_as", "covars_trans", "n.sims", "adjmat_file",
      "deaths_file", "pop_file", "covar_file", "covar_as_file", "geoagg_files", "shape_file",
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
    if (!is.list(settings$rake_to_gbd_version) && !is.character(settings$rake_to_gbd_version)) {
      stop("'rake_to_gbd_version' should be a list or a character vector")
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
    if (length(settings$rake_to_gbd_version) < 1) {
      stop("'rake_to_gbd' should be of length >= 1")
    }
  }
  if (length(settings$raking_area_var) != 1) {
    stop("'raking_area_var' should be length 1")
  }
  if (length(settings$sexes) < 1 || length(settings$sexes) > 3) {
    stop("'sexes' should be length 1 to 3")
  }
  if (length(settings$years) <= 1) {
    stop("'years' should be length >= 2")
  }
  if (length(settings$age_std_file) != 1) {
    stop("'age_std_file' should be length 1")
  }

  # check that n.sims > 0 but not super large
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

  # LT-HC validations
  if (!is.null(settings$lt_hc) && settings$lt_hc){
    if(settings$by_edu) stop("The code does not currently accomodate education models using the H-C method")
    if(is.null(settings$pop_growth_file)) stop("lt_hc is TRUE, but you have not specified pop_growth_file")
    if(!file.exists(settings$pop_growth_file)) stop("lt_hc is TRUE, but the pop_growth_file you specified does not exist")

    pop_growth <- readRDS(settings$pop_growth_file)
    if(nrow(pop_growth) == 0) stop("The pop_growth_file has no rows")
    expected_cols <- c("mcnty", "year", "sex", "age", "race", "pop", "state")
    if(!all(expected_cols %in% names(pop_growth))) stop(paste0("One of: ", paste(expected_cols,collapse=", "), " - is not in the pop_growth_file"))
    if(!all(c(settings$years, c(min(settings$years):(min(settings$years)-10))) %in% pop_growth$year)){
      stop("In pop_growth file: Missing the specified years and the 10 previous years before the minimum year that are necessary for calculating population growth")
    }
    if(settings$by_race & !all(settings$races %in% pop_growth$race)) stop("The model is by_race but pop_growth_file has the incorrect race values")
    if(!max(settings$ages) %in% pop_growth$age) stop("The terminal age group is missing from the pop_growth_file")
  } else {
    if (!is.null(settings$pop_growth_file)) stop("You have specified a pop_growth_file but lt_hc is FALSE")
  }

  # check that all input files exist
  if (!file.exists(settings$age_std_file)) {
    stop(paste0("Input file missing, age_std_file: ", settings$age_std_file))
  }

  # check that input files are the correct file type
  if (!grepl(".rds$", settings$age_std_file)) {
    stop(paste0("age_std_file should be of type rds but was passed: ", settings$age_std_file))
  }

  # check contents of settings
  if (strict) {
    if (!"version_id" %in% names(settings$rake_to_gbd_version)) {
      required_keys <- c("gbd_round_id", "decomp_step")
      missing_keys <- setdiff(required_keys, names(settings$rake_to_gbd_version))
      if (length(missing_keys > 0)) {
        stop(paste0(
          "'rake_to_gbd_version' is missing required key/s: ",
          paste(missing_keys, collapse = "; "),
          "\nEither 'version_id' or both '",
          paste(required_keys, collapse = " and "),
          "' must be specified"
        ))
      }
    }
  }
}
