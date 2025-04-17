#' @title add_with_one_off_changes
#'
#' @description Makes changes to a setting item before appending to a yaml ready list.
#'
#' @param section_y [list] yaml ready list
#' @param name [character] name (key) of the named list
#' @param value [object] value of section_y$name
#'
#' @return section_y list as modifed by this method
#'
#' @rdname add_with_one_off_changes
#' @export
add_with_one_off_changes <- function(section_y, name, value) {
  if (is.null(value)) {
    # adding null to named list requires name in single "[]"
    section_y[name] <- list(NULL)
  } else if ((identical(name, "races") || identical(name, "sexes") || identical(name, "edu_groups") ||
    identical(name, "edu_labels")) && length(value) == 1) {
    # single element vector needs explicit conversion to list()
    section_y[[name]] <- list(value)
  } else if (identical(name, "path_to_top_parent")) {
    # path_to_top_parent is a character. Single element numeric string (which gets evaluated
    # as numeric by eval(parse())) needs to be converted back to character
    section_y[[name]] <- as.character(value)
  } else if (identical(name, "years")) {
    # 'years' settings is not a list anymore.
    section_y[["begin_year"]] <- min(value)
    section_y[["end_year"]] <- max(value)
  } else if (identical(name, "covars_trans") || identical(name, "geoagg_files")) {
    # vector indices/names are not recognized unless converted to list.
    section_y[[name]] <- as.list(value)
  } else {
    section_y[[name]] <- value
  }
  return(section_y)
}

#' @title check_for_reserved_word
#'
#' @description Checks if the value is a reserved word in yaml
#'
#' @param value [object] R object
#'
#' @rdname check_for_reserved_word
#' @export
check_for_reserved_word <- function(value) {
  # Do not use YES|yes|NO|no, they are reserved words(boolean) for yaml
  if (identical("no", tolower(value)) || identical("yes", tolower(value))) {
    stop(sprintf("'%s' is a reserved word in yaml!!", value))
  }
}

#' @title eval_exp
#'
#' @description Evaluates submitted cause expression
#'
#' @param exp [character] Expression that has to be evaluated
#'
#' @return output from parsed and eval'ed expression (unless incoming exp is NA (returns NULL), is a function or causes a
#' a try-error (un-evaluated value is returned))
#'
#' @rdname eval_exp
#' @export
eval_exp <- function(exp) {
  # convert NA to NULL for comparison
  if (all(is.na(exp))) {
    return(NULL)
  }
  
  # gsub converts "formula" class to character vector
  # so evaluate if beforehand
  if (class(exp) == "formula") {
    output <- deparse(exp, width.cutoff = 500L)
    return(output)
  }
  # double quotes cannot be evaluated
  exp <- gsub("\"\"", "\"", exp)

  output <- try(eval(parse(text = exp)), silent = TRUE)
  if (class(output) == "try-error" || class(output) == "function") {
    return(exp)
  } else {
    exp <- eval(parse(text = exp))
    return(exp)
  }
}

#' @title settings_from_get_settings
#'
#' @description Uses get_settings() function to read settings.csv
#'
#' @param dir [character] Location of settings.csv
#'
#' @return list of settings as set by get_settings into temp env
#'
#' @rdname settings_from_get_settings
#' @export
settings_from_get_settings <- function(dir) {
  dest <- new.env()
  get_settings(dir, dest)
  settings <- as.list(dest)
  return(settings)
}

#' @title get_consolidated_settings
#'
#' @description Assembles one list of settings from ("[dir]/settings.csv"), ("[dir]/submitted_cause_list.csv") and
#'               ("[dir]/[cause]/settings.csv")
#'
#' @param dir [character] Model directory
#'
#' @return combined list of settings from sources listed in description
#'
#' @rdname get_consolidated_settings
#' @export
get_consolidated_settings <- function(dir) {
  main_settings <- settings_from_get_settings(dir) # just runs get_settings(), and does not validate these settings

  main_settings_y <- list()
  for (name in names(main_settings)) {
    if (all(is.na(main_settings[[name]]))) {
      main_settings[name] <- list(NULL)
    }
    setting_value <- main_settings[[name]]
    
    if (class(setting_value) == "formula") {
      fml <- deparse(exp, width.cutoff = 500L)
      main_settings_y[[name]] <- fml
    } else {
      check_for_reserved_word(setting_value)
      main_settings_y <- add_with_one_off_changes(main_settings_y, name, setting_value)
    }
  }

  # collect main_setting_y in consolidated settings_y
  settings_y <- list()
  settings_y$settings <- main_settings_y

  # If submitted_cause_list.csv is not present (e.g. 1D raking of bmi-prevalence and mean-bmi)
  # add an all-cause to the settings file to work well with the raking pipeline.
  submitted_causes_file <- file.path(dir, "submitted_cause_list.csv")
  if (!file.exists(submitted_causes_file)) {
    warning("WARNING: submitted_cause_list.csv not found at: ", submitted_causes_file, 
            "\nAn '_all' cause will be substituted. Please check 'settings.yaml' file.")
    submitted_causes <- list(
      "_all" = list(
        level = 0,
        cause_id = 294,
        cause_outline = "Total",
        parent_id = 294,
        path_to_top_parent = "294",
        run_model = 1)
    )
    settings_y$submitted_causes <- submitted_causes
    
    return(settings_y)
  }
  
  submitted_cause_list_dt <- ModelSettings$get_cause_datatable(dir = dir, use.csv = TRUE)
  submitted_causes <- submitted_cause_list_dt[, acause]
  submitted_causes_y <- list()
  
  for (cause in submitted_causes) {
    cause_setting <- settings_from_get_settings(file.path(dir, cause))
    submitted_cause <- submitted_cause_list_dt[acause == cause]
    # Don't check for acause column
    submitted_cause[, acause := NULL]

    submitted_cause_y <- list()

    # check for each submitted_cause setting against specific cause_setting and main_settings.
    for (name in names(submitted_cause)) {
      sub_cause_setting_value <- eval_exp(submitted_cause[[name]])
      check_for_reserved_word(sub_cause_setting_value)

      # check cause setting against submitted cause, if different error out
      if (name %in% names(cause_setting)) {
        if (all(is.na(cause_setting[[name]]))) {
          cause_setting[name] <- list(NULL)
        }
        cause_setting_value <- cause_setting[[name]]
        check_for_reserved_word(cause_setting_value)

        if (!identical(cause_setting_value, sub_cause_setting_value)) {
          msg <- (paste0(name, " for ", cause, " in cause setting and submitted_cause_list setting do not match!!"))
          stop(msg)
        }
      }

      # check submitted cause against main settings, include in yaml file IFF different
      if (name %in% names(main_settings)) {
        main_setting_value <- main_settings[[name]]
        if (identical(main_setting_value, sub_cause_setting_value)) {
          next
        }
      }
      # submitted cause setting has unique value
      submitted_cause_y <- add_with_one_off_changes(submitted_cause_y, name, sub_cause_setting_value)
    }

    # include in sumitted_causes if specific cause_setting has different setting than main_settings or are not
    # included in submitted_causes or main_settings
    for (name in names(cause_setting)) {
      if (name %in% names(main_settings)) {
        if (!identical(cause_setting[[name]], main_settings[[name]]) && !(name %in% names(submitted_cause_y))) {
          submitted_cause_y <- add_with_one_off_changes(submitted_cause_y, name, cause_setting[[name]])
        }
      } else {
        if (!(name %in% names(submitted_cause_y))) {
          # This usually does not happen but if it does include it with submitted causes.
          warning(paste0("Caution! cause_setting not in main_settings or submitted_cause.", cause, ": ", name))
          submitted_cause_y <- add_with_one_off_changes(submitted_cause_y, name, cause_setting[[name]])
        }
      }
    }

    # collect submitted_cause for yaml
    submitted_causes_y[[cause]] <- submitted_cause_y
  }

  # collect submitted_causes_y in consolidated settings_y
  settings_y$submitted_causes <- submitted_causes_y
  return(settings_y)
}

#' write_consolidate_settings
#' 
#' @description Writes consolidated settings returned from get_consolidated_settings() to a single "settings.yaml" file.
#'
#' @param dir [character] model directory
#' @param copy_J [logical] If TRUE, copies J: drive files used in the settings to dir/from_J_drive and also updates
#' ModelSettings$settings and settings.yaml with new relative paths.
#'
#' @rdname write_consolidated_settings
#' @export
write_consolidated_settings <- function(dir, copy_J = FALSE) {
  settings_yaml <- get_consolidated_settings(dir)
  # save consolidated settings to "settings.yaml" file
  write_yaml(settings_yaml, file.path(dir, "settings.yaml"), fileEncoding = "UTF-8")
  message("Done!! 'settings.yaml' saved to: ", dir)
  
  if(copy_J) {
    update_settings_with_J_independency(dir)
    message("J: drive paths in 'settings.yaml' are updated!")
  }
}

#' @title get_check_rake_exclusions
#'
#' @description  Checks for "check_rake_exclusions" in ("[dir]/settings.yaml") and returns it.
#'
#' @param dir [character] Model directory
#' @return named list of excluded measures for each cause with exclusions or NULL
#'
#' @rdname get_check_rake_exclusions
#' @export
get_check_rake_exclusions <- function(dir) {
  if (file.exists(file.path(dir, "settings.yaml"))) {
    consolidated_settings <- read_yaml(file.path(dir, "settings.yaml"), fileEncoding = "UTF-8")
    if ("check_rake_exclusions" %in% names(consolidated_settings)) {
      return(consolidated_settings$check_rake_exclusions)
    }
  }
}

#' @title update_settings_with_J_independency
#'
#' @description Uses get_updated_settings_on_copy_J_dependencies method to copy J drive files to dir/from_J_drive, and also updates
#' ModelSettings$settings and settings.yaml with relative new paths.
#'
#' @param dir[character] Model directory
#'
#' @rdname update_settings_with_J_independency
#' @export
update_settings_with_J_independency <- function(dir) {
  # copying J drive files requires that you have settings.yaml file because that is where updated paths are written to.
  if (!file.exists(file.path(dir, "settings.yaml"))) {
    lsae.utils::stop_or_quit(paste0("Copying files from J: requires yaml settings at: ", file.path(dir, "settings.yaml")),
      status = 1
    )
  }

  yaml_settings <- read_yaml(file.path(dir, "settings.yaml"), fileEncoding = "UTF-8")
  model_settings <- ModelSettings$from_yaml(dir)
  updated_settings <- model_settings$get_updated_settings_on_copy_J_dependencies()
  # only update settings that exist, do not add new settings in updated_settings
  # this prevents "years" from being added (YAML includes just begin_year and end_year)
  yaml_settings$settings <- modifyList(yaml_settings$settings, updated_settings[names(yaml_settings$settings)])
  # Add unused_j_paths from updated_settings when exists
  if (!is.null(updated_settings$unused_j_paths)){
    yaml_settings$settings$unused_j_paths <- updated_settings$unused_j_paths
  }
  
  write_yaml(yaml_settings, file.path(dir, "settings.yaml"), fileEncoding = "UTF-8")
}
