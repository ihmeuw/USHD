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
  # eval(parse(text = "C")) returns function
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
#' @param copy_J [logical] If TRUE, copies FILEPATH drive files used in the settings to dir/from_FILEPATH and also updates
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
    message("FILEPATH drive paths in 'settings.yaml' are updated!")
  }
}

#' write_consolidated_settings_burden
#' 
#' @description Writes settings.yaml used to attributable burden raking. The 
#' settings file needs most of the information from the BMI exposure files, as 
#' well as information about the cause hierarchy. The hierarchy should be 
#' constructed by lsae.shared::write_consolidated_settings(), as done for the 
#' mortality models. Unlike the mortality models, there is not a settings.csv
#' for the burden models, so we need to construct the settings.yaml from 
#' the metadata produced by the taternator and upstream metadata from the
#' database.
#' 
#' @param dir [character] model directory
#' @param rake_to_gbd_version [list] version of the GBD to which to rake
#' Default is 
#' list(
#'  release_id = 10, # GBD 2021 Iterative
#'  version = 295, # burdenator version.
#'  gbd_id_type = "cause_id",
#'  status = "best"
#' )
#' @param races [numeric] race IDs. Default is c(2,4,5,6,7)
#' @param ages [numeric] ages to include in the settings. Default is NULL.
#' For risks restricted to adults (like meta_bmi_adult), use seq(20, 85, 5). 
#' @param write [logical] If TRUE, writes the settings.yaml file. Default is TRUE.
#' Otherwise, returns the settings list
#' 
#' @rdname write_consolidated_settings_burden
#' @export
write_consolidated_settings_burden <- function(
  dir,
  rake_to_gbd_version = list(
      release_id = 10, # GBD 2021 iterative
      version = 295, # burdenator version
      gbd_id_type = "cause_id",
      status = "best"
  ),
  races = c(2,4,5,6,7),
  ages = NULL,
  write = TRUE
) {
  # Load AB metadata, constructed from the CLI when running taternator
  meta <- fromJSON(txt = paste0(dir, "/metadata.json"))$`cli-args`
  
  # get PAF compile ID from AB metadata
  paf_compile_run_id <- meta$paf_compile_run_id
  # get PAF (not PAF compile) run ID
  paf_compile_run <- get_paf_compile_model_run(paf_compile_run_id = paf_compile_run_id)
  # get the exposure model associated with the PAF run
  paf_run <- get_paf_run(paf_run_id = paf_compile_run$paf_run_id)
  exp_sd_model_id = paf_run$model_exp_run_id 
  # load table of exp_sd runs, then filter to the one we want
  all_exp_sd_models <- get_risk_exp_model_run(model_output_type = "exp_sd", get_best = FALSE)
  exp_sd_metadata_id <- all_exp_sd_models[model_exp_run_id == exp_sd_model_id]$model_exp_metadata_id
  # check that exp_sd_metadata_id is not NULL and length 1
  if(is.null(exp_sd_metadata_id) || length(exp_sd_metadata_id) != 1){
    stop("exp_sd_metadata_id is NULL or not length 1")
  }
  # load the metadata
  exp_sd_meta <- get_risk_exp_model_metadata(model_exp_metadata_id = exp_sd_metadata_id)
  # get population path
  pop_file_path <- unlist(exp_sd_meta[key == "pop_file"]$value)
  # ensure it's a single string, and not NULL
  if(length(pop_file_path) != 1){
    stop("pop_file_path is not a single string")
  }
  if(is.null(pop_file_path)){
    stop("pop_file_path is NULL")
  }

  ### Construct settings from the metadata, pop_file, and passed args
  settings_list <- list(
    area_var = "mcnty",
    race_together = TRUE,
    edu_groups = 1,
    n.sims = meta$n_draws,
    sexes = c(1,2),
    measure_ids = meta$measure_ids,
    by_race = TRUE,
    by_sex = TRUE,
    by_edu = FALSE,
    age_std_file = "FILEPATH/census_age_weights.rds",
    raking_area_var = "state",
    races = races,
    fatal_model_id = meta$fatal_run_id,
    nonfatal_run_id = meta$nonfatal_run_id,
    rake_to_gbd = TRUE,
    rake_to_gbd_version = rake_to_gbd_version,
    pop_file = pop_file_path,
    geoagg_files = list(
      natl = "FILEPATH/mcnty_to_natl_crosswalk.rds",
      state = "FILEPATH/mcnty_to_state_crosswalk.rds"
    ),
    edu = 1,
    ages = ages,
    begin_year = min(meta$year_ids),
    end_year = max(meta$year_ids),
    model_years = meta$year_ids
  )

  #### Next, we need to construct the cause hierarchy. 
  # We will  load submitted_cause_list.csv from the associated mortality model, 
  # and subset to the causes associated with high BMI
  
  # construct the path to the mortality model
  fatal_root <- "FILEPATH"
  fatal_model_name <- query_database(database = "public_database",
                                     query = sprintf("SELECT model_run_name FROM model_run WHERE model_run_id=%s", meta$fatal_run_id)
  )$model_run_name
  # load the submitted_cause_list.csv, which is in the fatal model directory 
  full_submitted_cause_list <- fread(
    file.path(fatal_root, fatal_model_name, "submitted_cause_list.csv"),
    header = TRUE, stringsAsFactors = FALSE)
  ## filter to the causes in the BMI hierarchy
  # Get cause_ids from the metadata files associated with an arbitrary mcnty
  ff <- list.files(sprintf("%s/draws/1001/metadata/", meta$versioned_out_dir), recursive = T, full.names = T)[1]
  cause_ids <- fromJSON(txt = ff)$dimensions$cause_id
  # subset submitted cause list
  submitted_cause_list <- full_submitted_cause_list[cause_id %in% cause_ids]
  # save as csv in AB model_dir
  fwrite(submitted_cause_list, file.path(dir, "/submitted_cause_list.csv"), row.names = FALSE)
  submitted_causes <- submitted_cause_list[, acause]

  # now add the cause hierarchy to the settings.yaml
  # each row of the cause hierarchy should be a list. Here is an example for acause == _all
  # list(
  #     "_all" = list(
  #       level = 0,
  #       cause_id = 294,
  #       cause_outline = "Total",
  #       parent_id = 294,
  #       path_to_top_parent = "294",
  #       run_model = 1)
  #   )
  # Adapted from write_consolidate_settings and get_consolidated_settings in lsae.shared
  cause_settings_list <- lapply(submitted_causes, function(ac){
    tmp <- submitted_cause_list[acause == ac]
    ll <- list(
        level = tmp$level,
        cause_id = tmp$cause_id,
        cause_outline = tmp$cause_outline,
        parent_id = tmp$parent_id,
        path_to_top_parent = tmp$path_to_top_parent,
        run_model = tmp$run_model
      # )
    )
    return(ll)
  })
  names(cause_settings_list) <- submitted_causes

  new_settings <- list()
  new_settings$settings <- settings_list
  new_settings$submitted_causes <- cause_settings_list
  
  if(write){
    write_yaml(new_settings, file = file.path(dir, "settings.yaml"), fileEncoding = "UTF-8")
  } else {
    return(new_settings)
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
#' @description Uses get_updated_settings_on_copy_J_dependencies method to copy FILEPATH files to dir/from_FILEPATH, and also updates
#' ModelSettings$settings and settings.yaml with relative new paths.
#'
#' @param dir[character] Model directory
#'
#' @rdname update_settings_with_J_independency
#' @export
update_settings_with_J_independency <- function(dir) {
  # copying FILEPATH files requires that you have settings.yaml file because that is where updated paths are written to.
  if (!file.exists(file.path(dir, "settings.yaml"))) {
    lsae.utils::stop_or_quit(paste0("Copying files from FILEPATH requires yaml settings at: ", file.path(dir, "settings.yaml")),
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
