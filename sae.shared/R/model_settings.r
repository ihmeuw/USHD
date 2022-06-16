# Implementation notes:
# Population data is used differently between the two model objects.
#
# The two situations where we need population data are:
#
# 1. To convert GBD data from count-space to rate-space
# 2. To enable the rake of lower draws to upper geography draws.
#
# So, the ModelSettings object needs population only for the lower draws, to facilitate #2.
# The GBDSettings object always needs population, for #1. GBD draws are always upper draws.

#' @title USHD model settings loader.
#'
#' @description Loads and validates settings and associated files for a USHD model directory.
#'
#' @rdname ModelSettings
#' @export
ModelSettings <- R6::R6Class(
  "ModelSettings",
  public = list(
    #' @field dir Directory to load from
    dir = NULL,

    #' @field settings settings list to load to
    settings = NULL,

    #' @field submitted_causes  submitted_causes data.table to load to
    submitted_causes = NULL,

    #' @field ages [integer] Vector of ages.
    ages = NULL,

    #' @field area_var [character] One of c('mcnty', 'state', 'natl') specifying geographic area.
    area_var = NULL,

    #' @field by_race [logical] True if race model
    by_race = FALSE,

    #' @field by_edu [logical] True if education model
    by_edu = FALSE,

    #' @field edu_groups [integer] Vector of edu groups.
    edu_groups = NULL,
    
    #' @field lt_hc [logical] True if H-C method should be used for life expectancy calculation
    lt_hc = FALSE,

    #' @field n.sims [integer] Number of draws or sims used in model
    n.sims = NULL,

    #' @field races [integer] Vector of races to load.
    races = NULL,

    #' @field rake_to_gbd [logical] Rake model to GBD
    rake_to_gbd = NULL,

    #' @field rake_to_gbd_version [character] Version of GBD to rake to (used for data exceptions)
    rake_to_gbd_version = NULL,

    #' @field raking_area_var [character] One of c('mcnty', 'state') specifying geographic area to rake to
    raking_area_var = NULL,

    #' @field sexes [integer] Vector of sexes (1=male, 2=female, 3=both)
    sexes = NULL,

    #' @field to_geo [logical] Rake model to geography
    to_geo = NULL,

    #' @field years [integer] Vector of years (as 4-digit integer)
    years = NULL,

    #' @field std_wt_ Cached std_wt data (ends with "_")
    std_wt_ = NULL,

    #' @field population_ Cached population data (ends with "_")
    population_ = NULL,

    #' @field io_engine Draw-loading object
    io_engine = NULL,

    #' @field lower_draws_schema Schema, for additional lower draws validation
    lower_draws_schema = NULL,
    
    #' @field verbose Controls verbosity for FileIOEngine schema checks
    verbose = NULL,

    #' @description initialize new loader
    #' @param dir A root directory for a USHD model.
    #' @param settings settings object.
    #' @param submitted_causes data.table version of cause tree  default = NULL
    #' @param validate [logical] Validate inputs? default = FALSE.
    initialize = function(dir, settings, submitted_causes = NULL, validate = FALSE, verbose = TRUE) {
      if (is.null(settings$by_edu)) {
        settings$by_edu <- FALSE
        settings$edu_groups <- edu_default
      }
      self$dir <- dir
      self$settings <- settings
      self$submitted_causes <- submitted_causes
      self$verbose = verbose
      self$io_engine <- FileIOEngine$new(settings, verbose) 
      self$lower_draws_schema <- Schema$lower_draws_schema(settings, verbose = verbose)

      if (validate) {
        stopifnot(typeof(dir) == "character")
        stopifnot(typeof(settings) == "list")
        validate_settings_for_post_estimation(settings = self$settings, strict = TRUE)
      }

      # assign values to class that we have explicit names for
      # other values will only be available via self$settings[['name of thing']]
      for (var in intersect(names(settings), names(self))) {
        self[[var]] <- settings[[var]]
      }
    },

    #' @description Return settings list specific to the provided cause.
    #' @param acause character cause
    settings_for_cause = function(acause) {
      if (is.null(self$submitted_causes)) {
        stop_or_quit(sprintf("self$submitted_causes is NULL - cannot get settings_for_cause(%s)", acause))
      }
      acause_settings <- self$submitted_causes[[acause]]
      if (is.null(acause_settings)) {
        stop_or_quit(sprintf("No data in self$submitted_causes for %s", acause))
      }
      # Avoid adding all additional settings from submitted_causes like cause_id, cause_outline, parent_id
      common.keys <- intersect(names(self$settings), names(acause_settings))
      cause.settings <- modifyList(self$settings, acause_settings[common.keys])
      result <- ModelSettings$new(
        dir = self$dir,  
        settings = cause.settings,
        submitted_causes = self$submitted_causes,
        validate = TRUE
      )

      result$submitted_causes <- NULL
      return(result)
    },

    #' @description Copies FILEPATH files listed on settings to dir/from_J_drive, updates the relative path in settings and returns it.
    #' This method also writes metadata of this task to dir/from_J_drive
    get_updated_settings_on_copy_J_dependencies = function() {
      make_output_dir(paste0(self$dir, "/from_J_drive"))
      copy_to_dir <- paste0(self$dir, "/from_J_drive")
      named_files <- c(
        "adjmat_file", "deaths_file", "pop_file", "geoagg_files", "shape_file", "age_std_file", "ref_lt_file",
        "covar_file", "covar_as_file", "covar_subpop_file"
      )

      message("Copying FILEPATH files.")
      metadata <- list()
      metadata <- append(metadata, paste0("Copying FILEPATH files started at: ", Sys.time()))
      
      # copies file from FILEPATH to local drive and returns new path and metadata
      get_new_path_on_copy_file <- function(file_path, metadata) {
        result <- list(new_path = NULL, metadata = metadata)
        if (startsWith(file_path, "FILEPATH")){
          msg <- paste0("files in limited_use cannot be copied, skipping: ", file_path)
          metadata <- append(metadata, msg)
          result$metadata <- metadata
        } else if (!file.exists(file_path)) {
          msg <- paste0("File is missing or you have no access, skipping: ", file_path)
          metadata <- append(metadata, msg)
          result$metadata <- metadata
        } else if (startsWith(fs::path_real(file_path), "FILEPATH") || startsWith(fs::path_real(file_path), "FILEPATH")) {
          real_file_path <- fs::path_real(file_path)
          file_name <- tail(unlist(strsplit(real_file_path, "/")), n = 1)
          
          new_path <- file.path(copy_to_dir, file_name)
          if (!file.exists(new_path)) {
            file.copy(real_file_path, copy_to_dir)
            msg <- paste0("File copied from : ", real_file_path, " to: ", new_path)
          } else {
            msg <- paste0(new_path, " exists, skipping.")
          }
          message(msg)
          metadata <- append(metadata, msg)
          result$new_path <- new_path
          result$metadata <- metadata
        }
        return(result)
      }

      for (named_file in named_files) {
        if (named_file %in% names(self$settings) && !is.null(self$settings[[named_file]])) {
          if (identical(named_file, "geoagg_files")) {
            named_sub_files <- names(self$settings$geoagg_files)

            for (named_sub_file in named_sub_files) {
              file_path <- self$settings$geoagg_files[[named_sub_file]]
              result <- get_new_path_on_copy_file(file_path, metadata)
              new_path <- result$new_path
              metadata <- result$metadata
              if (!is.null(new_path)) {
                self$settings$geoagg_files[[named_sub_file]] <- new_path
                # save original j drive path to 'unused_j_paths' list object
                self$settings$unused_j_paths$geoagg_files[[named_sub_file]] <- file_path
              }
            }
          } else {
            file_path <- self$settings[[named_file]]
            result <- get_new_path_on_copy_file(file_path, metadata)
            new_path <- result$new_path
            metadata <- result$metadata
            if (!is.null(new_path)) {
              self$settings[[named_file]] <- new_path
              # save original j drive path to 'unused_j_paths' list object
              self$settings$unused_j_paths[[named_file]] <- file_path
            }
          }
        }
      }

      metadata <- append(metadata, paste0("Relative file paths in settings have been updated!"))
      msg <- "Original FILEPATH paths are listed under 'unused_j_paths' !"
      message(msg)
      metadata <- append(metadata, msg)
      metadata <- append(metadata, paste0("Copying FILEPATH files done at: ", Sys.time()))

      metadata_file <- paste0("metadata_", format(Sys.time(), "%Y_%m_%d_%H_%M"), ".yaml")
      write_yaml(metadata, file = file.path(copy_to_dir, metadata_file), fileEncoding = "UTF-8")

      message("Metadata for this operation has been written to: ", metadata_file)
      message("Copying FILEPATH files done!")
      return(self$settings)
    },

    #' @description Load age-weights file.
    load_std_wt = function() {
      return(readRDS(self$settings$age_std_file)[, list(age, wt = wt / sum(wt))])
    },

    #' @description Load geographic weights file.
    #'
    #' @param level [character] One of natl, state or mcnty.  Geoagg_files setting usually only have state and natl
    load_geo_wts = function(level){
      return(readRDS(self$settings$geoagg_files[[level]]))
    }, 

    #' @name Load population
    #' @description Wraps a call to load_population_metadata, using class attributes.
    #'
    #' @param year [integer] Year of population data to pull.
    #' @param raking_area_var [character] Upper geography dimension.
    #' @return A population dataframe for the given year that maps from lower geography dimension to upper geography dimension.
    load_population = function(year, raking_area_var) {
      if (raking_area_var == "mcnty") raking_area_var <- self$settings$raking_area_var 
      weights_file <- self$settings$geoagg_files[[raking_area_var]]
      pop_file <- self$settings$pop_file
      area_var <- self$area_var
      by_race <- self$by_race
      by_edu <- self$by_edu
      if (is.null(self$by_edu)) by_edu <- FALSE
      for (var in c("weights_file", "pop_file", "area_var", "raking_area_var", "by_race")) {
        if (is.null(get(var))) stop(sprintf("Argument %s to load_population cannot be null!", var))
      }
      return(load_population_metadata(weights_file,
        pop_file,
        area_var,
        raking_area_var,
        year = year,
        by_race = by_race,
        by_edu = by_edu
      ))
    },

    #' @description Load draws associated with these settings.
    #'
    #' @param acause [character] Cause(s) to pull data for.
    #' @param measure [character] Specify which measure to pull. One of c('mx', 'yll', 'lt').
    #' @param year [integer] Year to pull data for.
    #' @param sex [integer] Sex to pull data for
    #' @param raked [character] One of c('raked', 'raked_temp', 'prelim_raked', 'unraked') specifying which type of data to pull.
    #' @param race_levels [integer] Vector of races to load. Defaults to all races available in settings.
    #' @param edu_levels [integer] Vector of education values to load. Defaults to all education groups in settings.
    #' @param age [integer] Specify if you want to subset down to a specific age.
    #' @param population [data.table] This argument is not used; it's left to keep interface identical with GBDSettings.
    #'
    #' @return data.frame of draws
    load_draws = function(acause, measure, year, sex, raked = "raked",
                          race_levels = self$races, edu_levels = self$edu_groups,
                          age = NULL, population = NULL) {
      return(self$io_engine$load_data(
        type = "draws",
        dir = self$dir,
        acause = acause,
        measure = measure,
        year = year,
        sex = sex,
        age = age,
        raked = raked,
        area_var = self$area_var,
        race_levels = race_levels,
        edu_levels = edu_levels
      ))
    },

    #' @description Load estimates associated with these settings.
    #'
    #' @param acause [character] Cause(s) to pull data for.
    #' @param measure [character] Specify which measure to pull. One of c('mx', 'yll', 'lt').
    #' @param year [integer] Year to pull data for.
    #' @param sex [integer] Sex to pull data for
    #' @param race_levels [integer] Vector of races to load. Defaults to all races available in settings.
    #' @param edu_levels [integer] Vector of education values to load. Defaults to all education groups in settings.
    #' @param age [integer] Specify if you want to subset down to a specific age.
    #'
    #' @return data.frame of estimates
    load_estimates = function(acause, measure, year, sex,
                              race_levels = self$races, edu_levels = self$edu_groups,
                              age = NULL) {
      return(self$io_engine$load_data(
        type = "est",
        dir = self$dir,
        acause = acause,
        measure = measure,
        year = year,
        sex = sex,
        age = age,
        raked = "raked", 
        area_var = self$area_var,
        race_levels = race_levels,
        edu_levels = edu_levels
      ))
    },

    #' @description Saves raked draws and estimates.
    #'
    #' @param draws [data.table] Draws to save.
    #' @param acause [character] Cause to save.
    #' @param measure [character] One of c('mx', 'yll'). Which measure to the data represents.
    #' @param year [integer] Year of data to save
    #' @param sex [integer] Sex of data to save.
    #' @param race [integer] Race of data to save
    #' @param edu [integer] edu level of data to save
    #' @param age [integer] Age to subset to. Only used for 2-D raking.
    #' @param type [character] What process is saving draws. Needs to be one of 'modeling', 'raking', 'aggregation', 'pred_yll'.
    save_draws_and_estimates = function(draws, acause, measure, year, sex, type, race = NULL, edu = NULL, age = NULL) {
      if (!type %in% c('modeling', 'raking', 'aggregation', 'pred_yll')) {
        stop_or_quit(sprintf("Type must be one of 'modeling', raking', 'aggregation', or 'pred_yll', was passed %s", type))
      } 
      if (is.na(getOption('ushd.use_adjusted_data')) && 'adjusted' %in% names(draws)){
        cleaned_draws <- draws[, list(
          level = self$area_var,
          area = get(self$area_var),
          year, sex, age, edu, race, sim, value, pop, acause, adjusted
        )]
      } else {
        cleaned_draws <- draws[, list(
          level = self$area_var,
          area = get(self$area_var),
          year, sex, age, edu, race, sim, value, pop, acause
        )]
      }

      # Calculate all ages if running all-cause
      is_1D_raking <- is.null(age)

      # calculate values for age groups 98 and 99
      # this is necessary for _all cause (1D raking) and all aggregation scripts
      #
      # for 2D raking we rake per-age files so there is not enough information to calc_all_ages()
      if (is_1D_raking) {
        std_wt <- self$std_wt[, list(age, wt = wt / sum(wt))]
        cleaned_draws <- calc_all_ages(cleaned_draws, std_wt, "value",
             by_vars = c("level", "area", "year", "sex", "edu", "race", "sim", "acause",
                         if ('adjusted' %in% names(draws)) 'adjusted')
        )
      }

      # collapse to get estimates
      est <- collapse_draws(
        cleaned_draws,
        "value",
        append(
          c("level", "area", "edu", "race", if ("adjusted" %in% names(cleaned_draws)) "adjusted"),
          setdiff(c("year", "sex", "age", "sim", "acause"), append(c("sim"), geo_choices))
        )
      )

      # Convert measure column name back from generic 'value' to measure for saving
      setnames(est, c("value_mean", "value_lb", "value_ub", "value_se"), paste0(measure, c("_mean", "_lb", "_ub", "_se")))
      setnames(cleaned_draws, "value", measure)
      self$io_engine$save_draws(
        draws = cleaned_draws, 
        acause = acause, 
        save_dir = self$dir, 
        measure = measure, 
        year = year, 
        sex = sex, 
        age = age,
        type = type,
        races = if (is.null(race)) self$settings$races else c(race),
        edu_groups = if (is.null(edu)) self$settings$edu_groups else c(edu)
      )
      
      if (type == 'modeling'){
        raked_string = 'unraked'
      } else {
        if (is.null(age)){
          raked_string = 'raked'
        } else {
          raked_string = 'raked_temp'
        }
      }
      self$io_engine$save_estimates(
        est = est, 
        acause = acause, 
        save_dir = self$dir, 
        measure = measure, 
        year = year, 
        sex = sex, 
        age = age,
        raked = raked_string,
        races = if (is.null(race)) self$settings$races else c(race),
        edu_groups = if (is.null(edu)) self$settings$edu_groups else c(edu))
    },

    #' @description Merges population onto draws, using shared columns.
    #'
    #' @param draws [data.frame] A dataframe of draws or estimates.
    #' @param population [data.frame] A dataframe of population
    #' @param measure [character] One of c('mx', 'yll', 'yld', 'lt'). Needed for validation
    #' @param value_col [character] Column that contains results.  Needed for validation
    #' 
    #' @return Merged draws/population file.
    merge_population = function(draws, population, measure, value_col = "value") {
      # Population doesn't have draws so remove column
      demographics <- intersect(names(draws), names(population))
      population_constants <- c(
        "age", "year", "sex", "wt",
        if (self$by_race) "race",
        if (self$by_edu) "edu",
        if (self$to_geo %in% names(population)) self$to_geo
      )
      merge_vars <- unique(c(demographics, population_constants))

      if (!all(merge_vars %in% names(population))) {
        stop(sprintf(
          "Some collapse variables are not in population data frame: %s",
          paste0(merge_vars[!merge_vars %in% names(population)], collapse = ", ")
        ))
      }
      population <- population[, list(pop = sum(pop)), by = merge_vars]
      draws <- merge(draws, population, by = demographics)

      if (nrow(draws) == 0) stop("Population did not merge correctly - returning an empty dataframe!")

      # You should have lower draws schema at this point.
      draws <- self$lower_draws_schema$validate(draws, measure = "value", value_col = "value")
      return(draws)
    },

    #' @description Load settings into target environment
    #'
    #' @param dest.env [environment] Target environment
    update_env = function(dest.env) {
      for (arg_name in names(self$settings)) {
        arg_value <- self$settings[[arg_name]]
        assign(arg_name, arg_value, envir = dest.env)
      }
    },

    #' @description Load cause specific settings into target environment
    #'
    #' @param acause [character] Cause as a string
    #' @param dest.env [environment] Target environment
    update_env_with_cause_settings = function(acause, dest.env) {
      cause_settings <- self$settings_for_cause(acause)
      cause_settings$update_env(dest.env)
    }
  ),
  active = list(
    #' @field std_wt
    std_wt = function(ignored) {
      if (!missing(ignored)) {
        stop("Cannot set std_wt value")
      }
      if (is.null(self$std_wt_)) {
        self$std_wt_ <- self$load_std_wt()
      }
      return(copy(self$std_wt_))
    }
  )
)

#' @title ModelSettings$from_dir
#'
#' @name ModelSettings$from_dir
#'
#' @description Creates a ModelSettings object from a directory
#'
#' @param dir [character] The root of a USHD model directory.
ModelSettings$from_dir <- function(dir, verbose = TRUE) {
  yaml.settings <- file.path(dir, "settings.yaml")
  if (file.exists(yaml.settings)) {
    ModelSettings$from_yaml(dir, verbose)
  } else {
    return(ModelSettings$from_csv(dir, verbose = verbose))
  }
}

#' @title ModelSettings$from_yaml
#'
#' @name ModelSettings$from_yaml
#'
#' @description Creates a ModelSettings object from a yaml file.
#'
#' @param dir [character] The root of a USHD model directory. Must contain a settings.yaml file.
ModelSettings$from_yaml <- function(dir, verbose = TRUE) {
  yaml.settings <- read_yaml(file.path(dir, "settings.yaml"), fileEncoding = "UTF-8")

  # reconstruct begin_year and end_year in yaml settings to years
  yaml.settings$settings$years <- yaml.settings$settings$begin_year:yaml.settings$settings$end_year
  yaml.settings$settings$begin_year <- NULL
  yaml.settings$settings$end_year <- NULL
  # yaml.settings contains settings and submitted_cause_list
  return(ModelSettings$new(dir, yaml.settings$settings, yaml.settings$submitted_causes, validate = TRUE, verbose = verbose))
}

#' @title ModelSettings$from_csv
#'
#' @name ModelSettings$from_csv
#'
#' @description Creates a ModelSettings object from a csv.
#'
#' @param dir [character] The root of a USHD model directory. Must contain a settings.csv file.
#' @param dest.env [environment] Additional environment to load settings into.
ModelSettings$from_csv <- function(dir, dest.env = NULL, verbose = TRUE) {
  csv.settings <- file.path(dir, "settings.csv")
  if (!file.exists(csv.settings)) {
    stop(sprintf("Attempting to read file %s; file was not found", csv.settings))
  }
  stopifnot(file.exists(csv.settings))

  tmp.env <- new.env()
  get_settings(dir, dest.env = tmp.env)

  if (!is.null(dest.env)) get_settings(dir, dest.env)

  return(ModelSettings$new(dir, as.list(tmp.env), submitted_causes = NULL, validate = TRUE, verbose = verbose))
}

#' @title ModelSettings$get_cause_datatable
#'
#' @name ModelSettings$get_cause_datatable
#'
#' @description Return cause data.table from settings.yaml, or read from submitted_cause_list.csv if forced
#'
#' @param dir [character] model directory
#' @param use.csv [logical] Force read from csv, otherwise read from settings.yaml
#' @param use.exceptions [logical] exclude cause_id_exclusion_list from constants.R
#'
#' @return [data.table] Causes from settings.yaml or from submitted_cause_list.csv
ModelSettings$get_cause_datatable <- function(dir = self$dir, use.csv = FALSE, use.exceptions = TRUE) {

  if (use.csv) {
    if (file.exists(file.path(dir, "submitted_cause_list.csv"))) {
      submitted_cause_list_dt <- fread(file.path(dir, "submitted_cause_list.csv"), header = TRUE, stringsAsFactors = FALSE, colClasses=c(path_to_top_parent='character'))
      warning("WARNING! Reading causes from submitted_cause_list.csv")
    } else {
      stop_or_quit(msg = paste0("submitted_cause_list.csv file (and use.csv == TRUE) is missing at: ", dir), status = 9)
    }
  } else {
    if (file.exists(file.path(dir, "settings.yaml"))) {
      consolidated_settings <- read_yaml(file.path(dir, "settings.yaml"), fileEncoding = "UTF-8")
      sc = consolidated_settings$submitted_causes

      make_into_str <- function(list_of_stuff) {
        return(sprintf("c(%s)", paste(list_of_stuff, collapse = "," )))
      }

      get_val <- function(name, attr) {
        cause.specific <- sc[[name]][[attr]]
        if (is.null(cause.specific)) {
          return(consolidated_settings$settings[[attr]])
        } else {
          return(cause.specific)
        }
      }

      sc.to.dt <- function(name, cs, sc) {data.table(acause = name,
                                                    cause_id = sc[[name]]$cause_id,
                                                    parent_id = sc[[name]]$parent_id,
                                                    cause_outline = sc[[name]]$cause_outline,
                                                    ages = make_into_str(get_val(name, "ages")),
                                                    sexes = make_into_str(get_val(name, "sexes")),
                                                    path_to_top_parent = sc[[name]]$path_to_top_parent,
                                                    run_model = sc[[name]]$run_model,
                                                    level = sc[[name]]$level) }

      submitted_cause_list_dt <- rbindlist(lapply(names(sc), sc.to.dt, cs = consolidated_settings, sc = sc))
    } else {
      stop_or_quit(msg = paste0("settings.yaml missing at: ", dir), status = 9)
    }
  }
  if (use.exceptions) {
    submitted_cause_list_dt <- submitted_cause_list_dt[!(cause_id %in% cause_id_exclusion_list)]
  }
  return(submitted_cause_list_dt)

}
#' @title GBD settings loader.
#'
#' @description Loads settings for GBD in a comparable format to USHD models.
#'
#' @rdname GBDSettings
#' @export
GBDSettings <- R6::R6Class(
  "GBDSettings",
  inherit = ModelSettings,
  public = list(
    #' @field dir load from gbd
    dir = "gbd",

    #' @field settings settings list to load to
    settings = NULL,

    # Variables commonly used in post-estimation pipeline
    #' @field ages [integer] Vector of ages.
    ages = NULL,

    #' @field area_var [character] One of c('mcnty', 'state', 'natl') specifying geographic area.
    area_var = NULL,

    #' @field by_edu [logical] True if education model
    by_edu = FALSE,

    #' @field by_race [logical] True if race model
    by_race = FALSE,

    #' @field edu_groups [integer] Vector of edu groups.
    edu_groups = edu_default,

    #' @field n.sims [integer] Number of draws or sims used in model
    n.sims = NULL,

    #' @field races [integer] Vector of races to load.
    races = race_default,

    #' @field rake_to_gbd [logical] Rake model to GBD This is specified in the lower directory settings
    rake_to_gbd = NULL,

    #' @field rake_to_gbd_version [character] Version of GBD to rake to (used for data exceptions)
    rake_to_gbd_version = NULL,

    #' @field sexes [integer] Vector of sexes (1=male, 2=female, 3=both)
    sexes = NULL,

    #' @field years [integer] Vector of years (as 4-digit integer)
    years = NULL,

    #' @field schema Schema object
    schema = NULL,
    
    #' @field verbose Controls verbosity of Schema validation 
    verbose = NULL,
    
    #' @description Create GBDSettings object.
    #'
    #' @param gbd_settings [list] List of inputs for GBD models
    #' @param schema Schema object
    initialize = function(gbd_settings, schema, verbose = TRUE) {
      if (typeof(gbd_settings) != "list") stop("Must specify GBD settings as a list.")
      if (!gbd_settings$to_geo %in% c("state", "natl")) stop("Can only specify 'state' or 'natl' for GBD upper draws!")

      required_attributes <- c("to_geo", "rake_to_gbd_version", "ages", "n.sims", "area_var")
      missing_attributes <- setdiff(required_attributes, names(gbd_settings))
      if (length(missing_attributes) > 0) stop(sprintf("Missing required attributes %s", paste0(missing_attributes, collapse = ",")))

      self$settings <- gbd_settings
      self$area_var <- gbd_settings$to_geo
      self$rake_to_gbd_version <- gbd_settings$rake_to_gbd_version
      self$n.sims <- gbd_settings$n.sims
      self$ages <- gbd_settings$ages

      # Assign default values to settings for race and edu
      gbd_settings$races <- race_default
      gbd_settings$edu_groups <- edu_default

      # Assign schema
      self$schema <- Schema$upper_draws_schema(gbd_settings, verbose = verbose)
    },

    #' @description Loads GBD draws. They will always be upper.
    #'
    #' @param acause [character] Cause of data to pull.
    #' @param year [integer] Year of data to pull.
    #' @param sex [integer] Sex of data to pull.
    #' @param measure [character] One of 'mx', 'yll', 'yld'.
    #' @param population [data.table] Population data.table, loaded from lower draws settings
    #' @param raked [character] Ignored for GBD; added to make interface consistent with ModelSettings.
    #' @param age [integer] Age to subset data to.
    #'
    #' @return Draws data.table
    load_draws = function(acause, year, sex, measure, population = NULL, raked, age = NULL) {
      if ('load_from_dir' %in% names(self$rake_to_gbd_version)){
        message(sprintf("Loading GBD draws from disk: %s", self$rake_to_gbd_version$load_from_dir))
        raw.upper.draws = self$load_from_disk(acause, year, sex, measure, self$ages)
      } else {
        raw.upper.draws = self$load_from_shared_function(acause, year, sex, measure)
      }
      
      draws <- self$process_gbd_draws(raw.upper.draws, acause, year, sex, measure, population, raked, age)
      # process_gbd_draws returns data with a 'value' column
      draws <- self$schema$validate(draws, measure = "value", value_col = "value")
  
      return(draws)
    },
    
    #' @description Loads GBD draws using shared function get_draws.
    #'
    #' @param acause [character] Cause of data to pull.
    #' @param year [integer] Year of data to pull.
    #' @param sex [integer] Sex of data to pull.
    #' @param measure [character] One of 'mx', 'yll', 'yld'.
    #'
    #' @return Draws data.table
    load_from_shared_function = function(acause, year, sex, measure){
      if (identical("yld", measure)) {
        causes <- NULL # not used
        cause_id <- 294
      } else {
        causes <- get_ids("cause")
        cause <- acause 
        cause_id <- causes[acause %in% cause, cause_id]
      }
      raw.upper.draws <- get_gbd_upper_draws(
        raking_area_var = self$area_var,
        cause_id = cause_id,
        year = year,
        sex = sex,
        ages = self$ages,
        draw_settings = self$rake_to_gbd_version,
        measure = measure,
        n.sims = self$n.sims
      )
      return(raw.upper.draws)
    },
    
    #' @description Loads GBD draws using shared function get_draws.
    #'
    #' @param acause [character] Cause of data to pull.
    #' @param year [integer] Year of data to pull.
    #' @param sex [integer] Sex of data to pull.
    #' @param measure [character] One of 'mx', 'yll', 'yld'.
    #' @param ages [integer] List of ages to pull.
    #'
    #' @return Draws data.table
    load_from_disk = function(acause, year, sex, measure, ages){
      if (!identical(measure, "mx")) {
        stop_or_quit("Can only load measure 'mx'.")
      }
      if (!identical(acause, "_all")) {
        stop_or_quit("Can only load cause_id 294 (_all)")
      }
      
      location_ids <- if (self$area_var == "natl") { 
        102
      } else if (self$area_var == "state") {
        523:573
      } else {
        stop(sprintf("Invalid area_var '%s' - expected 'natl' or 'state'", self$area_var))
      }
      
      age_group_ids <- translate_ages(ages, "age_group_id")

      get.draw.file.path <- function(loc_id) {
        file.path(self$rake_to_gbd_version$load_from_dir, sprintf("env_%s.csv", loc_id))
      }
      
      load.draw.file <- function(loc_id) {
        path <- get.draw.file.path(loc_id)
        data <- data.table::fread(path)
        
        # Assert the age groups you need are present
        stopifnot(all(c(31, 32, 235) %in% data$age_group_id))
        stopifnot(!160 %in% data$age_group_id)
        
        # aggregate 85-89, 90-94, 95+ into a single group
        over.85 <- data[age_group_id %in% c(31, 32, 235), ]  # these 3 groups make up "over 85"
        over.85.age_group_id <- 160 
        vars <- paste0("env_", 0:999)
        over.85[, (vars) := lapply(.SD, function(x) sum(x)), .SDcols=vars, by=c("location_id", "year_id", "sex_id")]
        over.85[, age_group_id := over.85.age_group_id]
        over.85[,entry := 1:.N, by=c("location_id","year_id","sex_id","age_group_id")]
        stopifnot(all.equal(unique(over.85$entry), c(1,2,3)))
        over.85 <- over.85[entry == 1]
        over.85[,entry := NULL]

        # merge in additional 85+ data
        data <- rbind(data, over.85)
        
        # files contain more results than we need - subset
        data <- data[year_id %in% year & sex_id %in% sex & age_group_id %in% age_group_ids, ]
        if (!all(age_group_ids %in% data$age_group_id)){
          missing_ages = age_group_ids[!age_group_ids %in% data$age_group_id]
          stop_or_quit(sprintf("Missing age groups from GBD data: %s", paste0(missing_ages, collapse = ",")), status = 3)
        }
        
        # standardize ages
        data[, age := ages[match(age_group_id, age_group_ids)]]
        data[, age_group_id := NULL]
        
        # standardize year/sex columns
        setnames(data, "year_id", "year")
        setnames(data, "sex_id", "sex")
        data$cause_id <- 294  
        
        # update to match schema provided by get_draws() shared function
        setnames(data, paste0("env_", 0:999), paste0("draw_", 0:999))
        if (self$n.sims == 100) {
          warning(sprintf("Downsampling: dropping all but first %i draws", self$n.sims))
          data[, (paste0("draw_", seq(self$n.sims, 999))) := NULL]
        }
        return(data)
      }
      
      return(rbindlist(lapply(location_ids, load.draw.file)))
      
    },

    #' @description Helper function for processing raw GBD data.
    #' Splitting off this function also helps with test coverage, so we don't have
    #' to make expensive database calls in the test suite.
    #'
    #' @param raw.upper.draws [data.table] Raw dataset of GBD draws, from shared functions.
    #' @param acause [character] Cause of data to pull.
    #' @param year [integer] Year of data to pull.
    #' @param sex [integer] Sex of data to pull.
    #' @param measure [character] One of 'mx', 'yll', 'yld'.
    #' @param population [data.table] Population data.table, loaded from lower draws settings
    #' @param raked [character] Ignored for GBD; added to make interface consistent with ModelSettings.
    #' @param age [integer] Age to subset data to.
    #'
    #' @return Draws data.table
    process_gbd_draws = function(raw.upper.draws, acause, year, sex, measure, population, raked, age) {
      causes <- get_ids("cause")
      # Get total population in preparation for computing mortality rate
      sex_to_subset <- sex 
      pop.by.dimensions <- population[sex %in% sex_to_subset & age %in% self$ages,
        list(pop = sum(pop)),
        by = c(self$area_var, "year", "sex", "age")
      ]

      # Add location_id to population so that it can merge with the GBD draws
      pop.by.dimensions <- add_gbd_location_ids(pop.by.dimensions, natl_area = 1, area_var_column = self$area_var) # nolint
      # calculate mortality rate (mx) from deaths
      stop.if.not.expected.cols(raw.upper.draws, expected = c("year", "sex", "age", "location_id"), "draws", extra.ok = TRUE)
      stop.if.not.expected.cols(pop.by.dimensions, expected = c("year", "sex", "age", "location_id"), "deaths", extra.ok = TRUE)
      upper_draws <- merge(raw.upper.draws, pop.by.dimensions, by = c("year", "sex", "age", "location_id"), all = TRUE)

      if ("version_id" %in% colnames(upper_draws)) upper_draws[, "version_id" := NULL]
      upper_draws[, "location_id" := NULL]
      expected.cols <- c(self$area_var, "year", "sex", "age", "pop", "cause_id", paste0("draw_", seq(0, self$n.sims - 1)))
      stop.if.not.expected.cols(upper_draws, expected = expected.cols, preamble = "upper_draws prior to melt")

      draws <- data.table::melt(
        upper_draws,
        id.vars = c(self$area_var, "year", "sex", "age", "pop", "cause_id"),
        value.name = "deaths",
        variable.name = "sim"
      )

      # get_gbd_upper_draws returns "ylds" in rate space, others "mx, yll" in count space
      if (identical("yld", measure)) {
        draws[, "value" := deaths]
      } else {
        draws[, "value" := deaths / pop]
      }

      # remove temporary variables
      draws[, c("deaths", "pop") := NULL]
      draws[, sim := as.integer(sim)]

      # Subset only to specified age
      if (!is.null(age)) {
        age_to_subset <- age
        draws <- draws[age == age_to_subset]
      }

      # Validation against an incorrect calculation.
      stopifnot(nrow(draws[is.na("value")]) == 0)

      rm(raw.upper.draws)

      # Validate schema
      if (identical("yld", measure)) {
        draws[, `:=`(acause = "all_cause", cause_id = 294)]
      } else {
        draws <- merge(draws, causes[, .(cause_id, acause)], by_vars = "cause_id")
      }
      
      # Add default education and race columns 
      draws[, race:=race_default]
      draws[, edu:=edu_default]
      
      # Drop cause_id
      draws[, cause_id:=NULL]
      return(draws)
    }
  )
)
