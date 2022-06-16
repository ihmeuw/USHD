#' @title touch
#'
#' @description use unix touch command to force file metadata update
#'
#' @return output from system call
#'
#' @rdname touch
#' @export
touch <- function(path) {
  system(sprintf("touch -a --no-create %s", path), wait = TRUE)
}

#' @title saveRDS
#' @description Save an RDS file with permissions that don't cause problems
#' @seealso base::saveRDS docs
#'
#' @export
saveRDS <- function(object, file = "", ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL) {
  base::saveRDS(object, file = file, ascii = ascii, version = version, compress = compress, refhook = refhook)
  Sys.chmod(file, mode = "666", use_umask = FALSE)
}

#' @title load_child_cause_metadata
#'
#' @description Load metadata for child causes given a parent acause
#'
#' @param dir a run directory with the file "submitted_cause_list.csv"
#' @param parent_acause the parent acause who's children's metadata will be returned. Defaults to NULL, meaning no filtering
#' @param sex sex to filter metadata to. Drops irrelevant causes for sex. Defaults to NULL, meaning no filtering
#' @param age age to filter metadata to. Drops irrelevant causes for age. Defaults to NULL, meaning no filtering
#'
#' @return cause data frame
#'
#' @rdname load_child_cause_metadata
#' @export
load_child_cause_metadata <- function(dir, parent_acause = NULL, sex = NULL, age = NULL) {

  causes <- ModelSettings$get_cause_datatable(dir=dir, use.csv = FALSE)
  # validate
  if (!setequal(expected.cause.cols, intersect(expected.cause.cols, colnames(causes)))) {
    stop_or_quit("Causes schema seems to have changed")
  }

  # Filter: to child causes, exclude causes that aren't relevant for this sex/age
  causes <- subset_causes(causes, parent_acause, sex, age)
  
  # update cause tree so that 294 AKA _all is not its own parent
  causes[cause_id == 294, parent_id := 0] # nolint

  return(causes)
}

#' @title make_output_dir
#'
#' @description Helper function to create output_dir
#' @param dest_dir [character] a directory to create
#' @param verbose [logical] Verbose output
#'
#' @export
make_output_dir <- function(dest_dir, verbose = FALSE) {
  if (!dir.exists(dest_dir)) {
    if (verbose) print(sprintf("Creating dir %s", dest_dir))
    dir.create(dest_dir, recursive = TRUE)
  }
  make_group_writable_dir(dest_dir)
}

#' @title make_group_writable_dir
#' @description  Helper function that creates a directory which is group-read/writable
#'
#' @param dest_dir [character] Directory to be modifed or created
#' @param recursive [logical] When TRUE, create full path to directory if it doesn't exist
#'
#' @export
make_group_writable_dir <- function(dest_dir, recursive = FALSE) {
  if (dir.exists(dest_dir)) {
    write_permission <- file.access(dest_dir, 2)
    if (!identical(write_permission[[dest_dir]], 0L)) {
      msg <- paste0("Results need to be written to dir which is write protected: ", dest_dir)
      stop_or_quit(msg, status = 1)
    }
    return(NULL)
  }
  old_mask <- Sys.umask()
  Sys.umask("0002") # group writable umask

  dir.create(dest_dir, recursive = recursive)

  # set Sys.umask to old_mask
  Sys.umask(old_mask)
}

#' @title set_group_ownership
#'
#' @description  Helper function that sets the group ownership to group_name for dest_dir recursively
#'
#' @param group_name [character] Group to make group owner
#' @param dest_dir [character] Directory to be modified
#'
#' @export
set_group_ownership <- function(group_name, dest_dir) {
  cmd <- paste0(fs::path_package("sae.shared", "tools", "set_group_ownership.sh"), " '", group_name, "' ", dest_dir)
  system(cmd, wait = TRUE)
}

#' @title write_csv
#'
#' @description Helper function that creates csv file with group writable permissions
#'
#' @param x [data.table] data to write. Can also be a matrix or data frame.
#' @param file [character] path to write file to.
#' @param update [logical] attempt to update file by appending information in x to it?
#' @param row.names [logical] whether to include row names in the output.
#' @param quote [logical or numeric] whether to quote per rules of write.csv
#' @param lock [logical] Lock file and prevent other process from writing at the same time
#'
#' @export

write_csv <- function(x, file = "", update = FALSE, row.names = TRUE, quote = TRUE, lock = FALSE) {
  # Read an existing csv file
  if (lock) {
    file_glob = paste0(file, ".*.lock")
    if (length(Sys.glob(file_glob)) > 0) {
      msg <- paste0("File ", file, " is locked by lockfile: ", Sys.glob(file_glob))
      stop_or_quit(msg, status = 1)
    } else {
      lock_file <- paste0(file, ".", Sys.getenv('SLURM_JOB_ID'), ".lock")
      system(sprintf("touch %s", lock_file), wait = TRUE)
      system(paste0("chmod 666 ", lock_file), wait = TRUE)
    }
  }
  if (update) {
    if (!file.exists(file)) {
      warning(paste0("WARNING! Writing to a new file. 'update' set to TRUE but file does not exist: ", file))
    } else {
      tryCatch(
        {
          existing_data <- fread(file)
        },
        error = function(cond) {
          print(cond)
          msg <- paste0("File missing read-permission, cannot update: ", file)
          stop_or_quit(msg, status = 1)
        }
      )
      x <- unique(rbind(existing_data, x))
    }
  }

  # Write csv file
  tryCatch(
    {
      write.csv(x, file = file, row.names = row.names, quote = quote)
      cmd <- paste0("chmod 664 ", file)
      system(cmd, wait = TRUE)
    },
    error = function(cond) {
      print(cond)
      msg <- paste0("Missing write-permission: ", file)
      stop_or_quit(msg, status = 1)
    }
  )
  if (lock) {
    unlink(lock_file)
  }

}

#' @title subset_causes
#' @description Helper function for load_child_cause_metadata. Filters by parent acause, sex, and age.  Removes
#' child causes based on age or sex exclusions.  For example, _otherncd includes sids which is only valid for age 0.
#' If age = 5 is passed in, sids is removed from the returned cause data table.
#'
#' @param causes [data.table] causes to be subsetted
#' @param parent_acause [character] subset based on this parent cause default: NULL
#' @param sex [integer] sex or sexes to subset to e.g., 1. default: NULL
#' @param age [integer] age or ages to subset to. If NULL, assumes the files are NOT age-specific. Default: NULL.
#'
#' @return subsetted causes data.table
#'
#' @export
subset_causes <- function(causes, parent_acause = NULL, sex = NULL, age = NULL) {
  
  if (!is.null(parent_acause)) {
    causes <- causes[parent_id == causes[acause == parent_acause, cause_id], ]
  }
  if (nrow(causes) == 0) stop_or_quit("Returning an empty cause dataframe! Subsetting to parent_acause removed all rows.", status = 3)
  if (!is.null(sex)) {
    for (i in 1:nrow(causes)) {
      causes[i, sex_incl := sex %in% eval(parse(text = sexes))] 
    }
    causes <- causes[sex_incl == T]
    causes[, sex_incl := NULL]
  }
  if (nrow(causes) == 0) stop_or_quit("Returning an empty cause dataframe! Subsetting by sex removed all rows.", status = 3)
  if (!is.null(age)) {
    for (i in 1:nrow(causes)) {
      causes[i, age_incl := age %in% eval(parse(text = ages))]
    }
    causes <- causes[age_incl == T]
    causes[, age_incl := NULL]
  }
  if (nrow(causes) == 0) stop_or_quit("Returning an empty cause dataframe! Subsetting by age removed all rows.", status = 3)
  
  return(causes)
}

#' @title cause_mx_draws_path
#' @description Create the filepath to relevant draw or estimate (est) files. Useful for reading or writing
#'
#' @param root the root directory, where the model to be raked lives.
#' @param acause the cause in question. Ex: "_all"
#' @param measure which measure to rake, "mx" for mortality rates, "yll" for ylls, "yld" for years lived with disability
#' @param type the type of data saved. Must be either "draws" or "est", where the latter is for summaries (mean/upper/lower)
#' @param area_var area var, the geography level of the lower draws. Ex: "natl", "mcnty"
#' @param year year. Ex: 2000
#' @param sex sex or sexes to use in file path e.g., 1. default: NULL
#' @param race race. If NULL, assumes the files are NOT race-specific. Defaults to \code{race_default}. Ex: 1
#' @param age age. If NULL, assumes the files are NOT age-specific. Defaults to NULL. Ex: 15
#' @param raked string defining raked status. Must be "raked", "unraked", "raked_temp", "prelim_raked". Defaults to "unraked"
#'
#' @return filename [character] Path to file
#' @export
cause_mx_draws_path <- function(root, acause, measure = "mx", type = "draws", area_var, year, sex, race = race_default, age = NULL, raked = "unraked", edu = NULL) {
  if (!type %in% c("draws", "est")) {
    stop(sprintf("Unexpected 'type': %s", type))
  }

  if (!measure %in% c("mx", "yll", "lt", "yld", "FULL_lt")) {
    stop(sprintf("Unexpected 'measure': %s", measure))
  }

  # Convert from string, ie "unraked", to file stub, ie ""
  if (is.null(age) & raked == "raked_temp") {
    stop("Rake status 'raked_temp' only applicable for age-specific files; NULL given for 'age'")
  }
  file_suffix <- switch(raked,
    unraked = "",
    prelim_raked = "_prelim_raked",
    raked_temp = "_TEMP",
    raked = "_raked",
    stop(sprintf("Invalid raked arg '%s'", raked))
  )

  # If education was specified as !"all", race must be "all".
  if (!is.null(edu)) {
    if (!(edu == edu_default) & (race != race_default)) {
      stop_or_quit(sprintf("edu non-null and race is not %s (race_default) - was passed %s", race_default, race), status = 8)
    }
  }

  p.with.edu <- sprintf(
    "%s/%s/%s_%s_%s_%s_%s%s%s%s%s.rds", root, acause, measure, type, area_var, year, sex,
    # race is optional; if provided prefix with a "_"
    ifelse(is.null(race), "", paste0("_", race)),
    # education is optional; if provided prefix with a "_"
    ifelse(is.null(edu), "", paste0("_", edu)),
    # age is only needed for raked_temp files; prefix with "_"
    ifelse(raked != 'raked_temp', "", paste0("_", age)),
    # similarly for type, if "" then no "_", otherwise a "_" prefix
    file_suffix
  )

  p.without.edu <- sprintf(
    "%s/%s/%s_%s_%s_%s_%s%s%s%s%s.rds", root, acause, measure, type, area_var, year, sex,
    # race is optional; if provided prefix with a "_"
    ifelse(is.null(race), "", paste0("_", race)),
    # age is only needed for raked_temp files; prefix with a "_"
    ifelse(raked != "raked_temp", "", paste0("_", age)),
    "", # omit edu from path
    # similarly for type, if "" then no "_", otherwise a "_" prefix
    file_suffix
  )

  p <- if (getOption("ushd.use_edu_paths", FALSE)) p.with.edu else p.without.edu

  return(p)
}
# Vectorize cause_mx_draws_path.
cause_mx_draws_path <- Vectorize(
  cause_mx_draws_path,
  vectorize.args = c("measure", "type", "area_var", "year", "sex", "race", "edu"),
  USE.NAMES = FALSE
)


#' @title Load compiled estimates
#'
#' @description load compiled estimates e.g. mx_est_all_raked.rds that are produced by the compile_estimates.r
#' script which contain all demographic and area level combinations including aggregates (sex == 3, race == race_default).
#'
#' @param root Model run directory
#' @param cause Cause subfolder within run directory
#' @param measure Either "mx" or "yll", which type of estimates to pull
#' @param raked Boolean, pull raked or unraked estimates
#' @param use_adjusted IFF an "adjusted" column is present, specify which
#'        values to use. This should match whatever value in the "adjusted" column you
#'        wish to keep. Default 1 (use adjusted data)
#'
#' @return compiled estimate table subsetted to adjusted value
#'
#' @export
load_compiled_estimates <- function(root, cause, measure, raked, use_adjusted = 1) {
  est_path <- sprintf("%s/%s/%s_est_all%s.rds", root, cause, measure, ifelse(raked, "_raked", ""))

  if (!file.exists(est_path)) {
    msg <- paste0("file ", est_path, " does not exist, load_compiled_estimates failed")
    stop(msg)
  }

  touch(est_path)
  est <- readRDS(est_path)

  est <- subset_to_adjusted_if_necessary(est, use_adjusted)

  return(est)
}


#' @title Subset to adjusted if necessary
#'
#' @description If the passed `data` has an "adjusted" column, subset the dt to only include the values passed in to `use_adjusted`
#'
#' @param data data.table to subset
#' @param use_adjusted IFF an "adjusted" column is present, specify which
#'        values to use. This should match whatever value in the "adjusted" column you
#'        wish to keep.
#'
#' @return returns `data` subsetted in accordance with `use_adjusted`
#'
#' @export
subset_to_adjusted_if_necessary <- function(data, use_adjusted) {
  if (is.na(use_adjusted)){
    warning("use_adjusted is NA - returning data with both adjusted- and non-adjusted values")
    return(data)
  }
  if (class(use_adjusted) == "logical") {
    use_adjusted <- as.integer(use_adjusted)
  }

  if ("adjusted" %in% colnames(data)) {
    if (use_adjusted %in% unique(data$adjusted)) {
      warning(sprintf("'adjusted' detected in input data - subsetting to only rows where adjusted == %s and removing column", use_adjusted))
      data <- data[adjusted == use_adjusted, ]
      data <- data[, adjusted := NULL]
    } else {
      valid_adjusted <- paste(unique(data$adjusted), collapse = ",")
      msg <- sprintf("use_adjusted is '%s' but the valid values are %s - cannot subset correctly", use_adjusted, valid_adjusted)
      stop_or_quit(msg, status = 11)
    }
  }
  return(data)
}

#' @title get_gbd_upper_draws
#'
#' @description Use get_draws with params set by this function to pull GBD data.
#'
#' @param raking_area_var [character] One of c('state', 'natl') specifying geographic area to retrieve data for
#' @param cause_id [integer] GBD ID for the cause or causes to retrieve data for
#' @param year [integer] Year or years to retrieve data for
#' @param sex [integer]. (males = 1 females = 2 both = 3)
#' @param ages [list] List of ages to retrieve data for. Example: c(0, 5, 10, 15)
#' @param draw_settings [list] named list specifying status, gbd_round_id, decomp_step, and version. From settings
#' @param measure [character] Measure to pull from gbd.  One of 'mx' 'yll', or 'yld'. Default to 'mx'.
#' @param n.sims [integer] Number of draws or sims used in model. Default to 1000
#' @param override_quit [logical] Pass though to stop_or_quit. If TRUE will always stop() rather than quit(). Default to FALSE
#'
#' @return draws [data.table] set of draws from get_draws, with extra columns removed
#'
#' @export
get_gbd_upper_draws <- function(raking_area_var, cause_id, year, sex, ages, draw_settings, measure = "mx", n.sims = 1000, override_quit = F) {
  location_ids <- if (raking_area_var == "natl") { # nolint
    102
  } else if (raking_area_var == "state") {
    523:573
  } else {
    stop(sprintf("Invalid raking_area_var '%s' - expected 'natl' or 'state'", raking_area_var))
  }

  measure_id <- if (measure == "mx") {
    1
  } else if (measure == "yll") {
    4
  } else if (measure == "yld") {
    3 
  } else {
    stop("measure passed to get_gbd_upper_draws must be either 'mx' or 'yll' or 'yld'")
  }

  if (identical(measure, "yld")) {
    metric <- 3 # returns data in rate space
    source <- "como"
  } else {
    metric <- 1 # count space 
    source <- "codcorrect"
  }

  age_group_ids <- translate_ages(ages, "age_group_id")
  draw_settings <- as.list(draw_settings)

  # Validate and set up downsampling of draws
  if (n.sims > 1000) {
    stop(sprintf("Found invalid value for n.sims. Must be <= 1000, received %i", n.sims))
  }
  
  cause_id <- inj_reporting_exception_cause_list(draw_settings, cause_id)

  result <- tryCatch(
    {
      draws <- get_draws(
        # provided by call
        gbd_id = cause_id,
        location_id = location_ids,
        year_id = year,
        sex_id = sex,
        age_group_id = age_group_ids,
        status = draw_settings[["status"]],
        gbd_round_id = draw_settings[["gbd_round_id"]],
        decomp_step = draw_settings[["decomp_step"]],
        version = draw_settings[["version"]],
        # hard-coded
        gbd_id_type = "cause_id",
        measure = measure_id,
        metric = metric,
        source = source
      )
    },
    error = function(err) {
      stop_or_quit(paste0("get_draws() threw an error: ", err), status = 5, override_quit = override_quit)
    }
  )

  if (n.sims < 1000) {
    warning(sprintf("Downsampling: Dropping all but first %i draws", n.sims))
    draws[, (paste0("draw_", seq(n.sims, 999))) := NULL]
  }
  # standardize column names
  draws[, age := ages[match(age_group_id, age_group_ids)]]
  data.table::setnames(draws, c("year_id", "sex_id"), c("year", "sex"))

  # delete unused columns
  cols_to_delete <- c("measure_id", "metric_id", "age_group_id", "version_id")
  draws <- draws[, (cols_to_delete) := NULL]
}

#' @title Load upper draws for flexible raking.
#'
#' @description Loads either GBD draws or USHD draws with given arguments, and verifies that they match upper-draws schema.
#'
#' @param gbd_upper_draws Do you want to load GBD draws? (logical)
#' @param by_race Are these draws disaggregated by race? (logical)
#' @param dir Directory to load draws from. Only used if `gbd_upper_draws` is FALSE.
#' @param to_geo Geographic level of upper model. Options are c('mcnty', 'state', 'natl')
#' @param from_geo Geographic level of lower model. Options are c('mcnty', 'state', 'natl')
#' @param year Year to subset draws to. (int)
#' @param sex Sex to subset draws to. Options are c(1, 2). (int)
#' @param ages List of ages to subset draws to. Example: c(0, 5, 10, 15)
#' @param gbd_version GBD version to use. Only used if `gbd_upper_draws` is TRUE.
#' @param demographics Raking variables to collapse to. Currently not implemented - will be needed
#'        to load USHD draws. (vector)
#' @param population Data frame with population weights. (data.frame)
#' @param population_constants Used to collapse population weights file. Not currently in use - will
#'        be added once we load USHD draws. (vector)
#' @param cause_id Cause ID to extract draws for. Only used if `gbd_upper_draws` is TRUE.
#' @param measure Do you want to pull mortality or YLL draws? Options: c('mx', 'yll')
#' @param sims Number of draws to extract (int).
#' @param raked Do you want to extract raked or unraked draws? Options: c('raked', 'unraked')
#'
#' @return data.frame of draws
#'
#' @export
load_upper_rake_draws <- function(gbd_upper_draws, by_race, dir, to_geo, from_geo,
                                  year, sex, ages, gbd_version, demographics, population, population_constants,
                                  cause_id = 294, measure = "mx", sims = 1000, raked = "raked") {
  EXPECTED_COLS <- c("year", "sex", "age", "sim", to_geo, paste0(to_geo, "_value"))

  if (gbd_upper_draws) {
    raw.upper.draws <- get_gbd_upper_draws(
      raking_area_var = to_geo,
      cause_id = cause_id,
      year = year,
      sex = sex,
      ages = ages,
      draw_settings = gbd_version,
      measure = measure,
      n.sims = sims
    )

    # Get total population in preparation for computing mortality rate
    sex_to_subset <- sex 
    pop.by.dimensions <- population[sex == sex_to_subset, list(pop = sum(pop)), by = c(to_geo, "year", "sex", "age")]

    # Add location_id to population so that it can merge with the GBD draws
    pop.by.dimensions <- add_gbd_location_ids(pop.by.dimensions, natl_area = 1, area_var_column = to_geo)
    # calculate mortality rate (mx) from deaths
    stop.if.not.expected.cols(raw.upper.draws, expected = c("year", "sex", "age", "location_id"), "draws", extra.ok = TRUE)
    stop.if.not.expected.cols(pop.by.dimensions, expected = c("year", "sex", "age", "location_id"), "deaths", extra.ok = TRUE)
    upper_draws <- merge(raw.upper.draws, pop.by.dimensions, by = c("year", "sex", "age", "location_id"), all = TRUE)

    upper_draws[, (c("location_id", "cause_id")) := NULL]
    expected.cols <- c(to_geo, "year", "sex", "age", "pop", paste0("draw_", seq(0, sims - 1)))
    stop.if.not.expected.cols(upper_draws, expected = expected.cols, preamble = "upper_draws prior to melt")

    draws <- data.table::melt(
      upper_draws,
      id.vars = c(to_geo, "year", "sex", "age", "pop"),
      value.name = "deaths",
      variable.name = "sim"
    )
    draws[, paste0(to_geo, "_value") := deaths / pop]
    draws[, c("deaths", "pop") := NULL]
    draws[, sim := as.integer(sim)]

    # Validation against an incorrect calculation.
    stopifnot(nrow(draws[is.na(get(paste0(to_geo, "_value")))]) == 0)

    # Clean up space
    rm(raw.upper.draws)
  } else {
    path <- cause_mx_draws_path(dir, acause = "_all", measure = measure, type = "draws", area_var = to_geo, year = year, sex = sex, race = race_default, raked = "raked")
    touch(path)
    draws <- readRDS(path)
    setnames(draws, measure, paste0(to_geo, "_value"))
    draws <- draws[, ..EXPECTED_COLS]
  }

  stop.if.not.expected.cols(
    draws,
    expected = EXPECTED_COLS,
    preamble = "Upper draws check"
  )

  return(draws)
}

#' @title Load lower draws for flexible raking.
#'
#' @description Loads USHD lower draws, and verifies that they match lower-draws schema.
#'
#' @param by_race Are these draws disaggregated by race? (logical)
#' @param dir Directory to load draws from.
#' @param to_geo Geographic level of upper model. Options are c('mcnty', 'state', 'natl')
#' @param from_geo Geographic level of lower model. Options are c('mcnty', 'state', 'natl')
#' @param year Year to subset draws to. (int)
#' @param sex Sex to subset draws to. Options are c(1, 2). (int)
#' @param ages List of ages to subset draws to. Example: c(0, 5, 10, 15)
#' @param demographics Raking variables to collapse to. (vector)
#' @param population Data frame with population weights. (data.frame)
#' @param population_constants Used to collapse population weights file. (vector)
#' @param measure Do you want to pull mortality or YLL draws? Options: c('mx', 'yll')
#' @param sims Number of draws to extract (int).
#' @param raked Do you want to extract raked or unraked draws? Options: c('raked', 'unraked')
#'
#' @return data.frame of draws
#'
#' @export
load_lower_rake_draws <- function(by_race, dir, to_geo, from_geo,
                                  year, sex, ages, demographics, population, population_constants,
                                  measure = "mx", sims = 1000, raked = "raked") {
  if (by_race) {
    draws <- read_county_race_draws(
      dir = dir,
      area_var = from_geo,
      year = year,
      sex = sex,
      raked = raked,
      acause = "_all", 
      race = "all",
      demographics = demographics,
      population = population,
      population_constants = population_constants,
      measure = measure
    )
  } else {
    draws <- read_county_draws(
      dir = dir,
      area_var = from_geo,
      year = year,
      sex = sex,
      raked = raked,
      acause = "_all", 
      demographics = demographics,
      population = population,
      population_constants = population_constants,
      measure = measure
    )
  }

  # Change to the general 'value' for rake
  setnames(draws, measure, "value")

  demographics <- demographics[!demographics == "cause"]

  stop.if.not.expected.cols(
    draws,
    expected = c(demographics, "value", "acause", "wt", "pop"),
    preamble = "Lower draws check"
  )

  return(draws)
}

#' @title load_population_metadata
#'
#' @description Load population/weight metadata
#'
#' @param weights_file [character] Path to weights file
#' @param pop_file [character] Path to pop file
#' @param area_var column name of more granular (lower) location level to be raked up from
#' @param raking_area_var [character] One of c('mcnty', 'state', 'natl') specifying geographic area to rake to
#' @param year [integer] Year to subset pop data to
#' @param by_race [logical] Are these draws disaggregated by race? 
#' @param by_edu [logical] Are these draws disaggregated by education? default: FALSE
#'
#' @return population data [data.table] subsetted by params
#'
#' @export
load_population_metadata <- function(weights_file, pop_file, area_var, raking_area_var, year, by_race, by_edu = FALSE) {
  year_tmp <- year

  if (!file.exists(weights_file)) stop(sprintf("weights file %s missing", weights_file))
  if (!file.exists(pop_file)) stop(sprintf("pop file %s missing", pop_file)) # nolint

  # columns: <area_var>, state, wt
  weights <- readRDS(weights_file)
  if (!area_var %in% colnames(weights)) stop(sprintf("area_var %s not in weights file", area_var))

  pop_df <- load_population(pop_file)

  # If not by-race, aggregate the population file to all-race.
  if (!by_race) {
    message("Aggregating population file to all-race")
    pop_df[, race := race_default]
    by_vars <- names(pop_df)[!names(pop_df) %in% c("race", "pop")]
    pop_df <- pop_df[, list(pop = sum(pop)), keyby = by_vars]
    if (nrow(pop_df) == 0) {
      msg <- sprintf("Aggregating population file %s led to 0 rows with by_vars %s", pop_file, paste(by_vars, collapse = ", "))
      stop_or_quit(msg, status = 4)
    }
  }

  pop_has_race <- "race" %in% colnames(pop_df)
  if (by_race) {
    if (!pop_has_race) {
      stop(sprintf("by_race is set to TRUE but population file %s does not have a race column", pop_file))
    }
  } else {
    if (pop_has_race) {
      # unexpected pop column - verify this is all-race
      if (!unique(pop_df$race) == race_default) {
        races <- unique(pop_df$race)
        stop(sprintf("by_race is FALSE but population file has races %s", paste(races, collapse = ", ")))
      }
    }
  }

  expected_cols <- c(area_var, raking_area_var, "year", "sex", "age", "pop")
  # pop file has mcnty and state vars, so when raking area var is natl, need to make
  # natl column and add state to expected vars
  if (raking_area_var == "natl") {
    pop_df[, natl := 1]
  }

  if (by_race) expected_cols <- c(expected_cols, "race")
  if (by_edu) expected_cols <- c(expected_cols, "edu")
  stop.if.not.expected.cols(pop_df, expected = expected_cols, preamble = "pop_file", extra.ok = TRUE)

  pop_df <- pop_df[year == year_tmp, ]
  # collapse population to sum for our dimensions
  by_cols <- c(area_var, "year", "sex", "age")
  if (by_race) by_cols <- c(by_cols, "race")
  if (by_edu) by_cols <- c(by_cols, "edu")
  pop_df <- pop_df[, list(pop = sum(pop)), keyby = by_cols]

  id_vars <- intersect(names(weights), names(pop_df))
  metadata <- merge(weights, pop_df, by = id_vars, all = TRUE)

  expected.metadata.cols <- c(raking_area_var, "year", "sex", "age", "pop", area_var, "wt")
  if (by_race) expected.metadata.cols <- c(expected.metadata.cols, "race")
  if (by_edu) expected.metadata.cols <- c(expected.metadata.cols, "edu")
  metadata <- metadata[, ..expected.metadata.cols]
  stop.if.not.expected.cols(metadata, expected = expected.metadata.cols, preamble = "metadata")

  return(metadata)
}



#' @title read_county_draws
#'
#' @description Read in county specific mortality draws for a given geographic level, year, sex, age(s), and cause.
#' Optionally attachs population if demographics and population dataframe are supplied
#'
#' @param dir run directory
#' @param area_var column name of more granular (lower) location level to be raked up from
#' @param year year to subset to (ex: 1995, 2015)
#' @param sex sex to subset to (ex: 1, 2)
#' @param raked String indicating what rake status to load - one of "unraked", "prelim_raked", or "raked"
#' @param age specific age to read in (Ex: 0, 15, 80). If age is NULL, reads in all ages except 98, 99. Defaults to NULL
#' @param acause acause(s) to read in. Defaults to "_all" for all-cause mortality. Can select more than one
#' @param demographics demographic columns. Must be non-null to attach population to draws. Defaults to NULL
#' @param population population dataset detailed at least by demographics.
#'        Must be non-null to attach population. Defaults to NULL
#' @param population_constants columns to carry over from population dataset to merge with draws. Typically "wt"
#' @param measure which measure to rake, "mx" for mortality rates, "yll" for ylls
#'
#' @export
read_county_draws <- function(dir, area_var, year, sex, raked, age = NULL, acause = "_all",
                              demographics = NULL, population = NULL, population_constants = c(), measure = "mx", filter.age = NULL) {
  if (!is.null(age) && !is.null(filter.age)) {
    stop_or_quit("Do not provide age and filter.age args to read_county_draws")
  }
  rbindlist(
    lapply(
      acause,
      read_county_draws_core,
      dir = dir,
      area_var = area_var,
      year = year,
      sex = sex,
      raked = raked,
      age = age,
      demographics = demographics,
      population = population,
      population_constants = population_constants,
      measure = measure,
      filter.age = filter.age
    ),
    use.names = TRUE
  )
}

#' @title read_county_draws_core
#' @description Helper function for read_county_draws. Forms the core of the function
#' for easier looping over
#'
#' @export
read_county_draws_core <- function(dir, area_var, year, sex, raked, age, acause, demographics, population, population_constants, measure = "mx", filter.age) {
  f <- cause_mx_draws_path(dir, acause = acause, measure = measure, type = "draws", area_var = area_var, year = year, sex = sex, race = race_default, age = age, raked = raked)
  if (!file.exists(f)) stop(sprintf("file %s does not exist", f))
  message(sprintf("Reading %s", f))
  touch(f)
  draws <- readRDS(f)

  # process draws
  if (is.null(age)) {
    # If age is null, we've read in the file with all age groups
    # either filter to a specific age OR omit special ages 98, 99 which are not used
    if (is.null(filter.age)) {
      draws <- draws[!(age %in% c(98, 99)), ]
    } else {
      draws <- draws[age == filter.age, ]
    }
  }
  draws <- draws[, acause := acause]
  if ("race" %in% names(draws)) {
    draws <- draws[, race := NULL]
  }
  draws <- draws[, level := NULL] 
  if (!area_var %in% colnames(draws)) {
    stop.if.not.expected.cols(draws,
      expected = "area", extra.ok = TRUE,
      preamble = sprintf("read_county_draws_core - checking for area as %s is not present in %s", area_var, f)
    )
    setnames(draws, "area", area_var)
  }

  if (!is.null(demographics) && !is.null(population)) {
    # We may have 'cause' and 'sim' in lower_raking_vars, but they will never be variables in population table.
    # Remove them from the vector of "by" columns here.
    demographics <- demographics[!demographics %in% c("sim", "cause")]
    population <- population[, list(pop = sum(pop)), by = c(demographics, population_constants)]
    draws <- merge(draws, population, by = demographics)
  }

  return(draws)
}
#' @title read_county_race_draws
#'
#' @description Read in county-race specific mortality draws for a given geographic level, year, sex, age(s),
#' set of races, and cause.
#' Optionally attaches population if demographics and population dataframe are supplied
#'
#' @param dir run directory
#' @param area_var column name of more granular (lower) location level to be raked up from
#' @param year year to subset to (ex: 1995, 2015)
#' @param sex sex to subset to (ex: 1, 2)
#' @param raked String indicating what rake status to load - one of "unraked", "prelim_raked", or "raked"
#' @param age specific age to read in (Ex: 0, 15, 80). If age is NULL, reads in all ages except 98, 99. Defaults to NULL
#' @param race race to read in. Defaults to "all", which mean each individual race in the run with be read in.
#' @param acause acause(s) to read in. Defaults to "_all" for all-cause mortality. Can select more than one
#' @param demographics demographic columns. Must be non-null to attach population to draws. Defaults to NULL
#' @param population population dataset detailed at least by demographics.
#'        Must be non-null to attach population. Defaults to NULL
#' @param population_constants columns to carry over from population dataset to merge with
#' @param measure which measure to load, "mx" for mortality rates, "yll" for ylls
#' @param use_adjusted IFF an "adjusted" column is present, specify which
#'        values to use. This should match whatever value in the "adjusted" column you
#'        wish to keep.
#'
#' @export
read_county_race_draws <- function(dir, area_var, year, sex, raked, age = NULL, race = "all", acause = "_all",
                                   demographics = NULL, population = NULL, population_constants = c(),
                                   measure = "mx", use_adjusted = 1, filter.age = NULL) {
  if (!is.null(age) && !is.null(filter.age)) {
    stop_or_quit("Do not provide age and filter.age args to read_county_race_draws")
  }

  rbindlist(
    lapply(
      acause,
      read_county_race_draws_core,
      dir = dir,
      area_var = area_var,
      year = year,
      sex = sex,
      raked = raked,
      age = age,
      race = race,
      demographics = demographics,
      population = population,
      population_constants = population_constants,
      measure = measure,
      use_adjusted = use_adjusted,
      filter.age = filter.age
    ),
    use.names = T
  )
}


#' @title read_county_race_draws_core
#' @description Helper function for read_county_race_draws Forms the core of the function
#' for easier looping over
#' @export
read_county_race_draws_core <- function(dir, area_var, year, sex, raked, age, race, acause, demographics,
                                        population, population_constants, measure, use_adjusted, filter.age) {
  dir.settings <- new.env()
  get_settings(get_cause_dir(dir, acause), dest.env = dir.settings)

  if (length(race) == 1 && race == "all") {
    race <- dir.settings$races
  } else {
    missing.races <- setdiff(race, dir.settings$races)
    if (length(missing.races) > 0) {
      stop(sprintf("read_country_race_draws: requested races %s but %s are not present in model run %s", paste(race, collapse = "/"), paste(missing.races, collapse = "/"), dir))
    }
  }

  files <- c()
  for (r in race) {
    files <- c(files, cause_mx_draws_path(dir, acause = acause, measure = measure, type = "draws", area_var = area_var, year = year, sex = sex, race = r, age = age, raked = raked))
  }

  if (!all(file.exists(files))) {
    missing <- files[!file.exists(files)]
    stop(sprintf("The following files are missing:\n\t%s", paste(missing, collapse = "\n\t")))
  }
  lapply(files, touch)
  draws <- rbindlist(lapply(files, readRDS))

  # process draws
  if (is.null(age)) {
    # If age is null, we've read in the file with all age groups
    # either filter to a specific age OR omit special ages 98, 99 which are not used
    if (is.null(filter.age)) {
      draws <- draws[!(age %in% c(98, 99)), ]
    } else {
      draws <- draws[age == filter.age, ]
    }
  }

  draws <- draws[, acause := acause]
  draws <- draws[, level := NULL] 
  if (!area_var %in% colnames(draws)) {
    stop.if.not.expected.cols(draws,
      expected = "area", extra.ok = TRUE,
      preamble = sprintf("read_county_race_draws_core - checking for area as %s is not present in %s", area_var, paste(files, collapse = ", "))
    )
    setnames(draws, "area", area_var)
  }

  draws <- subset_to_adjusted_if_necessary(draws, use_adjusted)

  if (!is.null(demographics) && !is.null(population)) {
    message("merging population onto draws")
    # Population doesn't have draws so remove column
    demographics <- demographics[demographics != "sim"]
    population <- population[, list(pop = sum(pop)), by = c(demographics, population_constants)]
    draws <- merge(draws, population, by = demographics)
  }

  if ("pop.x" %in% colnames(draws)) {
    if (all(draws$pop.x == draws$pop.y)) {
      message("Renaming things.")
      draws[, pop.y := NULL]
      setnames(draws, "pop.x", "pop")
    } else {
      stop("pop.x present but population values are NOT EQUAL")
    }
  }

  return(draws)
}

#' @title get_cause_dir
#'
#' @description Return directory for cause
#'
#' @param root_dir root model output directory.
#' @param acause character acause e.g., "_all", "_mental", or "tb_mdr"
#'
#' @return path to directory with outputs for cause
#'
#' @export
get_cause_dir <- function(root_dir, acause) {
  cause_dir <- file.path(root_dir, acause)
  if (dir.exists(cause_dir)) {
    return(cause_dir)
  }
  # handle case where model run is not cause specific and thus does not have cause-specific subdirs
  if (acause == "_all") {
    warning(sprintf("Outputs for acause '_all' not in '_all/' subdirectory - assuming in root dir '%s", root_dir))
    return(root_dir)
  } else {
    stop(
      "Model run does not have cause-specific death results. Can only request ",
      "all-cause draws, requested cause '", acause, "'"
    )
  }
}

#' @title expand_file_template
#'
#' @description Expand a file template into all desired permutations
#'
#' @param template a function to apply e.g., `paste0(dir, "/", var, "_est_", level, "_", year, "_", sex, "_", race, ifelse(raked, "_raked", ""), ".rds")`
#' @param ... named arguments for all values that need cross-join expansion
#'
#' @return vector of results from template
#' @examples
#' \dontrun{
#' dir <- "THE_DIR"
#' raked <- FALSE
#' ages <- c(1, 5, 10, 20)
#' sexes <- c(1, 2)
#' expand_file_template(
#'   paste0(dir, "/my_file_", age, "_", sex, ifelse(raked, "_raked", ""), ".rds"),
#'   age = ages,
#'   sex = sexes
#' )
#' # [1] "THE_DIR/my_file_1_1.rds"  "THE_DIR/my_file_1_2.rds"  "THE_DIR/my_file_5_1.rds"  "THE_DIR/my_file_5_2.rds" "THE_DIR/my_file_10_1.rds"
#' # [6] "THE_DIR/my_file_10_2.rds" "THE_DIR/my_file_20_1.rds" "THE_DIR/my_file_20_2.rds"
#' }
#' @export
expand_file_template <- function(template, ...) {
  # substitute the code block so that we may evaluate it in a different context
  template <- substitute(template)
  # get remaining args passed by name
  args <- list(...)
  # create a cross-join of these
  cj <- do.call(data.table::CJ, args)
  # evaluate the template
  # use our cross-join data.table as the environment
  # look for any values not in the cj in the calling parent environment
  eval(template, envir = cj, enclos = parent.frame())
}
