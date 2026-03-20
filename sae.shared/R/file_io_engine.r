#' @title FileIOEngine
#'
#' @description Does IO for USHD draws and estimates files.
#'
#' @rdname FileIOEngine
#' @export
FileIOEngine <- R6::R6Class(
  "FileIOEngine",
  public = list(
    #' @field lt_draws_schema Schema for lifetables
    lt_draws_schema = NULL,
    
    #' @field upper_draws_schema Schema for upper draws
    upper_draws_schema = NULL,
    
    #' @field estimates_schema Schema for estimates
    estimates_schema = NULL,
    
    #' @field settings ModelSettings object to create FileIOEngine object
    settings = NULL,
    
    #' @field verbose Used to control how noisy warnings are, mostly in Schema class
    verbose = NULL,
    
    #' @description Create FileIOEngine object.
    #'
    #' @param settings ModelSettings object to create FileIOEngine object
    initialize = function(settings, verbose = TRUE) {
      self$upper_draws_schema <- Schema$upper_draws_schema(settings, verbose)
      self$estimates_schema <- Schema$estimates_schema(settings, verbose)
      self$lt_draws_schema <- Schema$lt_draws_schema(settings, verbose)
      
      # Validate that required settings are present.
      # These are needed for save_draws.
      required_settings <- c("races", "edu_groups", "ages", "area_var")
      for (setting in required_settings) {
        if (!setting %in% names(settings)) stop(sprintf("Required setting %s is not present!", setting))
      }
      self$settings <- settings
      self$verbose <- verbose
    },
    
    #' @description Loads data saved on the filesystem.
    #'
    #' @param type [character] Specified which type of data to pull. One of c('draws', 'estimates').
    #' @param dir [character] Root directory to load data from.
    #' @param acause [character] Cause(s) to pull data for.
    #' @param measure [character] Specify which measure to pull. One of c('mx', 'yll', 'lt').
    #' @param year [integer] Year to pull data for.
    #' @param sex [integer] Sex to pull data for
    #' @param raked [character] One of c('raked', 'raked_temp', 'prelim_raked', 'unraked') specifying which type of data to pull.
    #' @param race_levels [vector] Integer vector of races to pull.
    #' @param edu_levels [vector] Integer vector of education to pull.
    #' @param area_var [character] One of c('mcnty', 'state', 'natl') specifying geographic area.
    #' @param age [integer] Age to subset final results to.
    #' @param location_id [integer] The location_id for which draws are loaded.
    #' @param measure_id [integer] 1: death, 2: daly, 3: yld, 4: yll. This does not always correspond to measure.
    #'
    #' @return data.frame of draws or estimates
    load_data = function(type, dir, acause, measure, year, sex, raked,
                         race_levels, edu_levels, area_var, age = NULL,
                         location_id = NULL, measure_id = NULL) {
      stopifnot(type %in% c("draws", "est"))
      stopifnot(raked %in% c("raked", "raked_temp", "prelim_raked", "unraked"))
      if (identical(raked, "raked_temp") && is.null(age)) {
        lsae.utils::stop_or_quit("Cannot specify raked = 'raked_temp' when age is NULL", status = 3)
      }
      files <- self$get_file_list(
        type = type,
        dir = dir,
        acause = acause,
        measure = measure,
        year = year,
        sex = sex,
        age = age,
        raked = raked,
        area_var = area_var,
        race_levels = race_levels,
        edu_levels = edu_levels,
        location_id = location_id,
        measure_id = measure_id
      )

      self$validate_all_files_exist(files)
      draws <- self$load_data_files(files, age, measure = measure)
      # Models may produce a second set of "adjusted" outputs that are better. Subset so that only one data set is returned.
      draws <- subset_to_adjusted_if_necessary(draws, getOption("ushd.use_adjusted_data"))
      
      return(draws)
    },
    
    #' @title process_output_draws_and_estimates
    #' 
    #' @description Translate area/level, age, acause to location_id, age_group_id and cause_id respectively. Also, 
    #' reshapes data.table from long to wide format.
    #'
    #' @param data [data.table] draws or estimates that needs processing before saving.
    #' @param measure [character] pass through to translate_ages
    #' @param pivot [logical] Setting TRUE will reshape output data (draws) from long to wide format.
    #'
    #' @return data.table
    #' @export
    process_output_draws_and_estimates = function(data, measure = NULL, pivot = TRUE){
      # calculate location_id
      data <- add_gbd_location_ids(data, natl_area = 1, use_merged_location_id = TRUE)
      
      # calculate age_group_id
      ages <- self$settings$ages
      age_group_ids <- translate_ages(ages, "age_group_id", measure = measure)
      data_ages <- unique(data$age)
      age_diff <- setdiff(data_ages, ages)
      if (length(age_diff) > 0) {
        message(glue::glue("Age mismatch between settings$ages and draws {paste(age_diff, collapse = ", ")}"))
        for (age in age_diff) {
          if (age == 98) {
            ages <- c(ages, 98)
            age_group_ids <- c(age_group_ids, 37)
          }
          if (age == 99) {
            ages <- c(ages, 99)
            age_group_ids <- c(age_group_ids, 38)
          }
          message(glue::glue("Adding age == {age}"))
        }
      }
      age_mapper <- data.table(age = ages, age_group_id = age_group_ids)
      data <- merge(data, age_mapper, by = "age")
      
      # calculate cause_id
      causes <- get_ids("cause")
      data <- merge(data, causes[, .(cause_id, acause)], by = "acause")
      
      cols_to_delete <- c("level", "area", "age", "acause") 
      if ("pop" %in% colnames(data)) {
        cols_to_delete <- append(cols_to_delete, "pop")
      }
      
      # rename cols with id suffix (year, sex)
      setnames(data, c("sex", "year"), c("sex_id", "year_id"))
      
      data[, (cols_to_delete) := NULL]
      
      if (pivot) {
        required_cols <- c("year_id", "sex_id", "race", "edu", "sim", "ab", "measure_id", "metric_id", "rei_id", 
                           "location_id", "age_group_id", "cause_id")
        missing_cols <- setdiff(required_cols, colnames(data))
        excess_cols <- setdiff(colnames(data), required_cols)
        if (length(missing_cols) > 0) {
          lsae.utils::stop_or_quit(paste0("Output draws is missing required columns: ", paste0(missing_cols, collapse = ", ")))
        }
        if (length(excess_cols) > 0) {
          lsae.utils::stop_or_quit(paste0("Output draws has surplus columns: ", paste0(missing_cols, collapse = ", ")))
        }
        # convert long to wide format data.table
        data[, sim := paste0("draw_",sim-1)]
        
        # reshape from long to wide format.
        data <- dcast(
          data, 
          year_id + sex_id + edu + race + measure_id + metric_id + rei_id + location_id + age_group_id + cause_id ~ sim,
          value.var = "ab",
          drop = FALSE,
          fill = 0
        )
        
      }
      
      # set column order.
      setcolorder(data, c("age_group_id", "cause_id", "location_id", "measure_id", "metric_id", "race", "sex_id", "year_id"))
      return(data)
    },
    
    #' @description Saves raked draws
    #'
    #' @param draws [data.table] Draws to save.
    #' @param acause [character] Cause to save.
    #' @param save_dir [character] Directory to save draws to.
    #' @param measure [character] One of c('mx', 'yll'). Which measure to the data represents.
    #' @param year [integer] Year of data to save
    #' @param sex [integer] Sex of data to save.
    #' @param age [integer] Age of data. Used only for cause-specific saves (2-D raking)
    #' @param races [list] List of race indexes to save.
    #' @param edu_groups [list] List of edu_group indexes to save.
    #' @param type [character] What process is saving draws. Needs to be one of 'modeling', raking', 'aggregation', 'pred_yll'.
    save_draws = function(draws, acause, save_dir, measure, year, sex, 
                          type, age = NULL, 
                          races = self$settings$races, edu_groups = self$settings$edu_groups) {
      if (!type %in% c('modeling', 'raking', 'aggregation', 'pred_yll')) lsae.utils::stop_or_quit(sprintf("Type must be one of 'modeling', raking', 'aggregation', or 'pred_yll', was passed %s", type))
      schema <- Schema$output_draws_schema(self$settings, measure, self$verbose)
      draws <- schema$validate(
        data = draws, 
        measure = measure, 
        value_col = if (measure == 'lt' | measure == "FULL_lt") 'mx' else measure
      )
      if (identical(measure, "ab")) {
        draws <- self$process_output_draws_and_estimates(draws, measure = measure)
        iterations <- expand.grid(location_id = draws[, unique(location_id)], measure_id = draws[, unique(measure_id)])
      } else {
        iterations <- expand.grid(race = races, edu = edu_groups)
      }
      
      if (type == 'raking'){
        # For all-cause raking, save both all-age and age-stratified files.
        # The only exception is YLD, which doesn't need the age-stratified files.
        # Risk factors BMI Prevalence ('overweight' and 'obese') and mean_bmi also don't need age-stratified files.
        # Attributable burden doesn't need age-stratified files as well
        if (identical(measure, "ab") || acause %in% c("_all", "all_cause", "overweight", "obese", "mean_bmi")){
          self$save_all_age_draws(
            draws = draws, 
            iterations = iterations, 
            save_dir = save_dir, 
            acause = acause, 
            measure = measure, 
            year = year, 
            sex = sex, 
            raked = 'raked') 
          if (measure %in% c("mx", "yll")){
            # You save an age-specific file for every age in settings. 
            for (age_subset in self$settings$ages) { 
              self$save_age_specific_draws(
                draws = draws, 
                age = age_subset, 
                iterations = iterations, 
                save_dir = save_dir, 
                acause = acause, 
                measure = measure, 
                year = year, 
                sex = sex, 
                raked = 'raked_temp') 
            }
          }
        } else { # Saving by-cause raking, which requires by-age files. 
          if (is.null(age)) stop("Must specify age to save cause-specific raking files!")
          self$save_age_specific_draws(
            draws = draws, 
            age = age, 
            iterations = iterations, 
            save_dir = save_dir, 
            acause = acause, 
            measure = measure, 
            year = year, 
            sex = sex, 
            raked = 'raked_temp')
        } 
      } else { 
        # type one of 'modeling', 'aggregation', or 'pred_yll'.  
        # These scenarios are identical except for how the 'raked' argument is treated.
        raked = switch(
          type, 
          modeling = 'unraked',
          aggregation = 'raked', 
          pred_yll = 'prelim_raked'
        )
        self$save_all_age_draws(
          draws = draws, 
          iterations = iterations, 
          save_dir = save_dir, 
          acause = acause, 
          measure = measure, 
          year = year, 
          sex = sex, 
          raked = raked 
        )
      }
    },
    
    #' @param draws [data.table] Draws to save.
    #' @param iterations [data frame] All combinations (expand.grid output) of race and edu inputs.
    #' @param save_dir [character] Directory to save draws to.
    #' @param acause [character] Cause to save.
    #' @param measure [character] One of c('mx', 'yll'). Which measure to the data represents.
    #' @param year [integer] Year of data to save
    #' @param sex [integer] Sex of data to save.
    #' @param raked [character] Raked suffix.  
    #' @description Helper function for save_draws(), to save all-age file. 
    save_all_age_draws = function(draws, iterations, save_dir, 
                                  acause, measure, year, sex, raked){
      for (i in 1:nrow(iterations)) {
        if (identical(measure, "ab")) {
          subset_draws <- draws[location_id == iterations$location_id[i] & measure_id == iterations$measure_id[i], ]
          self$save_file(
            subset_draws, save_dir, acause, measure, "draws", year, sex,
            raked = raked,
            location_id = iterations$location_id[i], 
            measure_id = iterations$measure_id[i]
          )
        } else {
          self$save_file(
            draws, save_dir, acause, measure, "draws",
            year, sex, iterations$race[i], iterations$edu[i], raked
          )
        }
      } 
    },
    
    #' @param age [integer] Age of data. Used only for cause-specific saves (2-D raking)
    #' @param draws [data.table] Draws to save.
    #' @param iterations [data frame] All combinations (expand.grid output) of race and edu inputs.
    #' @param save_dir [character] Directory to save draws to.
    #' @param acause [character] Cause to save.
    #' @param measure [character] One of c('mx', 'yll'). Which measure to the data represents.
    #' @param year [integer] Year of data to save
    #' @param sex [integer] Sex of data to save.
    #' @param raked [character] Raked suffix.  
    #' @description Helper function for save_draws(), to save age-stratified files.
    save_age_specific_draws = function(age, draws, iterations, save_dir, 
                                       acause, measure, year, sex, raked){
      if (is.null(age)) stop("Age must be specified for cause-specific rakes!") 
      for (i in 1:nrow(iterations)) {
        self$save_file(
          draws, save_dir, acause, measure, "draws",
          year, sex, iterations$race[i], iterations$edu[i], raked, age 
        )
      }
    },
    
    #' @description Saves raked estimates
    #'
    #' @param estimates [data.table] Estimates to save.
    #' @param acause [character] Cause to save.
    #' @param save_dir [character] Directory to save draws to.
    #' @param measure [character] One of c('mx', 'yll'). Which measure to the data represents.
    #' @param year [integer] Year of data to save
    #' @param sex [integer] Sex of data to save.
    #' @param age [integer] Age to subset to
    #' @param raked [character] Raked suffix.  
    #' @param races [list] List of race indexes to save.
    #' @param edu_groups [list] List of edu_group indexes to save.
    save_estimates = function(estimates, acause, save_dir, measure, year, sex, 
                              age, raked, 
                              races = self$settings$races, edu_groups = self$settings$edu_groups) {
      if (!raked %in% c('raked', 'unraked', 'prelim_raked', 'raked_temp')){
        stop(sprintf("Unknown raked type. Was passed %s, options are raked, unraked, prelim_raked, or raked_temp."))
      }
      schema <- Schema$output_estimates_schema(self$settings, measure, self$verbose)
      estimates <- schema$validate(
        data = estimates, 
        measure = measure, 
        value_col = if (measure == "lt" | measure == "FULL_lt") "mx_mean" else paste0(measure, "_mean")
      )
      
      if (identical(measure, "ab")) {
        estimates <- self$process_output_draws_and_estimates(estimates, measure=measure, pivot = FALSE)
        iterations <- expand.grid(location_id = estimates[, unique(location_id)], measure_id = estimates[, unique(measure_id)])
      } else {
        iterations <- expand.grid(race = races, edu = edu_groups)
      }
      
      for (i in 1:nrow(iterations)) {
        if (identical(measure, "ab")) {
          subset_estimates <- estimates[location_id == iterations$location_id[i] & measure_id == iterations$measure_id[i], ]
          self$save_file(
            data = subset_estimates, 
            save_dir = save_dir, 
            acause = acause, 
            measure = measure, 
            type = "est", 
            year = year, 
            sex = sex,
            raked = raked,
            location_id = iterations$location_id[i], 
            measure_id = iterations$measure_id[i]
          )
        } else {
          self$save_file(
            data = estimates,
            save_dir = save_dir,
            acause = acause,
            measure = measure,
            type = "est",
            year = year,
            sex = sex,
            race_subset = iterations$race[i],
            edu_subset = iterations$edu[i],
            raked = raked,
            age_subset = age
          )
        }
      }
    },
    
    #' @description Helper function for save_draws and save_estimates.
    #' @param data [data.table] data to be saved. May be subset prior to saving based on other args.
    #' @param save_dir [character] Directory for directory to save to
    #' @param acause [character] Cause being saved.
    #' @param measure [character] One of 'mx', 'yll', 'yld'.
    #' @param type [character] One of 'draws', 'est', 'lt'.
    #' @param year [integer] Year of data being saved.
    #' @param sex [integer] Sex of data being saved.
    #' @param race_subset race to subset data to while saving.
    #' @param edu_subset edu to subset data to while saving.
    #' @param raked [character] raked status of data.
    #' @param age_subset [integer] age to subset data to while saving.
    #' @param location_id [integer] The location_id for which draws are saved.
    #' @param measure_id [integer] 1: death, 2: daly, 3: yld, 4: yll. This does not always correspond to measure.
    save_file = function(data, save_dir, acause, measure, type, year, sex, race_subset,
                         edu_subset, raked, age_subset = NULL, location_id = NULL, measure_id = NULL) {
      if (identical(measure, "ab")) {
        # calling cause_mx_draws_path separately so Vectorize doesn't error.
        ofile <- cause_mx_draws_path(save_dir,
                                     measure = measure,
                                     type = type,
                                     area_var = self$settings$area_var,
                                     year = year,
                                     raked = raked,
                                     location_id = location_id, 
                                     measure_id = measure_id
        )
      } else {
        ofile <- cause_mx_draws_path(save_dir,
                                     acause = acause,
                                     measure = measure,
                                     year = year,
                                     type = type,
                                     area_var = self$settings$area_var,
                                     sex = sex,
                                     age = age_subset,
                                     race = race_subset,
                                     raked = raked,
                                     edu = edu_subset
        )
      }
      message(sprintf("Saving file %s", ofile))
      
      if(identical(measure, "ab")) {
        saveHDF(data, file_path = ofile)
      } else if (!is.null(age_subset)) {
        saveRDS(data[edu == edu_subset & race == race_subset & age == age_subset],
                file = ofile,
                compress = TRUE
        )
      } else {
        saveRDS(data[edu == edu_subset & race == race_subset],
                file = ofile,
                compress = TRUE
        )
      }
    },
    
    #' @description Returns list of files to load for a given type
    #'
    #' @param type [character] One of 'draws', 'est'.
    #' @param dir [character] Directory for path
    #' @param acause [character] Cause to load.
    #' @param measure [character] One of 'mx', 'yll', 'yld', 'lt', 'pred', 'ab'
    #' @param year [integer] Year to load
    #' @param sex [integer] Sex to load
    #' @param age [integer] Age to subset to
    #' @param raked [character] What kind of raked data would you like to load?
    #' @param area_var [character] One of c('mcnty', 'state', 'natl') specifying geographic area.
    #' @param race_levels [vector] List of races from ModelSettings class
    #' @param edu_levels [vector] List of education levels from ModelSettings class
    #' @param location_id [integer] The location_id/s for which draws are loaded.
    #' @param measure_id [integer] 1: death, 2: daly, 3: yld, 4: yll. This does not always correspond to measure.
    get_file_list = function(type, dir, acause, measure, year, sex, age, raked, area_var, race_levels, edu_levels,
                             location_id = NULL, measure_id = NULL) {
      stopifnot(type %in% c("draws", "est"))
      stopifnot(measure %in% c("mx", "yll", "yld", "lt", "pred", "ab"))
      if (length(race_levels) == 0) stop("Can only load by-race draws!")
      files <- c()
      
      # Attributable burden raking
      if (identical(measure, "ab")) {
        if (is.null(location_id) || is.null(measure_id)) {
          lsae.utils::stop_or_quit("For attributable raking (ab), both location_id(state) and measure_id(1, 2, 3 or 4) are required.")
        }
        
        for (i in location_id) {
          file <- cause_mx_draws_path(dir,
                                      measure = measure,
                                      type = type,
                                      area_var = area_var,
                                      year = year,
                                      raked = raked,
                                      location_id = i, 
                                      measure_id = measure_id
          )
          message(sprintf("Loading file %s", file))
          files <- c(files, file)
        }
        return(files)
      }
      
      # Create list of all race, education combinations
      # all-race and by-race handled the same because model settings include defaults for race_levels not yet present in self$edu
      if (!is.null(edu_levels)) {
        iterations <- expand.grid(cause = acause, race = race_levels, edu = edu_levels, sex = sex)
      } else {
        iterations <- expand.grid(cause = acause, race = race_levels, sex = sex)
      }
      for (i in 1:nrow(iterations)) {
        file <- cause_mx_draws_path(dir,
                                    acause = iterations$cause[i],
                                    measure = measure,
                                    type = type,
                                    area_var = area_var,
                                    year = year,
                                    sex = iterations$sex[i],
                                    race = iterations$race[i],
                                    age = age,
                                    raked = raked,
                                    edu = iterations$edu[i]
        )
        message(sprintf("Loading file %s", file))
        files <- c(files, file)
      }
      
      if (is.null(edu_levels)) {
        edu_num <- 1
      } else {
        edu_num <- length(edu_levels)
      }
      if (length(files) != (length(race_levels) * edu_num * length(acause) * length(sex))) {
        stop(sprintf("files is length %i but races * education is length %i", length(files), (length(race_levels) * edu_num * length(acause) * length(sex))))
      }
      return(files)
    },
    
    #' @description Validates that all files exist from a given list.
    #'
    #' @param files list of files
    validate_all_files_exist = function(files) {
      lapply(files, touch)
      if (!all(file.exists(files))) {
        missing <- files[!file.exists(files)]
        stop(sprintf("The following files are missing:\n\t%s", paste(missing, collapse = "\n\t")))
      }
    },
    
    #' @name load_data_file
    #' @description Loads a single draws or estimates file from disk.
    #'
    #' @param file Filepath to RDS file.
    #' @param age Age to subset to.
    #' @return data.frame subset down to age, if specified.
    load_data_file = function(file, age = NULL, measure = NULL) {
      if (identical(measure, "ab")) {
        draws <- readHDF(file)
      } else {
        draws <- readRDS(file)
      }
      
      # process draws
      # Drop aggregate age bins, no matter what kind of data you've loaded 
      if ("age" %in% colnames(draws)){
        draws <- draws[!(age %in% c(98, 99)), ]
      }
      
      # If age is specified, return only this value. 
      if (!is.null(age)) {
        age_subset = age # data.table doesn't like when column names are in global scope
        draws <- draws[age == age_subset, ]
      }
      
      if (!identical(measure, "ab")) {
        cause <- basename(dirname(file))
        draws[, acause := cause]
      }
      if (nrow(draws) == 0) stop("Returning an empty draws file!")
      return(draws)
    },
    
    #' @description Binds together results from the load_data_file function.
    #'
    #' @param files list of files
    #' @param age The age to subset to.
    load_data_files = function(files, age, measure) {
      draws <- rbindlist(lapply(files, self$load_data_file, age = age, measure = measure), use.names = TRUE)
      return(draws)
    },
    
    #' @description Validates columns for a given data frame
    #'
    #' @param draws [data.frame] Loaded draws
    #' @param type [character] Specified which type of data to pull. One of c('draws', 'est').
    #' @param raked [character] One of c('raked', 'raked_temp', 'prelim_raked', 'unraked') specifying which type of data to pull.
    #' @param measure [character] One of c('mx', 'yll', 'yld', 'lt', 'FULL_lt', 'pred', 'ab'). Used for schema$validate().
    #' @param drop_additional_columns [logical] Passed to schema$validate(). 
    #'
    #' @return Validated draws.
    validate_schema = function(draws, type, raked, measure, drop_additional_columns = TRUE) {
      if (type == "draws") {
        if (measure == "lt" | measure == "FULL_lt"){
          draws <- self$lt_draws_schema$validate(
            data = draws, 
            measure = measure, # Lifetables actually have 'mx' as their value column
            value_col = 'mx',
            drop_additional_columns = drop_additional_columns
          )
        } else {
          # Inside FileIOEngine, draws will always have upper draws schema.
          # Population variables are merged on in ModelSettings$load_lower_draws().
          draws <- self$upper_draws_schema$validate(
            data = draws, 
            measure = measure,
            value_col = 'value',
            drop_additional_columns = drop_additional_columns
          )
        } 
      } else if (type == "est") {
        draws <- self$estimates_schema$validate(
          data = draws, 
          measure = measure, 
          value_col = "mean",
          drop_additional_columns = drop_additional_columns
        )
      } else {
        stop(sprintf("Don't know what schema to use for type %s.", type))
      }
      if (nrow(draws) == 0) stop("validate_schema returned empty data frame.")
      return(draws)
    }
  )
)
