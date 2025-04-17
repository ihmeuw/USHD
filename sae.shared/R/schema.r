#' @title Schema
#'
#' @description Defines a set of required columns, their types, and accepted values.
#' This class will make the following corrections:
#' 1. If 'race' is not specified, assign the default race value
#' 2. If 'edu' is not specified, assign the default edu value.
#' 3. If 'area/level' are specified as the geographic variable, rename to 'mcnty/state/natl' etc.
#' This is helpful for merging with population.
#' 4. Correct measure columns ('mx', 'yll') to the more generic 'value'.
#' 5. Subset to required columns, after validating that they all exist
#'  (**Note - if education/race are not specified as required columns,
#' they will be dropped even if they have been assigned a default value.
#' We may want to revisit this design at some point.)
#'
#' After corrections, it will make the following checks:
#' 1. That all 'superset' columns (e.g., year, sex) have a subset of allowed values.
#' 2. That all 'exact match' columns (e.g., race) have exactly the specified values.
#'
#' @rdname Schema
#' @export
Schema <- R6::R6Class(
  "Schema",
  public = list(
    #' @field allowed_ages [integer] Ages that can have non-zero values in data.
    allowed_ages = NULL,
    
    #' @field exact_match_cols [character] columns whose values must exactly match schema
    exact_match_cols = NULL,

    #' @field required_cols [list] names of column names and values of list of schema requirements
    required_cols = NULL,

    #' @field superset_cols [character] columns whose values must be a subset of provided values in required_cols
    superset_cols = NULL,

    #' @field verbose [logical] If TRUE, print more log messages to console.
    verbose = NULL,

    #' @field purpose [character] One of c('load', 'save'). Is this schema being used for read or write?
    purpose = NULL,

    #' @field area_var [character] area data to have schema checked is relevant for.
    area_var = NULL,

    #' @name initialize
    #' @param required_cols [list] Columns that have to exist in data frame to pass validation.
    #' @param purpose [character] One of c('load', 'save'). Is this schema being used for read or write?
    #' Used to determine validation steps.
    #' @param settings [list] Settings passed from ModelSettings. Used to capture "settings$area_var".
    #' @param verbose [logical] If TRUE, print more log messages to console.
    initialize = function(required_cols, purpose, settings, verbose = TRUE) {
      # Initial validation
      if (!purpose %in% c("load", "save")) lsae.utils::stop_or_quit("purpose must be one of 'load', 'save'", status = 4)

      self$required_cols <- required_cols
      self$purpose <- purpose
      self$area_var <- settings$area_var # We can throw away the rest of 'settings', we just need area_var.
      self$verbose <- verbose

      # Make sure 'required_cols' is in the correct format
      stopifnot(typeof(required_cols) == "list")
      stopifnot(typeof(verbose) == "logical")
      if (is.null(names(required_cols))) stop("You must passed a named list for 'required_cols'.")
      for (col in names(required_cols)) {
        if (!typeof(required_cols[[col]]) == "list") stop(sprintf("Each required column element must be a list. Incorrectly specified column %s.", col))
        if (!setequal(names(required_cols[[col]]), c("type", "validation", "values"))) {
          stop(sprintf("Validation column %s needs to have elements 'type, validation, values.' Was passed %s.", col, paste0(names(required_cols[[col]]), collapse = ",")))
        }
        if (!required_cols[[col]][["type"]] %in% c("numeric", "character", "logical")) {
          stop(sprintf("Type for column %s must be one of numeric, character, or logical. Was passed %s.", col, required_cols[[col]][["type"]]))
        }
      }

      # Extract 'exact_match' and 'superset' column vectors
      superset_cols <- c()
      exact_match_cols <- c()
      for (col in names(required_cols)) {
        if (required_cols[[col]][["validation"]] == "superset") {
          superset_cols <- c(superset_cols, col)
        } else if (required_cols[[col]][["validation"]] == "exact_match") {
          exact_match_cols <- c(exact_match_cols, col)
        } else if (required_cols[[col]][["validation"]] == "age") {
          self$allowed_ages = required_cols[[col]][["values"]]
        } else {
          stop(sprintf("Invalid validation option passed for %s. Was passed %s, must be one of 'superset' or 'exact_match'.", col, required_cols[[col]][["validation"]]))
        }
      }
      self$superset_cols <- superset_cols
      self$exact_match_cols <- exact_match_cols
    },

    #' @description Corrects data to the accepted schema, if possible.
    #' @param results [list] Length 2 list.
    #' First element is data to be corrected, second element is an errors vector.
    #' @param measure [character] One of c('mx', 'yll', 'yld').
    #' @param drop_additional_columns [logical] Drop additional columns beyond 'required_cols' vector?
    #' @param purpose [character] If 'save', correct the geography columns to level/area.
    correct = function(results, measure, drop_additional_columns, purpose) {
      # Include race and education dimensions, if not specified
      results <- self$correct_race_dimension(results)
      results <- self$correct_edu_dimension(results)

      # Standardize column names
      if (purpose == "save") {
        results <- self$correct_geo_dimension_output(results)
        results <- self$correct_value_column_output(results, measure)
      } else {
        results <- self$correct_geo_dimension_input(results)
        if (measure != 'lt'){ # For lifetables, we want 'mx' not 'value'
          results <- self$correct_value_column_input(results)
        } 
      }

      # Subset to required columns
      missing_cols <- setdiff(names(self$required_cols), names(results$data))
      if (length(missing_cols) != 0) {
          stop(sprintf("Required column/s missing: %s", paste(missing_cols, collapse = ","))) # This has to be an error so the subset call below will work.
      }
      if (drop_additional_columns) results <- self$drop_additional_columns(results)

      if (nrow(results$data) == 0) results$errors <- c(results$errors, ("'correct' method is returning an empty data frame."))
      return(results)
    },

    #' @description Changes race dimension to accepted schema
    #' @param results [data.table] Data.table to cleanup.
    correct_race_dimension = function(results) {
      if (!"race" %in% names(results$data)) results$data[, race := race_default]
      return(results)
    },

    #' @description Changes edu dimension to accepted schema
    #' @param results [data.table] Data.table to cleanup.
    correct_edu_dimension = function(results) {
      # We expect a lot of backwards-compatibility changes for education,
      # so also retroactively correct these settings
      if (is.null(self$required_cols[["edu"]][["values"]]) | !"edu" %in% names(results$data)) {
        results$data[, edu := edu_default]
        self$required_cols[["edu"]][["values"]] <- edu_default
        self$required_cols[["edu"]][["type"]] <- "numeric"
        self$required_cols[["edu"]][["validation"]] <- "exact_match"
      }
      return(results)
    },

    #' @description Changes geography columns to match schema on read
    #' (Changes area/level to mcnty/state/natl)
    #' @param results [data.table] Data.table to cleanup.
    correct_geo_dimension_input = function(results) {
      # If data has 'mcnty' instead of 'level/area', pass.
      if (any(c("mcnty", "state", "natl") %in% names(results$data))) {
        return(results)
      }

      # Otherwise, we expect 'level' and 'area' to be in column names.
      if (!"level" %in% names(results$data)) {
        results$errors <- c(results$errors, (sprintf("Data does not have an acceptable geo schema. Requires one of 'mcnty' or 'level/area' columns. Data names are %s.", paste0(names(results$data), collapse = ", "))))
      }
      if (!"area" %in% names(results$data)) {
        results$errors <- c(results$errors, (sprintf("Data does not have an acceptable geo schema. Requires one of 'mcnty' or 'level/area' columns. Data names are %s.", paste0(names(results$data), collapse = ", "))))
      }
      geo_col <- unique(results$data$level)
      results$data[, level := NULL]
      setnames(results$data, "area", geo_col)

      return(results)
    },

    #' @description Changes geography columns to match schema on write
    #' (Changes mcnty/state/natl to area/level)
    #' @param results [data.table] Data.table to cleanup.
    correct_geo_dimension_output = function(results) {
      # If data has 'level/area', pass.
      if (all(c("level", "area") %in% names(results$data))) {
        return(results)
      }

      # Otherwise, we expect the area_var to be in data (mcnty/state/natl).
      geo_col <- self$area_var
      results$data$level <- geo_col
      setnames(results$data, geo_col, "area")

      return(results)
    },

    #' @description Corrects value column on read
    #' (Changes mx/yll to 'value')
    #' @param results [data.table] Data.table to cleanup.
    correct_value_column_input = function(results) {
      # First, correct vector of names. This catches all estimates cases.
      names <- names(results$data)
      names <- gsub("mx_|yll_", "", names)
      names(results$data) <- names

      # Then, rename specific columns. This catches the draws cases.
      if ("mx" %in% names(results$data)) setnames(results$data, "mx", "value")
      if ("yll" %in% names(results$data)) setnames(results$data, "yll", "value")
      if ("yld" %in% names(results$data)) setnames(results$data, "yld", "value")
      if ("pred" %in% names(results$data)) setnames(results$data, "pred", "value")
      if ("ab" %in% names(results$data)) setnames(results$data, "ab", "value")

      return(results)
    },

    #' @description Corrects value column on write
    #' @details Unlike correct_value_column_input, we only expect the column 'value'
    #' to be present in data at this time. So we don't need to correct for "_ub", "_lb", etc. fields.
    #' This is only needed for draws-saving, not estimates-saving.
    #' (Changes 'value' to mx/yll/yld)
    #' @param results [data.table] Data.table to cleanup.
    #' @param measure [character] One of c('mx', 'yll', 'yld'). Measure to correct column names to.
    correct_value_column_output = function(results, measure) {
      if ("value" %in% names(results$data)) setnames(results$data, "value", measure)
      return(results)
    },

    #' @description Drops unnecessary columns from the data
    #' @param results [data.table] Data.table to cleanup.
    drop_additional_columns = function(results) {
      keep_cols <- names(self$required_cols)
      if ('adjusted' %in% names(results$data) && is.na(getOption('ushd.use_adjusted_data'))){
        keep_cols = c(keep_cols, 'adjusted')
      }
      if (self$verbose) message(sprintf("Subsetting to required columns %s", paste0(keep_cols, collapse = ", ")))
      results$data <- results$data[, ..keep_cols]
      return(results)
    },

    #' @description Validates if the given data frame passes schema checks.
    #' @param data [data.table] Data to validate.
    #' @param measure [character] One of c('mx', 'yll', 'yld'). Measure to correct column names to.
    #' @param value_col [character] Column that contains mean results. Used to validate age.
    #' @param drop_additional_columns [logical] Drop additional columns beyond 'required_cols' vector?
    validate = function(data, measure, value_col, drop_additional_columns = TRUE) {
      # Correct columns if possible
      results <- list(data = data, errors = c())
      results <- self$correct(results, measure, drop_additional_columns, self$purpose)

      # For columns that require a subset of values, are values in schema's allowed range?
      if (self$verbose) print(sprintf("Validating superset columns %s", paste0(self$superset_cols, collapse = ",")))
      
      for (col in self$superset_cols) {
        values <- unique(results$data[, get(col)])
        allowed.values <- self$required_cols[[col]][["values"]]
        # NULL indicates that any values of a given type are acceptable (e.g., any float).
        if (!is.null(allowed.values) & any(!values %in% allowed.values)) {
          not_allowed_values <- setdiff(values, allowed.values)
          results$errors <- c(results$errors, (sprintf(
            "Values for %s are not in list of accepted values! Was passed %s; allowed is %s.",
            col, paste0(not_allowed_values, collapse = ", "),
            paste0(allowed.values, collapse = ", ")
          )))
        }
      }

      # For columns that require an exact match, do they match schema?
      if (self$verbose) print(sprintf("Validating exact_match columns %s", paste0(self$exact_match_cols, collapse = ",")))
      for (col in self$exact_match_cols) {
        values <- unique(results$data[, get(col)])
        if (!setequal(values, self$required_cols[[col]][["values"]])) {
          allowed <- paste0(self$required_cols[[col]][["values"]], collapse = ", ")
          passed <- paste0(values, collapse = ", ")
          results$errors <- c(results$errors, (sprintf("Values for %s do not exactly match allowed values. Allowed values are %s, was passed %s.", col, allowed, passed)))
        }
      }
      
      # Age needs a special validation. 
      # You can have any ages in the data, but ages not present in the settings must have all-zero values. 
      # We can do this by checking the largest, non-NA result by age
      # If 'allowed_ages' is not null, we know that age was passed as a required schema column
      if (!is.null(self$allowed_ages)){
        if (!value_col %in% names(results$data)){
          stop(sprintf("value_col %s is not in data. Available columns are: %s", 
                       value_col, paste0(names(results$data), collapse = ",")))
        }
        max_ages = results$data[, .(value = max(get(value_col), na.rm = TRUE)), by = 'age'] 
        unsanctioned_ages = results$data$age[!results$data$age %in% self$allowed_ages]
        for (age_subset in unique(unsanctioned_ages)){
          if (max_ages[age==age_subset, value]>0){
            results$errors <- c(results$errors, (sprintf("Non-zero values found for unmodeled ages: %s", age_subset)))
          }
        }
      } 
      
      # Do all columns have the specified types?
      for (col in names(self$required_cols)) {
        object_class <- lapply(results$data, class)[[col]]
        if (object_class == "integer") object_class <- "numeric"
        if (object_class != self$required_cols[[col]][["type"]]) {
          results$errors <- c(results$errors, (sprintf("%s is the incorrect type. Type is %s, and should be %s.",
                                                       col,
                                                       lapply(data, class)[[col]],
                                                       paste0(self$required_cols[[col]][["type"]], collapse = ", "))))
        }
      }

      data <- results$data
      errors <- results$errors

      if (length(errors) > 0) {
        for (error in errors) {
          message(errors)
        }
        stop("Some schema validation checks failed; review output.")
      }

      message("All schema validation checks passed.")
      return(data)
    }
  )
)


#' @title default_required_cols
#'
#' @description A helper method for the Schema class that sets default required column values.
#'
#' @param settings [list] A list of settings
#'
#' @return List of default columns.
default_required_cols <- function(settings) {
  # Translate from settings to column names
  geo_col <- settings$area_var
  mcnty <- list(type = "numeric", validation = "superset", values = NULL)
  state <- list(type = "numeric", validation = "superset", values = NULL)
  natl <- list(type = "numeric", validation = "superset", values = NULL)

  year <- list(type = "numeric", validation = "superset", values = settings$years)
  sex <- list(type = "numeric", validation = "superset", values = 1:3)
  race <- list(type = "numeric", validation = "superset", values = c(settings$races, race_default))
  edu <- list(type = "numeric", validation = "superset", values = c(settings$edu_groups, edu_default))
  age <- list(type = "numeric", validation = "age", values = settings$ages)
  
  required_cols <- list(
    year = year,
    sex = sex,
    race = race,
    edu = edu,
    age = age
  )

  if (geo_col == "mcnty") {
    required_cols$mcnty <- mcnty
  } else if (geo_col == "state") {
    required_cols$state <- state
  } else if (geo_col == "natl") {
    required_cols$natl <- natl
  } else {
    stop(sprintf("Don't know how to create schema with settings$area_var %s!", geo_col))
  }

  return(required_cols)
}

#' @title Schema$lower_draws_schema
#'
#' @name Schema$lower_draws_schema
#'
#' @description Returns a schema object for lower draws.
Schema$lower_draws_schema <- function(settings, verbose = TRUE) {
  required_cols <- default_required_cols(settings)

  value <- list(type = "numeric", validation = "superset", values = NULL)
  acause <- list(type = "character", validation = "superset", values = NULL)
  sim <- list(type = "numeric", validation = "superset", values = 1:settings$n.sims)
  pop <- list(type = "numeric", validation = "superset", values = NULL)
  wt <- list(type = "numeric", validation = "superset", values = NULL)

  additional_cols <- list(
    value = value,
    acause = acause,
    sim = sim,
    pop = pop,
    wt = wt
  )
  required_cols <- append(required_cols, additional_cols)

  return(Schema$new(required_cols, purpose = "load", settings = settings, verbose = verbose))
}

#' @title Schema$upper_draws_schema
#'
#' @name Schema$upper_draws_schema
#'
#' @description Returns a schema object for lower draws.
Schema$upper_draws_schema <- function(settings, verbose = TRUE) {
  required_cols <- default_required_cols(settings)

  value <- list(type = "numeric", validation = "superset", values = NULL)
  acause <- list(type = "character", validation = "superset", values = NULL)
  sim <- list(type = "numeric", validation = "superset", values = 1:settings$n.sims)

  additional_cols <- list(
    value = value,
    acause = acause,
    sim = sim
  )
  required_cols <- append(required_cols, additional_cols)

  return(Schema$new(required_cols, purpose = "load", settings = settings, verbose = verbose))
}

#' @title Schema$lt_draws_schema
#'
#' @name Schema$lt_draws_schema
#'
#' @description Returns a schema object for lifetables.
Schema$lt_draws_schema <- function(settings, verbose = TRUE) {
  required_cols <- default_required_cols(settings)
  
  mx <- list(type = "numeric", validation = "superset", values = NULL)
  ax <- list(type = "numeric", validation = "superset", values = NULL)
  qx <- list(type = "numeric", validation = "superset", values = NULL)
  ex <- list(type = "numeric", validation = "superset", values = NULL)
  sim <- list(type = "numeric", validation = "superset", values = 1:settings$n.sims)
  
  additional_cols <- list(
    mx = mx,
    ax = ax,
    qx = qx,
    ex = ex,
    sim = sim
  )
  required_cols <- append(required_cols, additional_cols)
  
  return(Schema$new(required_cols, purpose = "load", settings = settings, verbose = verbose))
}

#' @title Schema$estimates_schema
#'
#' @name Schema$estimates_schema
#'
#' @description Returns a schema object for estimates
Schema$estimates_schema <- function(settings, verbose = TRUE) {
  required_cols <- default_required_cols(settings)

  mean <- list(type = "numeric", validation = "superset", values = NULL)
  lb <- list(type = "numeric", validation = "superset", values = NULL)
  ub <- list(type = "numeric", validation = "superset", values = NULL)
  se <- list(type = "numeric", validation = "superset", values = NULL)

  additional_cols <- list(
    mean = mean,
    lb = lb,
    ub = ub,
    se = se
  )
  required_cols <- append(required_cols, additional_cols)

  return(Schema$new(required_cols, purpose = "load", settings = settings, verbose = verbose))
}

Schema$output_draws_schema <- function(settings, measure, verbose = TRUE) {
  level <- list(type = "character", validation = "superset", values = geo_choices)
  area <- list(type = "numeric", validation = "superset", values = NULL)

  year <- list(type = "numeric", validation = "superset", values = settings$years)
  sex <- list(type = "numeric", validation = "superset", values = 1:3)
  race <- list(type = "numeric", validation = "superset", values = c(settings$races, race_default))
  edu <- list(type = "numeric", validation = "superset", values = c(settings$edu_groups, edu_default))
  sim <- list(type = "numeric", validation = "superset", values = 1:settings$n.sims)
  age <- list(type = "numeric", validation = "age", values = unique(c(settings$ages, 98, 99))) # Include aggregate age categories
  acause <- list(type = "character", validation = "superset", values = NULL)

  required_cols <- list(
    level = level,
    area = area,
    year = year,
    sex = sex,
    race = race,
    edu = edu,
    sim = sim,
    age = age,
    acause = acause
  )

  if (measure == "mx") {
    mx <- list(type = "numeric", validation = "superset", values = NULL)
    required_cols <- append(required_cols, list(mx = mx))
  } else if (measure == "yll") {
    yll <- list(type = "numeric", validation = "superset", values = NULL)
    required_cols <- append(required_cols, list(yll = yll))
  } else if (measure == "yld") {
    yld <- list(type = "numeric", validation = "superset", values = NULL)
    required_cols <- append(required_cols, list(yld = yld))
  } else if (measure == "pred") {
    pred <- list(type = "numeric", validation = "superset", values = NULL)
    required_cols <- append(required_cols, list(pred = pred))
  } else if (measure == "ab") {
    ab <- list(type = "numeric", validation = "superset", values = NULL)
    measure_id <- list(type = "numeric", validation = "superset", values = NULL)
    metric_id <- list(type = "numeric", validation = "superset", values = NULL)
    rei_id <- list(type = "numeric", validation = "superset", values = NULL)
    required_cols <- append(required_cols, list(ab = ab, measure_id = measure_id, metric_id = metric_id, rei_id = rei_id))
  } else if (measure %in% c("lt", "FULL_lt")) {
    numeric_values <- list(type = "numeric", validation = "superset", values = NULL)
    # NOTE: life tables don't have a "lt" - instead they use "mx"
    required_cols <- append(
      required_cols,
      list(
        mx = numeric_values,
        ax = numeric_values,
        qx = numeric_values,
        ex = numeric_values
      )
    )

    if(measure == "FULL_lt") {
      required_cols <- append(
        required_cols,
        list(
          lx = numeric_values,
          dx = numeric_values,
          Lx = numeric_values,
          Tx = numeric_values
        )
      )
    }

  } else {
    stop(sprintf("Don't know how to check schema for measure %s", measure))
  }

  return(Schema$new(required_cols, purpose = "save", settings = settings, verbose = verbose))
}

Schema$output_estimates_schema <- function(settings, measure, verbose = TRUE) {
  level <- list(type = "character", validation = "superset", values = geo_choices)
  area <- list(type = "numeric", validation = "superset", values = NULL)

  year <- list(type = "numeric", validation = "superset", values = settings$years)
  sex <- list(type = "numeric", validation = "superset", values = 1:3)
  race <- list(type = "numeric", validation = "superset", values = c(settings$races, race_default))
  edu <- list(type = "numeric", validation = "superset", values = c(settings$edu_groups, edu_default))
  age <- list(type = "numeric", validation = "age", values = unique(c(settings$ages, 98, 99))) # Include aggregate age categories
  acause <- list(type = "character", validation = "superset", values = NULL)

  required_cols <- list(
    level = level,
    area = area,
    year = year,
    sex = sex,
    race = race,
    edu = edu,
    age = age,
    acause = acause
  )

  if (measure == "mx") {
    mx_mean <- list(type = "numeric", validation = "superset", values = NULL)
    mx_lb <- list(type = "numeric", validation = "superset", values = NULL)
    mx_ub <- list(type = "numeric", validation = "superset", values = NULL)
    mx_se <- list(type = "numeric", validation = "superset", values = NULL)

    required_cols <- append(required_cols, list(mx_mean = mx_mean, mx_lb = mx_lb, mx_ub = mx_ub, mx_se = mx_se))
  } else if (measure == "yll") {
    yll_mean <- list(type = "numeric", validation = "superset", values = NULL)
    yll_lb <- list(type = "numeric", validation = "superset", values = NULL)
    yll_ub <- list(type = "numeric", validation = "superset", values = NULL)
    yll_se <- list(type = "numeric", validation = "superset", values = NULL)

    required_cols <- append(required_cols, list(yll_mean = yll_mean, yll_lb = yll_lb, yll_ub = yll_ub, yll_se = yll_se))
  } else if (measure == "yld") {
    yld_mean <- list(type = "numeric", validation = "superset", values = NULL)
    yld_lb <- list(type = "numeric", validation = "superset", values = NULL)
    yld_ub <- list(type = "numeric", validation = "superset", values = NULL)
    yld_se <- list(type = "numeric", validation = "superset", values = NULL)

    required_cols <- append(required_cols, list(yld_mean = yld_mean, yld_lb = yld_lb, yld_ub = yld_ub, yld_se = yld_se))
  } else if (measure == "pred") {
    pred_mean <- list(type = "numeric", validation = "superset", values = NULL)
    pred_lb <- list(type = "numeric", validation = "superset", values = NULL)
    pred_ub <- list(type = "numeric", validation = "superset", values = NULL)
    pred_se <- list(type = "numeric", validation = "superset", values = NULL)

    required_cols <- append(
      required_cols, 
      list(pred_mean = pred_mean, pred_lb = pred_lb, pred_ub = pred_ub, pred_se = pred_se)
      )
  } else if (measure == "ab") {
    measure_id <- list(type = "numeric", validation = "superset", values = NULL)
    metric_id <- list(type = "numeric", validation = "superset", values = NULL)
    rei_id <- list(type = "numeric", validation = "superset", values = NULL)
    ab_mean <- list(type = "numeric", validation = "superset", values = NULL)
    ab_lb <- list(type = "numeric", validation = "superset", values = NULL)
    ab_ub <- list(type = "numeric", validation = "superset", values = NULL)
    ab_se <- list(type = "numeric", validation = "superset", values = NULL)
   
    required_cols <- append(
      required_cols, 
      list(measure_id = measure_id, metric_id = metric_id, rei_id = rei_id,
           ab_mean = ab_mean, ab_lb = ab_lb, ab_ub = ab_ub, ab_se = ab_se)
      )
  } else if (measure == "lt") {
    numeric_values <- list(type = "numeric", validation = "superset", values = NULL)

    # NOTE: life tables don't have a "lt" - instead they use "mx"
    required_cols <- append(
      required_cols,
      list(
        mx_mean = numeric_values,
        mx_lb = numeric_values,
        mx_ub = numeric_values,
        mx_se = numeric_values,
        ax_mean = numeric_values,
        ax_lb = numeric_values,
        ax_ub = numeric_values,
        ax_se = numeric_values,
        ex_mean = numeric_values,
        ex_lb = numeric_values,
        ex_ub = numeric_values,
        ex_se = numeric_values,
        qx_mean = numeric_values,
        qx_lb = numeric_values,
        qx_ub = numeric_values,
        qx_se = numeric_values
      )
    )
  } else {
    stop(sprintf("Don't know how to check schema for measure %s", measure))
  }

  return(Schema$new(required_cols, purpose = "save", settings = settings, verbose = verbose))
}
