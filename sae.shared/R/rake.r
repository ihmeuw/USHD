
#' @title rake
#'
#' @description  Generic function that can be used to rake by either geography or cause (or anything
#'               else). Assumes data are in rate space, and calculates metrics by population-
#'               weighting values.
#'
#' @param      data [character] -- data.table including data you to rake. Must include columns
#'                 named "value" (mortality/yll rate to be raked) and "pop_weight" (population weights to
#'                 use for geographically aggregating) and the columns corresponding to "agg_var"
#'                 and "constant_vars" below.
#' @param      agg_var [character] -- the variable on which you want to aggregate. "data" must
#'                 include a column named "{agg_var}_value".
#' @param      constant_vars [character] -- variables to keep in the dataset and by which to
#'                 stratify the calculations.
#' @param      replace_value [logical] -- if T, replaces the "value column already in the data with
#'                 raked values; if F, generates a new column named "raked_value" and stores values
#'                 there.
#' @param      weight_pops [logical] -- if T, uses "pop_weight" to create weighted averages when
#'                 aggregating; if F, uses simple sums. The first is appropriate for raking by
#'                 geography, and the later for raking by cause.
#'
#' @export
rake <- function(data, agg_var, constant_vars, replace_value = F, weight_pops = T) {
  print(paste0("Agg var is: ", agg_var))
  print("Constant vars are: ")
  print(constant_vars)

  # 1. generate population-weighted mortality rates
  data[, weighted_value := if (weight_pops) value * pop_weight else value]

  # 2. sum over weighted mortality rates to get a crude total mx
  data[, sum_weighted_value := sum(weighted_value), by = c(constant_vars, agg_var)]

  # 3. generate raking weights by dividing the upper-level (e.g., state) mx over the crude total mx
  data[, raking_weight := get(paste0(agg_var, "_value")) / sum_weighted_value]

  # if all county and state level counts are zero (e.g. when you rake by geography for neonatal, age >1),
  # raking_weight will be NA. Convert to zeros.
  data[is.na(raking_weight), raking_weight := 0]

  # If all county counts are zero, but the state counts are not, the raking_weight will be Inf. 
  # This is an issue because we can't set raking weight to zero (otherwise, 
  # USHD results will not match GBD results). However, without 
  # non-zero county-level values, we need a strategy to determine reasonable 
  # values that sum up to the state-level values.
  # This issue may need to be addressed outside of raking, but as a temporary solution, 
  # we distribute the state values to the counties assuming uniformity throughout 
  # areas being raked to a higher level.
  # If weight_pops = TRUE (and agg_var == "state"), we assume that we're raking
  # rates. If weight_pops = FALSE (and agg_var == "cause"), we assume that we're
  # raking counts. 
  if (any(is.infinite(data$raking_weight))) {
    # get measure from the global enviornment. If not "ab", return an error.
    # This corner case should not happen except in the context of ab, where 
    # zero's are possible.
    tmp_measure <- get("measure", envir = .GlobalEnv)
    if (tmp_measure != "ab") {
      stop("Found infinite raking weights. We implemented a solution for AB. 
            Check if the solution works for other types of measured before 
            using this fix.")
    }
  # Raise an error if agg_var is not state -- may not work for other types of raking
    if(agg_var != "state") {
      stop(paste("Infinite raking weights detected. This is because the lower-raking values 
            are zero, but upper values are not zero. We have a fix when raking
            to state. Have not tested for agg_var", agg_var))
    }
    warning(paste("Some raking weights are infinite. This is likely due to all county counts being zero, but the state counts being non-zero. 
            This is problematic, and needs to be investigated. To avoid convergence issues here, we will assume that 
            the value is uniform within ", paste0(c(constant_vars, agg_var), collapse = ",")))
    if(weight_pops == TRUE){
      # If we're raking rates (assumed b/c pop_weight = TRUE), the lower value 
      # (rate) is set to the upper value (rate)
      data[is.infinite(raking_weight), value := get(paste0(agg_var, "_value"))]
    } else {
      # If we're raking counts (assumed b/c pop_weight is FALSE), the lower value
      # (count) is set to a fraction of the upper value (count) proportional to 
      # population, which assumes that the rate is uniform.
      
      data[is.infinite(raking_weight), value := get(paste0(agg_var, "_value")) * pop_weight / sum(pop_weight), by = c(constant_vars, agg_var)]
    }
    # Set raking weight to 1 here because we've determined the value according to
    # the rule above. No further adjustment is needed.
    data[is.infinite(raking_weight), raking_weight := 1]
  }

  # 4. generate raked mx estimates by multiplying the original mx by the raking weights
  data[, raked_value := value * raking_weight]

  if (nrow(data[is.na(raked_value)]) > 0) {
    lsae.utils::stop_or_quit("Rake function produced NAs.", status = 20)
  }

  if (replace_value) {
    data[, value := raked_value]
    data[, raked_value := NULL]
  }

  data[, c("weighted_value", "sum_weighted_value", "raking_weight") := NULL]

  return(data)
}

#' @title Rake draws in two dimensions: geography and cause
#'
#' @description Draws are raked alternating from raking across geography (by cause)
#' and across cause (by geography) until the max change in mortality rate (mx)
#' or yll rate is below the tolerance threshold (tol). In the event that the rake
#' fails to converge within max_iterations, a diagnostic file is written with
#' the data points that failed to rake below the tolerance threshold.
#'
#' @param data dataset with draws.
#' @param geography_var variable for raking across geography. Ex: "natl", "mcnty"
#' @param cause_var variable for raking across cause. Ex: "parent"
#' @param geography_constant_vars constant variables for raking across geography
#' @param cause_constant_vars constant variable for raking across cause
#' @param max_iterations max number of raking iterations to run if change in mortality
#'        rate doesn't go below tolerance threshold
#' @param tol tolerance threshold
#' @param dir directory to write diagnostic files to
#' @param measure the measure being raked e.g., "mx" or "yll"
#'
#' @return data [data.table] Raked data.
#'
#' @export
rake_two_dimensions <- function(data, geography_var, cause_var, geography_constant_vars, cause_constant_vars,
                                max_iterations = 500, tol = 1e-10, dir, measure) {
  # Initiate
  data <- data.table::copy(data)
  data[, begin_value := value] # save original (beginning) mortality rates as begin_mx; this is used in the loop to display incremental changes
  data[, value_change := 1] # initialize mx_change so we can keep track

  # End conditions:
  # 1) The max change between raking iterations is less than tol.
  # 2) We've hit max iterations and should fail
  iter <- 1

  while (max(data$value_change, na.rm = T) > tol) {
    # Rake each dimension
    # First, rake across geography (by cause)
    cat(paste0("Raking across geography (", geography_var, ")\n"))
    data <- rake(data, agg_var = geography_var, constant_vars = geography_constant_vars, weight_pops = TRUE)

    # Check the max difference between the starting mx and raked mx to determine stability
    data[, value_change := abs(raked_value - begin_value)]
    cat(paste("...max difference:", max(data$value_change, na.rm = T), "\n"))
    data[, c("value", "begin_value") := raked_value]
    data[, raked_value := NULL]

    # Second, across cause (by geography)
    # NOTE: pop_weight is in terms of geography AND since we can sum rates across child causes -> parent cause
    # we use weight_pops = FALSE so the mortality rate is not population-weighted
    cat(paste0("Raking across cause (", cause_var, ")\n"))
    data <- rake(data, agg_var = cause_var, constant_vars = cause_constant_vars, weight_pops = FALSE)

    # Check the max difference between the starting mx and raked mx to determine stability
    data[, value_change := abs(raked_value - begin_value)]
    cat(paste("...max difference:", max(data$value_change, na.rm = T), "\n"))
    data[, c("value", "begin_value") := raked_value]
    data[, raked_value := NULL]

    fail_df <- data[value_change > tol, ]
    fail_cnt <- nrow(fail_df)
    # update the iteration number; stop after 500 (we can reasonably assume that it's not going to work at that point)
    iter <- iter + 1
    if (iter > max_iterations) {
      fn <- paste0(dir, "/reports/rake_errors/rake_", fail_df[1]$acause, "_", fail_df[1]$year, "_", fail_df[1]$sex, "_", fail_df[1]$age, "_mcnty", fail_df[1]$mcnty, "_sim", fail_df[1]$sim, "_", measure, ".rds")
      message(sprintf("Raking has iterated %d times without converging past tolerance of %g . \n Outputing %d sims to %s", iter, tol, fail_cnt, fn))
      saveRDS(fail_df,
        file = fn,
        compress = TRUE
      )
      break
    }

    cat(paste("\nRaking, iteration", iter, "fail_cnt", fail_cnt, "\n"))
  }

  # Drop unneeded columns
  data[, (c("begin_value", "value_change")) := NULL]

  # Validate rake in each dimension
  cat("\nValidating rake across geography\n")
  validate_rake(
    data,
    agg_var = geography_var,
    constant_vars = geography_constant_vars,
    err_msg = paste("Discrepancy in raking across geography"),
    weight_pops = TRUE,
    tol = tol
  )

  cat("Validating rake across cause\n")
  validate_rake(
    data,
    agg_var = cause_var,
    constant_vars = cause_constant_vars,
    err_msg = paste("Discrepancy in raking across cause\n"),
    weight_pops = FALSE,
    tol = tol
  )

  return(data)
}

#' args_cause_match
#'
#' @description  check incoming args against cause exceptions.  If there is not a parameter match in the cause lists provided
#' then match is FALSE.  Note that if cause list key is not present for parameter, match will be TRUE.
#'
#' @param args [named list] arguments to flexible_2d_raking.r
#' @param cause [named list] cause list from settings.yaml
#'
#' @return match Boolean
args_cause_match <- function(args, cause) {
  match = TRUE
  for (key in intersect(names(args), names(cause))) {
    if (!args[[key]] %in% cause[[key]]) {
      match = FALSE
    }
  }
  return(match)
}

#' get_max_iterations
#'
#' @description  get number of iterations to use for given set of arguments, and possible exceptions and returns it.
#'
#' @param dir [character] Model directory
#' @param args [named list] arguments to flexible_2d_raking.r
#' @param iterations [integer] number of iterations to default to
#'
#' @return number of iterations to allow in raking
#'
#' @export
get_max_iterations <- function(dir, args, iterations = 500) {
  exceptions <- get_rake_2D_exceptions(dir)
  cause <- exceptions[[args$parent_acause]]
  if (is.null(cause)) {
    return(iterations)
  } else {
    if (args_cause_match(args, cause)) {
      return(cause$iterations)
    } else {
      return(iterations)
    }
  }
}

#' get_rake_2D_exceptions
#'
#' @description  Checks for "rake_2D_exceptions" in ("[dir]/settings.yaml") and returns it.
#'
#' #' @param dir [character] Model directory
#' @return named list of rake_2d_exceptions for each cause
#'
#' Example settings.yaml:
#' rake_2D_exceptions:
#' _inj:
#'   year:
#'   - 2017
#'   - 2018
#'   age:
#'   - 45
#'   iterations: 1500
#' _sub:
#'   year:
#'   - 2011
#'   iterations: 1501
#'
#' @export
get_rake_2D_exceptions <- function(dir) {
  f <- file.path(dir, "settings.yaml")
  if (file.exists(f)) {
    consolidated_settings <- read_yaml(f, fileEncoding = "UTF-8")
    if ("rake_2D_exceptions" %in% names(consolidated_settings)) {
      return(consolidated_settings$rake_2D_exceptions)
    }
  }
}
