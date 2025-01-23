#' @title calc_all_ages
#'
#' @description Define function for adding crude and age-standardized rates to age-specific mx or yll draws.
#'
#' @param draws [data.table] a data.table of age-specific mx or yll draws plus the corresponding
#'                population counts to be used for generating crude rates.
#' @param std_wt [data.table] a data.table of the weights for generating age-standardized rates.
#' @param var [character] variable to collapse (ie, "mx" or "yll").
#' @param allow_missing_ages [boolean]  controls if all ages should be expected.
#' @param measure [character] measure (currently used by "pred" and "ab") to process.
#' @param rei_id [integer] if rei_id == 370 use subset of ages
#'
#' @return data.table of age-specific plus crude and age-standardized mx or yll rates.
#'
#' @author INDIVIDUAL_NAME
#'
#' @export
calc_all_ages <- function(draws, std_wt, var, by_vars = c("level", "area", "year", "sex", "race", "edu", "sim"),
                          measure = NULL, rei_id = NULL, allow_missing_ages = F) {
  if (!allow_missing_ages &
      (!setequal(unique(draws$age), unique(std_wt$age)))) {


    # Right now this applies to
    # 1. Fatal education models
    # 2. 2-dimensional raking (already run by-age)
    # 3. YLDs (???)
    # 4. bmi (pred)
    message(glue::glue("Do not need to calculate aggregate age groups for this model because not all ages are present, and allow_missing_ages is {allow_missing_ages}. Returning early from calc_all_ages."))
    draws[,pop := NULL]
    return(draws)
  }
  draws <- copy(draws)

  # In case this function is being used to recalculate all-age mx, drop already existing all age mx
  # so that the following test will work
  draws <- draws[!(age %in% c(98, 99)), ]

  # check that every age group is present before proceeding
  # every age group has to be present in order for the age standardized mortality rate to be correct
  expected.ages <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)

  # bmi (measure = pred, ab, rei_id = 370) has only 20:85 ages
  if (!is.null(measure) && measure %in% c("pred", "ab") &&
      (!is.null(rei_id) && rei_id == 370)) {
    bmi.expected.ages <- c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)
    if (setequal(unique(std_wt$age), bmi.expected.ages)) {
      warning("Warning! calc_all_ages with only 20+ age groups. This is used ONLY by bmi & ab raking and aggregation.")
      expected.ages <- bmi.expected.ages
    }
  }

  if (!setequal(unique(draws$age), expected.ages)) {
    missing <- setdiff(expected.ages, unique(draws$age))
    extra <- setdiff(unique(draws$age), expected.ages)
    msg <- sprintf("calc_all_ages. Missing ages (%s) and have extra ages (%s).", paste(missing, collapse = ", "), paste(extra, collapse = ", "))
    if (allow_missing_ages) {
      warning(msg)
    } else {
      lsae.utils::stop_or_quit(msg)
    }
  }

  setnames(draws, var, "value")

  if ("pop" %in% names(draws)) {
    # calculate weights for generating crude rates based on observed populations
    if(!identical(measure, "ab")){
      draws[pop < 1e-5, pop := 0] 
    }
    draws[, crude_wt := pop / sum(pop), by = by_vars]

    # for areas with no population, calculate weights based on total populations across all areas
    
    draws[, total := sum(pop), by = c(by_vars[!(by_vars == "area")], "age")]
    draws[is.na(crude_wt), crude_wt := total / sum(total), by = by_vars]

    # If crude_wt is still NaN, you can change it to 0 if both pop and total is 0 (divide by 0 error in R).
    draws[pop == 0 & total == 0 & is.na(crude_wt), crude_wt := 0]
    if (any(is.na(draws$crude_wt))) lsae.utils::stop_or_quit("Weights not assigned correctly in calc_all_ages.", status = 20)
  } else {
    lsae.utils::stop_or_quit("Population column not present in draws!", status = 33)
    # draws[, crude_wt:=1]
  }

  # collapse to get crude draws
  crude <- draws[, list(value = sum(value * crude_wt)), by = by_vars]
  crude[, age := 98L]
  draws[, c("pop", "total", "crude_wt") := NULL]

  # In the case where not all ages are here, we need to re-calculate the age standard weights for
  # the ages we have present. This is really meant for education.
  if (allow_missing_ages) {
    warning("Removing unused ages from age standard weights...")
    # remove unused ages
    std_wt <- std_wt[age %in% unique(draws$age), ]
    # Re-scale weights so that they add to 1. Got this idea from INDIVIDUAL_NAME. Note that this
    # re-calculation of weights always happens upon reading in std_wt, the only difference here is
    # that we've filtered ages before re-calculating.
    std_wt <- std_wt[, list(age, wt = wt / sum(wt))]
  }

  # merge on standard weights
  draws <- merge(draws, std_wt, by = "age")
  if (any(is.na(draws$wt))) lsae.utils::stop_or_quit("NA values in age standard weight column 'wt'")

  # collapse to get age-standardized rates
  std <- draws[, list(value = sum(value * wt)), by = by_vars]
  std[, age := 99L]
  draws[, wt := NULL]

  # combine age-specific rates with crude and age-standardized rates
  extra_draw_cols <- setdiff(colnames(draws), colnames(crude))
  if (length(extra_draw_cols) > 0 && is.null(measure)) {
    warning(sprintf("Draws has extra columns [%s] which are being removed", paste(extra_draw_cols, collapse = ", ")))
    draws[, (extra_draw_cols) := NULL]
  }
  draws <- rbind(draws, crude, std, use.names = T, fill = T)
  setnames(draws, "value", var)

  # resort, reorder, and return
  setkeyv(draws, c(by_vars, "age"))
  setcolorder(draws, c(by_vars, "age", var))

  return(draws)
}
