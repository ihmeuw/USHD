#' @title calc_all_ages
#'
#' @description Define function for adding crude and age-standardized rates to age-specific mx or yll draws.
#'
#' @param       draws [data.table] a data.table of age-specific mx or yll draws plus the corresponding
#'                population counts to be used for generating crude rates.
#' @param       std_wt [data.table] a data.table of the weights for generating age-standardized rates.
#' @param       var [character] variable to collapse (ie, "mx" or "yll").
#'
#' @return      data.table of age-specific plus crude and age-standardized mx or yll rates.
#'
#' @export
calc_all_ages <- function(draws, std_wt, var, by_vars = c("level", "area", "year", "sex", "race", "edu", "sim")) {
  
  # In case this function is being used to recalculate all-age mx, drop already existing all age mx
  # so that the following test will work
  draws <- draws[!(age %in% c(98, 99)), ]
  
  # check that every age group is present before proceeding
  # every age group has to be present in order for the age standardized mortality rate to be correct
  expected.ages <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)
  if (!setequal(unique(draws$age), expected.ages)) {
    missing <- setdiff(expected.ages, unique(draws$age))
    extra <- setdiff(unique(draws$age), expected.ages)
    msg <- sprintf("calc_all_ages. Missing ages %s and have extra ages %s", paste(missing, collapse = ", "), paste(extra, collapse = ", "))
    stop_or_quit(msg)
  }
  
  setnames(draws, var, "value")
  
  if ("pop" %in% names(draws)) {
    # calculate weights for generating crude rates based on observed populations
    draws[pop < 1e-5, pop := 0] # change effective 0s to 0s to avoid some edge cases where one age gets all the weight even if it also has effectively no population
    draws[, crude_wt := pop / sum(pop), by = by_vars]
    
    # for areas with no population, calculate weights based on total populations across all areas
    draws[, total := sum(pop), by = c(by_vars[!(by_vars == "area")], "age")]
    draws[is.na(crude_wt), crude_wt := total / sum(total), by = by_vars]
    
    # If crude_wt is still NaN, you can change it to 0 if both pop and total is 0 
    draws[pop == 0 & total == 0 & is.na(crude_wt), crude_wt := 0]
    if (any(is.na(draws$crude_wt))) stop_or_quit("Weights not assigned correctly in calc_all_ages.", status = 20)
  } else {
    stop_or_quit("Population column not present in draws!", status = 33)
  }
  
  # collapse to get crude draws
  crude <- draws[, list(value = sum(value * crude_wt)), by = by_vars]
  crude[, age := 98L]
  draws[, c("pop", "total", "crude_wt") := NULL]
  
  # merge on standard weights
  draws <- merge(draws, std_wt, by = "age")
  
  # collapse to get age-standardized rates
  std <- draws[, list(value = sum(value * wt)), by = by_vars]
  std[, age := 99L]
  draws[, wt := NULL]
  
  # combine age-specific rates with crude and age-standardized rates
  extra_draw_cols <- setdiff(colnames(draws), colnames(crude))
  if (length(extra_draw_cols) > 0) {
    warning(sprintf("Draws has extra columns [%s] which are being removed", paste(extra_draw_cols, collapse = ", ")))
    draws[, (extra_draw_cols) := NULL]
  }
  draws <- rbind(draws, crude, std, use.names = T)
  setnames(draws, "value", var)
  
  # resort, reorder, and return
  setkeyv(draws, c(by_vars, "age"))
  setcolorder(draws, c(by_vars, "age", var))
  
  return(draws)
}