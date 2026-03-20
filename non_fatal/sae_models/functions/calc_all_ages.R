####################################################################################################
## Description: Define function for adding crude and age-standardized rates to age-specific preds.
##
## Inputs:      draws - a data.table of age-specific mx or yll draws plus the corresponding
##                population counts to be used for generating crude rates.
##              std_wt - a data.table of the weights for generating age-standardized rates.
##	            var - variable to collapse (ie, "mx" or "yll").
##
## Outputs:     data.table of age-specific plus crude and age-standardized mx or yll rates.
####################################################################################################


calc_all_ages <- function(draws, std_wt, var, by_vars = c("level", "area", "year", "sex", "race", "sim")) {

  setnames(draws, var, "value")

  # calculate weights for generating crude rates based on observed populations
  draws[pop < 1e-5, pop := 0] # change effective 0s to 0s to avoid some annoying edge cases where one age gets all the weight even if it also has effectively no population
  draws[, crude_wt := pop / sum(pop), by = by_vars]

  # for areas with no population, calculate weights based on total populations across all areas
  draws[, total := sum(pop), by = c(by_vars[!(by_vars == "area")], "age")]
  draws[is.na(crude_wt), crude_wt := total / sum(total), by = by_vars]

  # collapse to get crude draws
  crude <- draws[, list(value = sum(value * crude_wt)), by = by_vars]
  crude[, age := 98L]
  draws[, c("pop", "total", "crude_wt") := NULL]

  # merge on standard weights
  draws <- merge(draws, std_wt, by = "age")

  # collapse to get age-standardized rates
  std <- draws[, list(value = weighted.mean(value, wt)), by = by_vars]
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
