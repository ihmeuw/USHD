####################################################################################################
## Description: Define functions for collapsing mx, yll and lt draws:
##
##              "collapse_draws": Generic function to calculate point estimates, confidence
##                intervals, and standard errors
##
##              "collapse_draws_lt": Specific collapse function for lifetables
##
## Inputs:      draws - a data.table of draws of some variable.
##              var - variable to collapse (ie, "mx" or "yll").
##              id_vars - id variables to collapse over, these also become the data.table keys.
##
## Outputs:     data.table with point estimates, confidence intervals, and standard errors of the
##              given variable for all specified id variables.
####################################################################################################


collapse_draws <- function(draws, var, id_vars = c("level", "area", "year", "sex", "race", "age")) {
  calc_metric <- c("mean", "lb", "median", "ub", "se")
  col_names <- paste(var, calc_metric, sep = "_")

  setnames(draws, var, "value")
  message("Note that draws with values of NA are being removed; be sure to check for NA draws!")
  est <- draws[, as.list(c(mean(value),
                           quantile(value, c(0.025, 0.5, 0.975), type = 5),
                           sd(value))),
               by = id_vars]

  setnames(draws, "value", var)
  setnames(est, c(id_vars, col_names))
  setcolorder(est, c(id_vars, col_names))
  setkeyv(est, id_vars)
  est
}
