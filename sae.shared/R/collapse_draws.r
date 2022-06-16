#' @title collapse_draws
#'
#' @description Define functions for collapsing mx, yll and lt draws:
#'
#'              "collapse_draws": Generic function to calculate point estimates, confidence
#'                                intervals, and standard errors
#'
#'
#' @param draws - a data.table of draws of some variable.
#' @param var - variable to collapse (ie, "mx" or "yll").
#' @param id_vars - id variables to collapse over, these also become the data.table keys.
#'
#' @return data.table with point estimates, confidence intervals, and standard errors of the
#'         given variable for all specified id variables.
#'
#'
#' @rdname collapse_draws
#' @export
collapse_draws <- function(draws, var, id_vars = c("level", "area", "year", "sex", "race", "edu", "age")) {
  calc_metric <- c("mean", "lb", "ub", "se")
  col_names <- paste(var, calc_metric, sep = "_")

  setnames(draws, var, "value")
  est <- draws[, as.list(c(
    mean(value),
    quantile(value, c(0.025, 0.975), type = 5),
    sd(value)
  )),
  by = id_vars
  ]

  setnames(draws, "value", var)
  setnames(est, c(id_vars, col_names))

  setcolorder(est, c(id_vars, col_names))
  setkeyv(est, id_vars)
  est
}
