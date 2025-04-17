#' Validate columns in a data.table or stop with an informative error message
stop.if.not.expected.cols <- function(dt, expected, preamble = NULL, extra.ok = FALSE) {
  cols <- colnames(dt)
  if (setequal(expected, cols)) return(NULL)
  if (extra.ok && length(setdiff(expected, cols)) == 0) return(NULL)
  missing <- setdiff(expected, cols)
  extra <- setdiff(cols, expected)
  msg <- sprintf(
    "PROBLEM COLUMNS\n%sMISSING: %s\nEXTRA: %s",
    ifelse(is.null(preamble), "", paste0(preamble, "\n")),
    paste(missing, collapse = ", "),
    paste(extra, collapse = ", ")
  )
  stop(msg)
}

#' Call stop() IFF interactive() and quit() otherwise
#'
#' Calling quit() in e.g., RStudio is painful because it ends the session. This is undesirable.
#' However calling stop() while R is running non-interactively removes the ability to quit with
#' a potentially useful status code. This utility function addresses both use cases.
#'
#' @param msg String message to emit before stop/quit call
#' @param status integer status code to emit IFF quit() is called
stop_or_quit <- function(msg, status = 1) {
    if (interactive()) {
      stop(msg)
    } else {
      message(msg)
      quit(save = "no", status = status)
    }
}

validate_rake <- function(data, agg_var, constant_vars, err_msg, weight_pops = T, tol = 1e-10) {
  if (weight_pops) {
    data[, check_value := weighted.mean(value, pop_weight), by = constant_vars]
  } else {
    data[, check_value := sum(value), by = constant_vars]
  }
  
  data[, diff := check_value - get(paste0(agg_var, "_value"))]
  if (any(is.na(data$diff)) || max(abs(data$diff)) > tol) {
    stop(paste0(err_msg, "\nMax difference: ", max(abs(data$diff)), "\nTolerance: ", tol))
  }
  
  data[, c("check_value", "diff") := NULL] # clean up temporary variables
}
