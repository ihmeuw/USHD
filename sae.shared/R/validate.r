#' @title stop.if.not.expected.cols
#'
#' @description Validate columns in a data.table or stop with an informative error message
#'
#' @export
stop.if.not.expected.cols <- function(dt, expected, preamble = NULL, extra.ok = FALSE) {
  cols <- colnames(dt)
  if (setequal(expected, cols)) {
    return(NULL)
  }
  if (extra.ok && length(setdiff(expected, cols)) == 0) {
    return(NULL)
  }
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

#' @title Call stop() IFF rlang::is_interactive() and quit() otherwise
#'
#' @description quit with a useful message/code
#'
#' @details You can can manually adjust whether this session is considered interactive via
#' \code{options(rlang_interactive = TRUE)} or \code{withr::with_options(list(rlang_interactive = TRUE)), function}
#'
#' @param msg String message to emit before stop/quit call
#' @param status integer status code to emit IFF quit() is called
#' @param override_quit logical, If T will always stop() rather than quit()
#'
#' @export
stop_or_quit <- function(msg, status = 1, override_quit = F) {
  if (rlang::is_interactive() | override_quit) {
    stop(msg)
  } else {
    message(msg)
    quit(save = "no", status = status)
  }
}

#' @title validate_rake
#'
#' @description Check that raked output is within tolerance.
#'
#' @param data [data.table] dataset with draws.
#' @param agg_var [character] variable for being raked
#' @param constant_vars [list] constant variables for raking across either cause or geography
#' @param err_msg [character] Initial string for error output
#' @param weight_pops [logical] whether to use global pop_weight. defaults to TRUE
#' @param tol [numeric] tolerance threshold defaults to 1e-10
#'
#' @export
validate_rake <- function(data, agg_var, constant_vars, err_msg, weight_pops = T, tol = 1e-10) {
  if (weight_pops) {
    data[, check_value := weighted.mean(value, pop_weight), by = c(constant_vars, agg_var)]
  } else {
    data[, check_value := sum(value), by = c(constant_vars, agg_var)]
  }
  
  data[, diff := check_value - get(paste0(agg_var, "_value"))]
  if (any(is.na(data$diff)) || max(abs(data$diff)) > tol) {
    message("age is ", paste(as.character(unique((data[is.na(value), ])$age), collapse = ", ")), " year is ", paste(as.character(unique((data[is.na(value), ])$year), collapse = ", ")))
    message("states are ", paste(as.character(unique((data[is.na(value), ])$state), collapse = ", ")))
    message("sims are ", paste(as.character(unique((data[is.na(value), ])$sim)), collapse = ", "))
    message("NA row count is", nrow(data[is.na(value), ]))
    stop(paste0(err_msg, "\nMax difference: ", max(abs(data$diff)), "\nTolerance: ", tol))
  }
  
  data[, c("check_value", "diff") := NULL] # clean up temporary variables
}