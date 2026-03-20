
#' @title overwrite_constants
#' 
#' @param new_race_default New race_default that overwrites race_default in constant.r
#'
#' @return new values for constants defined in constant.r
#' @export
overwrite_constants <- function(new_race_default) {
  # overwrite pacakge-scoped variable (race_default=1) with 9 because pred uses older race codes.
  env <- asNamespace("sae.shared")
  unlockBinding("race_default", env)
  env$race_default <- new_race_default
}