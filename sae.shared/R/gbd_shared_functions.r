#' @title load_gbd_shared_functions
#'
#' @description Load gbd shared functions in use.
#'                get_cause_metadata.R
#'                get_ids.R
#'                get_outputs.R
#'                get_draws.R
#'                get_population.R
#'
#' @rdname load_gbd_shared_functions
#' @export
load_gbd_shared_functions <- function() {
  source("FILEPATH/get_cause_metadata.R")
  source("FILEPATH/get_ids.R")
  source("FILEPATH/get_outputs.R")
  source("FILEPATH/get_draws.R")
  source("FILEPATH/get_population.R")
}
