#' @title Load population
#'
#' @description Load population from source. use_database flag determines whether to load from
#' the ushd database or file system.
#'
#' @param pop_file Complete filepath to population file as set in settings file. If
#' `use_database` is true, the file name of `pop_file` is taken as the short_covariate_name to
#' determine which population data from the ushd database.
#' @param use_database Boolean, Pull data from the ushd database? The default for this set by the
#' R option `ushd.use_database_for_population`. This is set in mortality/sae_models/.Rprofile, which
#' is called automatically when R is launched in that working directory.
#'
#' @return data.table of population data
#'
#' @rdname load_population
#' @export
load_population <- function(pop_path = NULL) {

  message("Loading population from population file on file system")
  pop <- data.table(readRDS(pop_path))
  
  return(pop)
}

