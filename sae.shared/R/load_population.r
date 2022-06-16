#' @title Load population
#'
#' @description Load population from source. use_database flag determines whether to load from
#' the ushd database or file system.
#'
#' @param pop_file Complete filepath to population file as set in settings file. If
#' `use_database` is true, the file name of `pop_file` is taken as the short_covariate_name to
#' determine which population data from the ushd database.
#' @param use_database Boolean, Pull data from the ushd database? The default for this set by the
#' R option `ushd.use_database_for_population`. This is set in .Rprofile, which
#' is called automatically when R is launched in that working directory.
#'
#' @return data.table of population data
#'
#' @rdname load_population
#' @export
load_population <- function(pop_file, use_database = getOption("ushd.use_database_for_population")) {
  if (is.null(use_database)) {
    message(".Rprofile was not loaded in or the default option ushd.use_database_for_population was not defined within for load_population. Defaulting to FALSE")
    use_database <- F
  }

  if (use_database) {
    library(lbd.loader, lib.loc = 'FILEPATH')
    lbd.loader::load_package('ushd.dbr')
    message("Loading population from database")
    pop_name <- tools::file_path_sans_ext(basename(pop_file))
    pop <- ushd.dbr::get_population_data(covariate_name = pop_name)
  } else {
    message("Loading population from population file on file system")
    pop <- data.table(readRDS(pop_file))
  }
  return(pop)
}
