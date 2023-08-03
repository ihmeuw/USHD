#' @title Load population
#'
#' @description Load population from source. 
#'
#' @param pop_file Complete filepath to population file as set in settings file 
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

