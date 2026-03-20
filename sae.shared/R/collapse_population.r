#' @title collapse_population_for_agg_race
#'
#' @param population [data.table] population that needs collapsing and weighting.
#' @param settings [R6Class] ModelSettings object.
#'
#' @return
#' @export
collapse_population_for_agg_race <- function(population, settings = NULL) {
  pop <- copy(population)
  setnames(pop, settings$area_var, "area")
  # collapse populations to the required dimensions to remove any unneeded stratifiers
  pop <- pop[, list(pop = sum(pop)), keyby = "area,year,sex,race,age"]
  
  # calculate aggregation weights for each area-year-sex-age
  pop[, wt := pop / sum(pop), by = "area,year,sex,age"]
  
  # this fails when the total population in a given area-year-sex-age is 0.
  # In these cases, we use the following hierarchy of backups: area-year-age (no sex),
  # area-year (no sex or age).
  pop[, pop2 := sum(pop), by = "area,year,age,race"]
  pop[, wt2 := pop2 / sum(pop2), by = "area,year,sex,age"]
  
  pop[, pop3 := sum(pop), by = "area,year,race"]
  pop[, wt3 := pop3 / sum(pop3), by = "area,year,sex,age"]
  
  pop[is.nan(wt), wt := wt2]
  pop[is.nan(wt), wt := wt3]
  
  return(pop)
}