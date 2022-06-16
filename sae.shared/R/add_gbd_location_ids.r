#' @title add_gbd_location_ids
#'
#' @description Define functions to add GBD location_ids or age_ids to a data table using US
#'              counties ids. For locations, the data table must include "level" and "area". For
#'              age groups, the data table must include "age".
#'
#' @param data [data.table] - data table with appropriate variables ("level" and "area" for
#'                            locations; "age" for age groups) to which the relevant GBD ID column should be
#'                            added
#' @param natl_area value in the "area" column to match natl values in data, Default: 0
#' @param area_var_column if provided, temporarily sets the "level" to this value and
#'                        renames the column to "area" for the function. If your data doesn't already have
#'                        a "level" and an "area" use this parameter to easily add location ids, Default: NULL
#' @return data.table with one additional column for the GBD location or age group id.
#'
#' @rdname add_gbd_location_ids
#' @export
add_gbd_location_ids <- function(data, natl_area = 0, area_var_column = NULL) {
  data <- data.table::copy(data)

  if (!is.null(area_var_column)) {
    data[, level := area_var_column] 
    data.table::setnames(data, area_var_column, "area")
  }

  # load locations
  loc <- fread("FILEPATH")
  loc <- rbind(
    loc[current == 1, list(level = "mcnty", area = mcnty, fips = cnty)],
    loc[current == 1, list(level = "state", area = state, fips = state)]
  )
  loc <- unique(loc)

  loc2 <- fread("FILEPATH")
  loc <- merge(loc, loc2[, list(fips, location_id)], by = "fips", all.x = T)
  loc <- loc[, list(level, area, location_id)]
  loc <- rbind(loc, data.table(level = "natl", area = natl_area, location_id = 102))

  # add location_id to data and return
  data <- merge(data, loc, by = c("level", "area"), all.x = T, allow.cartesian = T)

  # if necessary, revert changes from data set up
  if (!is.null(area_var_column)) {
    data[, level := NULL]
    data.table::setnames(data, "area", area_var_column)
  }

  return(data)
}
