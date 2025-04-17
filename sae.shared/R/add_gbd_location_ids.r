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
#' @param use_merged_location_id If TRUE, merged_location_id/s for counties are used when present. 
#'                               If FALSE (by default), location_id/s for counties are used.
#' @return data.table with one additional column for the GBD location or age group id.
#'
#' @rdname add_gbd_location_ids
#' @export
add_gbd_location_ids <- function(data, natl_area = 0, area_var_column = NULL, use_merged_location_id = FALSE) {
  data <- data.table::copy(data)
  # see if data is unique before adding location_id (used for check at end)
  start_unique <- unique(data)[, .N] == data[, .N]
  

  # if necessary, set up data. Original behavior maintained if area_var_column = NULL
  if (!is.null(area_var_column)) {
    data[, level := area_var_column] # nolint
    data.table::setnames(data, area_var_column, "area")
  }

  # load locations
  # note that these input files were updated in March 2022 to include merged_location_id 
  # column. This function was developed that update. 
  loc <- fread("FILEPATH")
  loc2 <- fread("FILEPATH")
  
  if (use_merged_location_id) {
    # replace location_id of counties in loc2 with merged_location_id in loc.
    mapper <- loc[location_id != merged_location_id, list(location_id, merged_location_id)]
    loc2$location_id <- plyr::mapvalues(loc2$location_id, mapper$location_id, mapper$merged_location_id)
    loc2 <- unique(loc2)
  }
  
  loc <- rbind(
    loc[current == 1, list(level = "mcnty", area = mcnty, fips = cnty)],
    loc[current == 1, list(level = "state", area = state, fips = state)]
  )
  loc <- unique(loc)

  loc <- merge(loc, loc2[, list(fips, location_id)], by = "fips", all.x = T)
  # make unique, otherwise there are multiple rows for "areas" composed of multiple counties
  #   when use_merged_location_id = TRUE
  loc <- unique(loc[, list(level, area, location_id)]) 
  loc <- rbind(loc, data.table(level = "natl", area = natl_area, location_id = 102))

  # add location_id to data and return
  data <- merge(data, loc, by = c("level", "area"), all.x = T, allow.cartesian = T)

  # if necessary, revert changes from data set up
  if (!is.null(area_var_column)) {
    data[, level := NULL] # nolint
    data.table::setnames(data, "area", area_var_column)
  }

  # check that the data are unique, otherwise return an error
  end_unique <- unique(data)[, .N] == data[, .N]

  if (!end_unique & start_unique) {
    lsae.utils::stop_or_quit("add_gbd_location_ids returns a non-unique table. If you get this error, there may be issues with how location_ids are assigned.")
  } else if (!end_unique & !start_unique) {
    warning("add_gbd_location_ids returns a non-unique table. Input was also not unique. Check for duplicates in input data.")
  }

  return(data)
}

#' @title get_mcnty_location_ids
#'
#' @param population [data.table] population data with "area", "level" columns for location_id.
#' @param state_location_id [integer] state location_id. If not passed, returns all location_id's for all mcnty's.
#'
#' @return numeric vector of merged_location_id/s for a given state_location_id
#' @rdname get_mcnty_location_ids
#' @export
get_mcnty_location_ids <- function(population, state_location_id = NULL) {
  N_start = population[, .N]
  # get state and mcnty location ids
  pop.with.loc.ids <- add_gbd_location_ids(population, natl_area = 1, area_var_column = "mcnty", 
                                           use_merged_location_id = TRUE)
  setnames(pop.with.loc.ids, "location_id", "mcnty_loc_id")
  pop.with.loc.ids <- add_gbd_location_ids(pop.with.loc.ids, natl_area = 1, area_var_column = "state",
                                           use_merged_location_id = TRUE)
  setnames(pop.with.loc.ids, "location_id", "state_loc_id")

  
  N_after = pop.with.loc.ids[, .N]  
  if(N_start != N_after){
    lsae.utils::stop_or_quit("get_mcnty_location_ids created duplicate rows. If you get this error, there may be issues with how location_ids are assigned.")
  }
  
  if (is.null(state_location_id)) {
    mcnty_loc_subset <- as.numeric(pop.with.loc.ids[, unique(mcnty_loc_id)])
  } else {
    mcnty_loc_subset <- as.numeric(pop.with.loc.ids[state_loc_id == state_location_id, unique(mcnty_loc_id)])
  }
  
  if (length(mcnty_loc_subset) == 0) {
    lsae.utils::stop_or_quit(paste0("No mcnty location id was found for state location id: ", state_location_id))
  }
  return(mcnty_loc_subset)
}
