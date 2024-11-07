####################################################################################################
## Description: Define functions to add GBD location_ids or age_ids to a data table using US
##              counties ids. For locations, the data table must include "level" and "area". For
##              age groups, the data table must include "age".
##
## Inputs:      data [data.table] - data table with appropriate variables ("level" and "area" for
##                locations; "age" for age groups) to which the relevant GBD ID column should be
##                added.
##              natl_area - value in the "area" column to match natl values in data.
##              area_var_column - if provided, temporarily sets the "level" to this value and
##                renames the column to "area" for the function. If your data doesn't already have
##                a "level" and an "area" use this parameter to easily add location ids.
##
## Outputs:     data.table with one additional column for the GBD location or age group id.
####################################################################################################

add_gbd_age_ids <- function(data) {
  
  # get age names
  us_ages <- sort(unique(data$age))
  age_name <- c(paste(us_ages[-length(us_ages)], us_ages[-1] - 1, sep = " to "), paste0(us_ages[length(us_ages)], " plus"))
  age_name <- gsub("0 to 0", "<1 year", age_name)
  age_name <- gsub("85 to 97", "85 plus", age_name)
  
  age_name[us_ages == 98] <- "All Ages"
  age_name[us_ages == 99] <- "Age-standardized"
  
  # get corresponding age_group_ids
  age_ids <- get_ids("age_group")
  setkey(age_ids, age_group_name)
  age_ids <- age_ids[J(age_name), ]
  age_ids[, us_age := us_ages]
  setkey(age_ids, us_age)
  
  # map to and return age_group_ids
  data$age_group_id <- age_ids[J(data$age), age_group_id]
  return(data)
  
}