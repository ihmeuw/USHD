
#' @title translate_ages
#'
#' @description Given a vector of ages, return the corresponding age_ids or age_names depending on type param
#'
#' @param ages [integer] vector of ages
#' @param type [character] Either "age_group_name" or "age_group_id"
#'
#' @return [vector] vector of age_ids or age_names depending on type param
#' @export
translate_ages <- function(ages, type) {
  if (length(ages) <= 1) {
    stop("This function won't work with list of ages of length 1 (or less)")
  }

  ages <- sort(ages)

  warning(paste0(
    "WARNING! The largest age in the list ages will become the terminal age group ",
    max(ages), " plus.",
    " You can check this with type='age_group_name'"
  ))

  age_name <- c(paste(ages[-length(ages)], ages[-1] - 1, sep = " to "), paste0(ages[length(ages)], " plus"))
  age_name <- gsub("0 to 0", "<1 year", age_name)

  if (type == "age_group_name") {
    return(age_name)
  }

  if (type == "age_group_id") {
    gbd_ages <- get_ids("age_group")
    setkey(gbd_ages, age_group_name)
    age_id <- gbd_ages[J(age_name), age_group_id]
    return(age_id)
  }
}
