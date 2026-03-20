#' Return demographics information
#'
#' @param model_settings sae.shared::ModelSettings instance with settings details.
#'
#' @return named list with location_id, sex_id, year_id, age_group_id, race, and edu.
ushd.get_demographics <- function(extra_dim, extra_dim_values) {
  # not clear if location_id needs to be valid in database OR just align with data
  location_ids <- 0:3109 # these are the "level" values for all "mcnty" area locations
  # these location_id values were gotten by
  # 1) querying the merged_counties table for all locations with a NULL
  # location_id_02 (signalling there is a 1:1 with location_id_01)
  # 2) sorting the numbers and compressing into R syntax
  location_ids <- c(
    574:641, 643:646, 648:650, 653:656, 666:666, 668:673, 675:677, 679:816,
    818:827, 829:830, 832:834, 836:836, 839:878, 880:956, 958:1117, 1119:1120,
    1122:1775, 1777:1779, 1781:2216, 2218:2379, 2381:2384, 2386:2975,
    2977:2985, 2987:3396, 3398:3399, 3401:3402, 3404:3405, 3407:3407,
    3410:3413, 3415:3415, 3417:3417, 3419:3419, 3421:3421, 3423:3424,
    3426:3428, 3432:3437, 3439:3448, 3450:3453, 3455:3465, 3467:3469,
    3471:3471, 3473:3481, 3483:3487, 3489:3499, 3501:3502, 3504:3509,
    3511:3518, 3520:3521, 3523:3523, 3526:3714)

  new.merged_county_ids <- 94061:94092

  location_ids <- c(location_ids, new.merged_county_ids)

  # years we expect to model as of May 2025
  year_ids <- 2000:2023

  # ages used
  age_group_ids <- suppressWarnings(sae.shared::translate_ages(
      c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85),
      type = "age_group_id"
  ))

  sexes <- 1:2

  res <- list(
    location_id = location_ids,
    sex_id = sexes,
    year_id = year_ids,
    age_group_id = age_group_ids
  )

  res[[extra_dim]] <- extra_dim_values

  # assign non-extra dimension with the default
  # but I suspect he is easy on one vs both extra dimensions
  if (identical(extra_dim, "race")) {
    res$edu <- sae.shared:::edu_default
  } else {
    res$race <- sae.shared:::race_default
  }
  return(res)
}
