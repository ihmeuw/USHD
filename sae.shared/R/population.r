#' @title add.population.weights
#'
#' @description Add population weights to data.table
#'
#' Adds population weights to data.table data as weight_var. Uses temporary variables .tmp.pop and .tmp.weight.
#'
#' @param data: data.table with columns: pop, year, sex, age, race, <area_var>.
#' @param area_var: column with area information e.g., "mcnty". This is probably the \code{area_var} loaded from settings.
#' @param weight_var: variable to assign population weights to.
#' @param area_var: Geographic level of the data. Options are c('mcnty', 'state', 'natl').
#'
#' @export
add.population.weights <- function(data, population, demographics, weight_var, area_var) {
  stop.if.not.expected.cols(data, expected = c("pop", "year", "sex", "age", area_var), preamble = "'data' arg for add.population.weights", extra.ok = TRUE)
  data <- copy(data)
  population <- copy(population)

  # reset tiny populations to 0 to avoid edge cases where one group gets all
  # the weight even though it has effectively 0 population
  tiny.pop <- data$pop < 1e-5 & data$pop != 0
  if (any(tiny.pop)) {
    warning(sprintf("Setting %i tiny populations to 0", sum(tiny.pop)))
    data[tiny.pop, pop := 0]
  }

  # calculate aggregation weights. include fallbacks where a given area_var/year/sex/age is 0.
  data[, get("weight_var") := pop / sum(pop), by = demographics]

  # this fails when the total population in a given area-year-sex-age is 0. In these cases, we use the
  # following hierarchy of backups: area-year-age-race (no sex), area-year-race (no sex or age).
  # 0 / 0 is NaN, positive number divided by 0 is Inf.
  nans <- is.nan(data[[weight_var]])
  if (any(nans)) {
    warning(sprintf("%i rows with NaN population weights (%s)", sum(nans), paste(demographics, collapse = "/")))
    fallback_demographics <- demographics[demographics != "sex"] # remove sex
    pop_demographics <- c(fallback_demographics[fallback_demographics %in% names(population)], if(by_race) "race", if(by_edu) "edu") 
    population[, .tmp.pop := sum(pop), by = pop_demographics] # numerator
    population[, .tmp.weight := .tmp.pop / sum(.tmp.pop), by = c(demographics[demographics %in% names(population)])]

    data <- merge(data, unique(population[, c(pop_demographics, ".tmp.weight"), with = F]), by = pop_demographics)
    nans <- is.nan(data[[weight_var]]) # Need to reassign mask as indices are changed on merge
    filled_nans <- nans & !is.nan(data[[".tmp.weight"]])

    warning(sprintf("Setting %i rows to fallback weight (%s)", sum(filled_nans), paste(pop_demographics, collapse = "/")))
    data[filled_nans, get("weight_var") := .tmp.weight]
    data[, .tmp.weight := NULL] # clean up temporary variable

    nans <- is.nan(data[[weight_var]])
    if (any(nans)) {
      fallback_demographics <- demographics[!demographics %in% c("sex", "age")] # remove sex and age
      pop_demographics <- c(fallback_demographics[fallback_demographics %in% names(population)], if(by_race) "race", if(by_edu) "edu")
      population[, .tmp.pop := sum(pop), by = pop_demographics]
      population[, .tmp.weight := .tmp.pop / sum(.tmp.pop), by = c(demographics[demographics %in% names(population)])]

      data <- merge(data, unique(population[, c(pop_demographics, ".tmp.weight"), with = F]), by = pop_demographics)
      nans <- is.nan(data[[weight_var]]) # Need to reassign mask as indices are changed on merge
      filled_nans <- nans & !is.nan(data[[".tmp.weight"]])

      warning(sprintf("Setting %i rows to fallback weight (%s)", sum(filled_nans), paste(pop_demographics, collapse = "/")))
      data[filled_nans, get("weight_var") := .tmp.weight]
      data[, .tmp.weight := NULL] # clean up temporary variable
    }

    nans <- is.nan(data[[weight_var]])
    if (any(nans)) {
      stop(sprintf("%i rows remain with NaN weight", sum(nans)))
    }
  }

  return(data)
}

#' @title validate_ushd_pop_to_gbd
#'
#' @description Validate that the USHD population is consistant with GBD population
#' for the relevant GBD round and decomp step at the national level by-age
#'
#' @param population USHD population dataframe with columns like age, year, sex, natl, pop
#' @param draw_settings list with at least two values: "gbd_round_id" and "decomp_step"
#' @param pop_file file path of USHD population. Only used for error message but still recommended
#'           to provide. Defaults to "not given"
#' @param tol tolerance for the absolute difference between USHD and GBD populations. As these are
#'      whole numbers, defaults to 0.1
#'
#' @export
validate_ushd_pop_to_gbd <- function(population, draw_settings, pop_file = "not given", tol = 0.1) {
  # Get GBD population for USA, relevant age groups, by sex, and the given year
  ages <- unique(population$age)
  age_group_ids <- translate_ages(ages, "age_group_id")

  if (85 %in% ages) {
    age_groups_85_plus <- c(31, 32, 235)
    # warn about implicit assumption of GBD age groups, which may change after GBD 2020
    if (draw_settings$gbd_round_id > 7) {
      warning(
        "Assuming 85+ corresponds to GBD age groups 31, 32, and 235, which may not be ",
        "true for GBD rounds past GBD 2020 (round 7). Please confirm this."
      )
    }
    age_group_ids <- age_group_ids[age_group_ids != 160]
    age_group_ids <- c(age_group_ids, age_groups_85_plus)
  }

  gbd_population <- get_population(
    age_group_id = age_group_ids,
    gbd_round_id = draw_settings$gbd_round_id,
    decomp_step = draw_settings$decomp_step,
    year_id = unique(population$year),
    location_id = 102, # USA
    sex_id = 1:2
  )

  # Check that we have something for GBD population
  if (nrow(gbd_population) == 0) {
    stop(paste0(
      "No GBD population found for GBD round ", draw_settings$gbd_round_id, ", ", draw_settings$decomp_step,
      " in the US for age group ids ", paste(sort(age_group_ids), collapse = ", ")
    ))
  }

  # Convert from GBD age group id to USHD age
  gbd_population[, age := ages[match(age_group_id, age_group_ids)]]

  # Sum GBD age groups that make up USHD age group 85
  # and tack back onto GBD population
  if (85 %in% ages) {
    gbd_population_85_plus <- gbd_population[age_group_id %in% age_groups_85_plus,
                                             list(population = sum(population)),
                                             by = c("location_id", "year_id", "sex_id", "run_id")
    ]
    gbd_population_85_plus[, `:=`(age = 85, age_group_id = NA)]

    # Combine back with other GBD population rows
    gbd_population <- rbind(gbd_population[!age_group_id %in% age_groups_85_plus], gbd_population_85_plus)
  }

  # Collapse USHD population to national, all-age (by sex, by year)
  population <- population[, list(ushd_population = sum(pop)), by = c("natl", "year", "sex", "age")]

  # Merge together and confirm sums equal each other
  both <- merge(population, gbd_population, by.x = c("year", "sex", "age"), by.y = c("year_id", "sex_id", "age"), all.x = TRUE)
  both[, difference := abs(ushd_population - population)]

  if (any(both$difference > tol)) {
    stop(paste0(
      "Max difference between USHD population and GBD population greater than tolerance: ",
      max(both$difference), " > ", tol, ".\nUSHD population file: ", pop_file, "\nGBD: round ",
      draw_settings$gbd_round_id, ", ", draw_settings$decomp_step
    ))
  }
}
