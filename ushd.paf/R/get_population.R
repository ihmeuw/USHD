ushd.get_population <- function(pop_file, year_id, age_group_id, sex_id, extra_dim) {
  pop <- readRDS(pop_file)

  pop <- ushd.add_age_group_id(pop)

  res <- pop[year %in% year_id & sex %in% sex_id & age_group_id %in% get("age_group_id"), ]

  setnames(res, "year", "year_id")
  setnames(res, "sex", "sex_id")
  setnames(res, "pop", "population")

  warning("ushd.get_population: WARNING! not translating location_id values or filtering them")
  setnames(res, "mcnty", "location_id")

  # with = FALSE signals to interpret the variables instead of as bare words
  # in other words, it makes extra_dim work as desired
  res[, c("age_group_id", "location_id", "year_id", "sex_id", "population", extra_dim), with = FALSE]
}

