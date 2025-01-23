# calculate growth rates
# Growth rates are necessary for the Horiuchi–Coale method we use in lifetables;
# knowning how the population is changing leads to better life expecancy
# estimates

# do not want all-race pop, we want by-race pop, so that we get by-race growth
# rates. This function will then take the weighted mean, weighted by
# race/ethnicity, to compute all-race averages. In this way, we don't get
# identical growth rates repeated across all counties. Each county will have a
# unique value, according to the racial/ethnic composition of the county.

calc_growth_rate <- function(dir, test_by_race, test_by_edu, n_years = 10, years, pop_growth_file, area_var) {

  pop_growth <- readRDS(pop_growth_file)
  pop_growth <- pop_growth[year <= max(years)]
  if (!("race" %in% names(pop_growth))) pop_growth[, race := all_pop_id]
  if (!("edu" %in% names(pop_growth))) pop_growth[, edu := all_pop_id]

  setnames(pop_growth, area_var, "area")
  # basic collapse to get rid of any extra dimensions, and to filter to ages
  pop_growth <-
    pop_growth[age %in% ages, list(pop = sum(pop)), by = "area,year,sex,race,edu,age"]
  stopifnot(nrow(pop_growth) > 0)

  # collapse to national level population, across area
  pop_growth <-
    pop_growth[age == max(ages)][, list(pop = sum(pop)), by = 'year,sex,race,edu,age']
  # calculate both-sex estimates
  pop_growth <-
    rbind(pop_growth, pop_growth[, list(sex = 3, pop = sum(pop)), by = 'year,race,edu,age'])

  diff_data <- data.table()

  # loop over all the years in pop file, from latest to earliest (reverse order)
  for (y in rev(sort(unique(pop_growth$year)))) {

    message(y)

    tmp_data <- copy(pop_growth) # initialize data

    if (y == min(unique(pop_growth$year))) {
      message("Reached minimum year in pop_growth! Done with looping, breaking early.")
      break()
    }

    # filter to current year and year before
    tmp_data <- tmp_data[year %in% c(y, y - 1)]
    # select previous year, set new column equal to pop for that year
    tmp_data[year == y - 1, last_year := pop]
    # let max_year be the maximum value in max_value, across race, edu, sex, and age, ignoring NA
    # values
    tmp_data[, last_year := max(last_year, na.rm = T), by = c("race", "edu", "sex", "age")]
    # filter tmp data down to only the current year y
    tmp_data <- tmp_data[year == y]
    # let growth rate be equal to the difference between pop in the current year and max population
    # from the previous year, divided by the max pop in the previous year
    tmp_data[, growth_rate := (pop - last_year) / last_year]
    # remove max pop from the previous year
    tmp_data[, last_year := NULL]

    # append temporary data to our data.table
    diff_data <- rbind(diff_data, tmp_data)
  }

  # test to make sure it worked
  if (test_by_edu) {
    test_val <- diff_data[sex == sexes[1] & year == max(years) & edu == 101, growth_rate]
    x <- diff_data[sex == sexes[1] & year == max(years) & edu == 101, pop]
    y <- diff_data[sex == sexes[1] & year == (max(years) - 1)  & edu == 101, pop]
    compare_val <- (x - y) / y

    stopifnot(test_val == compare_val & !is.null(test_val) & !is.null(compare_val) & length(compare_val) != 0 )
  }

  if (test_by_race) {
    test_val <- diff_data[sex == sexes[1] & year == max(years) & race == 5, growth_rate]
    compare_val <- (diff_data[sex == sexes[1] & year == max(years) & race == 5, pop] -
                      diff_data[sex == sexes[1] & year == (max(years)-1)  & race == 5, pop])/diff_data[sex == sexes[1] &
                                                                                                         year == (max(years)-1)  & race == 5, pop]
    stopifnot(test_val == compare_val & !is.null(test_val) & !is.null(compare_val) & length(compare_val) != 0)
  }


  # Now, we need to calculate the average 10 year change First, subset to just the years that have
  # enough data from which we can estimate the 10 year average Get rid of growth rates between 1999
  # and 2000, as well as 2009 and 2010, given changes in the Census and thus discontinuities
  diff_data[year %in% c(2000, 2010), growth_rate := NA]

  available_years <- sort(unique(pop_growth$year))
  # Don't want 1998, 1999 used if by edu. This is an exception for education population,
  # because the those years are made by taking proportions of population in 2000 or 2017 and
  # applying them to 1998, 1999
  if (by_edu) {
    available_years <- available_years[available_years >= 2000]
  }
  message(glue::glue("available years of population: {paste(available_years, collapse=',')}"))

  # this loops over each year in years. For each year, it will take the 10 earliest available years.
  # For example, for the year 2000, we want to pool 1991-2000 and average those years. However, that
  # is not always possible. By selecting the earliest available 10 years, we still get 10 years, and
  # they're as early as possible. For race/ethnicity, this method will be *equivalent* to what was
  # done before. For example for r/e population, for year 2000, the years 1991-2000 will be used.
  # For education population, we only have the year 2000 onward, so for 2000, 2000-2010 will be
  # used.
  for (y in years) { 
    message(glue::glue("year: {y}"))

    # choose the earliest available year: if y- (n_years - 1) is in the available years, else, use the minimum of
    # the available years. parenthesis in the test is very important.
    earliest_year <- ifelse((y - (n_years - 1)) %in% available_years, y - (n_years - 1), min(available_years))
    message(glue::glue("earliest year: {earliest_year}"))

    # set the year range. start from the chosen earliest year, and add n_years - 1 to it. The
    # parenthesis around the addition is very important.
    year_range <- c(earliest_year:(earliest_year + (n_years - 1)))
    
    year_range <- year_range[year_range %in% available_years] # exclude any years that are not available

    # if we excluded years, then add back years from the other end of the range to get back to
    # n_years this is getting pretty complicated in order to be general. The code doesn't make
    # assumptions about which years were excluded.
    # if (length(year_range) < n_years) {
    #   if (max(excluded_years) > max(year_range)) {
    #     year_range <- c((min(year_range) - length(excluded_years) ): max(year_range))
    #   } else {
    #     stop(glue::glue("Unhandled case for year y = {y}"))
    #   }
    # }

    message(glue::glue("year range: {paste(year_range, collapse=', ')}"))

    # Assert that there are 10 years selected
    stopifnot(length(year_range) == n_years)

    # Select the chosen years, and average across them, groups chosen by race, edu, sex, and age,
    # ignoring NA values.
    diff_data[year %in% year_range, mean_gr_tmp := mean(growth_rate, na.rm = T),
              by = c("race", "edu", "sex", "age")]
    # Set year to the current year y
    diff_data[year == y, mean_gr := mean_gr_tmp]
    # Discard temporary growth rate column
    diff_data[, mean_gr_tmp := NULL]

  }
  message("Done with year loop.")

  # test to make sure it worked
  if (test_by_edu) {
    test_val <- diff_data[sex == sexes[1] & year == max(years) & edu == 101, mean_gr]
    stopifnot(length(max(years):(max(years) - (n_years - 1))) == n_years)
    compare_val <- mean(diff_data[sex == sexes[1] & year %in% c(max(years):(max(years) - (n_years - 1))) & edu == 101, growth_rate], na.rm = T)

    stopifnot(test_val == compare_val & !is.null(test_val) & !is.null(compare_val) & length(compare_val) != 0)
  }

  if (test_by_race) {
    test_val <- diff_data[sex == sexes[1] & year == max(years) & race == 5, mean_gr]
    stopifnot(length(max(years):(max(years) - (n_years - 1))) == n_years)
    compare_val <- mean(diff_data[sex == sexes[1] & year %in% c(max(years):(max(years) - (n_years - 1))) & race == 5, growth_rate], na.rm = T)

    message(paste0("Difference in test values: ",round(abs(test_val - compare_val), 10)),"\n")
    stopifnot(abs(test_val - compare_val) < 0.000001 & !is.null(test_val) & !is.null(compare_val) & length(compare_val) != 0)
  }


  diff_data <- diff_data[, c("age", "pop", "growth_rate") := NULL]
  diff_data <- diff_data[year %in% years]
  if (!all(years %in% diff_data$year)) {
    stop(glue::glue("Some values in years are absent from diff_data: {setdiff(years, diff_data$year)}"))
  }

  # Now, merge this onto the mcnty-level population and calculate an all-race and all-edu value that
  # is weighted by the race-specific 85+ population Read population in again and use the same exact
  # decision tree as used in agg_races.r/agg_edu.r
  pop_full <- readRDS(pop_growth_file) # using pop growth file because in the case of validation, they won't match
  if (!("race" %in% names(pop_full))) pop_full[, race := all_pop_id]
  if (!("edu" %in% names(pop_full))) pop_full[, edu := all_pop_id]

  # first, calculate the mcnty-level population
  pop_full_mcnty <-
    pop_full[, list(pop = sum(pop), level = "mcnty"), keyby = "mcnty,year,sex,race,edu,age"]
  setnames(pop_full_mcnty, "mcnty", "area")
  # now the state level population
  pop_full_state <-
    pop_full[, list(pop = sum(pop), level = "state"), keyby = "state,year,sex,race,edu,age"]
  setnames(pop_full_state, "state", "area")
  # and national level
  pop_full_natl <-
    pop_full[, list(pop = sum(pop),
                    area = 1,
                    level = "natl"), keyby = "year,sex,race,edu,age"]
  pop_full <- rbind(pop_full_mcnty, pop_full_state, pop_full_natl)

  # now calculate both-sex values
  pop_full <- rbind(pop_full, pop_full[ ,list(pop = sum(pop), sex = 3), by = 'level,area,year,race,edu,age'])

  # reset tiny populations to 0 to avoid annoying edge cases where one group gets all of the weight
  # even though it has effectively no population
  pop_full[pop < 1e-5, pop := 0]

  # calculate aggregation weights for each area-level-year-sex-age
  pop_full[, wt := pop / sum(pop), by = "area,level,year,sex,age"]

  # this fails when the total population in a given level-area-year-sex-age is 0.
  # In these cases, we use the following hierarchy of backups: level-area-year-sex (no age),
  # level-area-year (no sex or age).
  pop_full[, pop2 := sum(pop), by = "area,level,year,sex,race,edu"]
  pop_full[, wt2 := pop2 / sum(pop2), by = "area,level,year,sex,age"]

  # do not include the both-sex values in the pooling over sex since this is duplicative
  pop_full[sex != 3, pop3 := sum(pop), by = "area,level,year,race,edu"]
  pop_full[sex != 3, wt3 := pop3 / sum(pop3), by = "area,level,year,sex,age"]

  pop_full[is.nan(wt), wt := wt2]
  pop_full[is.nan(wt), wt := wt3]

  stopifnot(nrow(pop_full[is.nan(wt)]) == 0)

  wts <-
    pop_full[age == 85, .(level, area, year, sex, race, edu, age, wt)]
  stopifnot(wts[, sum(wt), by = "level,area,year,sex,age"][, max(abs(V1 - 1))] < 1e-10)
  wts[, age := NULL]

  ## Merge the population weights with the growth rates. This causes growth rates to repeat across
  # counties, ages.
  diff_data <-
    merge(diff_data,
          wts,
          by = c("year", "sex", "race", "edu"),
          all.x = T)
  stopifnot(nrow(diff_data[is.na(wt)]) == 0)

  # weighted average across age, education, race
  diff_data[, weighted_gr := weighted.mean(mean_gr, wt), by = c("level", "area", "year", "sex")]
  # create all-race / all-edu growth rates and append them
  diff_data_all <- unique(diff_data[, .(year, sex, level, area, weighted_gr)])
  diff_data_all[, race := all_pop_id]
  diff_data_all[, edu := all_pop_id]
  setnames(diff_data_all, "weighted_gr", "mean_gr")
  diff_data <- diff_data[, c("wt", "weighted_gr") := NULL]
  diff_data <-
    rbind(diff_data, diff_data_all) # add on the all-race estimates

  # save to model directory
  if (!all(years %in% diff_data$year)) {
    stop(glue::glue("Some values in years are absent from diff_data: {setdiff(years, diff_data$year)}"))
  }

  saveRDS(diff_data, paste0(dir, "/growth_rates.rds"))

}


