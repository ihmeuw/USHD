#######################################################
# Calculate growth rates function
calc_growth_rates <- function(pop_growth, outdir, lbl, n_years, test_by_america = T){
  
  # Select populations at max age
  pop_growth <- pop_growth[age == 85]
  # Calculate both-sex estimates
  pop_growth <- rbind(pop_growth, pop_growth[, list(sex = 3, pop = sum(pop)), by = 'america,year,age'])
  
  # Loop over all the years in pop file, from latest to earliest (reverse order)
  diff_data <- data.table()
  
  for (y in rev(sort(unique(pop_growth$year)))) {
    
    message(y)
    
    tmp_data <- copy(pop_growth) # initialize data
    
    if (y == min(unique(pop_growth$year))) break("Done with looping")
    
    # filter to current year and year before
    tmp_data <- tmp_data[year %in% c(y, y - 1)]
    # select previous year, set new column equal to pop for that year
    tmp_data[year == y - 1, last_year := pop]
    tmp_data[, last_year := max(last_year, na.rm = T), by = c("america", "sex", "age")]
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
  if (test_by_america) {
    test_val <- diff_data[sex == 1 & year == max(years) & america == 5, growth_rate]
    compare_val <- (diff_data[sex == 1 & year == max(years) & america == 5, pop] -
                      diff_data[sex == 1 & year == (max(years)-1)  & america == 5, pop])/diff_data[sex == 1 & year == (max(years)-1)  & america == 5, pop]
    stopifnot(test_val == compare_val & !is.null(test_val) & !is.null(compare_val) & length(compare_val) != 0)
  }
  
  
  # Now, we need to calculate the average 10 year change First, subset to just the years that have
  # enough data from which we can estimate the 10 year average Get rid of growth rates between 1999
  # and 2000, as well as 2009 and 2010, given changes in the Census and thus discontinuities
  diff_data[year %in% c(2000, 2010), growth_rate := NA]
  
  available_years <- sort(unique(pop_growth$year))
  message(glue::glue("available years of population: {paste(available_years, collapse=',')}"))
  
  
  # this loops over each year in years. For each year, it will take the 10 earliest available years.
  # For example, for the year 2000, we want to pool 1991-2000 and average those years. 
  
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
    
    message(glue::glue("year range: {paste(year_range, collapse=',')}"))
    
    # Demonstrate equality to previous version of this code. This will only work on the r/e pop
    # file.
    message(glue::glue("Is this the same as y-{n_years - 1}? {y - (n_years - 1) == earliest_year}"))
    
    # Assert that there are 10 years selected
    stopifnot(length(year_range) == n_years)
    
    # Select the chosen years, and average across them, groups chosen by race, edu, sex, and age,
    # ignoring NA values.
    diff_data[year %in% year_range, mean_gr_tmp := mean(growth_rate, na.rm = T),
              by = c("america", "sex", "age")]
    # Set year to the current year y
    diff_data[year == y, mean_gr := mean_gr_tmp]
    # Discard temporary growth rate column
    diff_data[, mean_gr_tmp := NULL]
    
    
  }
  message("Done with year loop.")
  
  # test to make sure it worked
  if (test_by_america) {
    test_val <- diff_data[sex == 1 & year == max(years) & america == 5, mean_gr]
    stopifnot(length(max(years):(max(years) - (n_years - 1))) == n_years)
    compare_val <- mean(diff_data[sex == 1 & year %in% c(max(years):(max(years) - (n_years - 1))) & america == 5, growth_rate], na.rm = T)
    
    stopifnot(test_val == compare_val & !is.null(test_val) & !is.null(compare_val) & length(compare_val) != 0)
  }
  
  diff_data <- diff_data[, c("age", "pop", "growth_rate") := NULL]
  diff_data <- diff_data[year %in% years]
  
  
  # Now merge this onto the population weight and calculate an all-america value that is weighted by the america-specific 85+ population
  # reset tiny populations to 0 to avoid annoying edge cases where one group gets all of the weight
  # even though it has effectively no population
  pop_growth[pop < 1e-5, pop := 0]
  
  # calculate aggregation weights for each year-sex-age
  pop_growth[, wt := pop / sum(pop), by = "year,sex,age"]
  
  stopifnot(nrow(pop_growth[is.nan(wt)]) == 0)
  stopifnot(pop_growth[, sum(wt), by = "year,sex,age"][, max(abs(V1 - 1))] < 1e-10)
  
  wts <- pop_growth[age == 85, .(america, year, sex, wt)]
  
  
  ## Merge the population weights with the growth rates. 
  diff_data <- merge(diff_data, wts, by = c("america", "year", "sex"), all.x = T)
  stopifnot(nrow(diff_data[is.na(wt)]) == 0)
  
  # weighted average across america
  diff_data[, weighted_gr := weighted.mean(mean_gr, wt), by = c("year", "sex")]
  # create all-america growth rates and append them
  diff_data_all <- unique(diff_data[, .(year, sex, weighted_gr)])
  diff_data_all[, america := 99L]
  setnames(diff_data_all, "weighted_gr", "mean_gr")
  diff_data <- rbind(diff_data[,.(america,year,sex,mean_gr)], diff_data_all) # add on the all-race estimates
  
  # save to model directory
  saveRDS(diff_data, paste0(outdir, "/growth_rates_", lbl, ".rds"))
  
}


#########################################
# Calculate mortality and lifetable function
calc_sum_lifetable_draws <- function(draws, gr, outdir, lbl){
  
  # Calculate lifetable -------------------------------
  # Merge with growth rates
  data <- merge(draws, gr, by = c("america", "year", "sex"), all.x = T)
  stopifnot(nrow(data[is.na(mx)|is.na(mean_gr)]) == 0)
  
  # Calculate lifetables (including both sexes)
  lt <- data[, lifetable(mx = mx, sex = sex[1], use_graduation = T, extrap = F, lt_hc = T, gr = mean_gr), by = 'america,year,sex,draw']
  
  # Save
  if (resub | !file.exists(paste0(outdir, "/lt_all_", n_draws, "_draws_", lbl, ".rds"))){
    saveRDS(lt, paste0(outdir, "/lt_all_", n_draws, "_draws_", lbl, ".rds"))
  }
  
  
  # Summarize -----------------------
  lt_est <- lt[,list(ex_mean = mean(ex),
                     ex_lower = quantile(ex, 0.025, type = 5),
                     ex_upper = quantile(ex, 0.975, type = 5)),
               keyby = 'america,year,sex,age']
  
  # Formatting ----------------------
  lt_est[, sex_lbl := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both Sexes"))]
  lt_est[, america_lbl := factor(america, levels = 1:10, labels = america_labels)]
  
  if (resub | !file.exists(paste0(outdir, "/lt_all_", lbl, ".rds"))){
    saveRDS(lt_est, paste0(outdir, "/lt_all_est_", lbl, ".rds"))
  }
  
}


  