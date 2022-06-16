### calculate growth rates for hte purposes of the life expectancy methods

calc_growth_rate <- function(dir) {
  pop_growth <- readRDS(pop_growth_file)
  setnames(pop_growth, area_var, "area")
  pop_growth <- pop_growth[age %in% ages, list(pop = sum(pop)), by = "area,year,sex,race,age"]
  stopifnot(nrow(pop_growth) > 0)
  
  pop_growth <- pop_growth[age == max(ages)][,list(pop = sum(pop)), by='year,sex,race,age']
  # calculate both-sex estimates
  pop_growth <- rbind(pop_growth, pop_growth[,list(sex = 3, pop = sum(pop)), by='year,race,age'])
  
  diff_data <- data.table()
  for(y in rev(sort(unique(pop_growth$year)))) {
    
    message(y)
    
    tmp_data <- copy(pop_growth)
    if(y == min(unique(pop_growth$year))) break("Done with looping")
    
    tmp_data <- tmp_data[year %in% c(y, y-1)]
    tmp_data[year == y-1, last_year := pop]
    tmp_data[,last_year := max(last_year, na.rm=T), by=c("race","sex","age")]
    tmp_data <- tmp_data[year == y]
    tmp_data[,growth_rate := (pop-last_year)/last_year]
    tmp_data[,last_year := NULL]
    
    diff_data <- rbind(diff_data, tmp_data)
  }
  
  # test to make sure it worked
  test_val <- diff_data[sex == sexes[1] & year == max(years) & race == 5, growth_rate]
  compare_val <- (diff_data[sex == sexes[1] & year == max(years) & race == 5, pop] - 
                    diff_data[sex == sexes[1] & year == (max(years)-1)  & race == 5, pop])/diff_data[sex == sexes[1] & 
                                                      year == (max(years)-1)  & race == 5, pop]
  stopifnot(test_val == compare_val & !is.null(test_val) & !is.null(compare_val) & length(compare_val) != 0)
  
  # Now, we need to calculate the average 10 year change
  # First, subset to just the years that have enough data from which we can estimate the 10 year average
  # Get rid of growth rates between 1999 and 2000, as well as 2009 and 2010, given changes in the Census and thus discontinuities
  diff_data[year %in% c(2000, 2010), growth_rate := NA]
  
  for(y in years) {
    
    message(y)
    
    diff_data[year %in% c(y:(y-9)), mean_gr_tmp := mean(growth_rate, na.rm=T), by=c("race","sex","age")]
    diff_data[year == y, mean_gr := mean_gr_tmp]
    diff_data[,mean_gr_tmp := NULL]
    
  }
  
  # test to make sure it worked
  test_val <- diff_data[sex == sexes[1] & year == max(years) & race == 5, mean_gr]
  stopifnot(length(max(years):(max(years)-9)) == 10)
  compare_val <- mean(diff_data[sex == sexes[1] & year %in% c(max(years):(max(years)-9)) & race == 5, growth_rate],na.rm=T)
  
  stopifnot(test_val == compare_val & !is.null(test_val) & !is.null(compare_val) & length(compare_val) != 0)
  
  diff_data <- diff_data[,c("age","pop","growth_rate") := NULL]
  diff_data <- diff_data[year %in% years]
  
  # Now, merge this onto the mcnty-level population and calculate an all-race value that is weighted by the race-specific 85+ population
  pop_full <- readRDS(pop_file)
  # first, calculate the mcnty-level population
  pop_full_mcnty <- pop_full[, list(pop = sum(pop), level = "mcnty"), keyby = "mcnty,year,sex,race,age"]
  setnames(pop_full_mcnty, "mcnty", "area")
  # now the state level population
  pop_full_state <- pop_full[, list(pop = sum(pop), level = "state"), keyby = "state,year,sex,race,age"]
  setnames(pop_full_state, "state", "area")
  # and national level
  pop_full_natl <- pop_full[, list(pop = sum(pop), area = 1, level = "natl"), keyby = "year,sex,race,age"]
  pop_full <- rbind(pop_full_mcnty, pop_full_state, pop_full_natl)
  
  # now calculate both-sex values
  pop_full <- rbind(pop_full, pop_full[,list(pop = sum(pop), sex = 3), by='level,area,year,race,age'])
  
  # reset tiny populations to 0 to avoid annoying edge cases where one group gets all of the weight
  # even though it has effectively no population
  pop_full[pop < 1e-5, pop := 0]
  
  # calculate aggregation weights for each area-level-year-sex-age
  pop_full[, wt := pop / sum(pop), by = "area,level,year,sex,age"]
  
  # this fails when the total population in a given level-area-year-sex-age is 0.
  # In these cases, we use the following hierarchy of backups: level-area-year-sex (no age),
  # level-area-year (no sex or age).
  pop_full[, pop2 := sum(pop), by = "area,level,year,sex,race"]
  pop_full[, wt2 := pop2 / sum(pop2), by = "area,level,year,sex,age"]
  
  # do not include the both-sex values in the pooling over sex since this is duplicative
  pop_full[sex != 3, pop3 := sum(pop), by = "area,level,year,race"]
  pop_full[sex != 3, wt3 := pop3 / sum(pop3), by = "area,level,year,sex,age"]
  
  pop_full[is.nan(wt), wt := wt2]
  pop_full[is.nan(wt), wt := wt3]
  
  stopifnot(nrow(pop_full[is.nan(wt)]) == 0)
  
  wts <- pop_full[age == 85, .(level, area, year, sex, race, age, wt)]
  stopifnot(wts[, sum(wt), by = "level,area,year,sex,age"][, max(abs(V1 - 1))] < 1e-10)
  wts[,age := NULL]
  
  ## Merge the population weights with the growth rates
  diff_data <- merge(diff_data, wts, by=c("year","sex","race"), all.x=T)
  stopifnot(nrow(diff_data[is.na(wt)]) == 0)
  diff_data[,weighted_gr := weighted.mean(mean_gr, wt), by=c("level", "area", "year", "sex")]
  diff_data_allrace <- unique(diff_data[,.(year,sex,level,area,weighted_gr)])[,race := all_pop_id]
  setnames(diff_data_allrace, "weighted_gr", "mean_gr")
  diff_data <- diff_data[,c("wt", "weighted_gr") := NULL]
  diff_data <- rbind(diff_data, diff_data_allrace) # add on the all-race estimates
  
  # save to model directory
  saveRDS(diff_data, paste0(dir, "/growth_rates.rds"))
  
}


