#' Pool population across time by summing data for all adjacent years.
#'
#' @param data data.table with pop data. Must have column pop.
#' @param pool_size number of years to pool over. Must be odd.
#' @param id_vars columns in data to group by and sum. E.g. sex and age.
#'
#' @return data.table with data pooled across time.
pool_adj_years <- function(data, pool_size, id_vars, pop_var){
  
  setnames(data, pop_var, 'pop')
  # Empty data.table for binding intermediate pooling steps
  data_pooled_all <- data.table(mid_year = numeric())
  
  # Loop through years creating a unique pooling for each year
  year_range <- sort(unique(data$year))
  for(yr in year_range){
    # Grab years to pool over
    pool_years <- c((yr - floor(pool_size / 2)):(yr + floor(pool_size / 2)))
    
    # If terminal years, use first/last window of size pool_size instead
    # Else create window with current year as center point
    if(any(pool_years < min(year_range))){
      pool_years <- head(year_range, pool_size)
    }else if(any(pool_years > max(year_range))){
      pool_years <- tail(year_range, pool_size)
    }else{
      pool_years <- c((yr - floor(pool_size / 2)):(yr + floor(pool_size / 2)))
    }
    
    # Create midpoint to keep track of previously made pop pools
    mid_pool_year <- pool_years[ceiling(length(pool_years)/2)]
    mid_pool_years_list <- c()
    
    # If pop pool has been made before, grab it and set as new year
    # Else create pooled pop as normal
    if(mid_pool_year %in% mid_pool_years_list){
      data_pooled_yr <- data_pooled_all[mid_year == mid_pool_year]
      data_pooled_yr <- data_pooled_yr[year == unique(data_pooled_yr$year)[1]]
      data_pooled_yr[,year := yr]
      data_pooled_all <- rbind(data_pooled_all, data_pooled_yr)
    }else{
      data_pooled_yr <- data[year %in% pool_years]
      data_pooled_yr <- data_pooled_yr[,.(pop = sum(pop),
                                          year = yr,
                                          mid_year = mid_pool_year),
                                       by = id_vars]
      data_pooled_all <- rbind(data_pooled_yr, data_pooled_all,
                               fill = TRUE)
      mid_pool_years_list <- c(mid_pool_years_list, mid_pool_year)
    }
  }
  
  # Remove midpoint tracking column
  data_pooled_all$mid_year <- NULL
  setnames(data_pooled_all, 'pop', pop_var)
  
  return(data_pooled_all)
}