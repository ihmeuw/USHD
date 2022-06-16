#################################################################################################################
# Purpose: Store interpolation functions used for filling in missing years in
# covariate time series.
#
# The Standard Interpolation Function
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The "skeleton" of the rest of the interpolation functions, this
# uses linear interpolation across a span of given years, filling in
# missing years and holding the first/last values constant if the data
# values do not cover the full extent of the desired time period.

# Forecast-Backcast-Interpolate function.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This function not only uses linear interpolation for missing data points
# within the time frame, but uses transformations to ensure that data values never
# go outside a certain range (you can specify a log or logit transform for the variables
# so that they are between 0 and 1 (logit transform) or are always positive (log-transform).
# This function also uses forecasting/backcasting using the rate of change from a 5-year period (default),
# or the time period used to forecast/backcast can be specified. A variant of this function, that
# performs the same operation based on age/sex breakdowns, also exists in this file.
##################################################################################################################


# Standard Interpolation Function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## This is the "basic" interpolation function, that takes a data table, the year range in which you want to interpolate across, and the
## ID variables that you want to use.

# file path for records of interpolated years
root_dir <- ifelse(Sys.info()[1]=="Windows", "[FILEPATH]", "[FILEPATH]")
observed_year_dir <- paste0(root_dir, "[FILEPATH]")

standard_interp <- function(data, interp_years, id_vars, covar_name, by_race = F){
  interp <- function(x, y, xout) {
    if (length(x) == 1) {
      return(rep(y, length(xout)))
    } else {
      return(approx(x, y, xout, rule=2)$y)
    }
  }
  
  # open observed years file for editing (years NOT interpolated)
  sink(paste0(observed_year_dir, covar_name, if (by_race) "_by_race_ethn", "[FILEPATH]"))
  
  # determine years for which we have observations
  lapply(interp_years, function(yr) {
    if (yr %in% data$year) {
      message("Observations exist for ", yr)
      cat(paste0("Observations exist for ", yr, "\n"))
    } 
  })
  
  # close observed years file
  sink()
  
  data <- melt.data.table(data, id.vars = id_vars)
  data <- data[!is.na(value),]
  data <- data[, list(year = interp_years, value = interp(year, value, interp_years)),
               by = c('variable', 'mcnty', if (by_race) 'race_group')]
  cast_formula <- paste0('mcnty + year ', if (by_race) '+ race_group ', '~ variable')
  data <- dcast.data.table(data, formula = cast_formula)
}


# Age-Sex Interpolation/forecast-backcast Function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## A Modification of the forecast_back-cast function, this



forecast_backcast_interpolate_age_sex<-function(data,
                                                interp_years,
                                                covar_name,
                                                geog_id,
                                                log_transform_list = NULL,
                                                logit_transform_list = NULL,
                                                roc_year_override_f = FALSE,
                                                chosen_gap_f = NULL,
                                                roc_year_override_b = FALSE,
                                                chosen_gap_b = NULL){


  id_vars<-c(geog_id, "year","age","sex") # defining the ID variables for the transforms from wide to long.

  ### This function interpolates, and then forecasts and back-casts to the "target years", which is the full year-series
  ### that you want at the end of the process (ex: 1990-2014).
  #
  # Inputs:
  # - a data table with years, and geographic units varying long (long table), with 1 or more variables *ex columns in data table: year, mcnty, edu_ba, edu_hs, edu_grad
  # - interp_years: a list or sequence of the years you are looking for * ex: seq(1990,2014,1) or 1990:2014
  # - id_vars: the ID variables in your data set that aren't your varaibles of interest *ex: id_vars=c("mcnty", "year")
  #
  # This function:
  # - Discovers what years you "go in with"- your orig_years of observed data
  # - Learns whether you need to forecast and/or backcast
  # - Interpolates between intermediate years
  # - If you need to forecast:

  # Defining the function "interp", that you will apply to the data.table to fill in intermediate years and carry forward/backward the most recent time point:
    interp <- function(x, y, xout) {
      # if there is only 1 observation/time point, then repeat it for each observation within the time series you are looking for
      if (length(x) == 1) return(rep(y, length(xout)))
      # If there are multiple time points, fill in the in-between with linear interpolation, then extend out the first and last year's data
      # to the rest of the years in the years of interest
      else return(approx(x, y, xout, rule=2)$y)
    }

    # open observed years file for editing (years NOT interpolated, forecast, or backcast)
    sink(paste0(observed_year_dir, covar_name, "[FILEPATH]"))
    
    # determine years for which we have observations
    lapply(interp_years, function(yr) {
      if (yr %in% data$year) {
        message("Observations exist for ", yr)
        cat(paste0("Observations exist for ", yr, "\n"))
      } 
    })
    
    # close observed years file
    sink()

  # Discovering what the original years in the data set are:
    orig_years<-sort(unique(data$year))
    first_year_of_data<-min(orig_years)
    last_year_of_data<-max(orig_years)

  # Discovering what the first and last years of the target years:
    first_year_of_target_years<-sort(interp_years)[1]
    last_year_of_target_years<-sort(interp_years)[length((interp_years))] # we are sorting first just in case someone put a list that doesn't follow lowest-to-largest format

  # Interpolating in the intermediate years, and carrying forward/backward years that are the "same"
    data <- melt(data, id.vars=id_vars)
    data <- data[!is.na(value),]

  # Actually interpolating between values
if(geog_id=="mcnty"){
    data <- data[, list(year = interp_years, value = interp(year, value, interp_years)), by='variable,mcnty,age,sex'] # this line "calls" the interpolation function to be carried out on each variable and geography separately
}
if(geog_id=="mtract"){
  data <- data[, list(year = interp_years, value = interp(year, value, interp_years)), by='variable,mtract,age,sex'] # this line "calls" the interpolation function to be carried out on each variable and geography separately

}
  ##########################################
  ## LOGIT TRANSFORMING RELEVANT VARIABLES
  ##########################################
  # This will allow us to logit-transform any variables within the specified list to make sure the forecasted/back-casted values are between 0 and 1.
  if (length(logit_transform_list)>0){ # If there are actually variables in the logit_transform_list...

    # Stop if there are values greater than 1
    if (sum(data[(variable %in% logit_transform_list)&value>1]$value)) stop("can't logit-transform values greater than 1")

    data[variable %in% logit_transform_list, value:=qlogis(value)]} # logit transform the values!

if (length(log_transform_list)>0){
    data[variable %in% log_transform_list, value:=log(value)]} # logit transform the values!


###############
## Forecasting
###############
transformed_to_year_wide<-F
  # Check if you need to forecast
  if (last_year_of_data>=last_year_of_target_years){print("Looks like you don't need to forecast out in this data set!")}else{
    print(paste0("Looks like we need to apply rates of change from the past to get to ",last_year_of_target_years,"; the last year represented in this data set is ",last_year_of_data,"."))

    # "casting out" the data such that years are wide: we can now modify the early/late years' entries (which are currently the most recent/least recent year of data)
    # to be the interpolated version.
    if(geog_id=="mcnty"){
      data<-dcast.data.table(data, mcnty+variable+age+sex~year)
    }
    if(geog_id=="mtract"){
      data<-dcast.data.table(data, mtract+variable+age+sex~year)
    }
      transformed_to_year_wide<-T

    # Discover what years you need to forecast:
      years_you_need_to_forecast<-interp_years[interp_years>last_year_of_data]

    # Calculating the options you have for calculating a rate of change: how many years back do you observations go before the most recent year of data?
      forecasting_rate_of_change_options<-last_year_of_data-orig_years
      forecasting_rate_of_change_options<-forecasting_rate_of_change_options[forecasting_rate_of_change_options!=0] #getting rid of the "0" years difference; we can't calculate a rate of change from the most recent year alone
      cat("Your options for calculating a rate of change to forecast are this many years removed from your most recent data points:",forecasting_rate_of_change_options)

    # Testing to see if there are any options under 5 years, and choosing the 5-year increment if it exists, or overriding it to a different time gap
      if (sum(forecasting_rate_of_change_options %in% 1:5)>0){
        chosen_year_for_roc<-last_year_of_data-max(forecasting_rate_of_change_options[(forecasting_rate_of_change_options %in% 1:5)==TRUE]) # picking the biggest gap from 1-5 years that exists
        if (roc_year_override_f==TRUE){chosen_year_for_roc<-last_year_of_data-chosen_gap_f; print(paste0("Using the chosen gap of",chosen_gap_f))}
      }else{chosen_year_for_roc<-last_year_of_data-min(forecasting_rate_of_change_options)} # if there aren't any data points 5 years ago or sooner from the last data point, just use the most recent 2 data points to forecast.
  print(paste0("Using these years to come up with the rate of change:",chosen_year_for_roc," to ",last_year_of_data))
      data[["chosen_rate_of_change"]]<-(data[[as.character(last_year_of_data)]]-data[[as.character(chosen_year_for_roc)]])/(last_year_of_data-chosen_year_for_roc)
    # if the chosen rate of change is infinite, negative infinite, or NAN, have the rate of change be 0-- it's already  "maxed out" to 0 or 1 in that geography-year.
    data[is.infinite(chosen_rate_of_change)|is.na(chosen_rate_of_change),chosen_rate_of_change:=0 ]

    # Adding the rate of change from the year of choice into the years you need to forecast
    print("Forecasting!")
    for (forecast_yr in years_you_need_to_forecast){
      n_years_to_multiply_rate_of_change<-forecast_yr-last_year_of_data
      data[[as.character(forecast_yr)]]<-data[[as.character(forecast_yr)]] + (data[["chosen_rate_of_change"]]*n_years_to_multiply_rate_of_change)
    }
    # if the rate of change is NaN, it's because there are infinite or negative infinite values that are the same
    data[["chosen_rate_of_change"]]<-NULL # Getting rid of the rate of change column-- we'll add a different/new one if we need to back-cast.
    } #closing if-forecasting loop

################
##  BACKCASTING
################
  # First, check to see if you need to back-cast
    if (first_year_of_data<=first_year_of_target_years){print(paste0("Looks like you don't need to backcast in this data set! Your first year of data is ",first_year_of_data,", which is less than or equal to the target year range's lower bounds of ",first_year_of_target_years))}else{
      print(paste0("Looks like we need to apply rates of change to back-cast to ",first_year_of_target_years,"; the first year represented in this data set is ",first_year_of_data,"."))
      if (transformed_to_year_wide==F) {
        if(geog_id=="mcnty"){data<-dcast.data.table(data, mcnty+variable+age+sex~year)}
        if(geog_id=="mtract"){data<-dcast.data.table(data, mtract+variable+age+sex~year)}}
# transforming the data set in case you didn't need to forecast

    # Discover what years you need to back-cast:
      years_you_need_to_backcast<-interp_years[interp_years<first_year_of_data]

    # Calculating the options you have for calculating a rate of change: how many years back do you observations go before the most recent year of data?
      backcasting_rate_of_change_options<-orig_years-first_year_of_data
      backcasting_rate_of_change_options<-backcasting_rate_of_change_options[backcasting_rate_of_change_options!=0] #getting rid of the "0" years difference; we can't calculate a rate of change from one data point alone
      cat("Your options for calculating a rate of change to backcast are this many years removed from your first data point:",backcasting_rate_of_change_options)

    # Testing to see if there are any options under 5 years, and choosing the 5-year increment if it exists, or overriding it to a different time gap
    if (sum(backcasting_rate_of_change_options %in% 1:5)>0){
      chosen_year_for_roc<-first_year_of_data+max(backcasting_rate_of_change_options[(backcasting_rate_of_change_options %in% 1:5)==TRUE]) # picking the biggest gap from 1-5 years that exists
      if (roc_year_override_b==TRUE){chosen_year_for_roc<-first_year_of_data+chosen_gap_b}
    }else{chosen_year_for_roc<-first_year_of_data+min(backcasting_rate_of_change_options)} # if there aren't any data points 5 years ago or sooner from the last data point, just use the most recent 2 data points to forecast.

    data[["chosen_rate_of_change"]]<-(data[[as.character(chosen_year_for_roc)]]-data[[as.character(first_year_of_data)]])/(chosen_year_for_roc-first_year_of_data)
    # if the chosen rate of change is infinite, negative infinite, or NAN, have the rate of change be 0-- it's already  "maxed out" to 0 or 1 in that geography-year.
    data[is.infinite(chosen_rate_of_change)|is.na(chosen_rate_of_change),chosen_rate_of_change:=0 ]

    # Adding the rate of change from the year of choice into the years you need to forecast
    print("Back-casting!")
    for (backcast_yr in years_you_need_to_backcast){
      n_years_to_multiply_rate_of_change<-first_year_of_data-backcast_yr
      data[[as.character(backcast_yr)]]<-data[[as.character(backcast_yr)]] - (data[["chosen_rate_of_change"]]*n_years_to_multiply_rate_of_change)
    }
    data[["chosen_rate_of_change"]]<-NULL # Getting rid of the rate of change column-- no longer necessary
  }# Closing the back-casting clause.

data <- melt(data, id.vars=c(geog_id,"variable","age","sex"), variable.name="year")
data[, year:=as.numeric(as.character(year))]


################################################
## UNDO LOGIT TRANSFORMING RELEVANT VARIABLES
###############################################
### At this point, we need to get the variable we logit-tranformed back to normal.
if (length(logit_transform_list)>0){ # If there are actually variables in the logit_transform_list...
   data[variable %in% logit_transform_list, value:=plogis(value)]} # transform back out of logit space!

if (length(log_transform_list)>0){ # If there are actually variables in the log_transform_list...
  data[variable %in% log_transform_list, value:=exp(value)]} # transform back out of log space!

################################################
## Transform data from long to wide
###############################################
## Transforming the data back out into the original format, with years added and interpolation finished.
if (geog_id=="mcnty"){data <- dcast.data.table(data, mcnty + year + age + sex  ~ variable)} # "casting" the data back to wide from long (making the varaibles (in case you were interpolating more than 1))
if (geog_id=="mtract"){data <- dcast.data.table(data, mtract + year + age + sex ~ variable)} # "casting" the data back to wide from long (making the varaibles (in case you were interpolating more than 1))

return(data)
}# close function

#####################################
# NORMAL FORECAST/BACKCAST/INTERP
#####################################

forecast_backcast_interpolate<-function(data,
                                        interp_years,
                                        covar_name,
                                        geog_id,
                                        logit_transform_list = NULL,
                                        log_transform_list = NULL,
                                        roc_year_override_f = FALSE,
                                        chosen_gap_f = NULL,
                                        roc_year_override_b = FALSE,
                                        chosen_gap_b = NULL){

  id_vars<-c(geog_id, "year") # defining the ID variables for the transforms from wide to long.

  ### This function interpolates, and then forecasts and back-casts to the "target years", which is the full year-series
  ### that you want at the end of the process (ex: 1990-2014).
  #
  # Inputs:
  # - a data table with years, and geographic units varying long (long table), with 1 or more variables *ex columns in data table: year, mcnty, edu_ba, edu_hs, edu_grad
  # - interp_years: a list or sequence of the years you are looking for * ex: seq(1990,2014,1) or 1990:2014
  # - id_vars: the ID variables in your data set that aren't your variables of interest *ex: id_vars=c("mcnty", "year")
  #
  # This function:
  # - Discovers what years you "go in with"- your orig_years of observed data
  # - Learns whether you need to forecast and/or backcast
  # - Interpolates between intermediate years
  # - If you need to forecast:

  # Defining the function "interp", that you will apply to the data.table to fill in intermediate years and carry forward/backward the most recent time point:
  interp <- function(x, y, xout) {
    # if there is only 1 observation/time point, then repeat it for each observation within the time series you are looking for
    if (length(x) == 1) return(rep(y, length(xout)))
    # If there are multiple time points, fill in the in-between with linear interpolation, then extend out the first and last year's data
    # to the rest of the years in the years of interest
    else return(approx(x, y, xout, rule=2)$y)
  }

  # open observed years file for editing (years NOT interpolated, forecast, or backcast)
  sink(paste0(observed_year_dir, covar_name, "[FILEPATH]"))
  
  # determine years for which we have observations
  lapply(interp_years, function(yr) {
    if (yr %in% data$year) {
      message("Observations exist for ", yr)
      cat(paste0("Observations exist for ", yr, "\n"))
    } 
  })
  
  # close observed years file
  sink()

  # Discovering what the original years in the data set are:
  orig_years<-sort(unique(data$year))
  first_year_of_data<-min(orig_years)
  last_year_of_data<-max(orig_years)

  # Discovering what the first and last years of the target years:
  first_year_of_target_years<-sort(interp_years)[1]
  last_year_of_target_years<-sort(interp_years)[length((interp_years))] # we are sorting first just in case someone put a list that doesn't follow lowest-to-largest format

  # Interpolating in the intermediate years, and carrying forward/backward years that are the "same"
  data <- melt(data, id.vars=id_vars)
  data <- data[!is.na(value),]

  # Actually interpolating between values
  if (geog_id=="mcnty"){data <- data[, list(year = interp_years, value = interp(year, value, interp_years)), by='variable,mcnty'] }# this line "calls" the interpolation function to be carried out on each variable and geography separately
  if (geog_id=="mtract"){data <- data[, list(year = interp_years, value = interp(year, value, interp_years)), by='variable,mtract']} # this line "calls" the interpolation function to be carried out on each variable and geography separately

  ##########################################
  ## LOGIT TRANSFORMING RELEVANT VARIABLES
  ##########################################
  # This will allow us to logit-transform any variables within the specified list to make sure the forecasted/back-casted values are between 0 and 1.
  if (length(logit_transform_list)>0){ # If there are actually variables in the logit_transform_list...

    # If there are any values greater than 1, stop.
    if (sum(data[(variable %in% logit_transform_list)&value>1]$value)) stop("can't logit transform values greater than 1")
    data[round(value, digits=15) == 1, value := 1]
    data[variable %in% logit_transform_list, value:=qlogis(value)]} # logit transform the values!


  if (length(log_transform_list)>0){
    data[variable %in% log_transform_list, value:=log(value)]} # logit transform the values!

  ###############
  ## Forecasting
  ###############
  transformed_to_year_wide<-F
  # Check if you need to forecast
  if (last_year_of_data>=last_year_of_target_years){print("Looks like you don't need to forecast out in this data set!")}else{
    print(paste0("Looks like we need to apply rates of change from the past to get to ",last_year_of_target_years,"; the last year represented in this data set is ",last_year_of_data,"."))

    # "casting out" the data such that years are wide: we can now modify the early/late years' entries (which are currently the most recent/least recent year of data)
    # to be the interpolated version.
    if (geog_id=="mcnty"){data<-dcast.data.table(data, mcnty+variable~year)}
    if (geog_id=="mtract"){data<-dcast.data.table(data, mtract+variable~year)}
    transformed_to_year_wide<-T

    # Discover what years you need to forecast:
    years_you_need_to_forecast<-interp_years[interp_years>last_year_of_data]

    # Calculating the options you have for calculating a rate of change: how many years back do you observations go before the most recent year of data?
    forecasting_rate_of_change_options<-last_year_of_data-orig_years
    forecasting_rate_of_change_options<-forecasting_rate_of_change_options[forecasting_rate_of_change_options!=0] #getting rid of the "0" years difference; we can't calculate a rate of change from the most recent year alone
    cat("Your options for calculating a rate of change to forecast are this many years removed from your most recent data points:",forecasting_rate_of_change_options)

    # Testing to see if there are any options under 5 years, and choosing the 5-year increment if it exists, or overriding it to a different time gap
    if (sum(forecasting_rate_of_change_options %in% 1:5)>0){
      chosen_year_for_roc<-last_year_of_data-max(forecasting_rate_of_change_options[(forecasting_rate_of_change_options %in% 1:5)==TRUE]) # picking the biggest gap from 1-5 years that exists
      if (roc_year_override_f==TRUE){chosen_year_for_roc<-last_year_of_data-chosen_gap_f; print(paste0("Using the chosen gap of",chosen_gap_f))}
    }else{chosen_year_for_roc<-last_year_of_data-min(forecasting_rate_of_change_options)} # if there aren't any data points 5 years ago or sooner from the last data point, just use the most recent 2 data points to forecast.

    data[["chosen_rate_of_change"]]<-(data[[as.character(last_year_of_data)]]-data[[as.character(chosen_year_for_roc)]])/(last_year_of_data-chosen_year_for_roc)
    # if the chosen rate of change is infinite, negative infinite, or NAN, have the rate of change be 0-- it's already  "maxed out" to 0 or 1 in that geography-year.
    data[is.infinite(chosen_rate_of_change)|is.na(chosen_rate_of_change),chosen_rate_of_change:=0 ]

    # Adding the rate of change from the year of choice into the years you need to forecast
    print("Forecasting!")
    for (forecast_yr in years_you_need_to_forecast){
      n_years_to_multiply_rate_of_change<-forecast_yr-last_year_of_data
      data[[as.character(forecast_yr)]]<-data[[as.character(forecast_yr)]] + (data[["chosen_rate_of_change"]]*n_years_to_multiply_rate_of_change)
    }
    # if the rate of change is NaN, it's because there are infinite or negative infinite values that are the same
    data[["chosen_rate_of_change"]]<-NULL # Getting rid of the rate of change column-- we'll add a different/new one if we need to back-cast.
  } #closing if-forecasting loop

  ################
  ##  BACKCASTING
  ################
  # First, check to see if you need to back-cast
  if (first_year_of_data<=first_year_of_target_years){print(paste0("Looks like you don't need to backcast in this data set! Your first year of data is ",first_year_of_data,", which is less than or equal to the target year range's lower bounds of ",first_year_of_target_years))}else{
    print(paste0("Looks like we need to apply rates of change to back-cast to ",first_year_of_target_years,"; the first year represented in this data set is ",first_year_of_data,"."))
    if (transformed_to_year_wide==F) {
      if(geog_id=="mcnty"){data<-dcast.data.table(data, mcnty+variable~year)}
      if(geog_id=="mtract"){data<-dcast.data.table(data, mtract+variable~year)}
    }
      # transforming the data set in case you didn't need to forecast

    # Discover what years you need to back-cast:
    years_you_need_to_backcast<-interp_years[interp_years<first_year_of_data]

    # Calculating the options you have for calculating a rate of change: how many years back do you observations go before the most recent year of data?
    backcasting_rate_of_change_options<-orig_years-first_year_of_data
    backcasting_rate_of_change_options<-backcasting_rate_of_change_options[backcasting_rate_of_change_options!=0] #getting rid of the "0" years difference; we can't calculate a rate of change from one data point alone
    cat("Your options for calculating a rate of change to backcast are this many years removed from your first data point:",backcasting_rate_of_change_options)

    # Testing to see if there are any options under 5 years, and choosing the 5-year increment if it exists, or overriding it to a different time gap
    if (sum(backcasting_rate_of_change_options %in% 1:5)>0){
      chosen_year_for_roc<-first_year_of_data+max(backcasting_rate_of_change_options[(backcasting_rate_of_change_options %in% 1:5)==TRUE]) # picking the biggest gap from 1-5 years that exists
      if (roc_year_override_b==TRUE){chosen_year_for_roc<-first_year_of_data+chosen_gap_b}
    }else{chosen_year_for_roc<-first_year_of_data+min(backcasting_rate_of_change_options)} # if there aren't any data points 5 years ago or sooner from the last data point, just use the most recent 2 data points to forecast.

    data[["chosen_rate_of_change"]]<-(data[[as.character(chosen_year_for_roc)]]-data[[as.character(first_year_of_data)]])/(chosen_year_for_roc-first_year_of_data)
    # if the chosen rate of change is infinite, negative infinite, or NAN, have the rate of change be 0-- it's already  "maxed out" to 0 or 1 in that geography-year.
    data[is.infinite(chosen_rate_of_change)|is.na(chosen_rate_of_change),chosen_rate_of_change:=0 ]

    # Adding the rate of change from the year of choice into the years you need to forecast
    print("Back-casting!")
    for (backcast_yr in years_you_need_to_backcast){
      n_years_to_multiply_rate_of_change<-first_year_of_data-backcast_yr
      data[[as.character(backcast_yr)]]<-data[[as.character(backcast_yr)]] - (data[["chosen_rate_of_change"]]*n_years_to_multiply_rate_of_change)
    }
    data[["chosen_rate_of_change"]]<-NULL # Getting rid of the rate of change column-- no longer necessary
  }# Closing the back-casting clause.

  if (transformed_to_year_wide) {
    data <- melt(data, id.vars=c(geog_id,"variable"), variable.name="year")
    data[, year:=as.numeric(as.character(year))]
  }

  ################################################
  ## UNDO LOGIT TRANSFORMING RELEVANT VARIABLES
  ###############################################
  ### At this point, we need to get the variable we logit-tranformed back to normal.
  if (length(logit_transform_list)>0){ # If there are actually variables in the logit_transform_list...
    data[variable %in% logit_transform_list, value:=plogis(value)]} # transform back out of logit space!

  if (length(log_transform_list)>0){ # If there are actually variables in the log_transform_list...
    data[variable %in% log_transform_list, value:=exp(value)]} # transform back out of log space!

  ################################################
  ## Transform data from long to wide
  ###############################################
  ## Transforming the data back out into the original format, with years added and interpolation finished.
  if (geog_id=="mcnty"){data <- dcast.data.table(data, mcnty + year ~ variable)} # "casting" the data back to wide from long (making the varaibles (in case you were interpolating more than 1))
  if (geog_id=="mtract"){data <- dcast.data.table(data, mtract + year ~ variable)} # "casting" the data back to wide from long (making the varaibles (in case you were interpolating more than 1))

  return(data)
}# close function
