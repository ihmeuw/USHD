####################################################################################################
## Description: Calling these functions calculates population weights for aggregating covariates by
##              race/ethnicity from FIPS to merged counties. One set of weights is created for
##              Census data and another for ACS data, as each source begins with different race
##              groups: Census data are already split into non-Hispanic and Hispanic, whereas ACS
##              data are given as combined and need to split as part of the preparation step.
##
## Note: currently, 'other race' is included in population counts for each race group but excluded 
##       from covariate value estimation since we do not model that race group. This is a limitation
##       we need to state, but the effect is small; 'other race' accounts for only about 0.2% of the
##       non-Hispanic population as a whole.
####################################################################################################

library(data.table)

# create population weights for combined h+nh race groups in ACS data
load_acs_pop_weights <- function() {
  weights <- readRDS(paste0(root, "[FILEPATH]"))
  weights[, race := ifelse(race == "Two or More races", "multi", tolower(gsub(" .*", "", race)))]
  weights <- weights[, list(wt = sum(pop), hisp_pop = sum(pop[hisp == 1])), keyby = 'mcnty,year,race']
  hisp <- copy(weights[, list(mcnty, year, hisp_pop)])
  hisp <- hisp[, list(race = 'hisp', wt = sum(hisp_pop)), keyby = 'mcnty,year']
  white_nh <- copy(weights[race == 'white'])
  white_nh <- white_nh[, list(race = 'white_nh', wt = wt - hisp_pop), keyby = 'mcnty,year']  # get only non-Hispanic portion of white population
  weights[, hisp_pop := NULL]
  weights <- rbind(weights, hisp, white_nh); rm(hisp, white_nh)
  weights[, wt := 0.2*wt]  # arbitrarily reduce weights by constant factor to avoid overflow error later
  setnames(weights, 'race', 'race_group')  # to match with ACS data
  return(weights)
}

# create population weights for 1980, 1990, 2000 Census data
load_census_pop_weights <- function() {
  weights <- readRDS(paste0(root, "[FILEPATH]"))
  weights[loc, on = 'fips', mcnty := i.mcnty]
  weights <- weights[, list(wt = sum(pop)), keyby = 'year,mcnty,race_group']
  weights[is.na(wt), wt := 0]    # assume NA pop = 0 pop, since there are no other 0 values
  weights[, wt := 0.2*wt]  # arbitrarily reduce weights by constant factor to avoid overflow error later
  return(weights)
}