####################################################################################################
## Description: From 1988 through 1991, decedent's counties of death (and residence, if they were GA
##              residents) was coded to 999 if:
##                a. an HIV cause code appeared anywhere on their death certificate, and
##                b. there were three or fewer such certificates in that county-year.
##
##              Reassign these deaths to the counties that didn't have any uncensored HIV deaths.
##              Proportions are calculated using HIV deaths from 1987 and 1992-1995 in counties that
##              didn't have an uncensored HIV deaths in the year that is being adjusted.
####################################################################################################

library(data.table)

rm(list=ls())

data_dir <- "FILEPATH"

proportion_years <- c(1987, 1992:1995)
adjustment_years <- 1988:1991

# need to make sure we keep the cause columns as a character variable
cause_columns <- rep("character", 21)
cause_column_names <- c("cause", paste0("multiple_cause_", 1:20))
names(cause_columns) <- cause_column_names


# Read in deaths data -----------------------------------------------------

deaths <- lapply(c(proportion_years, adjustment_years), function(year) {
  print(year)
  deaths <- fread(paste0(data_dir, "data_", year, "_cleaned.csv"), colClasses=cause_columns)
})
deaths <- rbindlist(deaths, use.names = TRUE)
total_deaths_adjustment_years <- sum(deaths[year %in% adjustment_years]$deaths)


# Identify HIV deaths and counties that reported HIV deaths ---------------
deaths_outside_georgia <- deaths[state_res_alpha != "GA" & year %in% adjustment_years]
deaths <- deaths[state_res_alpha == "GA"]

# determine which Georgia deaths include HIV codes anywhere on their death certificate
deaths[, id := .I]
deaths_long_with_cause <- melt(deaths, id.vars=c("id", "year", "county_res"),
                               measure.vars=cause_column_names, value.name="cause")
deaths_long_with_cause[, cause := as.numeric(cause)]
deaths_long_with_cause <- deaths_long_with_cause[(cause >= 42 & cause < 45) | cause == 795.8] # subset to HIV causes
hiv_death_ids <- unique(deaths_long_with_cause$id)

# separate out hiv deaths
hiv_deaths_proportion_years <- deaths[id %in% hiv_death_ids & year %in% proportion_years]
hiv_deaths_adjustment_years <- deaths[id %in% hiv_death_ids & year %in% adjustment_years]
hiv_deaths_proportion_years[, id := NULL]
hiv_deaths_adjustment_years[, id := NULL]

GA_hiv_props <- lapply(adjustment_years, function(this_year){
  hiv_reporting_cnties <- unique(hiv_deaths_adjustment_years[year == this_year & full_fips_res_numeric != 13999, full_fips_res_numeric])
  proportions <- hiv_deaths_proportion_years[!full_fips_res_numeric %in% hiv_reporting_cnties]
  proportions <- proportions[, list(deaths=sum(deaths)), keyby=c("full_fips_res_numeric")]
  proportions <- proportions[, proportion := deaths / sum(deaths)]
  proportions[, deaths := NULL]
  proportions[, year := this_year]
  return(proportions)
  }
)

GA_hiv_props <- rbindlist(GA_hiv_props)
write.csv(GA_hiv_props, "FILEPATH",
          row.names = F)
