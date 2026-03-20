###########################################################################################################################################
## Description: Estimates proportions of primary race categories for multiracial individuals using the NCHS race bridging algorithm,
##              and specifically the coefficients from its logistic regression models.
##
##              Citation: National Center for Health Statistics (U.S.), ed. U.S. Census 2000 Population with Bridged Categories.
##                        Vital and Health Statistics, no. 135. Hyattsville, Md: National Center for Health Statistics, 2003.
##
## Input:      Data set with demographic variables for multiracial individuals.
## Output:     Data set with multiracial individuals allocated proportionally to primary races. Returns a list of two datatables: 
##              one with the multiracial-bridged respondents, and one for respondents missing information (dt_missing).
##
###########################################################################################################################################

bridge_multiracial <- function(dt) {
  print("Estimating primary race categories for multiracial individuals...")
  
  ######## 1. Setup
  library(mice)
  library(boot)
  
  
  ####### 2. Load inputs
  #### Load population proportions by race
  pop_totals_wide <- readRDS("FILEPATH")
  
  #### Load urban-rural codes
  urban_rural <- readRDS("FILEPATH")
  
  ## Replicate 2018 codes for 2019
  urban_rural_2019 <- urban_rural[year == 2018]
  urban_rural_2019$year <- 2019
  urban_rural <- rbindlist(list(urban_rural, urban_rural_2019), use.names = TRUE, fill = TRUE)
  
  #### Load state-cnty-mcnty crosswalk file
  locs <- fread("FILEPATH")
  locs[, FIPS_code := sprintf("%05d", cnty)]
  
  #### Load state-Census region crosswalk file
  regions <- fread("FILEPATH")
  
  #### Load bridging algorithm coefficients (Liebler 2022)
  coefs <- readRDS("FILEPATH")
  
  ## Replicate 2018 coefs for 2019
  coefs_2019 <- coefs[end_year == 2018]
  coefs_2019$end_year <- 2019
  coefs <- rbindlist(list(coefs, coefs_2019), use.names = TRUE, fill = TRUE)
  
  
  ####### 3. Merge data sets together
  dt[, cnty_char := sprintf("%05d", cnty)]
  dt <- merge(dt, unique(pop_totals_wide[, list(cnty, year, White_alone, Black_alone, AIAN_alone, API_alone, Two_or_More_races)]), by.x = c("cnty_char", "year"), by.y = c("cnty", "year"), all.x = TRUE)
  dt <- merge(dt, unique(locs[, list(FIPS_code, state_name)]), by.x = c("cnty_char"), by.y = c("FIPS_code"), all.x = TRUE)
  dt <- merge(dt, unique(regions[, list(state, region_name)]), by.x = c("state_name"), by.y = c("state"), all.x = TRUE)
  dt <- merge(dt, unique(urban_rural[, list(fips, year, rur_urb_code)]), by.x = c("cnty_char", "year"), by.y = c("fips", "year"), all.x = TRUE)
  dt[is.na(census_region), census_region := region_name]
  
  
  ####### 4. Estimate primary race proportions using coefficients from NCHS race-bridging regression models 
  #### Set up indicator variables for regression terms
  dt[, sex_male := 2 - sex] # Male sex
  dt[, c("census_region_northeast",  "census_region_midwest", "census_region_south", "census_region_west") := 
       list(as.integer(census_region == "Northeast"), as.integer(census_region == "Midwest"), as.integer(census_region == "South"), as.integer(census_region == "West"))] # Census region indicators
  dt[, c("urbanization_large_urban",  "urbanization_large_suburban", "urbanization_med_small_metro", "urbanization_non_metro") := 
       list(as.integer(rur_urb_code == 1), as.integer(rur_urb_code == 2), as.integer(rur_urb_code == 3), as.integer(rur_urb_code == 4))] # Urbanization indicators
  dt[, c("not_aian_ind", "not_api_ind", "not_black_ind") := list((1 - race_aian), (1 - race_api), (1 - race_black))]
  
  #### Determine multiracial category (for consistency with coefficient table)
  # AIAN-B  AIAN-B-W    AIAN-W ALLGROUPS     API-B     API-W       B-W
  
  ## AIAN-B
  dt[!is.na(race_aian) & race_aian == 1 & !is.na(race_black) & race_black == 1 & !is.na(race_white) & race_white == 0 & !is.na(race_api) & race_api == 0, multiple_race_category := "AIAN-B"]
  ## AIAN-B-W
  dt[!is.na(race_aian) & race_aian == 1 & !is.na(race_black) & race_black == 1 & !is.na(race_white) & race_white == 1 & !is.na(race_api) & race_api == 0, multiple_race_category := "AIAN-B-W"]
  ## AIAN-W
  dt[!is.na(race_aian) & race_aian == 1 & !is.na(race_black) & race_black == 0 & !is.na(race_white) & race_white == 1 & !is.na(race_api) & race_api == 0, multiple_race_category := "AIAN-W"]
  ## API-B
  dt[!is.na(race_aian) & race_aian == 0 & !is.na(race_black) & race_black == 1 & !is.na(race_white) & race_white == 0 & !is.na(race_api) & race_api == 1, multiple_race_category := "API-B"]
  ## API-W
  dt[!is.na(race_aian) & race_aian == 0 & !is.na(race_black) & race_black == 0 & !is.na(race_white) & race_white == 1 & !is.na(race_api) & race_api == 1, multiple_race_category := "API-W"]
  ## B-W
  dt[!is.na(race_aian) & race_aian == 0 & !is.na(race_black) & race_black == 1 & !is.na(race_white) & race_white == 1 & !is.na(race_api) & race_api == 0, multiple_race_category := "B-W"]
  ## Remaining
  dt[is.na(multiple_race_category), multiple_race_category := "ALLGROUPS"]

  #### Merge coefs onto dt
  dt_bridged <- merge(dt, coefs, by.x = c("multiple_race_category", "year"), by.y = c("multiple_race_category", "end_year"), all.x = TRUE, allow.cartesian = TRUE)
  
  #### Calculate primary race probabilities for logistic regression models (every combination other than ALLGROUPS)
  ## Question: Is age top-coded at 69 as in the original algorithm? This was not explicitly indicated in Liebler (2022). 
  dt_bridged[, eta := 
              age_in_yrs_per_10_yrs * (age /10) + 
              hispanic_origin_not_hisp_ref * hispanic + 
              male_female_reference * sex_male + 
              northeast * census_region_northeast + 
              midwest * census_region_midwest +
              south * census_region_south +
              large_suburban * urbanization_large_suburban +
              medium_small_metro * urbanization_med_small_metro +
              non_metro * urbanization_non_metro +
              county_pop_aian * AIAN_alone * 100 +
              log_of_county_pop_aian * log(AIAN_alone * 100) +
              county_pop_api * API_alone * 100 +
              county_pop_black * Black_alone * 100 +
              square_of_county_pop_black * (Black_alone * 100)^2 +
              county_pop_multiple_race * Two_or_More_races +
              not_aian * not_aian_ind +
              not_api * not_api_ind +
              not_black * not_black_ind +
              constant
            ]
  
  #### Calculate proportions
  ## First, inv.logit eta to get proportions for directly predicted races, where multiple_race_category is not ALLGROUPS or AIAN-B-W
  dt_bridged[!(multiple_race_category %in% c("ALLGROUPS", "AIAN-B-W")), prop := inv.logit(eta)]
  
  ## Calculate total proportions of directly predicted races
  dt_bridged[, total_prop := sum(prop), by = c("uid")]
  
  ## Now duplicate rows for reference groups
  duped <- rbindlist(list(
    copy(dt_bridged[multiple_race_category == "AIAN-B"])[, c("predicted_race", "prop") := list("AIAN", prop = 1 - total_prop)],
    copy(dt_bridged[multiple_race_category == "AIAN-W"])[, c("predicted_race", "prop") := list("WHITE", prop = 1 - total_prop)],
    copy(dt_bridged[multiple_race_category == "API-B"])[, c("predicted_race", "prop") := list("API", prop = 1 - total_prop)],
    copy(dt_bridged[multiple_race_category == "API-W"])[, c("predicted_race", "prop") := list("WHITE", prop = 1 - total_prop)],
    copy(dt_bridged[multiple_race_category == "B-W"])[, c("predicted_race", "prop") := list("WHITE", prop = 1 - total_prop)]
    ), use.names = TRUE, fill = TRUE)
  
  #### Calculate proportions for AIAN-B-W and ALLGROUPS (multinomial logistic regression model)
  dt_bridged[multiple_race_category %in% c("AIAN-B-W", "ALLGROUPS"), prop := exp(eta) / (1 + sum(exp(eta))), by = c("uid")]
  multinomial <- copy(dt_bridged[multiple_race_category %in% c("AIAN-B-W", "ALLGROUPS")])[, prop := 1 / (1 + sum(exp(eta))), by = c("uid")][predicted_race == "BLACK"][, predicted_race := "WHITE"]
  multinomial <- rbindlist(list(dt_bridged[multiple_race_category %in% c("AIAN-B-W", "ALLGROUPS")], multinomial), use.names = TRUE, fill = TRUE)
  
  ## Drop predicted races that are inapplicable to a given individual
  multinomial <- multinomial[!(predicted_race == "WHITE" & race_white == 0)]
  multinomial <- multinomial[!(predicted_race == "BLACK" & race_black == 0)]
  multinomial <- multinomial[!(predicted_race == "AIAN" & race_aian == 0)]
  multinomial <- multinomial[!(predicted_race == "API" & race_api == 0)]
  
  ## Recalculate total_prop and rescaled prop from remaining races
  multinomial[, total_prop := sum(prop), by = c("uid")]
  multinomial[, prop := prop * (1 / total_prop)]
  multinomial[, total_prop := sum(prop), by = c("uid")]

  #### Combine data sets
  dt_bridged <- rbindlist(list(dt_bridged[!(multiple_race_category %in% c("AIAN-B-W", "ALLGROUPS"))], duped, multinomial), use.names = TRUE, fill = TRUE)
  
  #### Check total props
  dt_bridged[, total_prop := sum(prop), by = c("uid")]

  #### Assign primary races
  dt_bridged[predicted_race == "WHITE", primary_race_ethnicity := "NH White"]
  dt_bridged[predicted_race == "BLACK", primary_race_ethnicity := "NH Black"]
  dt_bridged[predicted_race == "AIAN", primary_race_ethnicity := "NH AIAN"]
  dt_bridged[predicted_race == "API", primary_race_ethnicity := "NH API"]
  
  #### Set re_weight
  dt_bridged[, re_weight := prop]
  
  #### Identify columns to keep
  cols <- colnames(dt)
  
  #### Extract rows with missing re_weight
  dt_missing <- dt_bridged[is.na(re_weight), ..cols]
  dt_missing[, primary_race_ethnicity := NA]
  dt_missing <- unique(dt_missing)
  
  ####### 5. Return bridged data set, along with rows for which race could not be imputed
  return(list("dt_bridged" = dt_bridged[!is.na(re_weight), ..cols], "dt_missing" = dt_missing))
}
