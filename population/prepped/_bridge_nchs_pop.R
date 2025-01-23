###################################################################################################
## Description: As of 2021, NCHS no longer produces bridged-race population estimates.
## 
##              Step 1: Using bridged-race estimates + the census bureau's population estimates
##              from 2020, we can determine the number of multiracial (MR) people who were bridged
##              to any given single race group (e.g., AIAN bridged pop - AIAN alone pop = # of
##              MR people bridged to AIAN). Then, for each county/age/sex/year, we can
##              calculate the proportion of MR people who were bridged to each race group.
##              
##              Step 2: To census pop estimates for 2021+, we can apply the proportions from Step 1
##              to the "Two or more races" estimate, and then add the resulting totals to their
##              respective "[race] alone" groups.
##              
##              Note: this function is written specifically to be called from pop_by_race_ethn_1977.r.
##              It references objects directly from that script and won't work elsewhere.
##              
## Outputs:     Dataset with multiracial population bridged to primary races for the specified
##              data year (2021+)
###################################################################################################

bridge_nchs_pop <- function(census2020,  # the input census population estimates for 2020
                            dt_to_bridge,  # table of estimates to bridge
                            nchs2020,  # the 2020 bridged-race estimates from NCHS 
                            data_year  # data year you want to bridge estimates for
) {
  
  # Calculate bridging proportions ----------------------------------------------------------------
  pop_orig <- sum(dt_to_bridge[year == data_year, pop_alone])  # for verifying population totals at the end
  
  # aggregate ages to match census age groups if necessary
  if (!"age_start" %in% names(nchs2020)) {
    nchs2020[, age_start := 5 * floor(age / 5)]
  }
  nchs2020[, age_start := ifelse(age_start == 1, 0, age_start)]  # need 0-4 to be one age group for bridging
  nchs2020 <- nchs2020[, list(pop_bridged = sum(pop)), by = 'fips,year,sex,age_start,race']
  
  # merge with census dataset and calculate proportions
  census2020 <- census2020[race != 9]  # remove multiracial individuals
  
  props <- merge.data.table(census2020[race != 7], nchs2020[race != 7], all = T,  # drop Hispanic since it doesn't need to be bridged
                            by = c("fips", "year", "sex", "age_start", "race"))
  stopifnot(nrow(props[is.na(pop_alone) | is.na(pop_bridged)]) == 0)  # check for mismatches
  props[, pop_mr := pop_bridged - pop_alone]
  stopifnot(nrow(props[pop_mr < 0]) == 0)  # should be no negative population values
  
  # create additional population totals to create proportions for strata where total MR population is 0 or negative
  props[, pop_mr2 := sum(pop_mr), by = c("fips", "year", "age_start", "race")]  # agg over sex
  props[, age_grp := dplyr::case_when(age_start < 20 ~ "0-19", age_start %in% 20:35 ~ "20-39",
                                      age_start %in% 40:55 ~ "40-59", age_start %in% 60:75 ~ "60-79",
                                      age_start >= 80 ~ "80+", TRUE ~ NA_character_)]
  props[, pop_mr3 := sum(pop_mr), by = c("fips", "year", "age_grp", "race")]  # agg over 20-year age groups
  props[, pop_mr4 := sum(pop_mr), by = c("fips", "year", "race")]  # agg over all ages
  props[, state := ifelse(nchar(fips) == 4, as.numeric(substr(fips, 1, 1)), as.numeric(substr(fips, 1, 2)))]
  props[, pop_mr5 := sum(pop_mr), by = c("state", "year", "race")]  # agg over FIPS to state
  
  stopifnot(nrow(props[pop_mr5 == 0]) == 0)  # check that all strata have been accounted for
  props[, bridge_prop := pop_mr/sum(pop_mr), by = 'fips,year,sex,age_start']
  props[, bridge_prop2 := pop_mr2/sum(pop_mr), by = 'fips,year,age_start']
  props[, bridge_prop3 := pop_mr3/sum(pop_mr), by = 'fips,year,age_grp']
  props[, bridge_prop4 := pop_mr4/sum(pop_mr), by = 'fips,year']
  props[, bridge_prop5 := pop_mr5/sum(pop_mr), by = 'state,year']
  stopifnot(nrow(props[is.na(bridge_prop5)]) == 0)  # check that proportions exist everywhere
  props[, c("pop_mr", paste0("pop_mr", 2:5), "pop_bridged", "pop_alone") := NULL]
  
  # Apply proportions to specified data year ------------------------------------------------------
  dt_mr <- dt_to_bridge[year == data_year & race == 9]  # isolate MR individuals
  pop_mr_pre_bridge <- sum(dt_mr$pop_alone)  # verify total stays the same after bridging
  message("Pre-bridged MR population: ", pop_mr_pre_bridge)
  dt_mr[, race := NULL]
  
  # format and merge
  setnames(dt_mr, "pop_alone", "pop_mr")
  dt_mr[, age_grp := dplyr::case_when(age_start < 20 ~ "0-19", age_start %in% 20:35 ~ "20-39",
                                      age_start %in% 40:55 ~ "40-59", age_start %in% 60:75 ~ "60-79",
                                      age_start >= 80 ~ "80+", TRUE ~ NA_character_)]
  dt_bridged <- merge.data.table(dt_mr, props[, -"year"], all.x = T, by = c("fips", "sex", "age_start", "age_grp"))
  dt_bridged[, pop_bridged := ifelse(!is.na(bridge_prop), pop_mr * bridge_prop,
                                     ifelse(!is.na(bridge_prop2), pop_mr * bridge_prop2,
                                            ifelse(!is.na(bridge_prop3), pop_mr * bridge_prop3,
                                                   ifelse(!is.na(bridge_prop4), pop_mr * bridge_prop4,
                                                          ifelse(!is.na(bridge_prop5), pop_mr * bridge_prop5, NA)))))]
  stopifnot(nrow(dt_bridged[pop_bridged < 0 | is.na(pop_bridged)]) == 0)  # make sure bridged pop is within reasonable bounds and nonmissing
  stopifnot(sum(dt_bridged[race == 7, pop_bridged]) == 0)  # make sure no Hispanic population was bridged
  pop_mr_post_bridge <- sum(dt_bridged$pop_bridged)
  message("Post-bridged MR population: ", pop_mr_post_bridge)
  stopifnot(pop_mr_post_bridge == pop_mr_pre_bridge)  # check that the total MR population did not change
  rm(pop_mr_post_bridge, pop_mr_pre_bridge)
  
  # Aggregate and combine with non-MR population --------------------------------------------------
  dt_bridged <- dt_bridged[, list(pop_bridged = sum(pop_bridged)), by = 'fips,year,sex,age_start,race']
  dt_all <- dt_to_bridge[year == data_year & race != 9]
  dt_all <- merge.data.table(dt_all, dt_bridged, by = c("fips", "year", "sex", "age_start", "race"), all = T)
  dt_all[, pop_bridged := ifelse(is.na(pop_bridged), 0, pop_bridged)]  # cases where no pop was bridged will be NA in the merge; set to 0
  dt_all[, pop := pop_alone + pop_bridged]  # add bridged pop
  dt_all[, c("pop_alone", "pop_bridged") := NULL]
  
  # Check that population totals did not change and return bridged-race dt
  pop_now <- sum(dt_all$pop)
  stopifnot(pop_now == pop_orig)
  return(dt_all)
}