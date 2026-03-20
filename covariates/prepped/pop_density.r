####################################################################################################
## Description: Prepare the "pop_density" covariate (population density - number people per sq km)
##              using census bureau and NCHS population estimates and the area of each merged county
##              extracted from aggregating a county shape file to merged counties.
####################################################################################################

user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")
source("_functions/_prep_workspace.r")
library(tigris)
library(sf)

# Load custom prepped population ------------------------------------------------------------------
# this population file contains our custom intercensal estimates for 2010-2020
pop <- get_population_data(population_name = "pop_by_race_ethn_1977", year = 1990:2022)
pop <- pop[, list(pop = sum(pop)), by = c("year", "mcnty")]

# get source info for above pop output to put in metadata for database
nchs_meta <- get_population_metadata("nchs_pop_est_by_race_ethn")
census_meta <- get_population_metadata("census_pop_est_by_race_ethn")

# Calculate land areas of merged counties ----------------------------------------------------------
county_areas <- setDT(tigris::counties(year = 2021))
county_areas <- county_areas[, list(fips = as.numeric(GEOID), area_sqkm = ALAND/1000000)]
county_areas <- county_areas[fips < 60000]  # drop territories
county_areas[loc[, .(fips, mcnty)], on = "fips", mcnty := i.mcnty]  # join mcnty info
mcnty_areas <- county_areas[, list(area_sqkm = sum(area_sqkm)), by = "mcnty"]  # collapse to mcnty

# Calculate population density ---------------------------------------------------------------------
pop <- merge(pop, mcnty_areas, by = "mcnty", all.x = T)
pop[, pop_density := pop / area_sqkm]
pop[, log_pop_density := log(pop_density)]
pop[, c("pop", "area_sqkm") := NULL]  # no longer need these

# Format and save output ---------------------------------------------------------------------------
# run checks
check_missingness(pop)
check_var(pop, "year")

# save RDS file
setkeyv(pop, c("mcnty", "year"))
out_dir_pop <- paste0(cov_dir, "prepped/pop_density/")
out_dir_log_pop <- paste0(cov_dir, "prepped/log_pop_density/")
date_time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")  # name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
saveRDS(pop[, -"log_pop_density"], file = paste0(out_dir_pop, date_time_stamp, ".rds"))
saveRDS(pop[, -"pop_density"], file = paste0(out_dir_log_pop, date_time_stamp, ".rds"))
