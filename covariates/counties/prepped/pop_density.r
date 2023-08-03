####################################################################################################
## Description: Prepare the "pop_density" covariate (population density - number people per sq km)
##              using census bureau and NCHS population estimates and the area of each county
##              extracted from the county shape file.
####################################################################################################

user <- Sys.info()[["user"]]
source("counties/prepped/_prep_workspace.r")
library(tigris)
library(sf)

# Load source metadata and extractions -------------------------------------------------------------
census_meta <- get_population_metadata("census_pop_est_by_race")
nchs_meta <- get_population_metadata("nchs_pop_est_all_races")
pop <- rbind(readRDS(paste0(pop_dir, "[FILEPATH]", unique(census_meta[, file])))[, -"nid"],
             readRDS(paste0(pop_dir, "[FILEPATH]", unique(nchs_meta[, file])))[, -"nid"], fill = T)
pop <- pop[, list(pop = sum(pop)), by = c("fips", "year")]
stopifnot(sum(is.na(pop)) == 0)

# Collapse to merged counties ----------------------------------------------------------------------
pop <- merge(pop, loc, by = "fips", all.x = T)
pop <- pop[, list(pop = sum(pop)), by = c("mcnty", "year")]
stopifnot(sum(is.na(pop)) == 0)

# Calculate land areas of merged counties ----------------------------------------------------------
county_areas <- setDT(tigris::counties(year = 2020))
county_areas <- county_areas[, list(fips = as.numeric(GEOID), area_sqkm = ALAND/1000000)]
county_areas <- county_areas[fips < 60000]  # drop territories
county_areas[loc[, .(fips, mcnty)], on = "fips", mcnty := i.mcnty]  # join mcnty info
mcnty_areas <- county_areas[, list(area_sqkm = sum(area_sqkm)), by = "mcnty"]  # collapse to mcnty

# Calculate population density ---------------------------------------------------------------------
pop <- merge(pop, mcnty_areas, by = "mcnty", all.x = T)
pop[, pop_density := pop / area_sqkm]
pop[, log_pop_density := log(pop_density)]
pop[, c("pop", "state", "state_name", "area_sqkm") := NULL]  # no longer need these

# Format and save output ---------------------------------------------------------------------------
# run checks
check_missingness(pop)
check_var(pop, "year")

# save RDS file
setkeyv(pop, c("mcnty", "year"))
out_dir_pop <- paste0(cov_dir, "[FILEPATH]")
out_dir_log_pop <- paste0(cov_dir, "[FILEPATH]")
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))  # name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
saveRDS(pop[, -"log_pop_density"], file = paste0(out_dir_pop, date_time_stamp, ".rds"))
saveRDS(pop[, -"pop_density"], file = paste0(out_dir_log_pop, date_time_stamp, ".rds"))

# upload data to db
prepped_cov_id_pop <- save_prepped_covariate(path = paste0(out_dir_pop, date_time_stamp, ".rds"),
                                             description = "Population density per square kilometer, 1980-2020",
                                             prev_issues = "None",
                                             sources = list(c("census_pop_est_by_race", unique(census_meta[, file])),
                                                            c("nchs_pop_est_all_races", unique(nchs_meta[, file]))),
                                             ignore_unmatched = T)
prepped_cov_id_log_pop <- save_prepped_covariate(path = paste0(out_dir_log_pop, date_time_stamp, ".rds"),
                                                 description = "Log of population density per square kilometer, 1980-2020",
                                                 prev_issues = "None",
                                                 sources = list(c("census_pop_est_by_race", unique(census_meta[, file])),
                                                                c("nchs_pop_est_all_races", unique(nchs_meta[, file]))),
                                                 ignore_unmatched = T)

# make diagnostic maps and plots
make_maps_and_plots(plot_data = pop[, -"log_pop_density"], var = "pop_density", geo_var = "mcnty", parent_dir = cov_dir,
                    choropleth_map = mcnty_map, outline_map = state_map, title = "Population Density (per sq km)",
                    ylim = c(0, 30000), yformat = "comma", make_maps = T, make_line_plots = T)
make_maps_and_plots(plot_data = pop[, -"pop_density"], var = "log_pop_density", geo_var = "mcnty", parent_dir = cov_dir,
                    choropleth_map = mcnty_map, outline_map = state_map, title = "Log Population Density (per sq km)",
                    ylim = c(-6, 12), yformat = "decimal1", make_maps = T, make_line_plots = T)
