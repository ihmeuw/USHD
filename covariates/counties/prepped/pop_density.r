####################################################################################################
## Description: Prepare the "pop_density" covariate (population density - number people per sq km)
##              using census bureau and NCHS population estimates and the area of each county
##              extracted from the county shape file.
####################################################################################################

user <- Sys.info()[["user"]]
source("counties/prepped/_prep_workspace.r")

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

# Calculate areas of merged counties ---------------------------------------------------------------
shp <- readRDS(paste0(root,"[FILEPATH]"))
newProjString <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
shp <- spTransform(shp, CRS(newProjString))  # projecting to Albers equal area conic
shp@data$area_sqkm <- rgeos::gArea(shp, byid = T) / 1000000  # units of Albers equal area conic is meters, so this should give us km

# Calculate population density ---------------------------------------------------------------------
pop <- merge(pop, shp@data, by = "mcnty", all = T)
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
                                             description = "Population density per square kilometer",
                                             prev_issues = "None",
                                             sources = list(c("census_pop_est_by_race", unique(census_meta[, file])),
                                                            c("nchs_pop_est_all_races", unique(nchs_meta[, file]))),
                                             ignore_unmatched = T)
prepped_cov_id_log_pop <- save_prepped_covariate(path = paste0(out_dir_log_pop, date_time_stamp, ".rds"),
                                                 description = "Log of population density per square kilometer",
                                                 prev_issues = "None",
                                                 sources = list(c("census_pop_est_by_race", unique(census_meta[, file])),
                                                                c("nchs_pop_est_all_races", unique(nchs_meta[, file]))),
                                                 ignore_unmatched = T)

# make diagnostic maps and plots
make_maps_and_plots(plot_data = pop[, -"log_pop_density"], var = "pop_density", geo_var = "mcnty", parent_dir = cov_dir,
                    choropleth_map = mcnty_map, outline_map = state_map, title = "Population Density (per sq km)",
                    ylim = c(0, 30000), yformat = "comma", make_maps = F, make_line_plots = F)
make_maps_and_plots(plot_data = pop[, -"pop_density"], var = "log_pop_density", geo_var = "mcnty", parent_dir = cov_dir,
                    choropleth_map = mcnty_map, outline_map = state_map, title = "Log Population Density (per sq km)",
                    ylim = c(-6, 12), yformat = "decimal1", make_maps = F, make_line_plots = F)
