####################################################################################################
## Description: Prepare poverty rate by race/ethnicity using census 1980, 1990, and 2000 data and
##              ACS 2010-2019 5-year series, collapsing to 1977 OMB standard race groups (White, 
##              Black, AIAN, API, and Hispanic), and imputing to fill in intervening years.
##
## Note: currently, 'other race' and 'two or more races' are included in population counts for each
##       race group but excluded from covariate data since they do not fall into 1977 standard race
##       groups. Additionally, Black, AIAN, and API serve as proxies for NH Black, NH AIAN, and NH
##       API, respectively.
##
## Documentation on combining MOEs using variance: 
## https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch07.pdf
####################################################################################################

user <- Sys.info()[["user"]]
source("counties/prepped/_prep_workspace.r")

# INLA settings
pardiso_path <- NULL  # change this to your PARDISO license location, if you have one and want to use it
INLA:::inla.binary.install()  # use this to avoid error about linux version 'GLIBC_2.27' not found

# Prepare Census data ------------------------------------------------------------------------------
# load data and collapse to merged counties
census_meta <- get_covariate_metadata("census_poverty_by_race_ethn")
census_data <- readRDS(paste0(cov_dir, "[FILEPATH]", unique(census_meta[, file])))[, -"nid"]
census_data <- merge(census_data, loc, by = "fips", all.x = T)
census_data <- census_data[, list(poverty = sum(poverty), pop = sum(pop)), by = "mcnty,year,race_group"]
census_data[is.nan(poverty), poverty := NA]  # assign NA to NaNs that occur when data is missing across all counties in an mcnty
census_data[, moe := NA]  # for rbinding with acs data later

# combine NHOPI and Asian to get NH API (for 1977 OMB standard race groups)
api <- copy(census_data[race_group %in% c("NH NHOPI", "NH Asian")])
api <- api[, list(race_group = "NH API", poverty = sum(poverty), pop = sum(pop), moe = NA), by = 'mcnty,year']
census_data <- rbind(census_data, api); rm(api)
census_data <- census_data[race_group %in% c("Hispanic", "NH AIAN", "NH API", "NH Black", "NH White")]

# Prepare ACS data ---------------------------------------------------------------------------------
# load data, drop Other, Multiracial, and White combined, and collapse to merged counties
acs_meta <- get_covariate_metadata("acs_poverty_by_race_ethn")
acs_data <- readRDS(paste0(cov_dir, "[FILEPATH]", unique(acs_meta[, file])))[, -"nid"]
acs_data <- acs_data[!race_group %in% c("other", "multi", "white")]  # we have NH white already; drop other and multi for now
acs_data <- merge(acs_data, loc, by = "fips", all.x = T)
acs_data[, var := (moe / 1.645)^2]  # calculate variance from MOE
acs_data <- acs_data[, list(poverty = sum(poverty), var = sum(var), pop = sum(pop)), by = "mcnty,year,race_group"]
acs_data[, c("moe", "var") := list(1.645*sqrt(var), NULL)]  # convert back to MOE
acs_data[is.nan(poverty), poverty := NA]  # assign NA to NaNs that occur when data is missing across all counties in an mcnty
acs_data[is.nan(moe), moe := NA]

# combine Asian and NHOPI into API
api <- copy(acs_data[race_group %in% c("asian", "nhopi")])
api[, var := (moe / 1.645)^2]  # calculate variance from MOE
api <- api[, list(race_group = "api", poverty = sum(poverty), var = sum(var), pop = sum(pop)), by = 'mcnty,year']
api[, c("moe", "var") := list(1.645*sqrt(var), NULL)]  # convert back to MOE
acs_data <- rbind(acs_data, api); rm(api)
acs_data <- acs_data[race_group %in% c("hisp", "aian", "api", "black", "white_nh")]

# rename ACS race groups to their proxy groups (e.g., AIAN as a proxy for NH AIAN)
acs_data[, race_group := car::recode(race_group, "'aian'='NH AIAN'; 'api'='NH API'; 'black'='NH Black';
                                     'hisp'='Hispanic'; 'white_nh'='NH White'")]

# Combine Census and ACS data -----------------------------------------------------------------------
data <- rbindlist(list(census_data, acs_data), use.names = TRUE); rm(census_data, acs_data)

# Perform imputation --------------------------------------------------------------------------------
source("_functions/covariate_imputation_functions.R") # Source imputation functions
output_loc = paste0("[FILEPATH]", make_time_stamp(timestamp()), "/")
message(output_loc)
imputed <- impute_covariate(covar = data, covariate_name = "poverty_by_race_ethn", outcome_name = "poverty",
                            family = "binomial", save_plots = TRUE, output_loc = output_loc, lib_loc = rlibs_loc,
                            pardiso_path = pardiso_path, INLA_loc = rlibs_loc, load_saved_intermediate = FALSE,
                            states_to_model = NULL, years_to_model = 1990:2020)

# subset and rename columns
imputed <- imputed[, c("uid", "mcnty", "year", "race_group", "raw_poverty", "raw_pop", "mean_unscaled",
                       "sd", "median_unscaled", "lower_unscaled", "upper_unscaled")]
setnames(imputed, c("mean_unscaled", "median_unscaled", "lower_unscaled", "upper_unscaled"),
         c("poverty", "median", "lower", "upper"))

data <- copy(imputed[, c("mcnty", "year", "race_group", "poverty")])

# convert 1977 OMB race/ethnicity groups to standard IDs, add race set ID from ushd_shared.population_group_set table
data[, race := car::recode(race_group, "'Hispanic'=2; 'NH Black'=4; 'NH White'=5; 'NH AIAN'=6; 'NH API'=7; else=99")]
stopifnot(nrow(data[race == 99]) == 0)  # there should be no observations with missing/invalid race ID
setnames(data, "race_group", "race_label")
data[, race_set := pop_group_sets[population_group_set_name == "omb_1977", population_group_set_id]]
setcolorder(data, c("mcnty", "year", "race", "race_label", "race_set", "poverty"))

# Format and save output ----------------------------------------------------------------------------
# run checks
check_missingness(data)
check_var(data, "year")

# save RDS file
setkeyv(data, c("mcnty", "year"))
out_dir <- paste0(cov_dir, "[FILEPATH]")
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))  # name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
saveRDS(data, file = paste0(out_dir, date_time_stamp, ".rds"))

# upload data to db
prepped_cov_id <- save_prepped_covariate(path = paste0(out_dir, date_time_stamp, ".rds"),
                                         description = "Poverty rate by race/ethnicity, 1990-2020",
                                         prev_issues = "None",
                                         sources = list(c("census_poverty_by_race_ethn", unique(census_meta[, file])),
                                                        c("acs_poverty_by_race_ethn", unique(acs_meta[, file]))),
                                         ignore_unmatched = T)

# make diagnostic maps and plots
make_maps_and_plots(plot_data = data, var = "poverty", geo_var = "mcnty", parent_dir = cov_dir, by_race = T,
                    choropleth_map = mcnty_map, outline_map = state_map, title = "Poverty Rate",
                    ylim = c(0, 1), yformat = "decimal1", make_maps = T, make_line_plots = T)
