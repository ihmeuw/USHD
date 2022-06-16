####################################################################################################
## Description: Prepare educational attainment by race/ethnicity using census 1980, 1990, and 2000
##              data and ACS 2010-2019 5-year series, collapsing to 1977 OMB standard race groups
##              (White, Black, AIAN, API, and Hispanic), and imputing to fill in intervening years.
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
census_meta <- get_covariate_metadata("census_education_by_race_ethn")
census_data <- readRDS(paste0(cov_dir, "[FILEPATH]", unique(census_meta[, file])))[, -"nid"]
census_data <- merge(census_data, loc, by = "fips", all.x = T)
census_data <- census_data[, list(edu_hs = sum(edu_hs), edu_ba = sum(edu_ba), pop = sum(pop)), by = "mcnty,year,race_group"]
census_data[, c("edu_hs", "edu_ba") := list(ifelse(is.nan(edu_hs), NA, edu_hs), ifelse(is.nan(edu_ba), NA, edu_ba))]  # assign NA to NaNs that occur when data is missing across all counties in an mcnty
census_data[, c("moe_hs", "moe_ba") := NA]  # for rbinding with acs data later

# combine NHOPI and Asian to get NH API (for 1977 OMB standard race groups)
api <- copy(census_data[race_group %in% c("NH NHOPI", "NH Asian")])
api <- api[, list(race_group = "NH API", edu_hs = sum(edu_hs), edu_ba = sum(edu_ba), pop = sum(pop),
                  moe_hs = NA, moe_ba = NA), by = 'mcnty,year']
census_data <- rbind(census_data, api); rm(api)
census_data <- census_data[race_group %in% c("Hispanic", "NH AIAN", "NH API", "NH Black", "NH White")]

# Prepare ACS data ---------------------------------------------------------------------------------
# load data, drop Other, Multiracial, and White combined, and collapse to merged counties
acs_meta <- get_covariate_metadata("acs_education_by_race_ethn")
acs_data <- readRDS(paste0(cov_dir, "[FILEPATH]", unique(acs_meta[, file])))[, -"nid"]
acs_data <- acs_data[!race_group %in% c("other", "multi", "white")]
acs_data <- merge(acs_data, loc, by = "fips", all.x = T)
acs_data[, c("var_hs", "var_ba") := list((moe_hs / 1.645)^2, (moe_ba / 1.645)^2)]  # calculate variance from MOE
acs_data <- acs_data[, list(edu_hs = sum(edu_hs), var_hs = sum(var_hs), edu_ba = sum(edu_ba),
                            var_ba = sum(var_ba), pop = sum(pop)), by = "mcnty,year,race_group"]
acs_data[, c("moe_hs", "moe_ba", "var_hs", "var_ba") := list(1.645*sqrt(var_hs), 1.645*sqrt(var_ba), NULL, NULL)]  # convert back to MOE
acs_data[, c("edu_hs", "edu_ba") := list(ifelse(is.nan(edu_hs), NA, edu_hs), ifelse(is.nan(edu_ba), NA, edu_ba))]  # assign NA to NaNs that occur when data is missing across all counties in an mcnty
acs_data[, c("moe_hs", "moe_ba") := list(ifelse(is.nan(moe_hs), NA, moe_hs), ifelse(is.nan(moe_ba), NA, moe_ba))]

# combine Asian and NHOPI into API
api <- copy(acs_data[race_group %in% c("asian", "nhopi")])
api[, c("var_hs", "var_ba") := list((moe_hs / 1.645)^2, (moe_ba / 1.645)^2)]  # calculate variance from MOE
api <- api[, list(race_group = "api", edu_hs = sum(edu_hs), var_hs = sum(var_hs), edu_ba = sum(edu_ba),
                  var_ba = sum(var_ba), pop = sum(pop)), by = 'mcnty,year']
api[, c("moe_hs", "moe_ba", "var_hs", "var_ba") := list(1.645*sqrt(var_hs), 1.645*sqrt(var_ba), NULL, NULL)]  # convert back to MOE
acs_data <- rbind(acs_data, api); rm(api)
acs_data <- acs_data[race_group %in% c("hisp", "aian", "api", "black", "white_nh")]

# rename ACS race groups to their proxy groups (e.g., AIAN as a proxy for NH AIAN)
acs_data[, race_group := car::recode(race_group, "'aian'='NH AIAN'; 'api'='NH API'; 'black'='NH Black';
                                     'hisp'='Hispanic'; 'white_nh'='NH White'")]

# Combine Census and ACS data, split into edu_hs and edu_ba ----------------------------------------
data <- rbindlist(list(census_data, acs_data), use.names = TRUE); rm(census_data, acs_data)
data_hs <- copy(data[, list(mcnty, year, race_group, edu_hs, pop, moe_hs)])
data_ba <- copy(data[, list(mcnty, year, race_group, edu_ba, pop, moe_ba)])

# Perform imputation -------------------------------------------------------------------------------
source("_functions/covariate_imputation_functions.R") # Source imputation functions

# First impute education attainment (HS)
output_loc_to_pass = paste0("[FILEPATH]", make_time_stamp(timestamp()), "/")
message(output_loc_to_pass)
imputed_hs <- impute_covariate(covar = data_hs, covariate_name = "edu_hs_by_race_ethn", outcome_name = "edu_hs",
                               family = "binomial", save_plots = TRUE, output_loc = output_loc_to_pass, lib_loc = rlibs_loc,
                               pardiso_path = pardiso_path, INLA_loc = rlibs_loc, load_saved_intermediate = FALSE,
                               states_to_model = NULL, years_to_model = 1990:2019)

## Subset and rename columns
imputed_hs <- imputed_hs[, c("uid", "mcnty", "year", "race_group", "raw_edu_hs", "raw_pop", "mean_unscaled",
                             "sd", "median_unscaled", "lower_unscaled", "upper_unscaled")]
setnames(imputed_hs, c("mean_unscaled", "median_unscaled", "lower_unscaled", "upper_unscaled"),
         c("edu_hs", "median", "lower", "upper"))

data_hs <- copy(imputed_hs[, c("mcnty", "year", "race_group", "edu_hs")])

# convert 1977 OMB race/ethnicity groups to standard IDs and labels
data_hs[, race_1977_id := car::recode(race_group, "'Hispanic'=2; 'NH Black'=4; 'NH White'=5; 'NH AIAN'=6; 'NH API'=7; else=99")]
stopifnot(nrow(data_hs[race_1977_id == 99]) == 0)  # there should be no observations with missing/invalid race ID
setnames(data_hs, "race_group", "race_1977_label")
setcolorder(data_hs, c("mcnty", "year", "race_1977_id", "race_1977_label", "edu_hs"))

# Then impute education attainment (BA)
output_loc_to_pass = paste0("[FILEPATH]", make_time_stamp(timestamp()), "/")
message(output_loc_to_pass)
imputed_ba <- impute_covariate(covar = data_ba, covariate_name = "edu_ba_by_race_ethn", outcome_name = "edu_ba",
                               family = "binomial", save_plots = TRUE, output_loc = output_loc_to_pass, lib_loc = rlibs_loc,
                               pardiso_path = pardiso_path, INLA_loc = rlibs_loc, load_saved_intermediate = FALSE,
                               states_to_model = NULL, years_to_model = 1990:2019)

## Subset and rename columns
imputed_ba <- imputed_ba[, c("uid", "mcnty", "year", "race_group", "raw_edu_ba", "raw_pop", "mean_unscaled",
                             "sd", "median_unscaled", "lower_unscaled", "upper_unscaled")]
setnames(imputed_ba, c("mean_unscaled", "median_unscaled", "lower_unscaled", "upper_unscaled"),
         c("edu_ba", "median", "lower", "upper"))

data_ba <- copy(imputed_ba[, c("mcnty", "year", "race_group", "edu_ba")])

# convert 1977 OMB race/ethnicity groups to standard IDs and labels
data_ba[, race_1977_id := car::recode(race_group, "'Hispanic'=2; 'NH Black'=4; 'NH White'=5; 'NH AIAN'=6; 'NH API'=7; else=99")]
stopifnot(nrow(data_ba[race_1977_id == 99]) == 0)  # there should be no observations with missing/invalid race ID
setnames(data_ba, "race_group", "race_1977_label")
setcolorder(data_ba, c("mcnty", "year", "race_1977_id", "race_1977_label", "edu_ba"))

# Format and save output ----------------------------------------------------------------------------
# run checks
check_missingness(data_hs)
check_missingness(data_ba)
check_var(data_hs, "year")
check_var(data_ba, "year")

# save RDS file
setkeyv(data_hs, c("mcnty", "year"))
setkeyv(data_ba, c("mcnty", "year"))
out_dir_hs <- paste0(cov_dir, "[FILEPATH]")
out_dir_ba <- paste0(cov_dir, "[FILEPATH]")
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))  # name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
saveRDS(data_hs, file = paste0(out_dir_hs, date_time_stamp, ".rds"))
saveRDS(data_ba, file = paste0(out_dir_ba, date_time_stamp, ".rds"))

# upload data to db
prepped_cov_id_hs <- save_prepped_covariate(path = paste0(out_dir_hs, date_time_stamp, ".rds"),
                                            description = "HS+ educational attainment by race/ethnicity",
                                            prev_issues = "None",
                                            sources = list(c("census_education_by_race_ethn", unique(census_meta[, file])),
                                                           c("acs_education_by_race_ethn", unique(acs_meta[, file]))),
                                            ignore_unmatched = T)
prepped_cov_id_ba <- save_prepped_covariate(path = paste0(out_dir_ba, date_time_stamp, ".rds"),
                                            description = "BA+ educational attainment by race/ethnicity",
                                            prev_issues = "None",
                                            sources = list(c("census_education_by_race_ethn", unique(census_meta[, file])),
                                                           c("acs_education_by_race_ethn", unique(acs_meta[, file]))),
                                            ignore_unmatched = T)

# make diagnostic maps and plots
make_maps_and_plots(plot_data = data_hs, var = "edu_hs", geo_var = "mcnty", parent_dir = cov_dir, by_race = T,
                    choropleth_map = mcnty_map, outline_map = state_map, title = "HS+ Educational Attainment, Population 25+",
                    ylim = c(0, 1), yformat = "decimal1", make_maps = T, make_line_plots = T)
make_maps_and_plots(plot_data = data_ba, var = "edu_ba", geo_var = "mcnty", parent_dir = cov_dir, by_race = T,
                    choropleth_map = mcnty_map, outline_map = state_map, title = "BA+ Educational Attainment, Population 25+",
                    ylim = c(0, 1), yformat = "decimal1", make_maps = T, make_line_plots = T)
