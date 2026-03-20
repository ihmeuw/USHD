####################################################################################################
## Description: Prepare percent foreign-born population by race/ethnicity using census 1980, 1990,
##              and 2000 data and ACS 2010-2022 5-year series, collapsing to 1977 OMB standard race
##              groups (White, Black, AIAN, API, and Hispanic), and imputing to fill in intervening
##              years.
##
## Documentation on combining MOEs using variance: 
## https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch07.pdf
####################################################################################################

user <- Sys.info()[["user"]]
setwd("FILEPATH")
source("_functions/_prep_workspace.r")

# Prepare Census data ------------------------------------------------------------------------------
# load data and collapse to merged counties
census_meta <- get_covariate_metadata("census_foreign_born_by_race_ethn")
census_data <- readRDS(paste0(cov_dir, "raw/census_foreign_born_by_race_ethn/", unique(census_meta[, file])))[, -"nid"]
census_data <- merge(census_data, loc, by = "fips", all.x = T)
census_data <- census_data[, list(foreign_born = sum(foreign_born), pop = sum(pop)), by = "mcnty,year,race_group"]
census_data[is.nan(foreign_born), foreign_born := NA]  # assign NA to NaNs that occur when data is missing across all counties in an mcnty

# combine NHOPI and Asian to get NH API (for 1977 OMB standard race groups)
api <- copy(census_data[race_group %in% c("NH NHOPI", "NH Asian")])
api <- api[, list(race_group = "NH API", foreign_born = sum(foreign_born), pop = sum(pop), moe = NA), by = 'mcnty,year']
census_data <- rbindlist(list(census_data, api), use.names = T, fill = T); rm(api)
census_data <- census_data[race_group %in% c("Hispanic", "NH AIAN", "NH API", "NH Black", "NH White")]

# Prepare ACS data ---------------------------------------------------------------------------------
# load data, drop Other, Multiracial, and White combined, and collapse to merged counties
acs_meta <- get_covariate_metadata("acs_foreign_born_by_race_ethn")
acs_data <- readRDS(paste0(cov_dir, "raw/acs_foreign_born_by_race_ethn/", unique(acs_meta[, file])))[, -"nid"]
acs_data <- acs_data[!race_group %in% c("other", "multi", "white")]  # we have NH white already; drop other and multi for now
acs_data <- merge(acs_data, loc, by = "fips", all.x = T)
acs_data[, c("var", "var_pop") := list((moe / 1.645)^2, (moe_pop / 1.645)^2)]  # calculate variance from MOE
acs_data <- acs_data[, list(foreign_born = sum(foreign_born), var = sum(var), pop = sum(pop), var_pop = sum(var_pop)),
                     by = "mcnty,year,race_group"]
acs_data[, c("moe", "moe_pop", "var", "var_pop") := list(1.645*sqrt(var), 1.645*sqrt(var_pop), NULL, NULL)]  # convert back to MOE
acs_data[is.nan(foreign_born), foreign_born := NA]  # assign NA to NaNs that occur when data is missing across all counties in an mcnty
acs_data[is.nan(moe), moe := NA]

# combine Asian and NHOPI into API
api <- copy(acs_data[race_group %in% c("asian", "nhopi")])
api[, c("var", "var_pop") := list((moe / 1.645)^2, (moe_pop / 1.645)^2)]  # calculate variance from MOE
api <- api[, list(race_group = "api", foreign_born = sum(foreign_born), var = sum(var), pop = sum(pop), var_pop = sum(var_pop)),
           by = 'mcnty,year']
api[, c("moe", "moe_pop", "var", "var_pop") := list(1.645*sqrt(var), 1.645*sqrt(var_pop), NULL, NULL)]  # convert back to MOE
acs_data <- rbind(acs_data, api); rm(api)
acs_data <- acs_data[race_group %in% c("hisp", "aian", "api", "black", "white_nh")]

# rename ACS race groups to their proxy groups (e.g., AIAN as a proxy for NH AIAN)
acs_data[, race_group := car::recode(race_group, "'aian'='NH AIAN'; 'api'='NH API'; 'black'='NH Black';
                                     'hisp'='Hispanic'; 'white_nh'='NH White'")]

# Correct MOEs for population MOEs ----------------------------------------------------------------
# initial adjustment
acs_data[, moe_corrected := sqrt(moe^2 - ((foreign_born / pop)^2 * (moe_pop^2)))]

# use formula for ratios when moe^2 + ((fb / pop)^2 * (moe_pop^2)) is negative, per ACS guidance: see page 56 in https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch08.pdf
acs_data[is.nan(moe_corrected), moe_corrected := sqrt(moe^2 + ((foreign_born / pop)^2 * (moe_pop^2)))]

# use reported moe when pop == 0
acs_data[is.nan(moe_corrected), moe_corrected := moe]

# replace reported MOEs with corrected MOEs
acs_data[, c("moe", "moe_pop", "moe_corrected") := list(moe_corrected, NULL, NULL)]

# Combine Census and ACS data -----------------------------------------------------------------------
data <- rbindlist(list(census_data, acs_data), use.names = TRUE, fill = T); rm(census_data, acs_data)

# Perform imputation --------------------------------------------------------------------------------
source("_functions/covariate_imputation_functions.R") # Source imputation functions

date_time_stamp <- make_time_stamp(timestamp())
output_loc_to_pass = "FILEPATH"
message(output_loc_to_pass)
pardiso_path <- NULL  # change this to your PARDISO license location, if you have one and want to use it
imputed <- impute_covariate(covar = data, covariate_name = "foreign_born_by_race_ethn", outcome_name = "foreign_born",
                            family = "binomial", save_plots = TRUE, output_loc = output_loc_to_pass, lib_loc = rlibs_loc,
                            pardiso_path = pardiso_path, INLA_loc = rlibs_loc, load_saved_intermediate = FALSE,
                            states_to_model = NULL, years_to_model = 2000:max(interp_years))

# subset and rename columns
imputed <- imputed[, c("uid", "mcnty", "year", "race_group", "raw_foreign_born", "raw_pop", "mean_unscaled",
                       "sd", "median_unscaled", "lower_unscaled", "upper_unscaled")]
setnames(imputed, c("mean_unscaled", "median_unscaled", "lower_unscaled", "upper_unscaled"),
         c("foreign_born", "median", "lower", "upper"))

data <- copy(imputed[, c("mcnty", "year", "race_group", "foreign_born")])

# convert 1977 OMB race/ethnicity groups to standard IDs, add race set ID from ushd_shared.population_group_set table
data[, race := car::recode(race_group, "'Hispanic'=2; 'NH Black'=4; 'NH White'=5; 'NH AIAN'=6; 'NH API'=7; else=99")]
stopifnot(nrow(data[race == 99]) == 0)  # there should be no observations with missing/invalid race ID
setnames(data, "race_group", "race_label")
data[, race_set := pop_group_sets[population_group_set_name == "omb_1977", population_group_set_id]]
setcolorder(data, c("mcnty", "year", "race", "race_label", "race_set", "foreign_born"))

# Format and save output ----------------------------------------------------------------------------
# run checks
check_missingness(data)
check_var(data, "year")

# save RDS file
setkeyv(data, c("mcnty", "year"))
out_dir <- paste0(cov_dir, "prepped/foreign_born_by_race_ethn/")
saveRDS(data, file = paste0(out_dir, date_time_stamp, ".rds"))

# also save a version where prevalence is capped between 0 and 1
data[foreign_born > 1.0, foreign_born := 1.0]
data[foreign_born < 0.0, foreign_born := 0.0]
saveRDS(data, file = paste0(out_dir, date_time_stamp, "_capped_0_1.rds"))