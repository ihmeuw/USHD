####################################################################################################
## Description: Prepare poverty rate by race/ethnicity using census 1980, 1990, and 2000 data and
##              ACS 2010-2022 5-year series, collapsing to 1977 OMB standard race groups (White, 
##              Black, AIAN, API, and Hispanic), and imputing to fill in intervening years.
##
## Documentation on combining MOEs using variance: 
## https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch07.pdf
####################################################################################################

user <- Sys.info()[["user"]]
setwd("FILEPATH")
source("_functions/_prep_workspace.r")

# Prepare Census data ------------------------------------------------------------------------------
# load data and collapse to merged counties
census_meta <- get_covariate_metadata("census_poverty_by_race_ethn")
census_data <- readRDS(paste0(cov_dir, "raw/census_poverty_by_race_ethn/", unique(census_meta[, file])))[, -"nid"]
census_data <- merge(census_data, loc, by = "fips", all.x = T)
census_data <- census_data[, list(poverty = sum(poverty), pop = sum(pop)), by = "mcnty,year,race_group"]
census_data[is.nan(poverty), poverty := NA]  # assign NA to NaNs that occur when data is missing across all counties in an mcnty
census_data[, c("moe", "moe_pop") := NA]  # for rbinding with acs data later

# combine NHOPI and Asian to get NH API (for 1977 OMB standard race groups)
api <- copy(census_data[race_group %in% c("NH NHOPI", "NH Asian")])
api <- api[, list(race_group = "NH API", poverty = sum(poverty), pop = sum(pop), moe = NA, moe_pop = NA), by = 'mcnty,year']
census_data[, c("moe") := NA]  # for rbinding with acs data later

# combine NHOPI and Asian to get NH API (for 1977 OMB standard race groups)
api <- copy(census_data[race_group %in% c("NH NHOPI", "NH Asian")])
api <- api[, list(race_group = "NH API", poverty = sum(poverty), 
                  pop = sum(pop), moe = NA, moe_pop = NA), by = 'mcnty,year']
census_data <- rbind(census_data, api); rm(api)
census_data <- census_data[race_group %in% c("Hispanic", "NH AIAN", "NH API", "NH Black", "NH White")]

# Prepare ACS data ---------------------------------------------------------------------------------
# load data, drop Other, Multiracial, and White combined, and collapse to merged counties
acs_meta <- get_covariate_metadata("acs_poverty_by_race_ethn")
acs_data <- readRDS(paste0(cov_dir, "raw/acs_poverty_by_race_ethn/", unique(acs_meta[, file])))[, -"nid"]
acs_data <- acs_data[!race_group %in% c("other", "multi", "white")]  # we have NH white already; drop other and multi for now
acs_data <- merge(acs_data, loc, by = "fips", all.x = T)
acs_data <- rename(acs_data, moe = moe, moe_pop = pop_moe)
acs_data[, c("var_poverty", "var_pop") := list((moe / 1.645)^2, (moe_pop/1.645)^2)]  # calculate variance from MOE
acs_data <- acs_data[, list(poverty = sum(poverty), var_poverty = sum(var_poverty), 
                            var_pop = sum(var_pop), pop = sum(pop)), by = "mcnty,year,race_group"]
acs_data[, c("moe", "moe_pop", "var_poverty", "var_pop") := list(1.645*sqrt(var_poverty), 1.645*sqrt(var_pop), NULL, NULL)]  # convert back to MOE

#### Correct for population MOEs
## Initial adjustment
acs_data[, c("poverty", "pop", "moe", "moe_pop") := list(as.integer(poverty), as.integer(pop), as.integer(moe), as.integer(moe_pop))]
acs_data[, moe_corrected := sqrt(moe^2 - ((poverty^2 * moe_pop^2 / pop^2)))]

## Use formula for ratios when moe^2 + ((poverty / pop)^2 * (moe_pop^2)) is negative, per ACS guidance: see page 56 in https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch08.pdf
acs_data[is.nan(moe_corrected), moe_corrected := sqrt(moe^2 + ((poverty^2 * moe_pop^2 / pop^2)))]
acs_data[moe_corrected == 0, moe_corrected := sqrt(moe^2 + ((poverty^2 * moe_pop^2 / pop^2)))]

## Use reported moe when pop == 0
acs_data[is.nan(moe_corrected), moe_corrected := moe]

## Replace reported MOEs with corrected MOEs
acs_data[, c("moe", "moe_corrected") := list(moe_corrected, NULL)]

## Combine groups
acs_data[, c("var_poverty", "var_pop") := list((moe / 1.645)^2, (moe_pop/1.645)^2)]  # calculate variance from MOE
acs_data <- acs_data[, list(poverty = sum(poverty), var_poverty = sum(var_poverty), 
                            pop = sum(pop), var_pop = sum(var_pop)), by = "mcnty,year,race_group"]
acs_data[, c("moe", "moe_pop", "var_poverty", "var_pop") := list(1.645*sqrt(var_poverty), 1.645*sqrt(var_pop), NULL, NULL)]  # convert back to MOE
acs_data[is.nan(poverty), poverty := NA]  # assign NA to NaNs that occur when data is missing across all counties in an mcnty
acs_data[is.nan(moe), moe := NA]

# combine Asian and NHOPI into API
api <- copy(acs_data[race_group %in% c("asian", "nhopi")])
api[, c("var_poverty", "var_pop") := list((moe / 1.645)^2, (moe_pop / 1.645)^2)]  # calculate variance from MOE
api <- api[, list(race_group = "api", poverty = sum(poverty), var_poverty = sum(var_poverty), 
                  var_pop = sum(var_pop), pop = sum(pop)), by = 'mcnty,year']
api[, c("moe", "moe_pop", "var_poverty", "var_pop") := list(1.645*sqrt(var_poverty), 1.645*sqrt(var_pop), NULL, NULL)]  # convert back to MOE
api[, c("var_poverty", "var_pop") := list((moe / 1.645)^2, (moe_pop / 1.645)^2)]  # calculate variance from MOE
api <- api[, list(race_group = "api", poverty = sum(poverty), var_poverty = sum(var_poverty), 
                  var_pop = sum(var_pop), pop = sum(pop)), by = 'mcnty,year']
api[, c("moe", "moe_pop", "var_poverty", "var_pop") := list(1.645*sqrt(var_poverty), 1.645*sqrt(var_pop), NULL, NULL)]  # convert back to MOE
acs_data <- rbind(acs_data, api); rm(api)
acs_data <- acs_data[race_group %in% c("hisp", "aian", "api", "black", "white_nh")]

# rename ACS race groups to their proxy groups (e.g., AIAN as a proxy for NH AIAN)
acs_data[, race_group := car::recode(race_group, "'aian'='NH AIAN'; 'api'='NH API'; 'black'='NH Black';
                                     'hisp'='Hispanic'; 'white_nh'='NH White'")]

#### Correct edu MOEs for population MOEs
## Initial adjustment
acs_data[, moe_corrected := sqrt(moe^2 - ((poverty / pop)^2 * (moe_pop^2)))]

## Use formula for ratios when moe^2 + ((poverty / pop)^2 * (moe_pop^2)) is negative, per ACS guidance: see page 56 in https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch08.pdf
acs_data[is.nan(moe_corrected), moe_corrected := sqrt(moe^2 + ((poverty / pop)^2 * (moe_pop^2)))]

## Use reported moe when pop == 0
acs_data[is.nan(moe_corrected), moe_corrected := moe]

## Replace reported MOEs with corrected MOEs
acs_data[, c("moe", "moe_pop", "moe_corrected") := list(moe_corrected, moe_pop, NULL)] # should we do moe_pop := NULL instead of moe_pop?
# Drop rows with NA outcomes and NA pops
acs_data <- acs_data[!is.na(poverty) & !is.na(pop)]

# Combine Census and ACS data -----------------------------------------------------------------------
data <- rbindlist(list(census_data, acs_data), use.names = TRUE); rm(census_data, acs_data)

# Perform imputation --------------------------------------------------------------------------------
source("_functions/covariate_imputation_functions.R") # Source imputation functions
date_time_stamp <- make_time_stamp(timestamp())
output_loc = "FILEPATH"
message(output_loc)
pardiso_path <- NULL  # change this to your PARDISO license location, if you have one and want to use it
imputed <- impute_covariate(covar = data, covariate_name = "poverty_by_race_ethn", outcome_name = "poverty",
                            family = "binomial", save_plots = TRUE, output_loc = output_loc, lib_loc = rlibs_loc,
                            pardiso_path = pardiso_path, INLA_loc = rlibs_loc, load_saved_intermediate = FALSE,
                            states_to_model = NULL, years_to_model = 1990:max(interp_years))

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
out_dir <- paste0(cov_dir, "prepped/poverty_by_race_ethn/")
saveRDS(data, file = paste0(out_dir, date_time_stamp, ".rds"))