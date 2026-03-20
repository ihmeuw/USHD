####################################################################################################
## Description: Prepare income per capita by race/ethnicity of householder using census 1980,
##              1990, and 2000 data and ACS 2009-2022 5-year series, adjusting for inflation, and
##              imputing to fill in intervening years.
##
## Documentation on combining MOEs using variance: 
## https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch07.pdf
####################################################################################################

user <- Sys.info()[["user"]]
setwd("FILEPATH")
source("_functions/_prep_workspace.r")

# Load and collapse population by race/ethnicity for weighted means -------------------------------
pop_weights_census <- load_census_pop_weights()  # get weights for collapsing Census data to mcnty
pop_weights_acs <- get_population_data(population_name = "pop_by_race_ethn_1997")

# Prepare Census data ------------------------------------------------------------------------------
census_meta <- get_covariate_metadata("census_income_pc_by_race_ethn")
census_data <- readRDS(paste0(cov_dir, "raw/census_income_pc_by_race_ethn/", unique(census_meta[, file])))[, -"nid"]
census_data <- merge(census_data, loc, by = "fips", all.x = T)
census_data <- merge(census_data, pop_weights_census, by = c("mcnty", "year", "race_group"), all.x = T)
census_data <- census_data[, list(income_pc = weighted.mean(income_pc, wt, na.rm = T), wt = sum(wt)),
                           by = "mcnty,year,infl_year,race_group"]
census_data[is.nan(income_pc), income_pc := NA]  # assign NA to NaNs that occur when data is missing across all counties in an mcnty
census_data[, moe := NA]  # for rbinding with acs data later
rm(pop_weights_census)

# combine NHOPI and Asian to get NH API (for 1977 OMB standard race groups)
api <- copy(census_data[race_group %in% c("NH NHOPI", "NH Asian")])
api <- api[, list(race_group = "NH API", income_pc = weighted.mean(income_pc, wt, na.rm = T),
                  wt = sum(wt, na.rm = TRUE), moe = NA), by = 'mcnty,year,infl_year']
api[is.nan(income_pc), income_pc := NA]  # assign NA to NaNs that occur when data is missing across all counties in an mcnty
census_data <- rbind(census_data, api); rm(api)
census_data[, wt := NULL]
census_data <- census_data[race_group %in% c("Hispanic", "NH AIAN", "NH API", "NH Black", "NH White")]

# Prepare ACS data ---------------------------------------------------------------------------------
# load data, drop Other, Multiracial, and White combined, and collapse to merged counties
acs_meta <- get_covariate_metadata("acs_income_pc_by_race_ethn")
acs_data <- readRDS(paste0(cov_dir, "raw/acs_income_pc_by_race_ethn/", unique(acs_meta[, file])))[, -"nid"]
acs_data <- acs_data[!race_group %in% c("other", "multi", "white")]
acs_data <- merge(acs_data, loc, by = "fips", all.x = T)
acs_data[, var := (moe / 1.645)^2]  # calculate variance from MOE

# Recode population weights to match data
pop_weights_acs[, race_group := car::recode(race, "12='aian'; 13='asian'; 14='nhopi'; 11='black';
                                     2='hisp'; 10='white_nh'; 15='multi'")]
setnames(pop_weights_acs, "pop", "wt")
pop_weights_acs <- pop_weights_acs[, list(wt = sum(wt)), by = c("year", "mcnty", "race_group")]

acs_data <- merge(acs_data, pop_weights_acs, by = c("mcnty", "year", "race_group"), all.x = T)
# acs_data <- acs_data[, list(income_pc = weighted.mean(income_pc, wt, na.rm = T),
acs_data <- acs_data[, list(income_pc = weighted.mean(income_pc, wt, na.rm = T),
                            var = sum(var * (wt / sum(wt))^2, na.rm = TRUE), wt = sum(wt)), by = "mcnty,year,infl_year,race_group"]
acs_data[, c("moe", "var") := list(1.645 * sqrt(var), NULL)]  # convert back to MOE
acs_data[is.nan(income_pc), income_pc := NA]  # assign NA to NaNs that occur when data is missing across all counties in an mcnty
acs_data[is.nan(moe) | moe == 0, moe := NA]

# combine Asian and NHOPI into API
api <- copy(acs_data[race_group %in% c("asian", "nhopi")])
api[, var := (moe / 1.645)^2]  # calculate variance from MOE
api <- api[, list(race_group = "api", income_pc = weighted.mean(income_pc, wt, na.rm = T), wt = sum(wt), var = sum(var * (wt / sum(wt))^2, na.rm = TRUE)),
           by = 'mcnty,year,infl_year']
api[, c("moe", "var") := list(1.645 * sqrt(var), NULL)]  # convert back to MOE
api[is.nan(income_pc), income_pc := NA]  # assign NA to NaNs that occur when data is missing across all counties in an mcnty
api[is.nan(moe) | moe == 0, moe := NA]

acs_data <- rbind(acs_data, api); rm(api)
acs_data <- acs_data[race_group %in% c("hisp", "aian", "api", "black", "white_nh")]
acs_data <- acs_data[, wt := NULL]

# rename ACS race groups to their proxy groups (e.g., AIAN as a proxy for NH AIAN)
acs_data[, race_group := car::recode(race_group, "'aian'='NH AIAN'; 'api'='NH API'; 'black'='NH Black';
                                     'hisp'='Hispanic'; 'white_nh'='NH White'")]

# Combine Census and ACS data -----------------------------------------------------------------------
data <- rbindlist(list(census_data, acs_data), use.names = TRUE); rm(census_data, acs_data)

## Load CPI and adjust for inflation ---------------------------------------------------------------
bls_meta <- get_covariate_metadata("bls_cpi")
cpi <- readRDS(paste0(cov_dir, "raw/bls_cpi/", unique(bls_meta[, file])))[, -"nid"]
cpi$infl <- cpi[year == max(data$year) + 2, cpi] / cpi$cpi  # inflation year for ACS is the LAST year of the 5-year series (e.g., 2022 for 2018-2022 series)
setnames(cpi, "year", "infl_year")
cpi[, cpi := NULL]

data <- merge(data, cpi, by = "infl_year", all.x = T)
data[, income_pc := income_pc * infl]
data[, c("infl", "infl_year") := NULL]

# Merge populations in order to estimate effective sample sizes for decennial Census data
pops <- get_population_data(population_name = "pop_by_race_ethn_1977")
pops_collapsed <- as.data.table(aggregate(pop ~ mcnty + year + race, data = pops, sum))
setnames(pops_collapsed, "race", "code")
race_codes <- data.table("code" = c(5, 4, 6, 7, 2), "race_group" = c("NH White", "NH Black", "NH AIAN", "NH API", "Hispanic"))
pops_collapsed <- as.data.table(merge(pops_collapsed, race_codes, all.x = TRUE))
pops_collapsed[, code := NULL]
data <- merge(data, pops_collapsed, by = c("mcnty", "year", "race_group"), all.x = TRUE)

# Perform imputation --------------------------------------------------------------------------------
source("_functions/covariate_imputation_functions.R") # Source imputation functions
date_time_stamp <- make_time_stamp(timestamp())
output_loc_to_pass = "FILEPATH"
message(output_loc_to_pass)
pardiso_path <- NULL  # change this to your PARDISO license location, if you have one and want to use it
imputed <- impute_covariate(covar = data, covariate_name = "income_pc_by_race_ethn", outcome_name = "income_pc",
                            family = "gaussian", save_plots = TRUE, output_loc = output_loc_to_pass,
                            lib_loc = rlibs_loc, pardiso_path = pardiso_path, INLA_loc = rlibs_loc,
                            load_saved_intermediate = FALSE, years_to_model = 1990:max(interp_years))

# subset and rename columns
imputed <- imputed[, c("uid", "mcnty", "year", "race_group", "mean_unscaled", "sd", "median_unscaled",
                       "lower_unscaled", "upper_unscaled")]
setnames(imputed, c("mean_unscaled", "median_unscaled", "lower_unscaled", "upper_unscaled"),
         c("income_pc", "median", "lower", "upper"))

data <- copy(imputed[, c("mcnty", "year", "race_group", "income_pc")])

# convert 1977 OMB race/ethnicity groups to standard IDs, add race set ID from ushd_shared.population_group_set table
data[, race := car::recode(race_group, "'Hispanic'=2; 'NH Black'=4; 'NH White'=5; 'NH AIAN'=6; 'NH API'=7; else=99")]
stopifnot(nrow(data[race == 99]) == 0)  # there should be no observations with missing/invalid race ID
setnames(data, "race_group", "race_label")
data[, race_set := pop_group_sets[population_group_set_name == "omb_1977", population_group_set_id]]
setcolorder(data, c("mcnty", "year", "race", "race_label", "race_set", "income_pc"))

# Format and save output ----------------------------------------------------------------------------
# run checks
check_missingness(data)
check_var(data, "year")

# save RDS file
setkeyv(data, c("mcnty", "year"))
out_dir <- paste0(cov_dir, "prepped/income_pc_by_race_ethn/")
saveRDS(data, file = paste0(out_dir, date_time_stamp, ".rds"))