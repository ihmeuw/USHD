###################################################################################################
## Description: Prepare input data for race/ethnicity by educational attainment. Launched by:
##              .../covariates/counties/prepped/race_ethn_by_edu.r.
##              
## Output:      Combined Census and ACS dataset ready for covariate imputation. One file per r/e group
###################################################################################################
job_start <- Sys.time()
print(paste0("Prep data job START: ", job_start))

# Setup -------------------------------------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")

# get time stamp argument from launch script
args <- commandArgs(trailingOnly = TRUE)
time_stamp <- args[1]

# functions for converting between variance and MOE (for adding MOEs when collapsing to mcnty)
convert_to_variance <- function(x) {return((x/1.645)^2)}
convert_to_moes <- function(x) {return(1.645*sqrt(x))}

# Prepare Census data -----------------------------------------------------------------------------
# load data and collapse to merged counties
census_meta <- get_covariate_metadata("census_race_ethn_by_edu")
census_data <- readRDS(paste0(cov_dir, "FILEPATH", unique(census_meta[, file])))[, -"nid"]
census_data <- merge(census_data, loc, by = "fips", all.x = T)
census_data <- census_data[, list(nh_white = sum(nh_white), nh_black = sum(nh_black), nh_aian = sum(nh_aian),
                                  nh_api = sum(nh_api), nh_other = sum(nh_other), hisp = sum(hisp), pop = sum(pop)),
                           by = "mcnty,year,edu_label"]
census_data[, "nh_other" := NULL]  # drop Other race, since this is not a standard race category
census_data[, c("nh_white_moe", "nh_black_moe", "nh_aian_moe", "nh_api_moe", "hisp_moe") := NA]  # for rbinding with acs data later

# Prepare ACS data --------------------------------------------------------------------------------
# load data; drop Other, Multiracial, and White combined; collapse to merged counties
acs_meta <- get_covariate_metadata("acs_race_ethn_by_edu")
acs_data <- readRDS(paste0(cov_dir, "FILEPATH", unique(acs_meta[, file])))[, -"nid"]
acs_data[, c("white", "other", "multi", "white_moe", "other_moe", "multi_moe") := NULL]
acs_data <- merge(acs_data, loc, by = "fips", all.x = T)
acs_data[, c("black_var", "aian_var", "asian_var", "nhopi_var", "white_nh_var", "hisp_var") :=
           lapply(.SD, convert_to_variance), .SDcols = grep("_moe", names(acs_data), value = T)]  # calculate variance from MOE
acs_data <- acs_data[, list(black = sum(black), black_var = sum(black_var), aian = sum(aian), aian_var = sum(aian_var),
                            asian = sum(asian), asian_var = sum(asian_var), nhopi = sum(nhopi), nhopi_var = sum(nhopi_var),
                            white_nh = sum(white_nh), white_nh_var = sum(white_nh_var), hisp = sum(hisp),
                            hisp_var = sum(hisp_var), pop = sum(pop)), by = "mcnty,year,edu_label"]
acs_data[, c("black_moe", "aian_moe", "asian_moe","nhopi_moe", "white_nh_moe", "hisp_moe") := lapply(.SD, convert_to_moes),
         .SDcols = grep("_var", names(acs_data), value = T)]  # convert back to MOE
acs_data[, grep("_var", names(acs_data), value = T) := NULL]  # remove variances

# combine Asian and NHOPI into API
acs_data[, api := asian + nhopi]
acs_data[, c("asian_var", "nhopi_var") := lapply(.SD, convert_to_variance), .SDcols = c("asian_moe", "nhopi_moe")]  # calculate variance from MOE
acs_data[, api_var := asian_var + nhopi_var]  # get variance for API
acs_data[, api_moe := convert_to_moes(api_var)]  # convert API variance to API MOE
acs_data[, c("api_var", "asian", "nhopi", "asian_var", "nhopi_var", "asian_moe", "nhopi_moe") := NULL]  # drop API variance, NHOPI columns, & Asian columns

# rename ACS race groups to their proxy groups (e.g., AIAN as a proxy for NH AIAN)
setnames(acs_data, c("white_nh", "black", "aian", "api", "white_nh_moe", "black_moe", "aian_moe", "api_moe"),
         c("nh_white", "nh_black", "nh_aian", "nh_api", "nh_white_moe", "nh_black_moe", "nh_aian_moe", "nh_api_moe"))

# Combine Census and ACS data and write output for imputation step --------------------------------
data <- rbindlist(list(census_data, acs_data), use.names = TRUE); rm(census_data, acs_data)
re_cols <- c("nh_white", "nh_black", "nh_aian", "nh_api", "hisp")

# save one file per r/e group with relevant columns
for (re in re_cols) {
  cols <- c("mcnty", "year", "edu_label", re, paste0(re, "_moe"), "pop")
  temp <- copy(data[year >= 2000, mget(cols)])  # we only impute for 2000+ for this set of covariates
  setnames(temp, "edu_label", "edu")  # covariate imputation script looks for edu, not edu_label. set back to edu_label post-imputation
  temp[, edu :=car::recode(edu, "'lhs'='Less than HS'; 'hs'='HS graduate'; 'sc'='Some college'; 'ba'='College graduate'")]
  
  saveRDS(temp, paste0(cov_dir, "prepped/", ifelse(re == "hisp", "ethn_", "race_"), re,
                       "FILEPATH", time_stamp, ".rds"))
}

job_end <- Sys.time()
print(paste0("Prep data job END: ", job_end))
print(job_end-job_start)