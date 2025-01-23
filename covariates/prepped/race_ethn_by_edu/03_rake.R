###################################################################################################
## Description: Rake so that sum of proportion totals across all r/e groups is 1. Since each group
##              is imputed separately, the resulting sum of proportions is unlikely to be exactly 1
##              (but will be close).
##              Launched by:
##                  .../covariates/counties/prepped/race_ethn_by_edu.r
##              after 02_impute.R script is finished running for each r/e group
##              
## Output:      One final prepped dataset per r/e group
###################################################################################################
job_start <- Sys.time()
print(paste0("Rake data START: ", job_start))

# Setup -------------------------------------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")

# get time stamp argument from launch script
args <- commandArgs(trailingOnly = TRUE)
time_stamp <- args[1]

# get data source metadata for database uploads
census_meta <- get_covariate_metadata("census_race_ethn_by_edu")
acs_meta <- get_covariate_metadata("acs_race_ethn_by_edu")

# read in imputed datasets and merge together
re_cols <- c("nh_white", "nh_black", "nh_aian", "nh_api", "hisp")

imputed_dts <- lapply(re_cols, function(re) {
  parent_dir <- paste0(cov_dir, "prepped/", ifelse(re == "hisp", "ethn_", "race_"), re, "_by_edu/")
  readRDS(paste0(parent_dir, "02_imputed_data/", time_stamp, ".rds"))
})

data_imp <- Reduce(function(...) merge(..., all=TRUE, by = c("mcnty", "year", "edu", "pop")), imputed_dts)

# Rake --------------------------------------------------------------------------------------------
# rake proportion totals to 1
data_imp[, prop_sum := nh_white + nh_black + nh_aian + nh_api + hisp]
data_imp[, raking_factor := 1 / prop_sum]
data_imp[, c("race_nh_white", "race_nh_black", "race_nh_aian", "race_nh_api", "ethn_hisp") :=
           list(nh_white*raking_factor, nh_black*raking_factor, nh_aian*raking_factor, nh_api*raking_factor, hisp*raking_factor)]
data_imp[, eval(re_cols) := NULL]
data_imp[, prop_sum_raked := race_nh_white + race_nh_black + race_nh_aian + race_nh_api + ethn_hisp]

# final proportions may not all be exactly 1, so this check allows for a tiny bit of tolerance
# if at least one value of prop_sum_raked == 1 AND variance is essentially 0, all prop_sum_raked values must be essentially 1
stopifnot(all.equal(var(data_imp[, prop_sum_raked]), 0) & 1 %in% unique(data_imp[, prop_sum_raked]))
data_imp[, c(grep("raw_", names(data_imp), value = T), "prop_sum", "prop_sum_raked", "raking_factor", "pop") := NULL]

# Format and save output ----------------------------------------------------------------------------
# standardize edu IDs
setnames(data_imp, "edu", "edu_label")
data_imp[, edu := car::recode(edu_label, "'Less than HS'=101; 'HS graduate'=102; 'Some college'=103; 'College graduate'=104; else=100")]
stopifnot(nrow(data_imp[edu == 100]) == 0)  # there should not be any unknown edu

# run checks
check_missingness(data_imp)
check_var(data_imp, "year")

re_cov_names <- data.table(race = c(paste0("race_", re_cols[1:4]), paste0("ethn_", re_cols[5])),
                           descr = c("non-Hispanic White", "non-Hispanic Black", "non-Hispanic AIAN",
                                     "non-Hispanic API", "Hispanic"),
                           short_name = re_cols)
job_end <- Sys.time()
print(paste0("Rake data END: ", job_end))
print(job_end-job_start)

job_start <- Sys.time()
print(paste0("Upload/plot data START: ", job_start))

# upload one r/e group at a time to the db and make associated plots and maps
for (re in re_cov_names$race) {
  # subset to specific race/ethnicity
  temp <- copy(data_imp[, list(mcnty, year, get(re), edu, edu_label)])
  setnames(temp, "V3", re)  # re-establish name of race/ethnicity column
  
  # save RDS file
  setkeyv(temp, c("mcnty", "year"))
  
  # upload data to db
  re_descr <- re_cov_names[race == re, descr]  # get description for metadata
  re_short_name <- re_cov_names[race == re, short_name]  # get short name for model file path in metadata
}

job_end <- Sys.time()
print(paste0("Upload/plot data END: ", job_end))
print(job_end-job_start)