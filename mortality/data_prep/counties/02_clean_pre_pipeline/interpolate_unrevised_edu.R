########################################################################################################
# Description: Identify start and end birth proportions by education group and linearly calculate
#              adjustment proportions for inner years (2009-201X, max 2015) with unknown education.
#
# Input: data tables with imputed education values for state-years outside 2009-2015 and for those
#        within 2009-2015 that reported revised education.
# Output:
#         1. data tables with interpolated birth totals by mother's education for 2009 up to 2015
#         2. copies of data tables with imputed edu values for 1989-2008 and 2016+ (to achieve complete
#            time series in output folder)
#########################################################################################################

## Set-up and directories -------------------------------------------------------------------------------
library(data.table)
parent_dir <- paste0('FILEPATH',
                     'FILEPATH')
cov_dir <- 'FILEPATH'
loc_dir <- 'FILEPATH'
out_dir <- paste0(parent_dir, "FILEPATH")
args <- commandArgs(trailingOnly = TRUE)
archive_date <- args[1]
archive_dir <- paste0(out_dir, '_archive/', archive_date, '/')
if (!dir.exists(archive_dir)) dir.create(archive_dir)

# get merged counties
mcntys <- fread(paste0(loc_dir, 'merged_counties.csv'))
setnames(mcntys, "state", "state_fips")  # rename for easier merging

# get list of reporting state-years and list of final years that each state reported unrevised edu
reporting_states <- fread(paste0(parent_dir, 'documentation/births_edu_reporting_states.csv'))
end_years <- reporting_states[reporting_status == 0 & year %in% 2009:2015, max(year), by = "state_fips"]
setnames(end_years, "V1", "end_year")

## Function for cleaning dataset before interpolation ----------------------------------------------------
get_cleaned_data <- function(edu_data){
  # filter to relevant columns and states
  edu_data <- edu_data[, list(births = sum(births)), by = c('year','state_res_numeric','full_fips_res_numeric',
                                                            'mother_age','education')]
  edu_data <- edu_data[state_res_numeric %in% 1:56]  # keep only 50 states + DC 
  
  # bin ages
  edu_data <- merge(edu_data, data.table(mother_age = c(10,15,20,25,30,35,40,45,50),
                                         age_group = c(1,2,3,4,5,5,6,6,6)),
                    by = 'mother_age')
  
  # add merged counties
  edu_data <- merge(edu_data, unique(mcntys[, .(cnty, mcnty)]),
                    by.x = 'full_fips_res_numeric',
                    by.y = 'cnty',
                    allow.cartesian = T,
                    all.x = T)
  
  # add on education labels
  edu_data <- merge(edu_data, data.table(education = c(101,102,103,104,100),
                                         edu_label = c('Less than HS','HS graduate',
                                                       'Some college','College graduate','Unknown')),
                    by = 'education')
  
  # collapse
  edu_data <- edu_data[,.(births = sum(births)),.(year,mcnty,age_group,edu_label)]
  
  # square the dataset
  edu_data <- merge(data.table(expand.grid(year = unique(edu_data$year),
                                           mcnty = unique(mcntys$mcnty),
                                           age_group = unique(edu_data$age_group),
                                           edu_label = unique(edu_data$edu_label))),
                    edu_data,
                    by = c('year','mcnty','age_group','edu_label'),
                    all = T)
  edu_data[is.na(births), births := 0]
  
  # add final year that each state reported unrevised edu, filter each state to years 2008-end_year+1
  edu_data <- merge(edu_data, unique(mcntys[, .(mcnty, state_fips)]), by = "mcnty")  # add state res fips back
  edu_data <- merge(edu_data, end_years, by = "state_fips", all.x = T)
  edu_data <- edu_data[!is.na(end_year)]  # drop states where this missingness is not a problem
  temp <- lapply(sort(unique(edu_data[, state_fips])), function(state) {
    dt <- edu_data[state_fips == state]
    dt <- dt[year %in% 2008:(end_year+1)]
  })
  edu_data <- rbindlist(temp, use.names = T); rm(temp)
  
  # add full state names
  edu_data <- merge(edu_data, unique(mcntys[, c('state_name','mcnty')]), by = 'mcnty')
  
  return(edu_data)
}

# grab metro and non-metro mcntys
cov_dir <- 'FILEPATH'
urban_cov <- data.table(readRDS(paste0(cov_dir, 'FILEPATH')))
urban_cov <- merge(urban_cov, unique(mcntys[,c('mcnty','cnty')]), by.x = 'fips', by.y = 'cnty', all.x = T)
urban_cov[, metro := ifelse(rur_urb_code %in% 1:3,1,0)]
urban_cov[mcnty %in% c(101,1809), metro := 1]
urban_cov <- unique(urban_cov[,c('year','mcnty','metro')])
setnames(urban_cov, 'year','year_group')
urban_cov <- merge(data.table(year_group = c(rep(1993,14), rep(2003,10), rep(2013,8)), year = 1989:2020),
                   urban_cov, by = c('year_group'), allow.cartesian = T)[,-'year_group']

## Create and apply interpolation proportions ------------------------------------------------------------
# read in and format cleaned dataset
interp_years <- 2008:2016
dt_orig <- rbindlist(lapply(interp_years, function(yr) {
  message("Processing births data for ", yr)
  data <- fread(paste0(parent_dir, "FILEPATH", yr, "_imputed.csv"))  # read post-imputation files
}), use.names = T)

clean_dt <- get_cleaned_data(dt_orig)
clean_dt <- merge(clean_dt, urban_cov, by = c('year','mcnty'), all.x = T)  # merge on metro/non-metro info

# loop over each state and interpolate birth proportions
temp <- lapply(sort(unique(clean_dt[, state_name])), function(state) {
  # filter dataset to appropriate state and identify end year
  dt <- clean_dt[state_name == state]
  end_yr <- unique(dt[, end_year])
  dt[, end_year := NULL]  # don't need end year in dataset anymore
  
  # interpolate births proportions, pooling demographics as necessary to obtain usable proportions
  dt[, births_total_1 := sum(births), by = c('year','mcnty','age_group')]
  dt[year %in% c(2008, end_yr+1), prop_1 := births/births_total_1]
  dt[year %in% c(2008, end_yr+1) & is.nan(prop_1), prop_1 := 0]  # set undefined proportions as 0
  
  dt[, births_2 := sum(births), by = c('year','metro','age_group','edu_label')]
  dt[, births_total_2 := sum(births), by = c('year','metro','age_group')]
  dt[year %in% c(2008, end_yr+1), prop_2 := births_2/births_total_2]
  dt[year %in% c(2008, end_yr+1) & is.nan(prop_2), prop_2 := 0]  # set undefined proportions as 0
  
  dt[, births_3 := sum(births), by = c('year','state_fips','age_group','edu_label')]
  dt[, births_total_3 := sum(births), by = c('year','state_fips','age_group')]
  dt[year %in% c(2008, end_yr+1), prop_3 := births_3/births_total_3]
  dt[year %in% c(2008, end_yr+1) & is.nan(prop_3), prop_3 := 0]  # set undefined proportions as 0
  
  dt[, prop_1 := approx(x=year, y=prop_1, xout=2008:(end_yr+1))$y, by = c('mcnty','age_group','edu_label')]
  dt[, prop_2 := approx(x=year, y=prop_2, xout=2008:(end_yr+1))$y, by = c('mcnty','age_group','edu_label')]
  dt[, prop_3 := approx(x=year, y=prop_3, xout=2008:(end_yr+1))$y, by = c('mcnty','age_group','edu_label')]
  
  # restrict education for mothers aged 10-14 to "Less than HS"
  dt[age_group == 1, c("prop_1", "prop_2", "prop_3") := ifelse(edu_label == "Less than HS", 1, 0)]
  dt <- dt[year %in% 2009:end_yr]  # don't need outside years anymore
  dt[, c('births_2', 'births_3') := NULL]
  
  # select correct proportions and apply to birth totals
  min_denom_size <- 25
  dt[, prop_total_1 := sum(prop_1), by = c('year','mcnty','age_group')]
  dt[, prop_total_2 := sum(prop_2), by = c('year','mcnty','age_group')]
  dt[, prop := ifelse(births_total_1 >= min_denom_size, prop_1,
                      ifelse(births_total_2 >= min_denom_size, prop_2, prop_3))]
  dt[prop_total_1 == 0 & births_total_1 > 0, prop := prop_2]
  dt[prop_total_2 == 0 & births_total_2 > 0, prop := prop_3]
  dt[, prop_level := ifelse(births_total_1 >= min_denom_size, "prop_1",
                            ifelse(births_total_2 >= min_denom_size, "prop_2", "prop_3"))]
  dt[prop_total_1 == 0 & births_total_1 > 0, prop_level := "prop_2"]
  dt[prop_total_2 == 0 & births_total_2 > 0, prop_level := "prop_3"]
  
  # Alaska (min births_total_3 = 5)
  # Connecticut (10)
  # Hawaii (8)
  # Maine (2)
  # Rhode Island (5)
  # West Virginia (21)

  dt[, c('prop_1','prop_total_1','prop_2','births_total_1','births_total_2','births_total_3','births',
         'prop_3','prop_total_2') := NULL]
})
interp_props <- rbindlist(temp, use.names = T); rm(temp)

# save proportions for reference
fwrite(interp_props, paste0(out_dir, 'interpolation_props/births_edu_interp_props.csv'))

## Incorporate interpolated births and save outputs ------------------------------------------------------
orig_cols <- names(copy(dt_orig))
for (yr in sort(unique(interp_props[, year]))) {  # loop over relevant years
  props <- interp_props[year == yr]
  dt <- dt_orig[year == yr]
  births_before <- sum(dt$births)
  
  # reduce mcntys to relevant columns and merge to dt
  mcntys <- mcntys[, c('cnty','mcnty')]
  dt <- merge(dt, unique(mcntys), by.x = 'full_fips_res_numeric', by.y = 'cnty', all.x = T)
  
  # create mother age groups and labels
  dt <- merge(dt, data.table(mother_age = c(10,15,20,25,30,35,40,45,50), age_group = c(1,2,3,4,5,5,6,6,6)),
              by = 'mother_age')
  
  # add edu labels
  dt <- merge(dt, data.table(education = c(101,102,103,104,100), edu_label = c('Less than HS','HS graduate',
                                                                               'Some college','College graduate',
                                                                               'Unknown')), 
              by = 'education', all.x = T)
  
  dt_adjust <- dt[state_res_numeric %in% unique(props[, state_fips])]
  dt <- dt[!state_res_numeric %in% unique(props[, state_fips])]
  
  dt_adjust <- merge(dt_adjust[, -'edu_label'],
                     props[, c('year','mcnty','age_group','edu_label','prop')],
                     by = c('year','mcnty','age_group'), all.x = TRUE, allow.cartesian = TRUE)
  
  stopifnot(nrow(dt_adjust[is.na(prop)]) == 0)  # should have proportions for all cases
  
  dt_adjust[, births := births*prop]
  
  dt_adjust[, education := ifelse(edu_label == 'Less than HS', 101,
                                  ifelse(edu_label == 'HS graduate', 102,
                                         ifelse(edu_label == 'Some college', 103,
                                                ifelse(edu_label == 'College graduate', 104, 100))))]
  
  dt_adjust <- dt_adjust[, orig_cols, with = FALSE]
  dt <- dt[, orig_cols, with = FALSE]
  dt <- rbind(dt, dt_adjust)
  dt <- dt[, list(births = sum(births)), by = setdiff(names(dt), "births")]
  
  # sanity checks. make sure that...
  stopifnot(round(births_before - sum(dt$births)) == 0)  # birth total didn't change
  stopifnot(sum(dt[state_res_numeric %in% 1:56 & education == 100, births]) == 0)  # there are no births with Unknown edu
  
  # save output, including archived version
  setnames(dt, "education", "edu")  # to match format in backcasting script
  fwrite(dt, paste0(out_dir, 'births_', unique(dt[, year]), '_interpolated.csv'))
  fwrite(dt, paste0(archive_dir, 'births_', unique(dt[, year]), '_interpolated.csv'))
}

# save copies of datasets for years that don't need interpolation in same location as interpolated files
# this is so we have a complete time series for each step in data adjustment
non_interp_years <- c(1989:2008, 2016:2020)
for (yr in non_interp_years) {
  dt <- fread(paste0(parent_dir, "FILEPATH", yr, "_imputed.csv"))
  setnames(dt, "education", "edu")  # to match format in backcasting script
  fwrite(dt, paste0(out_dir, "births_", yr, "_interpolated.csv"))
  fwrite(dt, paste0(archive_dir, "/births_", yr, "_interpolated.csv"))  # archived version
}
