#################################################################
# Description: Apply imputation proportions to cleaned dataset
#              Single year launched by 01_impute_data.R
#################################################################

rm(list = ls())

args <- commandArgs(trailingOnly = TRUE)
year <- args[1]
run_type <- args[2]
archive_date <- args[3]

library(data.table)
L <- ifelse(Sys.info()[1]=='Windows', 'FILEPATH', 'FILEPATH')
ushd_dir <- paste0(L, 'FILEPATH')
repo_dir <- paste0('FILEPATH', Sys.info()[["user"]], 'FILEPATH')
functions_dir <- paste0(repo_dir, 'FILEPATH')
data_out_dir <- paste0(ushd_dir, 'FILEPATH', run_type, '/')
data_in_dir <- paste0(data_out_dir, if (run_type == 'births') '01_', 'pre_imputation/')
props_dir <- paste0(ushd_dir, 'FILEPATH', run_type, '/', if (run_type == 'births') '01_',
                    'FILEPATH')

adjust_unknown_edu <- function(dt, run_type){
  if (run_type == 'births') {
    births_before <- sum(dt$births)
  } else {
    deaths_before <- sum(dt$deaths)
  }
  origi_cols <- names(copy(dt))
  
  props <- get_proportion(paste0(run_type, '_edu_props'))[year == unique(dt$year)]
  
  # add mcntys
  mcntys <- fread(paste0('FILEPATH',
                         'merged_counties.csv'))[,c('cnty','mcnty')]
  
  dt[,county_temp := as.character(county_res)]
  dt[nchar(county_temp) < 3, county_temp := sprintf("%03d", as.numeric(county_temp))]
  dt[,fips := as.numeric(paste0(state_res_numeric,county_temp))]
  
  dt <- merge(dt, unique(mcntys),
              by.x = 'fips', by.y = 'cnty',
              all.x = T)
  
  # create age groups and age labels
  if(run_type == 'mortality'){
    dt[,age_group := ifelse(age %in% c('25','30'), 1,
                            ifelse(age %in% c('35','40'), 2,
                                   ifelse(age %in% c('45','50','55','60'), 3,
                                          ifelse(age %in% c('65','70','75',
                                                            '80','85','90',
                                                            '95'), 4, 
                                                 999))))]
    # add edu labels
    dt <- merge(dt, data.table(education = c(1,2,3,4,5,9),
                               edu_label = c('Less than HS','Less than HS',
                                             'HS graduate', 'Some college',
                                             'College graduate','Unknown')), 
                by = 'education',
                all.x = T)
    
    # add race groups
    dt[,row_id := .I]
    dt[,race_temp := recode_race(year, race, race_bridged,
                                 race_recode_40, hisp_recode),
       by = row_id]
  }else if (run_type == 'linked_births'){
    dt <- merge(dt, data.table(mother_age = c(10,15,20,25,
                                              30,35,40,45,50),
                               age_group = c(1,2,3,4,5,5,
                                             6,6,6)),
                by = 'mother_age')
    # add edu labels
    dt <- merge(dt, data.table(mother_education = c(1,2,3,4,5,9),
                               edu_label = c('Less than HS','Less than HS',
                                             'HS graduate', 'Some college',
                                             'College graduate','Unknown')), 
                by = 'mother_education',
                all.x = T)
  } else if (run_type == 'births') {
    dt <- merge(dt, data.table(mother_age = c(10,15,20,25,
                                              30,35,40,45,50),
                               age_group = c(1,2,3,4,5,5,
                                             6,6,6)),
                by = 'mother_age')
    # add edu labels
    dt <- merge(dt, data.table(education = c(1,2,3,4,5,9),
                               edu_label = c('Less than HS','Less than HS',
                                             'HS graduate', 'Some college',
                                             'College graduate','Unknown')), 
                by = 'education',
                all.x = T)
  }
  
  dt_adjust <- dt[edu_label == 'Unknown']
  dt <- dt[edu_label != 'Unknown']
  
  if(run_type == 'mortality'){
    dt_adjust <- merge(dt_adjust[,-'edu_label'], props,
                       by.x = c('year','mcnty','age_group',
                                'race_temp'), 
                       by.y = c('year','mcnty','age_group',
                                'race_group'),
                       all.x = TRUE,
                       allow.cartesian = TRUE)
  }else if (run_type %in% c('linked_births', 'births')){
    dt_adjust <- merge(dt_adjust[,-'edu_label'], props,
                       by.x = c('year','mcnty','age_group'), 
                       by.y = c('year','mcnty','age_group'),
                       all.x = TRUE,
                       allow.cartesian = TRUE)
  }
  
  dt_adjust[is.na(prop), edu_label := 'Unknown']
  dt_adjust[is.na(prop), prop := 1]
  
  if (run_type == 'births') {
    dt_adjust[, births := births * prop]
  } else {
    dt_adjust[,deaths := deaths * prop]
  }
  
  # standardize edu IDs for both dt and dt_adjust (100 = Unknown)
  if(run_type %in% c('mortality', 'births')){
    dt_adjust[,education := ifelse(edu_label == 'Less than HS', 101,
                                   ifelse(edu_label == 'HS graduate', 102,
                                          ifelse(edu_label == 'Some college', 103,
                                                 ifelse(edu_label == 'College graduate', 104, 100))))]
    dt[,education := ifelse(edu_label == 'Less than HS', 101,
                            ifelse(edu_label == 'HS graduate', 102,
                                   ifelse(edu_label == 'Some college', 103,
                                          ifelse(edu_label == 'College graduate', 104, 100))))]
  }else if(run_type == 'linked_births'){
    dt_adjust[,mother_education := ifelse(edu_label == 'Less than HS', 101,
                                          ifelse(edu_label == 'HS graduate', 102,
                                                 ifelse(edu_label == 'Some college', 103,
                                                        ifelse(edu_label == 'College graduate', 104, 100))))]
    dt[,mother_education := ifelse(edu_label == 'Less than HS', 101,
                                   ifelse(edu_label == 'HS graduate', 102,
                                          ifelse(edu_label == 'Some college', 103,
                                                 ifelse(edu_label == 'College graduate', 104, 100))))]
  }
  
  dt_adjust <- dt_adjust[,origi_cols, with = FALSE]
  dt <- dt[,origi_cols, with = FALSE]
  dt <- rbind(dt, dt_adjust)
  if (run_type == 'births') {
    dt <- dt[, list(births=sum(births)), by=setdiff(names(dt), "births")]
    stopifnot(round(births_before - sum(dt$births)) == 0)
  } else {
    dt <- dt[, list(deaths=sum(deaths)), by=setdiff(names(dt), "deaths")]
    stopifnot(round(deaths_before - sum(dt$deaths)) == 0)
  }
  
  return(dt)
}

file_name <- list.files(data_in_dir, as.character(year))
dt <- fread(paste0(data_in_dir, file_name))

if(run_type == 'mortality'){
  if(year %in% 1980:1988){
    dt[, education := 100]  # education is always missing before 1989. standardize ID here
  }
}

if(year %in% 1989:2020){
  dt <- adjust_unknown_edu(dt, run_type)
}
fwrite(dt, paste0(data_out_dir, ifelse(run_type == 'births', '02_pre_interpolation/births_', 'data_'),
                 unique(dt$year), ifelse(run_type == 'births', '_imputed.csv', '_cleaned.csv')), 
          row.names = FALSE)
dir.create(paste0(data_out_dir,
  ifelse(run_type == 'births', '02_pre_interpolation/', ''), 
  '_archive/', archive_date, '/'), 
  showWarnings = FALSE)
fwrite(dt, paste0(data_out_dir,
  ifelse(run_type == 'births', paste0('FILEPATH', archive_date, '/'), 
    paste0('_archive/', archive_date, '/data_')), 
                 unique(dt$year), ifelse(run_type == 'births', '_imputed.csv', '_cleaned.csv')), 
          row.names = FALSE)
