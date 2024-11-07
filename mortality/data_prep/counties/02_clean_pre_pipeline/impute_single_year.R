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
ushd_dir <- "FILEPATH"
repo_dir <- "FILEPATH"
functions_dir <- 'FILEPATH/data_prep/counties/02_clean_pre_pipeline/_functions/'
data_out_dir <- "FILEPATH"
data_in_dir <- "FILEPATH"
props_dir <- "FILEPATH"
source(paste0(functions_dir, 'prep_functions.R'))
source(paste0(functions_dir, 'file_check_functions.R'))

adjust_AK_NYC_VA <- function(dt){
  deaths_before <- sum(dt$deaths)
  
  props <- get_proportion('mortality_AK_NYC_VA_props')
  reassign <- fread("FILEPATH")
  
  dt_adjust <- dt[full_fips_res_numeric %in% unique(reassign$fips_from)]
  dt <- dt[!full_fips_res_numeric %in% unique(reassign$fips_from)]
  
  dt_adjust <- merge(dt_adjust, unique(reassign[, list(fips_from, group)]), 
                     by.x = 'full_fips_res_numeric', by.y ="fips_from", all.x=T)
  dt_adjust <- merge(dt_adjust, props, by='group', 
                     all=T, allow.cartesian=T)
  dt_adjust[, c('full_fips_res_numeric', 'deaths') := list(fips_to, proportion)]
  dt_adjust[, c('group', 'proportion', 'fips_to') := NULL]
  
  # add adjusted deaths back onto other deaths dataset
  dt <- rbind(dt, dt_adjust, use.names=T)
  dt <- dt[, list(deaths=sum(deaths)), by=setdiff(names(dt), "deaths")]
  
  stopifnot(round(deaths_before - sum(dt$deaths)) == 0)
  
  return(dt)
}

adjust_GA_hiv <- function(dt){
  deaths_before <- sum(dt$deaths)
  
  props <- get_proportion('mortality_GA_hiv_props')[year == unique(dt$year)]
  
  dt_adjust <- dt[full_fips_res_numeric == 13999]
  dt <- dt[full_fips_res_numeric != 13999]
  
  dt_adjust[, full_fips_res_numeric := NULL]
  dt_adjust <- merge(dt_adjust, props, all=T, allow.cartesian=T, by=c("year"))
  dt_adjust[, deaths := deaths * proportion]
  dt_adjust[, proportion := NULL]
  
  dt <- rbind(dt, dt_adjust)
  
  dt <- dt[, list(deaths=sum(deaths)), by=setdiff(names(dt), "deaths")]
  stopifnot(round(deaths_before - sum(dt$deaths)) == 0)
  
  return(dt)
}

adjust_unknown_ethn <- function(dt){
  deaths_before <- sum(dt$deaths)
  origi_cols <- names(copy(dt))
  
  props <- get_proportion('mortality_ethn_props')
  # add mcntys
  mcntys <- fread(paste0("FILEPATH"))[,c('cnty','mcnty')]
  dt <- merge(dt, unique(mcntys[,.(cnty, mcnty)]),
              by.x="full_fips_res_numeric",
              by.y = 'cnty',
              allow.cartesian = T,
              all.x = T)
  
  dt[,race_temp := ifelse(race %in% c(68,78,5,58,48,6,38,18,28,8,7,0),
                          4, race)]
  dt[,hisp_missing := ifelse(hisp_recode == 9, "missing", "not_missing")]
  
  dt_adjust <- dt[hisp_missing == 'missing' & state_res_numeric <= 56]
  dt <- dt[hisp_missing != 'missing' | state_res_numeric > 56]
  
  dt_adjust[,state := ifelse(is.na(mcnty), state_res_numeric, NA)]
  dt_adjust <- merge(dt_adjust, props,
                     by.x = c("state", "mcnty", "year", "sex", "race_temp"),
                     by.y = c("state", "mcnty", "year", "sex", "race"),
                     all.x = T)
  
  dt_adjust_hisp <- copy(dt_adjust)
  dt_adjust_nonhisp <- copy(dt_adjust)
  
  dt_adjust_hisp <- dt_adjust_hisp[,deaths := deaths * ratio]

    dt_adjust_hisp[,c('race_label_1997', 'race_label_1977') := 'Hispanic']
  
  dt_adjust_nonhisp <- dt_adjust_nonhisp[,deaths := deaths * (1 - ratio)]
  
  dt_adjust <- rbind(dt_adjust_hisp[,origi_cols, with = FALSE], 
                     dt_adjust_nonhisp[,origi_cols, with = FALSE])
  dt <- rbind(dt[,origi_cols, with = FALSE], 
              dt_adjust)
  
  
  dt <- dt[, list(deaths=sum(deaths)), by=setdiff(names(dt), "deaths")]
  stopifnot(round(deaths_before - sum(dt$deaths)) == 0)
  
  return(dt)
}

adjust_unknown_edu <- function(dt, run_type){
  if (run_type == 'births') {
    births_before <- sum(dt$births)
  } else {
    deaths_before <- sum(dt$deaths)
  }
  origi_cols <- names(copy(dt))
  
  props <- get_proportion(paste0(run_type, '_edu_props'))[year == unique(dt$year)]
  
  # add mcntys
  mcntys <- fread(paste0("FILEPATH"))[,c('cnty','mcnty')]
  
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
  if(year %in% 1980:1981){
    dt <- adjust_AK_NYC_VA(dt)
  }
  
  if(year %in% 1988:1991){
    dt <- adjust_GA_hiv(dt)
  }
  
  if(year %in% 2000:2020){
    dt <- adjust_unknown_ethn(dt)
  }
  
  if(year %in% 1980:1988){
    dt[, education := 100]  # education is always missing before 1989. standardize ID here
  }
}

if(year %in% 1989:2020){
  dt <- adjust_unknown_edu(dt, run_type)
}
fwrite(dt, "FILEPATH", row.names = FALSE)
