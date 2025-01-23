#################################################################
# Description: Create proportions of educational attainment from
#              the not unknown parts of the deaths files.
#################################################################

rm(list = ls())
library(data.table)
library(zoo)

repo_dir <- paste0('FILEPATH', Sys.info()[['user']], 'FILEPATH')
cov_dir <- 'FILEPATH'

# mcnty mapping
mcntys <- fread('FILEPATH/merged_counties.csv')

get_cleaned_data <- function(file_path){
  dt <- fread(file_path)
  # remove unknown ages and ages under 25
  dt <- dt[!is.na(age)]
  dt <- dt[!age %in% c('0d','7d','1-5mo','6-11mo',
                       '12-23mo','2-4','5','10','15',
                       '20','999','999i')]
  dt$age <- as.numeric(dt$age)
  # bin ages
  dt[,age_group := ifelse(age %in% c(25,30), 1,
                          ifelse(age %in% c(35,40), 2,
                                 ifelse(age %in% c(45,50,55,60), 3,
                                        4)))]
  # keep only 50 states + DC 
  dt <- dt[state_res_alpha %in% c(state.abb, 'DC')]
  
  # add merged counties
  dt <- merge(dt, unique(mcntys[,.(cnty, mcnty)]),
              by.x='full_fips_res_numeric',
              by.y = 'cnty',
              allow.cartesian = T,
              all.x = T)
  
  # add on education labels
  
  # to save run time, these edu IDs were not updated here but rather converted in impute_single_year.R.
  # these should be updated next time proportions are re-run to avoid confusion.
  dt <- merge(dt, data.table(education = c(1,2,3,4,5,9),
                             edu = c(1,1,2,3,4,0),
                             edu_label = c('Less than HS',
                                           'Less than HS',
                                           'HS graduate', 
                                           'Some college',
                                           'College graduate',
                                           'Unknown')), 
              by = 'education')
  
  # assign race groups according to the 1977 standard
  dt[,row_id := .I]
  dt[,race_group := recode_race(year, race, race_bridged, 
                                race_recode_40, hisp_recode),
            by = row_id]
  dt <- dt[race_group != 'NH Other Race']
  
  # remove missing deaths
  dt <- dt[!is.na(deaths)]
  
  # collapse
  dt <- dt[,.(deaths_1 = sum(deaths)),.(year,mcnty,age_group,edu_label,race_group)]
  return(dt)
}

# grab metro and non metro mcntys
urban_cov <- data.table(readRDS(paste0(cov_dir, 'FILEPATH')))
urban_cov <- merge(urban_cov, unique(mcntys[,c('mcnty','cnty')]), 
                   by.x = 'fips', by.y = 'cnty', all.x = T)
urban_cov[,metro := ifelse(rur_urb_code %in% 1:3,1,0)]
urban_cov[mcnty %in% c(101,1809), metro := 1]
urban_cov <- unique(urban_cov[,c('year','mcnty','metro')])
setnames(urban_cov, 'year','year_group')
urban_cov <- merge(data.table(year_group = c(rep(1993,14),
                                             rep(2003,10),
                                             rep(2013,8)),
                              year = 1989:2020),
                   urban_cov, by = c('year_group'),
                   allow.cartesian = T)[,-'year_group']

################################################################################
# CREATE PROPORTIONS
################################################################################
# get filenames
root <- paste0('FILEPATH')
files <- list.files(root, pattern='.csv')
files <- files[files %like% 'cleaned|adjusted']
files <- files[files %like% '1989|199|20']

# read in and format cleaned dataset
edu_props <- rbindlist(lapply(paste0(root, files),get_cleaned_data))

# square the dataset
edu_props <- merge(data.table(expand.grid(year = unique(edu_props$year),
                                          mcnty = unique(edu_props$mcnty),
                                          age_group = unique(edu_props$age_group),
                                          edu_label = unique(edu_props$edu_label),
                                          race_group = unique(edu_props$race_group))), 
                   edu_props, 
                   by = c('year','mcnty','age_group','edu_label','race_group'), 
                   all = T)
edu_props[is.na(deaths_1), deaths_1 := 0]

# add on states
edu_props <- merge(edu_props, 
                   unique(mcntys[,c('state_name','mcnty')]), 
                   by = 'mcnty')

# separate NYC and the rest of NY, since they are separate reporting areas for edu
nyc_counties <- mcntys[state_name == 'New York' & cnty_name %in%
                         c('New York County', 'Kings County', 
                           'Bronx County', 'Richmond County', 
                           'Queens County')]
edu_props[state_name == 'New York' & mcnty %in% nyc_counties[, mcnty], state_name := 'NYC']

# remove state/years that did not collect edu data
edu_props <- edu_props[!(state_name == 'Georgia' & year < 2010)]
edu_props <- edu_props[!(state_name == 'Louisiana' & year < 1991)]
edu_props <- edu_props[!(state_name == 'New York' & year < 1994)]
edu_props <- edu_props[!(state_name == 'Oklahoma' & year < 1997)]
edu_props <- edu_props[!(state_name == 'Rhode Island' & year < 2015)]
edu_props <- edu_props[!(state_name == 'South Dakota' & year < 2004)]
edu_props <- edu_props[!(state_name == 'Washington' & year < 1992)]

# merge on metro info
edu_props <- merge(edu_props, urban_cov, by = c('year','mcnty'), all.x = T)

# create year bins

#      Currently adding 2019-2020 to the 2016:2018 group
edu_props <- merge(edu_props,
                   data.table(year = 1989:2020,
                              year_pool = c(rep(1:10, each = 3),10,10)),
                   by = 'year', all = T)

edu_props <- edu_props[edu_label != 'Unknown']

edu_props[,deaths_total_1 := sum(deaths_1), by = c('year','mcnty',
                                                   'age_group','race_group')]
edu_props[,prop_1 := deaths_1 / deaths_total_1]

edu_props[,deaths_2 := sum(deaths_1), by = c('year_pool','mcnty','state_name',
                                             'age_group','race_group','edu_label')]
edu_props[,deaths_total_2 := sum(deaths_1), by = c('year_pool','mcnty','state_name',
                                                   'age_group','race_group')]
edu_props[,prop_2 := deaths_2 / deaths_total_2]

edu_props[,deaths_3 := sum(deaths_1), by = c('year','metro','state_name','age_group',
                                             'race_group','edu_label')]
edu_props[,deaths_total_3 := sum(deaths_1), by = c('year','metro','state_name',
                                                   'age_group','race_group')]
edu_props[,prop_3 := deaths_3 / deaths_total_3]

edu_props[,deaths_4 := sum(deaths_1), by = c('year_pool','metro','state_name',
                                             'age_group','race_group','edu_label')]
edu_props[,deaths_total_4 := sum(deaths_1), by = c('year_pool','metro','state_name',
                                                   'age_group','race_group')]
edu_props[,prop_4 := deaths_4 / deaths_total_4]

edu_props[,deaths_5 := sum(deaths_1), by = c('year','metro','age_group',
                                             'race_group','edu_label')]
edu_props[,deaths_total_5 := sum(deaths_1), by = c('year','metro',
                                                   'age_group','race_group')]
edu_props[,prop_5 := deaths_5 / deaths_total_5]

edu_props[,deaths_6 := sum(deaths_1), by = c('year_pool','metro','age_group',
                                             'race_group','edu_label')]
edu_props[,deaths_total_6 := sum(deaths_1), by = c('year_pool','metro',
                                                   'age_group','race_group')]
edu_props[,prop_6 := deaths_6 / deaths_total_6]

stopifnot(min(edu_props$deaths_total_6) > 50)

edu_props[,prop := ifelse(deaths_total_1 > 50, prop_1,
                          ifelse(deaths_total_2 > 50, prop_2,
                                 ifelse(deaths_total_3 > 50, prop_3,
                                        ifelse(deaths_total_4 > 50, prop_4,
                                               ifelse(deaths_total_5 > 50, prop_5,
                                                      prop_6)))))]

edu_props[,prop_level := ifelse(deaths_total_1 > 50, 'prop_1',
                                ifelse(deaths_total_2 > 50, 'prop_2',
                                       ifelse(deaths_total_3 > 50, 'prop_3',
                                              ifelse(deaths_total_4 > 50, 'prop_4',
                                                     ifelse(deaths_total_5 > 50, 'prop_5',
                                                            'prop_6')))))]

# unknowns at national level
unknown_edu_props <- copy(edu_props)
unknown_edu_props <- unknown_edu_props[,.(deaths_1 = sum(deaths_1)),
                                       .(year,mcnty,state_name,edu_label,race_group)]
unknown_edu_props[,age_group := 999]
unknown_edu_props[,deaths_2 := sum(deaths_1), by = c('year','state_name','edu_label')]
unknown_edu_props[,prop := deaths_2 / sum(deaths_1), by = c('year','state_name')]
unknown_edu_props[,prop_level := 'Unknown_props_national']

#unknown race
unknown_race_props <- copy(edu_props)
unknown_race_props <- unknown_race_props[,.(deaths_1 = sum(deaths_1)),
                                       .(year,mcnty,state_name,edu_label,age_group)]
unknown_race_props[,race_group := 'NH Other Race']
unknown_race_props[,deaths_2 := sum(deaths_1), by = c('year','state_name','age_group','edu_label')]
unknown_race_props[,prop := deaths_2 / sum(deaths_1), by = c('year','state_name','age_group')]
unknown_race_props[,prop_level := 'Unknown_props_national']

edu_props <- rbindlist(list(edu_props,
                   unknown_edu_props, 
                   unknown_race_props),
                   fill = TRUE)

edu_props <- edu_props[,c('year','mcnty','state_name','age_group',
                          'race_group','edu_label','prop',
                          'prop_level')]

#Remove New York 2015 outside of the Boroughs for imputation
edu_props[(year == 2015 & state_name == 'New York' & (!mcnty %in% c(1812,1833,1840,1850,1852))),
          prop := NA]
edu_props[, prop := na.approx(prop, na.rm=TRUE),
          by = c('mcnty','state_name','age_group','race_group','edu_label')]

write.csv(edu_props, paste0('FILEPATH',
                            'mortality_edu_props.csv'),
          row.names = F)
