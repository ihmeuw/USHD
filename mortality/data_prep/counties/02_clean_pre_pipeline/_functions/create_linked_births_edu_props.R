#################################################################
# Description: Create proportions of educational attainment from
#              the not unknown parts of the linked births files.
#################################################################

rm(list = ls())
library(data.table)
library(zoo)
repo_dir <- "FILEPATH"
functions_dir <- "FILEPATH"
parent_dir <- "FILEPATH"
source(paste0(functions_dir, 'prep_functions.R'))

# mcnty mapping
mcntys <- fread('"FILEPATH"')

# get list of reporting state-years to filter on
reporting_states <- fread("FILEPATH")
setnames(reporting_states, 'state_alpha', 'state_res_alpha')  # rename for easier merging

get_cleaned_data <- function(file_path){
  dt <- fread(file_path)
  dt <- dt[,c('state_res_alpha','mother_age',
              'mother_education','full_fips_res_numeric',
              'year','deaths')]

  # keep only 50 states + DC 
  dt <- dt[state_res_alpha %in% c(state.abb, 'DC')]
  
  # bin ages
  dt <- merge(dt, data.table(mother_age = c(10,15,20,25,
                                            30,35,40,45,50),
                             age_group = c(1,2,3,4,5,5,
                                           6,6,6)),
              by = 'mother_age')
  
  # add merged counties
  dt <- merge(dt, unique(mcntys[,.(cnty, mcnty)]),
              by.x='full_fips_res_numeric',
              by.y = 'cnty',
              allow.cartesian = T,
              all.x = T)
  
  # add on education labels
  dt <- merge(dt, data.table(mother_education = c(1,2,3,4,5,9),
                             edu = c(1,1,2,3,4,0),
                             edu_label = c('Less than HS',
                                           'Less than HS',
                                           'HS graduate', 
                                           'Some college',
                                           'College graduate',
                                           'Unknown')), 
              by = 'mother_education')
  
  # collapse
  dt <- dt[,.(deaths_1 = sum(deaths)),.(year,mcnty,age_group,edu_label)]
  return(dt)
}

# grab metro and non metro mcntys
# code below grabs the most recent version based on date/time of creation
files <- file.info(list.files("FILEPATH", full.names = TRUE))
file <- rownames(files)[which.max(files$mtime)]
urban_cov <- data.table(readRDS(file)); rm(file)
urban_cov <- merge(urban_cov, unique(mcntys[,c('mcnty','cnty')]), 
                   by.x = 'fips', by.y = 'cnty', all.x = T)
urban_cov[,metro := ifelse(rur_urb_code %in% 1:3,1,0)]
urban_cov[mcnty %in% c(101,1809), metro := 1]
urban_cov <- unique(urban_cov[,c('year','mcnty','metro')])
setnames(urban_cov, 'year','year_group')
urban_cov <- merge(data.table(year_group = c(rep(1993,14),
                                             rep(2003,10),
                                             rep(2013,7)),
                              year = 1989:2019),
                   urban_cov, by = c('year_group'),
                   allow.cartesian = T)[,-'year_group']

################################################################################
# CREATE PROPORTIONS
################################################################################
# get filenames
root <- "FILEPATH"
files <- list.files(root, pattern='.csv')
files <- files[files %like% 'cleaned']

# read in and format cleaned dataset
edu_props <- rbindlist(lapply(paste0(root, files),get_cleaned_data))

# # square the dataset
edu_props <- merge(data.table(expand.grid(year = unique(edu_props$year),
                                          mcnty = unique(edu_props$mcnty),
                                          age_group = unique(edu_props$age_group),
                                          edu_label = unique(edu_props$edu_label))),
                   edu_props,
                   by = c('year','mcnty','age_group','edu_label'),
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
report_states <- unique(reporting_states[,c('year','state_fips','state_res_alpha','reporting_status')])
report_states <- merge(report_states, 
                       unique(mcntys[,c('state','state_name')]),
                       by.x = 'state_fips', by.y = 'state',
                       all = TRUE)
report_states[state_res_alpha == 'NYC', state_name := 'NYC']

edu_props <- merge(edu_props, 
                   report_states[,c('year','state_name','reporting_status')],
                   by = c('year','state_name'),
                   all.x = T)

edu_props <- edu_props[reporting_status == 1]

# merge on metro info
edu_props <- merge(edu_props, urban_cov, by = c('year','mcnty'), all.x = T)

# create year bins
edu_props <- merge(edu_props,
                   data.table(year = 1989:2019,
                              year_pool = c(rep(1:10, each = 3),10)),
                   by = 'year', all = T)

edu_props <- edu_props[edu_label != 'Unknown']

edu_props[,deaths_total_1 := sum(deaths_1), by = c('year','mcnty',
                                                   'age_group')]
edu_props[,prop_1 := deaths_1 / deaths_total_1]

edu_props[,deaths_2 := sum(deaths_1), by = c('year_pool','mcnty','state_name',
                                             'age_group','edu_label')]
edu_props[,deaths_total_2 := sum(deaths_1), by = c('year_pool','mcnty','state_name',
                                                   'age_group')]
edu_props[,prop_2 := deaths_2 / deaths_total_2]

edu_props[,deaths_3 := sum(deaths_1), by = c('year','metro','state_name','age_group',
                                             'edu_label')]
edu_props[,deaths_total_3 := sum(deaths_1), by = c('year','metro','state_name',
                                                   'age_group')]
edu_props[,prop_3 := deaths_3 / deaths_total_3]

edu_props[,deaths_4 := sum(deaths_1), by = c('year_pool','metro','state_name',
                                             'age_group','edu_label')]
edu_props[,deaths_total_4 := sum(deaths_1), by = c('year_pool','metro','state_name',
                                                   'age_group')]
edu_props[,prop_4 := deaths_4 / deaths_total_4]

edu_props[,deaths_5 := sum(deaths_1), by = c('year','metro','age_group',
                                             'edu_label')]
edu_props[,deaths_total_5 := sum(deaths_1), by = c('year','metro',
                                                   'age_group')]
edu_props[,prop_5 := deaths_5 / deaths_total_5]

edu_props[,deaths_6 := sum(deaths_1), by = c('year_pool','metro','age_group',
                                             'edu_label')]
edu_props[,deaths_total_6 := sum(deaths_1), by = c('year_pool','metro',
                                                   'age_group')]
edu_props[,prop_6 := deaths_6 / deaths_total_6]

edu_props[,prop := ifelse(deaths_total_1 > 25, prop_1,
                          ifelse(deaths_total_2 > 25, prop_2,
                                 ifelse(deaths_total_3 > 25, prop_3,
                                        ifelse(deaths_total_4 > 25, prop_4,
                                               ifelse(deaths_total_5 > 25, prop_5,
                                                      prop_6)))))]

edu_props[,prop_level := ifelse(deaths_total_1 > 25, 'prop_1',
                                ifelse(deaths_total_2 > 25, 'prop_2',
                                       ifelse(deaths_total_3 > 25, 'prop_3',
                                              ifelse(deaths_total_4 > 25, 'prop_4',
                                                     ifelse(deaths_total_5 > 25, 'prop_5',
                                                            'prop_6')))))]

edu_props <- edu_props[,c('year','mcnty','state_name','age_group',
                          'edu_label','prop','prop_level')]

write.csv(edu_props, "FILEPATH",
          row.names = F)
