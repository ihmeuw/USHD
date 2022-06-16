#################################################################
# Description: Appends and formats CoD files into a single file 
#              for modeling
# Inputs:
#   source: Specifies the type data to grab
#     EX: county, county_race_ethn, county_edu
#################################################################

rm(list = ls())

library(data.table)
library(doParallel)
library(openxlsx)
library(odbc)
library(DBI)
library(ini)

SOURCE <- 'county_race_ethn'
OUT_DIR <- 'FILEPATH'
dir.create(OUT_DIR, showWarnings = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_location_metadata <- function(dt, cols = c('mcnty')){
  # location metadata
  locs <- fread('FILEPATH')[
    , c(cols,'location_id'), with = F]
  #round due to weird issues post merges that are fine
  death_count <- round(sum(dt$deaths),4)
  dt <- merge(dt, locs, by = 'location_id', all.x = T)

  if(death_count != round(sum(dt$deaths), 4)){
    stop(paste0('add_location_metadata failed for year: ',unique(dt$year_id)))
  }

  #discard non county level aggregates
  dt <- dt[location_id > 573]
  return(dt)
}

# change age_group_ids from the ones used in CoD
# to the ones that correspond to our age groups
fix_age_group_ids <- function(dt){
  
  dt[age_group_id %in% c(2:4,388,389), age_group_id := 28]
  dt[age_group_id %in% c(5,238), age_group_id := 34]
  dt[age_group_id %in% c(30:33,235), age_group_id := 160]

  return(dt)
}

format_demo_cols <- function(dt){

  if ('edu_group_id' %in% names(dt)){
    edu_map <- data.table(edu_group_id = c(0,1,2,3,4),
                          edu_label = c('Unknown', 'Less than HS',
                                        'HS graduate', 'Some college',
                                        'College graduate'),
                          educational_attainment_id = c(100,101,102,103,104))
    dt <- merge(dt, edu_map, by = 'edu_group_id', all.x = T)
  }

  if('population_group_id' %in% names(dt)){
    setnames(dt, 'population_group_id', 'race')
  }

  return(dt)
}

# use this function to fill in any weird gbd
# cause aggregations
aggregate_causes <- function(dt){
  dt[cause_id %in% c(716,940), cause_id := 1056]
}


get_aggregation <- function(year, source = SOURCE){

  dt <- fread('FILEPATH')

  dt <- dt[,intersect(names(dt),
                      c('population_group_id', 'edu_group_id', 'location_id',
                        'year_id', 'nid', 'age_group_id', 'sex_id', 'cause_id', 'deaths')),
           with = FALSE]

  dt <- add_location_metadata(dt, cols = c('mcnty'))

  dt <- fix_age_group_ids(dt)

  dt <- format_demo_cols(dt)
  
  dt <- aggregate_causes(dt)

  dt <- dt[,.(deaths = sum(deaths)),
           intersect(names(dt), c('year_id', 'nid', 'mcnty', 'age_group_id', 'sex_id', 'educational_attainment_id',
                                 'race', 'race_label', 'cause_id'))]

  return(dt)
}


#~~~~~~~~~~~~~~~~~~~~~~~ Run code ~~~~~~~~~~~~~~~~~~~~~~~
year_list <- 2000:2019

# format and write data to L:/
for(year in year_list){
  message(paste('Writing', year, 'final CoD data'))
  dt <- get_aggregation(year, SOURCE)
  dt <- dt[,-'nid']
  saveRDS(dt, file = paste0(OUT_DIR, 'cod_deaths_', year, '.RDS'))
}