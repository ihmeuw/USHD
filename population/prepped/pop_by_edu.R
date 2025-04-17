###################################################################################################
## Description: Prep population by mcnty-year-age-educational attainment. 
###################################################################################################

rm(list = ls())

# Setup -------------------------------------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("population$", getwd())) setwd("FILEPATH")
source("_functions/_prep_workspace.R")  # source custom settings for prepped populations
library(dplyr)
library(stringr)
library(labelled)
source("_functions/year_pooling.R")

pop_dir <- "FILEPATH"

# Load prepped data and crosswalks ----------------------------------------------------------------
mcnty <- fread("FILEPATH")
loc2 <- fread("FILEPATH")
acs_ipums_meta_df <- get_covariate_metadata("acs_ipums_pop_by_edu")  # load metadata for best extraction
acs_microdata <- readRDS(paste0(pop_dir, "raw/acs_ipums_pop_by_edu/", unique(acs_ipums_meta_df[, file])))[year <= 2023, -"nid"]  # read in file specified in metadata
acs_meta_df <- get_covariate_metadata("acs_pop_by_edu")
acs_tabs <- readRDS(paste0(pop_dir, "raw/acs_pop_by_edu/", unique(acs_meta_df[, file])))[, -"nid"]
census_pop_meta_df <- get_covariate_metadata("census_pop_by_edu")
census_pop <- readRDS(paste0(pop_dir, "raw/census_pop_by_edu/", unique(census_pop_meta_df[, file])))[, -c("nid", "file_path")]
census_ipums_meta_df <- get_covariate_metadata("census_ipums_pop_by_edu")
census_ipums_pop <- readRDS(paste0(pop_dir, "raw/census_ipums_pop_by_edu/", unique(census_ipums_meta_df[, file])))[, -"nid"]
ref_pop <- get_population_data(covariate_dataset_id = 289, year = 1998:2022, age = seq(20, 85, 5))
ref_pop <- ref_pop[,.(pop = sum(pop)),.(year,edu,sex,mcnty,state,age)]

# crosswalk files are puma to county mappings and weights which determine the proportion of each
# puma that resides in each county
matchDT <- setDT(readRDS("FILEPATH"))
setnames(matchDT, 'year','year_group')

# combine pumas that are a single puma in the acs dataset
matchDT[year_group != '2000' & puma %in% c(2201801, 2201802, 2201905), puma := 2277777]
matchDT[puma == 2277777, family := 9999999]

# divide the weights by 3 to ensure that the pumas get divided evenly
matchDT[puma == 2277777, w := w/3]

# weights are slightly off, set their sums back to 1
matchDT[, w_r := sum(w), by = c('year_group', 'family', 'puma')]
stopifnot(max(abs(matchDT$w_r - 1)) < .01)  # they should not be very far off from 1
matchDT[w_r == 0, w_r := 1]
matchDT[, w := w / w_r]
matchDT <- matchDT[, -'w_r']

# Process PUMA dataset ----------------------------------------------------------------------------
# bind Census and ACS puma datasets together
var_label(acs_microdata$year) <- NULL
acs_microdata[, year := as.integer(year)]
puma_dt <- rbind(census_ipums_pop, acs_microdata)

# create broader age groups such that we can match them to the acs county tabulations
# merge on dt, or look into switch
# broad agegp 0 will not be raked to acs tabs
puma_dt[, broad_agegp := ifelse(agegp == '[20,25)', 0,
                                ifelse(agegp %in% c('[25,30)','[30,35)'), 1,
                                       ifelse(agegp %in% c('[35,40)','[40,45)'), 2,
                                              ifelse(agegp %in% c('[45,50)','[50,55)', '[55,60)','[60,65)'), 3, 
                                                     ifelse(agegp %in% c('[65,70)','[70,75)','[75,80)','[80,85)','[85,105)'), 4,
                                                            NA)))))]
puma_dt <- puma_dt[!is.na(broad_agegp)]
puma_dt <- puma_dt[, .(pop = sum(pop)), .(puma, year, sex, agegp, edu, broad_agegp)]

# create year grouping to merge onto the crosswalk file
puma_dt[, year_group := ifelse(year == 2000, '2000',
                               ifelse(year %in% 2001:2009, '2000:2009',
                                      ifelse(year %in% 2010:2011, '2010:2011',
                                             ifelse(year %in% 2012:2013, '2012:2013',
                                                    ifelse(year %in% 2014:2015, '2014:2015',
                                                           ifelse(year %in% 2016:2018, '2016:2018',
                                                                  ifelse(year %in% 2019:2021, '2019:2021', '2022')))))))]

# hot fix for PUMAs 2201801, 2201802, 2201905 which require the 2000 mapping in 2005
puma_dt[year == 2005 & puma %in% c(2201801, 2201802, 2201905), year_group := '2000']

# merge on counties and weights; this will create a much larger dataset, but
# populations * weights should stay the same 
start_pop <- sum(puma_dt$pop)
puma_dt <- merge(puma_dt, matchDT[, -'family'], by = c('year_group', 'puma'), all.x = TRUE, allow.cartesian = TRUE)

# assert that all pumas have found a mapping
stopifnot(nrow(puma_dt[is.na(w)]) == 0)

# make sure the populations are the exact same
end_pop <- sum(puma_dt$pop * puma_dt$w)
stopifnot(start_pop == end_pop)

# create our initial "county" populations by
# multiplying our puma populations by our puma-county 
# weights, then aggregating population on counties
puma_dt <- puma_dt[, .(puma_pop = sum(pop * w)), .(year, mcnty, sex, agegp, broad_agegp, edu)]

# pyramid weight ipums files
puma_00 <- puma_dt[year == 2000]
puma_dt <- puma_dt[year != 2000]

# Pool over 5 years twice to create pyramid weights
puma_dt <- pool_adj_years(puma_dt, 5, 
                          c('mcnty','sex','agegp','broad_agegp','edu'), 
                          'puma_pop')

puma_dt <- pool_adj_years(puma_dt, 5, 
                          c('mcnty','sex','agegp','broad_agegp','edu'), 
                          'puma_pop')

# remove end years
grouped_years <- unique(puma_dt$year)
grouped_years <- (min(grouped_years) + 2):(max(grouped_years) - 2)
puma_dt <- puma_dt[year %in% grouped_years]

puma_dt <- rbind(puma_00, puma_dt)

# square the dataset
# assume missing data are zeroes
puma_dt_squared <- data.table(expand.grid(agegp = unique(puma_dt$agegp), mcnty = unique(puma_dt$mcnty),
                                          sex = unique(puma_dt$sex), edu = unique(puma_dt$edu),
                                          year = unique(puma_dt$year)))
puma_dt_squared[, broad_agegp := ifelse(agegp == '[20,25)', 0,
                                        ifelse(agegp %in% c('[25,30)','[30,35)'), 1,
                                               ifelse(agegp %in% c('[35,40)','[40,45)'), 2,
                                                      ifelse(agegp %in% c('[45,50)','[50,55)', '[55,60)','[60,65)'), 3, 
                                                             ifelse(agegp %in% c('[65,70)','[70,75)','[75,80)','[80,85)','[85,105)'), 4,
                                                                    NA)))))]

puma_dt_squared <- merge(puma_dt_squared, puma_dt,
                         by = c('year', 'mcnty', 'sex', 'edu', 'agegp', 'broad_agegp'), all = T)
puma_dt_squared[is.na(puma_pop), puma_pop := 0]

# pool over ages
puma_dt_age_pool <- data.table()
age_groups <- sort(unique(as.character(puma_dt_squared$agegp)))
for(a in age_groups){
  if(a == '[20,25)'){
    age_pool <- age_groups[1:2]
  }else if(a == '[85,105)'){
    age_pool <- age_groups[(length(age_groups) - 1):length(age_groups)]
  }else{
    age_pool <- age_groups[(match(a, age_groups) - 1):(match(a, age_groups) + 1)]
  }
  puma_dt_age_pool_a <- puma_dt_squared[agegp %in% age_pool]
  puma_dt_age_pool_a <- puma_dt_age_pool_a[,puma_pop_age_pooled := sum(puma_pop),.(year,mcnty,sex,edu)]
  puma_dt_age_pool_a <- puma_dt_age_pool_a[agegp == a]
  
  puma_dt_age_pool <- rbind(puma_dt_age_pool, puma_dt_age_pool_a)
}

# rake age pooled pop back down to puma pop
puma_dt_age_pool[,raking_factor := sum(puma_pop) / sum(puma_pop_age_pooled), .(year,mcnty,sex,agegp)]
puma_dt_age_pool[, puma_pop_age_pooled := puma_pop_age_pooled * raking_factor]

# mark where a 0 occurs only in that specific age group
puma_dt_age_pool[,use_age_pool := ifelse(puma_pop == 0 & puma_pop_age_pooled > 0, 1, 0)]

# mark the entire strata regardless of edu
puma_dt_age_pool[,use_age_pool := max(use_age_pool), .(year,mcnty,sex,agegp)]

# swap marked values to pooled values
puma_dt_age_pool[use_age_pool == 1, puma_pop := puma_pop_age_pooled]

puma_dt <- puma_dt_age_pool[,c('year','mcnty','sex','edu','agegp','broad_agegp','puma_pop')]

# Process county dataset ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# bind together the county level datasets
county_dt <- rbind(census_pop[year == 2000], acs_tabs)

# merge on and aggregate by our merged counties
county_dt <- merge(county_dt, mcnty[, c('cnty','mcnty')], by.x = 'fips', by.y = 'cnty', all.x = T)

# ensure all counties merged properly
stopifnot(nrow(county_dt[is.na(mcnty)]) == 0)

# remove under 25 year olds
county_dt <- county_dt[age_start != 18]

# create broad age groups to match the ones we made for the puma dataset
county_dt[, broad_agegp := ifelse(age_start == 25, 1,
                                  ifelse(age_start == 35, 2,
                                         ifelse(age_start == 45, 3, 4)))]

county_dt <- county_dt[, .(county_pop = sum(pop)), .(mcnty, year, sex, broad_agegp, edu)]

county_00 <- county_dt[year == 2000]
county_dt <- county_dt[year != 2000]

# pad end years with terminal years data for 5-year pool
min_year <- min(county_dt$year)
max_year <- max(county_dt$year)

county_dt <- rbind(county_dt, 
                   county_dt[year == min_year][,year := min_year - 2],
                   county_dt[year == min_year][,year := min_year - 1],
                   county_dt[year == max_year][,year := max_year + 1],
                   county_dt[year == max_year][,year := max_year + 2])
county_dt <- pool_adj_years(county_dt, 5, 
                            c('mcnty','sex','broad_agegp','edu'), 
                            'county_pop')

# remove end years
grouped_years <- unique(county_dt$year)
grouped_years <- (min(grouped_years) + 2):(max(grouped_years) - 2)
county_dt <- county_dt[year %in% grouped_years]

county_dt <- rbind(county_00, county_dt)

# Combine datasets and rake to county tabs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
raking_dt <- merge(puma_dt, county_dt,
                   by = c('year', 'mcnty', 'sex', 'edu', 'broad_agegp'),
                   all = T)
stopifnot(nrow(raking_dt) == nrow(puma_dt))

# create puma-county pops by broad age group to match county pops
raking_dt[, puma_pop_broad_agegp := sum(puma_pop), by = c('year','mcnty','sex','edu','broad_agegp')]

# rake puma-counties to counties
raking_dt[,raking_factor := county_pop / puma_pop_broad_agegp]
# hot fix for new age group
raking_dt[broad_agegp != 0 & is.na(raking_factor), raking_factor := 0]
raking_dt[broad_agegp == 0 & is.na(raking_factor), raking_factor := 1]
raking_dt[, raked_pop := puma_pop * raking_factor]

# counties do not have broad agegp 0
stopifnot(sum(county_dt$county_pop) == sum(raking_dt[broad_agegp != 0]$raked_pop))

# create new age group
raking_dt[, age := as.integer(str_extract(agegp, "[0-9]{2}"))]
raking_dt <- raking_dt[, .(pop = sum(raked_pop)), .(year, mcnty, sex, age, edu)]

# Interpolate missing year proportions and rake to GBD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# assume missing data are zeroes
interp_dt <- data.table(expand.grid(age = unique(raking_dt$age), mcnty = unique(raking_dt$mcnty),
                                    sex = unique(raking_dt$sex), edu = unique(raking_dt$edu),
                                    year = unique(raking_dt$year)))

interp_dt <- merge(interp_dt, raking_dt, by = c('year', 'mcnty', 'sex', 'edu', 'age'), all = T)
interp_dt[is.na(pop), pop := 0]

# Loving Texas (the least populous county in the US) has issues matching populations between NCHS and ACS due
# to missingness. Handle this by pooling years to create proportions
# Since files are already created from 5 year pools, we throw out overlapping years to create a year pool from
# 2000-2009 (using the 2000 census and the 2007 acs microdata) & 2010-2021 (using the 2012 and 2020 acs microdata)
interp_dt[, pooled_years := ifelse(year %in% 2000:2009, 1, 2)]
interp_dt[(mcnty == 2655) & (!year %in% c(2000, 2007, 2012, 2021)), pop := 0]
interp_dt[mcnty == 2655, pop := sum(pop), .(mcnty, sex, edu, age, pooled_years)]
interp_dt <- interp_dt[, -'pooled_years']

# create education group population proportions
interp_dt[, total_pop := sum(pop), by = .(year, mcnty, sex, age)]
interp_dt[, edu_prop := pop / total_pop]
interp_dt[is.na(edu_prop), edu_prop := 0]

# interpolate inner years
interp_inner <- interp_dt[, lapply(.SD, function(x) approx(x = year, y = x, xout = c(2001:2006))$y),
                          by = .(age, mcnty, sex, edu), .SDcols = 'edu_prop']
interp_inner[, year := rep(c(2001:2006), times = nrow(interp_inner) / length(c(2001:2006)))]

# use 2000 and 2021 proportions for terminal years
interp_outer <- rbind(interp_dt[year == 2000, -c('pop', 'total_pop')][, year := 1998],
                      interp_dt[year == 2000, -c('pop', 'total_pop')][, year := 1999],
                      interp_dt[year == 2021, -c('pop', 'total_pop')][, year := 2022])

interp_pop <- rbind(interp_dt[, -c('total_pop', 'pop')], interp_inner, interp_outer)

# merge on GBD raked populations
ref_pop <- ref_pop[, list(mcnty, year, sex, age, pop)]  # select variables of interest
interp_pop <- merge(interp_pop, ref_pop, by = c('year', 'mcnty', 'sex', 'age'))

# create populations by education that are in line with GBD
interp_pop[, pop := pop * edu_prop]

# set edu ids
setnames(interp_pop, 'edu', 'edu_label')
interp_pop[, edu_label := case_when(edu_label == 'less than HS' ~ 'Less than HS',  # standardize labels
                                    edu_label == 'HS grad' ~ 'HS graduate',
                                    edu_label == 'some college' ~ 'Some college',
                                    edu_label == 'college grad' ~ 'College graduate',
                                    TRUE ~ 'Unknown')]
interp_pop[, edu := ifelse(edu_label == 'Less than HS', 101,  # add edu IDs based on labels
                           ifelse(edu_label == 'HS graduate', 102,
                                  ifelse(edu_label == 'Some college', 103,
                                         ifelse(edu_label == 'College graduate', 104, 100))))]
interp_pop <- interp_pop[, c('year', 'mcnty', 'sex', 'age', 'edu', 'edu_label', 'pop')]

stopifnot(round(sum(interp_pop$pop), 8) - round(sum(ref_pop$pop), 8) == 0)

# Make diagnostic plots ---------------------------------------------------------------------------
data <- merge(interp_pop, unique(mcnty[, c('mcnty', 'state_name')]), by = 'mcnty')
date_time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
out_dir <- paste0(pop_dir, 'plots/pop_by_edu/', date_time_stamp, "/")
dir.create(out_dir, showWarnings = F)

pdf(paste0(out_dir, 'MCNTY_interp_pop_prop.pdf'), width = 14, height = 8)
for(state in unique(data$state_name)) {
  d2 <- data[state_name == state & age == 20]
  d2 <- d2[, .(pop = sum(pop)), .(mcnty, year, edu)]
  d2[, pop_total := sum(pop), by = c('mcnty','year')]
  d2[, prop := pop / pop_total]
  d2[is.na(prop), prop := 0]
  gg <- ggplot(data = d2, aes(x = year, y = prop, colour = factor(edu))) +
    geom_line() +
    facet_wrap(~ mcnty, scales = 'free') +
    ggtitle(paste0(state, ': Interp prop 20-24')) +
    scale_color_manual(name = "edu",
                       values = c("101" = "#000000", "102" = "#999933",
                                  "103" = "#6699CC", "104" = "#44AA99"),
                       labels = c("less than HS", "HS grad", "some college", "college grad")) +
    theme(strip.text = element_text(size = 6, margin = margin(0, 0, 0, 0, 'pt')))
  print(gg)
}
dev.off()

pdf(paste0(out_dir, 'STATES_interp_pop_prop.pdf'), width = 14, height = 8)
for(state in unique(data$state_name)) {
  d2 <- data[state_name == state & age == 20]
  d2 <- d2[, .(pop = sum(pop)), .(year, edu)]
  d2[, pop_total := sum(pop), by = c('year')]
  d2[, prop := pop / pop_total]
  d2[is.na(prop), prop := 0]
  gg <- ggplot(data = d2, aes(x = year, y = prop, colour = factor(edu))) +
    geom_line() +
    ggtitle(paste0(state, ': Interp prop 20-24')) +
    scale_color_manual(name = "edu",
                       values = c("101" = "#000000", "102" = "#999933",
                                  "103" = "#6699CC", "104" = "#44AA99"),
                       labels = c("less than HS", "HS grad", "some college", "college grad"))
  print(gg)
}
dev.off()

# Save output --------------------------------------------------------------
saveRDS(interp_pop, "FILEPATH")
