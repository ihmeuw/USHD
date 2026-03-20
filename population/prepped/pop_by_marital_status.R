###################################################################################################
## Description: Prep and interpolate population by mcnty-year-age-marital status
###################################################################################################

# Load packages -----------------------------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("population$", getwd())) setwd("FILEPATH")
source("_functions/_prep_workspace.R")  # source custom settings for prepped populations
library(dplyr)
library(stringr)
library(labelled)
source("_functions/year_pooling.R")

# Initial setup -----------------------------------------------------------------------------------
raw_dir <- "FILEPATH"
pop_dir <- "FILEPATH"

# Load prepped data and crosswalks ----------------------------------------------------------------
mcnty <- fread("FILEPATH")
loc <- fread("FILEPATH")
acs_ipums_meta <- get_population_metadata("acs_ipums_pop_by_marital")  # load metadata for best extraction
acs_microdata <- readRDS(paste0(pop_dir, "raw/acs_ipums_pop_by_marital/", unique(acs_ipums_meta[, file])))[, -"nid"]  # read in file specified in metadata
acs_meta <- get_population_metadata("acs_pop_by_marital")
acs_tabs <- readRDS(paste0(raw_dir, "acs_pop_by_marital/", unique(acs_meta$file)))
census_ipums_meta <- get_covariate_metadata("census_ipums_pop_by_marital")
census_ipums_pop <- readRDS(paste0(pop_dir, "raw/census_ipums_pop_by_marital/", unique(census_ipums_meta[, file])))[, -"nid"]
census_meta <- get_population_metadata("census_pop_by_marital")
census_pop <- readRDS(paste0(raw_dir, "census_pop_by_marital/", unique(census_meta$file)))[,!'nid']
ref_pop <- get_population_data(population_name = "pop_by_age_sex", year = 1998:2022, age = seq(20, 85, 5))

# crosswalk files are puma to county mappings and weights which determine the proportion of each
# puma that resides in each county
matchDT <- setDT(readRDS("FILEPATH"))
setnames(matchDT, 'year','year_group')

# combine PUMAs that are a single puma in the acs dataset
matchDT[year_group != '2000' & puma %in% c(2201801, 2201802, 2201905), puma := 2277777]
matchDT[puma == 2277777, family := 9999999]

# divide the weights by 3 to ensure that the PUMAs get divided evenly
matchDT[puma == 2277777, w := w/3]

# weights are slightly off, set their sums back to 1
matchDT[, w_r := sum(w), by = c('year_group', 'family', 'puma')]
stopifnot(max(abs(matchDT$w_r - 1)) < .01)  # they should not be very far off from 1
matchDT[w_r == 0, w_r := 1]
matchDT[, w := w / w_r]
matchDT <- matchDT[, -'w_r']

# Process PUMA dataset ----------------------------------------------------------------------------
var_label(acs_microdata$year) <- NULL
acs_microdata[, year := as.integer(year)]
puma_dt <- rbindlist(list(census_ipums_pop, acs_microdata), use.names = T, fill = T)

# create broader age groups such that we can match them to the ACS county tabulations
puma_dt[, broad_agegp := case_when(agegp == "[20,25)" ~ 20,
                                   agegp == "[25,30)" ~ 25,
                                   agegp == "[30,35)" ~ 30,
                                   agegp %in% c("[35,40)", "[40,45)") & year == 2000 ~ 35,
                                   agegp == "[35,40)" & year != 2000 ~ 35,
                                   agegp == "[40,45)" & year != 2000 ~ 40,
                                   agegp %in% c("[45,50)", "[50,55)") & year == 2000 ~ 45,
                                   agegp == "[45,50)" & year != 2000 ~ 45,
                                   agegp == "[50,55)" & year != 2000 ~ 50,
                                   agegp == "[55,60)" ~ 55,
                                   agegp == "[60,65)" ~ 60,
                                   agegp %in% c("[65,70)", "[70,75)") ~ 65,
                                   agegp %in% c("[75,80)","[80,85)") ~ 75,
                                   agegp == "[85,105)" ~ 85,
                                   TRUE ~ 9999)]
# remove unexpected age groups
puma_dt <- puma_dt[broad_agegp != 9999]

# create year grouping to merge onto the crosswalk file
puma_dt[, year_group := case_when(year == 2000 ~ "2000",
                                  year %in% 2001:2009 ~ "2000:2009",
                                  year %in% 2010:2011 ~ "2010:2011",
                                  year %in% 2012:2013 ~ "2012:2013",
                                  year %in% 2014:2015 ~ "2014:2015",
                                  year %in% 2016:2018 ~ "2016:2018",
                                  year %in% 2019:2021 ~ "2019:2021",
                                  TRUE ~ "2022")]

# hot fix for PUMAs 2201801, 2201802, 2201905 which require the 2000 mapping in 2005
puma_dt[year == 2005 & puma %in% c(2201801, 2201802, 2201905), year_group := '2000']

# merge on counties and weights, this will create a much larger dataset, but
# populations * weights should stay the same 
start_pop <- sum(puma_dt$pop)
puma_dt <- merge(puma_dt, matchDT[, -"family"], by = c("year_group", "puma"), all.x = TRUE,
                 allow.cartesian = TRUE)

# assert that all PUMAs have found a mapping
stopifnot(nrow(puma_dt[is.na(w)]) == 0)

# make sure the populations are the exact same
end_pop <- sum(puma_dt$pop * puma_dt$w)
stopifnot(start_pop == end_pop)

# create our initial "county" populations by multiplying our puma populations by our puma-county 
# weights, then aggregating population on counties
puma_dt <- puma_dt[, .(puma_pop = sum(pop*w)), .(year, mcnty, sex, agegp, broad_agegp, marital)]

# pyramid weight IPUMS files
puma_00 <- puma_dt[year == 2000]
puma_dt <- puma_dt[year != 2000]

# pool over 5 years twice to create pyramid weights
puma_dt <- pool_adj_years(puma_dt, 5, 
                          c('mcnty','sex','agegp','broad_agegp','marital'), 
                          'puma_pop')

puma_dt <- pool_adj_years(puma_dt, 5, 
                          c('mcnty','sex','agegp','broad_agegp','marital'), 
                          'puma_pop')

# remove end years
grouped_years <- unique(puma_dt$year)
grouped_years <- (min(grouped_years) + 2):(max(grouped_years) - 2)
puma_dt <- puma_dt[year %in% grouped_years]

puma_dt <- rbind(puma_00, puma_dt)

# Process county dataset --------------------------------------------------------------------------
# bind together the county-level datasets
county_dt <- rbind(census_pop[year == 2000, !"file_path"], acs_tabs[,!"nid"])

# merge on and aggregate by our merged counties
county_dt <- merge(county_dt, loc[,c("cnty","mcnty","state")], 
                   by.x = "fips", by.y = "cnty", all.x = T)

county_dt[, marital := case_when(marital == "former" ~ 2,
                                 marital == "current" ~ 1,
                                 marital == "never" ~ 3,
                                 TRUE ~ NA_real_)]

# exclude under-20-year-olds
# create broad age groups to match the ones we made for the puma dataset
county_dt <- county_dt[age_start > 19]
county_dt_copy <- copy(county_dt)  # create copy to merge back later
county_dt[, broad_agegp := as.numeric(as.character(county_dt$age_start))]

# table(county_dt$broad_agegp, county_dt$year)
county_dt <- county_dt[, .(county_pop = sum(pop)), .(mcnty, year, sex, broad_agegp, marital, state)]

# testing year pooling the county results
county_00 <- county_dt[year == 2000]
county_dt <- county_dt[year != 2000]

# pad end years with terminal years data for 5-year pool
min_year <- min(county_dt$year)
max_year <- max(county_dt$year)

county_dt <- rbindlist(list(county_dt, 
                            county_dt[year == min_year][,year := min_year - 2],
                            county_dt[year == min_year][,year := min_year - 1],
                            county_dt[year == max_year][,year := max_year + 1],
                            county_dt[year == max_year][,year := max_year + 2]),
                       use.names = T, fill = T)

county_dt <- pool_adj_years(county_dt, 5, 
                            c('mcnty','sex','broad_agegp','marital', 'state'), 
                            'county_pop')

# remove end years
grouped_years <- unique(county_dt$year)
grouped_years <- (min(grouped_years) + 2):(max(grouped_years) - 2)
county_dt <- county_dt[year %in% grouped_years]

county_dt <- rbind(county_00, county_dt)

# Combine datasets and rake to county tabs --------------------------------------------------------
raking_dt <- merge(puma_dt, county_dt, 
                   by = c("year", "mcnty", "sex", "marital", "broad_agegp"),
                   all = T)

if(nrow(raking_dt) != nrow(puma_dt)){
  message(paste(nrow(raking_dt[is.na(puma_pop)]), "county strata do not have corresponding puma data"))
}

# create PUMA-county pops by broad age group to match county pops
raking_dt[, puma_pop_broad_agegp := sum(puma_pop), 
          by = c("year", "mcnty", "sex", "marital", "broad_agegp", "state")]

raking_dt[, raking_factor := county_pop / puma_pop_broad_agegp]
raking_dt[is.na(raking_factor), raking_factor := 0]

# calculate raked population
raking_dt[, raked_pop := puma_pop * raking_factor]

#### variable descriptions for reference:
# county_pop: population at the county level for broad age group
# puma_pop_broad_agegp: population at the PUMA level for broad age group
# puma_pop: population at the PUMA level for small age group
# raked_pop: population at the county level for small age group

# Manual fixes for special cases ------------------------------------------------------------------
# cases where puma_pop_broad_agegp == 0: fill in 5-year age group values
raking_dt_sub <- merge(unique(raking_dt[is.na(puma_pop)|puma_pop_broad_agegp == 0, !"agegp"]),
                       unique(raking_dt[!is.na(puma_pop), c("agegp", "broad_agegp", "year")]),
                       by = c("broad_agegp", "year"), allow.cartesian = T)
raking_dt <- rbind(raking_dt[!(is.na(puma_pop)|puma_pop_broad_agegp == 0)], raking_dt_sub)

# cases where strata are missing from the PUMA microdata: use PUMA data to calculate state-level
#   proportion of small over broad age groups by sex, marital, year
puma_dt <- merge(puma_dt, unique(loc[, c("mcnty", "state")]), by = "mcnty")
puma_dt[, state_pop_broad := sum(puma_pop), by = .(year, state, sex, marital, broad_agegp)]
puma_dt[, state_pop_agegp := sum(puma_pop), by = .(year, state, sex, marital, agegp)]
puma_dt[, marital_prop := state_pop_agegp / state_pop_broad]

raking_dt <- merge(raking_dt, unique(puma_dt[, !c("mcnty", "state_pop_broad", "state_pop_agegp", "puma_pop")]),
                   by = c("broad_agegp", "state", "sex", "marital", "agegp", "year"), allow.cartesian = T)

# calculate raked_pop for subset using state-level proportions above
raking_dt[is.na(puma_pop)|puma_pop_broad_agegp == 0, raked_pop := marital_prop * county_pop]
stopifnot(sum(county_dt$county_pop) == sum(raking_dt$raked_pop))

# create new age group
raking_dt[, age := as.integer(str_extract(agegp, "[0-9]{2}"))]
raking_dt <- raking_dt[, .(pop = sum(raked_pop)), .(year, mcnty, sex, age, marital)]

# Interpolate missing year proportions and rake to GBD --------------------------------------------
# assume missing data are zeroes
interp_dt <- data.table(expand.grid(age = unique(raking_dt$age), mcnty = unique(raking_dt$mcnty),
                                    sex = unique(raking_dt$sex), marital = unique(raking_dt$marital),
                                    year = unique(raking_dt$year)))
interp_dt <- merge(interp_dt, raking_dt,
                   by = c("year", "mcnty", "sex", "marital", "age"), all = T)
interp_dt[is.na(pop), pop := 0]

interp_dt <- merge(interp_dt, unique(loc[, c("state", "mcnty")]), by = "mcnty")
interp_dt[, total_pop := sum(pop), by = .(year, mcnty, sex, age)]
interp_dt[, marital_prop := pop / total_pop]
interp_dt[, total_pop_st := sum(pop), by = .(year, state, sex, age)]
interp_dt[, pop_st := sum(pop), by = .(year, state, sex, age, marital)]
interp_dt[is.na(marital_prop), marital_prop := pop_st / total_pop_st]  # NOTE edu sets this to 0

# interpolate inner years
interp_inner <- interp_dt[, lapply(.SD, function(x) approx(x = year, y = x,
                                                           xout = c(2001:2006))$y),
                          by = .(age, mcnty, sex, marital, state), .SDcols = "marital_prop"]
interp_inner[, year := rep(c(2001:2006), times = nrow(interp_inner) / length(c(2001:2006)))]

# use 2021 proportions for terminal year of 2022 (check with each run)
interp_outer <- rbind(interp_dt[year == 2021, -c('pop', 'total_pop')][, year := 2022])
interp_pop <- rbind(interp_dt[, -c("total_pop", "pop", "total_pop_st", "pop_st")],
                    interp_inner, interp_outer[, -c("total_pop_st", "pop_st")])

# merge on GBD raked populations
interp_pop <- merge(interp_pop, ref_pop[, -"state"], by = c("year", "mcnty", "sex", "age"))

# create populations by marital status that are in line with GBD
interp_pop[, pop := pop * marital_prop]
interp_pop <- interp_pop[, c("year", "mcnty", "sex", "age", "marital", "pop")]
stopifnot(round(sum(interp_pop$pop), 4) - round(sum(ref_pop[year %in% unique(interp_pop$year)]$pop), 4) == 0)
rm(ref_pop, acs_tabs, acs_microdata, census_pop, census_ipums_pop)  # help clear some memory

# Make diagnostic plots ---------------------------------------------------------------------------
plot_dt <- merge(interp_pop, unique(mcnty[, c('mcnty', 'state_name')]), by = 'mcnty')
date_time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
out_dir <- paste0(pop_dir, 'plots/pop_by_marital_status/', date_time_stamp, "/")
dir.create(out_dir, showWarnings = F)

pdf(paste0(out_dir, 'MCNTY_interp_pop_prop.pdf'), width = 14, height = 8)
for(state in unique(plot_dt$state_name)) {
  d2 <- plot_dt[state_name == state & age == 20]
  d2 <- d2[, .(pop = sum(pop)), .(mcnty, year, marital)]
  d2[, pop_total := sum(pop), by = c('mcnty','year')]
  d2[, prop := pop / pop_total]
  d2[is.na(prop), prop := 0]
  gg <- ggplot(data = d2, aes(x = year, y = prop, colour = factor(marital))) +
    geom_line() +
    facet_wrap(~ mcnty, scales = 'free') +
    ggtitle(paste0(state, ': Interp prop ages 20-24')) +
    scale_color_manual(name = "marital",
                       values = c("1" = "#66c2a5", "2" = "#fc8d62", "3" = "#8da0cb"),
                       labels = c("current", "former", "never")) +
    theme(strip.text = element_text(size = 6, margin = margin(0, 0, 0, 0, 'pt')))
  print(gg)
}
dev.off()

pdf(paste0(out_dir, 'STATES_interp_pop_prop.pdf'), width = 14, height = 8)
for(state in unique(plot_dt$state_name)) {
  d2 <- plot_dt[state_name == state & age == 20]
  d2 <- d2[, .(pop = sum(pop)), .(year, marital)]
  d2[, pop_total := sum(pop), by = c('year')]
  d2[, prop := pop / pop_total]
  d2[is.na(prop), prop := 0]
  gg <- ggplot(data = d2, aes(x = year, y = prop, colour = factor(marital))) +
    geom_line() +
    ggtitle(paste0(state, ': Interp prop ages 20-24')) +
    scale_color_manual(name = "marital",
                       values = c("1" = "#66c2a5", "2" = "#fc8d62", "3" = "#8da0cb"),
                       labels = c("current", "former", "never"))
  print(gg)
}
dev.off()

# Save output --------------------------------------------------------------
saveRDS(interp_pop, "FILEPATH")
