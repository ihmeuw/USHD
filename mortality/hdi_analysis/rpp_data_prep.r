## Prepare regional price parity information for adjusting household consumption
library(readstata13)
library(haven)
library(readxl)
library(survey)
library(ggplot2)
library(RColorBrewer)
library(stats)
library(ggpubr)
library(sf)
library(gridExtra)
library(data.table)
library(mgsub)
library(dplyr)

years <- 2008:2021

locations <- fread("[MCNTY_LOCATION_INFORMATION]")

rpp_directory <- "[RPP_DATA_DIRECTORY]"
rpp_filename <- "[RPP_FILENAME]"
msa_rpp_filename <- "[MSA_RPP_FILENAME]"

# Shape files, to be uesed for vetting
puma_2012 <- readRDS("[PUMA_SHAPEFILE]")
puma_2012 <- as(puma_2012, "sf")

mcnty <- readRDS("[MCNTY_SHAPEFILE]")
mcnty <- as(mcnty, "sf")

state <- readRDS("[STATE_SHAPEFILE]")
state <- as(state, "sf")

# county to puma crosswalk
proj_dir <- "[PROJECT_DIRECTORY]"
county_puma_xw <- readRDS(file.path(proj_dir,"count_to_puma_xw.rds"))
plot_dir <- file.path(proj_dir,"plots")

# msa mapping
msa_mapping <- as.data.table(read_xls("[MSA_MAPPING_FILE]", skip = 2))
msa_mapping <- msa_mapping[1:1882,c(1,10,11)]
setnames(msa_mapping, c('cbsa','state','county'))
msa_mapping[,cnty := as.numeric(paste0(state, county))]
msa_mapping <- merge(msa_mapping[state <= 56],
                     unique(locations[,c('cnty','mcnty')]),
                     by = 'cnty',
                     all.x = T)
msa_mapping <- unique(msa_mapping[,c('cbsa','mcnty')])

cbsa_pop <- as.data.table(openxlsx::read.xlsx("[CBSA_POPULATION_FILE]"))
puma_pop <- as.data.table(readRDS('[PUMA_POPULATION_FILE]'))
setnames(puma_pop, 'version', 'year')
puma_pop <- puma_pop[,.(pop = sum(pop)),.(puma,year)]

## Process data
rpp <- fread(file.path(rpp_directory, rpp_filename), fill = T)
# lines 515 and up are just notes, so get rid of them
rpp <- rpp[1:515,]

rpp_msa <- fread(file.path(rpp_directory, msa_rpp_filename), fill = T)
rpp_msa <- rpp_msa[1:1930]

# General formatting
process_rpp <- function(rpp_tmp) {
  rpp_tmp <- rpp_tmp[GeoFIPS != "00000"] # get rid of the total US values
  rpp_tmp <- copy(rpp_tmp)
  rpp_tmp <- rpp_tmp[Description %like% "All items"] # want to include price adjustment for all types of goods
  rpp_tmp[,c("Region","LineCode","IndustryClassification","Description","Unit", "TableName") := NULL] # delete unused columns

  rpp_tmp <- melt.data.table(rpp_tmp, id.vars = c("GeoFIPS", "GeoName"), variable.name = "value", value.name = "price_adj")
  rpp_tmp$price_adj <- as.numeric(rpp_tmp$price_adj)
  rpp_tmp$value <- as.numeric(as.character(rpp_tmp$value))

  return(rpp_tmp)
}

rpp <- process_rpp(rpp)

rpp_msa <- process_rpp(rpp_msa)
rpp_msa <- rpp_msa[!(GeoName %like% "United States")]
rpp_msa[,state := tstrsplit(GeoName,",")[2]]
rpp_msa$state <- gsub(" \\(Metropolitan Statistical Area\\)", "", rpp_msa$state)

# Format location information
# State/metro/nonmetro
rpp[,state := as.integer(substr(GeoFIPS, start = 1, stop = 2))]
rpp[,urban_rural := as.integer(substr(GeoFIPS, start = 3, stop = 5))]
rpp[,urban_rural_code := ifelse(urban_rural == 998, "urban", "rural")]

setnames(rpp, "value", "year")
rpp <- melt.data.table(rpp, id.vars = c("state","urban_rural_code","year"),
                       measure.vars=c("price_adj"))
rpp <- rpp[year %in% years]

# Exclude 0's - these are locations where there is either no urban or no non-urban portion
rpp[,max_by_state := max(value), by=c("state","year")]

# get the states with 0, and see if these align with the states that have no MSAs
states_no_rural <- unique(rpp[value == 0, state])
unique(locations[state %in% states_no_rural, state_name])
# Notes:
# Delaware composed entirely of MSAs: https://www2.census.gov/programs-surveys/metro-micro/reference-maps/2020/state-maps/10_Delaware_2020.pdf
# DC is entirely metro. Makes sense
# NJ: https://www2.census.gov/geo/maps/metroarea/stcbsa_pg/Feb2013/cbsa2013_NJ.pdf
# RI: https://www2.census.gov/geo/maps/metroarea/stcbsa_pg/Feb2013/cbsa2013_RI.pdf
# In contrast, take Pennsylvania: https://www2.census.gov/programs-surveys/metro-micro/reference-maps/2020/state-maps/42_Pennsylvania_2020.pdf

# drop these rows, since they should all be mapped to MSAs
rpp <- rpp[value != 0]

## save for plotting later
rpp_state_2015 <- copy(rpp[year == 2015])

## Now, we need to map each of these to the PUMA level
rpp <- merge(rpp, unique(locations[,.(mcnty, state)]), by=c("state"), allow.cartesian = T)
rpp[,variable := NULL]

stopifnot(nrow(rpp[is.na(value)]) == 0)
stopifnot(setdiff(locations[current == 1, mcnty], unique(rpp$mcnty)) == 0)

# Process rpp_msa to match rpp
# Fix Dayton Ohio's cbsa number
rpp_msa[GeoFIPS == 19430, GeoFIPS := 19380]

# Use 2013 numbers for 2008:2012 for Enid OK
enid_2013 <- rpp_msa[GeoFIPS == 21420 & value == 2013]$price_adj
rpp_msa[GeoFIPS == 21420 & is.na(price_adj), price_adj := enid_2013]

# Use 2015 numbers for 2008:2014 for Twin Falls ID
twinfalls_2015 <- rpp_msa[GeoFIPS == 46300 & value == 2015]$price_adj
rpp_msa[GeoFIPS == 46300 & is.na(price_adj), price_adj := twinfalls_2015]

# Remove Poughkeepsie-Newburgh-Middletown, NY (Metropolitan Statistical Area) *
# Whose counties are part of New York-Newark-Jersey City, NY-NJ-PA (Metropolitan Statistical Area)
# and has missing values for most years anyways
rpp_msa <- rpp_msa[GeoFIPS != 39100]

# Fix Prescott AZ's cbsa number
rpp_msa[GeoFIPS == 39150, GeoFIPS := 39140]
rpp_msa <- merge(rpp_msa,
                 msa_mapping,
                 by.x = 'GeoFIPS',
                 by.y = 'cbsa',
                 all.x = T,
                 allow.cartesian = T)
stopifnot(nrow(rpp_msa[is.na(mcnty)]) == 0)

# Fix Denver Colorado which is more granular than our mcnty
# use weighted mean with cbsa pops
rpp_msa[,weight := 1]

# 14500 and 24540 pop found from cbsa_pop$CBSA.2012.pop
# 19740 here sum(as.numeric(cbsa_pop[CBSA.title %like% 'Denver' & FIPS.code %in% locations[mcnty == 292]$cnty]$County.2012.pop))
rpp_msa[GeoFIPS %in% c('14500','19740','24540') & mcnty == 292,
        weight := fcase(GeoFIPS == '14500', 305318,
                        GeoFIPS == '19740', 2293065,
                        GeoFIPS == '24540', 263691)]

rpp_msa <- rpp_msa[,.(price_adj = weighted.mean(price_adj, weight)),
                   .(value, state, mcnty)]

stopifnot(nrow(rpp_msa) != length(unique(rpp_msa$value) * length(0:3109)))

setnames(rpp_msa, c('year','state_abbr','mcnty','value'))

# check to make sure we no longer have the issue where some counties in states are rural but we have no rural RPP information
stopifnot(length(setdiff(unique(locations[state %in% states_no_rural, mcnty]), unique(rpp_msa$mcnty))))
rpp_final <- rbind(rpp_msa[year <= 2021, c('mcnty','year','value')],
                   rpp[urban_rural_code == 'rural' & !mcnty %in% unique(rpp_msa$mcnty),
                       c('mcnty','year','value')])
stopifnot(length(setdiff(locations[current == 1, mcnty], rpp_final$mcnty)) == 0)
rpp_final[,version := ifelse(year < 2012, 2000, ifelse(year >= 2012 & year <= 2021, 2012, 2020))] 

## save for plotting later
rpp_mcnty_2015 <- copy(rpp_final[year == 2015])

## merge on county-PUMA crosswalk and calculate PUMA-level values
rpp_final <- merge(rpp_final, county_puma_xw[version %in% unique(rpp_final$version)],
                   by=c("mcnty","version"),
                   all=T,
                   allow.cartesian=T)
stopifnot(nrow(rpp_final[is.na(prop_by_puma)]) == 0)

# crosswalk the values
rpp_final <- rpp_final[,list(price_adj = weighted.mean(value, prop_by_puma)), by='year,puma']

## save for plotting later
rpp_puma_2015 <- copy(rpp_final[year == 2015])

# divide by 100, since this is the value we will actually use in adjusting income
rpp_final[,price_adj := price_adj/100]
saveRDS(rpp_final, file.path(proj_dir,"income_adjustments.rds"))

############# Vetting
# Time trends by PUMA
pdf(file.path(plot_dir,"rpp_PUMA_time_trend.pdf"), height=6, width=12)

puma_to_state <- unique(merge(county_puma_xw,
                              unique(locations[,c('mcnty','state_name')]),
                              by = 'mcnty')[,c('puma','state_name')])
rpp_plotting <- merge(rpp_final,
                      puma_to_state,
                      by = 'puma')

gg <- ggplot(rpp_final, aes(x = year, y = price_adj, group = puma)) +
  geom_line(linewidth = .5, alpha = 0.01) +
  ggtitle('All PUMAs')

print(gg)

for(s in sort(unique(rpp_plotting$state_name))){
  gg <- ggplot(rpp_plotting[state_name == s], aes(x = year, y = price_adj, group = puma)) +
    geom_line(linewidth = .5) +
    ggtitle(paste0('PUMAs in ', s))
  print(gg)
}

dev.off()

# Time trends by MCNTY
pdf(file.path(plot_dir,"rpp_MCNTY_time_trend.pdf"), height=6, width=12)

rpp_plotting <- merge(rpp_mcnty,
                      unique(locations[,c('mcnty','state_name')]),
                      by = 'mcnty')
setnames(rpp_plotting, 'value', 'price_adj')
rpp_plotting[,price_adj := price_adj / 100]

gg <- ggplot(rpp_plotting, aes(x = year, y = price_adj, group = mcnty)) +
  geom_line(linewidth = .5, alpha = 0.01) +
  ggtitle('All MCNTYs')

print(gg)

for(s in sort(unique(rpp_plotting$state_name))){
  gg <- ggplot(rpp_plotting[state_name == s], aes(x = year, y = price_adj, group = mcnty)) +
    geom_line(linewidth = .5) +
    ggtitle(paste0('MCNTYs in ', s))
  print(gg)
}

dev.off()

# state level comparison
puma_to_state <- unique(merge(county_puma_xw,
                              unique(locations[,c('mcnty','state_name')]),
                              by = 'mcnty')[,c('puma','state_name')])
rpp_plotting <- merge(rpp_final,
                      puma_to_state,
                      by = 'puma')
rpp_plotting <- rbind(merge(rpp_plotting[year %in% c(2008,2011)],
                            puma_pop[year == 2000, -'year'],
                            by = c('puma'),
                            all = T),
                      merge(rpp_plotting[year >= 2012],
                            puma_pop[year == 2012, -'year'],
                            by = c('puma'),
                            all = T))
rpp_plotting <- rpp_plotting[,.(price_adj = weighted.mean(price_adj, pop)),
                             .(state_name,year)]

rpp_state <- fread("[STATE_RPP_FILE]")
rpp_state <- process_rpp(rpp_state[GeoFIPS != 0])
setnames(rpp_state, c('GeoName','value','price_adj'), c('state_name','year','price_adj_state'))
rpp_state[,price_adj_state := price_adj_state / 100]

rpp_plotting <- merge(rpp_plotting,
                      rpp_state[year %in% unique(rpp_plotting$year),-'GeoFIPS'],
                      by = c('state_name','year'),
                      all = T)

pdf(file.path(plot_dir,"rpp_puma_vs_state_scatter.pdf"), height=6, width=12)
gg <- ggplot(rpp_plotting, aes(x = price_adj, y = price_adj_state)) +
  geom_point() +
  geom_abline(color = 'red') +
  ggtitle('Aggregated PUMAs VS State')

print(gg)

gg <- ggplot(rpp_plotting, aes(x = price_adj, y = price_adj_state, color = state_name)) +
  geom_point() +
  geom_abline(color = 'red') +
  ggtitle('Aggregated PUMAs VS State by State')

print(gg)

gg <- ggplot(rpp_plotting, aes(x = price_adj, y = price_adj_state, color = as.factor(year))) +
  geom_point() +
  geom_abline(color = 'red') +
  ggtitle('Aggregated PUMAs VS State by Year')

print(gg)
dev.off()

# same but by mcnty instead
ushd_pop <- "[DB_CALL_TO_GET_USHD_POPULATION]"
rpp_plotting <- merge(rpp_mcnty,
                      ushd_pop[,.(pop = sum(pop)),.(year,mcnty)],
                      by = c('year','mcnty'),
                      all.x = T)
rpp_plotting <- merge(rpp_plotting,
                      unique(locations[,c('mcnty','state_name')]),
                      by = 'mcnty',
                      all.x = T)
rpp_plotting <- rpp_plotting[,.(price_adj = weighted.mean(value, pop)),.(year,state_name)]
rpp_plotting[,price_adj := price_adj / 100]
rpp_plotting <- merge(rpp_plotting,
                      rpp_state[year %in% unique(rpp_plotting$year),-'GeoFIPS'],
                      by = c('state_name','year'),
                      all = T)


pdf(file.path(plot_dir,"rpp_mcnty_vs_state_scatter.pdf"), height=6, width=12)
gg <- ggplot(rpp_plotting, aes(x = price_adj, y = price_adj_state)) +
  geom_point() +
  geom_abline(color = 'red') +
  ggtitle('Aggregated MCNTYs VS State')

print(gg)

gg <- ggplot(rpp_plotting, aes(x = price_adj, y = price_adj_state, color = state_name)) +
  geom_point() +
  geom_abline(color = 'red') +
  ggtitle('Aggregated MCNTYs VS State by State')

print(gg)

gg <- ggplot(rpp_plotting, aes(x = price_adj, y = price_adj_state, color = as.factor(year))) +
  geom_point() +
  geom_abline(color = 'red') +
  ggtitle('Aggregated MCNTYs VS State by Year')

print(gg)
dev.off()

# Get range of RPP across geographies
setnames(rpp_puma_2015, "price_adj", "value")
rpp_state_2015 <- copy(rpp_state[year == 2015])
setnames(rpp_state_2015, "price_adj_state", "value")
rpp_state_2015[,value := value * 100]

limits <- c(min(rpp_state_2015$value, rpp_mcnty_2015$value, rpp_puma_2015$value),
            max(rpp_state_2015$value, rpp_mcnty_2015$value, rpp_puma_2015$value))

summary(limits)
breaks <- c(82, 88, 94, 100, 106, 112, 118)

# Merge on shape files and show each step of the process
rpp_state_2015 <- merge(rpp_state_2015,
                        unique(locations[,c('state_name','state')]),
                        by = 'state_name')
data_by_state <- merge(state, rpp_state_2015, by="state", all=T)
data_by_mcnty <- merge(mcnty, rpp_mcnty_2015, by="mcnty", all=T)
data_by_puma <- merge(puma_2012, rpp_puma_2015, by="puma", all=T)

map_it <- function(fdata, bystate, facet_urban, state_shp, geo, breaks) {
  limits <- c(min(breaks), max(breaks))

  gg <- ggplot() +
    geom_sf(data = fdata, aes(fill = value), color = "black", lwd = 0.0001) +
    scale_fill_gradientn(colors = brewer.pal(7, "PuOr"),
                         na.value = "gray97",
                         limits = limits,
                         breaks = breaks) +
    coord_sf(expand = F) +
    theme_bw(base_size = 10) +
    labs(title = paste0("RPP, 2015, ",geo)) +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          axis.title = element_blank(), panel.grid.major = element_line(colour = "transparent"),
          panel.border = element_blank(),  panel.spacing = unit(0.1, "lines"),
          legend.position = "right", legend.justification = "center", legend.box = "vertical",
          legend.margin = margin(0, 0, 0, 0, unit = "lines"),
          strip.background = element_blank(), strip.text = element_text(size = 10)) +
    guides(fill = guide_colorbar(barwidth = 0.5, barheight = 7, nbin = 100, direction = "vertical",
                                 title = paste("RPP"), title.position = "top"))

  if(!bystate) {
    gg <- gg +
      geom_sf(data = state_shp, fill = NA, color = "gray80", lwd = 0.01)
  }

  if(facet_urban) {
    gg <- gg +
      facet_wrap(~ urban_rural_code, ncol = 1)
  }

  return(gg)
}

pdf(file.path(plot_dir,"rpp_maps.pdf"), height=6, width=12)

gg_state <- map_it(fdata = data_by_state,
                   bystate = T,
                   facet_urban = F,
                   state_shp = state,
                   geo = "state",
                   breaks = breaks)

gg_mcnty <- map_it(fdata = data_by_mcnty,
                   bystate = F,
                   facet_urban = F,
                   state_shp = state,
                   geo = "mcnty",
                   breaks = breaks)

gg_puma <- map_it(fdata = data_by_puma,
                  bystate = F,
                  facet_urban = F,
                  state_shp = state,
                  geo = "puma",
                  breaks = breaks)

print(ggarrange(plotlist = list(gg_state, gg_mcnty, gg_puma), nrow=1, common.legend = TRUE, legend = "right"))
print(gg_state)
print(gg_mcnty)
print(gg_puma)

dev.off()
