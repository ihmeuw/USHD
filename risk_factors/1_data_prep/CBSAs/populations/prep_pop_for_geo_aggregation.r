####################################################################################################
## Description: Prep population files to use for aggregating from the county to the state and
##                national level.
##                (1) collapse all population files to the merged county level
##                (2) collapse all population files to CBSA/remaing merged counties level
##                (3) format data.tables and save
## Output:      A data.table named geopop with the following variables: 'mcnty', 'year', 'sex',
##                'age', 'pop', 'state', 'natl'.
####################################################################################################

library(data.table)

rm(list=ls())
root <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
main_dir <- paste0(root, "FILEPATH")
pop_dir <- paste0(root, "FILEPATH")
loc_dir <- paste0(root, "FILEPATH")
years <- c(2002, 2017)

#load area code reference file
load(paste0(main_dir,"FILEPATH"))

#subset to variables needed to collapse
areacnty<-area[, c('mcnty','area.codeint')]

## Load county-level populations
pop <- readRDS(paste0(pop_dir, "nchs_pop_est.rds"))
pop <- pop[between(year, years[1], years[2]) & age >= 18,]
setnames(pop, "fips", "cnty")

## Collapse to the merged county level and add a state variable
load(paste0(loc_dir, "counties/merged_counties.rdata"))
loc <- loc[, list(cnty, mcnty, state)]

pop <- merge(pop, loc, by="cnty", all.x=T)
pop <- pop[, list(pop = sum(pop)), by='state,mcnty,year,sex,age']

## Collapase populations to the common CBSA/remainder states
pop<-merge(pop, areacnty, by='mcnty', all.y=F) 
pop <- pop[, list(pop = sum(pop)), by='area.codeint,year,sex,age,state']

## Format and save
pop <- pop[, list(area.codeint, year, sex, age, pop, state, natl = 0)]
setkeyv(pop, c("area.codeint", "year", "sex", "age","state"))
geopop <- pop

save(geopop, file=paste0(main_dir, "populations/pop_for_geo_aggregation.rdata"))

#------------------------------------------------------------------------------------------------------------------

#Create link file for area.codeint, natl and state levels
natl <- readRDS("FILEPATH") 
natl <- merge(natl, area[,c('mcnty','area.codeint')])
natl <- unique(natl[,c('natl','wt','area.codeint')])

state <- readRDS("FILEPATH")
state <- merge(state, area[,c('mcnty','area.codeint')])
state <- unique(state[,c('state','wt','area.codeint')])

saveRDS(natl, file=('FILEPATH'))
saveRDS(state, file=('FILEPATH'))

