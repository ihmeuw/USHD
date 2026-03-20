####################################################################################################
## Description: Prep population files to use for aggregating from the county to the state and
##                national level.
##                (1) collapse all population files to the merged county level
##                (2) format data.tables and save
## Output:      A data.table named geopop with the following variables: 'mcnty', 'year', 'sex',
##                'age', 'pop', 'state', 'natl'.
####################################################################################################

library(data.table)

rm(list=ls())
root <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
pop_dir <- paste0(root, "FILEPATH")
loc_dir <- paste0(root, "")
out_dir <- paste0(root, "")
years <- c(1995, 2012)

## Load county-level populations
load(paste0(pop_dir, "nchs_pop.rdata"))
pop <- pop[between(year, years[1], years[2]) & age >= 18,]
setnames(pop, "fips", "cnty")

## Collapse to the merged county level and add a state variable
load(paste0(loc_dir, "merged_counties.rdata"))
loc <- loc[, list(cnty, mcnty, state)]

pop <- merge(pop, loc, by="cnty", all.x=T)
pop <- pop[, list(pop = sum(pop)), by='state,mcnty,year,sex,age']

## Format and save
pop <- pop[, list(mcnty, year, sex, age, pop, state, natl = 0)]
setkeyv(pop, c("mcnty", "year", "sex", "age"))
geopop <- pop
save(geopop, file=paste0(out_dir, "pop_for_geo_aggregation.rdata"))
