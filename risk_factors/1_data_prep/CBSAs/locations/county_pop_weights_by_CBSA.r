#Create population weights by county in CBSAs common to SMART years 2005-2017

library(tidycensus)
library(data.table)
library(readxl)
library(dplyr)

rm(list = ls())
root <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
main_dir <- paste0(root, "FILEPATH")
pop_dir <- paste0(root, "FILEPATH")

#import merged county population by sex and age
mcntypop<-readRDS(paste0(pop_dir, "counties/prepped/pop_by_age_sex.rds"))

#create population weight by CBSA/state pairs (<1 for CBSAs in >1 state)
mcntypop <- aggregate(mcntypop$pop, by=list(mcntypop$mcnty, mcntypop$state, mcntypop$year), FUN=sum)
setnames(mcntypop, c('Group.1', 'Group.2', 'Group.3', 'x'), c('mcnty', 'state', 'year', 'pop'))#124,400=3110 mcnties x 40 years (1980 through 2019)

#load mcnty/area.code relationship file
load(paste0(main_dir,"locations/area.code_reference_file.rdata"))

#will need area.code to merge on to BRFSS data (CBSA not available but we can create area.code from state FIPS)
#include id that identifies separate parts of cbsas in different states -> xstate.cbsa
mcntypop <- merge(mcntypop[mcntypop$year==2010,], area[,c('state','mcnty','cbsa','area.codeint','area.code','xstate.cbsa')], by=c('state','mcnty'))
#checked that we have unique 1043 cbsa/state pairs - matches shapefile (including 998 CBSAs and 45 state remainders)

#create population weights by merged county within cbsa
mcntypop$sumpop <- ifelse(!is.na(mcntypop$cbsa), ave(mcntypop$pop, mcntypop$cbsa, FUN=sum), NA)
mcntypop$popweight <- ifelse(!is.na(mcntypop$cbsa), mcntypop$pop/mcntypop$sumpop, NA)

cbsa.popweight <- aggregate(mcntypop$popweight, by=list(mcntypop$xstate.cbsa), FUN=sum)
setnames(cbsa.popweight, c('Group.1','x'), c('xstate.cbsa', 'popweight'))
uniqueN(mcntypop[!is.na(mcntypop$cbsa),]$area.codeint)
uniqueN(mcntypop$area.codeint)

cbsa.popweight <- merge(cbsa.popweight, unique(area[,c('cbsa','area.code', 'state', 'xstate.cbsa', 'area.codeint')]), by='xstate.cbsa', all.y=F)
uniqueN(cbsa.popweight$cbsa)
sum(cbsa.popweight$popweight, na.rm=T)

rm(mcntypop, common_cbsa, cbsa.cnty.data, area)
#save output as CBSA/county population weights
save(cbsa.popweight, file=paste0(main_dir,'populations/cbsa_pop_weight.rdata'))

detach("package:tidyverse", unload=TRUE)
detach("package:tidyr", unload=TRUE)
detach("package:dplyr", unload=TRUE)

