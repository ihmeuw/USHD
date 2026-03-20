#Creating area.code decimal to numeric reference file
#file contains cbsa, state, area.code (decimal and integer) and indicator variable for common SMART CBSA
#create ID by: 
#CBSA by state (same for mcnties within same CBSA AND state) -- for use to create population weights upon which to weigh SMART BRFSS participants that fall in cross-state CBSA
#by CBSA by state only for CBSAs common across all SMART years (2005-2017) all other CBSAs will have same ID as non-CBSA state remainders -- for use to collapse covars/shapefile etc. and in modeling

library(data.table)

rm(list=ls())

root <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
main_dir <- paste0(root, "FILEPATH")
loc_dir <- paste0(root, "FILEPATH")
pop_dir <- paste0(root, "FILEPATH")

load(paste0(loc_dir, "/CBSAs/prepped_shp/cbsa_cnty_data1.rdata"))

cbsa.cnty.data1 <- cbsa.cnty.data

#find common CBSAs across SMART years
load(paste0(root, 'FILEPATH'))

common_cbsa<-Reduce(intersect, list(appendmsa4[year==2005,cbsa],appendmsa4[year==2006,cbsa],appendmsa4[year==2007,cbsa],appendmsa4[year==2008,cbsa],
                                    appendmsa4[year==2009,cbsa],appendmsa4[year==2010,cbsa],appendmsa4[year==2011,cbsa],appendmsa4[year==2012,cbsa],
                                    appendmsa4[year==2013,cbsa],appendmsa4[year==2014,cbsa],appendmsa4[year==2015,cbsa],appendmsa4[year==2016,cbsa],
                                    appendmsa4[year==2017,cbsa]))

common_cbsa<-data.frame(common_cbsa)
setnames(common_cbsa, 'common_cbsa', 'cbsa')
common_cbsa$common <- 1
common_cbsa <- common_cbsa[!is.na(common_cbsa$cbsa),]
sum(common_cbsa$common)

options(max.print = '10000')

uniqueN(cbsa.cnty.data[cbsa.cnty.data$cbsa %in% common_cbsa$cbsa, ]$cbsa)#only 68 cbsa common to all SMART years (2005-2017) amounting to 91 cbsa/state pairs

area <- merge(cbsa.cnty.data, common_cbsa, by='cbsa', all.x=T)
area[is.na(common),]$common <- 0
uniqueN(cbsa.cnty.data$state)#checked 50 states + DC

#create id that identifies separate parts of cbsas in states
#state.fips and cbsa seperated by . for cbsa areas and statefips for non-cbsa areas within each state
area$xstate.cbsa<-as.numeric(ifelse(!is.na(area$cbsa), paste(area$state, area$cbsa, sep="."), area$state))
#table(area$cbsa.state)

#create ID variable for CBAs and remainder states to merge by mcnty to all other data files 
#COMMON CBSAs IDs start with state FIPS, a "." then the 5 digits identifier and remainder start with 0. then the 2 digit state fips
area$area.code<-as.numeric(ifelse(area$common==1, paste(area$state, area$cbsa, sep="."), area$state))

area <- area[order(area.code),]
area[, area.codeint := as.numeric(factor(area.code, levels = unique(area.code))) - 1]
sum(area$common)#437 mcounties in common CBSAs

area$area.name <- ifelse(area$common==1, paste0('CBSA_', area$CBSA.Title," in ",area$state_name), paste0(area$state_name,'_remainder'))

save(area, file=paste0(main_dir,"locations/area.code_reference_file.rdata"))

uniqueN(area$area.codeint)#140
uniqueN(area[common==1,c('xstate.cbsa')])#68 common CBSAs creating 90 CBSA/state pairs
uniqueN(area$area.name)
table(area$area.name)
