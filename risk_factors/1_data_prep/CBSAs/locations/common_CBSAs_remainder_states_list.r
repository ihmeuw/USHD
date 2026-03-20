####################################################################################################
## Description: Create a list of common CBSAs (BRFSS SMART years 2005-2017) and remainder states
####################################################################################################

library(data.table)

rm(list = ls())
root <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
main_dir <- paste0(root, "FILEPATH")
parent_dir <- paste0(root, "FILEPATH")

#load list of all CBSA and remaining counties 
load(paste0(parent_dir, "CBSAs/prepped_shp/cbsa_cnty_data.rdata"))#cbsa.cnty.data

# import list of common CBSAs 
load(paste0(main_dir,"survey_data/common_cbsa_smart2005_2017.rdata"))#common_cbsa

cbsa.cnty.data$cbsa <- as.numeric(cbsa.cnty.data$cbsa)
  
# flag common CBSAs in CBSA/remaining counties file 
cbsa.cnty.data$common <- as.numeric(cbsa.cnty.data$cbsa %in% common_cbsa$cbsa)
cbsa.cnty.data[common==1,]
uniqueN(cbsa.cnty.data[common==1,]$cbsa)

# keep state column, CBSAs and common column
cbsa.cnty.data<-cbsa.cnty.data[, c('cbsa','state','state_name','common')]

# delete CBSAs other than SMARt common (common==0)
cbsa.cnty.data <- merge(cbsa.cnty.data, common_cbsa, by='cbsa', all.x=T)
cbsa.cnty.data$area.code <- ifelse(cbsa.cnty.data$common==1, 
                                   paste0("1.",cbsa.cnty.data$cbsa),
                                   paste0("0.",cbsa.cnty.data$state))
cbsa.cnty.data$cbsa <- NULL

# delete duplicate rows (keep one row per state remainder)
cbsa.remainder <- cbsa.cnty.data[!duplicated(cbsa.cnty.data)]

table(cbsa.remainder[area.code<1,]$state_name)
uniqueN(cbsa.remainder[area.code>1,]$area.code)
sum(cbsa.remainder$common)

cbsa.remainder$area.code <- as.numeric(cbsa.remainder$area.code)

# save list of common CBSAs and state remainders
save(cbsa.remainder, file=(paste0(main_dir, 'locations/common_cbsas_remainder_states.rdata')))

