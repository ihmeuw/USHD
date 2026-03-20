####################################################################################################
## Description: Prep shapefile of CBSAs and remaining (non-CBSA) merged counties.
##
## Output:      "FILEPATH" -- a data.table of
##                CBSAs and remaining counties with area.code identifier cbsa_xxx cnty_xxx.
##              "FILEPATH" -- corresponding 
##               shapefile.
####################################################################################################

library(data.table)
library(maptools) #collapse shapefile

rm(list = ls())

root <- ifelse(Sys.info()[1] == "Windows", "FILEPATH", "FILEPATH")
loc_dir <- paste0(root, "FILEPATH")
main_dir <- paste0(root, "FILEPATH")

## Load merged counties shapefile
cnty_map<-readRDS(paste0(loc_dir, 'FILEPATH'))

## Move AK and HI in the cnty shape file for making compact maps -----------------------------------
# pull out and relocate AK and HI
cnty_map <- spTransform(cnty_map, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
alaska <- cnty_map[cnty_map$state == 02,]
alaska@data <- data.frame(alaska@data)
rownames(alaska@data) <- as.character(alaska@data$cnty)
alaska <- elide(alaska, rotate = -50)
alaska <- elide(alaska, scale = max(apply(bbox(alaska), 1, diff)) / 2)
alaska <- elide(alaska, shift = c(-2500000, -2500000))
proj4string(alaska) <- proj4string(cnty_map)

hawaii <- cnty_map[cnty_map$state == 15,]
hawaii@data <- data.frame(hawaii@data)
rownames(hawaii@data) <- as.character(hawaii@data$cnty)
hawaii <- elide(hawaii, rotate = -35)
hawaii <- elide(hawaii, shift = c(5400000, -1400000))
proj4string(hawaii) <- proj4string(cnty_map)

# recombine
cnty_map <- cnty_map[!cnty_map$state %in% c(2, 15),]
cnty_map@data <- data.frame(cnty_map@data)
rownames(cnty_map@data) <- as.character(cnty_map@data$cnty)
cnty_map <- rbind(cnty_map, alaska, hawaii)
cnty_map <- cnty_map[order(cnty_map$cnty),]
cnty_map@data <- data.table(cnty_map@data)

rm(hawaii, alaska)
#plot(cnty_map)

#add mcnty 
loc <- fread(paste0(loc_dir, "/counties/merged_counties.csv"))
loc<-loc[,c('mcnty','cnty')]

cnty_map@data<-merge(cnty_map@data, loc, by='cnty')#all matched

# load CBSA-county delineation file
delineation<-data.table(readxl::read_xls('FILEPATH',
                                       .name_repair = "universal"))

delineation$cnty <-  as.integer(paste(sprintf("%02s",delineation$FIPS.State.Code),
                                    sprintf("%03s",delineation$FIPS.County.Code),sep=""))

#merge counties shapefile and CBSA delineation file
setnames(delineation, 'CBSA.Code', 'cbsa')
cnty_map@data<-merge(cnty_map@data, delineation[State.Name != 'Puerto Rico',], by="cnty", all.x=T)#3142 obs of 17 variables - keep non-CBSA counties

cbsa.cnty.shp<-cnty_map
rm(cnty_map, loc)

#collapse data by mcnty-----------------------------------------------------------------------------------------------------
cbsa.cnty.data<-cbsa.cnty.shp@data[,c('cnty','mcnty','cbsa','CBSA.Title','state','state_name')]

cbsa.cnty.data[duplicated(mcnty), c('state', 'mcnty')]
cbsa.cnty.data[mcnty %in% c(87,101,292,534,1809),]
cbsa.cnty.data <- cbsa.cnty.data[!((mcnty==87 & is.na(cbsa)) | (mcnty==101 & is.na(cbsa)) | (mcnty==292 & cbsa==19740) | (mcnty==292 & cbsa==14500) | 
                                     (mcnty==534 & is.na(cbsa)) | (mcnty==1809 & cbsa==10740)) ,]

cbsa.cnty.data<-unique(cbsa.cnty.data[,-'cnty'])
cbsa.cnty.data$cbsa<-as.numeric(cbsa.cnty.data$cbsa)
save(cbsa.cnty.data, file=paste0(loc_dir, "/CBSAs/prepped_shp/cbsa_cnty_data1.rdata"))

#run area.code_integer_reference file then run code below------------------------------------------------------------------------------------------

load(paste0(loc_dir, "/CBSAs/prepped_shp/cbsa_cnty_data1.rdata"))

#add area code
load(paste0(main_dir,"/locations/area.code_reference_file.rdata"))

cbsa.cnty.data<-merge(cbsa.cnty.data, area[,c('mcnty','area.codeint')], by='mcnty', all.x=T)
uniqueN(cbsa.cnty.data$area.codeint)
save(cbsa.cnty.data, file=paste0(loc_dir, "/CBSAs/prepped_shp/cbsa_cnty_data.rdata"))

#create shapefile
cbsa.cnty.shp@data<-merge(cbsa.cnty.shp@data, area[,c('mcnty','area.codeint')], by='mcnty', all.x=T)
# collapse counties in CBSAs by CBSA
cbsa.rem.cnty.shp <- unionSpatialPolygons(cbsa.cnty.shp, cbsa.cnty.shp@data$area.codeint)#2219 elements

plot(cbsa.rem.cnty.shp)

# save collapsed (common CBSAs/state remainders) shapefile
saveRDS(cbsa.rem.cnty.shp, file=paste0(loc_dir, "/CBSAs/prepped_shp/cbsa_cnty_shp.rds"))








     