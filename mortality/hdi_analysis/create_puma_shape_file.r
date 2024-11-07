## Prep PUMA shape file

library(data.table)
library(maptools)
library(rgeos)
library(rgdal)
library(rmapshaper, lib.loc = "[LIBRARY_LOCATION]")
library(sf)

rm(list = ls())

indir <- paste0("[INPUT_DIRECTORY]")
proj_dir <- "[PROJECT_DIRECTORY]"

# use crosswalk file to get the names of the PUMAs
county_puma_xw <- readRDS(file.path(proj_dir,"count_to_puma_xw.rds"))
county_puma_xw[,pop := NULL] # just keep the proportions
county_puma_xw <- county_puma_xw[version == 2012]

# 2010 US PUMAs TigerLines file
puma_tl <- readOGR(paste0(indir, "cartographic_pumas_2019/"), layer = "cb_2019_us_puma10_500k")

# state to puma map
loc_map <- data.table(state = as.integer(as.character(puma_tl@data$STATEFP10)),
                      puma = as.integer(as.character(puma_tl@data$GEOID)))

puma_tl@data <- data.table(state = as.integer(as.character(puma_tl@data$STATEFP10)),
                           puma = as.integer(as.character(puma_tl@data$GEOID)))
puma_tl <- puma_tl[puma_tl@data$state < 60,] # drop PR

setdiff(puma_tl@data$puma, county_puma_xw$puma)

puma_tl <- puma_tl[order(puma_tl@data$puma), ]
puma_tl@data <- puma_tl@data[order(puma_tl@data$puma), ]

# rename ID slots
for (ii in 1:length(puma_tl)) puma_tl@polygons[[ii]]@ID <- as.character(puma_tl@data$puma[ii])

## Move AK and HI in the cnty shape file for making compact maps -----------------------------------
# pull out and relocate AK and HI
puma_tl <- spTransform(puma_tl, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
alaska <- puma_tl[puma_tl$state == 2,]
alaska@data <- data.frame(alaska@data)
rownames(alaska@data) <- as.character(alaska@data$puma)
alaska <- elide(alaska, rotate = -50)
alaska <- elide(alaska, scale = max(apply(bbox(alaska), 1, diff)) / 2)
alaska <- elide(alaska, shift = c(-2500000, -2500000))
proj4string(alaska) <- proj4string(puma_tl)

hawaii <- puma_tl[puma_tl$state == 15,]
hawaii@data <- data.frame(hawaii@data)
rownames(hawaii@data) <- as.character(hawaii@data$puma)
hawaii <- elide(hawaii, rotate = -35)
hawaii <- elide(hawaii, shift = c(5400000, -1600000))
proj4string(hawaii) <- proj4string(puma_tl)

# recombine
puma_tl <- puma_tl[!puma_tl$state %in% c(2, 15),]
puma_tl@data <- data.frame(puma_tl@data)
rownames(puma_tl@data) <- as.character(puma_tl@data$puma)
puma_tl <- rbind(puma_tl, alaska, hawaii)
puma_tl <- puma_tl[order(puma_tl$puma),]
puma_tl@data <- data.table(puma_tl@data)

# make a list of the PUMAs in order to confirm that ms_simplify does not remove any of them
pumas_pre_simplify <- unique(puma_tl@data$puma)

# simplify
puma_tl <- st_as_sf(puma_tl) # converting to SF since otherwise ms_simplify crashes R (https://github.com/ateucher/rmapshaper/issues/83)
puma_tl <- ms_simplify(puma_tl, keep = 0.1, keep_shapes = T)
puma_tl <- as(puma_tl, "Spatial")
for (ii in 1:length(puma_tl)) puma_tl@polygons[[ii]]@ID <- as.character(puma_tl@data$puma[ii])
puma_tl@data <- data.table(puma_tl@data)

# make sure that we didn't drop any pumas
pumas_post_simplify <- unique(puma_tl@data$puma)

stopifnot(length(setdiff(pumas_pre_simplify, pumas_post_simplify)) == 0)
stopifnot(length(setdiff(pumas_post_simplify, pumas_pre_simplify)) == 0)

# save
saveRDS(puma_tl, file = paste0(indir, "/prepped_puma_shape_files/puma_2010_shape_file.rds"))
writeOGR(puma_tl, paste0(indir, "/prepped_puma_shape_files/"), layer = "puma_2010_shape_file",
         driver = "ESRI Shapefile", overwrite_layer = T)
