# Prep segragation index data used in America 8's definition

# setup
library(data.table)
library(mgsub)
library(xlsx)
library(openxlsx)
rm(list = ls())

indir <- "FILEPATH"
outdir <- "FILEPATH"

locs <- fread("FILEPATH")
pop_raw <- data.table(readRDS('"FILEPATH"'))
pop_raw <- pop_raw[race == 'Black' & year == 2000,.(pop = sum(pop)),.(fips)]

# load segragtion index data
di <- data.table(openxlsx::read.xlsx("FILEPATH", 
                                     sheet = 4, startRow = 2, cols = c(1:3, 200)))
di <- di[!is.na(County)]
di$FIPS <- as.numeric(di$FIPS)

di <- merge(di, 
            unique(locs[,c('cnty','mcnty')]),
            by.x = 'FIPS', by.y = 'cnty', all.x = T)
di <- merge(di,
            pop_raw,
            by.x = 'FIPS', by.y = 'fips',
            all.x = T)

setnames(di, c('cnty','state_name','cnty_name','segragetion_index','mcnty','pop'))

# take a population weighted mean of the cnty data to collapse to mcntys
di <- di[,.(segragetion_index = weighted.mean(x = segragetion_index, w = pop, na.rm = T)),.(mcnty)]
di[is.na(segragetion_index), segragetion_index := 0]

saveRDS(di, file = paste0(outdir, "segragetion_index.rds"))
