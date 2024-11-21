# Prep urban rural definitions used in several Americas definitions

# setup
library(data.table)
library(mgsub)
library(xlsx)
rm(list = ls())

indir <- "FILEPATH"
outdir <- "FILEPATH"

locs <- fread("FILEPATH")

# load rural urban continuum data
ur <- data.table(read.xlsx(paste0(indir, "FILEPATH"), 
                           sheetIndex = 1))

# Use 2013 for CT since they use new planning regions
ur_2013 <- data.table(read.xlsx(paste0(indir, "FILEPATH"),
                                sheetIndex = 1))
setnames(ur_2013, 'RUCC_2013', 'RUCC_2023')

ur <- rbind(ur[State != 'CT',-'Population_2020'],
            ur_2013[State == 'CT',-'Population_2010'])

# set to just metro/ nonmetro
ur[,urban := ifelse(!RUCC_2023 %in% 1:3, 0, RUCC_2023)]
ur$FIPS <- as.numeric(ur$FIPS)

# Remove Territories
ur <- ur[FIPS < 57000]

# Merge on mcntys
ur <- merge(ur, 
            locs[,c('mcnty','cnty')],
            by.x = 'FIPS',
            by.y = 'cnty',
            all.x = TRUE)
stopifnot(nrow(ur[is.na(mcnty)]) == 0)

# If part of a merged county is urban mark the entire merged county as urban
ur <- ur[,.(urban = max(urban)),.(mcnty)]

saveRDS(ur, file = paste0(outdir, "urban_rural.rds"))
