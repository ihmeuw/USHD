# Prep the percent of the API population that is Pacific Islander for each mcnty
# used in America 1's definition

# setup
library(data.table)
library(mgsub)
rm(list = ls())

indir <- "FILEPATH"
outdir <- "FILEPATH"

## Load and prep Decennial Census P2 data ------------------------------------------------------
# loop through years
year <- 2020
cat(paste0(year, "\n")); flush.console()

# load meta data and find variables of interest
meta <- fread("FILEPATH")
setnames(meta, c("var", "label"))
meta <- meta[grepl("Total", label),]
setnames(meta, c("variable", "race"))

# merge data with meta data
data <- fread("FILEPATH", header = TRUE)

if (names(which.max(table(sapply(data, class)))) == "character") data <- data[-1,] # remove variable labels
data <- melt(data, id.vars = c("GEO_ID", "NAME"))
setnames(data, "GEO_ID", "GEO.id2")

data <- merge(data, meta, all.y = T, by = "variable")
data <- as.data.table(data[, c('GEO.id2', 'value', 'race')])

# rename and format easy variables
setnames(data, "GEO.id2", "fips")
data[, fips := as.integer(gsub("0500000US", "", as.character(fips)))]
data[, value := as.numeric(as.character(value))]

# group and encode education status, collapse to less granular groups
data[, asian := as.integer(grepl("Asian", race))]
data[, nhopi := as.integer(grepl("Native Hawaiian", race))]
data[, multi := as.integer(!grepl("alone", race))]
data <- data[asian == 1 | nhopi == 1, ]

# drop territories
data <- data[fips < 60000,]
setnames(data, "value", "pop")
locs <- fread('FILEPATH')
data <- merge(data, 
              locs[,c('mcnty','cnty')],
              by.x = 'fips',
              by.y = 'cnty',
              all.x = TRUE)

data <- data[, list(asian_alone = sum(pop * (multi == 0 & asian == 1 & nhopi == 0)),
                    asian_in_comb_other = sum(pop * (multi == 1 & asian == 1 & nhopi == 0)),
                    nhopi_alone = sum(pop * (multi == 0 & asian == 0 & nhopi == 1)),
                    nhopi_in_comb_other = sum(pop * (multi == 1 & asian == 0 & nhopi == 1)),
                    both = sum(pop * (multi == 1 & asian == 1 & nhopi == 1)),
                    total = sum(pop)), mcnty]

data[, pct_PI := (nhopi_alone + nhopi_in_comb_other + both) / total]

data <- data[,c('mcnty','pct_PI')] 

saveRDS(data, file = paste0(outdir, "pct_PI", ".rds"))
