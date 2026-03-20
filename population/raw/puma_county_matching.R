###################################################################################################
## Description: Match merged county-PUMA and create unique families of merged counties-PUMAs
## 
##              (1) Create a list of each merged county, and all the PUMAs included in a given county
##              (2) Loop through the list of counties and look for overlapping PUMAs
##              (3) Counties with overlapping PUMAs are grouped in the same family
##              (4) Create a data.frame that contains the PUMA id, the county id, and the family id
##              (5) Do this for the different crosswalk data that we have (2000-2009, 2010-2011,
##                  2012-2013, 2014-2015, 2016-2018, 2019-2021, 2022)
##
## Output: A table named 'matchDT' containing: 'year', 'family', 'mcnty', 'puma','w'
###################################################################################################

# Setup -------------------------------------------------------------------------------------------
rm(list = ls())

library(data.table)
library(readstata13)
library(haven)
library(dplyr)

indir <- "FILEPATH"
outdir <- "FILEPATH"

merged_counties <- fread("FILEPATH")

cwlk <- setDT(readRDS(file = "FILEPATH"))
cwlk[, afact := as.numeric(levels(afact))[afact]]  # convert afact from factor to numeric
cwlk[year == "2016:2017", year := "2016:2018"]  # PUMA-county mapping did not change from 2017 to 2018; add 2018 to this group
cw19_21 <- fread("FILEPATH")[-1]  # load mapping for 2019-2021
cw19_21 <- cw19_21[, list(year = "2019:2021", puma = as.numeric(paste0(as.numeric(state), puma12)),
                          county = as.numeric(county), afact = as.numeric(afact))]
cwlk22 <- fread("FILEPATH")[-1]  # load mapping for 2022
cwlk22 <- cwlk22[, list(year = "2022", puma = as.numeric(paste0(as.numeric(state), puma22)),
                        county = as.numeric(county), afact = as.numeric(afact))]
cwlk <- rbindlist(list(cwlk, cw19_21, cwlk22), use.names = T); rm(cw19_21, cwlk22)  # bind all years together
cwlk <- cwlk[county < 60000]  # drop territories

# We use merged counties rather than counties to ensure continuity of the geographic units over time
cwlk <- cwlk[afact != 0]  # drop cases where PUMAs have no population overlap with counties
cwlk <- merge.data.table(cwlk, merged_counties[, list(county = cnty, mcnty)], by = "county", all.x = T)
cwlk_mcnty <- cwlk[, list(w = sum(afact)), by = 'year,puma,mcnty']

# Create "families" for each year pool ------------------------------------------------------------
matchDT <- lapply(sort(unique(cwlk_mcnty$year)), function(year_pool) {
  message("matching in ", year_pool)
  temp <- cwlk_mcnty[year == year_pool]
  listPuma <- unique(temp$puma)
  listmcnty <- unique(temp$mcnty)
  PumaInmcnty <- list()
  
  for(i in 1:length(listmcnty)){
    tmp <- subset(temp, mcnty == listmcnty[i])
    PumaInmcnty[[i]] <- unique(tmp$puma)
  }
  
  fam <- list()
  remaining <- 1:length(listmcnty)
  j <- 1
  while(length(remaining) > 0) {
    f <- remaining[1]
    i <- 1
    while(i <= length(f)) {
      for (k in 1:length(remaining)) {
        if(length(intersect(PumaInmcnty[[f[i]]], PumaInmcnty[[remaining[k]]])) > 0){
          f <- unique(c(f, remaining[k]))
        }
      }
      i <- i+1
    }
    fam[[j]] <- f
    j <- j + 1
    remaining <- remaining[!(remaining %in% f)]
  }
  
  match <- NULL
  for (i in 1:length(fam)) {
    for (k in 1:length(fam[[i]])) {
      match <- rbind(match, c(i, listmcnty[fam[[i]][k]]))
    }
  }
  
  match <- as.data.table(match)
  setnames(match, c("family", "mcnty"))
  match <- merge.data.table(match, temp, by = "mcnty", all.x = T)
})

matchDT <- rbindlist(matchDT, use.names = T, fill = T)

# Save output -------------------------------------------------------------------------------------
saveRDS(matchDT, file = paste0(outdir, "matchPumamcnty_all_years.rds"))