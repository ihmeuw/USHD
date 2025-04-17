##################################################################################################################
## Description: Create a crosswalk files for mapping from merged county to PUMA-mcnty and merged county
##              to CBSA-mcnty. Because merged counties nest within PUMA-mcnties and CBSA-mcnties,
##              this crosswalk is trivial -- each merged county contributes the entirety of its
##              population to a single PUMA-mcnty or CBSA-mcnty (ie, the weights are all 1). However,
##              it varies by year for PUMA-mcnties and CBSA-mcnties unlike for state and national level aggregations.
##
## Inputs:      Mapping of merged counties to states ("merged_counties.csv")
##
## Outputs:     mcnty_to_puma_mcnty_crosswalk.rds - data.table mapping mcnty to puma-mcnty.
##              mcnty_to_cbsa_mcnty_crosswalk.rds - data.table mapping mcnty to cbsa-mcnty.
##################################################################################################################

library(data.table)
"%nin%" <- Negate("%in%")

## set outdir
outdir <- paste0("FILEPATH")

archive_date <- format(Sys.Date(), "%Y_%m_%d")
archive_outdir <- paste0(outdir, "archive/archived_", archive_date, "/")
dir.create(archive_outdir, showWarnings = F)

## load list of merged counties, puma-mcnties, and cbsas
puma_mcnty <- readRDS("FILEPATH")
cbsa_mcnty <- readRDS("FILEPATH")

##################################################################################################################
## mcnty-to-cbsa-mcnty mapping ###################################################################################
##################################################################################################################

wts <- unique(cbsa_mcnty[, list(year, mcnty, cbsa_mcnty_code, version, is_metropolitan_division)])
wts[, wt := 1]
# two versions available in these years
wts <- wts[!(year == 2017 & version %nin% 2017)]
wts <- wts[!(year == 2015 & version %nin% 2015)]
wts <- wts[is_metropolitan_division == 0]
wts[, c("is_metropolitan_division", "version") := NULL]
setnames(wts, "cbsa_mcnty_code", "cbsa_mcnty")

# check that each mcnty appears once for each year
years <- length(unique(wts$year))
check_n <- nrow(setDT(wts)[,if(.N == years) .SD,by=mcnty])
if (nrow(wts) != check_n) {
  stop("There is an incorrect number of rows -> either more than or less than one mcnty per year/cbsa-mcnty")
}

setkey(wts, mcnty)
saveRDS(wts, file = paste0(outdir, "mcnty_to_cbsa_mcnty_crosswalk.rds"))
saveRDS(wts, file = paste0(archive_outdir, "mcnty_to_cbsa_mcnty_crosswalk.rds"))
rm(wts)

##################################################################################################################
## mcnty-to-puma-mcnty mapping ###################################################################################
##################################################################################################################

wts <- unique(puma_mcnty[, list(year, mcnty, puma_mcnty)])
wts[, wt := 1]

# check that each mcnty appears once for each year
years <- length(unique(wts$year))
check_n <- nrow(setDT(wts)[,if(.N == years) .SD,by=mcnty])
if (nrow(wts) != check_n) {
  stop("There is an incorrect number of rows -> either more than or less than one mcnty per year/puma-mcnty")
}

setkey(wts, mcnty)
saveRDS(wts, file = paste0(outdir, "mcnty_to_puma_mcnty_crosswalk.rds"))
saveRDS(wts, file = paste0(archive_outdir, "mcnty_to_puma_mcnty_crosswalk.rds"))





