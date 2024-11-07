### Calculate county to PUMA crosswalk
# Data downloaded from: https://mcdc.missouri.edu/applications/geocorr2022.html

library(data.table)

# location map
locations <- fread("[MCNTY_LOCATION_INFO]")
outdir <- "[PROJECT_DIRECTORY]"

process_puma_data <- function(tmp, yr) {
  tmp <- tmp[2:nrow(tmp)] # get rid of first row because these are just labels explaining each of the columns
  tmp[,county := as.numeric(county)]
  tmp[,afact := as.numeric(as.character(afact))]
  if(yr == "5") {
    tmp[,pop2k := as.numeric(pop2k)]
  } else {
    tmp[,pop20 := as.numeric(pop20)]
  }

  tmp <- tmp[!(stab %in% "PR")]
  tmp[,state := as.numeric(state)]

  tmp[,afact := NULL]

  setnames(tmp, paste0("puma",yr), "puma")

  return(tmp)
}

### 2020 counties to 2020 PUMAs
county_to_puma_xw_2020 <- fread("[PATH_TO_GEOCORR_DOWNLOAD]")
county_to_puma_xw_2020 <- process_puma_data(county_to_puma_xw_2020, yr = "22")
county_to_puma_xw_2020[,year := 2020]

## 2020 counties to 2012 PUMAs
county_to_puma_xw_2012 <- fread("[PATH_TO_GEOCORR_DOWNLOAD]")
county_to_puma_xw_2012 <- process_puma_data(county_to_puma_xw_2012, yr="12")
county_to_puma_xw_2012[,year := 2012]

## 2000 counties to 2000 PUMAs (relevant PUMAs pre-2012)
county_to_puma_xw_2000 <- fread("[PATH_TO_GEOCORR_DOWNLOAD]")
county_to_puma_xw_2000 <- process_puma_data(county_to_puma_xw_2000, yr = "5")
county_to_puma_xw_2000[,year := 2000]
setnames(county_to_puma_xw_2000, "pop2k", "pop20")
county_to_puma_xw_2000[puma %in% c("01801","01802","01905") & state==22, puma := "77777"] # these are combined in the ACS data

sum(county_to_puma_xw_2012[puma == " ", pop20]) # there are a few places with no PUMA name and extremely small populations. 
county_to_puma_xw_2012 <- county_to_puma_xw_2012[puma != " "]

# combine the versions
county_to_puma_xw <- rbind(county_to_puma_xw_2020,
                           county_to_puma_xw_2012,
                           county_to_puma_xw_2000, fill=T)

setdiff(county_to_puma_xw$county, locations$cnty) 

county_to_puma_xw <- merge(county_to_puma_xw, locations[,.(mcnty, county = cnty)], by="county",all=T)
county_to_puma_xw <- county_to_puma_xw[!is.na(puma)] # get rid of rows where the PUMAs are not in the dataset. 
stopifnot(nrow(county_to_puma_xw[is.na(mcnty)]) == 0)
stopifnot(nrow(county_to_puma_xw[is.na(pop20)]) == 0)

# now make sure all of the state are the same as in locations
stopifnot(length(setdiff(county_to_puma_xw$state, locations$state)) == 0)
stopifnot(length(setdiff(locations$state, county_to_puma_xw$state)) == 0)

county_to_puma_xw[, puma := 100000*as.integer(as.character(state)) + as.integer(as.character(puma))]

# make sure there are no replicates
puma_states <- unique(county_to_puma_xw[,.(puma, state)])
puma_states[,count := .N, by=c("puma")]
stopifnot(nrow(puma_states[count != 1]) == 0)
rm(puma_states)

## save a data object with the proportion of each county made up of each PUMA so that we can crosswalk population
puma_to_county_by_pop <- copy(county_to_puma_xw)

###### Get weights from crosswalking county to PUMA
# now calculate population weights
county_to_puma_xw[,prop_by_puma := pop20/sum(pop20), by='puma,year']

# make sure they sum to 1
county_to_puma_xw[,sum_wgt_check := sum(prop_by_puma), by="puma,year"]
stopifnot(nrow(county_to_puma_xw[abs(1 - sum_wgt_check) > 0.00001]) == 0)

county_to_puma_xw <- county_to_puma_xw[,.(mcnty, puma, prop_by_puma, version = year, pop = pop20)]
# because we had to combine PUMAs to create 2277777 for the 2000 version, we need to sum once more to combine these
# and because some counties are nested within mcnties
county_to_puma_xw <- county_to_puma_xw[,list(prop_by_puma = sum(prop_by_puma),
                                             pop = sum(pop)), by='mcnty,puma,version']

county_to_puma_xw[,sum_wgt_check := sum(prop_by_puma), by="puma,version"]
stopifnot(nrow(county_to_puma_xw[abs(1 - sum_wgt_check) > 0.00001]) == 0)
county_to_puma_xw[,sum_wgt_check := NULL]

saveRDS(county_to_puma_xw, file.path(outdir,"count_to_puma_xw.rds"))

###### Get weights for calculating the proportion of each county that each PUMA is composed of
puma_to_county_by_pop[,prop_within_county := pop20/sum(pop20), by='mcnty,year'] # so, within each county, we know the proportion of each PUMA
puma_to_county_by_pop[,sum_wgt_check := sum(prop_within_county), by="mcnty,year"]
stopifnot(nrow(puma_to_county_by_pop[abs(1 - sum_wgt_check) > 0.00001]) == 0)

puma_to_county_by_pop <- puma_to_county_by_pop[,.(mcnty, puma, prop_within_county, version = year, pop = pop20)]

# because we had to combine PUMAs to create 2277777 for the 2000 version, we need to sum once more to combine these
# and because some counties are nested within counties
puma_to_county_by_pop <- puma_to_county_by_pop[,list(prop_within_county = sum(prop_within_county),
                                             pop = sum(pop)), by='mcnty,puma,version']

puma_to_county_by_pop[,sum_wgt_check := sum(prop_within_county), by="mcnty,version"]
stopifnot(nrow(puma_to_county_by_pop[abs(1 - sum_wgt_check) > 0.00001]) == 0)
puma_to_county_by_pop[,sum_wgt_check := NULL]

saveRDS(puma_to_county_by_pop, file.path(outdir,"puma_pop_within_county.rds"))
