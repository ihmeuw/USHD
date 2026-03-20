####################################################################################################
## Description: Extract ACS 5-year average income per capita estimates by county and race/ethnicity.
##              The race/ethnicity groups include white alone, non-Hispanic white, black alone, AIAN
##              alone, Asian alone, NHOPI alone, some other race alone, two or more races, and
##              Hispanic.
##              
##              From the Census Reporter topic description for table B19301:
##              "Table B19301, 'Per Capita income', is simply the value for B19313 'Aggregate Income'
##              divided by the total population estimate for the summary geography. This statistic
##              is more or less the 'average' income. Note the potential for misunderstanding: A)
##              the aggregate income is divided among all people, not only those who actually had
##              income, and B) as with any average, outliers (very big earners) can have a
##              disproportionate effect on resulting figure."
##              
##              See https://censusreporter.org/topics/income/ for more details.
##
## Get your Census API key here: https://api.census.gov/data/key_signup.html
## Add key to .Renviron: census_api_key(key, overwrite = TRUE, install = TRUE)
## Reload .Renviron: readRenviron("~/.Renviron")
##
## Use load_variables(year, dataset = "acs5", cache = TRUE) to find variable ids
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")
library(tidycensus)

raw_file_dir <- "FILEPATH"
out_dir <- "FILEPATH"
years <- 2009:2022
nids <- data.table(year = years-2, nid = c(236693:236698, 280080, 399019, 399014, 448152, 476546,
                                           503429, 520867, 548126))

convert_to_variance <- function(x) {return((x/1.645)^2)}
convert_to_moe <- function(x) {return(1.645*sqrt(x))}

# Extract using Census API -------------------------------------------------------------------------

# In census data for 2022+, CT has new planning regions instead of counties. However, sub-county
# geography did not change, so we can map census tracts to their old county FIPS and aggregate
# 2022+ estimates to those old FIPS, at least in the short term.
agg_ct_tracts <- function(yr, include_moe = F) {
  message("Aggregating CT census tracts in ", yr)
  
  # income per capita estimates by race
  data_covar <- get_acs(geography = "tract",
                        state = "CT",
                        variables = c(white = 'B19301A_001', black = 'B19301B_001', aian = 'B19301C_001',
                                      asian = 'B19301D_001', nhopi = 'B19301E_001', other = 'B19301F_001',
                                      multi = 'B19301G_001', white_nh = 'B19301H_001', hisp = 'B19301I_001'),
                        year = yr,
                        survey = "acs5",
                        key = Sys.getenv('CENSUS_API_KEY'))
  setDT(data_covar)
  setnames(data_covar, c("variable", "estimate"), c("race_group", "income_pc"))
  data_covar[, NAME := NULL]
  
  # population estimates by race (for weighting during aggregation)
  data_pop <- get_acs(geography = "tract",
                      state = "CT",
                      variables = c(white = 'B01001A_001', black = 'B01001B_001', aian = 'B01001C_001',
                                    asian = 'B01001D_001', nhopi = 'B01001E_001', other = 'B01001F_001',
                                    multi = 'B01001G_001', white_nh = 'B01001H_001', hisp = 'B01001I_001'),
                      year = yr,
                      survey = "acs5",
                      key = Sys.getenv('CENSUS_API_KEY'))
  setDT(data_pop)
  setnames(data_pop, c("variable", "estimate"), c("race_group", "pop"))
  data_pop[, c("moe", "NAME") := NULL]
  
  # combine income pc and pop estimates
  data <- merge(data_covar, data_pop, by = c("GEOID", "race_group"), all = T); rm(data_covar, data_pop)
  data[, tract_fips := as.numeric(substr(GEOID, 5, 11))]  # isolate census tract FIPS
  data[, year := yr - 2]  # assign data year
  
  # map CT census tracts to old county FIPS by loading a CT tract-level dataset and isolating tract
  # and county fips from GEOID
  ct_2020_tract <- as.data.table(get_acs(geography = "tract", variables = c("B19301_001"),
                                         year = 2020, state = "CT", survey = "acs5"))
  ct_2020_tract[, c("tract_fips", "fips") := list(as.numeric(substr(GEOID, 6, 11)), as.numeric(substr(GEOID, 1, 5)))]
  ct_2020_tract[tract_fips == 990000, fips := 9001]  # manually assign FIPS to single county for water tracts (estimates are 0)
  ct_2020_tract[tract_fips == 990100, fips := 9007]
  tract_fips_map <- unique(ct_2020_tract[, list(tract_fips, fips)]); rm(ct_2020_tract)
  data[tract_fips_map, on = "tract_fips", fips := i.fips]  # add county fips to data
  
  # aggregate estimates to county fips using pop as weights for weighted means
  if (include_moe) {  # if we want to keep margins of error
    data[, var := convert_to_variance(moe), by = .(year, fips, tract_fips, race_group)]  # convert MOEs to variances for combining
    data <- data[, list(income_pc = weighted.mean(x = income_pc, w = pop, na.rm = T),  # agg to FIPS
                        var = sum(var * (pop / sum(pop))^2, na.rm = T), pop = sum(pop)), by = .(year, fips, race_group)]
    data[, c("moe", "var") := list(convert_to_moe(var), NULL)]  # convert back to MOE
    data[is.nan(income_pc), income_pc := NA]  # assign NA to NaNs that occur when data are completely missing across all tracts in a stratum
    data[is.nan(moe), moe := NA]
  } else {  # if we don't keep margins of error
    data <- data[, list(income_pc = weighted.mean(x = income_pc, w = pop, na.rm = T)), by = .(year, fips, race_group)]
  }
  data[, pop := NULL]  # pop no longer needed after weighted aggregation complete
  
  # get file path
  file_path <- list.files("FILEPATH",
                          pattern = "FILEPATH", full.names = T)

  data[, infl_year := yr]  # assign inflation year
  
  return(list("ct_data" = data, "ct_file_path" = file_path))
}

# US county-level estimates
extract_data <- function(yr) {
  data <- get_acs(geography = "county",
                  variables = c(white = 'B19301A_001', black = 'B19301B_001', aian = 'B19301C_001',
                                asian = 'B19301D_001', nhopi = 'B19301E_001', other = 'B19301F_001',
                                multi = 'B19301G_001', white_nh = 'B19301H_001', hisp = 'B19301I_001'),
                  year = yr,
                  survey = "acs5",
                  key = Sys.getenv('CENSUS_API_KEY'))
  setDT(data)
  setnames(data, c("variable", "estimate"), c("race_group", "income_pc"))
  data[, year := yr - 2]  # assign data year
  data[, infl_year := yr]  # assign inflation year
  data[, c("fips", "GEOID", "NAME") := list(as.numeric(GEOID), NULL, NULL)]
  
  # get file path
  if (yr >= 2019) {
    file_path <- list.files("FILEPATH",
                            pattern = "FILEPATH", full.names = T)
    if (yr >= 2022) {
      file_path <- file_path[!grepl("FILEPATH", file_path)]
    }
  } else {
    file_path <- list.files("FILEPATH",
                            pattern = "FILEPATH", full.names = T)
  }
  
  return(list("data" = data, "file_path" = file_path))
}
extractions <- lapply(years, extract_data)
data <- rbindlist(lapply(1:length(extractions), function(i) {extractions[[i]]$data}))  # rbind all data objects from extractions
ct_extractions <- lapply(years[years >= 2022], agg_ct_tracts, include_moe = T)
ct_data <- rbindlist(lapply(1:length(ct_extractions), function(i) {ct_extractions[[i]]$ct_data}))

# Combine and format data -------------------------------------------------------------------------
# replace CT planning region data with county data
nrow_start <- nrow(data)
data <- data[!(year >= 2020 & fips %in% 9000:9999)]  # drop planning region data
data <- rbindlist(list(data, ct_data), use.names = T, fill = T)
stopifnot(nrow_start - nrow(data) == 9*(max(data$year)-2019))  # difference in total rows should be total race groups (9) * each data year after 2019 (9 planning regions -> 8 counties)

# format and sort columns
data <- data[fips < 60000]  # filter out territories
data <- merge(data, nids, by = "year")  # add nids
setcolorder(data, c("year", "fips", "race_group", "income_pc", "moe", "infl_year"))

# Save output --------------------------------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- lapply(1:length(extractions), function(i) {as.list(extractions[[i]]$file_path)})  # get list of lists of file paths (1 list per data year)
ct_nid_path_list <- lapply(1:length(ct_extractions), function(i) {as.list(ct_extractions[[i]]$ct_file_path)})
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths
names(ct_nid_path_list) = nids[year >= 2020, nid]
nid_path_list <- list(nid_path_list, ct_nid_path_list)
keys <- unique(unlist(lapply(nid_path_list, names)))  # make 1 entry per NID (a simple list concatenation will create a separate entry for CT file paths)
nid_path_list <- setNames(do.call(mapply, c(FUN=c, lapply(nid_path_list, `[`, keys))), keys)

# save data
saveRDS(data, paste0(out_dir, date_time_stamp, ".rds"))