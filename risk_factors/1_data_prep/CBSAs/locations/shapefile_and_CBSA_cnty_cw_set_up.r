####################################################################################################
##
## Description: Prep shapefile of CBSAs and remaining (non-CBSA) merged counties 
##                for 2013-2019
##              Produce crosswalk for CBSAs/Metropolitan Divisions w/ cnty and mcnty
##
## Output:      cbsa_mcnty_cnty_crosswalk.rds -- Crosswalk (mapping) file that 
##                links CBSAs/Metro Divisions to counties and merged counties.
##              cbsa_shp_<<year_range>>.rds"
##                Shapefile of CBSAs and Metro Divisions for the years in the range.
##
####################################################################################################

library(data.table)
library(maptools)
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)

root <- "FILEPATH"
loc_dir <- "FILEPATH"
main_dir <- "FILEPATH"

# Set up counties/mcnty shapefile -----------------------------------------------

## Load [not-merged] counties shapefile
cnty_map<-readRDS(paste0(FILEPATH, '/cnty_mapping_shape_file.rds'))

# Turn into SF
cnty_map <- as(cnty_map, "sf")

# Merge counties to CBSAs -------------------------------------------------

# Load CBSA-county delineation file for all SMART years:
f <- list.files(path = "FILEPATH", pattern = "USA*", full.names = T)
cbsa_to_fips <- suppressMessages(rbindlist(lapply(f, function(f) as.data.table(readxl::read_xls(f, .name_repair = "universal", skip = 2))[, year := as.integer(stringr::str_sub(f, 71, 74))][, file := f]))) # read in and format crosswalks
# remove rows with missing FIPS.County.Code (a result of file footers read in)
cbsa_to_fips <- cbsa_to_fips[!is.na(FIPS.County.Code)]
cbsa_to_fips[, c("CBSA.Code", "Metro.Division.Code", "CSA.Code", "FIPS.State.Code", "FIPS.County.Code", "year") :=
               lapply(.SD, as.numeric), .SDcols = c("CBSA.Code", "Metro.Division.Code", "CSA.Code", "FIPS.State.Code", "FIPS.County.Code", "year")]
# Code the start/end year and month to the delineation file
file_dates <- as.data.table(list(file = f))
file_dates[, start_year := as.integer(stringr::str_sub(f, 71, 74))]
file_dates[, start_month := stringr::str_extract(stringr::str_sub(f, 76, 81), "[^_]+")] # Get character name or abbrev of month from title
file_dates[, start_month := grep(str_sub(start_month, 1, 3), month.abb, value = F, ignore.case = T), by = 1:nrow(file_dates) ] # Convert month name to numeric using built in month abbreviations `month.abb`
setkeyv(file_dates, c("start_year", "start_month")) # Order chronologically by file date
file_dates[, `:=`(end_year = shift(start_year, n = 1, type = "lead"),
                  end_month = (as.numeric(shift(start_month, n = 1, type = "lead"))-1))][end_month == 12, end_year := end_year - 1]

cbsa_to_fips <- cbsa_to_fips[file_dates, on = "file"][, file := NULL]
# Flag rows that are containing metropolitan divisions rather than CBSAs.
# Metropolitan divisions are subsections of CBSAs sometimes reported in SMART BRFSS.
# This indicator helps distinguish between regular CBSAs and those with divisions.
cbsa_to_fips[, contains_metro_div := ifelse(is.na(Metro.Division.Code), 0, 1)]
metro_divisions <- cbsa_to_fips[!is.na(Metro.Division.Code), 
                                .(Metro.Division.Code, Metropolitan.Division.Title, County.County.Equivalent, State.Name, FIPS.State.Code, FIPS.County.Code, year, start_year, start_month, end_year, end_month, CBSA.Code)][, is_metropolitan_division := 1]
setnames(metro_divisions, c("Metro.Division.Code", "Metropolitan.Division.Title", "CBSA.Code"), c("CBSA.Code", "CBSA.Title", "Parent.CBSA.Code"))
# Create a long-format version of the delimitation files that separates metropolitan divisions
# from their parent CBSAs.
cbsa_to_fips <- cbsa_to_fips[, .(CBSA.Code, CBSA.Title, contains_metro_div, County.County.Equivalent, State.Name, FIPS.State.Code, FIPS.County.Code, year, start_year, start_month, end_year, end_month)][, is_metropolitan_division := 0]
cbsa_to_fips <- rbind(cbsa_to_fips, metro_divisions, use.names = T, fill = T)
cbsa_to_fips[, fips := FIPS.State.Code*1000 + FIPS.County.Code]
cbsa_to_fips <- unique(cbsa_to_fips[FIPS.State.Code < 57])

# Merge CBSAs to cnty/mcnty map
cnty_map <- full_join(cnty_map, cbsa_to_fips, by = c("cnty"= "fips"))
cnty_map <- rename(cnty_map, cbsa = CBSA.Code) %>% arrange(year, cnty)

# Save
saveRDS(cnty_map %>% 
          as.data.table %>% 
          dplyr::select(cnty, cnty_name, is_metropolitan_division, Parent.CBSA.Code, state, state_name, mcnty, cbsa, CBSA.Title, year, start_year, start_month, end_year, end_month) %>% 
          unique(), 
        file=paste0(loc_dir, "/FILEPATH/cbsa_mcnty_cnty_crosswalk.rds"))

# Collapse mcnty file for CBSAs in each year where delineations change -----------
years <- unique(cbsa_to_fips$year)
for(yr in years){
  print(yr)
  # Subset to counties/CBSA in a given year
  cnty_shape_yr <- cnty_map %>% filter(year == yr)
  # Collapse the polygons by the identifier of CBSAs/state-remainder; make different
  # polygons for metropolitan divisions and the CBSAs that contain them
  cbsa_shape <- cnty_shape_yr %>%
    filter(is_metropolitan_division ==0) %>%
    group_by(cbsa) %>%
    summarise(geometry = sf::st_union(geometry)) %>%
    ungroup() %>% mutate(type = "CBSA")
  metropolitan_division_shape <- cnty_shape_yr %>%
    filter(is_metropolitan_division ==1) %>%
    group_by(cbsa) %>%
    summarise(geometry = sf::st_union(geometry)) %>%
    ungroup() %>% mutate(type = "metro_division")
  
  cbsa_shape <- rbind(cbsa_shape, metropolitan_division_shape)
  print(ggplot(cbsa_shape) + geom_sf())
  # Get year range the CBSAs correspond to (i.e., the year listed in loc, and the years until the next file)
  if(yr != 2020){
    yr_range <- paste0(seq(yr, years[which(years == yr) + 1] - 1, 1), collapse = "_")
  } else{
    yr_range <- 2020
  }
  # Save collapsed (common CBSAs/state remainders) shapefile
   saveRDS(cbsa_shape, file=paste0(FILEPATH, "/cbsa_shp_", yr_range,".rds"))
}