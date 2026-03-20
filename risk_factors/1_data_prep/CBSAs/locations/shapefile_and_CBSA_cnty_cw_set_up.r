####################################################################################################
## Description: Prep shapefile of CBSAs and remaining (non-CBSA) merged counties 
##                for 2013-2019
##              Produce crosswalk for CBSAs/Metropolitan Divisions w/ cnty and mcnty
##
## Output:      "FILEPATH" -- a data.table of
##                CBSAs and remaining counties with area.code identifier cbsa_xxx cnty_xxx.
##              "FILEPATH" -- corresponding 
##               shapefile.
##
####################################################################################################

stop("see note above about potential revision")

library(data.table)
library(maptools) #collapse shapefile
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)

rm(list = ls())

root <- ifelse(Sys.info()[1] == "Windows", "FILEPATH", "FILEPATH")
loc_dir <- paste0(root, "FILEPATH")
main_dir <- paste0(root, "FILEPATH")


# Set up counties/mcnty shapefile -----------------------------------------------

## Load [not-merged] counties shapefile
cnty_map<-readRDS(paste0(loc_dir, 'FILEPATH'))

# turn into SF
cnty_map <- as(cnty_map, "sf")

# Merge counties to CBSAs -------------------------------------------------

# load CBSA-county delineation file for all SMART years:
f <- list.files(path = "FILEPATH", pattern = "USA*", full.names = T)
cbsa_to_fips <- rbindlist(lapply(f, function(f) {
  dt <- as.data.table(readxl::read_xls(f, .name_repair = "universal", skip = 2))
  # Check if "Metro_Division_Code" exists and rename it
  if ("Metro.Division.Code" %in% names(dt)) {
    setnames(dt, old = "Metro.Division.Code", new = "Metropolitan.Division.Code")
  }
  # Your existing operations
  dt[, year := as.integer(stringr::str_sub(f, 71, 74))]
  dt[, file := f]
  return(dt)
})) #read in and format crosswalks

# remove rows with missing FIPS.County.Code (a result of file footers read in)
cbsa_to_fips <- cbsa_to_fips[!is.na(FIPS.County.Code)]
cbsa_to_fips[, c("CBSA.Code", "Metropolitan.Division.Code", "CSA.Code", "FIPS.State.Code", "FIPS.County.Code", "year") :=
               lapply(.SD, as.numeric), .SDcols = c("CBSA.Code", "Metropolitan.Division.Code", "CSA.Code", "FIPS.State.Code", "FIPS.County.Code", "year")]
# code the start/end year and month to the delineation file
file_dates <- as.data.table(list(file = f))
file_dates[, start_year := as.integer(stringr::str_sub(f, 71, 74))]
file_dates[, start_month := stringr::str_extract(stringr::str_sub(f, 76, 81), "[^_]+")] # get character name or abbrev of state from title
file_dates[, start_month := grep(str_sub(start_month, 1, 3), month.abb, value = F, ignore.case = T), by = 1:nrow(file_dates) ] # convert month name to numeric using built in month abbreviations `month.abb`
setkeyv(file_dates, c("start_year", "start_month")) # order chronologically by file date
file_dates[, `:=`(end_year = shift(start_year, n = 1, type = "lead"),
                  end_month = (as.numeric(shift(start_month, n = 1, type = "lead"))-1))][end_month == 12, end_year := end_year - 1]

cbsa_to_fips <- cbsa_to_fips[file_dates, on = "file"][, file := NULL]
# SMART reports responses for a metropolitan division when there are enough 
#   respondents in the areas. These locations have different codes than the CBSA
#   they are in (because they specify a subsection of the location). 
# When there is a metro division within a CBSA, add it to the end of the list of
#   CBSAs so that we can merge on it BUT make an indicator that it is a metro 
#   division so that it is possible to get a list of just the CBSAs if needed
cbsa_to_fips[, contains_metro_div := ifelse(is.na(Metropolitan.Division.Code), 0, 1)]
metro_divisions <- cbsa_to_fips[!is.na(Metropolitan.Division.Code), 
                                .(Metropolitan.Division.Code, Metropolitan.Division.Title, County.County.Equivalent, State.Name, FIPS.State.Code, FIPS.County.Code, year,start_year, start_month, end_year, end_month, CBSA.Code)][, is_metropolitan_division := 1]
setnames(metro_divisions, c("Metropolitan.Division.Code", "Metropolitan.Division.Title", "CBSA.Code"), c("CBSA.Code", "CBSA.Title", "Parent.CBSA.Code"))
# note that the line below does not create duplicates because metropolitan divisions & their parent
#   CBSAs are saved on the same line in the Census Delimitation files. The line
#   below grabs the CBSA codes corresponding to the parent CBSAS, while the line above
#   grabs the CBSA codes corresponding to the metropolitan divisions. By renaming
#   the MD columns, and then combining the DT again, we're effectively getting
#   a long-format version of the delimitation files, and separating the
#   columns corresponding to MDs and CBSAs.
cbsa_to_fips <- cbsa_to_fips[, .(CBSA.Code, CBSA.Title, contains_metro_div, County.County.Equivalent, State.Name, FIPS.State.Code, FIPS.County.Code, year,start_year, start_month, end_year, end_month)][, is_metropolitan_division := 0]
cbsa_to_fips <- rbind(cbsa_to_fips, metro_divisions, use.names = T, fill = T)
cbsa_to_fips[, fips := FIPS.State.Code*1000 + FIPS.County.Code]
cbsa_to_fips <- unique(cbsa_to_fips[FIPS.State.Code < 57])

# merge cbsas to cnty/mcnty map

cnty_map <- full_join(cnty_map, cbsa_to_fips, by = c("cnty"= "fips"))
cnty_map <- rename(cnty_map, cbsa = CBSA.Code) %>% arrange(year, cnty)

# save
saveRDS(cnty_map %>% 
          as.data.table %>% 
          dplyr::select(cnty, cnty_name, is_metropolitan_division, Parent.CBSA.Code, state, state_name, mcnty, cbsa, CBSA.Title, year, start_year, start_month, end_year, end_month) %>% 
          unique(), 
        file=paste0(loc_dir, "FILEPATH"))

# Collapse mcnty file for CBSAs in each year ------------------------------
years <- unique(cbsa_to_fips$year)
for(yr in years){ # CBSAs don't change every year, so only run for years where changes occur
  print(yr)
  # subset to counties/cbsa in a given year
  cnty_shape_yr <- cnty_map %>% filter(year == yr)
  
  # collapse the polygons by the identifier of CBSAs/state-remainder; make different
  #    polygons for metropolitan divisions and the CBSAs that contain them
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
  # get year range the CBSAs correspond to (i.e., the year listed in loc, and the years until the next file)
  if(yr != 2023){
    yr_range <- paste0(seq(yr, years[which(years == yr) + 1] - 1, 1), collapse = "_")
  } else{
    yr_range = 2023
  }
  # save collapsed (common CBSAs/state remainders) shapefile
   saveRDS(cbsa_shape, file=paste0(loc_dir, "/CBSAs/prepped_shp/cbsa_shp_", yr_range,".rds"))
}
