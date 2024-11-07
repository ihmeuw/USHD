####################################################################################################
## Description: Make adjustments to deaths assigned to election districts in Alaska and New York in 
##              1980-81. Also make adjustments to deaths in Poquoson City, VA that were assigned to 
##              York County, VA in 1980-81. This is done by:
##                1. Calculating the average proportion of deaths in each county with missing 1980-81
##                   deaths out of the total number of deaths in its reassignment group from 1982-87.
##                2. Using these calculated proportions to distribute the deaths in each reassignment
##                   group that were initially assigned to an election district or different county.
##                3. Saving a cleaned version of this adjusted deaths dataset.
####################################################################################################

library(data.table)

rm(list=ls())

data_dir <- "FILEPATH"
main_dir <- "FILEPATH"

# need to make sure we keep the cause columns as a character variable
cause_columns <- rep("character", 21)
names(cause_columns) <- c("cause", paste0("multiple_cause_", 1:20))

# Read in cleaned and adjusted deaths data --------------------------------

# subset just counties that we have reassigned in Alaska and New York
reassign <- fread("FILEPATH")
reassign_fips_to <- unique(reassign$fips_to)
reassign_fips_from <- unique(reassign$fips_from)

# read in cleaned deaths data we will use to distribute 1980-1981 deaths
deaths_82_87 <- lapply(1982:1987, function(year) {
  print(year)
  deaths <- fread("FILEPATH", colClasses = cause_columns)
  setnames(deaths, "full_fips_res_numeric", "fips_to")
  deaths <- deaths[fips_to %in% reassign_fips_to]
  deaths <- deaths[, list(deaths=sum(deaths)), by=c("year", "fips_to")]
})
deaths_82_87 <- rbindlist(deaths_82_87)

# calculate proportion of deaths in each reassignment group by year
deaths_82_87_proportions <- deaths_82_87[, list(deaths=sum(deaths)), by=c("fips_to")]
deaths_82_87_proportions <- merge(deaths_82_87_proportions, unique(reassign[, list(fips_to, group)]), all.x=T, by="fips_to")
deaths_82_87_proportions[, proportion := deaths / sum(deaths), by=c("group")]
deaths_82_87_proportions[, deaths := NULL]


write.csv(deaths_82_87_proportions, "FILEPATH",
          row.names = F)