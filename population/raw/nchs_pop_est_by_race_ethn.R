################################################################################
## Description: Format NCHS bridged race intercensal and post-censal population
##              estimates.
##
## Note: It is not usually possible to compare population estimates across
##       vintages due to frequent methodology changes by NCHS in estimating
##       population. These changes are outlined in detail in the documentation
##       for each vintage; check there first if you see anything concerning.
##       Link: https://www.cdc.gov/nchs/nvss/bridged_race.htm#data
################################################################################

# Load settings file and directories -------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("population$", getwd())) setwd("FILEPATH")
library(readr)

in_dir  <- "FILEPATH"
out_dir <- "FILEPATH"
out_dir2 <- "FILEPATH"
temp_dir <- "FILEPATH"
nids <- data.table(year = 1990:2020, nid = c(rep(108082, 10), rep(108100, 10), rep(505499, 11)))

# Load and prep 1990-1999 intercensal population estimates ---------------------
pop <- NULL
file_paths_90s <- c()  # create vector to keep track of 1990-99 file paths
for (states in c("AL_TO_IN", "IA_TO_MO", "MT_TO_SC", "SD_TO_WY")) {
  cat(paste("1990-1999 intercensal estimates, ", states, "-", Sys.time(), "\n")); flush.console()
  f <- paste0("FILEPATH")
  file_paths_90s <- c(file_paths_90s, f)  # add file path to vector
  data <- read_fwf(file = f, col_types = "iiiiiiiiiiiiiii",  # read_fwf takes one character shortcut per column (see ?read_fwf for details)
                   col_positions = fwf_widths(widths = c(2, 3, 2, 1, 1, rep(8,10)),
                                              col_names = c("state", "cnty", "age", "racesex", "hisp", paste0("pop", 1990:1999))))
  data <- setDT(data)
  
  # get county fips code
  data[, fips := 1000*state + cnty]
  
  # get sex variable
  data[, sex := ifelse(racesex %in% c(1, 3, 5, 7), 1, 2)]
  
  # categorize race
  data[racesex %in% 1:2, race := "White"]
  data[racesex %in% 3:4, race := "Black"]
  data[racesex %in% 5:6, race := "AIAN"]
  data[racesex %in% 7:8, race := "API"]
  data[, hisp := hisp - 1]
  
  # reshape and format
  data <- data[, c("fips", "sex", "race", "hisp", "age", paste0("pop", 1990:1999)), with = F]
  data <- melt(data, id.vars = c("fips", "sex", "race", "hisp", "age"), variable.name = "year", value.name = "pop")
  data[, year := as.integer(gsub("pop", "", as.character(year)))]
  pop[[length(pop) + 1]] <- data
}
rm(data); gc()

# Load and prep 2000-2009 intercensal population estimates ---------------------
file_paths_00s <- c()  # create vector to keep track of 2000-09 file paths
for (yy in 2000:2009) {
  cat(paste("2000-2009 intercensal estimates, year", yy, "-", Sys.time(), "\n")); flush.console()
  f <- "FILEPATH"
  file_paths_00s <- c(file_paths_00s, f)  # add file path to vector
  data <- read_fwf(file = f, col_types = "ciiiiiii",  # read_fwf takes one character shortcut per column (see ?read_fwf for details)
                   col_positions = fwf_widths(widths = c(8, 4, 2, 3, 2, 1, 1, 8),
                                              col_names = c("vintage", "year", "state", "cnty", "age", "racesex", "hisp", "pop")))
  data <- setDT(data)
  
  # get county fips code
  data[, fips := 1000*state + cnty]
  
  # get sex variable
  data[, sex := ifelse(racesex %in% c(1, 3, 5, 7), 1, 2)]
  
  # categorize race
  data[racesex %in% 1:2, race := "White"]
  data[racesex %in% 3:4, race := "Black"]
  data[racesex %in% 5:6, race := "AIAN"]
  data[racesex %in% 7:8, race := "API"]
  data[, hisp := hisp - 1]
  
  # format
  data <- data[, list(fips, year, sex, age, race, hisp, pop)]
  pop[[length(pop) + 1]] <- data
}
rm(data); gc()

# Load and prep 2010-2019 post-censal population estimates ---------------------
# (currently vintage 2019, will need to update each year)
cat(paste("2010-2020 postcensal estimates, -", Sys.time(), "\n")); flush.console()
f <- paste0(in_dir, "USA_VINTAGE_BRIDGED_RACE_POP_2010_2020_Y2022M10D06.TXT")
data <- read_fwf(file = f, col_types = "iiiiiiiiiiiiiiiii",  # read_fwf takes one character shortcut per column (see ?read_fwf for details)
                 col_positions = fwf_widths(widths = c(4, 2, 3, 2, 1, 1, rep(8, 12)),
                                            col_names = c("vintage", "state", "cnty", "age", "racesex", "hisp", "pop2010_april", paste0("pop", 2010:2020))))
data <- setDT(data)

# get county fips code
data[, fips := 1000*state + cnty]

# get sex variable
data[, sex := ifelse(racesex %in% c(1, 3, 5, 7), 1, 2)]

# categorize race
data[racesex %in% 1:2, race := "White"]
data[racesex %in% 3:4, race := "Black"]
data[racesex %in% 5:6, race := "AIAN"]
data[racesex %in% 7:8, race := "API"]
data[, hisp := hisp - 1]

# collapse and reshape
data <- data[, c("fips", "sex", "age", "race", "hisp", paste0("pop", 2010:2020)), with = F]
data <- melt(data, id.vars = c("fips", "sex", "race", "hisp", "age"), variable.name = "year", value.name = "pop")
data[, year := as.integer(gsub("pop", "", as.character(year)))]
pop[[length(pop) + 1]] <- data
rm(data); gc()

# Format and save --------------------------------------------------------------
pop <- rbindlist(pop, use.names = T)  # combine all years
pop[, race := factor(race, levels = c("White", "Black", "AIAN", "API"))]  # turn race to factor
pop <- pop[, lapply(.SD, function(x) if (is.factor(x)) x else as.integer(x))]  # turn non-factors to integers
pop <- merge(pop, nids, by = "year")  # add nids
setkeyv(pop, c("fips", "year", "sex", "age", "race", "hisp"))
setcolorder(pop, c("fips", "year", "nid", "sex", "age", "race", "hisp", "pop"))
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))  # name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- list("108082" = lapply(1:length(file_paths_90s), function(i) {file_paths_90s[i]}),
                      "108100" = lapply(1:length(file_paths_00s), function(i) {file_paths_00s[i]}),
                      "505499" = list(f))

# save output
saveRDS(pop, file = paste0(out_dir, date_time_stamp, ".rds"))

# create an all-races version, save & upload metadata to DB
pop <- pop[, list(pop = sum(pop)), by = 'fips,year,nid,sex,age']
setkeyv(pop, c("fips", "year", "sex", "age"))
saveRDS(pop, file = paste0(out_dir2, date_time_stamp, ".rds"))