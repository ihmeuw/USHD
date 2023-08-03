################################################################################
## Description: Format NCHS bridged race intercensal and post-censal population
##              estimates.
################################################################################

library(LaF)
library(data.table)
library(mgsub)

rm(list = ls())
root <- ifelse(Sys.info()[1] == "Windows", "[FILEPATH]", "[FILEPATH]")

in_dir  <- paste0(root, "[FILEPATH]")
out_dir <- paste0(root, "[FILEPATH]")
out_dir2 <- paste0(root, "[FILEPATH]")
if (!dir.exists(out_dir)) dir.create(out_dir)  # create new folder for outputs if one doesn't exist already
if (!dir.exists(out_dir2)) dir.create(out_dir2)  # create new folder for outputs if one doesn't exist already
nids <- data.table(year = 1990:2019, nid = c(rep(108082, 10), rep(108100, 10), rep(452981, 10)))

## Load and prep 1990-1999 intercensal population estimates --------------------
pop <- NULL
for (states in c("AL_TO_IN", "IA_TO_MO", "MT_TO_SC", "SD_TO_WY")) {
  cat(paste("1990-1999 intercensal estimates, ", states, "-", Sys.time(), "\n")); flush.console()
  f <- paste0(in_dir, "[FILEPATH]", states, ".TXT")
  data <- laf_open_fwf(filename = f, column_types = rep("integer", 15), column_widths = c(2, 3, 2, 1, 1, rep(8,10)),
                       column_names = c("state", "cnty", "age", "racesex", "hisp", paste0("pop", 1990:1999)))
  data <- data.table(data[,])

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

## Load and prep 2000-2009 intercensal population estimates --------------------
for (yy in 2000:2009) {
  cat(paste("2000-2009 intercensal estimates, year", yy, "-", Sys.time(), "\n")); flush.console()
  f <- paste0(in_dir, "[FILEPATH]", yy, "[FILEPATH]")
  data <- laf_open_fwf(filename = f, column_types = c("character", rep("integer", 7)), column_widths = c(8, 4, 2, 3, 2, 1, 1, 8),
                       column_names = c("vintage", "year", "state", "cnty", "age", "racesex", "hisp", "pop"))
  data <- data.table(data[,])

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

## Load and prep 2010-2019 post-censal population estimates --------------------
## (currently vintage 2019, will need to update each year)
cat(paste("2010-2019 postcensal estimates, -", Sys.time(), "\n")); flush.console()
f <- paste0(in_dir, "[FILEPATH]")
data <- laf_open_fwf(filename = f, column_types = rep("integer", 17), column_widths = c(4, 2, 3, 2, 1, 1, rep(8, 11)),
                     column_names = c("vintage", "state", "cnty", "age", "racesex", "hisp", "pop2010_april", paste0("pop", 2010:2019)))
data <- data.table(data[,])

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
data <- data[, c("fips", "sex", "age", "race", "hisp", paste0("pop", 2010:2019)), with = F]
data <- melt(data, id.vars = c("fips", "sex", "race", "hisp", "age"), variable.name = "year", value.name = "pop")
data[, year := as.integer(gsub("pop", "", as.character(year)))]
pop[[length(pop) + 1]] <- data
rm(data); gc()

## Format and save -------------------------------------------------------------
# combine all years
pop <- rbindlist(pop, use.names = T)

# make race a factor
pop[, race := factor(race, levels = c("White", "Black", "AIAN", "API"))]

# format
pop <- pop[, lapply(.SD, function(x) if (is.factor(x)) x else as.integer(x))]
pop <- merge(pop, nids, by = "year")  # add nids
setkeyv(pop, c("fips", "year", "sex", "age", "race", "hisp"))
setcolorder(pop, c("fips", "year", "nid", "sex", "age", "race", "hisp", "pop"))

# save
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))  # name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
saveRDS(pop, file = paste0(out_dir, date_time_stamp, ".rds"))

# create an all-races version and save
pop <- pop[, list(pop = sum(pop)), by = 'fips,year,nid,sex,age']
setkeyv(pop, c("fips", "year", "sex", "age"))
saveRDS(pop, file = paste0(out_dir2, date_time_stamp, ".rds"))
