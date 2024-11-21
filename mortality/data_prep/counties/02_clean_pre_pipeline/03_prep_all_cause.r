################################################################################
## Description: Prep an all-cause deaths file by race/ethnicity for
##              preliminary modeling of all-cause mortality and life expectancy
##              by race/ethnicity.
################################################################################

library(data.table)
library(ggplot2)

rm(list = ls())

# Load sae.shared functions
library(lbd.loader, lib.loc = sprintf("FILEPATH/lbd.loader-%s.%s",
                                      R.version$major,
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
R.utils::sourceDirectory('functions', modifiedOnly = FALSE)

## load location information
load("FILEPATH")
ps <- fread("FILEPATH")
ps <- unique(ps[, list(state = fips, state_alpha = alpha)])
loc <- unique(loc[,.(merged_location_id, mcnty, state)])


### prepped data:
data <- fread("FILEPATH")
nrow(data[race_label_1977 %in% c("None","NH Other Race") & year_id >= 2000])
# drop rows where the races could not be mapped:
data <- data[!(race_label_1977 %in% c("None","NH Other Race"))]

data_og <- copy(data)
# dictionary: 1 = NH White, 2 = NH Black, 3 = NH Other, 4 = Hispanic, 9 = Unknown

data <- data[year_id %in% c(2000:2022)]
data <- merge(data,loc,by.x="location_id", by.y="merged_location_id",all.x=T)
# check the merge
data[is.na(state)]
unique(data$state)

# check the counties to make sure the naming is correct
setdiff(loc$mcnty, data$mcnty)
setdiff(data$mcnty, loc$mcnty)

# keep track of total deaths
total <- data[, sum(deaths), keyby = 'mcnty,year_id,sex_id']

# re-map some age groups - need to confirm that this is correct
data[age_group_id == "1-5mo", age_group_id := 0]
data[age_group_id == "6-11mo", age_group_id := 0]
data[age_group_id == "0.01", age_group_id := 0]
data[age_group_id == "12-23mo", age_group_id := 1]
data[age_group_id == "2-4", age_group_id := 1]
data[age_group_id == "-9", age_group_id := NA]
data[,age_group_id := as.numeric(age_group_id)]
data[age_group_id > 85, age_group_id := 85]


#### some nice formatting for the rest of the analysis
setnames(data, c("age_group_id", "year_id", "sex_id", "race_1977"), c("age", "year", "sex", "race"))

## split unknown ages
## (proportional to deaths with known age in the same state, sex, race/ethnicity, year)
temp1 <- data[!is.na(age), list(state_deaths = sum(deaths)), by = 'state,year,sex,age,race']
temp1[, state_death_prop := state_deaths / sum(state_deaths), by = 'state,year,sex,race']
temp2 <- data[is.na(age), list(state, mcnty, year, sex, race, deaths)]
temp2 <- merge(temp2, temp1, by = c("state", "year", "sex", "race"),
               all.x = T, allow.cartesian = T)
temp2[, deaths := deaths * state_death_prop]
temp2 <- temp2[, list(state, year, mcnty, sex, age, race, deaths)]

data <- data[!is.na(age), ]
data <- data[,(names(temp2)), with=F]
data <- rbind(data, temp2)

data <- data[, list(deaths = sum(deaths)), keyby = 'mcnty,year,sex,age,race']

setnames(total, c("year_id", "sex_id"), c("year", "sex"))
stopifnot(all.equal(total, data[, sum(deaths), keyby = 'mcnty,year,sex']))
rm(temp1, temp2)

data[,cause_id := 294]


## save data
# deaths
setcolorder(data, c("mcnty", "year", "cause_id", "sex", "age", "race", "deaths"))
setkeyv(data, c("mcnty", "year", "cause_id", "sex", "age", "race", "deaths"))

saveRDS(data, file = "FILEPATH")
