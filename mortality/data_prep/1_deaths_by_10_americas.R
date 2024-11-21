#####################################################################
# Description: Appends deaths files for modeling by 10 Americas 
#              definitions
#####################################################################

rm(list = ls())

library(data.table)

# Load sae.shared functions
library(lbd.loader, lib.loc = sprintf("FILEPATH/lbd.loader-%s.%s",
                                      R.version$major,
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))

## load location information
load("FILEPATH")
ps <- fread("FILEPATH")
ps <- unique(ps[, list(state = fips, state_alpha = alpha)])
loc <- unique(loc[,.(merged_location_id, mcnty, state)])


### prepped data:
data <- fread("FILEPATH/deaths_by_cause_all_race_ethn.csv")
data <- data[year_id %in% c(2000:2021)]

if(current_mapping){
  # get rid of the rows where the races could not be mapped:
  data <- data[!(race_label_1977 %in% c("None","NH Other Race"))]
}else{
  # create original race mappings
  data[,race_1977 := fcase(race_orig %in% c(0, 78, 99), 999,
                           race_orig == 1, 5,
                           race_orig == 2, 4,
                           race_orig == 3, 6,
                           race_orig %in% c(4:67, 18, 28, 48, 10, 38, 58, 8, 68), 7)]
  data <- data[race_1977 != 999]
}

data_og <- copy(data)
data <- merge(data,loc,by.x="location_id", by.y="merged_location_id",all.x=T)
# check the merge
data[is.na(state)]
unique(data$state)

locs <- fread('FILEPATH/americas_definitions.csv')

death_count <- round(sum(data$deaths),4)
data <- merge(data, locs,
              by.x = c('mcnty','race_1977'),
              by.y = c('mcnty','race'),
              all.x = T)

if(death_count != round(sum(data$deaths), 4)){
  stop(paste0('add_location_metadata failed'))
}

# check the counties to make sure the naming is correct
setdiff(loc$mcnty, data$mcnty)
setdiff(data$mcnty, loc$mcnty)
setdiff(data$america, locs$america)

# keep track of total deaths
total <- data[, sum(deaths), keyby = 'mcnty,race_1977,america,year_id,sex_id']
setnames(total, 'race_1977', 'race')

# re-map some age groups
data[age_group_id == "1-5mo", age_group_id := 0]
data[age_group_id == "6-11mo", age_group_id := 0]
data[age_group_id == "0.01", age_group_id := 0]
data[age_group_id == "12-23mo", age_group_id := 1]
data[age_group_id == "2-4", age_group_id := 1]
data[age_group_id == "-9", age_group_id := NA]
data[,age_group_id := as.numeric(age_group_id)]
data[age_group_id > 85, age_group_id := 85]


#### some formatting for the rest of the analysis
setnames(data, c("age_group_id", "year_id", "sex_id", "race_1977"), c("age", "year", "sex", "race"))

## split unknown ages
## (proportional to deaths with known age in the same state, sex, race/ethnicity, year)
temp1 <- data[!is.na(age), list(state_deaths = sum(deaths)), by = 'state,year,sex,age,race']
temp1[, state_death_prop := state_deaths / sum(state_deaths), by = 'state,year,sex,race']
temp2 <- data[is.na(age), list(state, mcnty, america, year, sex, race, deaths)]
temp2 <- merge(temp2, temp1, by = c("state", "year", "sex", "race"),
               all.x = T, allow.cartesian = T)
temp2[, deaths := deaths * state_death_prop]
temp2 <- temp2[, list(state, year, mcnty, america, sex, age, race, deaths)]

data <- data[!is.na(age), ]
data <- data[,(names(temp2)), with=F]
data <- rbind(data, temp2)

data <- data[, list(deaths = sum(deaths)), keyby = 'mcnty,race,america,year,sex,age']

setnames(total, c("year_id", "sex_id"), c("year", "sex"))
stopifnot(all.equal(total, data[, sum(deaths), keyby = 'mcnty,race,america,year,sex']))
rm(temp1, temp2)

data[,cause_id := 294]


# Save data
OUT_DIR <- paste0('FILEPATH')
ARCHIVE_DIR <- paste0(OUT_DIR, '/_archive/', gsub('-', '_', Sys.Date()), '/')
dir.create(ARCHIVE_DIR, showWarnings = FALSE)

for(yr in sort(unique(data$year))){
  message(yr)
  saveRDS(data[year == yr], file = paste0(ARCHIVE_DIR, 'all_cause_deaths_', 
                                          yr, '.RDS'))
}
