##############################################################################################
## Description: Prep populations by mcnty-year-age-sex-race/ethnicity for the 1977 race
##              categories
## Outputs:     population by merged county, year, age, sex, and combined race/Hispanic
##                ethnicity group.
##############################################################################################

library(data.table)
library(ggplot2)
library(RColorBrewer)

rm(list = ls())

source(paste0("/_prep_workspace.R"))
pop_dir <- "[FILEPATH]"
loc <- fread("[FILEPATH]")
loc2 <- fread("[FILEPATH]")
archive_date <- paste0(gsub("-", "_", Sys.Date()), "/")  # folder named for plot archive date (today)

# identify and isolate last-updated population file, based on last modified time, for comparison
files <- file.info(list.files(paste0(pop_dir, "[FILEPATH]"), full.names = T))
last_archived <- rownames(files)[which.max(files$mtime)]; rm(files)

# create new folder for outputs if one doesn't exist already
if (!dir.exists(paste0(pop_dir, "[FILEPATH]"))) dir.create(paste0(pop_dir, "[FILEPATH]"))

## Load raw population data, and collapse to merged counties ---------------------------------

## Identify and isolate most up-to-date population source files, based on last modified time
files <- file.info(list.files(paste0(pop_dir, "[FILEPATH]"), full.names = T))
file <- rownames(files)[which.max(files$mtime)]

# nchs bridged race estimates
pop2 <- readRDS(file)
pop2 <- pop2[year >= 1998, ]
pop2 <- merge(pop2, loc[, list(fips = cnty, mcnty, state)], by = "fips", all.x = T)
pop2 <- pop2[, list(pop = sum(pop)), by = 'state,mcnty,year,sex,age,race,hisp']

# For the bridged race estimates, Hispanic ethnicity is still a single group (7)
# and non-Hispanics are split by bridged race (groups 1-4 = white, black, AIAN,
# and API)
pop2[, race := ifelse(hisp == 1, 7, race)]
pop2 <- pop2[, list(pop = sum(pop)), by = 'state,mcnty,year,sex,age,race']

## aggregate the age groups into 5-year groups
pop2[, age_start := 5 * floor(age / 5)]
pop2[, age_end := ifelse(age == 85, NA, age_start + 4)]
pop2[age == 0, age_end := 0]
pop2[age %between% c(1,4), age_start := 1]

pop2 <- pop2[, list(pop = sum(pop)), by = 'state,mcnty,year,sex,age_start,age_end,race']
pop2[,age := age_start]
pop2[,c("age_start","age_end") := NULL]

pop <- copy(pop2)

## Scale to GBD population -------------------------------------------------------------------
# get GBD age_group_ids
source("[FILEPATH]")
age_ids <- get_age_metadata(age_group_set_id = 19, gbd_round_id = 7)
age_ids <- age_ids[, list(age_group_id, age = age_group_years_start)]
age_ids[age < 1, age := 0]
age_ids[age >= 85, age := 85]
age_ids[age == 2, age := 1]

# get GBD population
gbd <- mortdb::get_mort_outputs(model_name = "population", run_id = 292, model_type = "estimate", 
                                location_ids = loc2[fips < 60, location_id], year_ids = unique(pop$year), 
                                sex_ids = 1:2, gbd_year = 2020, age_group_ids = age_ids$age_group_id)
gbd <- merge(gbd, age_ids, by = "age_group_id", all = T)
stopifnot(nrow(gbd[is.na(mean)]) == 0)
stopifnot(nrow(gbd[is.na(age)]) == 0)
gbd <- gbd[, list(gbd_pop = sum(mean)), by = 'location_id,year_id,sex_id,age']

# map to states
gbd <- merge(gbd, loc2[, list(location_id, state = fips)], by = "location_id", all.x = T)
gbd <- gbd[, list(state, year = year_id, sex = sex_id, age, gbd_pop)]

# rake population
pop <- merge(pop, gbd, by = c("state", "year", "sex", "age"), all = T)
pop[, rf := gbd_pop / sum(pop), by = 'state,year,sex,age']
pop[, pop_raked := pop * rf]


## Save the output ---------------------------------------------------------------------------
temp <- copy(pop)
temp[, c("pop", "gbd_pop", "rf") := NULL]
setnames(temp, "pop_raked", "pop")
setcolorder(temp, c("mcnty", "year", "sex", "age", "race", "pop"))
setkeyv(temp, c("mcnty", "year", "sex", "age", "race", "pop"))
cov_id <- cov_ids[covariate_name_short == "pop_by_race_ethn_1977", covariate_id]  # add covariate id
temp[, covariate_id := cov_id]

# name output file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))
saveRDS(temp, file = paste0(pop_dir, "[FILEPATH]", date_time_stamp, ".rds"))

## Make diagnostic plots ---------------------------------------------------------------------
dir.create(paste0(pop_dir, "[FILEPATH]", archive_date), showWarnings = F)
plot_dir <- paste0(pop_dir, "[FILEPATH]", archive_date)

# format relevant variables
pop <- copy(temp)
pop[, sex := factor(sex, 1:2, c("Males", "Females"))]
pop[, age_lab := factor(paste("Age:", age))]
pop[, race := factor(race, levels = c(1,2,3,4,7),
                     labels = c("NH White", "NH Black", "NH AIAN", "NH API", "Hispanic"))]

# plot state-level trends by race/ethnicity
fdata1 <- pop[, list(pop = sum(pop)), by = 'state,year,sex,age,race']
fdata1 <- melt(fdata1, id.vars = c("state", "year", "sex", "age", "race"), value.name = "pop")
fdata2 <- fdata1[, list(pop = sum(pop)), by = 'state,year,sex,race']
fdata3 <- fdata1[, list(pop = sum(pop)), by = 'state,year,race']

### compare to the old population estimates that have all races just to make sure nothing is super off
old_pop <- readRDS(last_archived)
old_pop[, sex := factor(sex, 1:2, c("Males", "Females"))]
old_pop[, age_lab := factor(paste("Age:", age))]
old_pop[, race := factor(race, levels = 1:7,
                     labels = c("NH White", "NH Black", "NH AIAN", "NH Asian",
                                "NH NHOPI", "NH Multiracial", "Hispanic"))]
fdata1_old <- old_pop[, list(pop = sum(pop)), by = 'state,year,sex,age,race']
fdata1_old <- melt(fdata1_old, id.vars = c("state", "year", "sex", "age", "race"), value.name = "pop")
fdata2_old <- fdata1_old[, list(pop = sum(pop)), by = 'state,year,sex,race']
fdata3_old <- fdata1_old[, list(pop = sum(pop)), by = 'state,year,race']

# combine data sets
all_data_high_level <- rbind(fdata3_old[,version := "OLD"], fdata3[,version := "NEW"])
all_data_mid_level <-  rbind(fdata2_old[,version := "OLD"], fdata2[,version := "NEW"])
all_data_low_level <-  rbind(fdata1_old[,version := "OLD"], fdata1[,version := "NEW"])

# prepare for scatters:
all_data_high_level_wide <- dcast.data.table(all_data_high_level, state + year + race ~ version, value.var="pop")
all_data_mid_level_wide <- dcast.data.table(all_data_mid_level, state + year + race + sex ~ version, value.var="pop")
all_data_low_level_wide <- dcast.data.table(all_data_low_level, state + year + race + sex + age ~ version, value.var="pop")

# create the plots
pdf(paste0(plot_dir, "[FILEPATH]"), width = 14, height = 8)

print(ggplot(all_data_high_level_wide,aes(OLD, NEW, color = race)) + geom_point() +
        geom_abline(intercept=0,slope=1) + theme_bw() +
        ggtitle("Difference in populations, aggregated to state level age and sex"))

print(ggplot(all_data_high_level_wide,aes(OLD, NEW, color = race)) + geom_point() +
        geom_abline(intercept=0,slope=1) + theme_bw() + facet_wrap(~year) +
        ggtitle("Difference in populations, aggregated to state level age and sex"))

print(ggplot(all_data_mid_level_wide,aes(OLD, NEW, color = race)) + geom_point() +
        geom_abline(intercept=0,slope=1) + theme_bw() + facet_wrap(~sex) +
        ggtitle("Difference in populations, aggregated to state level by age"))

for(a in unique(all_data_low_level_wide$age)){
  print(ggplot(all_data_low_level_wide[age == a],aes(OLD, NEW, color = race)) + geom_point() +
          geom_abline(intercept=0,slope=1) + theme_bw() + facet_wrap(~sex) +
          ggtitle(paste0("Difference in populations, aggregated to state level, age ",a)))
}


dev.off()
