##############################################################################################
## Description: Prep populations by mcnty-year-age-sex-race/ethnicity for the 1977 race
##              categories. Also produce populations by mcnty-year-age-sex by aggregating
##              across race, ensuring that raking versions are always aligned.
## Outputs:     1. population by merged county, year, age, sex, and combined race/Hispanic
##                 ethnicity group
##              2. population by merged county, year, age, and sex
##############################################################################################

library(ggplot2)
library(ggforce)
library(RColorBrewer)

source("_prep_workspace.R")
loc2 <- fread("[FILEPATH]")
archive_date <- paste0(gsub("-", "_", Sys.Date()), "/")  # folder named for plot archive date (today)

## Load raw population data, and collapse to merged counties ---------------------------------
# nchs bridged race estimates
pop_meta_df <- get_population_metadata("nchs_pop_est_by_race_ethn")  # load metadata for best extraction
pop <- readRDS("[FILEPATH]")  # read in file specified in metadata
pop <- merge(pop, loc[, list(fips, mcnty, state)], by = "fips", all.x = T)
pop <- pop[, list(pop = sum(pop)), by = 'state,mcnty,year,sex,age,race,hisp']

# For the bridged race estimates, Hispanic ethnicity is still a single group (7)
# and non-Hispanics are split by bridged race (groups 1-4 = white, black, AIAN,
# and API)
pop[, race := ifelse(hisp == 1, 7, race)]
pop <- pop[, list(pop = sum(pop)), by = 'state,mcnty,year,sex,age,race']

## aggregate the age groups into 5-year groups
pop[, age_start := 5 * floor(age / 5)]
pop[, age_end := ifelse(age == 85, NA, age_start + 4)]
# the below statements used to only be for years < 2010. from the original code, it looks like this is only for splitting purposes
# but, if this is not the case then this needs to be changed back
pop[age == 0, age_end := 0]
pop[age %between% c(1,4), age_start := 1]

pop <- pop[, list(pop = sum(pop)), by = 'state,mcnty,year,sex,age_start,age_end,race']
pop[,age := age_start]
pop[,c("age_start","age_end") := NULL]

## Scale to GBD population -------------------------------------------------------------------

# get GBD age_group_ids
source("[FILEPATH]")
age_ids <- get_age_metadata(age_group_set_id = 19, gbd_round_id = 7)
age_ids <- age_ids[, list(age_group_id, age = age_group_years_start)]
age_ids[age < 1, age := 0]
age_ids[age >= 85, age := 85]
age_ids[age == 2, age := 1]

# get GBD population
source("[FILEPATH]")
gbd <- get_population(run_id = 292, release_id = 15, age_group_id = age_ids$age_group_id, sex_id = 1:2,
                      year_id = unique(pop$year), location_id = loc2[fips < 60, location_id])

gbd <- merge(gbd, age_ids, by = "age_group_id", all = T)

stopifnot(nrow(gbd[is.na(population)]) == 0)
stopifnot(nrow(gbd[is.na(age)]) == 0)
gbd <- gbd[, list(gbd_pop = sum(population)), by = 'location_id,year_id,sex_id,age']

# map to states
gbd <- merge(gbd, loc2[, list(location_id, state = fips)], by = "location_id", all.x = T)
gbd <- gbd[, list(state, year = year_id, sex = sex_id, age, gbd_pop)]

# rake population
pop <- merge(pop, gbd, by = c("state", "year", "sex", "age"), all = T)
pop[, rf := gbd_pop / sum(pop), by = 'state,year,sex,age']
pop[, pop_raked := pop * rf]

## Create pop by age and sex -----------------------------------------------------------------
pop[, c("gbd_pop", "rf") := NULL]
setnames(pop, c("pop", "pop_raked"), c("pop_orig", "pop"))  # keep for plots
setcolorder(pop, c("mcnty", "year", "sex", "age", "race", "pop"))
setkeyv(pop, c("mcnty", "year", "sex", "age", "race", "pop"))

pop_age_sex <- copy(pop)
pop_age_sex <- pop_age_sex[, list(pop = sum(pop), pop_orig = sum(pop_orig)), by = 'mcnty,state,year,sex,age']

## Save the output ---------------------------------------------------------------------------

# recode race IDs to match those in shared database
pop[, race := car::recode(race, "1=5; 2=4; 3=6; 4=7; 7=2; else=NA")]
stopifnot(nrow(pop[is.na(race)]) == 0)

# assign race set ID from ushd_shared.population_group_set table
pop[, race_set := pop_group_sets[population_group_set_name == "omb_1977", population_group_set_id]]

# name output file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))
saveRDS(pop, paste0(pop_dir, "[FILEPATH]", date_time_stamp, ".rds"))
saveRDS(pop_age_sex, paste0(pop_dir, "[FILEPATH]", date_time_stamp, ".rds"))

# upload data to db
prepped_pop_id <- save_prepped_population(path = paste0(pop_dir, "[FILEPATH]", date_time_stamp, ".rds"),
                                          description = "Population by race/ethnicity, 1977 OMB standards, 1990-2020; run id 292",
                                          prev_issues = "none",
                                          sources = list(c("nchs_pop_est_by_race_ethn", unique(pop_meta_df[, file]))),
                                          ignore_unmatched = T)
prepped_pop_age_sex_id <- save_prepped_population(path = paste0(pop_dir, "[FILEPATH]", date_time_stamp, ".rds"),
                                                  description = "Population by age and sex, 1990-2020; run id 292",
                                                  prev_issues = "none",
                                                  sources = list(c("nchs_pop_est_by_race_ethn", unique(pop_meta_df[, file]))),
                                                  ignore_unmatched = T)

## Make diagnostic plots ---------------------------------------------------------------------
dir.create(paste0(pop_dir, "[FILEPATH]", archive_date), showWarnings = F)
plot_dir <- paste0(pop_dir, "[FILEPATH]", archive_date)

# format relevant variables
pop[, sex := factor(sex, 1:2, c("Males", "Females"))]
pop[, age_lab := factor(paste("Age:", age))]
pop[, race := factor(race, levels = c(2, 4, 5, 6, 7),
                     labels = c("Hispanic", "NH Black", "NH White", "NH AIAN", "NH API"))]

# plot state-level trends by race/ethnicity
fdata1 <- pop[, list(pop = sum(pop)), by = 'state,year,sex,age,race']
fdata1 <- melt(fdata1, id.vars = c("state", "year", "sex", "age", "race"), value.name = "pop")
fdata2 <- fdata1[, list(pop = sum(pop)), by = 'state,year,sex,race']
fdata3 <- fdata1[, list(pop = sum(pop)), by = 'state,year,race']

### compare to the old population estimates that have all races to make sure nothing is  off
pop_versions <- get_covariate_version("pop_by_race_ethn_1977")
old_pop_id <- pop_versions[nrow(pop_versions)-1, covariate_dataset_id]
old_pop <- get_population_data(covariate_dataset_id = old_pop_id)
old_pop[, sex := factor(sex, 1:2, c("Males", "Females"))]
old_pop[, age_lab := factor(paste("Age:", age))]
old_pop[, race := factor(race, levels = c(2, 4, 5, 6, 7),
                         labels = c("Hispanic", "NH Black", "NH White", "NH AIAN", "NH API"))]
fdata1_old <- old_pop[, list(pop = sum(pop)), by = 'state,year,sex,age,race']
fdata1_old <- melt(fdata1_old, id.vars = c("state", "year", "sex", "age", "race"), value.name = "pop")
fdata2_old <- fdata1_old[, list(pop = sum(pop)), by = 'state,year,sex,race']
fdata3_old <- fdata1_old[, list(pop = sum(pop)), by = 'state,year,race']

# combine data sets (common years only)
all_data_high_level <- rbind(fdata3_old[,version := "OLD"], fdata3[year %in% fdata3_old$year, version := "NEW"])
all_data_mid_level <-  rbind(fdata2_old[,version := "OLD"], fdata2[year %in% fdata2_old$year, version := "NEW"])
all_data_low_level <-  rbind(fdata1_old[,version := "OLD"], fdata1[year %in% fdata1_old$year, version := "NEW"])

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
        ggtitle("Difference in populations, aggregated to state level and age"))

for(a in unique(all_data_low_level_wide$age)){
  print(ggplot(all_data_low_level_wide[age == a],aes(OLD, NEW, color = race)) + geom_point() +
          geom_abline(intercept=0,slope=1) + theme_bw() + facet_wrap(~sex) +
          ggtitle(paste0("Difference in populations, aggregated to state level, age ",a)))
}

dev.off()
