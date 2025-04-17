##############################################################################################
## Description: Prep populations by mcnty-year-age-sex-race/ethnicity for the 1977 race
##              categories. Also produce populations by mcnty-year-age-sex by aggregating
##              across race, ensuring that raking versions are always aligned.
##############################################################################################

library(ggplot2)
library(ggforce)
library(RColorBrewer)

source("_functions/_prep_workspace.R")
source("_functions/_bridge_nchs_pop.R")
source("_functions/create_intercensal_2010_2020.R")
loc2 <- fread("FILEPATH")
archive_date <- paste0(gsub("-", "_", Sys.Date()), "/")  # folder named for plot archive date (today)

## Load raw population data -----------------------------------------------------------------------
## nchs bridged race estimates
pop_meta_df <- get_population_metadata("nchs_pop_est_by_race_ethn")  # load metadata for best extraction
pop <- readRDS(paste0(pop_dir, "raw/nchs_pop_est_by_race_ethn/", unique(pop_meta_df[, file])))  # read in file specified in metadata

## census population estimates for bridging 2021+
census_meta_df <- get_population_metadata("census_pop_est_by_race_ethn")
census_pop <- readRDS(paste0(pop_dir, "raw/census_pop_est_by_race_ethn/", unique(census_meta_df[, file])))[year >= 2020][, -"nid"]
setnames(census_pop, "race", "race_label")  # assign race ids to census estimates for merging
census_pop[, race := dplyr::case_when(race_label == "White alone" ~ 1, race_label == "Black alone" ~ 2,
                                      race_label == "AIAN alone" ~ 3, race_label %in% c("NHOPI alone", "Asian alone") ~ 4,
                                      race_label == "Two or More races" ~ 9, TRUE ~ NA_real_)]
stopifnot(nrow(census_pop[is.na(race)]) == 0)  # should be no missing race info
census_pop[, race := ifelse(hisp == 1, 7, race)]  # assign Hispanic based on indicator variable
census_pop <- census_pop[, list(pop_alone = sum(pop)), by = 'fips,year,sex,age_start,race']

## for the bridged race estimates, Hispanic ethnicity is still a single group (7) and non-Hispanics
## are split by bridged race (groups 1-4 = white, black, AIAN, and API)
pop[, race := ifelse(hisp == 1, 7, race)]
pop <- pop[, list(pop = sum(pop)), by = 'fips,year,sex,age,race']

## Bridge 2021+ estimates to 1977 race/ethnicity groups --------------------------------------------
## calculate proportions from NCHS 2020 for splitting 0-4 age group
props_u5 <- copy(pop[age < 5 & year == 2020])
props_u5[, age := ifelse(age == 0, 0, 1)]
props_u5 <- props_u5[, list(pop = sum(pop)), by = 'year,age']
props_u5[, freq := pop/sum(pop), by = 'year']
prop0 <- props_u5[age == 0, freq]
prop1to4 <- props_u5[age == 1, freq]; rm(props_u5)

## calculate bridged-race populations
pop_bridged <- rbindlist(lapply(unique(census_pop[year >= 2021, year]), function(yr) {
  message("bridging ", yr)
  temp <- bridge_nchs_pop(census2020 = census_pop[year == 2020], dt_to_bridge = census_pop[year == yr],
                          nchs2020 = pop[year == 2020], data_year = yr)
  
  # split 0-4 age group into 0 and 1-4
  pop_u5 <- copy(temp[age_start == 0])
  start_pop <- sum(pop_u5$pop)
  temp <- temp[age_start > 0]
  pop_u5 <- rbind(copy(pop_u5[, age_start := 0]), copy(pop_u5[, age_start := 1]))
  pop_u5[, pop := ifelse(age_start == 0, pop * prop0, pop * prop1to4)]
  stopifnot(all.equal(sum(pop_u5$pop), start_pop))
  temp <- rbind(pop_u5, temp); rm(pop_u5, start_pop)
  temp
}), fill = T, use.names = T)

## classify all population into 5-year age groups
pop[, age_start := 5 * floor(age / 5)]
pop[age %between% c(1, 4), age_start := 1]
pop[, age := NULL]
pop <- rbindlist(list(pop, pop_bridged), use.names = T, fill = T); rm(pop_bridged)
pop <- pop[, list(pop = sum(pop)), by = 'fips,year,sex,age_start,race']

## Estimate intercensal population values for 2010-2020 -------------------------------------------
# load raw postcensal 2020 file (update with each new vintage)
pc20 <- fread(paste0(root, "DATA/USA/POP_ESTIMATES/2020_2022/",
                     "USA_POP_EST_COUNTIES_AGE_SEX_RACE_HISPANIC_ORIGIN_2020_2022_Y2023M07D11.CSV"))
pc20 <- melt(pc20, id.vars = c("SUMLEV", "STATE", "COUNTY", "STNAME", "CTYNAME", "YEAR", "AGEGRP"),
             value.name = "pop")
pc20[, fips := as.integer(STATE * 1000 + COUNTY)]

# format race for bridging
pc20 <- pc20[grepl("^(H|NH)[[:alpha:]]", variable), ] # drop combined races and combined ethnicity totals
pc20 <- pc20[!grepl("AC_", variable), ] # drop "alone or in combination" counts (which overlap with each other)
pc20[grepl("^(H|NH)WA_", variable), race := 1]  # White alone
pc20[grepl("^(H|NH)BA_", variable), race := 2]  # Black alone
pc20[grepl("^(H|NH)IA_", variable), race := 3]  # AIAN alone
pc20[grepl("^(H|NH)AA_", variable), race := 4]  # Asian alone
pc20[grepl("^(H|NH)NA_", variable), race := 4]  # NHOPI alone
pc20[grepl("^(H|NH)TOM_", variable), race := 9]  # Two or more races
stopifnot(nrow(pc20[is.na(race)]) == 0)  # should be no missing race info

# format Hispanic ethnicity
pc20[grepl("^H", variable), hisp := 1L]
pc20[grepl("^NH", variable), hisp := 0]
pc20[, race := ifelse(hisp == 1, 7, race)]  # assign Hispanic based on indicator variable

# format age
pc20 <- pc20[!AGEGRP == 0, ] # drop all ages
pc20[, age_start := as.integer(seq(0, 85, 5))[AGEGRP]]

# format year
pc20[YEAR %in% 2:4, year := YEAR + 2018]  # 2+ = 2020+
pc20[, year := as.integer(year)]
pc20 <- pc20[year == 2020]

# format sex
pc20[grepl("_MALE", variable), sex := 1L]
pc20[grepl("_FEMALE", variable), sex := 2L]

# load county-level CT pops in 2020 and replace planning region-level pops in pc20
ct_pop <- readRDS("FILEPATH")[year == 2020]
ct_pop[, race := dplyr::case_when(race == "White alone" ~ 1, race == "Black alone" ~ 2,
                                  race == "AIAN alone" ~ 3, race %in% c("NHOPI alone", "Asian alone") ~ 4,
                                  race == "Two or More races" ~ 9, TRUE ~ NA_real_)]
stopifnot(nrow(ct_pop[is.na(race)]) == 0)  # should be no missing race info
ct_pop[, race := ifelse(hisp == 1, 7, race)]  # assign Hispanic based on indicator variable
pc20 <- pc20[!fips %in% 9000:9999]
pc20 <- rbindlist(list(pc20, ct_pop), use.names = T, fill = T)
pc20 <- pc20[, list(pop_alone = sum(pop)), by = 'fips,year,sex,age_start,race']

# bridge to 1977 race/ethnicity groups
pc20_bridged <- bridge_nchs_pop(census2020 = census_pop[year == 2020], dt_to_bridge = pc20,
                                nchs2020 = pop[year == 2020], data_year = 2020)

# split ages 0-4 using NCHS 2020 proportions
pop_u5 <- copy(pc20_bridged[age_start == 0])  # then apply to bridged post-2020 census estimates
start_pop <- sum(pop_u5$pop)
pc20_bridged <- pc20_bridged[age_start > 0]
pop_u5 <- rbind(copy(pop_u5[, age_start := 0]), copy(pop_u5[, age_start := 1]))
pop_u5[, pop := ifelse(age_start == 0, pop * prop0, pop * prop1to4)]
stopifnot(all.equal(sum(pop_u5$pop), start_pop))
pc20_bridged <- rbindlist(list(pop_u5, pc20_bridged), use.names = T, fill = T); rm(pop_u5, start_pop, prop0, prop1to4)

# estimate 2010-2020 intercensal population
pc20_bridged[, pop := as.numeric(pop)]  # convert pop values to numeric from character
pop_ic <- create_intercensal(pc10 = pop[year %in% 2010:2020], pc20_2020 = pc20_bridged)
pc20_22 <- rbind(pc20_bridged, pop[year >= 2021])  # save for vetting plots

## Replace 2010-2020 with intercensal estimates and format for raking -----------------------------
nchs_00_20 <- copy(pop[year %in% 2000:2020])  # save for vetting plots
pop <- pop[!year %in% 2010:2020]
pop <- rbindlist(list(pop, pop_ic), use.names = T, fill = T); rm(pop_ic, pc20, pc20_bridged, census_pop)

## collapse to mcnty, rename age_start to age, add state
pop <- merge(pop, loc[, list(fips, mcnty, state)], by = "fips", all.x = T)
setnames(pop, "age_start", "age")
pop <- pop[, list(pop = sum(pop)), by = 'state,mcnty,year,sex,age,race']
stopifnot(nrow(pop[is.na(mcnty)]) == 0)  # make sure merged counties are nonmissing

## Scale to GBD population ------------------------------------------------------------------------

# get GBD age_group_ids
source("FILEPATH/get_age_metadata.R")
age_ids <- get_age_metadata(release_id = 16)
age_ids <- age_ids[, list(age_group_id, age = age_group_years_start)]
age_ids[age < 1, age := 0]
age_ids[age >= 85, age := 85]
age_ids[age == 2, age := 1]

# get GBD population
source("FILEPATH/get_population.R")
gbd <- get_population(run_id = 377, release_id = 16, age_group_id = age_ids$age_group_id, sex_id = 1:2,
                      year_id = unique(pop$year), location_id = loc2[fips < 60, location_id])

gbd <- merge(gbd, age_ids, by = "age_group_id", all = T)

stopifnot(nrow(gbd[is.na(population)]) == 0)
stopifnot(nrow(gbd[is.na(age)]) == 0)
gbd <- gbd[, list(gbd_pop = sum(population)), by = 'location_id,year_id,sex_id,age']

# map to states
gbd <- merge(gbd, loc2[, list(location_id, state = fips)], by = "location_id", all.x = T)
gbd <- gbd[, list(state, year = year_id, sex = sex_id, age, gbd_pop)]

# load and format gbd 2023 run id 390 final estimates for plots
gbd_390 <- get_population(run_id = 390, release_id = 16, age_group_id = age_ids$age_group_id, sex_id = 1:2,
                         year_id = unique(pop$year), location_id = loc2[fips < 60, location_id])

gbd_390 <- merge(gbd_390, age_ids, by = "age_group_id", all = T)
stopifnot(nrow(gbd_390[is.na(population)]) == 0)
stopifnot(nrow(gbd_390[is.na(age)]) == 0)
gbd_390 <- gbd_390[, list(gbd_pop = sum(population)), by = 'location_id,year_id,sex_id,age']
gbd_390 <- merge(gbd_390, loc2[, list(location_id, state = fips)], by = "location_id", all.x = T)
gbd_390 <- gbd_390[, list(state, year = year_id, sex = sex_id, age, gbd_pop390 = gbd_pop)]

# rake population
pop <- merge(pop, gbd, by = c("state", "year", "sex", "age"), all = T)
pop[, rf := gbd_pop / sum(pop), by = 'state,year,sex,age']
pop[, pop_raked := pop * rf]

## Create pop by age and sex ----------------------------------------------------------------------
pop[, c("gbd_pop", "rf") := NULL]
setnames(pop, c("pop", "pop_raked"), c("pop_orig", "pop"))  # keep unraked pop for plots
setcolorder(pop, c("mcnty", "year", "sex", "age", "race", "pop"))
setkeyv(pop, c("mcnty", "year", "sex", "age", "race", "pop"))

pop_age_sex <- copy(pop)
pop_age_sex <- pop_age_sex[, list(pop = sum(pop), pop_orig = sum(pop_orig)), by = 'mcnty,state,year,sex,age']

## Save the output --------------------------------------------------------------------------------

# recode race IDs to match those in shared DB
pop[, race := car::recode(race, "1=5; 2=4; 3=6; 4=7; 7=2; else=NA")]
stopifnot(nrow(pop[is.na(race)]) == 0)

# assign race set ID from ushd_shared.population_group_set table
pop[, race_set := pop_group_sets[population_group_set_name == "omb_1977", population_group_set_id]]

# name output file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
saveRDS(pop, paste0(pop_dir, "/prepped/pop_by_race_ethn_1977/", date_time_stamp, ".rds"))
saveRDS(pop_age_sex, paste0(pop_dir, "/prepped/pop_by_age_sex/", date_time_stamp, ".rds"))

## Make diagnostic plots --------------------------------------------------------------------------
# 1. Pop by race/ethnicity
dir.create(paste0(pop_dir, "plots/pop_by_race_ethn_1977/", archive_date), showWarnings = F)
dir.create(paste0(pop_dir, "/plots/pop_by_age_sex/", archive_date), showWarnings = F)
plot_dir <- paste0(pop_dir, "plots/pop_by_race_ethn_1977/", archive_date)

# add gbd 2023 run id 390 pop for plots
pop <- merge(pop, gbd_390, by = c("state", "year", "sex", "age"), all = T)
pop[, rf390 := gbd_pop390 / sum(pop), by = 'state,year,sex,age']
pop[, pop_raked390 := pop * rf390]
pop[, c("gbd_pop390", "rf390") := NULL]

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

### compare to the old population estimates that have all races just to make sure nothing is super off
# grab last version of population for plot comparison
pop_versions <- get_covariate_version("pop_by_race_ethn_1977")  # load pop version metadata table
old_pop_id <- pop_versions[nrow(pop_versions), covariate_dataset_id]  # grab id from last row of version table
old_pop <- get_population_data(covariate_dataset_id = old_pop_id)  # specify id of last pop dataset to get the right output
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
pdf("FILEPATH", width = 14, height = 8)

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

# if you just want to plot the pyramids using the pre-existing file:
fdata1 <- merge(pop, unique(loc[,.(mcnty, cnty_name, state_name)]), by="mcnty", allow.cartesian=T)
fdata1 <- fdata1[year %in% c(seq(1990, 2020, 10))]
fdata1[, pop := pop / sum(pop), by = 'mcnty,year,race']
fdata1[sex == "Males", pop := -1 * pop]

for(r in unique(fdata1$race)) {
  message(r)
  pdf("FILEPATH", width = 17, height = 8)
  for (s in unique(fdata1$state_name)) {
    message(s)
    pages <- ceiling(fdata1[state_name == s & race == r, uniqueN(cnty_name)] / 48)
    for (y in unique(fdata1$year)) {
      message(y)
      for (p in 1:pages) {
        message(p)
        g <- ggplot(fdata1[state_name == s & year == y & race == r, ],
                    aes(y = pop, x = factor(age), fill = sex)) +
          coord_flip() +
          geom_bar(stat = "identity") +
          scale_y_continuous(labels = function(x) abs(x)) +
          scale_x_discrete(breaks = c(0, seq(5, 85, 20))) +
          labs(title = paste0("Population age-sex structure by county: ", s, ", ", y,", ",r),
               y = "Proportion of total population", x = "Age")
        if (pages == 1) {
          g <- g + facet_wrap(~ cnty_name)
        } else {
          g <- g + facet_wrap_paginate(~ cnty_name, nrow = 6, ncol = 8, page = p)
        }
        print(g)
      }
    }
  }

  dev.off()
}

# 2. Pop by age and sex
plot_dir <- paste0(pop_dir, "plots/pop_by_age_sex/", archive_date)

# format relevant variables
pop_age_sex[, sex := factor(sex, 1:2, c("Males", "Females"))]
pop_age_sex[, age_lab := factor(paste("Age:", age), levels = paste("Age:", sort(unique(age))))]
pc20_22 <- merge(pc20_22, loc[, list(fips, mcnty, state)], by = "fips", all.x = T)  # post-2020 census estimates, 2020-21
setnames(pc20_22, "age_start", "age")
pc20_22 <- pc20_22[, list(postcensal = sum(pop)), by = 'state,mcnty,year,sex,age,race']
pc20_22[, sex := factor(sex, 1:2, c("Males", "Females"))]
pc20_22[, race := car::recode(race, "1=5; 2=4; 3=6; 4=7; 7=2; else=NA")]
pc20_22[, race := factor(race, levels = c(2, 4, 5, 6, 7),
                         labels = c("Hispanic", "NH Black", "NH White", "NH AIAN", "NH API"))]
nchs_00_20 <- merge(nchs_00_20, loc[, list(fips, mcnty, state)], by = "fips", all.x = T)  # original NCHS bridged-race estimates, 2000-20
setnames(nchs_00_20, "age_start", "age")
nchs_00_20 <- nchs_00_20[, list(nchs = sum(pop)), by = 'state,mcnty,year,sex,age,race']
nchs_00_20[, sex := factor(sex, 1:2, c("Males", "Females"))]
nchs_00_20[, race := car::recode(race, "1=5; 2=4; 3=6; 4=7; 7=2; else=NA")]
nchs_00_20[, race := factor(race, levels = c(2, 4, 5, 6, 7),
                            labels = c("Hispanic", "NH Black", "NH White", "NH AIAN", "NH API"))]

# plot state-level raked, unraked, bridged (2000-20), and postcensal (2020-21) populations
fdata1 <- pop[, list(unraked = sum(pop_orig), raked23 = sum(pop), raked390 = sum(pop_raked390)),
              by = 'state,year,sex,age,race']
fdata1 <- merge(fdata1, nchs_00_20[, list(nchs = sum(nchs)), by = 'state,year,sex,age,race'],
                by = c("state", "year", "sex", "age", "race"), all = T)
fdata1 <- merge(fdata1, pc20_22[, list(postcensal = sum(postcensal)), by = 'state,year,sex,age,race'],
                by = c("state", "year", "sex", "age", "race"), all = T)
fdata1 <- melt(fdata1, id.vars = c("state", "year", "sex", "age", "race"), value.name = "pop")
fdata1[, group_label := case_when(variable == "unraked" & year %in% 1990:1999 ~ "Census/NCHS Intercensal estimates",
                               variable == "nchs" & year %in% 2000:2009 ~ "Census/NCHS Intercensal estimates",
                               variable == "nchs" & year %in% 2010:2019 ~ "Census/NCHS Postcensal estimates",
                               variable == "postcensal" & year >= 2021 ~ "Census/NCHS Postcensal estimates",
                               variable == "unraked" & year %in% 2010:2020 ~ "IHME Intercensal",
                               variable == "raked390" ~ "GBD 2023 run id 390",
                               variable == "raked23" ~ "GBD 2023 run id 377",
                               TRUE ~ "remove")]
fdata1 <- fdata1[group_label != "remove"]
fdata2 <- fdata1[, list(pop = sum(pop)), by = 'state,year,sex,age,variable,group_label']  # state-level
fdata3 <- fdata1[, list(pop = sum(pop)), by = 'state,year,sex,race,variable,group_label']
fdata4 <- fdata1[, list(pop = sum(pop)), by = 'state,year,sex,variable,group_label']
fdata5 <- fdata1[, list(pop = sum(pop)), by = 'state,year,variable,group_label']

fdata6 <- fdata1[, list(pop = sum(pop)), by = 'year,sex,age,variable,group_label']  # national-level
fdata7 <- fdata1[, list(pop = sum(pop)), by = 'year,sex,race,variable,group_label']
fdata8 <- fdata1[, list(pop = sum(pop)), by = 'year,sex,variable,group_label']
fdata9 <- fdata1[, list(pop = sum(pop)), by = 'year,variable,group_label']

group_colors <- c("Census/NCHS Intercensal estimates" = "#a6cee3", "Census/NCHS Postcensal estimates" = "#1f78b4",
                  "IHME Intercensal" = "#33a02c", "GBD 2023 run id 377" = "#fb9a99", "GBD 2023 run id 390" = "#e31a1c")

pdf("FILEPATH", width = 17, height = 8)

# national-level by year
print(ggplot(fdata9, aes(x = year, y = pop / 1000, color = group_label)) +
        geom_line() + geom_point() +
        labs(title = "United States", x = "Year", y = "Pop (1,000s)", color = "") +
        theme_minimal() +
        scale_color_manual(values = group_colors)
)

# national-level by year and sex
print(ggplot(fdata8, aes(x = year, y = pop / 1000, color = group_label)) +
        facet_grid(~ sex) +
        geom_line() + geom_point() +
        labs(title = "United States", x = "Year", y = "Pop (1,000s)", color = "") +
        theme_minimal() +
        scale_color_manual(values = group_colors)
)

# national-level by year, sex, race
print(ggplot(fdata7, aes(x = year, y = pop / 1000, color = group_label,
                         shape = sex, linetype = sex)) +
        facet_wrap(~ race, scales = "free_y") +
        geom_line() + geom_point() +
        labs(title = "United States", x = "Year", y = "Pop (1,000s)", color = "") +
        theme_minimal() +
        scale_color_manual(values = group_colors)
)

# national-level by year, sex, age
print(ggplot(fdata6, aes(x = year, y = pop / 1000, color = group_label,
                         shape = sex, linetype = sex)) +
        facet_wrap(~ age, scales = "free_y") +
        geom_line() + geom_point() +
        labs(title = "United States", x = "Year", y = "Pop (1,000s)", color = "") +
        theme_minimal() +
        scale_color_manual(values = group_colors)
)

for (s in unique(loc$state)) {
  
  # state-level by year
  g <- ggplot(fdata5[state == s, ], aes(x = year, y = pop / 1000, color = group_label)) +
    geom_line() + geom_point() +
    labs(title = loc2[fips == s, location_name], x = "Year", y = "Pop (1,000s)", color = "") +
    theme_minimal() +
    scale_color_manual(values = group_colors)
  print(g)

  # state-level by year and sex
  g <- ggplot(fdata4[state == s, ], aes(x = year, y = pop / 1000, color = group_label)) +
    facet_grid(~ sex) +
    geom_line() + geom_point() +
    labs(title = loc2[fips == s, location_name], x = "Year", y = "Pop (1,000s)", color = "") +
    theme_minimal() +
    scale_color_manual(values = group_colors)
  print(g)

  # state-level by year, sex, race
  g <- ggplot(fdata3[state == s, ], aes(x = year, y = pop / 1000, color = group_label,
                                        shape = sex, linetype = sex)) +
    facet_wrap(~ race, scales = "free_y") +
    geom_line() + geom_point() +
    labs(title = loc2[fips == s, location_name], x = "Year", y = "Pop (1,000s)", color = "") +
    theme_minimal() +
    scale_color_manual(values = group_colors)
  print(g)
  
  # state-level by year, sex, age
  g <- ggplot(fdata2[state == s, ], aes(x = year, y = pop / 1000, color = group_label,
                                        shape = sex, linetype = sex)) +
    facet_wrap(~ age, scales = "free_y") +
    geom_line() + geom_point() +
    labs(title = loc2[fips == s, location_name], x = "Year", y = "Pop (1,000s)", color = "") +
    theme_minimal() +
    scale_color_manual(values = group_colors)
  print(g)
}

dev.off()

rm(fdata1, fdata2, fdata3, fdata4, fdata5, fdata6, fdata7, fdata8, fdata9)

# plot mcnty-level time trends and population pyramids
fdata1 <- loc[current == 1, list(cnty_name = paste(cnty_name, collapse = ", ")), by = 'state_name,mcnty']
fdata1 <- merge(fdata1, pop_age_sex[, list(pop = sum(pop)), by = 'mcnty,year'], by = "mcnty")
fdata1[, max_pop := max(pop), by = "mcnty"]
fdata1[, max_pop := cut(max_pop, c(0, 10^c(2:6, 8)))]

fdata2 <- loc[current == 1, list(cnty_name = paste(cnty_name, collapse = ", ")), by = 'state_name,mcnty']
fdata2 <- merge(fdata2, pop_age_sex[year %in% c(seq(1990, 2020, 10)), ], by = "mcnty")
fdata2[, pop := pop / sum(pop), by = 'mcnty,year']
fdata2[sex == "Males", pop := -1 * pop]

pdf("FILEPATH", width = 17, height = 8)

for (s in unique(fdata1$state_name)) {

  g <- ggplot(fdata1[state_name == s, ],
              aes(x = year, y = pop, group = mcnty, label = cnty_name, color = cnty_name)) +
    facet_wrap(~ max_pop, scales = "free_y") +
    geom_line(show.legend = F) +
    geom_text(data = fdata1[state_name == s & year == 2020, ], hjust = -0.01, size = 3, show.legend = F) +
    lims(x = c(1983, 2023)) +
    labs(title = paste0("Total population by county: ", s), x = "Year", y = "Population")
  print(g)

  pages <- ceiling(fdata2[state_name == s, uniqueN(cnty_name)] / 48)
  for (y in unique(fdata2$year)) {
    for (p in 1:pages) {
      g <- ggplot(fdata2[state_name == s & year == y, ],
                  aes(y = pop, x = factor(age), fill = sex)) +
        coord_flip() +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = function(x) abs(x)) +
        scale_x_discrete(breaks = c(0, seq(5, 85, 20))) +
        labs(title = paste0("Population age-sex structure by county: ", s, ", ", y),
             y = "Proportion of total population", x = "Age")
      if (pages == 1) {
        g <- g + facet_wrap(~ cnty_name)
      } else {
        g <- g + facet_wrap_paginate(~ cnty_name, nrow = 6, ncol = 8, page = p)
      }
      print(g)
    }
  }
}

dev.off()
rm(fdata1, fdata2)
