###################################################################################################
## Description: Format census population estimates by race and ethnicity for 2000+.
###################################################################################################

# Load settings file and directories --------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("population$", getwd())) setwd("FILEPATH")
library(tidycensus, lib.loc = "FILEPATH")
library(ggplot2)

in_dir  <- "FILEPATH"
nhgis_in_dir <- "FILEPATH"
out_dir <- "FILEPATH"
nids <- data.table(year = 2000:2023,
                   nid = c(rep(438685, 10), rep(512373, 10), rep(560706, 4)))

# Load data ---------------------------------------------------------------------------------------
# load and combine 2000-2010 intercensal files
f <- list.files(path = "FILEPATH", pattern = "FILEPATH", full.names = T)
data_ic10 <- rbindlist(lapply(f, fread))
data_ic10[, type := "intercensal10"]

# load 2010-2020 postcensal (2010) files
f_pc10 <- "FILEPATH"
data_pc10 <- fread(f_pc10)
data_pc10[, type := "postcensal10"]

# load 2020-2023 postcensal (2020) files, combine with 2000-2019
f_pc20 <- "FILEPATH"
data_pc20 <- fread(f_pc20)
data_pc20[, type := "postcensal20"]
data <- rbind(data_ic10, data_pc10, data_pc20, fill = T); rm(data_ic10, data_pc10, data_pc20)

# Format ------------------------------------------------------------------------------------------
# reshape long
data <- melt(data, id.vars = c("SUMLEV", "STATE", "COUNTY", "STNAME", "CTYNAME", "YEAR", "AGEGRP", "type"),
             value.name = "pop")
data[, fips := as.integer(STATE * 1000 + COUNTY)]

# format race
data <- data[grepl("^(H|NH)[[:alpha:]]", variable), ] # drop combined races and combined ethnicity totals
data <- data[!grepl("AC_", variable), ] # drop "alone or in combination" counts (which overlap with each other)
data[grepl("^(H|NH)WA_", variable), race := "White alone"]
data[grepl("^(H|NH)BA_", variable), race := "Black alone"]
data[grepl("^(H|NH)IA_", variable), race := "AIAN alone"]
data[grepl("^(H|NH)AA_", variable), race := "Asian alone"]
data[grepl("^(H|NH)NA_", variable), race := "NHOPI alone"]
data[grepl("^(H|NH)TOM_", variable), race := "Two or More races"]
data[, race := factor(race, levels = c("White alone", "Black alone", "AIAN alone", "Asian alone", "NHOPI alone", "Two or More races"))]

# format Hispanic ethnicity
data[grepl("^H", variable), hisp := 1L]
data[grepl("^NH", variable), hisp := 0]

# format age
data <- data[!((type == "intercensal10" & AGEGRP == 99) | (type %in% c("postcensal10", "postcensal20") & AGEGRP == 0)), ] # drop all ages
data[type == "intercensal10", age_start := as.integer(c(0, 1, seq(5, 85, 5)))[AGEGRP + 1]]
data[type == "intercensal10", age_end := as.integer(c(0, 4, seq(9, 84, 5), NA))[AGEGRP + 1]]
data[type %in% c("postcensal10", "postcensal20"), age_start := as.integer(seq(0, 85, 5))[AGEGRP]]
data[type %in% c("postcensal10", "postcensal20"), age_end := as.integer(c(seq(4, 84, 5), NA))[AGEGRP]]

# format year
data[type == "intercensal10" & YEAR %in% 2:12, year := YEAR + 1998]  # 2-12 = 2000-2010
data[type == "postcensal10" & YEAR %in% 3:13, year := YEAR + 2007]  # 3-13 = 2010-2020
data[type == "postcensal20" & YEAR %in% 2:5, year := YEAR + 2018]  # 2-5 = 2020-2023
data[, year := as.integer(year)]

# format sex
data[grepl("_MALE", variable), sex := 1L]
data[grepl("_FEMALE", variable), sex := 2L]

# subset to desired observations
data <- data[!is.na(year), ] # drop census counts and bases (all in April; keeping July estimates)
data <- data[!(year == 2010 & type == "intercensal10")] # 2010 is in two files; use the version in the postcensal10 file
to_split <- copy(data[fips %in% 9000:9999 & year >= 2020 & type == "postcensal20"])  # isolate post-2020 census CT data to split; include 2020 for intercensal estimates calculation in prep code
data <- data[!(year == 2020 & type == "postcensal20")] # 2020 is in two files; use the version in the postcensal10 file to more closely match NCHS estimates when race bridging

# subset to formatted variables
data <- data[, list(fips, year, sex, age_start, age_end, race, hisp, pop)]
data[, pop := as.numeric(pop)]  # pop is type 'character', set as numeric
to_split <- to_split[, list(fips, year, sex, age_start, age_end, race, hisp, pop)]  # format CT pops
to_split[, pop := as.numeric(pop)]

# Split CT 2020-2023 planning regions -------------------------------------------------------------
# load tract-level 2020 DHCa estimates from NHGIS and format to match PEP
f_tracts <- "FILEPATH"
ct_tracts <- fread(f_tracts, header = T)[STATEA == 9]  # read in tract-level estimates and filter to CT
raw_var_names <- paste(c(paste0("00", 3:9), paste0("0", c(10:25, 27:49))), collapse = "|")
est_vars <- grep(raw_var_names, names(ct_tracts), value = T)  # define columns of interest
setnames(ct_tracts, c("TRACTA"), c("tract_fips"))
ct_tracts[, county_fips := 1000*STATEA+COUNTYA]
ct_tracts <- melt.data.table(ct_tracts, id.vars = c("county_fips", "tract_fips"), measure.vars = est_vars,
                            variable.name = "sex_age_race", value.name = "pop")
ct_tracts <- ct_tracts[!grepl("U70|U76|U8D", sex_age_race)]  # drop some other race & Hispanic any race (not contained in PEP estimates)
ct_tracts[, hisp := ifelse(grepl("U78|U79|U8A|U8B|U8C|U8E", sex_age_race), 1, 0)]  # set Hispanic indicator
stopifnot(nrow(ct_tracts[hisp == 1]) == nrow(ct_tracts[hisp == 0]))  # should be same number of Hisp/non Hisp rows
ct_tracts[, race := case_when(grepl("U71|U78", sex_age_race) ~ "White alone",
                              grepl("U72|U79", sex_age_race) ~ "Black alone",
                              grepl("U73|U8A", sex_age_race) ~ "AIAN alone",
                              grepl("U74|U8B", sex_age_race) ~ "Asian alone",
                              grepl("U75|U8C", sex_age_race) ~ "NHOPI alone",
                              grepl("U77|U8E", sex_age_race) ~ "Two or More races",
                              TRUE ~ NA_character_)]
stopifnot(nrow(ct_tracts[is.na(race)]) == 0)  # all rows should have a race group
ct_tracts[, race := factor(race, levels = c("White alone", "Black alone", "AIAN alone", "Asian alone", "NHOPI alone", "Two or More races"))]  # match PEP format
ct_tracts[, sex := ifelse(grepl(paste(paste0("0", 27:49), collapse = "|"), sex_age_race), 2, 1)]
stopifnot(nrow(ct_tracts[sex == 1]) == nrow(ct_tracts[sex == 2]))  # should be same number of male/female rows
ct_tracts[, age := case_when(grepl("003|027", sex_age_race) ~ 0, grepl("004|028", sex_age_race) ~ 5,
                             grepl("005|029", sex_age_race) ~ 10, grepl("006|007|030|031", sex_age_race) ~ 15,
                             grepl("008|009|010|032|033|034", sex_age_race) ~ 20,
                             grepl("011|035", sex_age_race) ~ 25, grepl("012|036", sex_age_race) ~ 30,
                             grepl("013|037", sex_age_race) ~ 35, grepl("014|038", sex_age_race) ~ 40,
                             grepl("015|039", sex_age_race) ~ 45, grepl("016|040", sex_age_race) ~ 50,
                             grepl("017|041", sex_age_race) ~ 55, grepl("018|019|042|043", sex_age_race) ~ 60,
                             grepl("020|021|044|045", sex_age_race) ~ 65, grepl("022|046", sex_age_race) ~ 70,
                             grepl("023|047", sex_age_race) ~ 75, grepl("024|048", sex_age_race) ~ 80,
                             grepl("025|049", sex_age_race) ~ 85, TRUE ~ NA_real_)]
stopifnot(nrow(ct_tracts[is.na(age)]) == 0)  # all ages should have a value

# map CT census tracts to planning region FIPS, then merge onto tract dataset
ct_tract_to_pr <- as.data.table(get_acs(geography = "tract", variables = c("B01001_001"),
                                        year = 2022, state = "CT", survey = "acs5"))
ct_tract_to_pr[, c("tract_fips", "pr_fips") := list(as.numeric(substr(GEOID, 6, 11)), as.numeric(substr(GEOID, 1, 5)))]
tract_pr_map <- unique(ct_tract_to_pr[, list(tract_fips, pr_fips)]); rm(ct_tract_to_pr)
ct_tracts[tract_pr_map, on = "tract_fips", pr_fips := i.pr_fips]
ct_tracts <- ct_tracts[!tract_fips %in% c(990000, 990100)]  # remove water tracts since they have 0 population and cause issues with proportion calculations

# aggregate tracts to county-planning region pairs and calculate splitting proportions
ct_props <- copy(ct_tracts[, list(pop = sum(pop)), by = c("county_fips", "pr_fips", "sex", "age", "race", "hisp")])
ct_props[, prop := pop/sum(pop), by = "pr_fips,sex,age,race,hisp"]
ct_props[, pop2 := sum(pop), by = "county_fips,pr_fips,sex,race,hisp"]  # agg over age when total pop in a stratum is 0
ct_props[, prop2 := pop2/sum(pop), by = 'pr_fips,sex,race,hisp']
ct_props[, pop3 := sum(pop), by = "county_fips,pr_fips,race,hisp"]  # then agg over age and sex
ct_props[, prop3 := pop3/sum(pop), by = 'pr_fips,race,hisp']
stopifnot(nrow(ct_props[is.na(prop3)]) == 0)  # check that proportions exist everywhere
ct_props[, paste0("pop", 2:3) := NULL]

# merge with CT PEP estimates and calculate pops for county FIPS
setnames(ct_props, "age", "age_start")
setnames(to_split, "fips", "pr_fips")
ct_pop <- merge.data.table(to_split, ct_props[, -"pop"], by = c("pr_fips", "sex", "age_start", "race", "hisp"),
                           all.x = T, allow.cartesian = T)
start_pop <- sum(to_split$pop)  # get starting pop to check that totals align after splitting
ct_pop[, pop := ifelse(!is.na(prop), pop * prop,
                                   ifelse(!is.na(prop2), pop * prop2,
                                          ifelse(!is.na(prop3), pop * prop3, NA)))]
stopifnot(nrow(ct_pop[pop < 0 | is.na(pop)]) == 0)  # make sure new pop is within reasonable bounds and nonmissing

# aggregate to county FIPS
setnames(ct_pop, "county_fips", "fips")
ct_pop <- ct_pop[, list(pop = sum(pop)), by = "year,fips,sex,age_start,race,hisp"]
stopifnot(nrow(ct_pop[is.na(pop)]) == 0)  # make sure all strata have population values
stopifnot(sum(ct_pop$pop) == start_pop)  # check that pop totals did not change

# save copy of FIPS-level CT estimates for race-bridging in prepped code
saveRDS(ct_pop, paste0(out_dir, "ct_pop_by_county_v2023.rds"))
ct_pop <- ct_pop[year >= 2021]  # drop 2020 since we are using the post-2010

# populate age_ends for ct
ct_pop[, age_end := ifelse(age_start == 85, NA, age_start + 4)]

# Replace CT in PEP estimates with county-level estimates in 2020+ --------------------------------
nrow_start <- nrow(data)
data <- data[!(year >= 2021 & fips %in% 9000:9999)]  # drop planning region-level rows in 2021+
data <- rbindlist(list(data, ct_pop), use.names = T, fill = T)

# difference in total rows should be:
# 1 per data year after 2020 (9 planning regions -> 8 counties) * unique combination of sex/age/race/hisp
expected_diff <- (max(data$year)-2020)*nrow(unique(data[year >= 2021, list(sex, age_start, race, hisp)]))
stopifnot(nrow_start - nrow(data) == expected_diff)

# vetting plots to check split estimates in 2023 pop vintage vs raw estimates from 2021 vintage
v21 <- fread("FILEPATH")
v21 <- v21[STATE == 9]
v21[, type := "postcensal20"]
v21 <- melt(v21, id.vars = c("SUMLEV", "STATE", "COUNTY", "STNAME", "CTYNAME", "YEAR", "AGEGRP", "type"),
             value.name = "pop")
v21[, fips := as.integer(STATE * 1000 + COUNTY)]
v21 <- v21[grepl("^(H|NH)[[:alpha:]]", variable), ] # drop combined races and combined ethnicity totals
v21 <- v21[!grepl("AC_", variable), ] # drop "alone or in combination" counts (which overlap with each other)
v21[grepl("^(H|NH)WA_", variable), race := "White alone"]
v21[grepl("^(H|NH)BA_", variable), race := "Black alone"]
v21[grepl("^(H|NH)IA_", variable), race := "AIAN alone"]
v21[grepl("^(H|NH)AA_", variable), race := "Asian alone"]
v21[grepl("^(H|NH)NA_", variable), race := "NHOPI alone"]
v21[grepl("^(H|NH)TOM_", variable), race := "Two or More races"]
v21[, race := factor(race, levels = c("White alone", "Black alone", "AIAN alone", "Asian alone", "NHOPI alone", "Two or More races"))]
v21[grepl("^H", variable), hisp := 1L]
v21[grepl("^NH", variable), hisp := 0]
v21 <- v21[!(AGEGRP == 0), ]
v21[, age_start := as.integer(seq(0, 85, 5))[AGEGRP]]
v21[, age_end := as.integer(c(seq(4, 84, 5), NA))[AGEGRP]]
v21[YEAR %in% 2:3, year := YEAR + 2018]  # 2-3 = 2020-2021
v21[, year := as.integer(year)]
v21 <- v21[year >= 2021]
v21 <- v21[!is.na(year), ] # drop census counts and bases (all in April; keeping July estimates)
v21[grepl("_MALE", variable), sex := 1L]
v21[grepl("_FEMALE", variable), sex := 2L]
v21 <- v21[, list(fips, year, sex, age_start, age_end, race, hisp, pop)]
v21[, pop_v21 := as.numeric(pop)]  # pop is type 'character', set as numeric
v21[, pop := NULL]
setnames(ct_pop, "pop", "pop_v23")
plot_data <- merge.data.table(ct_pop[year == 2021], v21,
                              by = c("year", "fips", "sex", "age_start", "race", "hisp"), all = T)
setnames(plot_data, "age_start", "age")
plot_data[, "age_end" := NULL]

pdf("FILEPATH")
print(
  ggplot(plot_data[, list(pop_v21 = sum(pop_v21)/1000, pop_v23 = sum(pop_v23)/1000), by = "year,fips"],
         aes(x = pop_v21, y = pop_v23)) +
    facet_wrap(~fips) +
    geom_point(size = 2) +
    geom_abline(slope = 1, alpha = 0.5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ggtitle("Total population by county FIPS in Connecticut") +
    xlab("Vintage 2021 raw (1000s)") + ylab("Vintage 2023 approximated (1000s)"))

print(
  ggplot(plot_data[, list(pop_v21 = sum(pop_v21)/1000, pop_v23 = sum(pop_v23)/1000), by = "year,fips,sex"],
         aes(x = pop_v21, y = pop_v23, color = as.factor(sex))) +
    facet_wrap(~fips) +
    geom_point(size = 2) +
    geom_abline(slope = 1, alpha = 0.5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    scale_color_manual(values = c("1" = "#756bb1", "2" = "#e6550d")) +
    ggtitle("Total population by county FIPS and sex in Connecticut") +
    xlab("Vintage 2021 raw (1000s)") + ylab("Vintage 2023 approximated (1000s)"))

for (rc in unique(plot_data$race)) {
  print(
    ggplot(plot_data[race == rc & hisp == 0, list(pop_v21 = sum(pop_v21)/1000, pop_v23 = sum(pop_v23)/1000), by = "year,fips"],
           aes(x = pop_v21, y = pop_v23)) +
      facet_wrap(~fips) +
      geom_point(size = 2) +
      geom_abline(slope = 1, alpha = 0.5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      ggtitle(paste0("Total non-Hispanic ", rc, " population by county in Connecticut")) +
      xlab("Vintage 2021 raw (1000s)") + ylab("Vintage 2023 approximated (1000s)"))
}
print(
  ggplot(plot_data[hisp == 1, list(pop_v21 = sum(pop_v21)/1000, pop_v23 = sum(pop_v23)/1000), by = "year,fips"],
         aes(x = pop_v21, y = pop_v23)) +
    facet_wrap(~fips) +
    geom_point(size = 2) +
    geom_abline(slope = 1, alpha = 0.5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ggtitle("Total Hispanic population by county in Connecticut") +
    xlab("Vintage 2021 raw (1000s)") + ylab("Vintage 2023 approximated (1000s)"))

for (a in unique(plot_data$age)) {
  print(
    ggplot(plot_data[age == a, list(pop_v21 = sum(pop_v21)/1000, pop_v23 = sum(pop_v23)/1000), by = "year,fips"],
           aes(x = pop_v21, y = pop_v23)) +
      facet_wrap(~fips) +
      geom_point(size = 2) +
      geom_abline(slope = 1, alpha = 0.5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      ggtitle(paste0("Total population in age group ", a, " by county in Connecticut")) +
      xlab("Vintage 2021 raw (1000s)") + ylab("Vintage 2023 approximated (1000s)"))
}

dev.off()

# quick peek plot of pop by race over time for sanity check
ggplot(data[, list(pop = sum(pop)/1000), by = "year,race"], aes(x = year, y = pop, color = as.factor(race))) +
     geom_line() + theme_minimal()

# Save data ---------------------------------------------------------------------------------------
data <- merge(data, nids, by = "year")  # add nids
stopifnot(nrow(data[is.na(fips)]) == 0)  # make sure there are no missing FIPS codes
date_time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")  # name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- list("438685" = lapply(1:length(f), function(i) {f[i]}),
                      "512373" = list(f_pc10),
                      "560706" = list(f_pc20))
# save output
saveRDS(data, file = paste0(out_dir, date_time_stamp, ".rds"))