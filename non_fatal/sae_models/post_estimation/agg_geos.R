####################################################################################################
## Description: Calculate for aggregated geographies
##              based on a population-based cross-walk file from "area_var" to "level".
##
## Passed args: dir [character] -- home directory for settings and final output
##              level [character] -- the geographic level to aggregate draws to
##              year [integer] -- year to calculate aggregated draws for
##              sex [integer] -- sex to calculate aggregated draws for
##              race [integer] -- race to calculate aggregated draws for
##              raked [logical] -- should raked (T) or unraked (F) draws be aggregated?
##
## Requires:    crosswalks from "area_var" to "level" (geoagg_files[level])
##              draws for "area_var":
##                "[dir]/draws_[area_var]_[year]_[sex]_[race].rds"
##                (or the corresponding raked files if raked == T)
##
## Outputs:     draws and estimates for the aggregated geography:
##                "[dir]/draws_[level]_[year]_[sex]_[race].rds"
##                (or the corresponding raked files if raked == T)
##
####################################################################################################

###### Load required libraries
pacman::p_load(R.utils, data.table)

###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (settings_loc <- commandArgs(TRUE)[[3]])
  (level <- commandArgs(TRUE)[[4]])
  (sex <- commandArgs(TRUE)[[5]])
  (race <- commandArgs(TRUE)[[6]])
  (year <- commandArgs(TRUE)[[7]])
  (output_dir_draws_est <- commandArgs(TRUE)[[8]])
  (var_name <- commandArgs(TRUE)[[9]]) 
  (imp <- commandArgs(TRUE)[[10]])
} else {
  level <- "natl"
  sex <- 1
  year <- 2012
  race <- 1
}

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

###### Assign settings from settings file
get_settings(settings_loc)
if(!exists("by_source")) {
  by_source <- FALSE
}

if (interactive()) {
  (output_dir_draws_est <- paste0(draws_est_output_dir, run_date, "/"))
  draws_file <- paste0("draws_", area_var, "_", year, "_", sex, "_", race, "_", imp, "_", edu, ".rds")
}

## Construct population weights --------------------------------------------------------------------
# load the population file, and subset to the specified year

if (file.exists(paste0(output_dir_draws_est, "/population.rds"))) {
  pop <- readRDS(paste0(output_dir_draws_est, "/population.rds"))
} else {
  pop <- readRDS(pop_file)
}

popage <- unique(as.character(pop$age))
if (max(popage)!=max(as.character(ages))&
    max(head(popage,-1))==max(as.character(ages))){
  pop[,age:=ifelse(age==max(popage), max(ages),as.character(age))]
  pop <- pop[, list(pop = sum(pop)), by = c(area_var, "year", "sex", "age", "race", "state")]
  pop$age <- as.integer(as.character(pop$age))
}

setnames(pop, area_var, "area")
pop <- pop[year == get("year", .GlobalEnv), ]

# collapse populations to the required dimensions to remove any unneeded stratifiers
if (!by_race) pop[, race := 1]

pop <- pop[, list(pop = sum(pop)), keyby = "area,year,sex,race,age"]


# load the crosswalk weights for the specified geographic level, merge with the population file,
# and calculate the population for each overlapping original area and crosswalk area


wts <- readRDS(geoagg_files[level])

if (level == "cbsa_mcnty") {
  setnames(wts, "cbsa_mcnty_code", "cbsa_mcnty", skip_absent = TRUE)
  metro <- wts[is_metropolitan_division == 1]
  for (i in 1:nrow(metro)) {
    wts <- wts[!(mcnty == metro[i, mcnty] & year == metro[i, year] & version == metro[i, version] & is_metropolitan_division == 0)]
  }
  wts <- unique(wts[, c("state", "mcnty", "cbsa_mcnty", "year")])
}

setnames(wts, c(area_var, level), c("area", "area2"))
id_vars <- intersect(c("area", "year", "sex", "race", "age"), names(wts))
id_vars2 <- intersect(c("area", "area2", "year", "sex", "race", "age", "wt"), names(wts))

wts <- unique(wts[, ..id_vars2])

# many NAs if not subsetted by year
if ("year" %in% id_vars) {
  wts <- wts[year == get("year", .GlobalEnv),]
}

if (is.null(wts$wt)) {
  wts$wt <- 1
}

if (level == "cbsa_mcnty") {
  pop <- merge(pop, wts, by = id_vars, all = TRUE, allow.cartesian = TRUE)
} else {
  pop <- merge(pop, wts, by = id_vars, all = TRUE)
}

pop[, pop := pop * wt]
pop[, wt := NULL]

# if needed, aggregate over race
if (race == 1) {
  pop[, race := 1]
  pop <- pop[, list(pop = sum(pop)), keyby = "area2,area,year,sex,race,age"]
}


# reset tiny populations to 0 to avoid annoying edge cases where one group gets all of the weight
# even though it has effectively no population
pop[pop < 1e-5, pop := 0]

# calculate aggregation weights by area2-year-sex-race-age
pop[, wt := pop / sum(pop), by = "area2,year,sex,race,age"]

# this fails when the total population in a given area2-year-sex-race-age is 0.
# In these cases, we use the following hierarchy of backups: area2-year-race-age (no sex),
# area2-year-race (no sex or age), and area2-year (no sex, age, or race)
pop[, pop2 := sum(pop), by = "area,area2,year,race,age"]
pop[, wt2 := pop2 / sum(pop2), by = "area2,year,sex,race,age"]

pop[, pop3 := sum(pop), by = "area,area2,year,race"]
pop[, wt3 := pop3 / sum(pop3), by = "area2,year,sex,race,age"]

pop[, pop4 := sum(pop), by = "area,area2,year"]
pop[, wt4 := pop4 / sum(pop4), by = "area2,year,sex,race,age"]

pop[is.nan(wt), wt := wt2]
pop[is.nan(wt), wt := wt3]
pop[is.nan(wt), wt := wt4]

# subset to the specified race and sex, and check that weights sum to 1
wts <- pop[sex == get("sex", .GlobalEnv) & race == get("race", .GlobalEnv),
         list(area, area2, year, sex, race, age, pop, wt)]

stopifnot(wts[, sum(wt), by = "area2,year,sex,race,age"][, max(abs(V1 - 1))] < 1e-10)

# load the age standard
std_wt <- readRDS(age_std_file)[, list(age, wt = wt / sum(wt))]

## Aggregate draws to "level" and collapse draws to est----------------------------------
# define function to aggregate given variable to a higher geographic level
agg_geo <- function(draws_file, wts, std_wt, draws_out_dir, est_out_dir, var_name = "pred") {
  
  # load draws
  draws <- readRDS(paste0(draws_out_dir, draws_file))
  
  # drop all ages, we recalculate these after aggregating by age
  draws <- draws[age < 98,]
  
  if (level == "state") {
    ### Load area recode file
    # doesn't exist for YLL:YLD models
    if (exists(paste0(output_dir, "/recode.rds"))) {
      area_mapping <- readRDS(paste0(output_dir, "/recode.rds"))$area
      
      ### Merge state names
      merged_counties <- fread("FILEPATH")
      area_mapping <- unique(merge(area_mapping, merged_counties, by = "mcnty")[, list("area" = mcnty, state_name)])
      
      ### Merge area_recode mapping
      draws <- merge(draws, area_mapping, by.x = "area", by.y = "area", all.x = TRUE)
    } else {
      ## Load mcnty to state mapping
      merged_counties <- fread("FILEPATH")
      merged_counties <-  unique(merged_counties[mcnty %in% unique(draws$area), list("area" = mcnty, state_name)])
      
      ## Merge with draws
      draws <- merge(draws, merged_counties, by = "area", all.x = TRUE)
    }
  }
  
  # aggregate value draws to "level"
  draws[, level := get("level", .GlobalEnv)]
  draws <- merge(draws, wts, by = c("area", "year", "sex", "race", "age"), allow.cartesian = TRUE)
  
  if (by_source) {
    merge_vars <- c("level", "area2", "year", "sex", "race", "age", "source", "sim")
  } else {
    merge_vars <- c("level", "area2", "year", "sex", "race", "age", "sim")
  }
  
  if ("adjusted" %in% names(draws)) {
    merge_vars <- c(merge_vars, "adjusted")
  }
  setnames(draws, var_name, "pred")
  draws <- draws[!is.na(pred), list(pred = sum(pred * wt), pop = sum(pop)), by = merge_vars]
  setnames(draws, c("area2"), c("area"))
  
  merge_vars <- c(merge_vars[merge_vars != "area2"], "area")
  
  # add crude and age-standardized rates
  draws <- calc_all_ages(draws, std_wt, "pred", merge_vars[!(merge_vars == "age")])
  setnames(draws, "pred", var_name)
  
  # collapse to get estimates
  est <- collapse_draws(draws, var_name, merge_vars[!(merge_vars == "sim")])
  
  # save draws and estimates
  out_draws_file <- paste0(draws_out_dir, gsub(area_var, level, draws_file))
  saveRDS(draws, file = out_draws_file)
  
  out_est_file <- paste0(est_out_dir, gsub("draws", "est", gsub(area_var, level, draws_file)))
  saveRDS(est, file = out_est_file)
}

# aggregate draws

agg_geo(paste0("draws_", area_var, "_", year, "_", sex, "_", race, "_", imp, "_", edu, ".rds"), wts, std_wt, draws_out_dir = paste0(output_dir_draws_est, "/draws/"), est_out_dir = paste0(output_dir_draws_est, "/est/"))
