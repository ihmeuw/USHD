####################################################################################################
## Description: Calculate draws for both sexes combined by
##              aggregating the draws for males and females using population-weighted averages.
##
## Passed args: dir [character] -- home directory for settings and final output
##              level [character] -- the geographic level to caculate both sexes combined draws for
##              year [integer] -- year to calculate both sexes combined draws for
##              race [integer] -- race to calculate both sexes combined draws for
##              raked [logical] -- should raked (T) or unraked (F) draws be aggregated?
##
## Requires:    populations (pop_file)
##              cross-walk files, if level != area_var (geoagg_files[level])
##              draws [if lt == T] by sex:
##                "[dir]/draws_[level]_[year]_[sex]_[race].rds"
##                (or the corresponding raked files if raked == T)
##
## Outputs:     draws and estimates for both sexes combined:
##                "[dir]/draws_[level]_[year]_3_[race].rds"
##                "[dir]/est_[level]_[year]_3_[race].rds"
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
  (race <- commandArgs(TRUE)[[5]])
  (year <- commandArgs(TRUE)[[6]])
  (output_dir_draws_est <- commandArgs(TRUE)[[7]])
  (var_name <- commandArgs(TRUE)[[8]])
  (imp <- commandArgs(TRUE)[[9]])
}

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

###### Assign settings from settings file
get_settings(settings_loc)

if (!exists("by_source")) {
  by_source <- FALSE
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

# if needed, aggregate over geography
if (level != area_var) {
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
    wts <- wts[year == get("year", .GlobalEnv), ]
    wts <- unique(wts[, c("area", "area2", "year")])
  } else {
    wts <- unique(wts[, c("area", "area2")])
  }
  
  if (is.null(wts$wt)) {
    wts$wt <- 1
  }
  
  if (level == "cbsa_mcnty") {
    pop <- merge(pop, wts, by = id_vars, all = TRUE, allow.cartesian = TRUE)
  } else {
    pop <- merge(pop, wts, by = id_vars, all = TRUE)
  }
  
  pop <- pop[, list(pop = sum(pop * wt)), by = "area2,year,sex,race,age"]
  setnames(pop, "area2", "area")
}

# if needed, aggregate over race
if (race == 1) {
  pop[, race := 1]
  pop <- pop[, list(pop = sum(pop)), keyby = "area,year,sex,race,age"]
} else if (exists("cause_id") & race == 1) {
  pop[, race := 1]
  pop <- pop[, list(pop = sum(pop)), keyby = "area,year,sex,race,age"]
}

# reset tiny populations to 0 to avoid annoying edge cases where one group gets all of the weight
# even though it has effectively no population
pop[pop < 1e-5, pop := 0]

# calculate aggregation weights for each area-year-race-age
pop[, wt := pop / sum(pop), by = "area,year,race,age"]

# this fails when the total population in a given area-year-race-age is 0.
# In these cases, we use the following hierarchy of backups: area-year-race (no age), area-year (no
# age or race)
pop[, pop2 := sum(pop), by = "area,year,sex,race"]
pop[, wt2 := pop2 / sum(pop2), by = "area,year,race,age"]

pop[, pop3 := sum(pop), by = "area,year,sex"]
pop[, wt3 := pop3 / sum(pop3), by = "area,year,race,age"]

pop[is.nan(wt), wt := wt2]
pop[is.nan(wt), wt := wt3]

# subset to the specified race, and check that weights sum to 1
wts <- pop[race == get("race", .GlobalEnv), list(area, year, sex, race, age, pop, wt)]
stopifnot(wts[, sum(wt), by = "area,year,race,age"][, max(abs(V1 - 1))] < 1e-10)

# load the age standard
std_wt <- readRDS(age_std_file)[, list(age, wt = wt / sum(wt))]

## Aggregate draws to both sexes combined and collapse draws to est----------------------
# define function to aggregate to both sexes combined
agg_sex <- function(draws_file, wts, std_wt, draws_out_dir, est_out_dir, var_name = "pred") {
  
  # load draws
  draws <- rbindlist(lapply(sexes, function(sex) {
    readRDS(paste0(draws_out_dir, gsub("SEX", sex, draws_file)))
  }))
  
  # drop all ages, we recalculate these after aggregating by age
  draws <- draws[!(age %in% c(98, 99)), ]  # drop all-ages mortality rates
  
  # if necessary, add in 0s for sexes we don't model (specific to certain causes, eg, maternal)
  if (length(sexes) == 1) {
    temp <- copy(draws)
    temp[, pred := 0]
    if (sexes == 1) temp[, sex := 2] else temp[, sex := 1]
    draws <- rbind(draws, temp)
    setkeyv(draws, c("level", "area", "year", "sex", "race", "age", "sim"))
    rm(temp); gc()
  }
  
  # aggregate value draws to both sexes combined
  draws <- merge(draws, wts, by = c("area", "year", "sex", "race", "age"))
  
  if (by_source) {
    merge_vars <- c("level","area","year","race","age", "source", "sim")
  } else {
    merge_vars <- c("level","area","year","race","age","sim")
  }
  
  if("adjusted" %in% names(draws)) {
    merge_vars <- c(merge_vars, "adjusted")
  }
  
  setnames(draws, var_name, "pred")
  draws <- draws[, list(pred = sum(pred * wt), pop = sum(pop)), by = merge_vars]
  draws[, sex := 3]
  
  # add crude and age-standardized rates
  draws <- calc_all_ages(draws, std_wt, "pred", by_vars = c(merge_vars[!(merge_vars == "age")], "sex"))
  setnames(draws, "pred", var_name)
  
  # collapse to get estimates
  est <- collapse_draws(draws, var_name, c(merge_vars[!(merge_vars == "sim")], "sex"))
  
  # save draws and estimates
  out_draws_file <- paste0(draws_out_dir, gsub("SEX", "3", draws_file))
  saveRDS(draws, file = out_draws_file)
  
  out_est_file <- paste0(est_out_dir, gsub("draws", "est", gsub("SEX", "3", draws_file)))
  saveRDS(est, file = out_est_file)
}

# aggregate draws
agg_sex(paste0("draws_", level, "_", year, "_SEX_", race, "_", imp, "_", edu, ".rds"), wts, std_wt, draws_out_dir = paste0(output_dir_draws_est, "/draws/"), est_out_dir = paste0(output_dir_draws_est, "/est/"))
