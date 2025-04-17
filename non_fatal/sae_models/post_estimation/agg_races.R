####################################################################################################
## Description: Calculate draws for all races combined by
##              aggregating the race-specific draws using population-weighted averages.
##
## Passed args: dir [character] -- home directory for settings and final output
##              year [integer] -- year to calculate all races combined draws for
##              sex [integer] -- sex to calculate all races combined draws for
##              raked [logical] -- should raked (T) or unraked (F) draws be aggregated?
##
## Requires:    populations (pop_file)
##              draws by race:
##                "[dir]/draws_[level]_[year]_[sex]_[race].rds"
##                (or the corresponding raked files if raked == T)
##
## Outputs:     draws and estimates for all races combined:
##                "[dir]/draws_[level]_[year]_[sex]_1.rds"
##                "[dir]/est_[level]_[year]_[sex]_1.rds"
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
  (sex <- commandArgs(TRUE)[[4]])
  (year <- commandArgs(TRUE)[[5]])
  (output_dir_draws_est <- commandArgs(TRUE)[[6]])
  (var_name <- commandArgs(TRUE)[[7]]) 
  (imp <- commandArgs(TRUE)[[8]])
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
  pop[, age:=ifelse(age==max(popage), max(ages), as.character(age))]
  pop <- pop[, list(pop = sum(pop)), by = c(area_var, "year", "sex", "age", "race", "state")]
  pop[, age := as.integer(as.character(age))]
}

setnames(pop, area_var, "area")
pop <- pop[year == get("year", .GlobalEnv), ]

# collapse populations to the required dimensions to remove any unneeded stratifiers
pop <- pop[, list(pop = sum(pop)), keyby = "area,year,sex,race,age"]

# reset tiny populations to 0 to avoid annoying edge cases where one group gets all of the weight
# even though it has effectively no population
pop[pop < 1e-5, pop := 0]

# calculate aggregation weights for each area-year-sex-age
pop[, wt := pop / sum(pop), by = "area,year,sex,age"]

# this fails when the total population in a given area-year-sex-age is 0.
# In these cases, we use the following hierarchy of backups: area-year-age (no sex),
# area-year (no sex or age).
pop[, pop2 := sum(pop), by = "area,year,age,race"]
pop[, wt2 := pop2 / sum(pop2), by = "area,year,sex,age"]

pop[, pop3 := sum(pop), by = "area,year,race"]
pop[, wt3 := pop3 / sum(pop3), by = "area,year,sex,age"]

pop[is.nan(wt), wt := wt2]
pop[is.nan(wt), wt := wt3]

# subset to the specified sex, and check that weights sum to 1
wts <- pop[sex == get("sex", .GlobalEnv), list(area, year, sex, race, age, pop, wt)]
stopifnot(wts[, sum(wt), by = "area,year,sex,age"][, max(abs(V1 - 1))] < 1e-10)

# load the age standard
std_wt <- readRDS(age_std_file)[, list(age, wt = wt / sum(wt))]
wtage <- unique(as.character(std_wt$age))
if (max(wtage)!=max(as.character(ages))&
    max(head(wtage,-1))==max(as.character(ages))){
  std_wt[, age:=ifelse(age==max(wtage), max(ages), as.character(age))]
  std_wt <- std_wt[, list(wt = sum(wt)), by = "age"]
  std_wt[, age := as.integer(as.character(age))]
}

## Aggregate draws to all races combined and collapse draws to estimates ----------------
# define function to aggregate given variable to all races combined
agg_race <- function(draws_file, wts, std_wt, draws_out_dir, est_out_dir, var_name = "pred") {
  
  # load draws
  draws <- rbindlist(lapply(races, function(race) {
    readRDS(paste0(draws_out_dir, gsub("RACE", race, draws_file)))
  }))
  
  # drop all ages, we recalculate these after aggregating by age
  draws <- draws[age < 98, ]
  
  # aggregate value draws to all races combined
  draws <- merge(draws, wts, by = c("area", "year", "sex", "race", "age"))
  
  if (by_source) {
    merge_vars <- c("level", "area", "year", "sex", "age", "source", "sim")
  } else {
    merge_vars <- c("level", "area", "year", "sex", "age", "sim")
  }
  
  if ("adjusted" %in% names(draws)) {
    merge_vars <- c(merge_vars, "adjusted")
  }
  
  setnames(draws, var_name, "pred")
  draws <- draws[, list(pred = sum(pred * wt), pop = sum(pop)), by = merge_vars]
  draws[, race := 1]
  
  # add crude and age-standardized rates
  draws <- calc_all_ages(draws, std_wt, "pred", c(merge_vars[!(merge_vars == "age")], "race"))
  setnames(draws, "pred", var_name)
  
  # collapse to get estimates
  est <- collapse_draws(draws, var_name, c(merge_vars[!(merge_vars == "sim")], "race"))
  
  # save draws and estimates
  out_draws_file <- paste0(draws_out_dir, gsub("RACE", 1, draws_file))
  saveRDS(draws, file = out_draws_file)
  
  out_est_file <- paste0(est_out_dir, gsub("draws", "est", gsub("RACE", 1, draws_file)))
  saveRDS(est, file = out_est_file)
}

# aggregate draws
agg_race(paste0("draws_", area_var, "_", year, "_", sex, "_RACE_", imp, "_", edu, ".rds"), wts, std_wt, draws_out_dir = paste0(output_dir_draws_est, "/draws/"), est_out_dir = paste0(output_dir_draws_est, "/est/"))
