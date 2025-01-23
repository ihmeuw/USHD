####################################################################################################
## Author:      INDIVIDUAL_NAME
##
## Description: Calculate mx, yll, yld, pred (bmi) and (optionally) life table draws for aggregated geographies
##              based on a population-based cross-walk file from "area_var" to "level".
##
## Passed args: dir [character] -- home directory for settings and final output
##              level [character] -- the geographic level to aggregate draws to
##              year [integer] -- year to calculate aggregated draws for
##              sex [integer] -- sex to calculate aggregated draws for
##              race [integer] -- race to calculate aggregated draws for
##              raked [logical] -- should raked (T) or unraked (F) draws be aggregated?
##              lt [logical] -- should life table draws be aggregated?
##
## Requires:    crosswalks from "area_var" to "level" (geoagg_files[level])
##              mx, yll, yld, pred, and lt draws [if lt == T] for "area_var":
##                "[dir]/mx_draws_[area_var]_[year]_[sex]_[race].rds"
##                "[dir]/yll_draws_[area_var]_[year]_[sex]_[race].rds"
##                "[dir]/yld_draws_[area_var]_[year]_[sex]_[race].rds"
##                "[dir]/draws_[area_var]_[year]_[sex]_[race].rds" (for pred)
##                "[dir]/lt_draws_[area_var]_[year]_[sex]_[race].rds"
##                (or the corresponding raked files if raked == T)
##
## Outputs:     mx, yll, yld, pred and lt [if lt == T] draws and estimates for the aggregated geography:
##                "[dir]/mx_draws_[level]_[year]_[sex]_[race].rds"
##                "[dir]/mx_est_[level]_[year]_[sex]_[race].rds"
##                "[dir]/yll_draws_[level]_[year]_[sex]_[race].rds"
##                "[dir]/yll_est_[level]_[year]_[sex]_[race].rds"
##                "[dir]/yld_draws_[level]_[year]_[sex]_[race].rds"
##                "[dir]/yld_est_[level]_[year]_[sex]_[race].rds"
##                "[dir]/draws_[level]_[year]_[sex]_[race]_[edu].rds" (for pred)
##                "[dir]/est_[level]_[year]_[sex]_[race]_[edu].rds" (for pred)
##                "[dir]/lt_draws_[level]_[year]_[sex]_[race].rds"
##                "[dir]/lt_est_[level]_[year]_[sex]_[race].rds"
##                (or the corresponding raked files if raked == T)
##
####################################################################################################

# FILEPATH/projects/LSAE/repos/lbd.loader/browse
library(lbd.loader, lib.loc = sprintf("'FILEPATH'",
                                      R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
suppressMessages(lbd.loader::load.containing.package())

## Get settings ------------------------------------------------------------------------------------
if (interactive()) {

  dir <- "FILEPATH"
  level <- "state"
  year <- 2009

  sex <- 2
  race <- 9
  edu <- 1
  raked <- TRUE
  run_lt <- FALSE
  run_mx <- FALSE
  run_yll <- FALSE
  run_yld <- FALSE
  run_pred <- TRUE

} else {
  parser <- argparse::ArgumentParser()
  add_dir_argument(parser)
  add_level_argument(parser)
  add_year_argument(parser)
  add_sex_argument(parser)
  add_race_argument(parser)
  add_edu_argument(parser)
  add_raked_argument(parser)
  add_lifetable_argument(parser)
  add_aggregation_skip_flags(parser)

  args = parser$parse_args(get_args())

  dir <- args$dir
  level <- args$level
  year <- args$year
  sex <- args$sex
  race <- args$race
  edu <- args$edu
  raked <- as.logical(args$raked)
  run_lt <- as.logical(args$run_lt)
  run_mx <- !args$skip_mx_aggregation
  run_yll <- !args$skip_yll_aggregation
  run_yld <- !args$skip_yld_aggregation
  run_pred <- !args$skip_pred_aggregation
}

for (setting in c('dir', 'level', 'year', 'sex', 'race', 'edu', 'raked', 'run_lt', 'run_mx', 'run_yll', 'run_yld',
                  'run_pred')){
  print(paste0(setting, ': ', get(setting)))
}

# If dealing with unraked (modeled) data, turn this option off to keep both adjusted == 0 and adjusted == 1 in the data.
if (!raked) setOption('ushd.use_adjusted_data', NA)

# BMI prevalence/exposure still uses old race codes, overwrite defaults
if (run_pred) {
  acause <- basename(dirname(dir))
  # overwrite_constants(new_race_default = 9)
  settings <- ModelSettings$from_yaml(dir)
  # race_default <- 9
  if(setequal(settings$races, c(1,2,3,4,7))){
    overwrite_constants(new_race_default = 9)
    # replace with OLD race and edu codes
    race_default <- 9
    rm(settings)
    settings <- ModelSettings$from_yaml(dir)
  } else{
    stopifnot(setequal(settings$races, c(2,4,5,6,7)))
  }
} else {
  acause = basename(dir)
  settings <- ModelSettings$from_yaml(dirname(dir))$settings_for_cause(acause)
}


save_settings <- copy(settings$settings)
# Add race_default (edu_default) to list of races (edu_groups) for the schema checks to pass when saving
if (race == race_default) save_settings$races <- unique(c(race_default, save_settings$races))
if (edu == edu_default) save_settings$edu_groups <- unique(c(edu_default, save_settings$edu_groups))
save_settings$area_var <- level
# This new object is needed because ModelSettings is hard-wired to save at its initial geographic level
saver <- ModelSettings$new(settings$dir, save_settings, submitted_causes = settings$submitted_causes)

## Construct population weights --------------------------------------------------------------------
# load the population file, and subset to the specified year
pop <- settings$load_population(year, level)
setnames(pop, settings$area_var, "area")

# collapse populations to the required dimensions to remove any unneeded stratifiers
if (!settings$by_race) pop[, race := race_default]
if (!settings$by_edu) pop[, edu := edu_default]
pop <- pop[, list(pop = sum(pop)), keyby = "area,year,sex,race,edu,age"]

# load the crosswalk weights for the specified geographic level, merge with the population file,
# and calculate the population for each overlapping original area and crosswalk area
wts <- settings$load_geo_wts(level)
setnames(wts, c(settings$area_var, level), c("area", "area2"))
id_vars <- intersect(c("area", "year", "sex", "race", "edu", "age"), names(wts))
pop <- merge(pop, wts, by = id_vars, all = T)
pop[, pop := pop * wt]
pop[, wt := NULL]

# if needed, aggregate over race and/or education
if (race == race_default) pop[, race := race_default]
if (edu == edu_default) pop[, edu := edu_default]

pop <- pop[, list(pop = sum(pop)), keyby = "area2,area,year,sex,race,edu,age"]


# reset tiny populations to 0 to avoid annoying eduge cases where one group gets all of the weight
# even though it has effectively no population
pop[pop < 1e-5, pop := 0]

# calculate aggregation weights by area2-year-sex-race-edu-age
pop[, wt := pop / sum(pop), by = "area2,year,sex,race,edu,age"]

# this fails when the total population in a given area2-year-sex-race-age is 0.
# In these cases, we use the following hierarchy of backups: area2-year-race-edu-age (no sex),
# area2-year-race-edu (no sex or age), and area2-year (no sex, age, or race)
pop[, pop2 := sum(pop), by = "area,area2,year,race,edu,age"]
pop[, wt2 := pop2 / sum(pop2), by = "area2,year,sex,race,edu,age"]

pop[, pop3 := sum(pop), by = "area,area2,year,race,edu"]
pop[, wt3 := pop3 / sum(pop3), by = "area2,year,sex,race,edu,age"]

pop[, pop4 := sum(pop), by = "area,area2,year"]
pop[, wt4 := pop4 / sum(pop4), by = "area2,year,sex,race,edu,age"]

pop[is.nan(wt), wt := wt2]
pop[is.nan(wt), wt := wt3]
pop[is.nan(wt), wt := wt4]

# subset to the specified race and sex, and check that weights sum to 1
wts <- pop[sex == get("sex", .GlobalEnv) & race == get("race", .GlobalEnv) & edu == get("edu", .GlobalEnv),
           list(area, area2, year, sex, race, edu, age, pop, wt)]
stopifnot(wts[, sum(wt), by = "area2,year,sex,race,edu,age"][, max(abs(V1 - 1))] < 1e-10)

# load the age standard
if (run_pred) {
  # If settings$measure is "pred" or "ab", settings$std_wt -> settings$load_std_wt() is calculated with subsetted ages.
  settings$measure <- "pred"
}
std_wt <- settings$load_std_wt()

## Aggregate mx and yll draws to "level" and collapse draws to est----------------------------------
# define function to aggregate given variable (mx or yll) to a higher geographic level
agg_geo <- function(acause, year, sex, race, edu, wts, std_wt, var, raked) {

  # load draws
  draws <- settings$load_draws(
    acause = acause,
    measure = var,
    year = year,
    sex = sex,
    raked = if (raked) "raked" else "unraked",
    race_levels = race,
    edu_levels = edu
  )

  # drop all ages, we recalculate these after aggregating by age
  draws <- draws[age < 98, ]

  # aggregate value draws to "level"
  draws[, level := get("level", .GlobalEnv)]
  if (!'area' %in% names(draws)){
    setnames(draws, settings$area_var, 'area')
  }
  draws <- merge(draws, wts, by = c("area", "year", "sex", "race", "edu", "age"), allow.cartesian = T)

  merge_vars <- c("level", "area2", "year", "sex", "race", "edu", "age", "sim")
  if("adjusted" %in% names(draws)) {
    merge_vars <- c(merge_vars,"adjusted")
  }

  draws <- draws[, list(value = sum(value * wt), pop = sum(pop)), by = merge_vars]
  setnames(draws, c("area2", "value"), c("area", var))

  merge_vars <- c(merge_vars[merge_vars != "area2"], "area")

  setnames(draws, var, "value")
  setnames(draws, "area", level)

  draws[, acause := acause]
  saver$save_draws_and_estimates(
    draws = draws,
    acause = acause,
    measure = var,
    rei_id = settings$settings$rei_id,
    year = year,
    sex = sex,
    race = race,
    edu = edu,
    type = if (raked) 'aggregation' else 'modeling'
  )
}

# aggregate mx draws
if (run_mx) {
  agg_geo(
    acause = acause,
    year = year,
    sex = sex,
    race = race,
    edu = edu,
    wts = wts,
    std_wt = std_wt,
    var = "mx",
    raked = raked
  )
}

# aggregate yll draws
if (run_yll) {
  agg_geo(
    acause = acause,
    year = year,
    sex = sex,
    race = race,
    edu = edu,
    wts = wts,
    std_wt = std_wt,
    var = "yll",
    raked = raked
  )
}

# aggregate yld draws
if (run_yld) {
  agg_geo(
    acause = acause,
    year = year,
    sex = sex,
    race = race,
    edu = edu,
    wts = wts,
    std_wt = std_wt,
    var = "yld",
    raked = raked
  )
}

# aggregate pred (bmi) draws
if (run_pred) {
  agg_geo(
    acause = acause,
    year = year,
    sex = sex,
    race = race,
    edu = edu,
    wts = wts,
    std_wt = std_wt,
    var = "pred",
    raked = raked
  )
}

## Aggregate lt draws to "level" -------------------------------------------------------------------
if (run_lt) {

  # load lt draws
  draws <- settings$load_draws(
    acause = acause,
    measure = 'lt',
    year = year,
    sex = sex,
    raked = if (raked) "raked" else "unraked",
    race_levels = race,
    edu_levels = edu
  )
  setnames(draws, settings$area_var, 'area')
  draws$level <- level

  if (any(is.na(draws$ax))) lsae.utils::stop_or_quit("NAs found in 'ax' from raw lifetable outputs - exiting.", status = 12)

  # duplicate the weights for age 85+ to use for all specific age groups above 85
  wts[, pop := NULL]
  wts <- rbind(wts, wts[age == 85, list(age = seq(90, 110, 5)), by = "area,area2,year,sex,race,edu,wt"])

  # merge lt draws with population weights
  draws <- merge(draws, wts, by = c("area", "year", "sex", "race", "edu", "age"), allow.cartesian = T)

  # aggregate the mx and ax columns to "level"
  merge_vars <- c("level","area2","year","sex","race","edu", "age","sim")
  if("adjusted" %in% names(draws)) {
    merge_vars <- c(merge_vars,"adjusted")
  }

  draws <- draws[, list(mx = sum(mx * wt),
                        ax = sum(ax * (mx * wt)/sum(mx * wt))),
                 keyby = merge_vars]
  setnames(draws, "area2", "area")
  merge_vars <- c(merge_vars[merge_vars != "area2"], "area")

  # for consistency, recalculate the life table using the aggregated mx and ax columns
  draws <- add_growth_rates(dir = dir, data = draws, idvars = c("year", "sex", "race", "edu", "area", "level"),
                            lt_hc = settings$lt_hc)

  draws <- draws[, lifetable(mx = mx, ax = ax, sex = sex[1], use_graduation = F, extrap = F,
                             lt_hc = settings$lt_hc, gr = mean_gr, adult_ages_only = settings$by_edu)[, list(age, mx, ax, qx, lx, dx, Lx, Tx, ex)],
                 by = c(merge_vars[!(merge_vars == "age")])]

  # get rid of growth rate
  if("mean_gr" %in% names(draws)) {
    draws[, mean_gr := NULL]
  }

  draws[, acause := "_all"]

  saver$io_engine$save_draws(
    draws = draws,
    acause = "_all",
    save_dir = saver$dir,
    measure = "FULL_lt",
    year = year,
    sex = sex,
    age = NULL,
    type = if (raked) 'aggregation' else 'modeling',
    race = race,
    edu = edu
  )

  draws <- draws[,c("lx","dx","Lx","Tx","acause") := NULL]

  # save "level" lt draws
  save_vars <- c("level","area","year","sex","race", "edu", "age","sim")
  if("adjusted" %in% names(draws)) {
    save_vars <- c(save_vars,"adjusted")
  }

  setkeyv(draws, save_vars)
  setcolorder(draws, c(save_vars, "mx", "ax", "qx", "ex"))

  draws[, acause := "_all"] # do this again to make sure it is in the correct order
  saver$io_engine$save_draws(
    draws = draws,
    acause = "_all",
    save_dir = saver$dir,
    measure = "lt",
    year = year,
    sex = sex,
    type = if (raked) 'aggregation' else 'modeling',
    races = race,
    edu_groups = edu
  )

  # take out NA draws; based on ax because qx will always be 1 in the terminal age group
  draws <- draws[!is.na(ax)]

  # save "level" lt estimates
  est <- collapse_draws_lt(draws = draws, dir = dir, id_vars = c(save_vars[!(save_vars == "sim")]), education = settings$by_edu)
  est[, acause := "all"]
  saver$io_engine$save_estimates(
    estimates = est,
    acause = "_all",
    save_dir = saver$dir,
    measure = "lt",
    year = year,
    sex = sex,
    age = NULL,
    races = race,
    edu_groups = edu,
    raked = if (raked) "raked" else "unraked"
  )
}
