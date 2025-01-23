####################################################################################################
## Author:      INDIVIDUAL_NAME
##
## Description: Calculate mx, yll, yld and (optionally) life table draws for all education levels combined by
##              aggregating the education-specific draws using population-weighted averages.
##
## Passed args: dir [character] -- home directory for settings and final output
##              year [integer] -- year to calculate all education levels combined draws for
##              sex [integer] -- sex to calculate all education levels combined draws for
##              race [integer] -- race to combine education levels for
##              raked [logical] -- should raked (T) or unraked (F) draws be aggregated?
##              lt [logical] -- should life table draws be aggregated?
##
## Requires:    populations (pop_file)
##              mx, yll, yld and lt draws [if lt == T] by race:
##                "[dir]/mx_draws_[level]_[year]_[sex]_[race].rds"
##                "[dir]/yll_draws_[level]_[year]_[sex]_[race].rds"
##                "[dir]/yld_draws_[level]_[year]_[sex]_[race].rds"
##                "[dir]/lt_draws_[level]_[year]_[sex]_[race].rds"
##                (or the corresponding raked files if raked == T)
##
## Outputs:     mx, yll, yld, and lt [if lt == T] draws and estimates for all races combined:
##                "[dir]/mx_draws_[level]_[year]_[sex]_[race_default].rds"
##                "[dir]/mx_est_[level]_[year]_[sex]_[race_default].rds"
##                "[dir]/yll_draws_[level]_[year]_[sex]_[race_default].rds"
##                "[dir]/yll_est_[level]_[year]_[sex]_[race_default].rds"
##                "[dir]/yld_draws_[level]_[year]_[sex]_[race_default].rds"
##                "[dir]/yld_est_[level]_[year]_[sex]_[race_default].rds"
##                "[dir]/lt_draws_[level]_[year]_[sex]_[race_default].rds"
##                "[dir]/lt_est_[level]_[year]_[sex]_[race_default].rds"
##                (or the corresponding raked files if raked == T)
##
####################################################################################################

# FILEPATH/projects/LSAE/repos/lbd.loader/browse
library(lbd.loader, lib.loc = sprintf("'FILEPATH'", R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
suppressMessages(lbd.loader::load.containing.package())

## Get settings ------------------------------------------------------------------------------------
if (interactive()) {
  dir = "FILEPATH"
  year = 2009
  sex = 2
  race = race_default
  raked = TRUE
  run_lt = TRUE
  run_mx = TRUE
  run_yll = FALSE
  run_yld = FALSE
} else {
  parser <- argparse::ArgumentParser()
  add_dir_argument(parser)
  add_year_argument(parser)
  add_sex_argument(parser)
  add_raked_argument(parser)
  add_lifetable_argument(parser)
  add_aggregation_skip_flags(parser)

  args <- parser$parse_args(get_args())

  dir <- args$dir
  year <- args$year
  sex <- args$sex
  race <- race_default
  raked <- as.logical(args$raked)
  run_lt <- as.logical(args$run_lt)
  run_mx <- !args$skip_mx_aggregation
  run_yll <- !args$skip_yll_aggregation
  run_yld <- !args$skip_yld_aggregation
}

for (setting in c("dir", "year", "sex", "raked", "run_lt", "run_mx", "run_yll", "run_yld")) {
  print(paste0(setting, ": ", get(setting)))
}

# If dealing with unraked (modeled) data, turn this option off to keep both adjusted == 0 and adjusted == 1 in the data.
if (!raked) setOption('ushd.use_adjusted_data', NA)

acause = basename(dir)
settings <- ModelSettings$from_yaml(dirname(dir))$settings_for_cause(acause)


save_settings <- copy(settings$settings)
# Add race_default to list of races for the schema checks to pass when saving
if (race == race_default) save_settings$races <- unique(c(race_default, save_settings$races))
# This new object is needed because ModelSettings is hard-wired to save at its initial geographic level
saver <- ModelSettings$new(settings$dir, save_settings, submitted_causes = settings$submitted_causes)

## Construct population weights --------------------------------------------------------------------
# load the population file, and subset to the specified year
pop <- settings$load_population(year, settings$raking_area_var)
setnames(pop, settings$area_var, "area")

# collapse populations to the required dimensions to remove any unneeded stratifiers
if (!settings$by_race) pop[, race := race_default]
if (!settings$by_edu) pop[, edu := edu_default]
pop <- pop[, list(pop = sum(pop)), keyby = "area,year,sex,race,edu,age"]

# reset tiny populations to 0 to avoid annoying edge cases where one group gets all of the weight
# even though it has effectively no population
pop[pop < 1e-5, pop := 0]

# calculate aggregation weights for each area-year-sex-age
pop[, wt := pop / sum(pop), by = "area,year,sex,age"]

# this fails when the total population in a given area-year-sex-age is 0.
# In these cases, we use the following hierarchy of backups: area-year-age (no sex),
# area-year (no sex or age).
pop[, pop2 := sum(pop), by = "area,year,age,edu,race"]
pop[, wt2 := pop2 / sum(pop2), by = "area,year,sex,age"]

pop[, pop3 := sum(pop), by = "area,year,edu,race"]
pop[, wt3 := pop3 / sum(pop3), by = "area,year,sex,age"]

pop[is.nan(wt), wt := wt2]
pop[is.nan(wt), wt := wt3]

# subset to the specified sex, and check that weights sum to 1
wts <- pop[sex == get("sex", .GlobalEnv), list(area, year, sex, race, edu, age, pop, wt)]
stopifnot(wts[, sum(wt), by = "area,year,sex,age"][, max(abs(V1 - 1))] < 1e-10)

# load the age standard
std_wt <- settings$load_std_wt()

## Aggregate mx and yll draws to all races combined and collapse draws to estimates ----------------
# define function to aggregate given variable (mx or yll) to all races combined
agg_edu <- function(acause, year, sex, race, wts, std_wt, var, raked) {
  # load draws
  # ModelSettings class defaults to race_levels = settings$races
  draws <- settings$load_draws(
    acause = acause,
    measure = var,
    year = year,
    sex = sex,
    raked = if (raked) "raked" else "unraked",
    edu_levels = settings$edu_groups, # want to load all education groups, since we're aggregating across edu
    race_levels = race
  )

  setnames(draws, settings$area_var, 'area')
  draws[, level := settings$area_var]
  draws <- draws[age < 98, ]

  # aggregate value draws to all races combined
  draws <- merge(draws, wts, by = c("area", "year", "sex", "race", "edu", "age"))

  merge_vars <- c("level", "area", "year", "sex", "age", "sim")
  if ("adjusted" %in% names(draws)) {
    merge_vars <- c(merge_vars, "adjusted")
  }

  draws <- draws[, list(value = sum(value * wt), pop = sum(pop)), by = merge_vars]

  draws[, edu := edu_default]
  setnames(draws, "value", var)

  # add crude and age-standardized rates
  draws <-
    calc_all_ages(draws,
                  std_wt,
                  var,
                  c(merge_vars[!(merge_vars == "age")], "edu"),
                  allow_missing_ages = settings$by_edu)
  # resort, reorder, and return
  by_vars <- c(merge_vars, "edu")
  setkeyv(draws, by_vars)
  setcolorder(draws, c(by_vars, var))

  est <- collapse_draws(draws, var, c(merge_vars[!(merge_vars == "sim")], "edu"))

  # save draws and estimates
  draws[, acause := acause]
  saver$io_engine$save_draws(
    draws = draws,
    acause = acause,
    save_dir = saver$dir,
    measure = var,
    year = year,
    sex = sex,
    type = if (raked) 'aggregation' else 'modeling',
    races = race,
    edu_groups = edu_default
  )

  est[, acause := acause]
  saver$io_engine$save_estimates(
    estimates  = est,
    acause = acause,
    save_dir = saver$dir,
    measure = var,
    year = year,
    sex = sex,
    races = race,
    edu_groups = edu_default,
    raked = if (raked) "raked" else "unraked",
    age = NULL
  )
}

# aggregate mx draws
if (run_mx) {
  agg_edu(
    acause = acause,
    year = year,
    sex = sex,
    race = race,
    wts = wts,
    std_wt = std_wt,
    var = "mx",
    raked = raked
  )
}

# aggregate yll draws
if (run_yll) {
  agg_edu(
    acause = acause,
    year = year,
    sex = sex,
    race = race,
    wts = wts,
    std_wt = std_wt,
    var = "yll",
    raked = raked
  )
}

# aggregate yld draws
if (run_yld) {
  agg_edu(
    acause = acause,
    year = year,
    sex = sex,
    race = race,
    wts = wts,
    std_wt = std_wt,
    var = "yld",
    raked = raked
  )
}

## Aggregate lt draws to all races combined --------------------------------------------------------
if (run_lt) {

  # load lt draws
  draws <- settings$load_draws(
    acause = acause,
    measure = 'lt',
    year = year,
    sex = sex,
    raked = if (raked) "raked" else "unraked",
    race_levels = race,
    edu_levels = settings$edu_groups
  )
  setnames(draws, settings$area_var, 'area')
  draws[, level := settings$area_var]

  # duplicate the weights for age 85+ to use for all specific age groups above 85
  wts[, pop := NULL]
  wts <- rbind(wts, wts[age == 85, list(age = seq(90, 110, 5)), by = "area,year,sex,race,edu,wt"])

  # merge lt draws with population weights
  draws <- merge(draws, wts, by = c("area", "year", "sex", "race", "edu", "age"))

  # aggregate the mx and ax columns to all edu combined
  merge_vars <- c("level", "area", "year", "sex", "age", "sim", "race")
  if ("adjusted" %in% names(draws)) {
    merge_vars <- c(merge_vars, "adjusted")
  }

  draws <- draws[, list(
    mx = sum(mx * wt),
    ax = sum(ax * (mx * wt) / sum(mx * wt))
  ),
  keyby = merge_vars]

  draws[, edu := edu_default]
  draws[, race := race_default]

  # for consistency, recalculate the life table using the aggregated mx and ax columns
  draws <- add_growth_rates(dir = dir, data = draws, idvars = c("year","sex","race", "edu", "area", "level"),
                            lt_hc = settings$lt_hc)

  draws <- draws[, lifetable(mx = mx, ax = ax, sex = sex[1], use_graduation = F, extrap = F,
                             lt_hc = settings$lt_hc, gr = mean_gr, adult_ages_only = settings$by_edu)[, list(age, mx, ax, qx, ex)],
                 by = c(merge_vars[!(merge_vars == "age")], "edu")]

  # get rid of growth rate
  if ("mean_gr" %in% names(draws)) {
    draws[, mean_gr := NULL]
  }

  # save lt draws for all education levels combined
  save_vars <- c("level", "area", "year", "sex", "race", "edu", "age", "sim")
  if ("adjusted" %in% names(draws)) {
    save_vars <- c(save_vars, "adjusted")
  }

  setkeyv(draws, save_vars)
  setcolorder(draws, c(save_vars, "mx", "ax", "qx", "ex"))

  draws[, acause := "_all"]
  saver$io_engine$save_draws(
    draws = draws,
    acause = "_all",
    save_dir = saver$dir,
    measure = "lt",
    year = year,
    sex = sex,
    type = if (raked) 'aggregation' else 'modeling',
    races = race,
    edu_groups = edu_default
  )

  # take out NA draws; based on ax because qx will always be 1 in the terminal age group
  draws <- draws[!is.na(ax)]

  # save lt estimates for all education levels combined
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
    edu_groups = edu_default,
    raked = if (raked) "raked" else "unraked"
  )

}
