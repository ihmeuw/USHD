####################################################################################################
## Description: Calculate mx, yll, yld, pred (bmi) and (optionally) life table draws for all races combined by
##              aggregating the race-specific draws using population-weighted averages.
##
## Passed args: dir [character] -- home directory for settings and final output
##              year [integer] -- year to calculate all races combined draws for
##              sex [integer] -- sex to calculate all races combined draws for
##              raked [logical] -- should raked (T) or unraked (F) draws be aggregated?
##              lt [logical] -- should life table draws be aggregated?
##
## Requires:    populations (pop_file)
##              mx, yll, yld, pred and lt draws [if lt == T] by race:
##                "[dir]/mx_draws_[level]_[year]_[sex]_[race].rds"
##                "[dir]/yll_draws_[level]_[year]_[sex]_[race].rds"
##                "[dir]/yld_draws_[level]_[year]_[sex]_[race].rds"
##                "[dir]/draws_[level]_[year]_[sex]_[race].rds" (for pred)
##                "[dir]/lt_draws_[level]_[year]_[sex]_[race].rds"
##                (or the corresponding raked files if raked == T)
##
## Outputs:     mx, yll, yld, pred, and lt [if lt == T] draws and estimates for all races combined:
##                "[dir]/mx_draws_[level]_[year]_[sex]_[race_default].rds"
##                "[dir]/mx_est_[level]_[year]_[sex]_[race_default].rds"
##                "[dir]/yll_draws_[level]_[year]_[sex]_[race_default].rds"
##                "[dir]/yll_est_[level]_[year]_[sex]_[race_default].rds"
##                "[dir]/yld_draws_[level]_[year]_[sex]_[race_default].rds"
##                "[dir]/yld_est_[level]_[year]_[sex]_[race_default].rds"
##                "[dir]/draws_[level]_[year]_[sex]_[race_default]_[edu].rds" (for pred)
##                "[dir]/est_[level]_[year]_[sex]_[race_default]_[edu].rds" (for pred)
##                "[dir]/lt_draws_[level]_[year]_[sex]_[race_default].rds"
##                "[dir]/lt_est_[level]_[year]_[sex]_[race_default].rds"
##                (or the corresponding raked files if raked == T)
##
####################################################################################################

library(lbd.loader, 
        lib.loc = sprintf("FILEPATH", 
                          R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
suppressMessages(lbd.loader::load.containing.package())

## Get settings ------------------------------------------------------------------------------------
if (interactive()) {
  dir = "FILEPATH"
  year = 2006
  sex = 1
  edu = 1
  raked = FALSE
  run_lt = TRUE
  run_mx = FALSE
  run_yll = FALSE
  run_yld = FALSE
  run_pred = TRUE
} else {
  parser <- argparse::ArgumentParser()
  add_dir_argument(parser)
  add_year_argument(parser)
  add_sex_argument(parser)
  add_edu_argument(parser)
  add_raked_argument(parser)
  add_lifetable_argument(parser)
  add_aggregation_skip_flags(parser)

  args <- parser$parse_args(get_args())

  dir <- args$dir
  year <- args$year
  sex <- args$sex
  edu <- args$edu
  raked <- as.logical(args$raked)
  run_lt <- as.logical(args$run_lt)
  run_mx <- !args$skip_mx_aggregation
  run_yll <- !args$skip_yll_aggregation
  run_yld <- !args$skip_yld_aggregation
  run_pred <- !args$skip_pred_aggregation
}

for (setting in c("dir", "year", "sex", "edu", "raked", "run_lt", "run_mx", "run_yll", "run_yld", "run_pred")) {
  print(paste0(setting, ": ", get(setting)))
}

# If dealing with unraked (modeled) data, turn this option off to keep both adjusted == 0 and adjusted == 1 in the data.
if (!raked) setOption('ushd.use_adjusted_data', NA)

# some BMI prevalence/exposure models still use old race codes, overwrite defaults
if (run_pred) {
  acause <- basename(dirname(dir))
  settings <- ModelSettings$from_yaml(dir)
  if(setequal(settings$races, c(1,2,3,4,7))){
    overwrite_constants(new_race_default = 9)
    # replace with OLD race and edu codes
    race_default <- 9
  } else{
    stopifnot(setequal(settings$races, c(2,4,5,6,7)))
  }
} else {
  acause = basename(dir)
  settings <- ModelSettings$from_yaml(dirname(dir))$settings_for_cause(acause)
}

# create new object for saving
save_settings <- copy(settings$settings)
# Add race_default (edu_default) to list of races (edu_groups) for the schema checks to pass when saving
if (edu == edu_default) save_settings$edu_groups <- unique(c(edu_default, save_settings$edu_groups))
# This new object is needed because ModelSettings is hard-wired to save at its initial geographic level
saver <- ModelSettings$new(settings$dir, save_settings, submitted_causes = settings$submitted_causes)

## Construct population weights --------------------------------------------------------------------
# load the population file, and subset to the specified year
pop <- settings$load_population(year, settings$raking_area_var)
pop <- collapse_population_for_agg_race(pop = pop, settings = settings)

# subset to the specified sex, and check that weights sum to 1
wts <- pop[sex == get("sex", .GlobalEnv), list(area, year, sex, race, age, pop, wt)]
stopifnot(wts[, sum(wt), by = "area,year,sex,age"][, max(abs(V1 - 1))] < 1e-10)

# load the age standard
if (run_pred) {
  settings$measure <- "pred"
}
std_wt <- settings$load_std_wt()

## Aggregate mx and yll draws to all races combined and collapse draws to estimates ----------------
# define function to aggregate given variable (mx or yll) to all races combined
agg_race <- function(acause, year, sex, edu, wts, std_wt, var, raked) {
  # load draws
  draws <- settings$load_draws(
    acause = acause,
    measure = var,
    year = year,
    sex = sex,
    raked = if (raked) "raked" else "unraked",
    edu_levels = edu
  )

  setnames(draws, settings$area_var, 'area')
  draws[, level:=settings$area_var]
  draws <- draws[age < 98, ]

  # aggregate value draws to all races combined
  draws <- merge(draws, wts, by = c("area", "year", "sex", "race", "age"))

  merge_vars <- c("level", "area", "year", "sex", "age", "sim")
  if ("adjusted" %in% names(draws)) {
    merge_vars <- c(merge_vars, "adjusted")
  }

  draws <- draws[, list(value = sum(value * wt), pop = sum(pop)), by = merge_vars]

  draws[, race := race_default]
  setnames(draws, "value", var)

  # add crude and age-standardized rates
  draws <- calc_all_ages(draws, std_wt, var, c(merge_vars[!(merge_vars == "age")], "race"),
                         measure = var)

  est <- collapse_draws(draws, var, c(merge_vars[!(merge_vars == "sim")], "race"))

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
    races = race_default,
    edu_groups = edu
  )

  est[, acause:=acause]
  saver$io_engine$save_estimates(
    estimates = est,
    acause = acause,
    save_dir = saver$dir,
    measure = var,
    year = year,
    sex = sex,
    races = race_default,
    edu_groups = edu,
    raked = if (raked) "raked" else "unraked",
    age = NULL
  )
}

# aggregate mx draws
if (run_mx) {
  agg_race(
    acause = acause,
    year = year,
    sex = sex,
    edu = edu,
    wts = wts,
    std_wt = std_wt,
    var = "mx",
    raked = raked
  )
}

# aggregate yll draws
if (run_yll) {
  agg_race(
    acause = acause,
    year = year,
    sex = sex,
    edu = edu,
    wts = wts,
    std_wt = std_wt,
    var = "yll",
    raked = raked
  )
}

# aggregate yld draws
if (run_yld) {
  agg_race(
    acause = acause,
    year = year,
    sex = sex,
    edu = edu,
    wts = wts,
    std_wt = std_wt,
    var = "yld",
    raked = raked
  )
}

# aggregate pred (bmi) draws
if (run_pred) {
  agg_race(
    acause = acause,
    year = year,
    sex = sex,
    edu = edu,
    wts = wts,
    std_wt = std_wt,
    var = "pred",
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
    race_levels = settings$races,
    edu_levels = edu
  )
  setnames(draws, settings$area_var, 'area')
  draws[, level:=settings$area_var]

  # duplicate the weights for age 85+ to use for all specific age groups above 85
  wts[, pop := NULL]
  wts <- rbind(wts, wts[age == 85, list(age = seq(90, 110, 5)), by = "area,year,sex,race,wt"])

  # merge lt draws with population weights
  draws <- merge(draws, wts, by = c("area", "year", "sex", "race", "age"))

  # aggregate the mx and ax columns to all races combined
  merge_vars <- c("level", "area", "year", "sex", "age", "sim","edu")
  if ("adjusted" %in% names(draws)) {
    merge_vars <- c(merge_vars, "adjusted")
  }

  draws <- draws[, list(
    mx = sum(mx * wt),
    ax = sum(ax * (mx * wt) / sum(mx * wt))
  ),
  keyby = merge_vars]

  draws[, race := race_default]

  # for consistency, recalculate the life table using the aggregated mx and ax columns
  draws <- add_growth_rates(dir = dir, data = draws, idvars = c("year", "sex", "race", "edu", "area", "level"),
                            lt_hc = settings$lt_hc)

  draws <- draws[, lifetable(mx = mx, ax = ax, sex = sex[1], use_graduation = F, extrap = F,
                             lt_hc = settings$lt_hc, gr = mean_gr)[, list(age, mx, ax, qx, lx, dx, Lx, Tx, ex)],
    by = c(merge_vars[!(merge_vars == "age")], "race")]

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
    race = race_default,
    edu = edu
  )
  
  draws <- draws[,c("lx","dx","Lx","Tx","acause") := NULL]

  # save lt draws for all races combined
  save_vars <- c("level", "area", "year", "sex", "race", "age", "sim","edu")
  if ("adjusted" %in% names(draws)) {
    save_vars <- c(save_vars, "adjusted")
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
    races = race_default,
    edu_groups = edu
  )

  # take out NA draws; based on ax because qx will always be 1 in the terminal age group
  draws <- draws[!is.na(ax)]

  # save lt estimates for all races combined
  est <- collapse_draws_lt(draws = draws, dir = dir, id_vars = c(save_vars[!(save_vars == "sim")]))
  est[, acause := "all"]
  saver$io_engine$save_estimates(
    estimates = est,
    acause = "_all",
    save_dir = saver$dir,
    measure = "lt",
    year = year,
    sex = sex,
    age = NULL,
    races = race_default,
    edu_groups = edu,
    raked = if (raked) "raked" else "unraked"
  )

}
