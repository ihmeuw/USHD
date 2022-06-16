####################################################################################################
## Description: Calculate mx, yll, yld and (optionally) life table draws for both sexes combined by
##              aggregating the draws for males and females using population-weighted averages.
##
## Passed args: dir [character] -- home directory for settings and final output
##              level [character] -- the geographic level to caculate both sexes combined draws for
##              year [integer] -- year to calculate both sexes combined draws for
##              race [integer] -- race to calculate both sexes combined draws for
##              raked [logical] -- should raked (T) or unraked (F) draws be aggregated?
##              lt [logical] -- should life table draws be aggregated?
##
## Requires:    populations (pop_file)
##              cross-walk files, if level != area_var (geoagg_files[level])
##              mx, yll, and lt draws [if lt == T] by sex:
##                "[dir]/mx_draws_[level]_[year]_[sex]_[race]_[edu].rds"
##                "[dir]/yll_draws_[level]_[year]_[sex]_[race]_[edu].rds"
##                "[dir]/yld_draws_[level]_[year]_[sex]_[race]_[edu].rds"
##                "[dir]/lt_draws_[level]_[year]_[sex]_[race]_[edu].rds"
##                (or the corresponding raked files if raked == T)
##
## Outputs:     mx, yll, and lt [if lt == T] draws and estimates for both sexes combined:
##                "[dir]/mx_draws_[level]_[year]_3_[race]_[edu].rds"
##                "[dir]/mx_est_[level]_[year]_3_[race]_[edu].rds"
##                "[dir]/yll_draws_[level]_[year]_3_[race]_[edu].rds"
##                "[dir]/yll_est_[level]_[year]_3_[race]_[edu].rds"
##                "[dir]/yld_draws_[level]_[year]_3_[race]_[edu].rds"
##                "[dir]/yld_est_[level]_[year]_3_[race]_[edu].rds"
##                "[dir]/lt_draws_[level]_[year]_3_[race]_[edu].rds"
##                "[dir]/lt_est_[level]_[year]_3_[race]_[edu].rds"
##                (or the corresponding raked files if raked == T)
##
####################################################################################################

library(lbd.loader, lib.loc = sprintf("FILEPATH", R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
suppressMessages(lbd.loader::load.containing.package())

## Get settings ------------------------------------------------------------------------------------
parser <- argparse::ArgumentParser()
add_dir_argument(parser)
add_level_argument(parser)
add_year_argument(parser)
add_race_argument(parser)
add_edu_argument(parser)
add_raked_argument(parser)
add_lifetable_argument(parser)
add_aggregation_skip_flags(parser)

args <- parser$parse_args(get_args())

dir <- args$dir
level <- args$level
year <- args$year
race <- args$race
edu <- args$edu
raked <- as.logical(args$raked)
run_lt <- as.logical(args$run_lt)
run_mx <- ! args$skip_mx_aggregation
run_yll <- ! args$skip_yll_aggregation
run_yld <- ! args$skip_yld_aggregation

for (setting in c('dir', 'level', 'year', 'race', 'edu', 'raked', 'run_lt', 'run_mx', 'run_yll', 'run_yld')){
  print(paste0(setting, ': ', get(setting)))
}

# If dealing with unraked (modeled) data, turn this option off to keep both adjusted == 0 and adjusted == 1 in the data.
if (!raked) setOption('ushd.use_adjusted_data', NA)

acause = basename(dir)
settings <- ModelSettings$from_dir(dirname(dir))$settings_for_cause(acause)

# create new object for saving.
save_settings <- copy(settings$settings)
if (race == race_default) save_settings$races <- unique(c(race_default, save_settings$races))
if (edu == edu_default) save_settings$edu_groups <- unique(c(edu_default, save_settings$edu_groups))
save_settings$area_var <- level
# This new object is needed because ModelSettings is hard-wired to save at its initial geographic level
saver <- ModelSettings$new(settings$dir, save_settings, submitted_causes = settings$submitted_causes)

## Construct population weights --------------------------------------------------------------------
pop <- settings$load_population(year, settings$raking_area_var)
setnames(pop, settings$area_var, "area")

# collapse populations to the required dimensions to remove any unneeded stratifiers
if (!settings$by_race) pop[, race := race_default]
if (!settings$by_edu) pop[, edu:=edu_default]
pop <- pop[, list(pop = sum(pop)), keyby = "area,year,sex,race,edu,age"]

# if needed, aggregate over geography
if (level != settings$area_var) {
  wts <- settings$load_geo_wts(level)
  setnames(wts, c(settings$area_var, level), c("area", "area2"))
  id_vars <- intersect(c("area", "year", "sex", "race", "edu", "age"), names(wts))
  pop <- merge(pop, wts, by = id_vars, all = T)
  pop <- pop[, list(pop = sum(pop * wt)), by = "area2,year,sex,race,edu,age"]
  setnames(pop, "area2", "area")
}

# if needed, add race column
if (race == race_default) {
  pop[, race := race_default]
}
# if needed, add edu column
if (edu == edu_default) {
  pop[, edu := edu_default]
}
# if needed, aggregate over edu and/or race
if (race == race_default | edu == edu_default) {
  pop <- pop[, list(pop = sum(pop)), keyby = "area,year,sex,race,edu,age"]
}


# reset tiny populations to 0 to avoid annoying edge cases where one group gets all of the weight
# even though it has effectively no population
pop[pop < 1e-5, pop := 0]

# calculate aggregation weights for each area-year-race-edu-age
pop[, wt := pop / sum(pop), by = "area,year,race,edu,age"]

# this fails when the total population in a given area-year-race-edu-age is 0.
# In these cases, we use the following hierarchy of backups: area-year-race-edu (no age), area-year (no
# age, race, or edu)
pop[, pop2 := sum(pop), by = "area,year,sex,race,edu"]
pop[, wt2 := pop2 / sum(pop2), by = "area,year,race,edu,age"]

pop[, pop3 := sum(pop), by = "area,year,sex"]
pop[, wt3 := pop3 / sum(pop3), by = "area,year,race,edu,age"]

pop[is.nan(wt), wt := wt2]
pop[is.nan(wt), wt := wt3]

# subset to the specified race and edu, and check that weights sum to 1
wts <- pop[race == get("race", .GlobalEnv) & edu == get("edu", .GlobalEnv), list(area, year, sex, race, edu, age, pop, wt)]
stopifnot(wts[, sum(wt), by = "area,year,race,edu,age"][, max(abs(V1 - 1))] < 1e-10)

# load the age standard
std_wt <- settings$load_std_wt()

## Aggregate mx and yll draws to both sexes combined and collapse draws to est----------------------
# define function to aggregate given variable (mx or yll) to both sexes combined
agg_sex <- function(acause, year, race, edu, wts, std_wt, var, raked) {

  draws <- saver$load_draws(
    acause = acause, 
    measure = var, 
    year = year, 
    sex = saver$sexes, 
    raked = if (raked) "raked" else "unraked",
    race_levels = race,
    edu_levels = edu
  )

  setnames(draws, saver$area_var, 'area')
  draws[, level:= get('level', envir = .GlobalEnv)]
  
  draws <- draws[!(age %in% c(98, 99)), ]  # drop all-ages mortality rates

  # if necessary, add in 0s for sexes we don't model (specific to certain causes, eg, maternal)
  if (length(settings$sexes) == 1) {
    temp <- copy(draws)
    temp[, value := 0]
    if (settings$sexes == 1) temp[, sex := 2] else temp[, sex := 1]
    draws <- rbind(draws, temp)
    setkeyv(draws, c("level", "area", "year", "sex", "race", "edu", "age", "sim"))
    rm(temp); gc()
  }

  # aggregate value draws to both sexes combined
  draws <- merge(draws, wts, by = c("area", "year", "sex", "race", "edu", "age"))

  merge_vars <- c("level","area","year","race", "edu", "age","sim")
  if("adjusted" %in% names(draws)) {
    merge_vars <- c(merge_vars,"adjusted")
  }

  draws <- draws[, list(value = sum(value * wt), pop = sum(pop)), by = merge_vars]
  draws[, sex := 3]

  setnames(draws, "area", level)
  draws[, acause := basename(dir)]
  saver$save_draws_and_estimates(
    draws = draws,
    acause = basename(dir),
    measure = var,
    year = year,
    sex = 3,
    race = race,
    edu = edu,
    type = if (raked) 'aggregation' else 'modeling'
  )
}

# aggregate mx draws
if (run_mx) {
  agg_sex(
    acause = acause,
    year = year,
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
  agg_sex(
    acause = acause,
    year = year,
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
  agg_sex(
    acause = acause,
    year = year,
    race = race,
    edu = edu,
    wts = wts,
    std_wt = std_wt,
    var = "yld",
    raked = raked
  )
}

## Aggregate lt draws to both sexes combined -------------------------------------------------------
if (run_lt) {
  message("Aggregating life tables")

  draws <- saver$load_draws(
    acause = acause,
    measure = 'lt',
    year = year,
    sex = c(1, 2),
    raked = if (raked) "raked" else "unraked",
    race_levels = race,
    edu_levels = edu
  )
  setnames(draws, saver$area_var, 'area')
  draws[, level := get('level', envir = .GlobalEnv)]

  # duplicate the weights for age 85+ to use for all specific age groups above 85
  wts[, pop := NULL]
  wts <- rbind(wts, wts[age == 85, list(age = seq(90, 110, 5)), by = "area,year,sex,race,edu,wt"])

  if (!'area' %in% names(draws)){
    setnames(draws, settings$area_var, 'area')
  }

  # merge lt draws with population weights
  draws <- merge(draws, wts, by = c("area", "year", "sex", "race", "edu", "age"))

  # aggregate the mx and ax columns to both sexes combined
  merge_vars <- c("level","area","year","race", "edu", "age","sim")
  if("adjusted" %in% names(draws)) {
    merge_vars <- c(merge_vars,"adjusted")
  }

  draws <- draws[, list(mx = sum(mx * wt),
                        ax = sum(ax * (mx * wt) / sum(mx * wt))),
                 keyby = merge_vars]
  draws[, sex := 3]

  # for consistency, recalculate the life table using the aggregated mx and ax columns
  draws <- add_growth_rates(dir = dir, data = draws, idvars = c("year","sex","race","area", "level"),
                            lt_hc = settings$lt_hc)

  draws <- draws[, lifetable(mx = mx, ax = ax, sex = sex[1], graduation = F, extrap = F,
                             lt_hc = settings$lt_hc, gr = mean_gr)[, list(age, mx, ax, qx, ex)],
                 by = c(merge_vars[!(merge_vars == "age")], "sex")]

  # get rid of growth rate
  if ("mean_gr" %in% names(draws)) {
    draws[, mean_gr := NULL]
  }

  # save lt draws for both sexes combined
  save_vars <- c("level","area","year","sex","race","edu", "age","sim")
  if("adjusted" %in% names(draws)) {
    save_vars <- c(save_vars,"adjusted")
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
    sex = 3,
    type = if (raked) 'aggregation' else 'modeling',
    races = race,
    edu_groups = edu
  )

  # take out NA draws; based on ax because qx will always be 1 in the terminal age group
  draws <- draws[!is.na(ax)]

  est <- collapse_draws_lt(draws = draws, dir = dir, id_vars = c(save_vars[!(save_vars == "sim")]))
  est[, acause := "all"]
  saver$io_engine$save_estimates(
    estimates = est,
    acause = "_all",
    save_dir = saver$dir,
    measure = "lt",
    year = year,
    sex = 3,
    age = NULL,
    races = race,
    edu_groups = edu,
    raked = if (raked) "raked" else "unraked"
  )
}
