####################################################################################################
## Description: Generate yll rate draws and estimates for all areas/ages for a given year, sex, and
##              race.
##
## Passed args: dir [character] -- home directory for settings and final output
##              year [integer] -- year to generate yll draws for
##              sex [integer] -- sex to generate yll draws for
##              race [integer] -- race to generate yll draws for
##              raked [logical] -- should raked mx draws be used?
##
## Requires:    mx draws ("[dir]/mx_draws_[area_var]_[year]_[sex]_[race].rds", or the
##                corresponding raked version).
##              lt draws ("[dir]/lt_draws_[area_var]_[year]_[sex]_[race].rds", or the
##                corresponding raked version).
##              interpolated reference life table (ref_lt_file)
##              populations (pop_file)
##              age standard file (age_std_file)
##
## Outputs:     yll draws and estimates:
##                "[dir]/yll_draws_[area_var]_[year]_[sex]_[race].rds"
##                "[dir]/yll_est_[area_var]_[year]_[sex]_[race].rds"
##                (or the corresponding raked files if raked == T)
##                NOTE: if raked == T these files will have a "_prelim_raked" suffix and not "_raked" suffix.
##                e.g., "[dir]/yll_est_mcnty_2008_2_3_prelim_raked.rds"
##
## Note:        Despite being saved in the data as "yll" these are really yll_rates
##
####################################################################################################

library(R.utils)
library(data.table)

library(lbd.loader, lib.loc = sprintf("FILEPATH", R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
suppressMessages(lbd.loader::load.containing.package())

## Get settings ------------------------------------------------------------------------------------
if (interactive()){
  dir <- 'FILEPATH'
  year <- 2002
  sex <- 1
  race <- 1
  raked <- FALSE
  edu <- 1
} else {
  parser <- argparse::ArgumentParser()
  parser$add_argument("--dir", type = "character", help = "Lower model directory")
  parser$add_argument("--year", type="integer", help="Year to subset to for rake operation")
  parser$add_argument("--sex", type = "integer", help="Sex to subset to for rake operation.", choices=c(1, 2))
  parser$add_argument("--race", type = "integer", help="Race dimension to calculate lifetables for.")
  parser$add_argument("--raked", type = "character", help = "TRUE if you want to load raked files.")
  parser$add_argument("--edu", type = "integer", help = "Education dimension to calculate lifetables for.")

  args <- parser$parse_args(get_args())
  for (key in names(args)) {
    assign(key, args[[key]])
  }

  print("Arguments passed were:")
  for (arg in names(args)){
    print(paste0(arg, ": ", get(arg)))
  }

  raked = as.logical(raked)
}

# If dealing with unraked (modeled) data, turn this option off to keep both adjusted == 0 and adjusted == 1 in the data.
if (!raked) setOption('ushd.use_adjusted_data', NA)

# Split 'cause' and 'dir' apart so you can use cause_mx_draws_path
acause = basename(dir)
settings <- ModelSettings$from_yaml(dirname(dir))$settings_for_cause(acause)

## Load inputs -------------------------------------------------------------------------------------
# average age at death (from life tables)
lt_file = cause_mx_draws_path(root = dirname(dir),
                              acause = "_all", # We only ever create life tables for all-cause.
                              measure = "lt",
                              type = "est",
                              area_var = settings$area_var,
                              year = year,
                              sex = sex,
                              race = race,
                              raked = ifelse(raked, "raked", "unraked"),
                              edu = edu)
message(sprintf("Reading life table file %s", lt_file))
if (!file.exists(lt_file)) lsae.utils::stop_or_quit(sprintf("life table file %s was not found!", lt_file), status = 9)

death_age <- readRDS(lt_file)

# get the terminal age group using the life table data
# there should not be any age 98 and 99 in the life table file, but just in case I have removed these
max_age <- max(death_age[!(age %in% c(98,99)), age])

stop.if.not.expected.cols(death_age, c("level", "area", "year", "sex", "race", "edu", "age"),
                          preamble="Columns validation for LT files", extra.ok=TRUE)

death_age[, death_age := age + ifelse(age == max_age, ex_mean, ax_mean)]

keep_vars <- c("level","area","year","sex", if (settings$by_race) "race", if (settings$by_edu) "edu", "age")
if("adjusted" %in% names(death_age)) {
  keep_vars <- c(keep_vars, "adjusted")
}
keep_vars_with_death_age = c(keep_vars, 'death_age')
death_age = death_age[, ..keep_vars_with_death_age]

death_age[death_age > 105, death_age := 105]
death_age[, death_age := round(death_age, 2)]

# mx draws
draws <- settings$load_draws(
  acause = acause,
  measure = "mx",
  year = year,
  sex = sex,
  race = race,
  edu = edu,
  raked = ifelse(raked, "raked", "unraked")
)
setnames(draws, settings$area_var, 'area')
draws[, level:=settings$area_var]
setnames(draws, 'value', 'mx')
draws <- draws[!(age %in% c(98, 99)), ]  # drop all-ages mortality rates

stop.if.not.expected.cols(draws, keep_vars, preamble="Draws for pred_yll", extra.ok=TRUE)

draws <- merge(draws, death_age, by = keep_vars)
if (any(is.na(draws$death_age))) lsae.utils::stop_or_quit("NAs found in death_age - will create issues producing life tables!", status = 20)

# interpolated reference life table
ref_lt <- fread(settings$settings$ref_lt_file)
draws <- merge(draws, ref_lt, by = "death_age", all.x = T)

pop <- load_population(pop_path = settings$pop_file)

if (!settings$by_race) pop[, race := race_default]
if (!settings$by_edu) pop[, edu := edu_default]
pop <- pop[mget(c("year", "sex", "race", "edu"), .GlobalEnv), list(area = get(settings$area_var), age, pop), on = c("year", "sex", if (settings$by_race) "race", if (settings$by_edu) "edu")]
pop <- pop[, list(pop = sum(pop)), by = "area,age"]

# age standard
std_wt <- settings$load_std_wt()

## Calculate YLL inputs -------------------------------------------------------------------------------------
# age-specific yll rates
draws[, yll := mx * ex]
if (any(is.na(draws$yll))) lsae.utils::stop_or_quit("NAs produced in YLL calculation", status = 20)
draws[, c("ex", "mx", "death_age") := NULL]

# all ages yll rates
merge_vars <- c("level","area","year","sex",if (settings$by_race) "race", if (settings$by_edu) "edu", "sim")
if("adjusted" %in% names(draws)) {
  merge_vars <- c(merge_vars,"adjusted")
}
draws <- merge(draws, pop, by = c("area", "age"))

draws <- calc_all_ages(draws, std_wt, "yll", merge_vars, allow_missing_ages = settings$by_edu)

# calculate point estimates and CIs
merge_vars <- c(merge_vars[!(merge_vars == "sim")], "age")
est <- collapse_draws(draws, "yll", merge_vars)

# calc_all_ages drops race, edu, acause (and warns as such)
draws[, race := race]
draws[, edu := edu]
draws[, acause := acause]
est[, race := race]
est[, edu := edu]
est[, acause := acause]

# save draws and estimates
settings$io_engine$save_draws(
  draws = draws,
  acause = acause,
  save_dir = settings$dir,
  measure = "yll",
  year = year,
  sex = sex,
  age = NULL,
  type = if (raked) 'pred_yll' else 'modeling',
  race = race,
  edu_groups = edu
)

settings$io_engine$save_estimates(
  est = est,
  acause = acause,
  save_dir = settings$dir,
  measure = "yll",
  year = year,
  sex = sex,
  raked = if (raked) 'prelim_raked' else 'unraked',
  races = c(race),
  edu_groups = c(edu),
  age = NULL
)
