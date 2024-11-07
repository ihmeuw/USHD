####################################################################################################
## Description: Run life table functions on mx draws to get life table draws.
##
## Passed args: dir [character] -- home directory for settings and final output
##              year [integer] -- year to generate life table draws for
##              sex [integer] -- sex to generate life table draws for
##              race [integer] -- race to generate life table draws for
##              raked [logical] -- should raked mx draws be used?
##
## Requires:    mx draws ("[dir]/mx_draws_[area_var]_[year]_[sex]_[race]_[edu].rds", or the
##                corresponding raked version).
##
## Outputs:     lt draws and estimates:
##                "[dir]/lt_draws_[area_var]_[year]_[sex]_[race]_[edu].rds"
##                "[dir]/lt_est_[area_var]_[year]_[sex]_[race]_[edu].rds"
##                (or the corresponding raked files if raked == T)
##
####################################################################################################

library(R.utils)
library(data.table)

library(lbd.loader, lib.loc = sprintf("FILEPATH", R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
suppressMessages(lbd.loader::load.containing.package())


## Get settings ------------------------------------------------------------------------------------
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

# If dealing with unraked (modeled) data, turn this option off to keep both adjusted == 0 and adjusted == 1 in the data. 
if (!raked) setOption('ushd.use_adjusted_data', NA)

acause = basename(dir)
if (!identical(acause, "_all")) stop_or_quit("Something wrong - can only pred_lt.r using acause '_all'")

# This requires yaml settings be present.
acause = basename(dir)
settings <- ModelSettings$from_dir(dirname(dir))$settings_for_cause(acause)

## Boolean that determines if the life table should extrapolate the older ages
## Currently set to False because we are calculating the LTs using age 85+ as the oldest age group.
extrap_bool <- F

## Run life table code on mx draws to get lt draws -------------------------------------------------
# load draws
draws <- settings$load_draws(
  acause = acause,
  measure = "mx",
  year = year,
  sex = sex,
  race = race,
  edu = edu,
  raked = ifelse(raked, "raked", "unraked")
)
if (any(c(98, 99) %in% draws$age)) draws = draws[age <= max(settings$ages)] 

merge_vars <- c(settings$area_var, "year", "sex", "race", "edu", "sim")
if("adjusted" %in% names(draws)) {
  merge_vars <- c(merge_vars,"adjusted")
}

setnames(draws, settings$area_var, "area")
draws[,level := settings$area_var]

draws <- add_growth_rates(dir = dir, data = draws,
                          idvars = c("year","sex","area", "level", "race"),
                          lt_hc = settings$lt_hc)

setnames(draws, "area", settings$area_var)
draws[,level := NULL]

draws <- draws[, lifetable(mx = value, sex = sex[1], graduation = T, extrap = extrap_bool,
                           lt_hc = settings$lt_hc, gr = mean_gr)[, list(age, mx, ax, qx, lx, dx, Lx, Tx, ex)],
               by = merge_vars]

setnames(draws, settings$area_var, "area")
draws[, level := settings$area_var]
draws[, acause := "_all"] 

settings$io_engine$save_draws(
  draws = draws, 
  acause = "_all", 
  save_dir = settings$dir, 
  measure = "FULL_lt", 
  year = year, 
  sex = sex,
  age = NULL, 
  type = if (raked) 'aggregation' else 'modeling',
  race = race,
  edu = edu
)

draws <- draws[,c("lx","dx","Lx","Tx") := NULL]

# format and save lt draws
save_vars <- c("level", "area", "year", "sex", "race", "edu", "age", "sim")
if("adjusted" %in% names(draws)) {
  save_vars <- c(save_vars,"adjusted")
}

draws[, acause := "_all"] 
setkeyv(draws, save_vars)
setcolorder(draws, c(save_vars, "mx", "ax", "qx", "ex"))

settings$io_engine$save_draws(
  draws = draws, 
  acause = "_all", 
  save_dir = settings$dir, 
  measure = "lt", 
  year = year, 
  sex = sex, 
  races = c(race), 
  edu_groups = c(edu),
  age = NULL, 
  type = if (raked) 'aggregation' else 'modeling'
)

.tmp.path <- cause_mx_draws_path(
  root = dirname(dir),
  acause = "_all",
  measure = "lt",
  type = "draws",
  area_var = settings$area_var,
  year = year,
  sex = sex,
  race = race,
  raked = if (raked) "raked" else "unraked",
  edu = edu
)

path <- file.path(
  dirname(.tmp.path),
  gsub("draws", "draws_removed", basename(.tmp.path))
)
saveRDS(draws[is.na(ax)], file = path)
draws <- draws[!is.na(ax)]

# calculate point estimates, confidence intervals, and standard errors for lt
merge_vars <- c(merge_vars[!(merge_vars == "sim")], "age")
setnames(draws, "area", settings$area_var)
draws[, level := NULL]
est <- collapse_draws_lt(draws = draws, dir = dir, id_vars = merge_vars)
setnames(est, settings$area_var, "area")
est[, level := settings$area_var]
est[, acause := "_all"]

settings$io_engine$save_estimates(
  est = est, 
  acause = "_all", 
  save_dir = settings$dir, 
  measure = "lt", 
  year = year, 
  sex = sex, 
  raked = if (raked) "raked" else "unraked",
  races = c(race), 
  edu_groups = c(edu),
  age = NULL
)