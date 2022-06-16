####################################################################################################
## Description: Rake all cause mortality or Years Lived with Disability (yld) to sum up properly
##              to a specified aggregate geography.
##
## Inputs:      from_dir [character] -- the directory for all models where the raking variable
##              is disaggregated (e.g., by race/ethnicity.)
##              to_dir [character] -- the directory with one more level of raking aggregation
##              (e.g., all-race)
##              year [numeric] -- year to be raked
##              sex [numeric] -- sex to be raked
##              measure [character] -- which measure to rake, "mx" for mortality rates, "yll" for
##                  ylls and "yld" for Years Lived with Disability
##              from_geo [character] -- Geography level of the lower model.
##              to_geo [character] -- Geography level of the upper model.
##              common_raking_vars [character vector] -- List of variables that are at the same level
##                  of aggregation for both models.
##              lower_raking_vars [character vector] -- List of variables to rake by for lower model.
##              upper_raking_vars [character vector] -- List of variables to rake by for upper model.
##
####################################################################################################

library(lbd.loader, lib.loc = sprintf("FILEPATH", R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
suppressMessages(lbd.loader::load.containing.package())

## Get settings ------------------------------------------------------------------------------------
parser <- argparse::ArgumentParser()
parser$add_argument("--from_dir", type = "character", help = "Lower model directory")
parser$add_argument("--to_dir", type = "character", help = "Upper model directory")
parser$add_argument("--year", type = "integer", help = "Year to subset to for rake operation")
parser$add_argument("--sex", type = "integer", help = "Sex to subset to for rake operation.", choices = c(1, 2))
parser$add_argument("--measure", type = "character", help = "Which measure to rake.", choices = c("mx", "yll", "yld"))
parser$add_argument("--from_geo",
                    type = "character",
                    help = "Geography level for lower model.", choices = geo_choices
)
parser$add_argument("--to_geo",
                    type = "character",
                    help = "Geography level for upper model.", choices = geo_choices
)
parser$add_argument("--common_raking_vars", type = "character", help = "Variables at the same level of granularity between the two models.")
parser$add_argument("--lower_raking_vars", type = "character", help = "Variables to rake by for the lower model.")
parser$add_argument("--upper_raking_vars", type = "character", help = "Variables to rake by for the upper model.")

args <- parser$parse_args(get_args())
for (key in names(args)) {
  assign(key, args[[key]])
}

vector_args <- c("common_raking_vars", "lower_raking_vars", "upper_raking_vars")
for (i in 1:length(vector_args)) {
  assign(vector_args[i], unlist(strsplit(get(vector_args[i]), ",")))
}

print("Arguments passed were:")
for (arg in names(args)) {
  if (arg %in% vector_args) {
    print(paste0(arg, ": ", paste0(get(arg), collapse = ","), ". Vector length is ", length(get(arg))))
  } else {
    print(paste0(arg, ": ", get(arg)))
  }
}

#------------------------------------
# SET UP WORKSPACE
#------------------------------------
if (!dir.exists(from_dir)) {
  stop(sprintf("--from_dir %s does not exist", from_dir))
}
if (to_dir != "gbd" & !dir.exists(to_dir)) {
  stop(sprintf("--to_dir %s does not exist", to_dir))
}

if ("edu" %in% lower_raking_vars) R.utils::setOption("ushd.use_edu_paths", TRUE)

# Load the settings from the passed directories.
from_settings <- ModelSettings$from_dir(from_dir)
from_settings$to_geo <- to_geo 
if (to_dir == "gbd") {
  to_settings <- list(
    to_geo = to_geo,
    rake_to_gbd_version = from_settings$rake_to_gbd_version,
    ages = from_settings$ages,
    n.sims = from_settings$n.sims,
    area_var = to_geo
  )
  to_settings <- GBDSettings$new(gbd_settings = to_settings)
} else {
  to_settings <- ModelSettings$from_dir(to_dir)
}

gbd_upper_draws <- to_dir == "gbd"
by_race <- "race" %in% lower_raking_vars
by_edu <- "edu" %in% lower_raking_vars
if (by_race & by_edu) stop("Cannot rake a by-race and by-education model!")

expected_common_raking_vars <- intersect(upper_raking_vars, lower_raking_vars)
stopifnot(sort(expected_common_raking_vars) == sort(common_raking_vars))

#------------------------------------
# GET DRAWS
#------------------------------------
# Result is population summed by "area_var". Weights file is determined by "to_geo"
population <- from_settings$load_population(year, to_geo)

cause_name <- if (identical("yld", measure)) "all_cause" else "_all"

lower_draws <- from_settings$load_draws(
  acause = cause_name,
  measure = measure,
  year = year,
  sex = sex,
  raked = ifelse(measure == "yll", "prelim_raked", "unraked")
)

# Add population and weight column.
lower_draws <- from_settings$merge_population(lower_draws, population, measure=measure)

if (!to_geo %in% names(lower_draws)) {
  geo_vars <- c("mcnty", to_geo)
  lower_draws <- merge(lower_draws, unique(population[, ..geo_vars]), by = "mcnty")
}

upper_draws <- to_settings$load_draws(
  acause = cause_name,
  measure = measure,
  year = year,
  sex = sex,
  raked = "raked",
  population = if (identical("gbd", to_dir)) population else NULL
)

# Ensure that ages in upper draws match lower draws. 
if (by_edu){
  upper_draws = upper_draws[age %in% lower_draws$age]
}

#---------------------------
# PRE-RAKING VALIDATION
#--------------------------
# Ensure that the set and number of draws in (lower) draws matches those in upper_draws
stopifnot(all(unique(lower_draws[, sim]) == unique(upper_draws[, sim])))

# The dimensions of common raking vars should be the same between the two datasets
for (var in common_raking_vars) {
  print(sprintf("Testing common dimension %s...", var))
  if (!setequal(lower_draws[[var]], upper_draws[[var]])){
    stop_or_quit(sprintf("Common raking dimension %s has different values in upper and lower draws!", var), status = 3)
  }
}

setnames(upper_draws, "value", paste0(to_settings$area_var, "_value"))
draws <- merge.flexible(lower_draws, upper_draws)
rm(lower_draws, upper_draws)
gc()
#------------------------------------
# RAKE, AND VALIDATE
#------------------------------------

if (!"race" %in% names(population)) population[, race := race_default] 
draws <- add.population.weights(draws, population, demographics = upper_raking_vars, weight_var = "pop_weight", from_geo)

# call rake(), just once (one-dimensional raking only requires 1 iteration)
rake(draws, agg_var = to_geo, constant_vars = common_raking_vars, replace_value = T)

# check that the raked results do add up properly
validate_rake(
  draws,
  agg_var = to_geo,
  constant_vars = common_raking_vars,
  err_msg = sprintf("Discrepancy in raking %s to %s!", from_geo, to_geo)
)

#------------------------------------
# SAVE OUTPUTS
#------------------------------------
draws[, acause := cause_name]
from_settings$save_draws_and_estimates(
  draws = draws,
  acause = cause_name,
  measure = measure,
  year = year,
  sex = sex,
  type = 'raking'
)

message("FINISHED")
