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
##                  Years of Life Lost and "yld" for Years Lived with Disability
##                  "pred" for bmi prevalence-estimates (overweight, obese) and mean_bmi.
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
if (interactive()) {
  # `cp -as ABS_PATH_TO_SRC dest` to keep it clean-ish

  # Second rake: by-race to all-race

  from_dir <- "FILEPATH"
  to_dir <- "FILEPATH"
  year <- 2013
  sex <- 2
  measure <- "mx"
  from_geo <- "mcnty"
  to_geo <- "mcnty"
  common_raking_vars <- c("year", "sex", "age", "sim", "mcnty") # equal to the :shared_demographics" in the old scripts
  lower_raking_vars <- c("year", "sex", "age", "sim", "mcnty", "edu") # equal to "lower_demographics" in the old scripts
  upper_raking_vars <- c("year", "sex", "age", "sim", "mcnty") # equal to "geography_demographics" in the old scripts

} else {
  parser <- argparse::ArgumentParser()
  parser$add_argument("--from_dir", type = "character", help = "Lower model directory")
  parser$add_argument("--to_dir", type = "character", help = "Upper model directory")
  parser$add_argument("--year", type = "integer", help = "Year to subset to for rake operation")
  parser$add_argument("--sex", type = "integer", help = "Sex to subset to for rake operation.", choices = c(1, 2))
  parser$add_argument(
    "--measure",
    type = "character",
    help = "Which measure to rake.",
    choices = c("mx", "yll", "yld", "pred")
    )
  parser$add_argument(
    "--from_geo",
    type = "character",
    help = "Geography level for lower model.", choices = geo_choices
  )
  parser$add_argument(
    "--to_geo",
    type = "character",
    help = "Geography level for upper model.", choices = geo_choices
  )
  parser$add_argument(
    "--common_raking_vars",
    type = "character",
    help = "Variables at the same level of granularity between the two models."
    )
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
from_settings$to_geo <- to_geo # This is used to merge population onto draws
if (to_dir == "gbd") {
  if (measure %in% c("mx", "yll", "yld", "pred")) {
    to_settings <- list(
      to_geo = to_geo,
      rake_to_gbd_version = from_settings$rake_to_gbd_version,
      ages = from_settings$ages,
      n.sims = from_settings$n.sims,
      area_var = to_geo
    )
    to_settings <- GBDSettings$new(gbd_settings = to_settings)
  }
} else {
  to_settings <- ModelSettings$from_dir(to_dir)
}

# Create logic-flow variables from command-line args
gbd_upper_draws <- to_dir == "gbd"
by_race <- "race" %in% lower_raking_vars
by_edu <- "edu" %in% lower_raking_vars
if (by_race & by_edu) stop("Cannot rake a by-race and by-education model!")

# Validate that later logic will work correctly
expected_common_raking_vars <- intersect(upper_raking_vars, lower_raking_vars)
stopifnot(sort(expected_common_raking_vars) == sort(common_raking_vars))

#------------------------------------
# GET DRAWS
#------------------------------------
# Result is population summed by "area_var". Weights file is determined by "to_geo"
population <- from_settings$load_population(year, to_geo)

if (identical(measure, "yld")) {
  cause_name <- "all_cause"
} else if (identical(measure, "pred")){
  # cause_name (e.g. overweight) is used for getting correct cause_id while loading GBD draws for BMI-prevalence.
  cause_name <- basename(dirname(from_dir))
} else {
  cause_name <- "_all"
}

lower_draws <- from_settings$load_draws(
  acause = cause_name,
  measure = measure,
  year = year,
  sex = sex,
  raked = ifelse(measure == "yll", "prelim_raked", "unraked")
)

# Add population and weight column. These are used by 'add.population.weights' after lower + upper draws are merged.
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
) # GBD data needs population to calculate upper mortality rates, USHD data does not

# Ensure that ages in upper draws match lower draws.
# There can be a discrepancy when raking an education model (subset of ages)
# To a general county model. GBD ages should be subset when to_settings is created.
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
    lsae.utils::stop_or_quit(sprintf("Common raking dimension %s has different values in upper and lower draws!", var), status = 3)
  }
}

# Merge
setnames(upper_draws, "value", paste0(to_settings$area_var, "_value")) # Rename upper geography value to what rake() will expect
draws <- merge.flexible(lower_draws, upper_draws)
rm(lower_draws, upper_draws)
gc()
#------------------------------------
# RAKE, AND VALIDATE
#------------------------------------


# We should make this consistent - i.e., decide if we include all potential columns with aggregate value, or no.
if (!"race" %in% names(population)) population[, race := race_default] # defined in functions/constants.r
draws <- add.population.weights(draws, population, demographics = upper_raking_vars, weight_var = "pop_weight", from_geo)

# call rake(), just once (one-dimensional raking only requires 1 iteration)
# Implementation notes -
# The rake() function expects {agg_var}_value to always be in the correct space.
# Where correct space is defined as:
#   * count space iff weight_pops == TRUE (default)
#   * rate space iff weight_pops == FALSE (only when explicitly passed as weight_pops = F)
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
