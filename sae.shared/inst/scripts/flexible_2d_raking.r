####################################################################################################
##
## Description: Rake from_dir to to_dir across two dimensions (currently geo and cause, or
##                  race and cause - edu coming soon!)
##
## Inputs:      from_dir [character] -- the directory for all models where the raking variable
##                  is disaggregated (e.g., by race/ethnicity.)
##              to_dir [character] -- the directory with one more level of raking aggregation
##                  (e.g., all-race)
##              parent_acause [character] -- Parent acause to rake to
##              year [numeric] -- year to be raked
##              sex [numeric] -- sex to be raked
##              measure [character] -- which measure to rake, "mx" for mortality rates, "yll" for
##                  ylls
##              draws [integer] -- Number of draws to pull for the rake operation.
##              common_raking_vars [character vector] -- List of variables that are at the same level
##                  of aggregation for both models.
##              target_dims [character vector] -- List of target dimensions to rake across
##              verbose [character] -- "TRUE" or "FALSE" - Additional output for debugging
##
## Note:        Some inputs are read from the settings.csv file in the from_dir
##
####################################################################################################

# https://stash.ihme.washington.edu/projects/LSAE/repos/lbd.loader/browse
library(lbd.loader, lib.loc = sprintf("FILEPATH", R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
suppressMessages(lbd.loader::load.containing.package())

## Get settings ------------------------------------------------------------------------------------

parser <- argparse::ArgumentParser()
parser$add_argument("--from_dir", type = "character", help = "Lower model directory")
parser$add_argument("--to_dir", type = "character", help = "Upper model directory")
parser$add_argument("--parent_acause", type = "character", help = "Parent Cause")
parser$add_argument("--year", type = "integer", help = "Year to subset to for rake operation")
parser$add_argument("--age", type = "integer", help = "Age to subset to for rake operation")
parser$add_argument("--sex", type = "integer", help = "Sex to subset to for rake operation.", choices = c(1, 2))
parser$add_argument("--measure", type = "character", help = "Which measure to rake.", choices = c("mx", "yll"))
parser$add_argument("--target_dims",
                    type = "character",
                    help = "Dimensions to rake across."
)
parser$add_argument("--to_geo",
                    type = "character",
                    help = "Geography level for upper model.", choices = geo_choices
)
parser$add_argument("--draws", type = "integer", help = "Number of draws to rake at.")
parser$add_argument("--common_raking_vars", type = "character", help = "Variables at the same level of granularity between the two models.")
parser$add_argument("--use_verbose", type = "character", default = "FALSE", choices = c("TRUE", "FALSE"))
parser$add_argument("--iterations", type = "integer", help = "Max number of iterations allowed", default = 500L)

args <- parser$parse_args(get_args())
for (key in names(args)) {
  assign(key, args[[key]])
}

verbose <- as.logical(use_verbose)

vector_args <- c("common_raking_vars", "target_dims")
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

if ("edu" %in% target_dims) R.utils::setOption("ushd.use_edu_paths", TRUE)

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

if (length(target_dims) < 2 & length(target_dims) > 3){
  stop(sprintf("Number of target_dims (%s) = %d - not allowed. Must choose between 2 and 3 target dimensions", paste(as.character(target_dims), collapse = ", "), length(target_dims)))
} 

other_dim <- NULL # Default value, used for cause/geo rake
if (by_race <- "race" %in% target_dims) {
  other_dim <- "race"
  other_list <- from_settings$races
}
if (by_edu <- "edu" %in% target_dims) {
  other_dim <- "edu"
  other_list <- from_settings$edu_list
}
if (by_race & by_edu) {
  stop("race and edu both in target_dims - not allowed")
}

by_cause <- "cause" %in% target_dims
by_geo <- "geo" %in% target_dims

raking_area_var <- to_geo
if (by_geo) {
  population_constants <- c("wt", from_settings$raking_area_var)
} else {
  population_constants <- c("wt")
}

geography_demographics <- unique(c("acause", to_geo, common_raking_vars))

max.tol <- 1e-10

if (draws != 1000) {
  warning(sprintf("draws is %i. Only draws 0-%i will be used.", draws, draws - 1))
}

# Load metadata objects
if (by_cause) {
  # This function filters the subset of child causes to those which are not age or sex excluded
  # Raking is then conducted only on those causes for this age-sex combination
  # compile_raked.r then makes the dataset square again by adding the excluded age-sex combinations
  # back as 0.0
  causes <- load_child_cause_metadata(
    from_dir,
    parent_acause = parent_acause,
    sex = sex,
    age = age
  )

  # Flag for triggering additional manipulations if a subset of child causes are modeled by USHD
  # This is different than the age-sex exclusions handled above.
  not_all_child_causes_modeled <- nrow(causes[run_model == 0]) > 0
} else {
  causes = data.table(
    'acause' = '_all',
    'run_model' = 1,
    'path_to_top_parent' = 294
  )
  not_all_child_causes_modeled <- FALSE
}
#------------------------------------
# GET DRAWS
#------------------------------------
# We need three data frames -
#   1. lower_cause_draws
#   2. upper_parent_cause_draws
#   3. upper_parent_geography_draws, OR draws from the "absorbed" dimension (race or education).

population <- from_settings$load_population(year, to_geo)

if (verbose) print(sprintf("Rake by %s", paste(as.character(target_dims), collapse = ", ")))

lower_demographics <- c("acause", other_dim, from_settings$area_var, common_raking_vars)
parent_cause_demographics <- c(other_dim, from_settings$area_var, common_raking_vars)

# Load lower draws (At the lower level of geography/other dimension (race, edu), and at the child-cause level)
message("Loading lower draws.")
lower_draws <- from_settings$load_draws(
  acause = causes[run_model == 1, acause],
  measure = measure,
  year = year,
  sex = sex,
  age = age,
  raked = ifelse(measure == "yll", "prelim_raked", "unraked")
)

lower_draws <- from_settings$merge_population(lower_draws, population, measure=measure)

# Attach population to parent cause draws (saved by age)
# Load upper parent cause draws (At the lower level of geography/other dimension (race, edu), and at the parent-cause level)
message("Loading upper parent cause draws.")
upper_parent_cause_draws <- from_settings$load_draws(
  acause = parent_acause,
  measure = measure,
  year = year,
  sex = sex,
  age = age,
  raked = if (parent_acause == "_all") "raked" else "raked_temp",
  population = population
)

# Load upper geography draws (At the upper level of geography/other dimension (race, edu), and at the child-cause level)
message("Loading upper geography draws.")
upper_geography_draws <- to_settings$load_draws(
  acause = causes[, acause],
  measure = measure,
  year = year,
  sex = sex,
  age = age,
  raked = if (to_dir == "gbd" || parent_acause == "_all") "raked" else "raked_temp",
  population = if (to_dir == "gbd") population else NULL
)

upper_geography_draws[, level := to_geo]
setnames(upper_geography_draws, "value", paste0(raking_area_var, "_value"))
#---------------------------
# PRE-RAKING VALIDATION
#--------------------------
# The dimensions of common raking vars should be the same between the two datasets
for (var in common_raking_vars) {
  message(sprintf("Checking common variable %s...", var))
  stopifnot(sort(unique(lower_draws[[var]])) == sort(unique(upper_parent_cause_draws[[var]])))
  stopifnot(sort(unique(lower_draws[[var]])) == sort(unique(upper_geography_draws[[var]])))
}

# If rates for the parent are all zero (as happens for restricted ages), return 0s
if (nrow(upper_parent_cause_draws[value != 0]) == 0) {
  message("Saving draws of 0 because the parent draws are all 0s")

  for (cause in causes[, acause]) {
    
    make_output_dir(file.path(from_dir, cause), verbose)
    from_settings$save_draws_and_estimates(
      draws = lower_draws[acause == cause],
      acause = cause,
      measure = measure,
      year = year,
      sex = sex,
      age = age,
      type = "raking"
    )
  }
  
  message("FINISHED")

} else {
  
  # if some child causes are not modeled, calculate the cause fraction represented by the remaining
  # causes and then remove the excluded causes from upper draws.
  if (not_all_child_causes_modeled) {
    upper_res <- remove_excluded_cause_draws(upper_geography_draws, causes, raking_area_var, unique(c(common_raking_vars, 'race', 'edu')))
    upper_geography_draws <- upper_res[["data"]]
    removed_upper_draws <- upper_res[["removed_draws"]]
  }
  
  # Run raking: county to national ------------------------------------------
  
  # Ensure that the set and number of draws in (lower) draws matches those in upper_draws
  stopifnot(all(unique(lower_draws[, sim]) == unique(upper_parent_cause_draws[, sim])))
  stopifnot(all(unique(lower_draws[, sim]) == unique(upper_geography_draws[, sim])))
  
  setnames(upper_parent_cause_draws, c("value"), c("parent_value"))
  
  # Merge all draws together
  message("Merging lower draws and upper parent cause draws")
  raking_val <- merge.flexible(lower_draws, upper_parent_cause_draws)
  
  if (!to_geo %in% names(raking_val)) {
    geo_vars <- c("mcnty", to_geo)
    raking_val <- merge(raking_val, unique(population[, ..geo_vars]), by = "mcnty")
  }
  
  message("Merging result onto upper geography draws")
  
  # Validate that all causes present in lower draws are also available in upper geography draws. 
  unavailable_causes = unique(raking_val[!acause %in% upper_geography_draws$acause, acause])
  
  allowed_gbd_unavailability = paste0(
    rep("mater_neonat", 25), # Causes
    "-",
    c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85,
      5, 55, 60, 65, 70, 75, 80, 85), 
    "-", 
    c(rep(1, 17), rep(2, 8)) # Sexes
  )
  
  if (length(unavailable_causes) > 0){
    # Check which causes are unavailable, and error if any are not expected. 
    for (cause in unavailable_causes){
      if (!paste0(cause, "-", age, "-", sex) %in% allowed_gbd_unavailability){
        lsae.utils::stop_or_quit(sprintf("%s is available in lower draws but not upper draws - check GBD data availability.", 
                                         cause, status = 3))
      } 
    }
    # Case 1. The fix is to temporarily remove these draws and then add them back in. 
    # You can apply this for all unavailable_causes at once if the chunk above did not error.
    removed_lower_draws = raking_val[!acause %in% upper_geography_draws$acause]
    stopifnot(all(removed_lower_draws$value == 0))
    raking_val = raking_val[acause %in% upper_geography_draws$acause]
    
    stopifnot(setequal(raking_val$acause, upper_geography_draws$acause))
    raking_val <- merge.flexible(raking_val, upper_geography_draws)
    raking_val = rbind(raking_val, removed_lower_draws, fill = TRUE)
    
  } else {
    raking_val <- merge.flexible(raking_val, upper_geography_draws)
  }
  
  # if there are no predictions in the upper level, fill in with 0s
  raking_val[is.na(get(paste0(raking_area_var, "_value"))), paste0(raking_area_var, "_value") := 0]
  
  # The upper raking dimension needs to be renamed to "parent". 
  setnames(raking_val, "acause_target", "parent")
  
  # If some child causes are excluded from raking,
  # remove the cause fraction represented by these causes from the parent cause mortality rate.
  # Namely, multiply the parent value by the cause fraction of the remaining causes calculated for the upper geography
  if (not_all_child_causes_modeled) {
    raking_val <- scale_down_parent_val(raking_val, removed_upper_draws, raking_area_var, common_raking_vars)
  }
  
  # calculate population weights to aggregate to upper geography
  raking_val <- add.population.weights(raking_val, population, demographics = geography_demographics, weight_var = "pop_weight", area_var = from_settings$area_var)
  
  # confirm that the marginals add up properly:
  # the weighted mean of the parent value's and the sum of the upper_geog value's should be equal
  confirm_equal_margins(
    raking_val,
    geography_value_col = paste0(raking_area_var, "_value"),
    parent_value_col = "parent_value",
    geography_demographics = geography_demographics,
    parent_cause_demographics = parent_cause_demographics
  )
  
  stop.if.not.expected.cols(raking_val,
                            expected = c(
                              "parent", "parent_value", "value",
                              raking_area_var, paste0(raking_area_var, "_value")
                            ),
                            extra.ok = TRUE
  ) 
  
  # Race and education are now always present, and should always be in by-vars if we are not raking by one of those dimensions.
  geography_constant_vars = unique(c(intersect(geography_demographics, lower_demographics)))
  cause_constant_vars = unique(c(intersect(parent_cause_demographics, lower_demographics)))
  
  # if race is not one of the variables that we are summing over, and it is not already in the lists of variables, then add it
  if(!("race" %in% lower_demographics & !("race" %in% geography_demographics))) {
    cause_constant_vars <- unique(c(cause_constant_vars, "race"))
    geography_constant_vars <- unique(c(geography_constant_vars, "race"))
  } 
  
  # then same for edu
  if(!("edu" %in% lower_demographics & !("edu" %in% geography_demographics))) {
    cause_constant_vars <- unique(c(cause_constant_vars, "edu"))
    geography_constant_vars <- unique(c(geography_constant_vars, "edu"))
  } 
  
  raked_output_draws <- rake_two_dimensions(
    raking_val,
    geography_var = raking_area_var,
    cause_var = "parent",
    geography_constant_vars = geography_constant_vars,
    cause_constant_vars = cause_constant_vars,
    max_iterations = get_max_iterations(from_dir, args, iterations),
    tol = max.tol,
    dir = from_dir,
    measure = measure
  )
  
  # if some child causes were excluded from raking, calculate them now from the cause fractions
  if (not_all_child_causes_modeled) {
    
    # if race is not one of the variables that we are summing over, and it is not already in the lists of variables, then add it
    if(!("race" %in% lower_demographics & !("race" %in% geography_demographics))) {
      lower_add_draws_vars <- unique(c(lower_demographics, "race"))
      geography_add_draws_vars <- unique(c(geography_demographics, "race"))
    } else {
      lower_add_draws_vars <- copy(lower_demographics)
      geography_add_draws_vars <- copy(geography_demographics)
    }
    
    # then same for edu
    if(!("edu" %in% lower_demographics & !("edu" %in% geography_demographics))) {
      lower_add_draws_vars <- unique(c(lower_add_draws_vars, "edu"))
      geography_add_draws_vars <- unique(c(geography_add_draws_vars, "edu"))
    } 
    
    raked_output_draws <- add_excluded_cause_draws(
      raked_output_draws,
      removed_draws = removed_upper_draws,
      causes_df = causes,
      lower_demographics = lower_add_draws_vars,
      geography_demographics = geography_add_draws_vars,
      geoagg_file = from_settings$settings$geoagg_files[[raking_area_var]]
    )
  }
  
  #------------------------------------
  # SAVE OUTPUTS
  #------------------------------------
  # Save draws and estimates
  for (cause in causes[, acause]) {
    
    make_output_dir(file.path(from_dir, cause), verbose)
    from_settings$save_draws_and_estimates(
      draws = raked_output_draws[acause == cause],
      acause = cause,
      measure = measure,
      year = year,
      sex = sex,
      age = age,
      type = "raking"
    )
  }
  
}

message("FINISHED")

