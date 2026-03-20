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
if (interactive()) {
  # Second rake: by-race to all-race
  from_dir <- "FILEPATH"
  to_dir <- "FILEPATH"
  year <- 2001
  sex <- 1
  measure <- "mx"
  from_geo <- "mcnty"
  to_geo <- "mcnty"
  common_raking_vars <- c("year", "sex", "age", "sim", "mcnty") # equal to the :shared_demographics" in the old scripts
  lower_raking_vars <- c("year", "sex", "age", "sim", "mcnty", "acause") # equal to "lower_demographics" in the old scripts
  upper_raking_vars <- c("year", "sex", "age", "sim", "mcnty") # equal to "geography_demographics" in the old scripts
  
  parent_acause <- "cvd_valvu"

} else {
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
  
  ## PK add
  parser$add_argument("--parent_acause", type = "character", help = "Parent Cause")

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

if ("edu" %in% lower_raking_vars) R.utils::setOption("ushd.use_edu_paths", TRUE) # this should always be true anyway now

# Load the settings from the passed directories.
from_settings <- ModelSettings$from_dir(from_dir)
from_settings$to_geo <- to_geo # This is used to merge population onto draws
if (to_dir == "gbd") {
  to_settings <- list(
    to_geo = to_geo,
    rake_to_gbd_version = NULL,
    ages = from_settings$ages,
    n.sims = from_settings$n.sims,
    area_var = to_geo
  )
  to_settings <- GBDSettings$new(gbd_settings = to_settings)
} else if (identical(normalizePath(from_dir), normalizePath(to_dir)) & "acause" %in% lower_raking_vars) {
  
  message("Your to_dir and from_dir are identical, and acause is in the lower raking draws")
  to_settings <- copy(from_settings) # when the directories are equal, just set this to be equivalent to the settings from the input directory
  
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

by_cause <- "acause" %in% lower_raking_vars

## The below was copied from the flexible 2D raking script
if (by_cause) {
  # This function filters the subset of child causes to those which are not age or sex excluded
  # Raking is then conducted only on those causes for this age-sex combination
  # compile_raked.r then makes the dataset square again by adding the excluded age-sex combinations
  # back as 0.0
  causes <- load_child_cause_metadata(
    from_dir,
    parent_acause = parent_acause,
    sex = sex,
  )
  
  # get rid acause = _all of when acause_parent = _all and load_child_cause_metadata() returns both the level 0 and 1 causes
  if(nrow(causes) > 1 & "_all" %in% causes$acause) {
    causes <- causes[!(parent_id == 0 & acause == "_all")]
  }

  # Flag for triggering additional manipulations if a subset of child causes are modeled by USHD
  # This is different than the age-sex exclusions handled above.
  not_all_child_causes_modeled <- nrow(causes[run_model == 0]) > 0
} else {
  causes = data.table(
    'acause' = if (identical("yld", measure)) "all_cause" else '_all',
    'run_model' = 1,
    'path_to_top_parent' = 294
  )
  not_all_child_causes_modeled <- FALSE
}


lower_draws <- from_settings$load_draws(
  acause = causes[run_model == 1]$acause,
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

upper_cause <- if (by_cause) parent_acause else causes$acause

upper_draws <- to_settings$load_draws(
  acause = upper_cause,
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
    stop_or_quit(sprintf("Common raking dimension %s has different values in upper and lower draws!", var), status = 3)
  }
}

# Merge
setnames(upper_draws, "value", paste0(to_settings$area_var, "_value")) # Rename upper geography value to what rake() will expect
draws <- merge.flexible(lower_draws, upper_draws)
rm(lower_draws)
gc()
#------------------------------------
# RAKE, AND VALIDATE
#------------------------------------


if(not_all_child_causes_modeled) {
  
  # Pull the GBD draws
  
  if("mater_neonat" %in% causes[run_model == 0, acause]) stop("mater_neonat should be aggregated from USHD models, not adjusted as a missing cause")
  
  print("CALCULATING IF THERE IS EXCESS MORTALITY THAT NEEDS TO BE ACCOUNTED FOR")
  
  #missing_cause_calc <- copy(draws)
  # Sum across child causes and then get an average
  draws[, child_sum := sum(value), by = upper_raking_vars]
  avg_vars <- upper_raking_vars[!(upper_raking_vars == "sim")]
  draws[, child_total := mean(child_sum), by = avg_vars]
  
  # We also want to get the average parent cause value
  draws[, paste0(to_settings$area_var, "_mean") := mean(get(paste0(to_settings$area_var, "_value"))), by=avg_vars]
  
  # Now, we need to determine which ages are relevant for adjusting, because if the missing causes do not cover those ages then we
  # don't want to adjust the parent cause
  ages_to_adjust <- c()
  for(ag in unique(causes[run_model == 0, ages])) {
    ages_to_adjust <- c(ages_to_adjust, unique(eval(parse(text = ag))))
  }
  
  print(paste0("The relevant ages for adjustment of missing causes are: ",paste(ages_to_adjust, collapse=", ")))
  
  # Calculate the gap between the mean value in the upper draws and the mean sum of the values in the lower draws
  draws[, excess_mx := get(paste0(to_settings$area_var, "_mean")) - child_total]
  message(paste0(round(nrow(draws[excess_mx < 0 & age %in% ages_to_adjust])/nrow(draws[age %in% ages_to_adjust])*100, 2), "% of rows have negative excess mortality. Setting these to 0"))
  draws[excess_mx < 0 & age %in% ages_to_adjust, excess_mx := 0]
  draws[!(age %in% ages_to_adjust), excess_mx := 0] # if we are dealing with excluded ages, set the excess mortality to 0
  
  # Now, adjust the upper draws so that we adjust mortality using a cause fraction of excess mortality divided by the average parent value
  # and multiply this cause fraction by every draw of the upper draws
  draws[, cf := excess_mx/get(paste0(to_settings$area_var, "_mean"))]
  # If the cause fraction is NaN, that means the parent cause is 0 and thus it is a restricted ages
  # But, let's test to make sure that is the case before proceeding
  
  if(nrow(draws[is.nan(cf)]) > 0) {
    message("Detecting NaN cause fractions. Checking if the parent cause has restricted ages")
    upper_draws_sum <- upper_draws[,list(summed = sum(get(paste0(to_settings$area_var, "_value")))), by='age']
    zero_ages <- upper_draws_sum[summed == 0]
    if(nrow(zero_ages) == 0) stop("Detected NaN cause fractions, but there are not restricted ages in the parent cause")
    
    draws[age %in% zero_ages$age, cf := 0]
    
    stopifnot(nrow(draws[is.nan(cf)]) == 0)
    
  }
  
  draws[, missing_mx := get(paste0(to_settings$area_var, "_value")) * cf]
  draws[, paste0(to_settings$area_var, "_value") := get(paste0(to_settings$area_var, "_value")) * (1-cf)]
  stopifnot(nrow(draws[abs(cf - 1) < 0.00001]) == 0) # if the cause fraction is 1, this will cause issues in raking

  ## Save the excess mortality as draws across the missing causes
  # This makes the rest of the pipeline work because the code assumes that you want to compile all causes, regardless of if you ran them or not
  collapse_string <- paste(c(upper_raking_vars,"race","edu"), collapse = ",")
  missing_draws <- unique(draws[,list(value = missing_mx),
                         by=collapse_string])
  
  # Merge on the causes first, then we will iterate through this and make sure that the age restrictions are applied correctly
  ## Will everything add up still once we given the missing causes these average differences? Well, we will have added some deaths to the child causes, then these get raked down,
  # or up, and then I think the mean raked values will add up?
  missing_cause_dt <- data.table(acause = causes[run_model == 0, acause], indic = 1)
  missing_draws[,indic := 1]
  missing_draws <- merge(missing_draws, missing_cause_dt, by="indic", allow.cartesian=T)
  
  # Now apply the age restrictions
  for(cc in causes[run_model == 0, acause]) {
    missing_draws[acause == cc & !(age %in% eval(parse(text = causes[acause == cc, ages]))), 
                  c("value","indic") := 0] # set both the value and indicator to 0
  }

  # Now count the number of missing draws so that we know if we need to divide by a value
  missing_draws[,total_missing := sum(indic), by=upper_raking_vars]
  # If none of the missing causes cover this age, then just keep the value at 0
  # Otherwise, divide by the number of missing causes so that we know how to split the draws across causes
  missing_draws[,value := ifelse(total_missing == 0, value, value/total_missing)]
  
  # Validate what we just did
  stopifnot(nrow(missing_draws[total_missing == 0 & value != 0]) == 0)
  
  missing_draws[,c("total_missing","indic") := NULL]
  
  # Now get rid of these extra columns
  draws <- draws[,c("child_total","excess_mx", paste0(to_settings$area_var, "_mean"), "cf", "child_sum", "missing_mx") := NULL]
  
}

rm(upper_draws)
gc()

# We should make this consistent - i.e., decide if we include all potential columns with aggregate value, or no.
if (!"race" %in% names(population)) population[, race := race_default] # defined in functions/constants.r


if(!by_cause) {
  draws <- add.population.weights(draws, population, demographics = upper_raking_vars, weight_var = "pop_weight", from_geo)
  
  # call rake(), just once (one-dimensional raking only requires 1 iteration)
  # Implementation notes -
  # The rake() function expects {agg_var}_value to always be in the correct space.
  # Where correct space is defined as:
  #   * count space iff weight_pops == TRUE (default)
  #   * rate space iff weight_pops == FALSE (only when explicitly passed as weight_pops = F)
  rake(draws, agg_var = to_geo, constant_vars = common_raking_vars, replace_value = T)
} else if(by_cause & to_geo == from_geo) {
  
  # population weighting does not make sense for raking across causes within the same mcnty/sim/race
  
  rake(draws, agg_var = to_geo, constant_vars = common_raking_vars, replace_value = T, weight_pops = F)
  
}


# check that the raked results do add up properly
validate_rake(
  draws,
  agg_var = to_geo,
  constant_vars = common_raking_vars,
  weight_pops = if (by_cause & to_geo == from_geo) F else T,
  err_msg = sprintf("Discrepancy in raking %s to %s!", from_geo, to_geo)
)


if(not_all_child_causes_modeled) {
  message("Adding on missing draws")
  # Add some missing columns to missing_draws that will be the same across causes
  missing_draws <- merge(missing_draws, 
                         unique(draws[,.(age,year,sex,sim,race,edu,mcnty,
                                         pop, wt, mcnty_value, acause_target)]),
                         by=c("age","year","sex","sim","race","edu","mcnty"),
                         all=T)
  
  stopifnot(nrow(missing_draws[is.na(value)]) == 0)
  stopifnot(nrow(missing_draws[is.na(mcnty_value)]) == 0)
  
  stopifnot(length(setdiff(names(missing_draws), names(draws))) == 0)
  stopifnot(length(setdiff(names(draws), names(missing_draws))) == 0)
  
  
  draws <- rbind(draws, 
                 missing_draws)
  
}

#------------------------------------
# SAVE OUTPUTS
#------------------------------------
if(by_cause) {
  
  for (cause in causes[, acause]) {
    
    make_output_dir(file.path(from_dir, cause), verbose)
    
    # save both age specific and total
    for(a in c(to_settings$ages)) {
      
      message(a)
      
      from_settings$save_draws_and_estimates(
        draws = draws[acause == cause],
        acause = cause,
        measure = measure,
        year = year,
        sex = sex,
        age = a,
        type = "raking"
      )
      
    }
    
  }
} else {
  draws[, acause := cause_name]
  from_settings$save_draws_and_estimates(
    draws = draws,
    acause = cause_name,
    measure = measure,
    year = year,
    sex = sex,
    type = 'raking'
  )
}


message("FINISHED")
