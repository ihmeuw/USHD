####################################################################################################
## Description: For memory reasons, we have to run raking by age and save a cause-age-sex-year-
##              specific file. Compile these into cause-sex-year-specific files, add all ages
##              and age-standardized rates, collapse, and save draws and estimates. The output
##              of this script mimics pred_mx.r.
##
## Inputs:      dir [character] -- the directory for all models
##              year [numeric] -- year to be compiled
##              sex [numeric] -- sex to be compiled
##              race [numeric] -- race to be compiled
##              cause [character] -- acause code for the cause to be compiled
##              measure [character] -- which measure to rake, "mx" for mortality rates, "yll" for
##                  ylls
##
####################################################################################################

library(lbd.loader, lib.loc = sprintf("FILEPATH", R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
suppressMessages(lbd.loader::load.containing.package())

## Get settings ------------------------------------------------------------------------------------
if (interactive()) {
  # original data, but lets you save your aggregations in-place without issue
  # cp -as $PWD/2020_08_18_level1_race 2020_08_18_level1_race-$USER

  dir <- "FILEPATH"
  year <- 2019
  sex <- 2
  acause <- "mater_neonat"

  measure <- "mx"
  testing <- TRUE
} else {
  parser <- argparse::ArgumentParser()
  add_dir_argument(parser)
  add_year_argument(parser)
  add_sex_argument(parser)
  parser$add_argument("acause")
  parser$add_argument("measure")

  args <- get_args()
  print(args)
  args <- parser$parse_args(args)
  print(args)

  dir <- args$dir
  year <- args$year
  sex <- args$sex
  acause <- args$acause
  measure <- args$measure
  testing <- TRUE
}

# Load the top-level settings from the passed directory.
from_settings <- ModelSettings$from_dir(dir)

from_settings$settings_for_cause(acause)$update_env(.GlobalEnv)

if (acause == "mater_neonat"){
  if (sex == 1) { # Males
    ages = c(0, 1) # Do 5 and 10 go in here?? I don't think so? We don't have results for these ages.
  } else { # Females
    ages = c(0, 1, 10, 15, 20, 25, 30, 35, 40, 45, 50)
  }
}

demographics <- c("level", "area", "year", "sex", "race", "edu", "age", "acause", "sim")

load_age_draws <- function(age) { from_settings$load_draws(age = age,
                                                           year = year,
                                                           sex = sex,
                                                           raked = "raked_temp",
                                                           acause = acause,
                                                           edu_levels = c(edu_default),
                                                           measure = measure) }
draws <- lapply(
  ages,
  load_age_draws
)
draws <- rbindlist(draws)

# Convert to level and area columns, reset value to measure
draws[, level := area_var]
setnames(draws, area_var, "area")
setnames(draws, "value", measure)

## Impute zeros for ages that were excluded, if any ------------------------------------------------
# get all ages, which are saved like "c(0, 1, 5, 10, 15, 20, ..., 80, 85)"
settings_table <- as.data.table(read.csv(file.path(dir, "settings.csv"), header = F,  stringsAsFactors = F))
all_ages <- eval(parse(text = settings_table[V1 == "ages", V2]))

# Additionally, get raking_area_var from top level settings, if it doesn't already exist. This
# covers the case where this setting was added to top level after cause-specific settings were
# made, and raking_area_var wouldn't be in cause-specific settings.
if (is.null(raking_area_var)) {
  raking_area_var <- settings_table[V1 == "raking_area_var", V2]
}

# if there is a difference between all_ages and ages, then make the square data.table and impute zeros in
# restricted ages
if (length(setdiff(all_ages, ages)) > 0) {
  # make a square set of data with all combinations of age, sim, location, race
  square_data <- data.table()[, CJ(
    level = unique(draws$level),
    area = unique(draws$area),
    year = year,
    sex = sex,
    race = races,
    edu = edu_groups,
    age = as.integer(all_ages),
    acause = acause,
    sim = unique(draws$sim)
  )]

  # the following code will fill NAs, so there shouldn't be any before this merge
  stopifnot(!(any(is.na(draws))))

  # merge these tables together, so that rows are made for excluded ages
  draws <- merge(square_data, draws, by = demographics, all.x = T)

  # fill in zeros for excluded ages
  draws[is.na(get(measure)) & !(age %in% ages), (measure) := 0]
  # need to attach pop to the imputed rows
  draws[, pop := NULL]
}

if (! "pop" %in% colnames(draws)) {
  
  population <- load_population_metadata(
    weights_file = geoagg_files[[raking_area_var]],
    pop_file = pop_file,
    area_var = area_var,
    raking_area_var = raking_area_var,
    year = year,
    by_race = by_race
  )

  population <- population[sex == get("sex", parent.frame())]
  population[, level := area_var]
  population[, (c("wt", raking_area_var)) := NULL]
  setnames(population, area_var, "area")
  
  if(!("race" %in% names(population))) population[,race := race_default]
  if(!("edu" %in% names(population))) population[,edu := edu_default]
  
  # merge on intersect to account for possibility of race/education being present in pop file.
  draws <- merge(draws, population, by = demographics[!demographics %in% c("sim", "acause")], all.x = T)
  stopifnot("pop" %in% colnames(draws))

  # check that the objective of this section is complete: every age group should be present
  stopifnot(length(setdiff(all_ages, unique(draws$age))) == 0)
}


nas <- sum(is.na(draws[, list(measure, level, area, year, sex, race, edu, age)]))
if (nas) {
  stop(sprintf("Unexpected Nulls present: %i of %i rows total", sum(nas), nrow(draws)))
}

## Calculate all-ages, age-standardized draws, collapse draws and save -----------------------------
# load the age standard
std_wt <- from_settings$load_std_wt()
draws <- calc_all_ages(draws, std_wt, measure, by_vars = demographics[!demographics %in% c("age")], allow_missing_ages = from_settings$by_edu)
# calculate point estimates and CIs
est <- collapse_draws(draws, measure, id_vars = demographics[!demographics %in% c("sim")])

est[, acause := NULL]

# Check to make sure that both estimates and draws have all-age categories
if (!all(c('98', '99') %in% unique(draws$age))){
  lsae.utils::stop_or_quit("Draws do not have aggregate age categories in compile_raked!", status=14)
}
if (!all(c('98', '99') %in% unique(est$age))){
  lsae.utils::stop_or_quit("Estimates do not have aggregate age categories in compile_raked!", status=14)
}
# save draws and estimates
save.dir <- get_cause_dir(dir, acause)
for (race in races) {
  saveRDS(draws[race == get("race", parent.frame()), ],
          file = cause_mx_draws_path(dir, acause = acause, measure = measure, type = "draws",
                                     area_var = area_var, year = year, sex = sex,
                                     edu = edu_default,
                                     race = race, raked = "raked"),
          compress = T)

  saveRDS(est[race == get("race", parent.frame()), ],
          file = cause_mx_draws_path(dir, acause = acause, measure = measure, type = "est",
                                     area_var = area_var, year = year, sex = sex,
                                     race = race, edu = edu_default, raked = "raked"),
          compress = T)
}
