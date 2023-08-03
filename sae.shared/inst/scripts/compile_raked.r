####################################################################################################
## Description: For memory reasons, we have to run raking by age and save a cause-age-sex-year-
##              specific file. Compile these into cause-sex-year-specific files, add all ages
##              and age-standardized rates, collapse, and save draws and estimates.
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

# Load the top-level settings from the passed directory.
from_settings <- ModelSettings$from_dir(dir)
from_settings$settings_for_cause(acause)$update_env(.GlobalEnv)

demographics <- c("level", "area", "year", "sex", if (by_race) "race", if (by_edu) "edu", "age", "acause", "sim")

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

draws[, level := area_var]
setnames(draws, area_var, "area")
setnames(draws, "value", measure)

## Impute zeros for ages that were excluded, if any ------------------------------------------------
settings_table <- as.data.table(read.csv(file.path(dir, "settings.csv"), header = F,  stringsAsFactors = F))
all_ages <- eval(parse(text = settings_table[V1 == "ages", V2]))

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
    age = as.integer(all_ages),
    acause = acause,
    sim = unique(draws$sim)
  )]
  
  stopifnot(!(any(is.na(draws))))
  draws <- merge(square_data, draws, by = demographics, all.x = T)
  
  # fill in zeros for excluded ages
  draws[is.na(get(measure)) & !(age %in% ages), (measure) := 0]
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
  
  # merge on intersect to account for possibility of race/education being present in pop file.
  draws <- merge(draws, population, by = demographics[!demographics %in% c("sim", "acause")], all.x = T)
  stopifnot("pop" %in% colnames(draws))
  
  # check that the objective of this section is complete: every age group should be present
  stopifnot(length(setdiff(all_ages, unique(draws$age))) == 0)
}

if (! "race" %in% colnames(draws)) {
  draws[, race := race_default]
}

nas <- sum(is.na(draws[, list(measure, level, area, year, sex, race, age)]))
if (nas) {
  stop(sprintf("Unexpected Nulls present: %i of %i rows total", sum(nas), nrow(draws)))
}

## Calculate all-ages, age-standardized draws, collapse draws and save -----------------------------
std_wt <- readRDS(age_std_file)[, list(age, wt = wt / sum(wt))]
if ('race' %in% names(draws)){
  draws <- calc_all_ages(draws, std_wt, measure, by_vars = demographics[!demographics %in% c("age")])
  est <- collapse_draws(draws, measure, id_vars = demographics[!demographics %in% c("sim")])
} else {
  draws <- calc_all_ages(draws, std_wt, measure, by_vars = demographics[!demographics %in% c("age", "race")])
  est <- collapse_draws(draws, measure, id_vars = demographics[!demographics %in% c("sim", "race")])
}


est[, acause := NULL]

if (!all(c('98', '99') %in% unique(draws$age))){
  stop_or_quit("Draws do not have aggregate age categories in compile_raked!", status=14)
}
if (!all(c('98', '99') %in% unique(est$age))){
  stop_or_quit("Estimates do not have aggregate age categories in compile_raked!", status=14)
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