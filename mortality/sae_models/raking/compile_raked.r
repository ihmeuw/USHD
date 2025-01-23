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

stopifnot(grepl("mortality/sae_models$", getwd()))

suppressMessages(suppressWarnings({
  library(R.utils)
  library(data.table)
  sourceDirectory("functions/", modifiedOnly = FALSE)
}))

## Get settings ------------------------------------------------------------------------------------
if (interactive()) {

  dir <- "FILEPATH"
  year <- 2009
  sex <- 2
  acause <- "neo_lung"
  measure <- "mx"
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
}


# cause-agnostic settings. One reason for this is files from rake_by_geography_and_cause.r are age
# specific, and there won't be files for restricted ages.
data_dir <- get_cause_dir(dir, acause)
get_settings(data_dir)


demographics <- c("level", "area", "year", "sex", if (by_race) "race", "age", "acause", "sim")

## Load and combine draws for all ages -------------------------------------------------------------
# load the "area_var"-level mx draws at race/ethnicity level
draws <- lapply(
  ages,
  read_county_race_draws,
  dir = dir,
  area_var = area_var,
  year = year,
  sex = sex,
  raked = "raked_temp",
  acause = acause,
  race = "all",
  measure = measure
)
draws <- rbindlist(draws)

# Convert to level and area columns
draws[, level := area_var]
setnames(draws, area_var, "area")

## Impute zeros for ages that were excluded, if any ------------------------------------------------
# get all ages, which are saved like "c(0, 1, 5, 10, 15, 20, ..., 80, 85)"

# cause-agnostic. data_dir is cause-specific. as such, the ages read from settings in dir will be
# every possible age, while the ages in data_dir could be age restricted.
settings_table <- as.data.table(read.csv(file.path(dir, "settings.csv"), header = F,  stringsAsFactors = F))
all_ages <- eval(parse(text = settings_table[V1 == "ages", V2]))

# Additionally, get raking_area_var from top level settings, if it doesn't already exist. This
# covers the case where this setting was added to top level after cause-specific settings were
# made, and raking_area_var wouldn't be in cause-specific settings.
if (!exists("raking_area_var")) {
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

  # the following code will fill NAs, so there shouldn't be any before this merge
  stopifnot(!(any(is.na(draws))))

  # merge these tables together, so that rows are made for excluded ages
  draws <- merge(square_data, draws, by = demographics, all.x = T)

  # fill in zeros for excluded ages
  draws[is.na(mx) & !(age %in% ages), mx := 0]
  # need to attach pop to the imputed rows
  draws[, pop := NULL]
}

# after filling in mx, and dropping pop, there shouldn't be any NAs
stopifnot(!(any(is.na(draws))))

if (! "pop" %in% colnames(draws)) {
  
  population <- load_population_metadata(
    weights_file = geoagg_files[raking_area_var],
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
  draws[, race := all_pop_id]
}

nas <- sum(is.na(draws[, list(measure, level, area, year, sex, race, age)]))
if (nas) {
  stop(sprintf("Unexpected Nulls present: %i of %i rows total", sum(nas), nrow(draws)))
}

## Calculate all-ages, age-standardized draws, collapse draws and save -----------------------------
# add all-ages rates
std_wt <- readRDS(age_std_file)[, list(age, wt = wt / sum(wt))]
draws <- calc_all_ages(draws, std_wt, measure, by_vars = demographics[demographics != "age"], allow_missing_ages = by_edu)

# calculate point estimates and CIs
est <- collapse_draws(draws, measure, id_vars = demographics[demographics != "sim"])
est[, acause := NULL]

# save draws and estimates
save.dir <- get_cause_dir(dir, acause)
for (race in races) {
  saveRDS(draws[race == get("race", parent.frame()), ],
          file = cause_mx_draws_path(dir, acause = acause, measure = measure, type = "draws", area_var = area_var, year = year, sex = sex, race = race, raked = "raked"),
          compress = T)

  saveRDS(est[race == get("race", parent.frame()), ],
          file = cause_mx_draws_path(dir, acause = acause, measure = measure, type = "est", area_var = area_var, year = year, sex = sex, race = race, raked = "raked"),
          compress = T)
}

# remove temp files
prelim_files <- c()
for (age in ages) {
  for (race in races) {
    prelim_files <- c(prelim_files, cause_mx_draws_path(dir, acause = acause, measure = measure, type = "draws", area_var = area_var, year = year, sex = sex, race = race, age = age, raked = "raked_temp"))
  }
}

file.remove(prelim_files)
message(sprintf("Removing preliminary files:\n\t%s", paste(prelim_files, collapse = "\n\t")))
