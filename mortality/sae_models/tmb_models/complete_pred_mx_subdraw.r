####################################################################################################
## Description: Reads in the model draws, fixed effects, and labels generated by pred_mx and then
##              calculates the mx draws for a particular chunk of draws (read in by the array job).
##              Also calculates the age-standardized and all-age values.
##              Saves this chunk of draws for all ages/locations/races for each year separately.
##
## Passed args: dir [character] -- home directory for settings and final output
##              sex [integer] -- sex to generate predictions for
##              race [integer] -- race to generate predictions for
##              edu [integer] -- educational attainment group to generate predictions for
##              validate [logical] -- is this a validation model? if so, only mx draws for
##                areas in the validation set are created
##              resub [logical] -- are you resubmitting? if yes, it will not re-save files that already
##                exist (or where the final file with all draws exists)
##              * note that the range of subdraws is read in from an array job
##
## Requires:    prepped data file ("FILEPATH/data.rds") from limited use directory
##              file specifying areas in the validation set, if validate is T (gs_file)
##              populations (pop_file)
##              age standard file (age_std_file)
##              draws generated by pred_mx ([dir]/initial_sims_[sex]_[race]_[edu].rds)
##              fixed effects generated by pred_mx ([dir]/fe_sims_[sex]_[race]_[edu].rds)
##              names of the effects used for subsetting the draws ([dir]/mean_[sex]_[race]_[edu].rds)
##
## Outputs:     mx draws and estimates, saved in separate files by year and draw "chunk":
##                "[dir]/mx_draws_[area_var]_[year]_[sex]_[race]_[edu]_[max_subdraw].rds"
##
####################################################################################################

stopifnot(grepl("mortality/sae_models$", getwd()))

library(R.utils)
library(data.table)
library(Matrix)
library(splines)
sourceDirectory("functions/", modifiedOnly=F)

set.seed(98121)

## Get settings ------------------------------------------------------------------------------------
if (interactive()) {
  dir = '[FILEPATH]'
  sex = 2
  race = 99
  edu = 1
  validate = FALSE
  resub = TRUE
} else {
  args <- commandArgs(trailingOnly = TRUE)
  dir <- args[1]
  sex <- as.integer(args[2])
  race <- as.integer(args[3])
  edu <- as.integer(args[4])
  validate <- as.logical(args[5])
  resub <- as.logical(args[6])
}

get_settings(dir)

current_dir <- gsub("[FILEPATH]", "", dir)
lu_root <- file.path("[FILEPATH]", LU_folder, "[FILEPATH]")
lu_modeldir <- file.path(lu_root, current_dir)

## read in array arguments
## get the draw value
if (interactive()) {
  task_id = 1
  draw_val = c(1:50)
} else {
  draw_args <- fread(paste0(dir, "/draw_args_sex_", sex, "_", race, "_", edu, ".csv"))
  task_id <- ifelse(is.na(as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))), 1, as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID")))
  draw_val <- c(draw_args[task_id, start_draw]:draw_args[task_id, end_draw])
}

# print out all the variables that control the behavior of this script
cat(
  glue::glue(
    "Settings passed are:",
    "dir = '{dir}'",
    "sex = {sex}",
    "race = {race}",
    "edu = {edu}",
    "validate = {validate}",
    "resub = {resub}",
    "draw_val = c({min(draw_val)}:{max(draw_val)})",
    "task_id = {task_id}\n",
    "",
    .sep = "\n"
  )
)

# other settings
cat(
  glue::glue(
    "",
    "Other settings:",
    "model: {model}",
    "race_together = {race_together}",
    "edu_together = {edu_together}",
    "by_race = {by_race}",
    "by_edu = {by_edu}",
    "races =  c({paste(races, collapse = ', ')})",
    "edu_groups = c({paste(edu_groups, collapse = ', ')})\n",
    .sep = "\n"
  )
)

## Load data and model fit -------------------------------------------------------------------------
# Data must be read in from Limited Use
data <- readRDS(file.path(lu_modeldir, "data.rds"))

# subset the data
data <- data[sex_fit == get("sex", .GlobalEnv) & race_fit == get("race", .GlobalEnv) & edu_fit == get("edu", .GlobalEnv), ]
loc <- fread("[FILEPATH]")
states <- unique(loc$state)

# variable numbers
races_to_model <- sort(unique(data$race))
num_r <- length(races_to_model)
edus_to_model <- unique(data$edu)
num_e <- length(edus_to_model)

stopifnot(num_e == length(unique(data$edu)))
stopifnot(num_r == length(unique(data$race)))

# get number of values for the rest of the variables
num_j <- max(data$area) + 1 # number of areas
num_t <- max(data$year) + 1 # number of years
num_a <- max(data$age) + 1 # number of ages
num_s <- length(unique(data$sex)) 
indexed_areas <- seq(0, num_j-1L)

simvars <- paste0("V", 1:length(draw_val))

## Break out by draws and predict each one separately
# Random effects
sims <- readRDS(paste0(dir, "/initial_sims_", sex, "_", race, "_", edu, ".rds"))
# Summed fixed effects (intercept and covariates)
fe <- readRDS(paste0(dir, "/fe_sims_", sex, "_", race, "_", edu, ".rds"))

# Tells us which column corresponds to which random effect
mean <- readRDS(paste0(dir, "/mean_", sex, "_", race, "_", edu, ".rds"))

# Subset to the draws of interest
sims <- sims[, draw_val]
fe <- fe[, draw_val]

# Get spline information, if present in settings.csv
if (!is.null(age_knots_spec)) {
  age_spline_info <- build_age_spline(ages, age_knots_spec)
  s_age <- as.data.table(age_spline_info[[1]])
}


if (!is.null(year_knots_num) | !is.null(year_knots_spec)) {
  if(!is.null(year_knots_spec)) {
    time_spline_info <- build_time_spline(years, year_knots_spec = year_knots_spec,
                                          detach_spline_19_21 = detach_spline_19_21,
                                          detach_spline_19_22 = detach_spline_19_22)
  } else {
    time_spline_info <- build_time_spline(years, year_knots_num = year_knots_num,
                                          detach_spline_19_21 = detach_spline_19_21,
                                          detach_spline_19_22 = detach_spline_19_22)
  }

  s_year <- as.data.table(time_spline_info[[1]])
}


##### First deal with the spline, which all of these models have in common
re1 <- data.table(as.matrix(sims[names(mean) == "re1", ]))

# get the combinations of age/time spline bases, areas, and races and edus
combos <- as.data.table(expand.grid(y_spline = seq(1, ncol(s_year)),
                                    a_spline = seq(1, ncol(s_age)),
                                    area = indexed_areas,
                                    race = races_to_model,
                                    edu = edus_to_model))

stopifnot(nrow(re1) == nrow(combos))

re1 <- cbind(re1, combos)

s_age[, age := .I-1]
s_year[, year := .I-1]
s_year <- melt.data.table(s_year, id.vars="year", value.name = "y_spline_value", variable.name = "y_spline")
s_age <- melt.data.table(s_age, id.vars="age", value.name = "a_spline_value", variable.name = "a_spline")

s_year[, y_spline := as.integer(as.character(y_spline))]
s_age[, a_spline := as.integer(as.character(a_spline))]

re1 <- merge(re1, s_year, by="y_spline", allow.cartesian=T)
# remove 0s, since this will be multiplied by the random effect and thus contribute nothing to the total value for that year
re1 <- re1[y_spline_value != 0]
re1 <- merge(re1, s_age, by="a_spline", allow.cartesian=T)
# remove 0s again
re1 <- re1[a_spline_value != 0]

gc() # clears about half of the memory

# simvars
re1<-re1[, (simvars) := lapply(.SD, function(x) sum(x*y_spline_value*a_spline_value)),
         .SDcols=simvars, by=c("area", "race", "edu", "age", "year")]
del_vars <- c("y_spline", "y_spline_value", "a_spline", "a_spline_value")
re1 <- re1[, (del_vars) := NULL]
re1 <- unique(re1)

setkeyv(re1, c("year", "age", "area", "race", "edu"))
re <- as.matrix(re1[data[, list(year, age, area, race, edu)], simvars, with = F])

if (any(is.na(re))) {stop("re1 has NA values")}

re2 <- data.table(as.matrix(sims[names(mean) == "re2", ]))

combos <- as.data.table(expand.grid(age = as.integer(as.factor(ages))-1,
                                    area = indexed_areas,
                                    race = races_to_model))

stopifnot(nrow(re2) == nrow(combos))
re2 <- cbind(re2, combos)

if (any(is.na(re2))) {stop("re2 has NA values")}

setkeyv(re2, c("age", "area", "race"))

# Add all random effects together 
re <- re +
  as.matrix(re2[data[, list(age, area, race)], simvars, with = F])


if (any(is.na(re))) {stop("after adding re2, re has NA values")}

re3 <- data.table(as.matrix(sims[names(mean) == "re3", ]))


combos <- as.data.table(expand.grid(year = as.integer(as.factor(years))-1,
                                    age = as.integer(as.factor(ages))-1,
                                    race = races_to_model,
                                    edu = edus_to_model))

stopifnot(nrow(re3) == nrow(combos))
re3 <- cbind(re3, combos)

if (any(is.na(re3))) {stop("re3 has NA values")}

setkeyv(re3, c("year", "race", "edu", "age"))

# Add all random effects together 
re <- re +
  as.matrix(re3[data[, list(year, race, edu, age)], simvars, with = F])


## Shocks indicators

re4 <- data.table(as.matrix(sims[names(mean) == "re4", ]))
re4[, area := 1:num_j - 1L]
setkeyv(re4, "area")

re <- re + data$indic_1 * as.matrix(re4[data[, list(area)], simvars, with = F])

re5 <- data.table(as.matrix(sims[names(mean) == "re5", ]))
re5[, area := 1:num_j - 1L]
setkeyv(re5, "area")

re <- re + data$indic_2 * as.matrix(re5[data[, list(area)], simvars, with = F])

## now the age effects
re6 <- data.table(as.matrix(sims[names(mean) == "re6", ]))
re6[, age := 1:num_a - 1L]
setkeyv(re6, "age")

re <- re + data$indic_1 * as.matrix(re6[data[, list(age)], simvars, with = F])

re7 <- data.table(as.matrix(sims[names(mean) == "re7", ]))
re7[, age := 1:num_a - 1L]
setkeyv(re7, "age")

re <- re + data$indic_2 * as.matrix(re7[data[, list(age)], simvars, with = F])

if (any(is.na(re))) {stop("after adding effects for shocks indicators, re has NA values")}

## calculate mx and combine with identifying information -------------------------------------------

draws <- fe + re

rm(fe, re); gc()

draws <- data.table(exp(draws))

draws[, area := as.integer(data$area)]
draws[, sex := as.integer(data$sex)] 

# Decoding year and age
draws[, year := as.integer(data$year + years[1])]
draws[, age := as.integer(ages[1 + data$age])]

# Decoding race and edu
if(is.factor(data$race)) {
  draws[, race := as.integer(as.character(data$race))]
} else {
  draws[, race := as.integer(data$race)]
}

draws[, edu := as.integer(data$edu)]

## Split out draws by year, calculate all-ages, age-standardized draws, collapse draws, and save ---
# load and subset the population file, then merge onto draws
pop <- readRDS(pop_file)
if (!by_race) {
  pop[, race := all_pop_id]
}

if (!by_edu) {
  pop[, edu := all_pop_id]
}

# filter pop depending on the values of race_together and edu_together
# in any case, subset pop to the current sex and years
if(!sex_together) {
  pop <- pop[sex == get("sex", .GlobalEnv)]
}

pop <- pop[sex %in% sexes & year %in% get("years", .GlobalEnv), ]
pop <- pop[race %in% unique(data$race) & edu %in% unique(data$edu), ]

if (by_edu & setequal(ages, seq(25, 85, 5))) {
  pop <- pop[age %in% ages, ]
}

# now that all filtering is done, aggregate pop. We'll aggregate the same way regardless of the values
# of race_together or edu_together.
pop <- pop[, list(pop = sum(pop)), by = c(area_var, "year", "age", "race", "edu", "sex")] 

# if race is a factor (because of processing), convert to integer
if(is.factor(pop$race)) pop[,race := as.integer(as.character(race))]
setnames(pop, area_var, "area")

# set merge variables for later
draws <- merge(draws, pop, by = c("area", "year", "age", "edu", "race", "sex"), all = TRUE)

replace_na_func <- function(x) ifelse(is.na(x), 0, x)
draws[!(age %in% ages), (simvars) := lapply(.SD, replace_na_func), .SDcols = simvars]

# No missing values are expected
stopifnot(!any(is.na(draws)))

# load the age standard
std_wt <- readRDS(age_std_file)[, list(age, wt = wt / sum(wt))]

# split up data and process separately by year
all_draws <- draws
rm(draws)


if ("misclassification_draws.rds" %in% list.files(dir, full.names = F)) {
  # assume that the user wants to use the misclassification draws that are all combined
  mc <- readRDS(paste0(dir, "/misclassification_draws.rds"))
}


all_draws <- lapply(years, function(this_year) all_draws[year == this_year, ])
for (this_year in years) {
  ## Check if the final file already exists if resubmission is specified
  if(sex_together) {
    expected_files <- c()
    for(s in sexes) {
      expected_files <- c(expected_files, paste0(dir, "/mx_est_mcnty_", this_year, "_", s, "_", unique(data$race), "_", unique(data$edu), ".rds"))
    }
  } else {
    expected_files <- paste0(dir, "/mx_est_mcnty_", this_year, "_", sex, "_", unique(data$race), "_", unique(data$edu), ".rds")
  }
  
  stopifnot(!(by_race & by_edu))

  if(resub & all(file.exists(expected_files))) {

    all_draws[[which(years == this_year)]] <- NA
    message("Skipping")

  } else {
    cat(paste(Sys.time(), this_year, "\n"))
    draws <- all_draws[[which(years == this_year)]]
    draws[, level := area_var]
    if (nrow(draws) == 0) next # this can happen in validation models

    # reshape long for easier processing
    draws <- melt(draws, id.vars = c("level", "area", "year", "sex", "race", "edu", "age", "pop"), variable.name = "sim", value.name = "mx")

    draws[, sim := as.integer(sim)]
    # re-code the draws to correspond to draw_val
    draws[, sim := sim + (min(draw_val) - 1)]

    # specify the columns by which to summarize/aggregate/etc since now we have both adjusted and unadjusted points
    # this is used for the calc_all_ages function
    all_age_by_vars <- c("level", "area", "year", "sex", "race", "edu", "sim")

    # if applying the misclassication ratios, look for a file saved with these
    if ("misclassification_draws.rds" %in% list.files(dir, full.names = F)) {

      mc_merge_vars <- c("sim", "sex", "race", "edu", "age", "area") # initialize / reset
      cols_intersect <- intersect(names(draws), names(mc))
      mc_merge_vars <- intersect(mc_merge_vars, cols_intersect)
      message(glue::glue("Merge columns for misclassification are {paste(mc_merge_vars, collapse=', ')}."))

      draws <- merge(draws, mc, by = mc_merge_vars, all.x = T)
      if (nrow(draws[is.na(ratio)]) > 0) stop("Classification ratios did not merge on correctly")

      # save the pre-mislcassification adjusted
      draws_temp <- copy(draws)[, adjusted := 0]
      draws <- rbind(draws_temp, draws[, c("mx", "adjusted") := list(mx*ratio, 1)])
      draws[, ratio := NULL]
      all_age_by_vars <- c(all_age_by_vars, "adjusted")
    } else if (by_race) {

      draws[,adjusted := 1]
      all_age_by_vars <- c(all_age_by_vars, "adjusted")

    }

    # add crude and age-standardized rates
    draws <- calc_all_ages(draws, std_wt, "mx", by_vars = all_age_by_vars, allow_missing_ages = by_edu)

    # save draws and estimates
    if(sex_together) {
      sexes_to_model <- sexes
    } else {
      sexes_to_model <- sex
    }

    for(s in sexes_to_model) {
      for (r in races_to_model) {
        for(e in edus_to_model) {
          message(paste0("Working on race ", r))
          message(paste0("Working on edu ", e))
          message(paste0("Working on sex ", s))

          saveRDS(draws[race == r & edu == e & sex == s], file = paste0(dir, "/mx_draws_", area_var, "_", this_year, "_", s, "_",
                                                                        r, "_", e, "_",
                                                                        max(draw_val), ".rds"))
        }

      }
    }


    all_draws[[which(years == this_year)]] <- NA
    rm(draws); gc()
  }
}

# If there are warnings, display them:
warnings()

message("DONE.")
