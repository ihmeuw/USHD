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
## Requires:    prepped data file ("[limited_use_dir]/data.rds") from limited use directory
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
args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]
sex <- as.integer(args[2])
race <- as.integer(args[3])
edu <- as.integer(args[4])
validate <- as.logical(args[5])
resub <- as.logical(args[6])

get_settings(dir)

current_dir <- gsub("FILEPATH", "", dir)
lu_root <- file.path("FILEPATH", LU_folder, "FILEPATH")
lu_modeldir <- file.path(lu_root, current_dir)

## read in array arguments
## get the draw value
draw_args <- fread(paste0(dir, "/draw_args_sex_", sex, "_", race, "_", edu, ".csv"))
task_id <- ifelse(is.na(as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))), 1, as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID")))
draw_val <- c(draw_args[task_id, start_draw]:draw_args[task_id, end_draw])

# print out all the variables that control the behavior of this script
# draw_val and task_id are not passed in as arguments but do control behavior
# This provides the information that distinguishes each array job in the job-output
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
    .sep = "\n"
  )
)

# other settings
cat(
  glue::glue(
    "Other settings:",
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

# get the model fit
out <- readRDS(file.path(dir, paste0("model_fit_", sex, "_", race, "_", edu, ".rds")))  # there is a separate file for each race and edu

# subset the data
if(!("sex_fit" %in% names(data))) {
  data[,sex_fit := sex]
}

data <- data[sex_fit == get("sex", .GlobalEnv) & race_fit == get("race", .GlobalEnv) & edu_fit == get("edu", .GlobalEnv), ]

# Specify dimensions of variables
races_to_model <- unique(data$race)
num_r <- length(races_to_model)
edus_to_model <- unique(data$edu)
num_e <- length(edus_to_model)

stopifnot(num_e == length(unique(data$edu)))
stopifnot(num_r == length(unique(data$race)))

# get number of values for the rest of the variables
num_j <- max(data$area) + 1 # number of areas
num_t <- max(data$year) + 1 # number of years
num_a <- max(data$age) + 1 # number of ages
num_s <- length(unique(data$sex)) # sex was not indexed because this is just something we are trying

# the simvars are not actually the draw vals because when the matrices get subset the column numbers change
simvars <- paste0("V", 1:length(draw_val))

# If a validation model, subset the data so that predictions are generated only for the validation
# set However, keep all race/ethnicities in the counties that are in the validation set. That is,
# keep all the counties in the validation set.
if (validate) {
  gs_mx <- readRDS(gs_file)
  gs_mx <- gs_mx[year %in% years, ]
  # drop race and edu because we want to keep all races in counties that have at least one race in
  # the validation set
  gs_mx[, `:=`(race = NULL, edu = NULL)]
  # get rid of duplicates in the counties that had more than one race
  gs <- unique(gs_mx[sex == get("sex", .GlobalEnv),
                     list(area = get(area_var), year = year - years[1])])
  data <- merge(gs, data, by = c("area", "year"), all.x = T)
  stopifnot(!any(is.na(data)))
  setkeyv(data, c("area", "year", "sex"))
  rm(gs, gs_mx)
  gc()
}


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
if(!is.null(age_knots_spec)) {
  age_knots <- which(ages %in% age_knots_spec) - 1
  s_age <- as.data.table(bs((1:num_a) - 1, knots = age_knots[1:(length(age_knots) - 1)], degree = 1, intercept = F))
}

if(!is.null(year_knots_num)) {
  year_knots <- seq(1, length(years), length.out = year_knots_num) - 1
  s_year <- as.data.table(bs((1:num_t) - 1, knots = year_knots[1:(length(year_knots) - 1)], degree = 1, intercept = F))
}


if (model %in% c("spline_iid_race_all_one_variance",
                 "spline_iid_race_one_var_flexible_re_covs",
                 "spline_iid_year_age_indicator",
                 "spline_iid_race_one_indic_flexible_re_covs",
                 "spline_iid_year_two_indicators",
                 "spline_iid_race_two_indics_flexible_re_covs")) {

  ## 4-way interaction
  re1 <- data.table(as.matrix(sims[names(mean) == "re1", ]))

  # get the combinations of age/time spline bases, areas, and races and edus
  combos <- as.data.table(expand.grid(y_spline = seq(1, ncol(s_year)), a_spline = seq(1, ncol(s_age)), area = seq(0, num_j-1L),
                                      race = races_to_model, edu = edus_to_model))

  stopifnot(nrow(re1) == nrow(combos))

  re1 <- cbind(re1, combos)

  s_age[, age := .I-1]
  s_year[, year := .I-1]
  s_year <- melt.data.table(s_year, id.vars="year", value.name = "y_spline_value", variable.name = "y_spline")
  s_age <- melt.data.table(s_age, id.vars="age", value.name = "a_spline_value", variable.name = "a_spline")

  s_year[, y_spline := as.integer(as.character(y_spline))]
  s_age[, a_spline := as.integer(as.character(a_spline))]

  re1 <- merge(re1, s_year, by="y_spline", allow.cartesian=T)
  re1 <- merge(re1, s_age, by="a_spline", allow.cartesian=T)

  # simvars
  re1<-re1[, (simvars) := lapply(.SD, function(x) sum(x*y_spline_value*a_spline_value)),
           .SDcols=simvars, by=c("area", "race", "edu", "age", "year")]
  del_vars <- c("y_spline", "y_spline_value", "a_spline", "a_spline_value")
  re1 <- re1[, (del_vars) := NULL]
  re1 <- unique(re1)
  setkeyv(re1, c("year", "age", "area", "race", "edu"))

  re <- as.matrix(re1[data[, list(year, age, area, race, edu)], simvars, with = F])

  if (any(is.na(re))) {stop("re1 has NA values")}

  ## Area effect
  re2 <- data.table(as.matrix(sims[names(mean) == "re2", ]))
  re2[, area := 1:num_j - 1L]
  setkeyv(re2, "area")

  if (any(is.na(re2))) {stop("re2 has NA values")}

  ## Sum the initial random effects (re1 and re2) by adding re2 into re
  re <- re + as.matrix(re2[data[, list(area)], simvars, with = F])

  if (any(is.na(re))) {stop("after adding re2, re has NA values")}

  ### Now, read in the 3rd random effect
  re3 <- data.table(as.matrix(sims[names(mean) == "re3", ]))

  combos <- as.data.table(expand.grid(year = as.integer(as.factor(years))-1,
                                      age = as.integer(as.factor(ages))-1,
                                      race = races_to_model,
                                      edu = edus_to_model))
  
  stopifnot(nrow(re3) == nrow(combos))
  re3 <- cbind(re3, combos)
  
  if (any(is.na(re3))) {stop("re3 has NA values")}

  setkeyv(re3, c("year", "race", "edu", "age"))
  
  # Add all random effects together (re1 and re2 already summed earlier)
  re <- re +
    as.matrix(re3[data[, list(year, race, edu, age)], simvars, with = F])
  
  if (any(is.na(re))) {stop("after adding re3, re has NA values")}
  

  if (model %like% "flexible") {


    message(glue::glue("Working on sub-population covariate effects for model {model}..."))

    covar_subpop_random_effects <- data.table(as.matrix(sims[names(mean) == "covar_subpop_re_matrix", ]))
    if (nrow(covar_subpop_random_effects) == 0) {
      stop("covar_subpop_random_effects has 0 rows: nothing named covar_subpop_re_matrix in mean.")
    }

    # add in the effect of the race/ethnicity or edu specific covariates by iterating over each of the race/ethnicity or edu specific covariates
    for (i in c(1:length(covars_subpop))) {
      message(glue::glue("Working on covariate number {i}: {covars_subpop[i]}"))

      chunk_size <- nrow(covar_subpop_random_effects) / length(covars_subpop)
      message(glue::glue("chunk_size = {chunk_size}"))

      # the row/index that marks the end of the current covariates chunk of covar_subpop_re_matrix
      end_row <- chunk_size * i

      # create an index that identifies the correct rows of covar_subpop_re_matrix. Add 1 at the start so that the index begins at 1, because row numbers in data.table begins at 1.
      begin_row <- end_row - chunk_size + 1
      message(glue::glue("begin_row is {begin_row}"))
      message(glue::glue("end_row is {end_row}"))

      covar_subpop_random_effect_rows <- c((begin_row):(end_row))
      message(glue::glue("covar_subpop_random_effect_rows is {paste(covar_subpop_random_effect_rows, collapse=',')}"))

      # check that the number of rows is correct
      if (length(covar_subpop_random_effect_rows) != chunk_size) {
        stop(glue::glue("covar_subpop_random_effect_rows should be of length {chunk_size} but instead it has length {length(covar_subpop_random_effect_rows)}"))
      }

      # subset / pull out the rows that are for this covariate
      covar_subpop_random_effect <- covar_subpop_random_effects[covar_subpop_random_effect_rows, ]

      # check that the number of rows is correct
      if (nrow(covar_subpop_random_effect) != chunk_size) {
        stop(glue::glue("covar_subpop_random_effect should be of length {chunk_size} but instead it has {nrow(covar_subpop_random_effect)} rows"))
      }

      # include race and edu so that it can be merged onto the data
      covar_subpop_random_effect[, race := races_to_model]
      covar_subpop_random_effect[, edu := edus_to_model]
      setkeyv(covar_subpop_random_effect, c("race", "edu"))

      # pull out the current covariate
      covar_random_effect <- data[, get(covars_subpop[i])]

      message("adding to re...")
      re <- re + (covar_random_effect * covar_subpop_random_effect[data[, list(race, edu)], simvars, with = F])
      if (any(is.na(re))) {stop("after adding covar_subpop_random_effect, re has NA values")}
      message(glue::glue("Done with loop {i}."))
    } 

    message(glue::glue("Done with sub-population covariate effects for model {model}"))
  }
  
  #### Indicator models

  if (model %in% c("spline_iid_year_age_indicator",
                   "spline_iid_race_one_indic_flexible_re_covs")) {

    re4 <- data.table(as.matrix(sims[names(mean) == "re4", ]))

    re4[, area := 1:num_j - 1L]
    setkeyv(re4, "area")

    if (any(is.na(re4))) {stop("re4 has NA values")}
    
    # and the age effect
    re5 <- data.table(as.matrix(sims[names(mean) == "re5", ]))
    
    re5[, age := 1:num_a - 1L]
    setkeyv(re5, "age")
    
    if (any(is.na(re5))) {stop("re5 has NA values")}
    
    re <- re + data$indic * as.matrix(re4[data[, list(area)], simvars, with = F]) +
      data$indic * as.matrix(re5[data[, list(age)], simvars, with = F])

  }

  
  if(model %in% c("spline_iid_year_two_indicators",
                  "spline_iid_race_two_indics_flexible_re_covs")) {
    
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
    
  }

} 

## calculate mx and combine with identifying information -------------------------------------------

draws <- fe + re

rm(fe, re); gc()

draws <- data.table(exp(draws))
draws[, area := as.integer(data$area)]
draws[, sex := as.integer(data$sex)] 

# Decoding year and age
draws[, year := as.integer(data$year + years[1])]
draws[, age := as.integer(ages[1 + data$age])]


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

# now that all filtering is done, aggregate pop. We'll aggregate the same way regardless of the values
# of race_together or edu_together.
pop <- pop[, list(pop = sum(pop)), by = c(area_var, "year", "age", "race", "edu", "sex")] # sex added back into the merge so that sex_together models will work

# if race is a factor (because of processing), convert to integer
if(is.factor(pop$race)) pop[,race := as.integer(as.character(race))]

setnames(pop, area_var, "area")
# set merge variables for later
draws <- merge(draws, pop, by = c("area", "year", "age", "edu", "race", "sex"), all = TRUE)  


# If a validation model, subset the data so that predictions are generated only for the validation
# set However, keep all race/ethnicities in the counties that are in the validation set. That is,
# keep all the counties in the validation set.
if (validate) {
  gs_mx <- readRDS(gs_file)
  gs_mx <- gs_mx[year %in% years, ]
  # drop race and edu because we want to keep all races in counties that have at least one race in
  # the validation set
  gs_mx[, `:=`(race = NULL, edu = NULL)]
  # get rid of duplicates in the counties that had more than one race
  gs <- unique(gs_mx[sex == get("sex", .GlobalEnv),
                     list(area = get(area_var), year)])
  draws <- merge(gs, draws, by = c("area", "year"), all.x = T)
  stopifnot(!any(is.na(draws)))
  setkeyv(draws, c("area", "year"))
  rm(gs, gs_mx)
  gc()
}


replace_na_func <- function(x) ifelse(is.na(x), 0, x)
draws[!(age %in% ages), (simvars) := lapply(.SD, replace_na_func), .SDcols = simvars]

# No missing values are expected
stopifnot(!any(is.na(draws)))

## Set draws for particular ages to 0 for some drug use causes
set_draws_0 <- function(x) 0

# Set draws to 0 in certain ages for some drug use disorders causes
if("cause_id" %in% ls(envir = .GlobalEnv)) {
  if(!is.null(cause_id)) {
    if(cause_id %in% c(561, 562)) {
      draws[age %in% c(1,5,10), (simvars) := lapply(.SD, set_draws_0), .SDcols = simvars]
    }
  }
}

# load the age standard
std_wt <- readRDS(age_std_file)[, list(age, wt = wt / sum(wt))]

# split up data and process separately by year
all_draws <- draws
rm(draws)

# Read in misclassification draws
if ("misclassification_draws.rds" %in% list.files(dir, full.names = F)) {
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

    if ("misclassification_draws.rds" %in% list.files(dir, full.names = F)) {

      mc_merge_vars <- c("sim", "sex", "race", "edu", "age", "area") # initialize / reset
      cols_intersect <- intersect(names(draws), names(mc))
      mc_merge_vars <- intersect(mc_merge_vars, cols_intersect)
      message(glue::glue("Merge columns for misclassification are {paste(mc_merge_vars, collapse=', ')}."))


      draws <- merge(draws, mc, by = mc_merge_vars, all.x = T)
      if (nrow(draws[is.na(ratio)]) > 0) stop("Classification ratios did not merge on correctly")

      draws_temp <- copy(draws)[, adjusted := 0]
      draws <- rbind(draws_temp, draws[, c("mx", "adjusted") := list(mx*ratio, 1)])
      draws[, ratio := NULL]
      all_age_by_vars <- c(all_age_by_vars, "adjusted")
      
    } else if (by_race) {

      draws[,adjusted := 1] # if by-race but not using a misclassification adjustment, just set the adjusted column to 1
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
