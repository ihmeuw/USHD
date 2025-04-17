####################################################################################################
## Description: Prep inputs for YLD models to estimate all-cause YLDs.
##              Recode age groups and years to run sequentially from zero. Also prep CAR structure
##              matrices for spatial, temporal, and age effects.
##              If performing cross -validation (cross_val in settings file), then creates folds
##
## Passed args: repo [character] -- location of nonfatal repository
##              output_dir [character] -- location for model outputs
##              settings_loc [character] -- file path of settings file
##              imp [character] -- imputation / draw-model number
##
## Requires:    covariates data, if used (covars, covars_as, covars_trans, covar_file, covar_as_file)
##              neighborhood adjacency matrix (adjmat_file)
##
## Outputs:     prepped data file ("[output_dir]/data_IMP.rds")
##              graphs for age, time, and year random effects ("[output_dir]/re_graphs.rdata")
##
####################################################################################################

###### Load required libraries
pacman::p_load(R.utils, data.table, TMB, boot)

###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (output_dir_draws_est <- commandArgs(TRUE)[[3]])
  (settings_loc <- commandArgs(TRUE)[[4]])
  (imp <- commandArgs(TRUE)[[5]])
}

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

###### Assign settings from settings file
get_settings(settings_loc)

##### Process data_file object
data_file <- data.table("name" = "Single source", "location" = gsub("imp", imp, data_file))

print(paste0("Processing ", data_file$location))

##### Load and prep inputs
combined_dt <- readRDS(data_file$location)

###### Add a data source field
combined_dt$source <- data_file$name

###### Get mcnty:state mapping and location meta data
locs <- fread("FILEPATH")

## Standardize outcome names
setnames(combined_dt, outcome, "rate")
dt_state <- unique(combined_dt[, list(state, year, sex, age, rate)])
dt_state$agg_id <- 0:(nrow(dt_state) - 1)

dt_mcnty <- combined_dt[level == "mcnty" & race != 1]
if (!exists("covars_subpop_5plus")) {
  covars_subpop_5plus <- NULL
}
if (!exists("covars_subpop_1plus")) {
  covars_subpop_1plus <- NULL
}
if (!exists("covars_subpop_15plus")) {
  covars_subpop_15plus <- NULL
}
if (!exists("covars_subpop_20_to_60")) {
  covars_subpop_20_to_60 <- NULL
}
if (!exists("covars_subpop_5_to_60")) {
  covars_subpop_5_to_60 <- NULL
}
if (!exists("covars_subpop_5_to_15")) {
  covars_subpop_5_to_15 <- NULL
}

cols_to_keep <- c("year", "area", "sex", "level", "age", "source", "race", covars_subpop, covars_subpop_20plus, covars_subpop_60under, covars_subpop_1plus,
                  covars_subpop_5plus, covars_subpop_15plus, covars_subpop_5_to_15, covars_subpop_65plus, covars_subpop_20_to_60, covars_subpop_5_to_60)

## Ensure all covars are being scaled
stopifnot(length(covars_scale) == length(c(covars_subpop, covars_subpop_20plus, covars_subpop_60under, covars_subpop_1plus,
                                           covars_subpop_5plus, covars_subpop_15plus, covars_subpop_5_to_15, covars_subpop_65plus, covars_subpop_20_to_60, covars_subpop_5_to_60)))

dt_mcnty <- dt_mcnty[, ..cols_to_keep]
dt_mcnty$level <- NULL

dt_mcnty <- merge(dt_mcnty, unique(locs[, list(mcnty, state, state_name)]), by.x = "area", by.y = "mcnty", all.x = TRUE)

merge_vars <- intersect(names(dt_mcnty),names(dt_state))
combined_dt <- merge(dt_mcnty, dt_state, by = merge_vars, all = TRUE)


###### Resort
setkeyv(combined_dt, c("area", "year", "sex", "race", "age", "source", strat_vars))

###### Set indicator for certain ages as some covariates are age-restricted
combined_dt[, under_20 := as.integer(age < 20)]
combined_dt[, over_60 := as.integer(age > 60)]
combined_dt[, under_5 := as.integer(age < 5)]
combined_dt[, under_1 := as.integer(age < 1)]
combined_dt[, under_15 := as.integer(age < 15)]
combined_dt[, ages_5_to_15 := as.integer(age %in% c(5:15))]
combined_dt[, ages_65plus := as.integer(65 <= age)]
combined_dt[, ages_20_to_60 := as.integer(age %in% c(20:60))]
combined_dt[, ages_5_to_60 := as.integer(age %in% c(5:60))]

combined_dt <- combined_dt[year %in% years & age %in% ages & sex %in% sexes & race %in% races]

saveRDS(combined_dt, file = paste0(output_dir, "/data_pre_factor.rds"))

recodes <- list()

#### Recode vars to start from 0
# Race
combined_dt[, race_recode := as.integer(factor(race, levels = races)) - 1L]
recodes$race <- data.table(cbind.data.frame(var = "race", "race" = as.integer(races), "race_recode" = as.integer(factor(races, levels = races)) - 1L))
combined_dt[, c("race", "race_recode") := list(race_recode, NULL)]

# Source
if (is.null(source_levels)) {
  source_levels <- c("state")
}

combined_dt[, source_index_recode := as.integer(factor(source, levels = source_levels)) - 1L]
recodes$source <- data.table(cbind.data.frame(var = "source", "source_index" = source_levels, "source_index_recode" = as.integer(factor(source_levels, levels = source_levels)) - 1L))
combined_dt[, c("source_index", "source_index_recode") := list(source_index_recode, NULL)]

# Sex
combined_dt[, sex_recode := as.integer(factor(sex, levels = sexes)) - 1L]
recodes$sex <- data.table(cbind.data.frame(var = "sex", "sex" = as.integer(sexes), "sex_recode" = as.integer(factor(sexes, levels = sexes)) - 1L))
combined_dt[, c("sex", "sex_recode") := list(sex_recode, NULL)]

# Year
combined_dt[, year_recode := as.integer(factor(year, levels = years, ordered = TRUE)) - 1L]
recodes$year <- data.table(cbind.data.frame(var = "year", "year" = as.integer(years), "year_recode" = as.integer(factor(years, levels = years)) - 1L))
combined_dt[, c("year", "year_recode") := list(year_recode, NULL)]

# Age
combined_dt[, age_recode := as.integer(factor(age, levels = ages, ordered = TRUE)) - 1L]
recodes$age <- data.table(cbind.data.frame(var = "age", "age" = as.integer(ages), "age_recode" = as.integer(factor(ages, levels = ages)) - 1L))
combined_dt[, c("age", "age_recode") := list(age_recode, NULL)]

# Area (mcnty)
area_levels <- unique(locs$mcnty)
combined_dt[, area_recode := as.integer(factor(area, levels = area_levels)) - 1L]
recodes$area <- data.table(cbind.data.frame(var = "mcnty", "area" = as.integer(area_levels), "area_recode" = as.integer(factor(area_levels, levels = area_levels)) - 1L))
setnames(recodes$area, "area", "mcnty")
combined_dt[, c("area", "area_recode") := list(area_recode, NULL)]

# State
state_levels <- unique(locs$state)
combined_dt[, state_recode := as.integer(factor(state, levels = state_levels)) - 1L]
recodes$state <- data.table(cbind.data.frame(var = "state", "state" = as.integer(state_levels), "state_recode" = as.integer(factor(state_levels, levels = state_levels)) - 1L))
combined_dt[, c("state", "state_recode") := list(state_recode, NULL)]

###### Save recoding file
saveRDS(recodes, file = paste0(output_dir, "/recode.rds"))

###### Create structure matrices for space, time, and age effects
# no spatial effects
if (!exists("adjmat")) { # If adjmat was not created above...
  adjmat <- readRDS(adjmat_file)
}

if (grepl("state", adjmat_file)) {
  graph_k <- diag(apply(adjmat, 1, sum)) - adjmat
  graph_k <- as(graph_k, "dgTMatrix")
  graph_j <- NULL
} else {
  graph_j <- diag(apply(adjmat, 1, sum)) - adjmat
  graph_j <- as(graph_j, "dgTMatrix")
  graph_k <- NULL
  rm(adjmat)
}

num_t <- nrow(recodes$year)
num_a <- nrow(recodes$age)

graph_t <- matrix(rep(0, num_t^2), nrow = num_t, ncol = num_t)
graph_t[abs(row(graph_t) - col(graph_t)) == 1] <- 1
graph_t <- diag(apply(graph_t, 1, sum)) - graph_t
graph_t <- as(graph_t, "dgTMatrix")

if (length(ages) > 1) {
  graph_a <- matrix(rep(0, num_a^2), nrow = num_a, ncol = num_a)
  graph_a[abs(row(graph_a) - col(graph_a)) == 1] <- 1
  graph_a <- diag(apply(graph_a, 1, sum)) - graph_a
  graph_a <- as(graph_a, "dgTMatrix")
} else {
  graph_a <- NULL
}

###### Save prepped data
setkeyv(combined_dt, c("area", "year", "sex", "race", "age", "source", strat_vars))
# saveRDS(combined_dt, file = paste0(output_dir, "/data.rds"))
save(graph_a, graph_j, graph_t, graph_k, file = paste0(output_dir, "/re_graphs.RData"))

###### Get and save population estimates
if (exists("pop_covariate_dataset_id")) {
  if (!is.null(pop_covariate_dataset_id)) {
    pop <- get_population_data(covariate_dataset_id = pop_covariate_dataset_id)
    pop[, "race_set" := NULL] # Drop race_set to avoid confusion
  }
} else {
  pop <- readRDS(pop_file)
}

###### Check race codes for population data
validate_race_codes(DT = pop,
                    race_set_id = 1, # specifies DB code
                    includes_all_race = F, # says not to expect the all-race variable (1)
                    silent = T, # doesn't print anything if the outputs look okay
                    expect_missing_race = c(3)) # tells the function that it's okay if NH Other is missing


## Restrict to requested races, sexes, ages, and years
pop <- pop[(race %in% races) & (sex %in% sexes) & (age %in% ages) & (year %in% years)]

## Save population data
saveRDS(pop, file = paste0(output_dir_draws_est, "/population.rds"))

###### Now run code that was previously embedded in fit_mod scripts
#### Add intercept and define num variables
combined_dt[, int := 1L]

#### Calculate num variables
num_s <- nrow(recodes$sex)
num_j <- nrow(recodes$area)
num_t <- nrow(recodes$year)
num_a <- nrow(recodes$age)
num_r <- nrow(recodes$race)
num_d <- nrow(recodes$source)
num_k <- nrow(recodes$state)

#### Save num variables
save(num_s, num_j, num_t, num_a, num_r, num_d, num_k, file = paste0(output_dir, "/num_vars.RData"))

###### Read in mcnty:state aggregation weights
mcnty_state_agg_wts <- readRDS(paste0("FILEPATH"))

###### Recode variables in state_mcnty data set to ordered integer (0-indexed)
mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$sex[, -"var"], on = "sex"]
mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$year[, -"var"], on = "year"]
mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$age[, -"var"], on = "age"]
mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$race[, -"var"], on = "race"]
mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$area[, -"var"], on = "mcnty"]
mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$state[, -"var"], on = "state"]
mcnty_state_agg_wts$mcnty_state_uid <- 0:(nrow(mcnty_state_agg_wts) - 1)

###### Clean up recoded values
mcnty_state_agg_wts[, c("year", "sex", "age", "race", "area", "state") := list(year_recode, sex_recode, age_recode, race_recode, area_recode, state_recode)]
mcnty_state_agg_wts[, c("year_recode", "age_recode", "race_recode", "sex_recode", "area_recode", "state_recode", "mcnty") := NULL]

###### Set unique IDs
dt_agg <- copy(combined_dt)

###### Derive mapping from dt_state to dt_mcnty
merge_vars <- intersect(names(mcnty_state_agg_wts), names(dt_agg))
dt_agg <- merge(dt_agg, mcnty_state_agg_wts, merge_vars, all.x = TRUE)


###### Drop rows with 0 wt for modeling but save before dropping for making predictions
setkeyv(dt_agg, "agg_id")
### Check that agg_wts sum to 1
stopifnot(dt_agg[, sum(agg_wt), by = "agg_id"][, min(abs(V1 - 1))] < 1e-10)

###### If cross validation is being performed, save full data set (for predictions), then exclude 1 fold from modeling data
###### imp indicates what fold should be excluded

# for reproducability
set.seed(12345)

if (cross_val) {
  # assumes 1 data file
  fold <- (imp / n.folds)
  fold_vecs <- fread(gsub(paste0("model_data_", imp, ".rds"), "folds_vec.csv", data_file[1, location]))
  fwrite(fold_vecs, paste0(output_dir, "/folds_vec.csv"))
  ## recode
  fold_vecs <- merge(fold_vecs, recodes$state, by = "state")
  fold_vecs[, c("state", "state_recode", "var") := list(state_recode, NULL, NULL)]
  dt_agg <- merge(dt_agg, fold_vecs, by = "state", all = T)
  saveRDS(dt_agg, file = paste0(output_dir, "/data_full_", imp, ".rds")) # save prediction data with all rows
  dt_agg_copy <- copy(dt_agg)
  dt_agg <- dt_agg[!(agg_wt == 0) & fold_vec != fold] # exclude fold for modeling data
  
  ###### Check that agg_wts sum to 1 (again)
  stopifnot(dt_agg[, sum(agg_wt), by = "agg_id"][, min(abs(V1 - 1))] < 1e-10)
} else {
  # save full data set for predictions
  saveRDS(dt_agg, file = paste0(output_dir, "/data_full_", imp, ".rds"))
}

# drop strata with no agg_wt for modeling
dt_agg <- dt_agg[!(agg_wt == 0)]
stopifnot(dt_agg[, sum(agg_wt), by = "agg_id"][, min(abs(V1 - 1))] < 1e-10)

#  set subs to NA for ages under 15 and resp for ages under 1 so it is not used for scaling
# subs has values filled in for under 15 that should be ignored (according to subs modeler)
# and resp is 0 for ages under 1 always for the US

if ("yll_rate_subs" %in% covars_scale) {
  dt_agg[under_15 == 1, subs := NA]
}

if ("yll_rate_resp" %in% covars_subpop_1plus) {
  dt_agg[under_1 == 1, resp := NA]
}

###### Check states object
if (exists("states")) {
  if ((length(states) > 0) & states[1] != "all") {
    #### Retain only data for requested states for model fitting
    dt_agg <- dt_agg[(state_name %in% states)]
  }
}

###### Transform yld rates using emplogit to accommodate rows with yld rate of 0
emplogit <- function (x, eps = 1e-3) log((eps + x)/(1 - x + eps))
dt_agg[rate == 0, rate := inv.logit(emplogit(rate, min(combined_dt[rate > 0, rate]) / 2))]

###### Transform covariates
if (!is.null(covars_trans)) {
  for (var in names(covars_trans)) {
    ## Pull out function to be used to transform vars
    ## and covars to be transformed are stored in names of the vector
    fun <- eval(parse(text = paste("function(x)", covars_trans[var])))
    eval(parse(text = paste0("dt_agg$", var, " <- fun(dt_agg$", var, ")")))
  }
}

###### scale covariates
if (!is.null(covars_scale)) {
  for (var in covars_scale) {
    eval(parse(text = paste0("dt_agg$", var, " <- scale(dt_agg$", var, ")")))
  }
}

###### Save data set
saveRDS(dt_agg, file = paste0(output_dir, "/data_", imp, ".rds"))

###### End
print("Script completed")

