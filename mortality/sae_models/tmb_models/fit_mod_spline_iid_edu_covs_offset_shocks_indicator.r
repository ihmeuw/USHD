####################################################################################################
## This one has covariates besides offset
##
## Description: see appendix for the life expectancy by educational attainment paper for full model 
##              specification.
##
## Passed args: dir [character] -- home directory for settings and final output
##              sex [integer] -- sex to run model for
##              race [integer] -- race to run model for. 99 means all races at once.
##              edu [integer] -- edu to run model for. 99 means all edus at once.
##
## Requires:    prepped data file ("[FILEPATH]/data.rds") from limited use directory
##              random effect graphs ("[dir]/re_graphs.rdata")
##
## Outputs:     fitted model object ("[dir]/model_fit_test_[sex].rds")
##              model-fitting log ("[dir]/model_fitting_test_[sex].txt")
##              model-fitting timings ("[dir]/model_fit_time_test_[sex].rds")
##
####################################################################################################
start <- Sys.time() # get start time

stopifnot(grepl("mortality/sae_models$", getwd()))

.libPaths(c("FILEPATH", "FILEPATH")) 

for (env_var in c("OMP_NUM_THREADS", "SET_OMP_THREADS", "MKL_NUM_THREADS", "SET_MKL_THREADS")) {
  val <- Sys.getenv(env_var, unset = "<UNSET>")
  message(sprintf("%s is set to '%s'", env_var, val))
}

.libPaths(c("FILEPATH", .libPaths()))
library(TMB)

library(R.utils)
library(data.table)
library(optimx)
library(splines)
sourceDirectory("functions/")

set.seed(98121)

tmb.ok <- .Call("have_tmb_symbolic", PACKAGE = "TMB")
if (tmb.ok) {
  message("TMB ok - have_tmb_symbolic")
} else {
  message("TMB PROBLEM - have_tmb_symbolic is false. runSymbolicAnalysis WILL NOT DO ANYTHING")
}

## Get settings ------------------------------------------------------------------------------------
if (interactive()) {
  # dir <- "FILEPATH"
  dir <- "FILEPATH"
  sex <- 2
  race <- 1
  edu <- 99
} else {
  args <- commandArgs(trailingOnly = TRUE)
  dir <- args[1]
  sex <- as.integer(args[2])
  race <- as.integer(args[3])
  edu <- as.integer(args[4])
}

message_fit_args()

get_settings(dir)

message("Done with settings")

current_dir <- gsub("FILEPATH", "", dir)
lu_root <- paste0("FILEPATH",LU_folder,"FILEPATH")
lu_modeldir <- paste0(lu_root,"/",current_dir)

# check that the settings were created before the raw data
if(!(check_settings_made_before_data(dir = dir, ludir = lu_modeldir))) {
  stop("settings.csv was prepared before data.rds. Re-run prep_inputs.r to create data.rds")
}

if (!interactive()) sink(file = paste0(dir, "/model_fitting_", sex, ".txt"), type = "output", split = T)

## Format data for TMB -----------------------------------------------------------------------------
cat("\n\n***** Format data\n"); flush.console()
load(paste0(dir, "/re_graphs.rdata"))
data <- readRDS(paste0(lu_modeldir, "/data.rds"))

# add state
loc <- fread('FILEPATH/merged_counties.csv')
if ("state" %in% names(data)) {
  data[, state := NULL]
}
data <- merge(data, unique(loc[, .(mcnty, state)]), by.x = "area", by.y = "mcnty", all.x = T)
if (any(is.na(data$state))) stop("state column of data has NA values")


# DROP ROWS OF DATA WHERE THERE IS NO EDUCATION INFORMATION if relevant, it would have been added in
# prep_inputs. If this column isn't present, no data will be dropped. This column is expected to
# exist if by_edu is True
if (by_edu) {
  stopifnot("edu_reporting_status" %in% names(data))
}

if ("edu_reporting_status" %in% names(data)) {
  data <- data[edu_reporting_status == 1, ] # drop non reporting / keep only reporting
  data[, edu_reporting_status := NULL] # remove the reporting column
  # There should not be any deaths marked as unknown
  stopifnot(nrow(data[edu == 100, ]) == 0)
  # There should not be any NA pop
  if (any(is.na(data$pop))) stop("There are NA values in the pop column of data")
}

if (any(is.na(data))) stop("NAs in data")

# index the data. This indexes race, education, filters to the sex specified in the args,
# and sets the intercept. It also will filter to race or edu if needed (which is not usually needed).
data <- index_edu_race(data)

# manually encode state starting from zero.
data[, state_f := as.integer(as.factor(state)) - 1]
fwrite(x = unique(data[, .(state, state_f)]), file.path(dir, "state_factor_table.csv"))
data[, state := NULL]
setnames(data, "state_f", "state")
# check that it is zero-indexed
stopifnot(min(data$state) == 0)
# check that there are still 51 entries (the 50 states plus Washington D.C.)
stopifnot(length(unique(data$state)) == 51)
# check that the entries are expected
stopifnot(setequal(seq(0, 50, 1), unique(data$state)))


# define the count of each variable
counts <- count_variables(data)
for (i in unique(1:length(counts))) {
  message(glue::glue("{names(counts[i])} is {counts[[i]]}"))
  assign(names(counts[i]), counts[[i]])
}

# custom num state
num_s <- max(data$state) + 1

## setup spline bases on age and time
if (!is.null(year_knots_num) | !is.null(year_knots_spec)) {
  if (!is.null(year_knots_spec)) {
    time_spline_info <- build_time_spline(
      years,
      year_knots_spec = year_knots_spec,
      detach_spline_19_21 = detach_spline_19_21,
      detach_spline_19_22 = detach_spline_19_22
    )
  } else {
    time_spline_info <- build_time_spline(
      years,
      year_knots_num = year_knots_num,
      detach_spline_19_21 = detach_spline_19_21,
      detach_spline_19_22 = detach_spline_19_22
    )
  }

  s_year <- time_spline_info[[1]]
  graph_ts <- time_spline_info[[2]]
  num_year_spline <- time_spline_info[[3]]
}

if (!is.null(age_knots_spec)) {
  age_spline_info <- build_age_spline(ages, age_knots_spec)
  s_age <- age_spline_info[[1]]
  graph_as <- age_spline_info[[2]]
  num_age_spline <- age_spline_info[[3]]
}

if (pop_add1) {
  if ("adjust_pop" %in% names(data)) {
    message("Adjusting 0 population based on pop and deaths from all-cause mortality")
    data[adjust_pop == 1, pop := 1]
  } else {
    message("Adjusting 0 population based on pop and deaths for this cause")
    data[pop == 0 & deaths > pop & !is.na(deaths), pop := 1]
  }
}

# read in the priors, if needed
if (is.null(prior_type)) {
  prior_type <- "loggamma"
  prior_list <- default_priors(3)
}

# generates the hyperpriors parameters. e.g.: re1_par1, re1_par2, re1_log_sigma, etc, for the main
# random effects (i.e., not related to covariates)
gen_hyperpriors_parameters(prior_list)

# Set data for TMB ---------------------------------------------------------------

# check that new state column, which is important to this model, is present
stopifnot("state" %in% names(data))

# similarly, check for covars_offset
if (is.null(covars_offset)) stop("covars_offset is NULL")
stopifnot(covars_offset %in% names(data))

# check that covar_subpop_hyperpriors_settings is present
if (is.null(covar_subpop_hyperpriors_settings)) {
  stop("covar_subpop_hyperpriors_settings is NULL")
}

# check that new indic columns are present
stopifnot("indic_1" %in% names(data))
stopifnot("indic_2" %in% names(data))

ii <- which(data$pop > 0)
tmb_data <- list(
  Y = data$deaths[ii],
  N = data$pop[ii],
  J = data$area[ii],
  A = data$age[ii],
  T = data$year[ii],
  E = data$edu[ii],
  X = as.matrix(data[ii, c("int", covars, covars_as, covars_subpop), with = F]), 
  X1 = as.matrix(data[ii, covars_subpop, with = F]),
  S = data$state[ii], # state column
  I_first = data$indic_1[ii], # indicator for if a year is 2001 or not
  I_second = data$indic_2[ii],
  graph_j = graph_j,
  graph_t = graph_t,
  graph_a = graph_a,
  s_age = s_age,
  s_year = s_year,
  graph_as = graph_as,
  graph_ts = graph_ts,
  num_subpop = num_subpop,
  re1_prior_param = list(type = prior_type, par1 = re1_par1, par2 = re1_par2),
  re2_prior_param = list(type = prior_type, par1 = re2_par1, par2 = re2_par2),
  re3_prior_param = list(type = prior_type, par1 = re3_par1, par2 = re3_par2),
  re4_prior_param = list(type = prior_type, par1 = re4_par1, par2 = re4_par2),
  re5_prior_param = list(type = prior_type, par1 = re5_par1, par2 = re5_par2),
  re6_prior_param = list(type = prior_type, par1 = re6_par1, par2 = re6_par2),
  re7_prior_param = list(type = prior_type, par1 = re7_par1, par2 = re7_par2),
  
  covar_subpop_hyperpriors_parameters = gen_covar_subpop_hyperpriors_parameters(prior_type, covar_subpop_hyperpriors_settings),
  # new object for the offset covariate
  X_offset = as.matrix(data[ii, covars_offset, with = F]), 
  # prior_params / hyperprior_parameters for Offset covar random effect.
  covar_offset_hyperpriors_parameters = list(type = prior_type,
                                             par1 = covar_subpop_hyperpriors_settings$re_cov_par1,
                                             par2 = covar_subpop_hyperpriors_settings$re_cov_par2)
)

if (is.null(tmb_data$covar_offset_hyperpriors_parameters)) {
  stop("tmb_data$covar_offset_hyperpriors_parameters is NULL")
}

## Set parameters for TMB --------------------------------------------------------------------------
cat("\n\n***** Set parameters\n"); flush.console()
tmb_par <- list(
  B = rep(0, length(covars) + length(covars_as) + length(covars_subpop) + 1), # intercept & covariate effects

  B_offset = rep(0, num_subpop),

  re1 = array(rep(0, num_j * num_year_spline * num_age_spline * num_subpop), dim = c(num_year_spline, num_age_spline, num_j, num_subpop)),
  re1_log_sigma = re1_log_sigma,
  logit_rho_1j = 0,
  logit_rho_1t = 0,
  logit_rho_1a = 0,

  re2 = rep(0, num_j), # area-level random intercept
  re2_log_sigma = re2_log_sigma,
  logit_rho_2 = 0,

  re3 = array(rep(0, num_t * num_a * num_subpop), dim = c(num_t, num_a, num_subpop)), # age-year-edu-level random intercept
  re3_log_sigma = re3_log_sigma,
  logit_rho_3t = 0,
  logit_rho_3a = 0,

  # These effects are for the shocks indicators
  re4 = rep(0, num_j), # area/age-level random intercept (IID)
  re4_log_sigma = re4_log_sigma,

  re5 = rep(0, num_j), # area/age-level random intercept (IID)
  re5_log_sigma = re5_log_sigma,

  re6 = rep(0, num_a), # area/age-level random intercept (IID)
  re6_log_sigma = re6_log_sigma,

  re7 = rep(0, num_a), # area/age-level random intercept (IID)
  re7_log_sigma = re7_log_sigma,

  # subpop covariate related random effects
  covar_subpop_re_matrix = gen_covar_subpop_re_matrix(num_subpop = num_subpop),

  # log sigmas / hyperpriors for subpop covariate related random effects
  covar_subpop_log_sigmas = gen_covar_subpop_log_sigmas(covar_subpop_hyperpriors_settings),

  covar_offset_random_effect = array(rep(0, num_subpop * num_a * num_s), dim = c(num_subpop, num_a, num_s)), # 4 education by 13 ages by 51 states
  covar_offset_log_sigma = covar_subpop_hyperpriors_settings$re_cov_log_sigma # using the same log_sigma that the subpop covariates use
) # end tmb par

message(glue::glue("The dimensions of covar_subpop_re_matrix is ({paste(dim(tmb_par$covar_subpop_re_matrix), collapse = ', ')})"))

saveRDS(tmb_par, paste0(dir, "/model_parameters_", sex, "_", race, "_", edu, ".rds"))

message("Done initializing data")

## Fit model ---------------------------------------------------------------------------------------
# compile CPP code for objective function
TMB::compile("tmb_models/mod_spline_iid_edu_covs_offset_shocks_indicator.cpp")
dyn.load(dynlib("tmb_models/mod_spline_iid_edu_covs_offset_shocks_indicator"))
config(tape.parallel = 0, DLL = "mod_spline_iid_edu_covs_offset_shocks_indicator")

# make objective function
random_for_obj <- c(
  "B",
  paste0("re", 1:7),
  "covar_subpop_re_matrix",
  "B_offset",
  "covar_offset_random_effect"
)

for (item in random_for_obj) {
  if (item %in% names(tmb_par)) {
    print(glue::glue("{item} is in tmb_par"))
  } else {
    stop(glue::glue("{item} is NOT in tmb_par"))
  }
}

cat("\n\n***** Make objective function\n"); flush.console()
obj <- MakeADFun(
  tmb_data,
  tmb_par,
  random = random_for_obj,
  DLL = "mod_spline_iid_edu_covs_offset_shocks_indicator",
  map = map
)
runSymbolicAnalysis(obj)

# optimize objective function
cat("\n\n***** Optimize objective function\n"); flush.console()
opt_time <- proc.time()
for (method in c("nlminb", "L-BFGS-B", "Nelder-Meade", "CG")) {
  cat(paste("\n  *** Method: ", method, "\n"))
  opt <-
    optimx(
      par = obj$par,
      fn = function(x)
        as.numeric(obj$fn(x)),
      gr = obj$gr,
      method = method,
      control = list(iter.max = 500, eval.max = 500),
      itnmax = 200
    )
  print(opt)
  if (opt$convcod == 0) break
}

(opt_time <- proc.time() - opt_time)
if (opt$convcod != 0) stop("Model did not converge")

# get standard errors
cat("\n\n***** Extract standard errors\n"); flush.console()
se_time <- proc.time()
saveRDS(obj$report(),
        file = paste0(dir, "/model_fit_OPT_", sex, "_", race, "_", edu, ".rds")) # temporarily save this so that we can see the covariance matrix

out <- sdreport(obj, getJointPrecision = T)
(se_time <- proc.time() - se_time)

# save model output
cat("\n\n***** Save model output\n"); flush.console()
saveRDS(out, file = paste0(dir, "/model_fit_", sex, "_", race, "_", edu, ".rds"))
saveRDS(
  rbind(se_time, opt_time),
  file = paste0(dir, "/model_fit_time_", sex, "_", race, "_", edu, ".rds")
)

duration <- round(x = difftime(Sys.time(), start, units = "min"), digits = 2)


cat("\n\nDone!\n"); flush.console()
cat(glue::glue("Done in {duration} minutes.")); flush.console()

if (!interactive()) sink()
