####################################################################################################
##
## Description: Fit the following model in TMB:
##
##              D_{j,a,t} ~ Poisson(m_{j,a,t} * P_{j,a,t})
##              log(m_{j,a,t}) = B0 + B*X_{j,t} + B*X1_{j,t,r} + u1_{j,a',t',r}*S(a')*S(t') + u2_{j} + 
##                u3_{a,t,r} + u4_{r}*X1[j,t,r]
##              u1_{j,a',t',r} ~ LCAR:LCAR:LCAR:IID(rho_1j, rho_1a, rho_1t, rho_1j, sigma_1)
##              u2_{j} ~ LCAR(rho_2, sigma_2)
##              u3_{a,t,r} ~ LCAR:LCAR:IID(rho_3a, rho_3t, sigma_3)
##              u4_{r} ~ IID(sigma_subpop) [vector of IID random effects multiplied by each R/E specific covariate]
##              where j = area, a = age group, t = year, r = race, S(a') = age spline, S(t') = time spline, D = deaths, P = population, and m = mx
##
## Passed args: dir [character] -- home directory for settings and final output
##              sex [integer] -- sex to run model for
##
## Requires:    prepped data file ("[limited_use_dir]/data.rds") from limited use directory
##              random effect graphs ("[dir]/re_graphs.rdata")
##
## Outputs:     fitted model object ("[dir]/model_fit_test_[sex].rds")
##              model-fitting log ("[dir]/model_fitting_test_[sex].txt")
##              model-fitting timings ("[dir]/model_fit_time_test_[sex].rds")
##
####################################################################################################

.libPaths(c("FILEPATH", .libPaths()))
library(TMB)

library(R.utils)
library(data.table)
library(optimx)
library(splines)
sourceDirectory("functions/")

set.seed(98121)

tmb.ok <- .Call("have_tmb_symbolic",PACKAGE="TMB")
if (tmb.ok) {
  message("TMB ok - have_tmb_symbolic")
} else {
  message("TMB PROBLEM - have_tmb_symbolic is false. runSymbolicAnalysis WILL NOT DO ANYTHING")
}

## Get settings ------------------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]
sex <- as.integer(args[2])
race <- as.integer(args[3])
edu <- as.integer(args[4])

get_settings(dir)

message_fit_args()

message("Done with settings")

current_dir <- gsub("FILEPATH", "", dir)
lu_root <- paste0("FILEPATH",LU_folder,"FILEPATH")
lu_modeldir <- paste0(lu_root,"/",current_dir)

sink(file = paste0(dir, "/model_fitting_", sex, "_", race, "_", edu, ".txt"), type = "output", split = T)

## Format data for TMB -----------------------------------------------------------------------------
cat("\n\n***** Format data\n"); flush.console()
load(paste0(dir, "/re_graphs.rdata"))
data <- readRDS(paste0(lu_modeldir, "/data.rds"))

# index the data
data <- index_edu_race(data)

# define the number of each variable
counts <- count_variables(data)
for (i in unique(1:length(counts))) {
  assign(names(counts[i]), counts[[i]])
}

## setup spline bases on age and time
if (!is.null(year_knots_num)) {
  time_spline_info <- build_time_spline(years, year_knots_num)
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

# generates the hyperpriors parameters. e.g.: re1_par1, re1_par2, re1_log_sigma, etc, for the main
# random effects (i.e., not related to covariates)
gen_hyperpriors_parameters(prior_list)

## get rid of deaths that are NA for cvd_pah
if(get("cause_id", .GlobalEnv) == 1004) {
  data <- data[year >= (match(2004, years)-1)]
}

# Set data for TMB ---------------------------------------------------------------

ii <- which(data$pop > 0)
tmb_data <- list(
  Y = data$deaths[ii],
  N = data$pop[ii],
  J = data$area[ii],
  A = data$age[ii],
  T = data$year[ii],
  R = data$race[ii],
  X = as.matrix(data[ii, c("int", covars, covars_as, covars_subpop), with = F]),
  X1 = as.matrix(data[ii, covars_subpop, with = F]),
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
  # NOTE: all covariate random effects are being given the same hyperpriors parameters
  covar_subpop_hyperpriors_parameters = gen_covar_subpop_hyperpriors_parameters(prior_type, covar_subpop_hyperpriors_settings)
  )

## Set parameters for TMB --------------------------------------------------------------------------
cat("\n\n***** Set parameters\n"); flush.console()
tmb_par <- list(
  B = rep(0, length(covars) + length(covars_as) + length(covars_subpop) + 1), # intercept & covariate effects

  # area-year spline-age spline-race-level random intercept
  re1 = array(rep(0, num_j * num_year_spline * num_age_spline * num_subpop), dim = c(num_year_spline, num_age_spline, num_j, num_subpop)),
  re1_log_sigma = re1_log_sigma,
  logit_rho_1j = 0,
  logit_rho_1t = 0,
  logit_rho_1a = 0,

  # area-level random intercept
  re2 = rep(0, num_j),
  re2_log_sigma = re2_log_sigma,
  logit_rho_2 = 0,

  # age-year-race-level random intercept
  re3 = array(rep(0, num_t * num_a * num_subpop), dim = c(num_t, num_a, num_subpop)),
  re3_log_sigma = re3_log_sigma,
  logit_rho_3t = 0,
  logit_rho_3a = 0,

  # covariate related random effects
  covar_subpop_re_matrix = gen_covar_subpop_re_matrix(num_subpop = num_subpop),

  # log sigmas / hyperpriors for covariate related random effects
  covar_subpop_log_sigmas = gen_covar_subpop_log_sigmas(covar_subpop_hyperpriors_settings)
)

saveRDS(tmb_par, paste0(dir, "/model_parameters_", sex, "_", race, "_", edu, ".rds"))

map <- NULL

message("Done initializing data")

## Fit model ---------------------------------------------------------------------------------------
# compile CPP code for objective function
TMB::compile("tmb_models/mod_spline_iid_race_one_var_flexible_re_covs.cpp")

dyn.load(dynlib("tmb_models/mod_spline_iid_race_one_var_flexible_re_covs"))
config(tape.parallel = 0, DLL = "mod_spline_iid_race_one_var_flexible_re_covs")

# make objective function
cat("\n\n***** Make objective function\n"); flush.console()
obj <-
  MakeADFun(
    tmb_data,
    tmb_par,
    random = c("B", paste0("re", 1:3), "covar_subpop_re_matrix"),
    DLL = "mod_spline_iid_race_one_var_flexible_re_covs",
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
  if (opt$convcod == 0)
    break
}

(opt_time <- proc.time() - opt_time)
if (opt$convcod != 0) stop("Model did not converge")

# get standard errors
cat("\n\n***** Extract standard errors\n"); flush.console()
se_time <- proc.time()
saveRDS(obj$report(),
        file = paste0(dir, "/model_fit_OPT_", sex, "_", race, "_", edu, ".rds"))

out <- sdreport(obj, getJointPrecision = T)
(se_time <- proc.time() - se_time)

# save model output
cat("\n\n***** Save model output\n"); flush.console()
saveRDS(out, file = paste0(dir, "/model_fit_", sex, "_", race, "_", edu, ".rds"))
saveRDS(
  rbind(se_time, opt_time),
  file = paste0(dir, "/model_fit_time_", sex, "_", race, "_", edu, ".rds")
)
sink()
