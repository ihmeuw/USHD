####################################################################################################
## Description: Fit the following model in TMB (separately by sex) for YLD rates:
##
##              R_{k,t,a} ~ Normal(R_{k,t,a}, sigma^2)
##              R{k,t,a} = Σj∈{k,r} (R{j,t,a,r}*P{j,t,a,r}) / Σj∈{k,r} P{j,t,a,r}
##              logit(R_{j,a,t,r}) = B0 + (B1 + u1_{a})*X1{j,t,a,r}*U=0 + u2_{t,a}
##              u2_{t,a} ~ LCAR:LCAR(rho_t, rho_a, sigma)
##
##              where k = states, j = mcnty (area), a = age group, t = year, r = race,
##              X1 = covariates with random slopes by age,
##              X# = age-restricted covariates with random slopes by age,
##              U# = age indicator,
##              #'s: 2 = 20+ covariates; 3 = 60 and under covariates; 4 = 15+ covariates
##              R = YLD rate, and P = population
##
##              This model fits data at the mcnty-level by race/ethnicity and evaluates the 
##              likelihood at the all-race, state-level
##
## Passed args: output_dir [character] -- home directory for settings and final output
##
## Requires:    prepped data file ("[output_dir]/data.rds")
##              random effect graphs ("[output_dir]/re_graphs.rdata")
##
## Outputs:     fitted model object ("[output_dir]/model_fit_[sex].rds")
##              model-fitting log ("[output_dir]/model_fitting_[sex].txt")
##              model-fitting timings ("[output_dir]/model_fit_time_[sex].rds")
##              (covariance matrix): ("[output_dir]/model_fit_OPT_[sex].rds")
##              fit workspace: ("[output_dir]/fit_workspace_[sex].RData")
##              pre-fit model object ("[output_dir]/obj_pre_fit_[sex].rds)
##
####################################################################################################
###### Load required libraries
pacman::p_load(R.utils, data.table, splines, optimx, dplyr)

for (env_var in c("OMP_NUM_THREADS", "SET_OMP_THREADS", "MKL_NUM_THREADS", "SET_MKL_THREADS")) {
  val <- Sys.getenv(env_var, unset = "<UNSET>")
  message(sprintf("%s is set to '%s'", env_var, val))
}

if (!dir.exists("FILEPATH")) {
  stop("FILEPATH DOES NOT EXIST - figure out your bindmounts!")
}

.tmb.dir <- "FILEPATH"
if (!dir.exists(.tmb.dir)) {
  stop(sprintf("Dir %s does not exist", .tmb.dir))
}

library(TMB, lib.loc = .tmb.dir)

tmb.ok <- .Call("have_tmb_symbolic", PACKAGE = "TMB")
if (tmb.ok) {
  message("TMB ok - have_tmb_symbolic")
} else {
  message("TMB PROBLEM - have_tmb_symbolic is false. runSymbolicAnalysis WILL NOT DO ANYTHING")
}

###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (settings_loc <- commandArgs(TRUE)[[3]])
  (sex <- commandArgs(TRUE)[[4]])
  (initial_fit_for_starting_values <- as.logical(commandArgs(TRUE)[[5]]))
  (imp <- commandArgs(TRUE)[[6]])
  (output_dir_parent <- commandArgs(TRUE)[[7]])
} else {
  sex <- 1
}

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs[!funcs %in% "load_sae_shared.R"]) {
  source(paste0(repo, "/functions/", func))
}

if (!exists("initial_fit_for_starting_values")) {
  initial_fit_for_starting_values <- FALSE
}

###### Assign settings from settings file
get_settings(settings_loc)

message("Done with settings")

sink(file = paste0(output_dir, "/model_fitting_", sex, "_", imp, ".txt"), type = "output", split = TRUE)

###### Format data for TMB
cat("\n\n***** Format data\n"); flush.console()
load(paste0(output_dir, "/re_graphs.RData"))
dt_agg <- readRDS(paste0(output_dir, "/data_", imp,".rds"))

#### Load recode mapping
recode <- readRDS(paste0(output_dir, "/recode.rds"))

#### Subset to requested sex
if (by_sex) {
  dt_agg <- dt_agg[sex == recode$sex[sex == get("sex", .GlobalEnv), sex_recode],]
}

#### Add intercept and define num variables
dt_agg[, int := 1L]

load(paste0(output_dir, "/num_vars.RData"))

#### Produce and save prediction frame
# We use the final survey version for prediction
if (by_sex) {
  pred_frame <- as.data.table(expand.grid(int = 1, area = 0:(num_j - 1), year = 0:(num_t - 1), 
                                          sex = recode$sex[sex == get("sex", .GlobalEnv), sex_recode], 
                                          race = 0:(num_r - 1), age = 0:(num_a - 1))) 
  pred_frame$uid <- 1:nrow(pred_frame)
  saveRDS(pred_frame, file = paste0(output_dir, "/pred_frame_", sex, ".rds"))  
} else {
  pred_frame <- as.data.table(expand.grid(int = 1, area = 0:(num_j - 1), year = 0:(num_t - 1), sex = 0:(num_s - 1), race = 0:(num_r - 1), age = 0:(num_a - 1)))  
  pred_frame$uid <- 1:nrow(pred_frame)
  saveRDS(pred_frame, file = paste0(output_dir, "/pred_frame.rds"))  
}

###### Read in the priors
if (is.null(prior_type)) {
  prior_type <- "loggamma"
  prior_list <- list()
  for (i in 1:6) {
    prior_list[[i]] <- c(1, 1000, -3)
  }
}

gen_hyperpriors_parameters(prior_list)

if (!exists("rho_prior")) { # Set default rho prior
  rho_prior <- c(0, 1.5)
}

###### Establish TMB data object
tmb_data <- list(
  Yx_k = dt_agg$rate,
  J_k = dt_agg$area,
  A_k = dt_agg$age,
  K_k = dt_agg$state,
  T_k = dt_agg$year,
  R_k = dt_agg$race,
  
  # age indicators (in terms of age groups)
  U2_k = dt_agg$under_20,
  U4_k = dt_agg$under_5,
  
  dt_agg_rows_k = dt_agg$agg_id,
  agg_wt_xwalk = dt_agg$agg_wt,
  
  X_k = as.matrix(dt_agg[, c("int", covars, covars_as, covars_re), with = FALSE]),
  X1_k = as.matrix(dt_agg[, covars_subpop, with = FALSE]),
  X2_k = as.matrix(dt_agg[, covars_subpop_20plus, with = FALSE]),
  X4_k = as.matrix(dt_agg[, covars_subpop_5plus, with = FALSE]),
  
  graph_a = graph_a,
  graph_t = graph_t,
  
  num_a = num_a,
  num_t = num_t,
  
  re1_prior_param = list(type = prior_type, par1 = re1_par1, par2 = re1_par2),
  
  # this function returns one list of values that is shared for every sub-population covariate random effect.
  # so each covariate has same hyperprior parameters
  covar_subpop_hyperpriors_parameters = gen_covar_subpop_hyperpriors_parameters(prior_type, covar_subpop_hyperpriors_settings),
  covar_subpop_hyperpriors_parameters_20plus = gen_covar_subpop_hyperpriors_parameters(prior_type, covar_subpop_hyperpriors_settings),
  covar_subpop_hyperpriors_parameters_5plus = gen_covar_subpop_hyperpriors_parameters(prior_type, covar_subpop_hyperpriors_settings),
  
  rho_mean = rho_prior[1],
  rho_variance = rho_prior[2],
  
  family = substr(family, 1, 1))

###### Set parameters for TMB
cat("\n\n***** Set parameters\n"); flush.console()

if (fit_model_for_starting_values & !initial_fit_for_starting_values) {
  ###### Load saved model object (assume it is imp = 0)
  model_fit <- readRDS(paste0(output_dir_parent, "/imputation0/model_fit_", sex, "_0.rds"))
  tmb_par <- list(
    logSigma = model_fit$par.fixed[which(names(model_fit$par.fixed) == "logSigma")], # for standard deviation
    B = model_fit$par.fixed[which(names(model_fit$par.fixed) == "B")], # global intercept
    B1 = model_fit$par.fixed[which(names(model_fit$par.fixed) == "B1")], # covariate fixed
    B2 = model_fit$par.fixed[which(names(model_fit$par.fixed) == "B2")], # covariate fixed
    B4 = model_fit$par.fixed[which(names(model_fit$par.fixed) == "B4")], # covariate fixed
    
    re1 = array(model_fit$par.random[which(names(model_fit$par.random) == "re1")], dim = c(num_t, num_a)), # age-year-race-edu-marital-level random intercept
    re1_log_sigma = model_fit$par.fixed[which(names(model_fit$par.fixed) == "re1_log_sigma")],
    logit_rho_1a = model_fit$par.fixed[which(names(model_fit$par.fixed) == "logit_rho_1a")],
    logit_rho_1t = model_fit$par.fixed[which(names(model_fit$par.fixed) == "logit_rho_1t")],
    
    # function below is used for random effects on sub-pop covariate random effects
    covar_subpop_re_matrix = array(model_fit$par.random[which(names(model_fit$par.random) == "covar_subpop_re_matrix")], dim = c(num_a, length(covars_subpop))),
    covar_subpop_log_sigmas = model_fit$par.fixed[which(names(model_fit$par.fixed) == "covar_subpop_log_sigmas")], 
    
    covar_subpop_re_matrix_20plus = array(model_fit$par.random[which(names(model_fit$par.random) == "covar_subpop_re_matrix_20plus")], dim = c(num_a, length(covars_subpop_20plus))),
    covar_subpop_log_sigmas_20plus = model_fit$par.fixed[which(names(model_fit$par.fixed) == "covar_subpop_log_sigmas_20plus")],
    
    covar_subpop_re_matrix_5plus = array(model_fit$par.random[which(names(model_fit$par.random) == "covar_subpop_re_matrix_5plus")], dim = c(num_a, length(covars_subpop_5plus))), 
    covar_subpop_log_sigmas_5plus = model_fit$par.fixed[which(names(model_fit$par.fixed) == "covar_subpop_log_sigmas_5plus")]
  )
} else {
  tmb_par <- list(
    logSigma = 0, # for standard deviation
    B = rep(0, 1), # global intercept
    B1 = rep(0, length(covars_subpop)), # covariate fixed
    B2 = rep(0, length(covars_subpop_20plus)),
    B4 = rep(0, length(covars_subpop_5plus)),
    
    re1 = array(rep(0, num_t * num_a), dim = c(num_t, num_a)), # age-year random intercept
    re1_log_sigma = re1_log_sigma,
    logit_rho_1a = 0,
    logit_rho_1t = 0,
    
    # function below is used for random effects on sub-pop covariate random effects
    covar_subpop_re_matrix = gen_covar_subpop_re_matrix(num_subpop = num_a), 
    covar_subpop_log_sigmas = gen_covar_subpop_log_sigmas(covar_subpop_hyperpriors_settings, covars_subpop),
    
    covar_subpop_re_matrix_20plus = matrix(0, nrow = num_a, ncol = length(covars_subpop_20plus)),
    covar_subpop_log_sigmas_20plus = gen_covar_subpop_log_sigmas(covar_subpop_hyperpriors_settings, covars_subpop_20plus),
    
    covar_subpop_re_matrix_5plus =  matrix(0, nrow = num_a, ncol = length(covars_subpop_5plus)),
    covar_subpop_log_sigmas_5plus = gen_covar_subpop_log_sigmas(covar_subpop_hyperpriors_settings, covars_subpop_5plus)
  )
}

saveRDS(tmb_par, paste0(output_dir, "/model_parameters_", sex, ".rds"))

map <- NULL

message("Done initializing data")

###### Fit model
## Compile CPP code for objective function
TMB::compile(paste0(repo, "/tmb_models/models/mod_", model, ".cpp"))
dyn.load(dynlib(paste0(repo, "/tmb_models/models/mod_", model)))
config(tape.parallel = 0, DLL = paste0("mod_", model))

## Make objective function
cat("\n\n***** Make objective function\n"); flush.console()
obj <- MakeADFun(tmb_data, tmb_par, random = c("re1", 
                                              "covar_subpop_re_matrix",  "covar_subpop_re_matrix_20plus", 
                                              "covar_subpop_re_matrix_5plus"), 
                 DLL = paste0("mod_", model), map = map)
runSymbolicAnalysis(obj)
saveRDS(obj, file = paste0(output_dir, "/obj_", sex, ".rds"))

## Optimize objective function
cat("\n\n***** Optimize objective function\n"); flush.console()
opt_time <- proc.time()
for (method in c("nlminb")) {
  cat(paste("\n  *** Method: ", method, "\n"))
  opt <- nlminb(start = obj$par, objective = function(x) as.numeric(obj$fn(x)), gradient = obj$gr,
                control = list(trace = 6,
                iter.max = 500, # increased max number of iterations b/c we have move
                eval.max = 500))
  
  print(opt)
  if (opt$convergence == 0) break
}

(opt_time <- proc.time() - opt_time)
if (opt$convergence != 0) stop("Model did not converge")

## Get standard errors
cat("\n\n***** Extract standard errors\n"); flush.console()
se_time <- proc.time()
saveRDS(obj$report(), file = paste0(output_dir, "/model_fit_OPT_", sex, ".rds")) # Temporarily save this so that we can see the covariance matrix

out <- sdreport(obj, getJointPrecision = T)
(se_time <- proc.time() - se_time)

## Save model output
cat("\n\n***** Save model output\n"); flush.console()
saveRDS(out, file = paste0(output_dir, "/model_fit_", sex, "_", imp, ".rds"))
fwrite(rbind(se_time, opt_time), file = paste0(output_dir, "/model_fit_time_", sex, "_", imp, ".csv"))
sink()
