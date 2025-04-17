####################################################################################################
## Description: Fit the following model in TMB (separately by sex):
##
##              C_{j,a,t,r} ~ Binomial(N_{j,a,t,r}, p_{j,a,t,r})
##              logit(p_{j,a,t,r,e,m,d}) = B0 + B*X_{j,t} + B2_{d | d != BRFSS} + u1_{j,t',a',r}*S(t')*S(a') + u2_{j} + u3_{a,r,e,m}*(U=1) + u5_{r,a,d} + u6_{t} + u7_{a} + u8_{r} + u9_{t,r,e,m}*(U=1) + u10_{w,r,e,m}*(U=1)
##              u1_{j,t',a',r} ~ LCAR:LCAR:IID(rho_1j, rho_1a, rho_1t, sigma_1)
##              u2_{j} ~ LCAR(rho_2, sigma_2)
##              u3_{a,r,e,m} ~ LCAR:IID:LCAR:IID(rho_at, rho_3e, sigma_3)
##              u5_{a,d_r} ~ LCAR:IID(sigma_5)
##              u6_{t} ~ LCAR(rho_4t, sigma_4)
##              u7_{a} ~ LCAR(rho_4a, sigma_4)
##              u8_{r} ~ IID(sigma_4)
##              u9_{t,r,e,m} ~ LCAR:IID:LCAR:IID(rho_9t, rho_9e, sigma_9)
##              u10_{w,r,e,m} ~ IID:IID:LCAR:IID(sigma_10)
##              where j = area, a = age group, t = year, r = race, S(a') = age spline, S(t') = age spline, e = education, m = marital status, U = post-stratification indicator (1 = post-stratify by strat_vars), d = data source, w = state
##              N = population, C = cases (count), and p = prevalence
##
##              k represents combined PUMA-mcnty geographic units or states.
##
##              This model is fit simultaneously to data reported at mcnty, PUMA, and/or state level; PUMA-level data use combined puma_mcnty as the
##              geographic units.
##
## Passed args: output_dir [character] -- home directory for settings and final output
##
## Requires:    prepped data file ("[output_dir]/data.rds")
##              random effect graphs ("[output_dir]/re_graphs.rdata")
##
## Outputs:     fitted model object ("[output_dir]/model_fit_test.rds")
##              model-fitting log ("[output_dir]/model_fitting_test.txt")
##              model-fitting timings ("[output_dir]/model_fit_time_test.rds")
##
####################################################################################################
###### Load required libraries
pacman::p_load(R.utils, data.table, splines, dplyr)

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

tmb.ok <- .Call("have_tmb_symbolic",PACKAGE="TMB")
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
  (initial_fit_for_starting_values <- commandArgs(TRUE)[[5]])
  (fold <- commandArgs(TRUE)[[6]])
} else {
  sex <- 1
  imp <- 0
}

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs[funcs != "load_sae_shared.R"]) {
  source(paste0(repo, "/functions/", func))
}

if (!exists("initial_fit_for_starting_values")) {
  initial_fit_for_starting_values <- FALSE
}

###### Assign settings from settings file
get_settings(settings_loc)
message("Done with settings")

#### Set output_dir for validation runs
if (type == "validation" & n_folds > 1) {
  output_dir <- paste0(output_dir, "/fold_", fold)
}

message(output_dir)

###### Load objects
cat("\n\n***** Load objects\n"); flush.console()
load(paste0(output_dir, "/re_graphs.RData"))
load(paste0(output_dir, "/num_vars.RData"))
dt <- readRDS(paste0(output_dir, "/data.rds"))
recode <- readRDS(paste0(output_dir, "/recode.rds"))

#### Subset to requested sex
if (by_sex) {
  dt <- dt[sex == recode$sex[sex == get("sex", .GlobalEnv), sex_recode],]
}

#### Set cases for requested holdout fold to NA
if (n_folds > 1) {
  dt[holdout == fold, cases := NA_real_]
}

#### Get index of gold standard source
if (!is.null(gold_standard_source)) {
  gold_standard <- recode$source[source_index == gold_standard_source, source_index_recode]
}

###### Get mcnty:state mapping
locs <- fread("FILEPATH")

#### Produce and save prediction frame
if (by_sex) {
  if (is.null(gold_standard_source) | by_source) {
    pred_frame <- as.data.table(expand.grid(int = 1, area = 0:(num_j - 1), year = 0:(num_t - 1), sex = recode$sex[sex == get("sex", .GlobalEnv), sex_recode], race = 0:(num_r - 1), age = 0:(num_a - 1), edu = 0:(num_e - 1), marital = 0:(num_m - 1), source = 0:(num_d - 1)))
  } else {
    pred_frame <- as.data.table(expand.grid(int = 1, area = 0:(num_j - 1), year = 0:(num_t - 1), sex = recode$sex[sex == get("sex", .GlobalEnv), sex_recode], race = 0:(num_r - 1), age = 0:(num_a - 1), edu = 0:(num_e - 1), marital = 0:(num_m - 1), source = gold_standard))
  }
  
  # Merge state onto pred_frame
  temp <- unique(recode$state[recode$area[locs, on = "mcnty", c("mcnty", "area_recode", "state")], on = "state"][, c("area_recode", "state_recode")])
  setnames(temp, c("area_recode", "state_recode"), c("area", "state"))
  pred_frame <- pred_frame[temp, on = "area"]
  
  pred_frame$uid <- 1:nrow(pred_frame)
  saveRDS(pred_frame, file = paste0(output_dir, "/pred_frame_", sex, ".rds"))
} else {
  if (is.null(gold_standard_source) | by_source) {
    pred_frame <- as.data.table(expand.grid(int = 1, area = 0:(num_j - 1), year = 0:(num_t - 1), sex = 0:(num_s - 1), race = 0:(num_r - 1), age = 0:(num_a - 1), edu = 0:(num_e - 1), marital = 0:(num_m - 1), source = 0:(num_d - 1)))
  } else {
    pred_frame <- as.data.table(expand.grid(int = 1, area = 0:(num_j - 1), year = 0:(num_t - 1), sex = 0:(num_s - 1), race = 0:(num_r - 1), age = 0:(num_a - 1), edu = 0:(num_e - 1), marital = 0:(num_m - 1), source = gold_standard))  
  }
  
  # Merge state onto pred_frame
  temp <- unique(recode$state[recode$area[locs, on = "mcnty", c("mcnty", "area_recode", "state")], on = "state"][, c("area_recode", "state_recode")])
  setnames(temp, c("area_recode", "state_recode"), c("area", "state"))
  pred_frame <- pred_frame[temp, on = "area"]
  
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

if (prior_type %in% c("pc", "loggamma", "half_normal")) {
  for (j in 1:length(prior_list)) {
    assign(paste0("re", j, "_par1"), prior_list[[j]][1])
    assign(paste0("re", j, "_par2"), prior_list[[j]][2])
    assign(paste0("re", j, "_log_sigma"), prior_list[[j]][3])
  }
}

###### Set data objects
dt_mcnty <- dt[level == "mcnty"]
dt_agg_rows <- dt[level == "aggregate"]

## Standardize covars
covars <- gsub("_by_race_ethn", "", covars)

if (nrow(dt_agg_rows) == 0) {
  dt_agg_rows <- data.table(cases = numeric(), sample_size = numeric(), area = numeric(), age = numeric(), year = numeric(), race = numeric(), edu = numeric(), marital = numeric(), post_stratify = numeric(), source_index = numeric(), agg_id = numeric(), agg_wt = numeric(), int = numeric())
  dt_agg_rows <- cbind(dt_agg_rows, setNames(data.table(matrix(nrow = 0, ncol = length(covars))), covars))
}

###### Sort aggregate data
setkeyv(dt_agg_rows, "agg_id")

###### Save data sets
saveRDS(dt_mcnty, file = paste0(output_dir, "/data_mcnty_", sex, ".rds"))
saveRDS(dt_agg_rows, file = paste0(output_dir, "/data_agg_rows_", sex, ".rds"))

###### Establish TMB data object
tmb_data <- list(
  Y = dt_mcnty$cases,
  N = dt_mcnty$sample_size,
  J = dt_mcnty$area,
  A = dt_mcnty$age,
  T = dt_mcnty$year,
  R = dt_mcnty$race,
  E = dt_mcnty$edu,
  M = dt_mcnty$marital,
  U = dt_mcnty$post_stratify,
  DR = dt_mcnty$source_race,
  W = dt_mcnty$state,
  
  Gallup = dt_mcnty[, as.integer(source_index == recode$source[source_index == "Gallup", source_index_recode])],
  GOLD_STANDARD = dt_mcnty[, as.integer(source_index == gold_standard)],
  
  Y_k = dt_agg_rows$cases,
  N_k = dt_agg_rows$sample_size,
  J_k = dt_agg_rows$area,
  A_k = dt_agg_rows$age,
  T_k = dt_agg_rows$year,
  R_k = dt_agg_rows$race,
  E_k = dt_agg_rows$edu,
  M_k = dt_agg_rows$marital,
  U_k = dt_agg_rows$post_stratify,
  DR_k = dt_agg_rows$source_race,
  W_k = dt_agg_rows$state,
  
  dt_agg_rows_k = dt_agg_rows$agg_id,
  agg_wt_xwalk = dt_agg_rows$agg_wt,
  
  Gallup_k = dt_agg_rows[, as.integer(source_index == recode$source[source_index == "Gallup", source_index_recode])],
  GOLD_STANDARD_k = dt_agg_rows[, as.integer(source_index == gold_standard)],
  
  X = as.matrix(dt_mcnty[, c("int"), with = FALSE]),
  X1 = as.matrix(dt_mcnty[, covars, with = FALSE]),
  X_k = as.matrix(dt_agg_rows[, c("int"), with = FALSE]),
  X1_k = as.matrix(dt_agg_rows[, covars, with = FALSE]),
  
  graph_j = graph_j,
  graph_t = graph_t,
  graph_a = graph_a,
  graph_e = graph_e,
  s_age = s_age,
  s_year = s_year,
  graph_as = graph_as,
  graph_ts = graph_ts,
  
  num_r = num_r,
  num_e = num_e,
  num_m = num_m,
  num_d = num_d,
  num_dr = num_dr,
  num_w = num_w,
  
  re1_prior_param = list(type = prior_type, par1 = re1_par1, par2 = re1_par2),
  re2_prior_param = list(type = prior_type, par1 = re2_par1, par2 = re2_par2),
  re3_prior_param = list(type = prior_type, par1 = re3_par1, par2 = re3_par2),
  re5_prior_param = list(type = prior_type, par1 = re5_par1, par2 = re5_par2),
  re6_prior_param = list(type = prior_type, par1 = re6_par1, par2 = re6_par2),
  re7_prior_param = list(type = prior_type, par1 = re7_par1, par2 = re7_par2),
  re8_prior_param = list(type = prior_type, par1 = re8_par1, par2 = re8_par2),
  re9_prior_param = list(type = prior_type, par1 = re9_par1, par2 = re9_par2),
  re10_prior_param = list(type = prior_type, par1 = re10_par1, par2 = re10_par2),
  
  family = substr(family, 1, 1)
)

###### Set parameters for TMB
cat("\n\n***** Set parameters\n"); flush.console()
if (fit_model_for_starting_values & initial_fit_for_starting_values == "FALSE") {
} else {
  tmb_par <- list(
    B = rep(0, 1), # global intercept
    B1 = rep(0, length(covars)), # covariate fixed
    
    B2_Gallup = rep(0, 1),
    
    re1 = array(rep(0, num_j * num_year_spline * num_age_spline * num_r), dim = c(num_year_spline, num_age_spline, num_j, num_r)),
    re1_log_sigma = re1_log_sigma,
    logit_rho_1j = 0,
    logit_rho_1t = 0,
    logit_rho_1a = 0,
    
    re2 = rep(0, num_j), # area-level random intercept
    re2_log_sigma = re2_log_sigma,
    logit_rho_2 = 0,
    
    re3 = array(rep(0, num_a * num_r * num_e * num_m), dim = c(num_a, num_r, num_e, num_m)), # age-race-edu-marital-level random intercept
    re3_log_sigma = re3_log_sigma,
    logit_rho_3a = 0,
    logit_rho_3e = 0,
    
    re5 = array(rep(0, num_dr * num_a), dim = c(num_dr, num_a)), # race-age-source random intercepts, only for race-source categories present
    re5_log_sigma = re5_log_sigma,
    logit_rho_5a = 0,
    
    re6 = rep(0, num_t), # year-level random intercept
    re6_log_sigma = re6_log_sigma,
    logit_rho_6t = 0,
    re7 = rep(0, num_a), # age-level random intercept
    re7_log_sigma = re7_log_sigma,
    logit_rho_7a = 0,

    re8 = rep(0, num_r), # race-level random intercept
    re8_log_sigma = re8_log_sigma,
    
    re9 = array(rep(0, num_t * num_r * num_e * num_m), dim = c(num_t, num_r, num_e, num_m)), # year-race-edu-marital-level random intercept
    re9_log_sigma = re9_log_sigma,
    logit_rho_9t = 0,
    logit_rho_9e = 0,
    
    re10 = array(rep(0, num_w * num_r * num_e * num_m), dim = c(num_w, num_r, num_e, num_m)), # state-race-edu-marital-level random intercept
    re10_log_sigma = re10_log_sigma,
    logit_rho_10e = 0
  )
}

saveRDS(tmb_par, paste0(output_dir, "/model_parameters_", sex, ".rds"))

map <- NULL

message("Done initializing data")

###### Fit model
## Compile CPP code for objective function
TMB::compile(paste0(repo, "/tmb_models/models/mod_", model, ".cpp"), longint = TRUE)
dyn.load(dynlib(paste0(repo, "/tmb_models/models/mod_", model)))
config(tape.parallel = 0, DLL = paste0("mod_", model))

## Make objective function
cat("\n\n***** Make objective function\n"); flush.console()
obj <- MakeADFun(tmb_data, tmb_par, random = c("B", "B1", "B2_Gallup", paste0("re", c(1:3, 5:10))), DLL = paste0("mod_", model), map = map)
runSymbolicAnalysis(obj)

par_names <- names(obj$env$par)
par_names <- par_names[!grepl("rho|sigma|cor", par_names)]
hess <- obj$env$spHess(random = TRUE)
rownames(hess) <- par_names
colnames(hess) <- par_names
saveRDS(hess, file = paste0(output_dir, "/hess_", sex, ".rds"))

## Optimize objective function
cat("\n\n***** Optimize objective function\n"); flush.console()
opt_time <- proc.time()
for (method in c("nlminb")) {
  
  opt_time <- proc.time()
  message("Trying nlminb...")
  opt <- nlminb(start = obj$par, objective = function(x) as.numeric(obj$fn(x)), gradient = obj$gr,
                control = list(trace = 6))
  print(opt)
  opt_time <- proc.time() - opt_time
  print(opt_time)
  
  if (opt$convergence == 0) break
}

if (opt$convergence != 0) stop("Model did not converge")

## Get standard errors
cat("\n\n***** Extract standard errors\n"); flush.console()
se_time <- proc.time()
saveRDS(obj$report(), file = paste0(output_dir, "/model_fit_OPT_", sex, ".rds")) # Temporarily save this so that we can see the covariance matrix

out <- sdreport(obj, getJointPrecision = T)
(se_time <- proc.time() - se_time)

## Save model output
cat("\n\n***** Save model output\n"); flush.console()
saveRDS(out, file = paste0(output_dir, "/model_fit_", sex, ".rds"))
fwrite(rbind(se_time, opt_time), file = paste0(output_dir, "/model_fit_time_", sex, ".csv"))
sink()
