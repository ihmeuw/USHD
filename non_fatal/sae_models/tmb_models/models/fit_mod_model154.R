####################################################################################################
## Description: Fit the following model in TMB (separately by sex): (exactly the same as model 148, but specified the MakeADFun call differently so that the fixed effects are not passed as "random")
##
##              C_{j,a,t,r,e,m,d} ~ Binomial(N_{j,a,t,r,e,m,d}, p_{j,a,t,r,e,m,d})
##              logit(p_{j,a,t,r,e,m,d}) = B0 + B*X_{j,t} + u1_{j,t',a',r}*S(a')*S(t') + u2_{j} +  B6_{t'}*S(t')  + B7_{a'}*S(a') + u3_{a,r,e,m}*(U=1) + u5_{d2,r|d2!="BRFSS_LLCP"} + u8_{a,d2|d2!="BRFSS_LLCP"}
##              u1_{j,t',a',r} ~ LCAR:LCAR:LCAR:IID(rho_1j, sigma_1) -- using linear splines with LCAR effects on basis functions.
##              u2_{j} ~ LCAR(rho_2, sigma_2)
##              u3_{a,r,e,m} ~ LCAR:IID:LCAR:IID(rho_3a, rho_3e, sigma_3)
##              u5_{r,d2|d2!="BRFSS_LLCP"} ~ IID(sigma_5)
##              u8_{a,d2|d2!="BRFSS_LLCP"} ~ LCAR:IID(sigma_8, rho_8a)
##              B6_{t'}*S(t') -- using natural splines on time, fixed effect
##              B7_{a'}*S(a') -- using natural splines on age, fixed effect
##              where j = area, a = age group, t = year, r = race, S(a') = age spline, S(t') = age spline, e = education, m = marital status, U = post-stratification indicator (1 = post-stratify by strat_vars), d2 = data source (with BRFSS_LANDLINE_ONLY and BRFSS_LLCP separated)
##              N = population, C = cases (count), and p = prevalence
##
##              k represents combined PUMA-mcnty geographic units or states.
##
##              This model is fit simultaneously to data reported at mcnty, CBSA, and/or state level; CBSA-level data use combined cbsa_mcnty as the
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
.libPaths(c("FILEPATH")) # temporary hack to resolve issue with .libPaths()
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
if (tmb.ok == 1) {
  print("TMB ok - have_tmb_symbolic")
} else {
  print("TMB PROBLEM - have_tmb_symbolic is false. runSymbolicAnalysis WILL NOT DO ANYTHING")
}

###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (settings_loc <- commandArgs(TRUE)[[3]])
  (sex <- commandArgs(TRUE)[[4]])
  (initial_fit_for_starting_values <- commandArgs(TRUE)[[5]])
  (fold <- commandArgs(TRUE)[[6]])
} else{
  repo <- "FILEPATH"
  output_dir <- "/FILEPATH"
  settings_loc <- paste0(output_dir, "/settings.csv")
  fold <- 1
  imp <- 3
  # if string imputation not in the directory name, add it
  if (!grepl("imputation", output_dir)) {
    output_dir <- paste0(output_dir,'/imputation',imp)
  }
  sex <- 2
  initial_fit_for_starting_values <- "FALSE"
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
  gold_standard <- recode$source_v2[source_v2_index == gold_standard_source, source_v2_index_recode]
  # check that gold_standard is found (otherwise, it was probably not correctly specified in the settings file)
  if (length(gold_standard) == 0) {
    stop("Gold standard source not found in source_v2_index_recode")
  }
  # check that the index is zero, otherwise the assumptions when creating the fixed effect on
  #   not-gold standard source will not be met
  if (gold_standard != 0) {
    stop("Gold standard source must be index 0, otherwise the assumptions when creating the fixed effect on not-gold standard source will not be met")
  }
}

#### Produce and save prediction frame
if (by_sex) {
  if (is.null(gold_standard_source) | by_source) {
    pred_frame <- as.data.table(expand.grid(int = 1, area = 0:(num_j - 1), year = 0:(num_t - 1), sex = recode$sex[sex == get("sex", .GlobalEnv), sex_recode], race = 0:(num_r - 1), age = 0:(num_a - 1), edu = 0:(num_e - 1), marital = 0:(num_m - 1), source_v2 = 0:(num_d_v2 - 1)))
  } else {
    pred_frame <- as.data.table(expand.grid(int = 1, area = 0:(num_j - 1), year = 0:(num_t - 1), sex = recode$sex[sex == get("sex", .GlobalEnv), sex_recode], race = 0:(num_r - 1), age = 0:(num_a - 1), edu = 0:(num_e - 1), marital = 0:(num_m - 1), source_v2 = gold_standard))
  }
  pred_frame$uid <- 1:nrow(pred_frame)
  saveRDS(pred_frame, file = paste0(output_dir, "/pred_frame_", sex, ".rds"))  
} else {
  if (is.null(gold_standard_source) | by_source) {
    pred_frame <- as.data.table(expand.grid(int = 1, area = 0:(num_j - 1), year = 0:(num_t - 1), sex = 0:(num_s - 1), race = 0:(num_r - 1), age = 0:(num_a - 1), edu = 0:(num_e - 1), marital = 0:(num_m - 1), source_v2 = 0:(num_d_v2 - 1)))
  } else {
    pred_frame <- as.data.table(expand.grid(int = 1, area = 0:(num_j - 1), year = 0:(num_t - 1), sex = 0:(num_s - 1), race = 0:(num_r - 1), age = 0:(num_a - 1), edu = 0:(num_e - 1), marital = 0:(num_m - 1), source_v2 = gold_standard))
  }
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
  D_V2 = dt_mcnty$source_v2_index, # source split by BRFSS_LLCP and BRFSS_LANDLINE_ONLY
  D_V2_no_gs = dt_mcnty$source_v2_no_gs, # source split by BRFSS_LLCP and BRFSS_LANDLINE_ONLY, excluding the gold standard source
  DR_V2 = dt_mcnty$source_v2_race,
  
  GOLD_STANDARD = dt_mcnty[, as.integer(source_v2_index == gold_standard)],
  
  Y_k = dt_agg_rows$cases,
  N_k = dt_agg_rows$sample_size,
  J_k = dt_agg_rows$area,
  A_k = dt_agg_rows$age,
  T_k = dt_agg_rows$year,
  R_k = dt_agg_rows$race,
  E_k = dt_agg_rows$edu,
  M_k = dt_agg_rows$marital,
  U_k = dt_agg_rows$post_stratify,
  D_V2_k = dt_agg_rows$source_v2_index, # source split by BRFSS_LLCP and BRFSS_LANDLINE_ONLY
  D_V2_no_gs_k = dt_agg_rows$source_v2_no_gs, # source split by BRFSS_LLCP and BRFSS_LANDLINE_ONLY, excluding the gold standard source
  DR_V2_k = dt_agg_rows$source_v2_race,
  GOLD_STANDARD_k = dt_agg_rows[, as.integer(source_v2_index == gold_standard)],
  dt_agg_rows_k = dt_agg_rows$agg_id,
  agg_wt_xwalk = dt_agg_rows$agg_wt,
 
  X = as.matrix(dt_mcnty[, c("int"), with = FALSE]),
  X1 = as.matrix(dt_mcnty[, covars, with = FALSE]),
  X_k = as.matrix(dt_agg_rows[, c("int"), with = FALSE]),
  X1_k = as.matrix(dt_agg_rows[, covars, with = FALSE]),
  
  graph_j = graph_j,
  graph_a = graph_a,
  graph_e = graph_e,
  graph_as = graph_as,
  graph_ts = graph_ts,
  s_age = s_age, # linear spline bases (for RE1)
  s_year = s_year, # linear spline bases (for RE1)
  s_year_ns = s_year_ns_no_intercept, # natural spline bases (for RE6)
  s_age_ns = s_age_ns_no_intercept, # natural spline bases (for RE7)
    
  num_r = num_r,
  num_e = num_e,
  num_m = num_m,
  num_d_v2 = num_d_v2,
  num_dr_v2 = num_dr_v2,
  num_year_spline_ns = ncol(s_year_ns_no_intercept), # dimensions of natural spline (for RE6) -- not needed for the LCAR linear splines b/c we pass in a graph
  num_age_spline_ns = ncol(s_age_ns_no_intercept), # dimensions of natural spline (for RE7) -- not needed for the LCAR linear splines b/c we pass in a graph
  
  re1_prior_param = list(type = prior_type, par1 = re1_par1, par2 = re1_par2),
  re2_prior_param = list(type = prior_type, par1 = re2_par1, par2 = re2_par2),
  re3_prior_param = list(type = prior_type, par1 = re3_par1, par2 = re3_par2),
  re5_prior_param = list(type = prior_type, par1 = re5_par1, par2 = re5_par2),
  re8_prior_param = list(type = prior_type, par1 = re8_par1, par2 = re8_par2),
  
  family = substr(family, 1, 1)
)

###### Set parameters for TMB
cat("\n\n***** Set parameters\n"); flush.console()
if (fit_model_for_starting_values & initial_fit_for_starting_values == "FALSE") {
} else {
  tmb_par <- list(
    B = rep(0, 1), # global intercept
    B1 = rep(0, length(covars)), # covariate fixed
    
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
    
    re5 = rep(0, num_dr_v2), # race-source random intercepts, only for race-source categories present
    re5_log_sigma = re5_log_sigma,

    re8 = array(rep(0, num_a * (num_d_v2 - 1)), dim = c(num_a, (num_d_v2 - 1))), # age-source random intercepts. Do NOT estimate an effect for the gold-standard source
    re8_log_sigma = re8_log_sigma,
    logit_rho_8a = 0,
    
    B6 = rep(0, ncol(s_year_ns_no_intercept)), # year-spline FE
        
    B7 = rep(0, ncol(s_age_ns_no_intercept)) # age-spline FE
      
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
obj <- MakeADFun(tmb_data, 
                tmb_par, 
                random = c(paste0("re", c(1:3,5,8))),  # compared to model 148, do not pass the fixed effects here
                DLL = paste0("mod_", model), 
                map = map,
                inner.control = list(maxit = 200))
runSymbolicAnalysis(obj)

# use retape to restore pointers in obj if you want to investigate in a fresh session
saveRDS(obj, file = paste0(output_dir, "/obj_pre_fit_", sex, ".rds"))

par_names <- names(obj$env$par)
par_names <- par_names[!grepl("rho|sigma|cor|B", par_names)] # adding B to the list of things to remove b/c we are no longer passing the fixed effects to random
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
                control = list(trace = 6,
                               iter.max = 500, # increased max number of iterations b/c we have move
                               eval.max = 500)) # the fixed effect optimization out of inner optimization --> into outer optimization (nlmbinb)
  print(opt)
  opt_time <- proc.time() - opt_time
  print(opt_time)
  
  if (opt$convergence == 0) break
}

if (opt$convergence != 0) stop("Model did not converge")

## Get standard errors
cat("\n\n***** Extract standard errors\n"); flush.console()
se_time <- proc.time()
saveRDS(obj$report(), file = paste0(output_dir, "/model_fit_OPT_", sex, ".rds")) 

out <- sdreport(obj, getJointPrecision = T)
(se_time <- proc.time() - se_time)

## Save model output
cat("\n\n***** Save model output\n"); flush.console()
saveRDS(out, file = paste0(output_dir, "/model_fit_", sex, ".rds"))
fwrite(rbind(se_time, opt_time), file = paste0(output_dir, "/model_fit_time_", sex, ".csv"))
sink()
