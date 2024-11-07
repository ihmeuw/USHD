
## Description: Fit the following model in TMB:
##
##              D_{j,a,t} ~ Poisson(m_{j,a,t} * P_{j,a,t})
##              log(m_{j,a,t}) = B0 + B*X_{j,t} + u1_{j,a',t',r}*S(a')*S(t') + u2_{j} + u3_{a,t,r} +
##                u4_{j}*I + u5_{j}*I + u6_{a}*I + u7_{a}*I
##
##              u1_{j,a',t',r} ~ LCAR:LCAR:LCAR:IID(rho_1j, rho_1a, rho_1t, rho_1j, sigma_1)
##              u2_{j,a,r} ~ LCAR:IID:IID(rho_2, sigma_2)
##              u3_{a,t,r} ~ LCAR:LCAR:IID(rho_3a, rho_3t, sigma_3)
##              u4_{j} ~ IID(sigma_4)
##              u5_{j} ~ IID(sigma_5)
##              u6_{a} ~ IID(sigma_6)
##              u7_{a} ~ IID(sigma_7)
##              where j = area, a = age group, t = year, r = race, S(a') = age spline, S(t') = time spline, D = deaths, P = population, and m = mx
##
## Passed args: dir [character] -- home directory for settings and final output
##              sex [integer] -- sex to run model for
##              race [integer] -- race/ethnicity to run model for
##              edu [integer] -- education group to run model for
##
## Requires:    prepped data file ("[FILEPATH]/data.rds") from limited use directory
##              random effect graphs ("[dir]/re_graphs.rdata")
##
## Outputs:     fitted model object ("[dir]/model_fit_[sex]_[race]_[edu].rds")
##              model-fitting log ("[dir]/model_fitting_[sex]_[race]_[edu].txt")
##              model-fitting timings ("[dir]/model_fit_time_[sex]_[race]_[edu].rds")
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

message("Done with settings")

get_settings(dir)

current_dir <- gsub("FILEPATH", "", dir)
lu_root <- paste0("FILEPATH",LU_folder,"FILEPATH")
lu_modeldir <- paste0(lu_root,"/",current_dir)

# check that the settings were created before the raw data
if(!(check_settings_made_before_data(dir = dir, ludir = lu_modeldir))) {
  stop("settings.csv was prepared before data.rds. Re-run prep_inputs.r to create data.rds")
}

sink(file = paste0(dir, "/model_fitting_", sex, "_", race, "_", edu, ".txt"), type = "output", split = T)

## Format data for TMB -----------------------------------------------------------------------------
cat("\n\n***** Format data\n"); flush.console()
load(paste0(dir, "/re_graphs.rdata"))
data <- readRDS(paste0(lu_modeldir, "/data.rds"))

# index the data
data <- index_edu_race(data)

# define the number of each variable
counts <- count_variables(data)
for(i in unique(1:length(counts))) {
  assign(names(counts[i]), counts[[i]])
}


## setup spline bases on age and time
if (!is.null(year_knots_num) | !is.null(year_knots_spec)) {
  if(!is.null(year_knots_spec)) {
    time_spline_info <- build_time_spline(years, year_knots_spec = year_knots_spec,
                                          detach_spline_19_21 = detach_spline_19_21,
                                          detach_spline_19_22 = detach_spline_19_22,
                                          dir = dir, sex = sex, race = race, edu = edu)
  } else {
    time_spline_info <- build_time_spline(years, year_knots_num = year_knots_num,
                                          detach_spline_19_21 = detach_spline_19_21,
                                          detach_spline_19_22 = detach_spline_19_22,
                                          dir = dir, sex = sex, race = race, edu = edu)
  }
  
  s_year <- time_spline_info[[1]]
  graph_ts <- time_spline_info[[2]]
  num_year_spline <- time_spline_info[[3]]
}

if (!is.null(age_knots_spec)) {
  age_spline_info <- build_age_spline(ages, age_knots_spec,
                                      dir = dir, sex = sex, race = race, edu = edu)
  s_age <- age_spline_info[[1]]
  graph_as <- age_spline_info[[2]]
  num_age_spline <- age_spline_info[[3]]
}


if(pop_add1) {
  if("adjust_pop" %in% names(data)) {
    
    message("Adjusting 0 population based on pop and deaths from all-cause mortality")
    data[adjust_pop == 1, pop := 1]
    
  } else {
    
    message("Adjusting 0 population based on pop and deaths for this cause")
    data[pop == 0 & deaths > pop & !is.na(deaths), pop := 1]
    
  }
  
}

gen_hyperpriors_parameters(prior_list)


ii <- which(data$pop > 0)
tmb_data <- list(
  Y = data$deaths[ii],
  N = data$pop[ii],
  J = data$area[ii],
  A = data$age[ii],
  T = data$year[ii],
  R = data$race[ii],
  X = as.matrix(data[ii, c("int", covars, covars_as, covars_subpop), with = F]),
  I_first = data$indic_1[ii], # indicator for 2001
  I_second = data$indic_2[ii], # indicator for 2005
  graph_j = graph_j,
  graph_t = graph_t,
  graph_a = graph_a,
  s_age = s_age,
  s_year = s_year,
  graph_as = graph_as,
  graph_ts = graph_ts,
  num_r = num_r,
  num_a = num_a,
  re1_prior_param = list(type = prior_type, par1 = re1_par1, par2 = re1_par2),
  re2_prior_param = list(type = prior_type, par1 = re2_par1, par2 = re2_par2),
  re3_prior_param = list(type = prior_type, par1 = re3_par1, par2 = re3_par2),
  re4_prior_param = list(type = prior_type, par1 = re4_par1, par2 = re4_par2),
  re5_prior_param = list(type = prior_type, par1 = re5_par1, par2 = re5_par2),
  re6_prior_param = list(type = prior_type, par1 = re6_par1, par2 = re6_par2),
  re7_prior_param = list(type = prior_type, par1 = re7_par1, par2 = re7_par2))

## Set parameters for TMB --------------------------------------------------------------------------
cat("\n\n***** Set parameters\n"); flush.console()
tmb_par <- list(
  B = rep(0, length(covars) + length(covars_as) + length(covars_subpop) + 1), # intercept & covariate effects
  
  re1 = array(rep(0, num_j * num_year_spline * num_age_spline * num_r), dim = c(num_year_spline, num_age_spline, num_j, num_r)),
  re1_log_sigma = re1_log_sigma,
  logit_rho_1j = 0,
  logit_rho_1t = 0,
  logit_rho_1a = 0,
  
  re2 = array(rep(0, num_a * num_j * num_r), dim = c(num_a, num_j, num_r)), # area/race/age-level random intercept
  logit_rho_2j = 0,
  re2_log_sigma = re2_log_sigma,
  
  re3 = array(rep(0, num_t * num_a * num_r), dim = c(num_t, num_a, num_r)), # age-year-race-level random intercept
  re3_log_sigma = re3_log_sigma,
  logit_rho_3t = 0,
  logit_rho_3a = 0,
  
  re4 = rep(0, num_j), # area-level random intercept (IID) to be multiplied by the indicator
  re4_log_sigma = re4_log_sigma,
  
  re5 = rep(0, num_j), # area-level random intercept (IID) to be multiplied by the indicator
  re5_log_sigma = re5_log_sigma,
  
  re6 = rep(0, num_a), # age-level random intercept (IID) to be multiplied by the indicator
  re6_log_sigma = re6_log_sigma,
  
  re7 = rep(0, num_a), # age-level random intercept (IID) to be multiplied by the indicator
  re7_log_sigma = re7_log_sigma)

saveRDS(tmb_par, paste0(dir, "/model_parameters_", sex, "_", race, "_", edu, ".rds"))

map <- NULL

message("Done initializing data")

## Fit model ---------------------------------------------------------------------------------------
# compile CPP code for objective function
TMB::compile("tmb_models/mod_spline_iid_race_age_mcnty_two_indics.cpp")

dyn.load(dynlib("tmb_models/mod_spline_iid_race_age_mcnty_two_indics"))
config(tape.parallel = 0, DLL = "mod_spline_iid_race_age_mcnty_two_indics")

# make objective function
cat("\n\n***** Make objective function\n"); flush.console()
obj <- MakeADFun(tmb_data, tmb_par, random = c("B", paste0("re", 1:7)), DLL = "mod_spline_iid_race_age_mcnty_two_indics", map = map)
runSymbolicAnalysis(obj)

# optimize objective function
cat("\n\n***** Optimize objective function\n"); flush.console()
opt_time <- proc.time()
for (method in c("nlminb", "L-BFGS-B", "Nelder-Meade", "CG")) {
  cat(paste("\n  *** Method: ", method, "\n"))
  opt <- optimx(par = obj$par, fn = function(x) as.numeric(obj$fn(x)), gr = obj$gr,
                method = method, control = list(iter.max = 500, eval.max = 500), itnmax=200)
  print(opt)
  if (opt$convcod == 0) break
}

(opt_time <- proc.time() - opt_time)
if (opt$convcod != 0) stop("Model did not converge")

# get standard errors
cat("\n\n***** Extract standard errors\n"); flush.console()
se_time <- proc.time()
saveRDS(obj$report(),  file = paste0(dir, "/model_fit_OPT_", sex, "_", race, "_", edu, ".rds")) # temporarily save this so that we can see the covariance matrix

out <- sdreport(obj, getJointPrecision = T)
(se_time <- proc.time() - se_time)

# save model output
cat("\n\n***** Save model output\n"); flush.console()
saveRDS(out, file = paste0(dir, "/model_fit_", sex, "_", race, "_", edu, ".rds"))
saveRDS(rbind(se_time, opt_time), file = paste0(dir, "/model_fit_time_", sex, "_", race, "_", edu, ".rds"))
sink()
