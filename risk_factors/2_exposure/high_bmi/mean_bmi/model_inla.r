####################################################################################################
## Description: Run INLA model (at the state level) for mean BMI by imputation
##              
##              Uses settings code from launch_risks.R
####################################################################################################

if (!interactive()) {
  output_dir <- commandArgs(TRUE)[[1]]
  output_dir_draws_est <- commandArgs(TRUE)[[2]]
  settings_file <- commandArgs(TRUE)[[3]]
  imp <- commandArgs(TRUE)[[4]]
  s <- commandArgs(TRUE)[[5]]
} else {
  imp <- 1
  s <- 1
}

# set up ------------------------------------------------------------------
library(data.table)
library(Hmisc)
INLA_loc <- NULL

#### Load INLA package
if (!is.null(INLA_loc)) { # Load INLA version from user-specified path
  library(INLA, lib.loc = INLA_loc)
} else { # Load INLA version from Singularity image
  library(INLA)
}

message('packages loaded')

###### Source functions
nonfatal_repo <- paste0("FILEPATH")
funcs <- list.files(paste0(nonfatal_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(nonfatal_repo, "/functions/", func)))
}

##### Set data location and settings locations, and read in settings
settings_path <- "FILEPATH"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc)

message('settings loaded')

if (race_code_set =='old') {
  stopifnot(race_all == 9)
} else if (race_code_set =='db') {
  stopifnot(race_all == 1)
}

# import model indexes
# This table links each model ID number in the settings file to a model formula to provide a less error prone, central system for tracking models
models <- setDT(read.csv('FILEPATH'))

# import data 
combined_dt <- readRDS(paste0(output_dir, "/combined_dt.RDS"))
combined_dt <- combined_dt[!is.na(race) & !is.na(sex) & !is.na(age) & !is.na(year)] #exclude age 98/99. Do NOT include race != all_race b/c all_race is 1, but race now refers to the race variables indexed to 1 (so all_race is now NA) 
combined_dt <- combined_dt[sex == s]

if (isTRUE(include_underwt)) {
  setnames(combined_dt, paste0(c('pred_bmi_', 'obese_category_', 'overwt_category_', 'underwt_category_'), imp),
           c('pred_bmi', 'obese', 'overwt', 'underwt'))
} else {
  setnames(combined_dt, c(paste0('pred_bmi_', imp), paste0('obese_', imp), paste0('obese_category_', imp), 
                          paste0('overwt_', imp), paste0('overwt_category_', imp), paste0('obese_prop_', imp), paste0('obese_prop_category_', imp), paste0('overwt_not_obese_', imp), paste0('overwt_not_obese_category_', imp)), c('pred_bmi', 'obese', 'obese_category', 'overwt', 'overwt_category', 'obese_prop', 'obese_prop_category', 'overwt_not_obese', 'overwt_not_obese_category'))
}

if (!is.null(collapse_vars)) {
  combined_dt[, (collapse_vars) := NA] 
}

combined_dt[, `:=`(sex2 = sex, sex3 = sex, sex4 = sex, sex5 = sex, race2 = race, age2 = age, age3 = age,
                   race3 = race, race4 = race, race5 = race, overwt2 = overwt, overwt3 = overwt, obese2 = obese, obese3 = obese)]

## Fit INLA model
# fit the model on each imputed set
message("mean model:\n", mean_model)

# obtain model formula from mean_model ID number
formula <- as.formula(models[model == mean_model]$formula)

start.time <- Sys.time()

if (!use_gam) {
  mod <- inla(formula,
              data = combined_dt,
              family = "gaussian",
              scale = sample_size,
              control.predictor = list(compute = TRUE, link = 1), #Boolean variable that indicates whether the marginal densities for the linear predictor should be computed
              control.compute = list(config = TRUE, waic = TRUE, cpo = FALSE),
              control.inla = list(strategy = "gaussian", int.strategy = "eb"),
              verbose = TRUE)
  
  summary(mod)
  print(mod$logfile) # use this to get the verbose output; want to store it so that we can check eigenvalues
  
} else if (use_gam) {
  library(mgcv)
  mod <- gam(formula, data = combined_dt, weights = sqrt(sample_size))
}

print(Sys.time())

end.time <- Sys.time()

print(second_round <- end.time - start.time)

#### Save model object to disk
saveRDS(mod, file = paste0(output_dir_draws_est, "/mod", imp, "sex", s, ".RDS"))
message('mod saved')

if (!use_gam) {
  # Retrieve predictions
  stopifnot(nrow(mod$summary.fitted.values) == combined_dt[, .N])
  # NOTE -- combined_dt is not square, so this only makes preds in strata with observations
  if (models[model == mean_model]$formula %like% "log\\(pred_bmi\\)") {
    preds <- cbind(combined_dt, exp(mod$summary.fitted.values[1:nrow(combined_dt),]$mean)) # Bind to summary predictions from INLA model
  } else {
    preds <- cbind(combined_dt, mod$summary.fitted.values[1:nrow(combined_dt),]$mean) # Bind to summary predictions from INLA model
  }
  setnames(preds, "V2", "pred_mean")
  
  saveRDS(preds, paste0(output_dir_draws_est, "/preds", imp, "sex", s, ".RDS"))
  
  #################################################################################################
  #generate draws
  set.seed(123456) #add 6 for imp 6 (testing)
  
  message('generate draws')
  
  draws <- inla.posterior.sample(100, mod)
  predictors <- nrow(combined_dt) 
  start <- predictors + 1
  
  registerDoParallel(6)
  effects_draws <- foreach(i = 1:length(draws), .combine = 'cbind') %dopar% {
    draw <- draws[i][[1]]$latent
    terms <- draw[start:length(draw),, drop = FALSE]
  }
  
  #create age 14 rows: age14 corresponds to ages 85+, we are applying the same parameters from age 80+ to ages 80-84 and 85+
  if (models[model == mean_model]$formula %like% 'as.integer\\(year\\), model = "ar1", group = as.integer(age)') {
    aa <-seq(length(years),(length(years)*(length(ages)-1)),by=length(years))
    age14 <- effects_draws[c(paste0('as.integer(year):',aa)),]
    rownames(age14) <- c(paste0('as.integer(year):',aa+1))
    # for year/age interaction (in this order) the length of the interaction term is #years * #age groups (up to 80 since 80+ is aggregated in the model input across all years)
    # to duplicate the same interaction parameter value for ages 80 and 85, we basically insert the parameter value after each age 80 parameter
    exaa <- (length(years)+1):(length(years)*(length(ages)-1))
    exaa <- exaa[! exaa %in% aa]
    rownames(effects_draws[c(paste0('as.integer(year):',exaa)),]) <- c(paste0('as.integer(year):',exaa+1))
    effects_draws <- rbind(effects_draws, age14)
    rownames(effects_draws)
  } else if (models[model==mean_model]$formula %like% "as.integer\\(age\\), model = 'ar1', group = race" | 
             (models[model==mean_model]$formula %like% "as.integer\\(age\\), model = 'iid', group = race4") | 
             (models[model==mean_model]$formula %like% "\\(as.integer\\(age\\), model = 'ar', order = 2, group = race4")) {
    aa <-seq(length(ages)-1,(length(ages)-1)*length(races),by=length(ages)-1)
    #as.integer(age) is the random effect for a specific age–race combination
    # for year/age interaction (in this order) the length of the interaction term is #age groups (up to 80) * #race groups
    # to duplicate the same interaction parameter value for ages 80 and 85, we basically insert the parameter value after each age 80 parameter
    age14 <- effects_draws[c(paste0('as.integer(age):',aa)),]
    rownames(age14) <- c(paste0('as.integer(age):',aa+which(aa %in% aa)))
    exaa <- (length(ages)-1):((length(ages)-1)*length(races))
    # reindexes the as.integer(age): rows to "make room" for the age 85 effects
    for(a in 1:(length(aa)-1)){
      message(a)
      sub <- c(exaa[exaa<=aa[a+1] & exaa>aa[a]])
      effects <- rbind(effects_draws[c(paste0('as.integer(age):',sub)),])
      rownames(effects) <- c(paste0('as.integer(age):',sub + a))
      assign(paste0('effects',a), effects)
    } 
    list_effects <- list(effects_draws[rownames(effects_draws) %in% c(paste0('as.integer(age):', 1:min(aa))),], effects1, effects2, effects3, effects4, age14)
    effects_draws_age <- do.call('rbind', list_effects)
    effects_draws <- rbind(effects_draws[!rownames(effects_draws) %in% grep('as.integer(age):', c(rownames(effects_draws)), value=T, fixed = T),], effects_draws_age)
    rownames(effects_draws)
    grep('age', rownames(effects_draws), value=T)
  } else if (models[model == mean_model]$formula %like% "as.integer\\(age\\), model = 'iid'") {
    age14 <- effects_draws['as.integer(age):13',, drop = F]
    rownames(age14) <- c('as.integer(age):14')
    effects_draws <- rbind(effects_draws, age14)
  } 
  
  if (models[model==mean_model]$formula %like% 'as.integer\\(age2\\), overwt') {
    age14 <- effects_draws['as.integer(age2):13',, drop = F] # use drop = F so that this is no coerced into a vector
    rownames(age14) <- c('as.integer(age2):14')
    effects_draws <- rbind(effects_draws, age14)
  }
  
  if (models[model==mean_model]$formula %like% 'as.integer\\(age3\\), obese') {
    age14 <- effects_draws['as.integer(age3):13',, drop = F]
    rownames(age14) <- c('as.integer(age3):14')
    effects_draws <- rbind(effects_draws, age14)
  }
  
  if ((models[model == mean_model]$formula %like% "as.integer\\(age2\\), model = 'iid', replicate = as.integer\\(as.factor\\(sex3\\)\\)") |
      (models[model == mean_model]$formula %like% "as.integer\\(age2\\), model = 'ar1', group = as.integer\\(as.factor\\(sex3\\)\\), control.group = list\\(model = 'iid'\\)\\)") |
      (models[model == mean_model]$formula %like% "as.integer\\(age2\\), model = 'ar', order = 2, group = as.integer\\(as.factor\\(sex3\\)\\), control.group = list\\(model = 'exchangeable'\\), constr = TRUE")) {

    age14 <- effects_draws['as.integer(age2):13',, drop = F]
    rownames(age14) <- c('as.integer(age2):14')
    
    age28 <- effects_draws['as.integer(age2):26',, drop = F]
    rownames(age28) <- c('as.integer(age2):28')
    
    effects_draws_temp <- effects_draws[c(paste0('as.integer(age2):', 14:26)),]
    rownames(effects_draws_temp) <- c(paste0('as.integer(age2):', 15:27))
    
    effects_draws <- rbind(effects_draws[which(!(rownames(effects_draws) %in% c(paste0('as.integer(age2):', 14:26)))),], effects_draws_temp)
    effects_draws <- rbind(effects_draws, age14)
    effects_draws <- rbind(effects_draws, age28)
  }
  
  # check that  rownames are unique, otherwise there is an error
  if (length(unique(rownames(effects_draws))) != nrow(effects_draws)) {
    stop("Row names are not unique")
  }
  
  #save draws
  saveRDS(effects_draws, paste0(output_dir_draws_est, "/effects_draws", imp, "sex", s, ".RDS"))
}

message('SCRIPT COMPLETED')
