####################################################################################################
## Description: Define a function to impute/smooth covariates by county-year-race/ethnicity, prior
##              to their inclusion as predictors in fatal, non-fatal, or risk factor SAE models.
##              An SAE model is fit in which the input covariate values are treated as model outcomes.
##              Model predictions are returned.
##
## Inputs:      covar [data.table] - data table with raw covariate values and other variables as
##                appropriate (required for all models: "mcnty" for merged county ID, "year" for
##                calendar year; context-specific: "moe" for margin of error, "pop" for population
##                size)
##
##              covariate_name [character] - name of the covariate, conforming to the names of the covariate
##                prep files (e.g., "poverty_by_race_ethn")
##
##              outcome_name [character] - name of the column in covar that represents the outcome variable
##                (e.g., "poverty")
##
##              family [character] - likelihood family for modeling (current options include "gaussian" and
##                "nbinomial")
##
##              save_plots [boolean] - should diagnostic plots be saved to disk after model fitting?
##
##              output_loc [character] - path to location where files should be saved
##
##              pardiso_path [character] - path to location of license for optional PARDISO matrix solver
##
##              INLA_loc [character] - path to location of optional personal installation of the INLA package,
##                if a different version is required than what is provided in the Singularity image (see
##                notes below about how to install this)
##
##              lib_loc [character] - path to location of central R library
##              
##              load_saved_intermediate [boolean] - load saved intermediate files from disk and skip to the final modeling stage (for ACS
##               binomial models)
##
##              form [character] - model formula (optional; if NULL, a default formula will be used, depending on the covariate name)
##
##              shape [character] - path to shapefile to be used for generating new adjacency matrix or subsetting
##                locations; this shapefile is also used for generating map plots
##
##              states_to_model [character vector] - vector of state names to subset shapefile and data for modeling,
##                and for which to make predictions; e.g., c("Arizona", "California")
##
##              years_to_model [integer vector] - vector of years to subset data for modeling and for which to make
##                predictions; e.g., c(2000:2018)
##
##              race_groups [character vector] - vector of race/ethnicity groups for modeling and for which predictions should be made
##
## Outputs:     input data.table with additional columns for mean, median, upper and lower 95% posterior estimates for
##                covariate, and additional rows for any county-race-year combinations missing from input
####################################################################################################

### Generalized covariate imputation function
impute_covariate <- function(covar,
                             covariate_name = NULL,
                             outcome_name = NULL,
                             family = "gaussian",
                             save_plots = FALSE,
                             output_loc = NULL,
                             pardiso_path = NULL,
                             INLA_loc = NULL,
                             lib_loc = paste0("[FILEPATH]", rversion, "/"),
                             load_saved_intermediate = FALSE,
                             form = NULL,
                             shape = "[FILEPATH]",
                             states_to_model = NULL,
                             years_to_model = c(1990:2018),
                             race_groups = c("Hispanic", "NH AIAN", "NH API", "NH Black", "NH White"),
                             edu_groups = c("< HS", "HS or equivalent", "Some college", "BA or higher"),
                             stratify_by = "race_ethn") {
  
  #### Load INLA package
  library(INLA)
  
  #### Load additional required libraries
  pacman::p_load(ggplot2, data.table, rgdal, ggthemes, cowplot, splines, boot) # Load required libraries
  library(mapproj, lib.loc = lib_loc)
  
  #### Create output folder
  dir.create(output_loc, recursive = T)  # need recursive to create multiple folders (on first run for each covariate)
  
  #### Load shapefile
  shape <- readRDS(shape)
  
  #### Load FIPS:merged county reference table
  merged_counties <- fread("[FILEPATH]")
  merged_counties <- merged_counties[current == 1]
  
  #### Prep adjacency matrix
  if (!is.null(states_to_model)) { # Subset by states (if requested)
    #### Subset shapefile to requested states
    shape <- shape[shape@data$state_name %in% states_to_model,]
    
    #### Create new adjacency matrix
    temp <- spdep:::poly2nb(shape) # Construct neighbors list
    mat <- Matrix(spdep:::nb2mat(temp, style = "B", zero.policy = TRUE)) # Convert to adjacency matrix
    
    #### Create list of all merged counties in requested states (for later subsetting)
    mcntys <- unique(merged_counties[state_name %in% states_to_model, mcnty])
  } else { # Load saved adjacency matrix
    mat <- readRDS("[FILEPATH]")
    
    #### Create list of all merged counties across all states (for later subsetting)
    mcntys <- unique(merged_counties$mcnty)
  }
  
  if (covariate_name %in% c("income_median_by_race_ethn", "poverty_by_race_ethn", "unemployed_by_race_ethn", "edu_hs_by_race_ethn", "edu_ba_by_race_ethn", "foreign_born_by_race_ethn", "college_enr_by_race_ethn", "income_pc_by_race_ethn", "poverty_by_edu", "foreign_born_by_edu", "unemployed_by_edu", "insured_by_edu")) { # Derive SE estimate for ACS data
    #https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch07.pdf
    covar$se <- covar$moe / 1.645
  }
  
  #### Transform SE for income data
  if (covariate_name == "income_median_by_race_ethn") {
    #### Delta transformation of SE to log space
    covar$log_se <- sqrt((1 / covar$income_median)^2 * (covar$se)^2)
  } else if (covariate_name == "income_pc_by_race_ethn") {
    #### Delta transformation of SE to log space
    covar$log_se <- sqrt((1 / covar$income_pc)^2 * (covar$se)^2)
  }
  
  #### Check for missing race-county-year combinations and create empty rows for these
  # First, create a data table with all combinations, for merging with covar
  if (stratify_by == "race_ethn") {
    all_combos <- as.data.table(expand.grid(mcnty = mcntys, year = years_to_model, race_group = race_groups))
  } else if (stratify_by == "edu") {
    all_combos <- as.data.table(expand.grid(mcnty = mcntys, year = years_to_model, edu = edu_groups))
  }
  covar <- merge(covar, all_combos, all.y = TRUE)
  
  #### Optionally specify PARDISO license, and establish smtp method for INLA
  if (!is.null(pardiso_path)) {
    inla.setOption("pardiso.license", pardiso_path)
    smtp <- "pardiso"
  } else {
    smtp <- "taucs"
  }
  
  #### Read in and merge ACS sampling file; these data will be used in sample size estimation
  acs_sampling <- fread("[FILEPATH]")
  covar <- merge(covar, acs_sampling, by.x = "year", by.y = "Year", all.x = TRUE)
  
  #### Calculate effective sample sizes
  if (covariate_name %in% c("poverty_by_race_ethn", "unemployed_by_race_ethn", "edu_hs_by_race_ethn", "edu_ba_by_race_ethn", "foreign_born_by_race_ethn", "college_enr_by_race_ethn",
                            "poverty_by_edu", "foreign_born_by_edu", "unemployed_by_edu", "insured_by_edu", "nh_aian_by_edu", "nh_api_by_edu", "nh_white_by_edu", "nh_black_by_edu",
                            "hisp_by_edu")) {
    if (!load_saved_intermediate) {
      covar[, p := get(outcome_name) / pop]
      
      #### Calculate SEs in p-space
      covar$se_p <- covar$se / covar$pop
      
      #### Estimate N_eff (effective sample size) for non-decennial years
      covar[!(year %in% c(1990, 2000)), neff := p * (1 - p) / (se_p)^2] # eqn. (9) in https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6690503/
      setnames(covar, c(eval(outcome_name), "pop"), c(paste0("raw_", eval(outcome_name)), "raw_pop"))
      covar$uid <- 1:nrow(covar)
      covar[is.nan(p), p := NA]
      
      #### Transform p using emplogit to accommodate rows with p of 0 or 1
      emplogit <- function (x, eps = 1e-3) log((eps + x)/(1 - x + eps))
      covar[, p_emplogit := inv.logit(emplogit(p, min(covar[p > 0, p], na.rm = TRUE) / 2))]
      
      #### Order rows by year and county
      covar <- covar[order(year, mcnty)]
      
      #### Estimate N_eff for decennial Census data
      covar[year %in% c(1990), neff := round(raw_pop * 0.2, 0)] # 1990 decennial census long-form data were taken from a 20% sample: https://www.census.gov/history/www/through_the_decades/overview/1990.html
      covar[year %in% c(2000), neff := round(raw_pop * 0.17, 0)] # 2000 decennial census long-form data were taken from a 17% sample: https://www.census.gov/history/www/through_the_decades/overview/2000.html
      
      #### Calculate N_eff from empirical logit-transformed p for rows with p of 0 or 1
      covar[!(year %in% c(1990, 2000)) & (p %in% c(0, 1)), neff := p_emplogit * (1 - p_emplogit) / (se_p)^2] # eqn. (9) in https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6690503/
      
      #### Calculate outcome and set pop to equal neff
      covar[, eval(outcome_name) := neff * p]
      covar$pop <- covar$neff
      
      #### Save file
      write.csv(covar, paste0(output_loc, "[FILEPATH]"))
    } else {
      covar <- fread(paste0(output_loc, "[FILEPATH]"))
    }
  } else if (covariate_name %in% c("income_pc_by_race_ethn")) {
    if (!load_saved_intermediate) {
      covar$uid <- 1:nrow(covar)
      covar <- covar[order(year, mcnty)]
      
      #### Estimate sample size for ACS data using average yearly ACS sampling rates and household sizes
      # https://www.census.gov/acs/www/methodology/sample-size-and-data-quality/sample-size/)
      covar[!(year %in% c(1990, 2000)), neff := round((pop / household_size) * sampling_rate, 0)]
      covar[(year %in% c(1990, 2000)), neff := round((pop / household_size) * 0.1, 0)]
      
      #### Create a county-race variable
      covar[, county_race := paste0(mcnty, "_", race_group)]
      
      #### Set 0 neff values to 1 when income_pc is not NA (i.e., assume that when income_pc has a reported value, at least 1 individual was sampled)
      covar[!is.na(income_pc) & neff == 0, neff := 1]
      
      #### Set negative and 0-valued income_pc to NA (these cause problems later and are all from county-races with tiny populations, and hence are unreliable to begin with)
      covar[income_pc <= 0, income_pc := NA]
      
      #### Set NaN values to NA (these reflect zero populations)
      covar[is.nan(income_pc), income_pc := NA]
      
      #### Calculate sigma (in log space)
      covar[, sigma := log_se * sqrt(neff)]
      
      #### Set 0-value neffs to NA
      covar[neff == 0, neff := NA]
      
      #### Now model sigma; scale precision by neff
      sigma_form <- log(sigma^2) ~ f(county_race, model = "iid") + f(inla.group(income_pc), model = "rw1", scale.model = TRUE) + f(as.factor(uid), model = "iid")
      mod <- inla(sigma_form, data = covar, family = "gaussian", control.predictor = list(link = 1), scale = (covar$neff), control.inla = list(strategy = "gaussian", int.strategy = "eb", h = 1e-3, diagonal = 10), control.compute = list(config = FALSE, waic = TRUE, smtp = smtp), verbose = TRUE)
      mod <- inla(sigma_form, data = covar, family = "gaussian", control.predictor = list(link = 1), scale = (covar$neff), control.inla = list(strategy = "gaussian", int.strategy = "eb", h = 1e-3, diagonal = 1), control.mode=list(result = mod, restart = TRUE), control.compute = list(config = FALSE, waic = TRUE, smtp = smtp), verbose = TRUE)
      mod <- inla(sigma_form, data = covar, family = "gaussian", control.predictor = list(link = 1), scale = (covar$neff), control.inla = list(strategy = "gaussian", int.strategy = "eb", h = 1e-3), control.mode=list(result = mod, restart = TRUE), control.compute = list(config = FALSE, waic = TRUE, smtp = smtp), verbose = TRUE)

      # Retrieve predictions
      preds <- cbind(covar, sqrt(exp(mod$summary.fitted.values[1:nrow(covar),]$mean))) # Bind to summary predictions from INLA model
      setnames(preds, "V2", "sigma_modeled")

      #### Save model object and predictions to disk
      saveRDS(mod, file = paste0(output_loc, "[FILEPATH]"))
      
      # Derive final SE
      covar <- copy(preds)
      covar[is.na(log_se), log_se := sigma_modeled / sqrt(neff)]
      
      write.csv(covar, paste0(output_loc, "[FILEPATH]"))
    }
  } else if (covariate_name %in% c("property_value_median_by_race_ethn")) {
    setnames(covar, c(eval(outcome_name), "pop"), c(paste0("raw_", eval(outcome_name)), "raw_pop"))
    covar$neff <- round(covar$pop / 10, 0) # Decennial census "covariate" data are taken from a 10% sample
    covar$se_scale <- 1 / sqrt(covar$neff)
  } else {
    if (load_saved_intermediate) {
      covar <- fread(paste0(output_loc, "[FILEPATH]"))
    }
  }
  
  if (stratify_by == "race_ethn") {
    covar$edu <- "all"
  } else if (stratify_by == "edu") {
    covar$race_group <- "all"
  }
  
  #### Create county-race and county-edu variables
  covar[, county_race := paste0(mcnty, "_", race_group)]
  covar[, county_edu := paste0(mcnty, "_", edu)]
  
  #### Set race_group and edu to character
  covar$race_group <- as.character(covar$race_group)
  covar$edu <- as.character(covar$edu)
  
  #### Create a unique ID
  covar$uid <- 1:nrow(covar)
  
  #### Order data set
  covar <- covar[order(year, mcnty)]
  
  #### Set up INLA data stack
  if (family == "gaussian") {
    if (covariate_name %in% c("income_pc_by_race_ethn")) {
      covar[is.infinite(log_se), log_se := NA]
    } else if (covariate_name %in% c("property_value_median_by_race_ethn")) {
      covar[property_value_median <= 0, property_value_median := NA]
      covar[is.nan(property_value_median), property_value_median := NA]
      covar[se_scale == "Inf", se_scale := NA]
    }
    stk.y <- inla.stack(data = list(y = as.matrix(covar[, ..outcome_name])), A = list(1), effects = list(list(b0 = 1, mcnty = as.factor(covar$mcnty),
                                                                                                              mcnty2 = as.factor(covar$mcnty),
                                                                                                              year = covar$year,
                                                                                                              race = covar$race_group,
                                                                                                              county_race = covar$county_race,
                                                                                                              uid = covar$uid,
                                                                                                              ID.year = (covar$year - min(covar$year) + 1),
                                                                                                              ID.year2 = (covar$year - min(covar$year) + 1))))
  } else if (family == "nbinomial") {
    ## Set any race-county-years with pop == 0 to have outcome = NA and pop = 1, in order to fit model and generate predictions
    covar[pop == 0, eval(outcome_name) := NA]
    covar[pop == 0, pop := 1]
    
    stk.y <- inla.stack(data = list(y = as.matrix(covar[, ..outcome_name])), A = list(1), effects = list(list(b0 = 1, mcnty = as.factor(covar$mcnty),
                                                                                                              mcnty2 = as.factor(covar$mcnty),
                                                                                                              year = covar$year,
                                                                                                              race = covar$race_group,
                                                                                                              county_race = covar$county_race,
                                                                                                              uid = covar$uid,
                                                                                                              ID.year = (covar$year - min(covar$year) + 1),
                                                                                                              ID.year2 = (covar$year - min(covar$year) + 1))))
  } else if (family == "binomial") {
    ## Set any race-county-years with pop == 0 to have outcome = NA and pop = 1, in order to fit model and generate predictions
    covar[round(pop, 0) == 0, eval(outcome_name) := NA]
    covar[round(pop, 0) == 0, pop := 1]
    covar[is.na(pop), eval(outcome_name) := NA]
    covar[is.na(pop), pop := 1]
    covar[is.nan(pop), eval(outcome_name) := NA]
    covar[is.nan(pop), pop := 1]
    covar[is.infinite(pop), eval(outcome_name) := NA]
    covar[is.infinite(pop), pop := 1]
    
    stk.y <- inla.stack(data = list(y = round(as.matrix(covar[, ..outcome_name]), 0), Ntrials = round(covar$pop, 0)), A = list(1), effects = list(list(b0 = 1, mcnty = as.factor(covar$mcnty),
                                                                                                                                                       mcnty2 = as.factor(covar$mcnty),
                                                                                                                                                       year = covar$year,
                                                                                                                                                       race = covar$race_group,
                                                                                                                                                       county_race = covar$county_race,
                                                                                                                                                       edu = covar$edu,
                                                                                                                                                       county_edu = covar$county_edu,
                                                                                                                                                       uid = covar$uid,
                                                                                                                                                       ID.year = (covar$year - min(covar$year) + 1),
                                                                                                                                                       ID.year2 = (covar$year - min(covar$year) + 1))))
  }
  
  #### Define default model formulae
  if (is.null(form)) {
    if (covariate_name %in% c("income_median_by_race_ethn")) {
      form <- log(y) ~ -1 + b0 + f(ID.year, model = "ar1", replicate = as.integer(race), group = as.integer(mcnty), control.group = list(model = "besag", graph = mat, scale.model = TRUE), values = unique(ID.year))
    } else if (covariate_name %in% c("income_pc_by_race_ethn")) {
      form <- log(y) ~ -1 + f(b0, model = 'linear', prec.linear = 0.1) + f(mcnty, model = 'bym2', graph = mat, values = unique(mcnty)) + f(ID.year, model = 'ar1', replicate = as.integer(as.factor(county_race))) + f(ID.year2, model = 'ar1', replicate = as.integer(mcnty)) + f(race, model = 'iid')
    } else if (covariate_name %in% c("property_value_median_by_race_ethn")) {
      form <- log(y) ~ -1 + f(b0, model = "linear", prec.linear = 0.1) + f(mcnty, model = "bym2", graph = mat, values = unique(mcnty)) + f(ID.year, model = "ar1", replicate = as.integer(as.factor(county_race))) + f(ID.year2, model = "ar1", replicate = as.integer(mcnty)) + f(race, model = "iid")
    } else if (covariate_name %in% c("poverty_by_race_ethn", "unemployed_by_race_ethn", "edu_hs_by_race_ethn", "edu_ba_by_race_ethn", "foreign_born_by_race_ethn", "college_enr_by_race_ethn")) {
      form <- y ~ -1 + f(b0, model = 'linear', prec.linear = 0.1) + f(mcnty, model = 'bym2', graph = mat, values = unique(mcnty)) + f(ID.year, model = 'ar1', replicate = as.integer(as.factor(county_race))) + f(ID.year2, model = 'ar1', replicate = as.integer(mcnty)) + f(race, model = 'iid')
    } else if (covariate_name %in% c("poverty_by_edu", "foreign_born_by_edu", "unemployed_by_edu", "insured_by_edu", "nh_aian_by_edu", "nh_api_by_edu", "nh_white_by_edu", "nh_black_by_edu", "hisp_by_edu")) {
      form <- y ~ -1 + f(b0, model = 'linear', prec.linear = 0.1) + f(mcnty, model = 'bym2', graph = mat, values = unique(mcnty)) + f(ID.year, model = 'ar1', replicate = as.integer(as.factor(county_edu))) + f(ID.year2, model = 'ar1', replicate = as.integer(mcnty)) + f(edu, model = 'iid')
    } else if (covariate_name %in% c("correct_facil_by_race_ethn")) {
      form <- y ~ -1 + f(b0, model = "linear", prec.linear = 0.1) + f(mcnty, model = "bym2", graph = mat, values = unique(mcnty)) + f(ID.year, model = "ar1", replicate = as.integer(as.factor(county_race))) + f(ID.year2, model = "ar1", replicate = as.integer(mcnty)) + f(race, model = "iid")
    } else if (covariate_name %in% c("rural_by_race_ethn")) {
      form <- y ~ -1 + f(b0, model = "linear", prec.linear = 0.1) + f(mcnty, model = "bym2", graph = mat, replicate = as.integer(race), values = unique(mcnty)) + f(ID.year, model = "ar1", replicate = as.integer(as.factor(county_race))) + f(ID.year2, model = "ar1", replicate = as.integer(mcnty)) + f(race, model = "iid")
    }
  }
  
  #### Fit INLA model
  if (family == "gaussian") {
    if (covariate_name == "income_pc_by_race_ethn") {
      model <- inla(form, family = "gaussian", data = inla.stack.data(stk.y), control.predictor = list(A = inla.stack.A(stk.y), link = 1), scale = (1 / covar$log_se^2), control.inla = list(strategy = "gaussian", int.strategy = "eb", h = 1e-3), control.compute = list(config = TRUE, waic = TRUE, smtp = smtp), verbose = TRUE)
    } else if (covariate_name == "property_value_median_by_race_ethn") {
      model <- inla(form, family = "gaussian", data = inla.stack.data(stk.y), control.predictor = list(A = inla.stack.A(stk.y), link = 1), control.inla = list(strategy = "gaussian", int.strategy = "eb", h = 1e-3), control.compute = list(config = TRUE, waic = TRUE, smtp = smtp), verbose = TRUE, num.threads = 1)
    } else {
      model <- inla(form, family = "gaussian", data = inla.stack.data(stk.y), control.predictor = list(A = inla.stack.A(stk.y), link = 1), scale = (1 / covar$log_se^2), control.inla = list(int.strategy = "auto"), control.compute = list(config = TRUE, waic = TRUE, smtp = smtp), verbose = TRUE, num.threads = 1)
    }
  } else if (family == "nbinomial") {
    model <- inla(form, family = "nbinomial", data = inla.stack.data(stk.y), control.predictor = list(A = inla.stack.A(stk.y), link = 1), control.inla = list(int.strategy = "auto"), control.compute = list(config = TRUE, waic = TRUE, smtp = smtp), verbose = TRUE, num.threads = 1)
  } else if (family == "binomial") {
    if (covariate_name %in% c("correct_facil_by_race_ethn")) {
      model <- inla(form, family = "binomial", data = inla.stack.data(stk.y), Ntrials = Ntrials, control.predictor = list(A = inla.stack.A(stk.y), link = 1), control.inla = list(strategy = "gaussian", int.strategy = "eb", h = 1e-5), control.compute = list(config = TRUE, waic = TRUE, smtp = smtp), verbose = TRUE)
    } else if (covariate_name %in% c("foreign_born_by_race_ethn")) {
      model <- inla(form, family = "binomial", data = inla.stack.data(stk.y), Ntrials = Ntrials, control.predictor = list(A = inla.stack.A(stk.y), link = 1), control.inla = list(strategy = "gaussian", int.strategy = "eb", h = 1e-3, diagonal = 100), control.compute = list(config = FALSE, waic = TRUE, smtp = smtp), verbose = TRUE)
      model <- inla(form, family = "binomial", data = inla.stack.data(stk.y), Ntrials = Ntrials, control.predictor = list(A = inla.stack.A(stk.y), link = 1), control.inla = list(strategy = "gaussian", int.strategy = "eb", h = 1e-3, diagonal = 10), control.mode=list(result = model, restart = TRUE), control.compute = list(config = FALSE, waic = TRUE, smtp = smtp), verbose = TRUE)
      model <- inla(form, family = "binomial", data = inla.stack.data(stk.y), Ntrials = Ntrials, control.predictor = list(A = inla.stack.A(stk.y), link = 1), control.inla = list(strategy = "gaussian", int.strategy = "eb", h = 1e-3, diagonal = 1), control.mode=list(result = model, restart = TRUE), control.compute = list(config = FALSE, waic = TRUE, smtp = smtp), verbose = TRUE)
      model <- inla(form, family = "binomial", data = inla.stack.data(stk.y), Ntrials = Ntrials, control.predictor = list(A = inla.stack.A(stk.y), link = 1), control.inla = list(strategy = "gaussian", int.strategy = "eb", h = 1e-3), control.mode=list(result = model, restart = TRUE), control.compute = list(config = TRUE, waic = TRUE, smtp = smtp), verbose = TRUE)
    } else {
      model <- inla(form, family = "binomial", data = inla.stack.data(stk.y), Ntrials = Ntrials, control.predictor = list(A = inla.stack.A(stk.y), link = 1), control.inla = list(strategy = "gaussian", int.strategy = "eb", h = 1e-3), control.compute = list(config = TRUE, waic = TRUE, smtp = smtp), verbose = TRUE)
    }
  }
  
  #### Save model object to disk
  saveRDS(model, file = paste0(output_loc, "/model.RDS"))
  
  #### Save formula and model summary to readme
  con <- paste0(output_loc, "[FILEPATH]")
  capture.output(print(form), file = file(con), append = TRUE)
  capture.output(summary(model), file = file(con), append = TRUE)
  
  #### Derive predictions
  preds <- cbind(covar, model$summary.fitted.values[1:nrow(covar),]) # Bind to summary predictions from INLA model
  
  if (covariate_name %in% c("income_median_by_race_ethn", "income_pc_by_race_ethn", "property_value_median_by_race_ethn")) {
    preds$mean_unscaled <- exp(preds$mean)
    preds$median_unscaled <- exp(preds$`0.5quant`)
    preds$lower_unscaled <- exp(preds$`0.025quant`)
    preds$upper_unscaled <- exp(preds$`0.975quant`)  
  } else if (covariate_name %in% c("poverty_by_race_ethn", "unemployed_by_race_ethn", "edu_hs_by_race_ethn", "edu_ba_by_race_ethn",
                                   "foreign_born_by_race_ethn", "college_enr_by_race_ethn", "correct_facil_by_race_ethn", "rural_by_race_ethn",
                                   "poverty_by_edu", "foreign_born_by_edu", "unemployed_by_edu", "insured_by_edu", "nh_aian_by_edu", "nh_api_by_edu",
                                   "nh_white_by_edu", "nh_black_by_edu", "hisp_by_edu")) {
    preds$mean_unscaled <- preds$mean
    preds$median_unscaled <- preds$`0.5quant`
    preds$lower_unscaled <- preds$`0.025quant`
    preds$upper_unscaled <- preds$`0.975quant`
  }
  
  bym_1 <- cbind(levels(as.factor(covar$mcnty)), model$summary.random$mcnty[1:length(levels(as.factor(covar$mcnty))),])
  bym_2 <- cbind(levels(as.factor(covar$mcnty)), model$summary.random$mcnty[(length(levels(as.factor(covar$mcnty))) + 1):nrow(model$summary.random$mcnty),])
  colnames(bym_1)[1] <- "mcnty"
  colnames(bym_2)[1] <- "mcnty"
  bym_1$mcnty <- as.integer(as.character(bym_1$mcnty))
  bym_2$mcnty <- as.integer(as.character(bym_2$mcnty))
  
  #### Save predictions to disk
  write.csv(preds, paste0(output_loc, "[FILEPATH]"))
  write.csv(bym_1, paste0(output_loc, "[FILEPATH]"))
  write.csv(bym_2, paste0(output_loc, "[FILEPATH]"))
  
  #### Optionally save plots
  if (save_plots) {
    #### Save INLA plots to disk
    dir.create(path = paste0(output_loc, "[FILEPATH]"))
    plot(model, pdf = TRUE, prefix = paste0(output_loc, "[FILEPATH]"))
    
    #### Plot observed vs. predicted, all counties, by year and race
    preds <- merge(preds, unique(merged_counties[, c("mcnty", "state_name")]), by = "mcnty") # Merge state names onto preds
    pdf(paste0(output_loc, "[FILEPATH]"), width = 8.5, height = 11)
    
    for (state in unique(preds$state_name)) {
      plot_years <- unique(preds[state_name == state & !is.na(get(outcome_name)), year])
      gg <- plot_scatterplot(preds_dt = preds[state_name == state & year %in% plot_years], state = state, covariate_name = covariate_name, stratify_by = stratify_by)
      print(gg)
    }
    dev.off()
    
    #### Prep objects needed for plots
    dir.create(path = paste0(output_loc, "/time_series"))
    plot_titles <- data.table("name" = c("income_median_by_race_ethn", "poverty_by_race_ethn", "unemployed_by_race_ethn", "edu_hs_by_race_ethn", "edu_ba_by_race_ethn",
                                         "foreign_born_by_race_ethn", "college_enr_by_race_ethn", "income_pc_by_race_ethn", "correct_facil_by_race_ethn",
                                         "property_value_median_by_race_ethn", "rural_by_race_ethn", "poverty_by_edu", "foreign_born_by_edu", "unemployed_by_edu",
                                         "insured_by_edu", "nh_aian_by_edu", "nh_api_by_edu", "nh_white_by_edu", "nh_black_by_edu", "hisp_by_edu"),
                              "title" = c("Median Household Income", "Poverty Rate", "Unemployment Rate", "Edu Attainment (HS)", "Edu Attainment (BA)", "Foreign-Born",
                                          "College Enrollment", "Income per Capita", "Correct. Facility Prop.", "Median Property Value", "Rural Population %", "Poverty Rate",
                                          "Foreign-Born", "Unemployment Rate", "Insured Rate", "% NH AIAN", "% NH API", "% NH White", "% NH Black", "% Hispanic"))

    #### Loop through counties and save time-series plots to disk
    for (county_loop in unique(preds$mcnty)) {
      print(county_loop)
      
      #### Set up error bars
      if (covariate_name %in% c("poverty_by_race_ethn", "unemployed_by_race_ethn", "edu_hs_by_race_ethn", "edu_ba_by_race_ethn", "foreign_born_by_race_ethn", "college_enr_by_race_ethn", "poverty_by_edu",
                                "foreign_born_by_edu", "unemployed_by_edu", "insured_by_edu", "nh_aian_by_edu", "nh_api_by_edu", "nh_white_by_edu", "nh_black_by_edu", "hisp_by_edu")) {
        preds[, error_lower := (get(paste0("raw_", eval(outcome_name))) / raw_pop) - 1.96 * se_p]
        preds[, error_upper := (get(paste0("raw_", eval(outcome_name))) / raw_pop) + 1.96 * se_p]
        preds[!is.na(error_lower) & error_lower < 0, error_lower := 0]
        preds[!is.na(error_upper) & error_upper > 1, error_upper := 1]
      }
      
      if (stratify_by == "race_ethn") {
        plot_list <- vector(mode = "list", length = length(race_groups))
        for (i in 1:length(race_groups)) {
          plot_list[[i]] <- plot_time_series(preds_dt = preds, county_current = county_loop, race_current = race_groups[i], edu_current = "all", covariate_name = covariate_name, stratify_by = stratify_by)
        }
      } else if (stratify_by == "edu") {
        plot_list <- vector(mode = "list", length = length(edu_groups))
        for (i in 1:length(edu_groups)) {
          plot_list[[i]] <- plot_time_series(preds_dt = preds, county_current = county_loop, race_current = "all", edu_current = edu_groups[i], covariate_name = covariate_name, stratify_by = stratify_by)
        }
      }
      
      title <- ggdraw() +
        labs(title = paste0(plot_titles[name == covariate_name, title], " (", merged_counties[mcnty == county_loop, cnty_name], ", ",  merged_counties[mcnty == county_loop, state_name], ")")) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) + theme(plot.margin = margin(10, 0, 0, 0))
      
      pdf(paste0(output_loc, "[FILEPATH]", "mcnty_", county_loop, ".pdf"), width = 8.5, height = 11)
      plots <- plot_grid(plotlist = plot_list, ncol = 2)
      print(plot_grid(title, plots, ncol = 1, rel_heights = c(0.05, 1)))
      dev.off()
    }
    
    #### Plot maps of reported and imputed values
    ## Project and fortify shapefile
    shape <- spTransform(shape, "+init=epsg:4326")
    counties <- fortify(shape, region = "mcnty") # Fortify SpatialPolygonsDataFrame to facilitate plotting
    
    #### Create directory for maps
    dir.create(path = paste0(output_loc, "/maps"))
    
    if (stratify_by == "race_ethn") {
      for (race_current in unique(preds$race_group)) {
        pdf(paste0(output_loc, "[FILEPATH]", race_current, ".pdf"), width = 8.5, height = 11)
        for (year_current in unique(preds$year)) {
          print(plot_pred_maps(preds_dt = preds, race_current = race_current, edu_current = "all", year_current = year_current, counties = counties, shape = shape, covariate_name = covariate_name, stratify_by = stratify_by))
        }
        dev.off()
      }
    } else if (stratify_by == "edu") {
      for (edu_current in unique(preds$edu)) {
        pdf(paste0(output_loc, "[FILEPATH]", edu_current, ".pdf"), width = 8.5, height = 11)
        for (year_current in unique(preds$year)) {
          print(plot_pred_maps(preds_dt = preds, race_current = "all", edu_current = edu_current, year_current = year_current, counties = counties, shape = shape, covariate_name = covariate_name, stratify_by = stratify_by))
        }
        dev.off()
      } 
    }
    
    # ### Save maps of BYM2 spatial effects
    pdf(paste0(output_loc, "[FILEPATH]"), width = 8.5, height = 11)
    print(plot_bym_maps(bym_1, bym_2, counties = counties, shape = shape, label = plot_titles[name == covariate_name, title]))
    dev.off()
  }
  
  return(preds)
}

#### Define function for county-year-race scatterplots
plot_scatterplot <- function (preds_dt, state, covariate_name, stratify_by = "race_ethn") {
  gg <- ggplot(data = preds_dt) + theme_minimal() + geom_abline(intercept = 0, slope = 1, size = 0.25)
  
  ### Covariate-specific options
  if (covariate_name == "income_median_by_race_ethn") {
    gg <- gg + geom_point(aes(x = income_median / 10000, y = mean_unscaled / 10000, size = pop), alpha = 0.2, color = 1) +
      geom_errorbar(aes(y = (mean_unscaled / 10000), xmin = (income_median - 1.96 * se) / 10000, xmax = (income_median + 1.96 * se) / 10000), width = 0.02, alpha = 0.3, color = 2) +
      labs(title = paste0("Median Household Income, ", state), x = "Reported Estimate ($10,000s)", y = "Mean Imputed Estimate ($10,000s)") +
      scale_y_continuous(labels = scales:::comma, trans = "pseudo_log") + scale_x_continuous(labels = scales:::comma, trans = "pseudo_log")
  } else if (covariate_name == "income_pc_by_race_ethn") {
    gg <- gg + geom_point(aes(x = income_pc / 10000, y = mean_unscaled / 10000, size = pop), alpha = 0.2) +
      labs(title = paste0("Income per Capita, ", state), x = "Reported Estimate ($10,000s)", y = "Mean Imputed Estimate ($10,000s)") +
      scale_y_continuous(labels = scales:::comma, trans = "pseudo_log") + scale_x_continuous(labels = scales:::comma, trans = "pseudo_log")
  } else if (covariate_name %in% c("poverty_by_race_ethn", "poverty_by_edu")) {
    gg <- gg + geom_point(aes(x = raw_poverty / raw_pop, y = mean_unscaled, size = pop), alpha = 0.2) +
      labs(title = paste0("Poverty Rate, ", state), x = "Reported Estimate", y = "Mean Imputed Estimate") + coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
  } else if (covariate_name %in% c("unemployed_by_race_ethn", "unemployed_by_edu")) {
    gg <- gg + geom_point(aes(x = raw_unemployed / raw_pop, y = mean_unscaled, size = pop), alpha = 0.2) +
      labs(title = paste0("Unemployment Rate, ", state), x = "Reported Estimate", y = "Mean Imputed Estimate")
  } else if (covariate_name == "edu_hs_by_race_ethn") {
    gg <- gg + geom_point(aes(x = raw_edu_hs / raw_pop, y = mean_unscaled, size = pop), alpha = 0.2) +
      labs(title = paste0("Edu Attainment (HS), ", state), x = "Reported Estimate", y = "Mean Imputed Estimate")
  } else if (covariate_name == "edu_ba_by_race_ethn") {
    gg <- gg + geom_point(aes(x = raw_edu_ba / raw_pop, y = mean_unscaled, size = pop), alpha = 0.2) +
      labs(title = paste0("Edu Attainment (BA), ", state), x = "Reported Estimate", y = "Mean Imputed Estimate")
  } else if (covariate_name %in% c("foreign_born_by_race_ethn", "foreign_born_by_edu")) {
    gg <- gg + geom_point(aes(x = raw_foreign_born / raw_pop, y = mean_unscaled, size = pop), alpha = 0.2) +
      labs(title = paste0("Foreign-Born, ", state), x = "Reported Estimate", y = "Mean Imputed Estimate")
  } else if (covariate_name == "college_enr_by_race_ethn") {
    gg <- gg + geom_point(aes(x = raw_college_enr / raw_pop, y = mean_unscaled, size = pop), alpha = 0.2) +
      labs(title = paste0("College Enrollment, ", state), x = "Reported Estimate", y = "Mean Imputed Estimate")
  } else if (covariate_name == "correct_facil_by_race_ethn") {
    gg <- gg + geom_point(aes(x = correct_facil / pop, y = mean_unscaled, size = pop), alpha = 0.2) +
      labs(title = paste0("Correctional Facility Proportion, ", state), x = "Reported Estimate", y = "Mean Imputed Estimate")
  } else if (covariate_name == "property_value_median_by_race_ethn") {
    gg <- gg + geom_point(aes(x = property_value_median, y = mean_unscaled, size = pop), alpha = 0.2) +
      labs(title = paste0("Median Property Value, ", state), x = "Reported Estimate", y = "Mean Imputed Estimate")
  } else if (covariate_name == "rural_by_race_ethn") {
    gg <- gg + geom_point(aes(x = rural / pop, y = mean_unscaled, size = pop), alpha = 0.2) +
      labs(title = paste0("Rural Population %, ", state), x = "Reported Estimate", y = "Mean Imputed Estimate")
  } else if (covariate_name == "insured_by_edu") {
    gg <- gg + geom_point(aes(x = raw_insured / pop, y = mean_unscaled, size = pop), alpha = 0.2) +
      labs(title = paste0("Insured Population %, ", state), x = "Reported Estimate", y = "Mean Imputed Estimate")
  } else if (covariate_name == "nh_aian_by_edu") {
    gg <- gg + geom_point(aes(x = raw_nh_aian / raw_pop, y = mean_unscaled, size = pop), alpha = 0.2) +
      labs(title = paste0("% NH AIAN, ", state), x = "Reported Estimate", y = "Mean Imputed Estimate")
  } else if (covariate_name == "nh_api_by_edu") {
    gg <- gg + geom_point(aes(x = raw_nh_api / raw_pop, y = mean_unscaled, size = pop), alpha = 0.2) +
      labs(title = paste0("% NH API, ", state), x = "Reported Estimate", y = "Mean Imputed Estimate")
  } else if (covariate_name == "nh_white_by_edu") {
    gg <- gg + geom_point(aes(x = raw_nh_white / raw_pop, y = mean_unscaled, size = pop), alpha = 0.2) +
      labs(title = paste0("% NH White, ", state), x = "Reported Estimate", y = "Mean Imputed Estimate")
  } else if (covariate_name == "nh_black_by_edu") {
    gg <- gg + geom_point(aes(x = raw_nh_black / raw_pop, y = mean_unscaled, size = pop), alpha = 0.2) +
      labs(title = paste0("% NH Black, ", state), x = "Reported Estimate", y = "Mean Imputed Estimate")
  } else if (covariate_name == "hisp_by_edu") {
    gg <- gg + geom_point(aes(x = raw_hisp / raw_pop, y = mean_unscaled, size = pop), alpha = 0.2) +
      labs(title = paste0("% Hispanic, ", state), x = "Reported Estimate", y = "Mean Imputed Estimate")
  }
  
  gg <- gg + theme(plot.title = element_text(hjust = 0.5, face = "bold")) + theme(legend.position = "right") +
    theme(panel.border=element_blank(), axis.line=element_line())
  if (stratify_by == "race_ethn") {
    gg <- gg + facet_grid(rows = vars(year), cols = vars(race_group))
  } else if (stratify_by == "edu") {
    gg <- gg + facet_grid(rows = vars(year), cols = vars(edu))
  }
  gg <- gg + annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) + annotate("segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf) +
    theme(axis.ticks.x = element_line(size = 0.5), axis.ticks.y = element_line(size = 0.5)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  return(gg)
}

#### Define function for income county-year-race plots
plot_time_series <- function (preds_dt, county_current, race_current, edu_current, covariate_name, stratify_by = "race_ethn") {
  source <- c("Observed" = "white", "Modeled" = "red")
  gg <- ggplot(data = preds_dt[mcnty == county_current & race_group == race_current & edu == edu_current]) + theme_classic()
  
  if (stratify_by == "race_ethn") {
    current_title <- race_current
  } else if (stratify_by == "edu") {
    current_title <- edu_current
  }
  
  #### Covariate-specific behavior
  if (covariate_name == "income_median_by_race_ethn") {
    gg <- gg + geom_errorbar(aes(x = year, ymin = income_median - 1.96 * se, ymax = income_median + 1.96 * se), width = 0.1, alpha = 0.25) +
      geom_point(aes(x = year, y = income_median, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "Median Household Income ($)", fill = "Source")
  } else if (covariate_name == "income_pc_by_race_ethn") {
    preds_dt[is.na(se), se := sqrt((log_se)^2 / (1 / income_pc)^2)]
    gg <- gg + geom_errorbar(aes(x = year, ymin = income_pc - 1.96 * se, ymax = income_pc + 1.96 * se), width = 0.1, alpha = 0.25) +
      geom_point(aes(x = year, y = income_pc, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "Income per Capita ($)", fill = "Source")
  } else if (covariate_name %in% c("poverty_by_race_ethn", "poverty_by_edu")) {
    gg <- gg + geom_errorbar(aes(x = year, ymin = error_lower, ymax = error_upper), width = 0.1, alpha = 0.25) +
      geom_point(aes(x = year, y = raw_poverty / raw_pop, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "Poverty Rate", fill = "Source")
  } else if (covariate_name %in% c("unemployed_by_race_ethn", "unemployed_by_edu")) {
    gg <- gg + geom_errorbar(aes(x = year, ymin = error_lower, ymax = error_upper), width = 0.1, alpha = 0.25) +
      geom_point(aes(x = year, y = raw_unemployed / raw_pop, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "Unemployment Rate", fill = "Source")
  } else if (covariate_name == "edu_hs_by_race_ethn") {
    gg <- gg + geom_errorbar(aes(x = year, ymin = error_lower, ymax = error_upper), width = 0.1, alpha = 0.25) +
      geom_point(aes(x = year, y = raw_edu_hs / raw_pop, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "Edu Attainment (HS)", fill = "Source")
  } else if (covariate_name == "edu_ba_by_race_ethn") {
    gg <- gg + geom_errorbar(aes(x = year, ymin = error_lower, ymax = error_upper), width = 0.1, alpha = 0.25) +
      geom_point(aes(x = year, y = raw_edu_ba / raw_pop, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "Edu Attainment (BA)", fill = "Source")
  } else if (covariate_name %in% c("foreign_born_by_race_ethn", "foreign_born_by_edu")) {
    gg <- gg + geom_errorbar(aes(x = year, ymin = error_lower, ymax = error_upper), width = 0.1, alpha = 0.25) +
      geom_point(aes(x = year, y = raw_foreign_born / raw_pop, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "Foreign-Born", fill = "Source")
  } else if (covariate_name == "college_enr_by_race_ethn") {
    gg <- gg + geom_errorbar(aes(x = year, ymin = error_lower, ymax = error_upper), width = 0.1, alpha = 0.25) +
      geom_point(aes(x = year, y = raw_college_enr / raw_pop, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "College Enrollment", fill = "Source")
  } else if (covariate_name == "correct_facil_by_race_ethn") {
    gg <- gg + 
      geom_point(aes(x = year, y = correct_facil / pop, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "Correct. Facility Prop.", fill = "Source")
  } else if (covariate_name == "property_value_median_by_race_ethn") {
    gg <- gg + 
      geom_point(aes(x = year, y = property_value_median, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "Median Property Value", fill = "Source")
  } else if (covariate_name == "rural_by_race_ethn") {
    gg <- gg + geom_point(aes(x = year, y = rural / pop, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "Rural Pop.", fill = "Source")
  } else if (covariate_name == "insured_by_edu") {
    gg <- gg + geom_point(aes(x = year, y = raw_insured / pop, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "Insured Pop.", fill = "Source")
  } else if (covariate_name == "nh_aian_by_edu") {
    gg <- gg + geom_point(aes(x = year, y = raw_nh_aian / pop, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "% NH AIAN", fill = "Source")
  } else if (covariate_name == "nh_api_by_edu") {
    gg <- gg + geom_point(aes(x = year, y = raw_nh_api / pop, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "% NH API", fill = "Source")
  } else if (covariate_name == "nh_white_by_edu") {
    gg <- gg + geom_point(aes(x = year, y = raw_nh_white / pop, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "% NH White", fill = "Source")
  } else if (covariate_name == "nh_black_by_edu") {
    gg <- gg + geom_point(aes(x = year, y = raw_nh_black / pop, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "% NH Black", fill = "Source")
  } else if (covariate_name == "hisp_by_edu") {
    gg <- gg + geom_point(aes(x = year, y = raw_hisp / pop, fill = "Observed"), size = 3, shape = 21) +
      labs(title = current_title, x = "Year", y = "% Hispanic", fill = "Source")
  }
  
  gg <- gg + geom_point(aes(x = year, y = mean_unscaled, fill = "Modeled"), size = 2, shape = 21) +
    geom_ribbon(aes(x = year, ymin = lower_unscaled, ymax = upper_unscaled), alpha = 0.1) +
    geom_line(aes(x = year, y = mean_unscaled)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) + theme(legend.position = "right") +
    scale_fill_manual(values = source)
  
  return(gg)
}

#### Define function for poverty map plots
plot_pred_maps <- function (preds_dt, race_current, edu_current, year_current, counties, shape, covariate_name, stratify_by) {
  source <- c("Observed" = "white", "Modeled" = "red")
  
  gg1 <- ggplot() + theme_map()
  gg2 <- ggplot() + theme_map() + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = mean_unscaled), map = counties)
  
  if (stratify_by == "race_ethn") {
    current_title <- race_current
  } else if (stratify_by == "edu") {
    current_title <- edu_current
  }
  
  if (covariate_name == "income_median_by_race_ethn") {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = income_median), map = counties) +
      labs(title = paste0("Household Income, Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Household Income')
    gg2 <- gg2 + labs(title = paste0("Household Income, Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Household Income')
  } else if (covariate_name %in% c("poverty_by_race_ethn", "poverty_by_edu")) {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(raw_poverty / raw_pop)), map = counties) +
      labs(title = paste0("Poverty Rate, Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Poverty Rate') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
    gg2 <- gg2 + labs(title = paste0("Poverty Rate, Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Poverty Rate') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
  } else if (covariate_name %in% c("unemployed_by_race_ethn", "unemployed_by_edu")) {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(raw_unemployed / raw_pop)), map = counties) +
      labs(title = paste0("Unemployment Rate, Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Unemployment Rate') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
    gg2 <- gg2 + labs(title = paste0("Unemployment Rate, Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Unemployment Rate') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
  } else if (covariate_name == "edu_hs_by_race_ethn") {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(raw_edu_hs / raw_pop)), map = counties) +
      labs(title = paste0("Edu Attainment (HS), Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Edu Attainment (HS)') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
    gg2 <- gg2 + labs(title = paste0("Edu Attainment (HS), Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Edu Attainment (HS)') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
  } else if (covariate_name == "edu_ba_by_race_ethn") {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(raw_edu_ba / raw_pop)), map = counties) +
      labs(title = paste0("Edu Attainment (BA), Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Edu Attainment (BA)') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
    gg2 <- gg2 + labs(title = paste0("Edu Attainment (BA), Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Edu Attainment (BA)') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
  } else if (covariate_name %in% c("foreign_born_by_race_ethn", "foreign_born_by_edu")) {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(raw_foreign_born / raw_pop)), map = counties) +
      labs(title = paste0("Foreign-Born, Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Foreign-Born') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
    gg2 <- gg2 + labs(title = paste0("Foreign-Born, Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Foreign-Born') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
  } else if (covariate_name == "college_enr_by_race_ethn") {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(raw_college_enr / raw_pop)), map = counties) +
      labs(title = paste0("College Enrollment, Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='College Enrollment') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
    gg2 <- gg2 + labs(title = paste0("College Enrollment, Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='College Enrollment') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
  } else if (covariate_name == "correct_facil_by_race_ethn") {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(correct_facil / pop)), map = counties) +
      labs(title = paste0("Correctional Facility Prop., Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Correctional Facility Prop.') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
    gg2 <- gg2 + labs(title = paste0("Correctional Facility Prop., Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Correctional Facility Prop.') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
  } else if (covariate_name == "property_value_median_by_race_ethn") {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(property_value_median)), map = counties) +
      labs(title = paste0("Median Property Value, Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Median Property Value')
    gg2 <- gg2 + labs(title = paste0("Median Property Value, Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Median Property Value')
  } else if (covariate_name == "rural_by_race_ethn") {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(rural / pop)), map = counties) +
      labs(title = paste0("Rural Population Prop., Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Rural Population Prop.') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
    gg2 <- gg2 + labs(title = paste0("Rural Population Prop., Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Rural Population Prop.') + scale_fill_gradient(low="white", high="red", limits = c(0, 1))
  } else if (covariate_name == "income_pc_by_race_ethn") {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(income_pc)), map = counties) +
      labs(title = paste0("Income Per Capita, Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Income Per Capita') + scale_fill_gradient(low="white", high="red", limits = c(0, max(preds_dt[year == year_current & race_group == race_current, c(income_pc, mean_unscaled)], na.rm = TRUE)))
    gg2 <- gg2 + labs(title = paste0("Income Per Capita, Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Income Per Capita') + scale_fill_gradient(low="white", high="red", limits = c(0, max(preds_dt[year == year_current & race_group == race_current, c(income_pc, mean_unscaled)], na.rm = TRUE)))
  } else if (covariate_name == "insured_by_edu") {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(raw_insured)), map = counties) +
      labs(title = paste0("Insured Population Prop., Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Insured Population') + scale_fill_gradient(low="white", high="red", limits = c(0, max(preds_dt[year == year_current & race_group == race_current, c(raw_insured, mean_unscaled)], na.rm = TRUE)))
    gg2 <- gg2 + labs(title = paste0("Insured Population Prop., Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Insured Population') + scale_fill_gradient(low="white", high="red", limits = c(0, max(preds_dt[year == year_current & race_group == race_current, c(raw_insured, mean_unscaled)], na.rm = TRUE)))
  } else if (covariate_name == "nh_aian_by_edu") {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(raw_nh_aian)), map = counties) +
      labs(title = paste0("% NH AIAN, Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='NH AIAN Population') + scale_fill_gradient(low="white", high="red", limits = c(0, max(preds_dt[year == year_current & race_group == race_current, c(raw_nh_aian, mean_unscaled)], na.rm = TRUE)))
    gg2 <- gg2 + labs(title = paste0("% NH AIAN, Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='NH AIAN Population') + scale_fill_gradient(low="white", high="red", limits = c(0, max(preds_dt[year == year_current & race_group == race_current, c(raw_nh_aian, mean_unscaled)], na.rm = TRUE)))
  } else if (covariate_name == "nh_api_by_edu") {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(raw_nh_api)), map = counties) +
      labs(title = paste0("% NH API, Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='NH API Population') + scale_fill_gradient(low="white", high="red", limits = c(0, max(preds_dt[year == year_current & race_group == race_current, c(raw_nh_api, mean_unscaled)], na.rm = TRUE)))
    gg2 <- gg2 + labs(title = paste0("% NH API, Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='NH API Population') + scale_fill_gradient(low="white", high="red", limits = c(0, max(preds_dt[year == year_current & race_group == race_current, c(raw_nh_api, mean_unscaled)], na.rm = TRUE)))
  } else if (covariate_name == "nh_white_by_edu") {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(raw_nh_white)), map = counties) +
      labs(title = paste0("% NH White, Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='NH White Population') + scale_fill_gradient(low="white", high="red", limits = c(0, max(preds_dt[year == year_current & race_group == race_current, c(raw_nh_white, mean_unscaled)], na.rm = TRUE)))
    gg2 <- gg2 + labs(title = paste0("% NH White, Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='NH White Population') + scale_fill_gradient(low="white", high="red", limits = c(0, max(preds_dt[year == year_current & race_group == race_current, c(raw_nh_white, mean_unscaled)], na.rm = TRUE)))
  } else if (covariate_name == "nh_black_by_edu") {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(raw_nh_black)), map = counties) +
      labs(title = paste0("% NH Black, Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='NH Black Population') + scale_fill_gradient(low="white", high="red", limits = c(0, max(preds_dt[year == year_current & race_group == race_current, c(raw_nh_black, mean_unscaled)], na.rm = TRUE)))
    gg2 <- gg2 + labs(title = paste0("% NH Black, Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='NH Black Population') + scale_fill_gradient(low="white", high="red", limits = c(0, max(preds_dt[year == year_current & race_group == race_current, c(raw_nh_black, mean_unscaled)], na.rm = TRUE)))
  } else if (covariate_name == "hisp_by_edu") {
    gg1 <- gg1 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = as.numeric(raw_hisp)), map = counties) +
      labs(title = paste0("% Hispanic, Reported: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Hispanic Population') + scale_fill_gradient(low="white", high="red", limits = c(0, max(preds_dt[year == year_current & race_group == race_current, c(raw_hisp, mean_unscaled)], na.rm = TRUE)))
    gg2 <- gg2 + labs(title = paste0("% Hispanic, Imputed: ", current_title, ", ", year_current), x = "Longitude", y = "Latitude", fill='Hispanic Population') + scale_fill_gradient(low="white", high="red", limits = c(0, max(preds_dt[year == year_current & race_group == race_current, c(raw_hisp, mean_unscaled)], na.rm = TRUE)))
  }

  gg1 <- gg1 + geom_polygon(data = shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.1) +
    expand_limits(x = counties$long, y = counties$lat) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) + theme(legend.position = "right") + coord_map()
  gg2 <- gg2 + geom_map(data = preds_dt[year == year_current & race_group == race_current & edu == edu_current], aes(map_id = mcnty, fill = mean_unscaled), map = counties) +
    geom_polygon(data = shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.1) +
    expand_limits(x = counties$long, y = counties$lat) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) + theme(legend.position = "right") + coord_map()
  
  return(plot_grid(gg1, gg2, ncol = 1))
}

#### Define function for bym2 spatial effect plots
plot_bym_maps <- function (bym_1, bym_2, counties, shape, label) {
  gg1 <- ggplot() + theme_map() + geom_map(data = bym_1, aes(map_id = mcnty, fill = mean), map = counties) +
    geom_polygon(data = shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.1) +
    expand_limits(x = counties$long, y = counties$lat) + scale_fill_continuous(type = "viridis") +
    labs(title = paste0(label, ", BYM2 Unstructured Spatial Effect Mean"), x = "Longitude", y = "Latitude", fill='Random Effect') +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) + theme(legend.position = "right") + coord_map()
  gg2 <- ggplot() + theme_map() + geom_map(data = bym_2, aes(map_id = mcnty, fill = mean), map = counties) +
    geom_polygon(data = shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.1) +
    expand_limits(x = counties$long, y = counties$lat) + scale_fill_continuous(type = "viridis") +
    labs(title = paste0(label, ", BYM2 Structured Spatial Effect Mean"), x = "Longitude", y = "Latitude", fill='Random Effect') +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) + theme(legend.position = "right") + coord_map()
  return(plot_grid(gg1, gg2, ncol = 1))
}

#### Format a time stamp for run dates
make_time_stamp <- function(time_stamp) {
  run_date <- gsub("-", "_", Sys.time())
  run_date <- gsub(":", "_", run_date)
  run_date <- gsub(" ", "_", run_date)
  
  if (time_stamp == FALSE) run_date <- 'scratch'
  
  return(run_date)
}
