###############################################################################################################
## Description: Combine different draw-models or imputations into one final estimate. For example, 100 models
##              may be run for a cause and 10 draws are taken from each model. Ten draws from all 100 models
##              are combined to have 1000 total draws.
##
## Passed args: repo [character] -- location of nonfatal repository
##              output_dir_draws_est [character] -- location for model outputs (draws and est files). same as output_dir for YLD models
##              settings_loc [character] -- file path of settings file
##              map_path [character] -- path for array job parameters
##
## Outputs:     combined final estimates with 1000 draws and summary estimates
##
###############################################################################################################


rm(list=ls())
library(data.table)
library(reshape2)
library(dplyr)
library(foreach)

## Load settings -----------------------------------------------------------------------------------
###### Set up objects from command args

if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (settings_loc <- commandArgs(TRUE)[[3]])
  (sex <- commandArgs(TRUE)[[4]])
  (race <- as.integer(commandArgs(TRUE)[5]))
  (validate <- as.logical(commandArgs(TRUE)[6]))
  (resub <- as.logical(commandArgs(TRUE)[7]))
  (by_sex <- as.logical(commandArgs(TRUE)[8]))
  (output_dir_draws_est <- commandArgs(TRUE)[9])
  (by_source <- commandArgs(TRUE)[10])
  (imp <- commandArgs(TRUE)[11])
}

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  print(func)
  source(paste0(repo, "/functions/", func))
}

###### Assign settings from settings file
get_settings(settings_loc)
race <- 99

#### Read in parameter draws for each draw-model / imputation
draws <- foreach (1:n.imp, .combine = "cbind") %do% {
  
  ## read in array arguments
  ## get the draw value
  if (by_sex) {
    draw_args <- fread(paste0(output_dir_draws_est, "/draw_args_", sex, "_", race, ".csv"))
    task_id <- ifelse(is.na(as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))), 1, as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID")))
    
    draw_val <- c(draw_args[task_id, start_draw]:draw_args[task_id, end_draw])
    re_file <- paste0(output_dir_draws_est, "/initial_sims_", sex, "_", race, "_", imp, ".rds")
    
    sims <- readRDS(re_file)
    mean <- readRDS(paste0(output_dir_draws_est, "/mean_", sex, "_", race, "_", imp, ".rds"))
    stratpop <- readRDS(paste0(output_dir_draws_est, "/stratpop_", sex, "_", race, ".rds"))
  } else {
    draw_args <- fread(paste0(output_dir_draws_est, "/draw_args_", race, ".csv"))
    task_id <- ifelse(is.na(as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))), 1, as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID")))
    
    draw_val <- c(draw_args[task_id, start_draw]:draw_args[task_id, end_draw])
    re_file <- paste0(output_dir_draws_est, "/initial_sims_", race, "_", imp, ".rds")
    
    sims <- readRDS(re_file)
    mean <- readRDS(paste0(output_dir_draws_est, "/mean_", race, "_", imp, ".rds"))
    stratpop <- readRDS(paste0(output_dir_draws_est, "/stratpop_", race, ".rds"))
  }
  
  sims <- sims[, draw_val]
  
  # the simvars are not actually the draw vals because when the matrices get subset the column numbers change
  simvars <- paste0("V", 1:length(draw_val))
  
  # Get spline information, if present in settings.csv
  if (!is.null(age_knots_spec)) {
    age_knots <- which(ages %in% age_knots_spec) - 1
    s_age <- as.data.table(bs((1:num_a) - 1, knots = age_knots[1:(length(age_knots) - 1)], degree = 1, intercept = F)) 
  }
  
  if (!is.null(year_knots_num)) {
    year_knots <- seq(1, length(years), length.out = year_knots_num) - 1
    s_year <- as.data.table(bs((1:num_t) - 1, knots = year_knots[1:(length(year_knots) - 1)], degree = 1, intercept = F)) 
  }
  
  # calculate draws for the fixed portion of the model (same for all models)
  if (grepl("state", model)) {
    # Load full data set (that did not drop NA agg weights) to get cov values and scale with same mean and SD as modeling data
    if (!cross_val) {
      data <- readRDS(paste0(output_dir, "/data_full_", imp,".rds"))
    } else {
      data <- readRDS(paste0(output_dir, "/data_full.rds"))
    }
    mod_data <- readRDS(paste0(output_dir, "/data_", imp,".rds"))
    if ("state" %in% names(pred_frame)) {
      setkeyv(mod_data, c("area", "state","year", "sex", "race", "age"))
      setkeyv(data,  c("area", "state", "year", "sex", "race", "age"))
    } else {
      setkeyv(mod_data, c("area", "year", "sex", "race", "age"))
      setkeyv(data,  c("area", "year", "sex", "race", "age"))
    } # same as pred_frame
    
    for (col in covars_scale) {
      dt_center <- eval(parse(text = paste0("attr(mod_data$", col,", 'scaled:center')")))
      dt_scale <- eval(parse(text = paste0("attr(mod_data$", col,", 'scaled:scale')")))
      eval(parse(text = paste0("data$", col, " <- scale(data$", col, ", center = ", dt_center, ", scale = ", dt_scale, ")")))
    }
    rm(mod_data)
    
    # subset the data
    data <- data[sex == recode$sex[sex == get("sex", .GlobalEnv), sex_recode], ]
    data[, int := 1]
    if ("state" %in% names(pred_frame)) {
      setkeyv(data, c("area", "state","year", "sex", "race", "age"))
    } else {
      setkeyv(data, c("area", "year", "sex", "race", "age"))
    } # same as pred_frame
    
    
    # Check order of rows is the same (check if subsetting cols changes order of DT)
    if(!all.equal(pred_frame[,list(int, area, year, sex, race, age)], data[, list(int, area, year, sex, race, age)])) {
      stop("pred_frame and data for covariates are not equal")
    }
    
    # Global intercept
    B_0 <- sims[grepl("^B", names(mean)) & !grepl("^B1", names(mean)) & !grepl("^B2", names(mean)) & !grepl("^B3", names(mean)) & !grepl("^B4", names(mean)) & !grepl("^B5", names(mean)), ]
    fe_0 <- as.matrix(data[, c("int"), with = F]) %*% B_0
    
    data[, index := .I] # add index to help with merges
    B_1 <- sims[grepl("^B1", names(mean)), ]
    
    # Multiply covariate values by fixed effect
    fe_1 <- as.matrix(data[, c(covars_subpop), with = F]) %*% B_1
    
    fe <- fe_0 + fe_1
    
    # define function
    replace_na_func <- function(x) ifelse(is.na(x), 0, x)
    
    if (!model %in% c("state_model9", "state_model10", "state_model12","state_model17", "state_model21", "state_model22")) {
      B_2 <- sims[grepl("^B2", names(mean)), ]
      # Value of covariates are NA for those under 20 -> set to 0
      data[, (covars_subpop_20plus) := lapply(.SD, replace_na_func), .SDcols = covars_subpop_20plus]
      
      # Multiply covariate values by fixed effect
      fe_2 <- as.matrix(data[, c(covars_subpop_20plus), with = F]) %*% B_2
      # Set fe2 to 0 for ages under 20
      fe_2 <- fe_2 * as.integer(data[, age >= recode$age[age == 20, age_recode]])
      
      fe <- fe + fe_2
      
    }
    
    if(model %in% c("state_model8", "state_model10", "state_model12", "state_model13", "state_model15", "state_model19","state_model20")) {
      B_3 <- sims[grepl("^B3", names(mean)), ]
      # Value of covariates are NA for those in age groups over 60-64
      data[, (covars_subpop_60under) := lapply(.SD, replace_na_func), .SDcols = covars_subpop_60under]
      
      # Multiply covariate values by fixed effect
      fe_3 <- as.matrix(data[, c(covars_subpop_60under), with = F]) %*% B_3
      # Set fe3 to 0 for ages over 60
      fe_3 <- fe_3 * as.integer(data[, age <= recode$age[age == 60, age_recode]])
      fe <- fe + fe_3
    }
    
    if(model %in% c("state_model13", "state_model15")) {
      B_4 <- sims[grepl("^B4", names(mean)), ]
      # Value of covariates are NA for those in age groups over 5
      data[, (covars_subpop_5plus) := lapply(.SD, replace_na_func), .SDcols = covars_subpop_5plus]
      
      # Multiply covariate values by fixed effect
      fe_4 <- as.matrix(data[, c(covars_subpop_5plus), with = F]) %*% B_4
      # Set fe4 to 0 for ages under 5
      fe_4<- fe_4 * as.integer(data[, age >= recode$age[age == 5, age_recode]])
      fe <- fe + fe_4
    }
    
    if(model %in% c("state_model15", "state_model24")) {
      B_5 <- sims[grepl("^B5", names(mean)), ]
      # Value of covariates are NA for those in age groups over 5
      data[, (covars_subpop_1plus) := lapply(.SD, replace_na_func), .SDcols = covars_subpop_1plus]
      
      # Multiply covariate values by fixed effect
      fe_5 <- as.matrix(data[, c(covars_subpop_1plus), with = F]) %*% B_5
      # Set fe5 to 0 for ages under 1
      fe_5 <- fe_5 * as.integer(data[, age >= recode$age[age == 1, age_recode]])
      fe <- fe + fe_5
    }
    
    if(model %in% c("state_model20")) {
      B_4 <- sims[grepl("^B4", names(mean)), ]
      # Value of covariates are NA for those in age groups over 15
      data[, (covars_subpop_15plus) := lapply(.SD, replace_na_func), .SDcols = covars_subpop_15plus]
      
      # Multiply covariate values by fixed effect
      fe_4 <- as.matrix(data[, c(covars_subpop_15plus), with = F]) %*% B_4
      # Set fe4 to 0 for ages under 5
      fe_4<- fe_4 * as.integer(data[, age >= recode$age[age == 15, age_recode]])
      fe <- fe + fe_4
    }
    
    fe <- as.matrix(fe)
    saveRDS(fe, paste0(output_dir_draws_est, "/fe_sims_", sex, "_", race, "_", imp,".rds"))
    
    # rm(B)
  }
  ## Year effect
  if (!model %in% c("state_model4", "state_model12", "state_model15", "state_model17","state_model18", "state_model19", "state_model20", "state_model21", "state_model22", "state_model24")) {
    re1 <- data.table(as.matrix(sims[names(mean) == "re1",]))
    re1[, year := 1:num_t - 1L]
    setkeyv(re1, "year")
  }
  
  ## Age effect
  if (!model %in% c("state_model12", "state_model15", "state_model17", "state_model18", "state_model19","state_model20", "state_model21", "state_model22", "state_model24")) {
    re2 <- data.table(as.matrix(sims[names(mean) == "re2",]))
    re2[, age := 1:num_a - 1L]
    setkeyv(re2, "age")
  }
  
  ## State effect
  if (model %in% c("state_model21")) {
    re2 <- data.table(as.matrix(sims[names(mean) == "re2",]))
    re2[, state := 1:num_k - 1L]
    setkeyv(re2, "state")
  }
  
  ## Age-year random intercept
  if (model %in% c("state_model12", "state_model15", "state_model17", "state_model18", "state_model19","state_model20", "state_model21", "state_model24")) {
    combos <- as.data.table(expand.grid(year = 0:(num_t - 1), age = 0:(num_a - 1)))
    re1 <- data.table(as.matrix(sims[names(mean) == "re1", ]))
    re1 <- cbind(re1, combos)
    setkeyv(re1, c("year", "age"))
  }
  
  ## Age-year-state random intercept
  if (model %in% c("state_model22")) {
    
    combos <- as.data.table(expand.grid(year = 0:(num_t - 1), age = 0:(num_a - 1), state = 0:(num_k - 1)))
    re1 <- data.table(as.matrix(sims[names(mean) == "re1", ]))
    re1 <- cbind(re1, combos)
    setkeyv(re1, c("year", "age", "state"))
  }
  
  if (!model %in% c("state_model4", "state_model12", "state_model15","state_model17", "state_model18", "state_model19", "state_model20", "state_model21", "state_model22", "state_model24")) {
    re <- as.matrix(as.matrix(re1[pred_frame[, list(year)], simvars, with = FALSE]) + 
                      as.matrix(re2[pred_frame[, list(age)], simvars, with = FALSE]))
    
  } else if (model %in% c("state_model12", "state_model15", "state_model17", "state_model18", "state_model19", "state_model20", "state_model24")){
    re <- as.matrix(re1[pred_frame[, list(year, age)], simvars, with = FALSE])
  } else if (model %in% c("state_model21")) {
    re <- as.matrix(as.matrix(re1[pred_frame[, list(year, age)], simvars, with = FALSE]) + 
                      as.matrix(re2[data[, list(state)], simvars, with = FALSE]))
    
  } else if (model %in% c("state_model22")) {
    re <- as.matrix(re1[pred_frame[, list(year, age, state)], simvars, with = FALSE])
  }
  ## Covariates with random slopes by age
  
  covar_subpop_random_effects <- data.table(as.matrix(sims[names(mean) == "covar_subpop_re_matrix", ]))
  
  # add in the effect of the race/ethnicity or edu specific covariates by iterating over each of the race/ethnicity or edu specific covariates
  for (i in c(1:length(covars_subpop))) {
    print(glue::glue("Working on covariate number {i}: {covars_subpop[i]}"))
    
    # how long is each section of covar_subpop_re_matrix? that is, which entries of this vector correspond to the current covariate?
    # NOTE it is vital to use nrow(); length() gives the number of elements because sims is a Matrix
    chunk_size <- nrow(covar_subpop_random_effects) / length(covars_subpop)
    print(glue::glue("chunk_size = {chunk_size}"))
    
    # the row/index that marks the end of the current covariates chunk of covar_subpop_re_matrix
    end_row <- chunk_size * i
    
    # create an index that identifies the correct rows of covar_subpop_re_matrix. Add 1 at the start so that the index begins at 1, because row numbers in data.table begins at 1.
    begin_row <- end_row - chunk_size + 1
    print(glue::glue("begin_row is {begin_row}"))
    print(glue::glue("end_row is {end_row}"))
    
    covar_subpop_random_effect_rows <- c((begin_row):(end_row))
    print(glue::glue("covar_subpop_random_effect_rows is {paste(covar_subpop_random_effect_rows, collapse=',')}"))
    
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
    
    # include age for merging?
    covar_subpop_random_effect[, age := 0:(num_a - 1)]
    setkeyv(covar_subpop_random_effect, "age")
    
    # pull out the current covariate
    # vector
    covar_random_effect <- data[, get(covars_subpop[i])]
    
    # Add these age covariate slopes to the other random effects
    print("adding to re...")
    re <- re + (covar_random_effect * covar_subpop_random_effect[pred_frame[, list(age)], simvars, with = F])
    print(glue::glue("Done with loop {i}."))
  } 
  
  if (!model %in% c("state_model9", "state_model10", "state_model12", "state_model17", "state_model21", "state_model22")){ ## Covariates restricted to 20+ with random slopes by age
    
    covar_subpop_random_effects_20plus <- data.table(as.matrix(sims[names(mean) == "covar_subpop_re_matrix_20plus", ]))
    
    # add in the effect of the race/ethnicity or edu specific covariates by iterating over each of the race/ethnicity or edu specific covariates
    for (i in c(1:length(covars_subpop_20plus))) {
      print(glue::glue("Working on covariate number {i}: {covars_subpop_20plus[i]}"))
      
      # how long is each section of covar_subpop_re_matrix? that is, which entries of this vector correspond to the current covariate?
      chunk_size <- nrow(covar_subpop_random_effects_20plus) / length(covars_subpop_20plus)
      print(glue::glue("chunk_size = {chunk_size}"))
      
      # the row/index that marks the end of the current covariates chunk of covar_subpop_re_matrix
      end_row <- chunk_size * i
      
      # create an index that identifies the correct rows of covar_subpop_re_matrix. Add 1 at the start so that the index begins at 1, because row numbers in data.table begins at 1.
      begin_row <- end_row - chunk_size + 1
      print(glue::glue("begin_row is {begin_row}"))
      print(glue::glue("end_row is {end_row}"))
      
      covar_subpop_random_effect_rows_20plus <- c((begin_row):(end_row))
      print(glue::glue("covar_subpop_random_effect_rows_20plus is {paste(covar_subpop_random_effect_rows_20plus, collapse=',')}"))
      
      # check that the number of rows is correct
      if (length(covar_subpop_random_effect_rows_20plus) != chunk_size) {
        stop(glue::glue("covar_subpop_random_effect_rows_20plus should be of length {chunk_size} but instead it has length {length(covar_subpop_random_effect_rows_20plus)}"))
      }
      
      # subset / pull out the rows that are for this covariate
      covar_subpop_random_effect_20plus <- covar_subpop_random_effects_20plus[covar_subpop_random_effect_rows_20plus, ]
      
      # check that the number of rows is correct
      if (nrow(covar_subpop_random_effect_20plus) != chunk_size) {
        stop(glue::glue("covar_subpop_random_effect_20plus should be of length {chunk_size} but instead it has {nrow(covar_subpop_random_effect_20plus)} rows"))
      }
      
      # include age for merging
      covar_subpop_random_effect_20plus[, age := 0:(num_a - 1) ]
      setkeyv(covar_subpop_random_effect_20plus, "age")
      
      # pull out the current covariate
      covar_random_effect_20plus <- data[, get(covars_subpop_20plus[i])]
      
      # Add these age covariate slopes to the other random effects
      print("adding to re...")
      re <- re + (covar_random_effect_20plus * covar_subpop_random_effect_20plus[pred_frame[, list(age)], simvars, with = F] * as.integer(pred_frame[, age >= recode$age[age == 20, age_recode]]))
      print(glue::glue("Done with loop {i}."))
    } 
  }
  if (model %in% c("state_model8", "state_model10", "state_model12", "state_model13", "state_model15", "state_model19", "state_model20")) {
    covar_subpop_random_effects_60under <- data.table(as.matrix(sims[names(mean) == "covar_subpop_re_matrix_60under", ]))
    
    # add in the effect of the race/ethnicity or edu specific covariates by iterating over each of the race/ethnicity or edu specific covariates
    for (i in c(1:length(covars_subpop_60under))) {
      print(glue::glue("Working on covariate number {i}: {covars_subpop_60under[i]}"))
      
      # how long is each section of covar_subpop_re_matrix? that is, which entries of this vector correspond to the current covariate?
      chunk_size <- nrow(covar_subpop_random_effects_60under) / length(covars_subpop_60under)
      print(glue::glue("chunk_size = {chunk_size}"))
      
      # the row/index that marks the end of the current covariates chunk of covar_subpop_re_matrix
      end_row <- chunk_size * i
      
      # create an index that identifies the correct rows of covar_subpop_re_matrix. Add 1 at the start so that the index begins at 1, because row numbers in data.table begins at 1.
      begin_row <- end_row - chunk_size + 1
      print(glue::glue("begin_row is {begin_row}"))
      print(glue::glue("end_row is {end_row}"))
      
      covar_subpop_random_effect_rows_60under <- c((begin_row):(end_row))
      print(glue::glue("covar_subpop_random_effect_rows_60under is {paste(covar_subpop_random_effect_rows_60under, collapse=',')}"))
      
      # check that the number of rows is correct
      if (length(covar_subpop_random_effect_rows_60under) != chunk_size) {
        stop(glue::glue("covar_subpop_random_effect_rows_60under should be of length {chunk_size} but instead it has length {length(covar_subpop_random_effect_rows_60under)}"))
      }
      
      # subset / pull out the rows that are for this covariate
      covar_subpop_random_effect_60under <- covar_subpop_random_effects_60under[covar_subpop_random_effect_rows_60under, ]
      
      # check that the number of rows is correct
      if (nrow(covar_subpop_random_effect_60under) != chunk_size) {
        stop(glue::glue("covar_subpop_random_effect_60under should be of length {chunk_size} but instead it has {nrow(covar_subpop_random_effect_60under)} rows"))
      }
      
      # include age for merging
      covar_subpop_random_effect_60under[, age := 0:(num_a - 1) ]
      setkeyv(covar_subpop_random_effect_60under, "age")
      
      # pull out the current covariate
      covar_random_effect_60under <- data[, get(covars_subpop_60under[i])]
      
      # Add these age covariate slopes to the other random effects
      print("adding to re...")
      re <- re + (covar_random_effect_60under * covar_subpop_random_effect_60under[pred_frame[, list(age)], simvars, with = F] * as.integer(pred_frame[, age <= recode$age[age == 60, age_recode]]))
      print(glue::glue("Done with loop {i}."))
    } 
  }
  
  if (model %in% c( "state_model13", "state_model15")) {
    covar_subpop_random_effects_5plus <- data.table(as.matrix(sims[names(mean) == "covar_subpop_re_matrix_5plus", ]))
    
    # add in the effect of the race/ethnicity or edu specific covariates by iterating over each of the race/ethnicity or edu specific covariates
    for (i in c(1:length(covars_subpop_5plus))) {
      print(glue::glue("Working on covariate number {i}: {covars_subpop_5plus[i]}"))
      
      # how long is each section of covar_subpop_re_matrix? that is, which entries of this vector correspond to the current covariate?
      chunk_size <- nrow(covar_subpop_random_effects_5plus) / length(covars_subpop_5plus)
      print(glue::glue("chunk_size = {chunk_size}"))
      
      # the row/index that marks the end of the current covariates chunk of covar_subpop_re_matrix
      end_row <- chunk_size * i
      
      # create an index that identifies the correct rows of covar_subpop_re_matrix. Add 1 at the start so that the index begins at 1, because row numbers in data.table begins at 1.
      begin_row <- end_row - chunk_size + 1
      print(glue::glue("begin_row is {begin_row}"))
      print(glue::glue("end_row is {end_row}"))
      
      covar_subpop_random_effect_rows_5plus <- c((begin_row):(end_row))
      print(glue::glue("covar_subpop_random_effect_rows_5plus is {paste(covar_subpop_random_effect_rows_5plus, collapse=',')}"))
      
      # check that the number of rows is correct
      if (length(covar_subpop_random_effect_rows_5plus) != chunk_size) {
        stop(glue::glue("covar_subpop_random_effect_rows_5plus should be of length {chunk_size} but instead it has length {length(covar_subpop_random_effect_rows_5plus)}"))
      }
      
      # subset / pull out the rows that are for this covariate
      covar_subpop_random_effect_5plus <- covar_subpop_random_effects_5plus[covar_subpop_random_effect_rows_5plus, ]
      
      # check that the number of rows is correct
      if (nrow(covar_subpop_random_effect_5plus) != chunk_size) {
        stop(glue::glue("covar_subpop_random_effect_5plus should be of length {chunk_size} but instead it has {nrow(covar_subpop_random_effect_5plus)} rows"))
      }
      
      # include age for merging
      covar_subpop_random_effect_5plus[, age := 0:(num_a - 1) ]
      setkeyv(covar_subpop_random_effect_5plus, "age")
      
      # pull out the current covariate
      covar_random_effect_5plus <- data[, get(covars_subpop_5plus[i])]
      
      # Add these age covariate slopes to the other random effects
      print("adding to re...")
      re <- re + (covar_random_effect_5plus * covar_subpop_random_effect_5plus[pred_frame[, list(age)], simvars, with = F] * as.integer(pred_frame[, age >= recode$age[age == 5, age_recode]]))
      print(glue::glue("Done with loop {i}."))
    } 
  }
  
  if (model %in% c( "state_model20")) {
    covar_subpop_random_effects_15plus <- data.table(as.matrix(sims[names(mean) == "covar_subpop_re_matrix_15plus", ]))
    
    # add in the effect of the race/ethnicity or edu specific covariates by iterating over each of the race/ethnicity or edu specific covariates
    for (i in c(1:length(covars_subpop_15plus))) {
      print(glue::glue("Working on covariate number {i}: {covars_subpop_15plus[i]}"))
      
      # how long is each section of covar_subpop_re_matrix? that is, which entries of this vector correspond to the current covariate?
      chunk_size <- nrow(covar_subpop_random_effects_15plus) / length(covars_subpop_15plus)
      print(glue::glue("chunk_size = {chunk_size}"))
      
      # the row/index that marks the end of the current covariates chunk of covar_subpop_re_matrix
      end_row <- chunk_size * i
      
      # create an index that identifies the correct rows of covar_subpop_re_matrix. Add 1 at the start so that the index begins at 1, because row numbers in data.table begins at 1.
      begin_row <- end_row - chunk_size + 1
      print(glue::glue("begin_row is {begin_row}"))
      print(glue::glue("end_row is {end_row}"))
      
      covar_subpop_random_effect_rows_15plus <- c((begin_row):(end_row))
      print(glue::glue("covar_subpop_random_effect_rows_15plus is {paste(covar_subpop_random_effect_rows_15plus, collapse=',')}"))
      
      # check that the number of rows is correct
      if (length(covar_subpop_random_effect_rows_15plus) != chunk_size) {
        stop(glue::glue("covar_subpop_random_effect_rows_15plus should be of length {chunk_size} but instead it has length {length(covar_subpop_random_effect_rows_15plus)}"))
      }
      
      # subset / pull out the rows that are for this covariate
      covar_subpop_random_effect_15plus <- covar_subpop_random_effects_15plus[covar_subpop_random_effect_rows_15plus, ]
      
      # check that the number of rows is correct
      if (nrow(covar_subpop_random_effect_15plus) != chunk_size) {
        stop(glue::glue("covar_subpop_random_effect_15plus should be of length {chunk_size} but instead it has {nrow(covar_subpop_random_effect_15plus)} rows"))
      }
      
      # include age for merging
      covar_subpop_random_effect_15plus[, age := 0:(num_a - 1) ]
      setkeyv(covar_subpop_random_effect_15plus, "age")
      
      # pull out the current covariate
      covar_random_effect_15plus <- data[, get(covars_subpop_15plus[i])]
      
      # Add these age covariate slopes to the other random effects
      print("adding to re...")
      re <- re + (covar_random_effect_15plus * covar_subpop_random_effect_15plus[pred_frame[, list(age)], simvars, with = F] * as.integer(pred_frame[, age >= recode$age[age == 15, age_recode]]))
      print(glue::glue("Done with loop {i}."))
    } 
  }
  
  if (model %in% c( "state_model15", "state_model24")) {
    covar_subpop_random_effects_1plus <- data.table(as.matrix(sims[names(mean) == "covar_subpop_re_matrix_1plus", ]))
    
    # add in the effect of the race/ethnicity or edu specific covariates by iterating over each of the race/ethnicity or edu specific covariates
    for (i in c(1:length(covars_subpop_1plus))) {
      print(glue::glue("Working on covariate number {i}: {covars_subpop_1plus[i]}"))
      
      # how long is each section of covar_subpop_re_matrix? that is, which entries of this vector correspond to the current covariate?
      chunk_size <- nrow(covar_subpop_random_effects_1plus) / length(covars_subpop_1plus)
      print(glue::glue("chunk_size = {chunk_size}"))
      
      # the row/index that marks the end of the current covariates chunk of covar_subpop_re_matrix
      end_row <- chunk_size * i
      
      # create an index that identifies the correct rows of covar_subpop_re_matrix. Add 1 at the start so that the index begins at 1, because row numbers in data.table begins at 1.
      begin_row <- end_row - chunk_size + 1
      print(glue::glue("begin_row is {begin_row}"))
      print(glue::glue("end_row is {end_row}"))
      
      covar_subpop_random_effect_rows_1plus <- c((begin_row):(end_row))
      print(glue::glue("covar_subpop_random_effect_rows_1plus is {paste(covar_subpop_random_effect_rows_1plus, collapse=',')}"))
      
      # check that the number of rows is correct
      if (length(covar_subpop_random_effect_rows_1plus) != chunk_size) {
        stop(glue::glue("covar_subpop_random_effect_rows_1plus should be of length {chunk_size} but instead it has length {length(covar_subpop_random_effect_rows_1plus)}"))
      }
      
      # subset / pull out the rows that are for this covariate
      covar_subpop_random_effect_1plus <- covar_subpop_random_effects_1plus[covar_subpop_random_effect_rows_1plus, ]
      
      # check that the number of rows is correct
      if (nrow(covar_subpop_random_effect_1plus) != chunk_size) {
        stop(glue::glue("covar_subpop_random_effect_1plus should be of length {chunk_size} but instead it has {nrow(covar_subpop_random_effect_1plus)} rows"))
      }
      
      # include age for merging
      covar_subpop_random_effect_1plus[, age := 0:(num_a - 1) ]
      setkeyv(covar_subpop_random_effect_1plus, "age")
      
      # pull out the current covariate
      covar_random_effect_1plus <- data[, get(covars_subpop_1plus[i])]
      
      # Add these age covariate slopes to the other random effects
      print("adding to re...")
      # Note: temp change to >= 5 because did not include age group 1 for this all-cause model run
      re <- re + (covar_random_effect_1plus * covar_subpop_random_effect_1plus[pred_frame[, list(age)], simvars, with = F] * as.integer(pred_frame[, age >= recode$age[age == 5, age_recode]]))
      print(glue::glue("Done with loop {i}."))
    } 
  }
  
  # calculate total draws; we recycle the fe object here to reduce memory load
  fe <- fe + re
  
  draws <- as.matrix(fe) # rename the combined fe + re object to draws
  rm(fe, re); gc()
}

#### Load prediction frame
if (by_sex) {
  pred_frame <- readRDS(paste0(output_dir, "/pred_frame_", sex, ".rds"))
} else {
  pred_frame <- readRDS(paste0(output_dir, "/pred_frame.rds"))
}

if ("state" %in% names(pred_frame)) {
  setkeyv(pred_frame, c("area", "state","year", "sex", "race", "age"))
} else {
  setkeyv(pred_frame, c("area", "year", "sex", "race", "age"))
}


#### Recode pred_frame
pred_frame_recoded <- copy(pred_frame)
if (!is.null(pred_frame_recoded$race)) {
  pred_frame_recoded <- merge(pred_frame_recoded, recode$race, by.x = "race", by.y = "race_recode")
  pred_frame_recoded[, c("race", "race.y", "var") := list(race.y, NULL, NULL)]
}
if (!is.null(pred_frame_recoded$source)) {
  pred_frame_recoded <- merge(pred_frame_recoded, recode$source, by.x = "source", by.y = "source_index_recode")
  pred_frame_recoded[, c("source", "var") := list(source_index, NULL)]
}
if (!is.null(pred_frame_recoded$sex)) {
  pred_frame_recoded <- merge(pred_frame_recoded, recode$sex, by.x = "sex", by.y = "sex_recode")
  pred_frame_recoded[, c("sex", "sex.y", "var") := list(sex.y, NULL, NULL)]
}
if (!is.null(pred_frame_recoded$year)) {
  pred_frame_recoded <- merge(pred_frame_recoded, recode$year, by.x = "year", by.y = "year_recode")
  pred_frame_recoded[, c("year", "year.y", "var") := list(year.y, NULL, NULL)]
}
if (!is.null(pred_frame_recoded$age)) {
  pred_frame_recoded <- merge(pred_frame_recoded, recode$age, by.x = "age", by.y = "age_recode")
  pred_frame_recoded[, c("age", "age.y", "var") := list(age.y, NULL, NULL)]
}
if (!is.null(pred_frame_recoded$survey_version)) {
  pred_frame_recoded[, survey_version := NULL]
}
if (!is.null(pred_frame_recoded$area)) {
  pred_frame_recoded <- merge(pred_frame_recoded, recode$area, by.x = "area", by.y = "area_recode")
  pred_frame_recoded[, c("area", "mcnty", "var") := list(mcnty, NULL, NULL)]
}
if (!is.null(pred_frame_recoded$state)) {
  pred_frame_recoded <- merge(pred_frame_recoded, recode$state, by.x = "state", by.y = "state_recode")
  pred_frame_recoded[, c("state", "state.y", "var") := list(state, NULL, NULL)]
}
if (!is.null(pred_frame_recoded$edu)) {
  pred_frame_recoded <- merge(pred_frame_recoded, recode$edu, by.x = "edu", by.y = "edu_recode")
  pred_frame_recoded[, c("edu", "edu.y", "var") := list(edu.y, NULL, NULL)]
}
if (!is.null(pred_frame_recoded$phone)) {
  pred_frame_recoded <- merge(pred_frame_recoded, recode$phone, by.x = "phone", by.y = "phone_recode")
  pred_frame_recoded[, c("phone", "phone.y", "var") := list(phone.y, NULL, NULL)]
}
if (!is.null(pred_frame_recoded$marital)) {
  pred_frame_recoded <- merge(pred_frame_recoded, recode$marital, by.x = "marital", by.y = "marital_recode")
  pred_frame_recoded[, c("marital", "marital.y", "var") := list(marital.y, NULL, NULL)]
}
if (!is.null(pred_frame_recoded$tenure)) {
  pred_frame_recoded <- merge(pred_frame_recoded, recode$tenure, by.x = "tenure", by.y = "tenure_recode")
  pred_frame_recoded[, c("tenure", "tenure.y", "var") := list(tenure.y, NULL, NULL)]
}
pred_frame_recoded[, int := NULL]

# Set key (and order) of pred_frame_recoded to match pred_frame and data
if ("state" %in% names(pred_frame)) {
  setkeyv(pred_frame_recoded, c("area", "state","year", "sex", "race", "age"))
} else {
  setkeyv(pred_frame_recoded, c("area", "year", "sex", "race", "age"))
} # same as pred_frame

#### Add pred_frame_recoded to draws
cols <- c("area", "age", "year", "sex", "race", strat_vars)
draws <- cbind(draws, pred_frame_recoded[, ..cols])

if (!is.null(strat_vars)) {
  #### Load post-stratification pop file
  stratpop <- readRDS(paste0(output_dir_draws_est, "/stratpop_", sex, "_", race, ".rds"))
  
  #### Convert factors to integers
  stratpop$age <- as.integer(as.character(stratpop$age))
  if ("edu" %in% strat_vars) {
    stratpop$edu <- as.integer(as.character(stratpop$edu))
  }
  if ("marital" %in% strat_vars) {
    stratpop$marital <- as.integer(as.character(stratpop$marital))
  }
  if ("phone" %in% strat_vars) {
    stratpop$phone <- as.integer(as.character(stratpop$phone))
  }
  if ("gq" %in% strat_vars) {
    stratpop$gq <- as.integer(as.character(stratpop$gq))
  }
  
  
  #### Merge post-stratification weights onto draws
  draws <- merge(draws, stratpop, by.x = c("area", "age", "year", "sex", "race", strat_vars), by.y = c("mcnty", "age", "year", "sex", "race", strat_vars), all.x = TRUE)
  
  #### Set wt to 1 when NA (i.e., for ages < 20)
  draws[is.na(wt), wt := 1]
  
  #### Collapse by post-stratification vars
  draws_agg <- list()
  for (ss in 1:draw_width) {
    print(ss)
    col <- paste0("V", ss)
    draws_agg[[ss]] <- draws[, list(sim = ss, pred = weighted.mean(get(col), wt, na.rm = TRUE)), by = c("area", "year", "sex", "age", "race")]
  }
  draws <- rbindlist(draws_agg)
  
  #### dcast back to wide
  draws <- dcast(draws, area + year + sex + age + race ~ sim, value.var = "pred")
}

## Split out draws by year, calculate all-ages, age-standardized draws, collapse draws, and save ---
# load and subset the population file, then merge onto draws
pop <- readRDS(pop_file)
if (!by_race) pop[, race := 9]

if (race_together) {
  if (by_sex) {
    pop <- pop[sex == get("sex", .GlobalEnv), list(pop = sum(pop)), by = c(area_var, "year", "age", "race")]
    merge_vars <- c("area", "year", "age")
  } else {
    pop <- pop[, list(pop = sum(pop)), by = c(area_var, "year", "age", "race", "sex")]
    merge_vars <- c("area", "year", "age", "sex")
  }
  merge_vars <- c(merge_vars, "race")
} else {
  if (by_sex) {
    pop <- pop[race == get("race", .GlobalEnv) & sex == get("sex", .GlobalEnv),
               list(pop = sum(pop)), by = c(area_var, "year", "age")]
    merge_vars <- c("area", "year", "age")
  } else {
    pop <- pop[race == get("race", .GlobalEnv),
               list(pop = sum(pop)), by = c(area_var, "year", "age", "sex")]
    merge_vars <- c("area", "year", "age", "sex")
  }
}

setnames(pop, area_var, "area")
draws <- merge(draws, pop, by = merge_vars, all.x = TRUE)

# No missing values are expected
stopifnot(!any(is.na(draws)))

# load the age standard
std_wt <- readRDS(age_std_file)[, list(age, wt = wt / sum(wt))]

# split up data and process separately by year
all_draws <- draws
rm(draws)

all_draws <- lapply(years, function(this_year) all_draws[year == this_year, ])
for (this_year in years) {
  cat(paste(Sys.time(), this_year, "\n"))
  draws <- all_draws[[which(years == this_year)]]
  draws[, level := area_var]
  if (nrow(draws) == 0) next # this can happen in validation models
  
  # reshape long for easier processing
  draws <- melt(draws, id.vars = c("level", "area", "year", "sex", "race", "age", "pop"), variable.name = "sim", value.name = "pred")
  draws[, sim := as.integer(sim)]
  
  # re-code the draws to correspond to draw_val
  draws[, sim := sim + (min(draw_val) - 1)]
  
  # specify the columns by which to summarize/aggregate/etc since now we have both adjusted and unadjusted points
  merge_vars <- c("level", "area", "year", "sex", "race", "sim")
  
  # add all-ages rates
  draws <- calc_all_ages(draws, std_wt, "pred", merge_vars)
  
  # save draws and estimates
  if (race_together) {
    for (r in races) {
      message(r)
      if (by_sex) {
        saveRDS(draws[race == r], file = paste0(output_dir_draws_est, "/draws/draws_", area_var, "_", this_year, "_", sex, "_", r, "_",
                                                max(draw_val),"_", imp, ".rds"))
      } else {
        for (s in 1:2) {
          saveRDS(draws[race == r & sex == s], file = paste0(output_dir_draws_est, "/draws/draws_", area_var, "_", this_year, "_", s, "_", r, "_",
                                                             max(draw_val),"_", imp, ".rds"))
        }
      }
    }
  } else {
    saveRDS(draws, file = paste0(output_dir_draws_est, "/draws/draws_", area_var, "_", this_year, "_", sex, "_", race, "_",
                                 max(draw_val), "_", imp, ".rds"))
  }
  
  all_draws[[which(years == this_year)]] <- NA
  rm(draws); gc()
}
