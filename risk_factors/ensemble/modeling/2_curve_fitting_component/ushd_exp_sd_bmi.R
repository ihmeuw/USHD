#' @Title calc_metab_bmi_adult_exp_sd_ushd
#' 
#' @Description Adapted calc_metab_bmi_adult_exp_sd function from 
#' central computation/paf/custom/metab_bmi_adult.R
#' 
#' This function estimates the population SD of BMI that minimizes the error at
#' overweight and obese cutpoints for a given mean BMI, prevalence overweight, 
#' prevalence obese, and set of ensemble distribution weights.
#' 
#' Added functionality for education and race, and adapted to use the USHD estimates
#' 
#' @param exp_dt DEPRECATED. Use exp_mean_dir instead. data table of mean BMI. On GBD, this comes from
#' get_exp(rei_id=rei_id, location_id=location_id, year_id=year_id, sex_id=sex_id,
#'         gbd_round_id=gbd_round_id, decomp_step=decomp_step, n_draws=n_draws)
#' @param exp_mean_dir Directory containing mean BMI estimates
#' @param imp Which imputation(s) to run for (should correspond to the draws for a given tasks)
#' @param location_id vector of location_id values to return data for. Defaults to NULL (all counties).
#' @param year_id vector of years to return data for.
#' @param sex_id vector of sex_ids to return data for. Probably 1:2, occasionally just 1 OR 2
#' @param draws_keep an integer or vector of integers with the draw number(s) to keep. 
#' Use draws rescaled for imputation, i.e., 4th draw of 4th imputation should have 
#' draw number 304. Defaults to NULL, i.e., keep all draws
#' @param extra_dim character name of extra dimension of data. This should be "race" or "edu"
#' @param extra_dim_values identifier to load relative to extra_dim. E.g., if
#' extra_dim is "race" and extra_dim_values is 1:3 this loads data for races 1-3
#' @param fx_length The number of increments in fx (the approximate ensemble distribution)
#' @param round_digits a named list representing the number of decimal points
#' to round to before estimating the exp_sd. The exp_sd will be estimate once for each
#' unique set of rounded values, so the more rounding, the fewer computations (but
#' larger possibility for error.)
#' The default is not to meaningfully round:
#'     list(exp_mean = 6,
#'          prev_under = 6,
#'          prev_over = 6,
#'          prev_obese = 6)
#' @param threshold_weights named list ("uw", "ov", "ob") of weight to put on each 
#' prevalence estimate in the objective function. Defaults is:
#' list(uw = 0.16, ov = 0.42, ob = 0.42)
#' Replaces `incl_underweight` argument (although it can still be specified in the
#' settings.csv, and the child_ushd_sd_optimization.R script will convert it into
#' an appropriate threshold_weights args with the defaults)
#' @param save_score logical (default to false). Should we save the score associated
#' with the exp_sd? If saving score, will also save exp_mean. Used for vetting only
#' because otherwise unnecessary to save extra columns
#' @param ens_weight_version_name Name of ensemble weight version in ushd.dbr. See details:
#' @param ensemble_weight_version_ids Ensemble weight version ID in ushd.dbr. Per DB documentation
#' (see link above), ensemble_weight_version_ids supersedes ens_weight_version_name.
#' In general, it's easier to use ens_weight_version_name -- I recommend specifying 
#' the version ID if you'd like to use the NOT best version for a given version_type.
#'
#'
#' @return

# load sae.shared package-----------------------------------------------------------------------------------------------
# sae.shared is dependent on lbd.loader
library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                      R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))

sae.shared_repo <- NULL

# If working directly out of cloned repo of sae.shared, pass its path instead (IFF necessary).

if (is.null(sae.shared_repo)) {
  # loads the most current library maintained by LSAE. If specific version is required,
  # pass it with "version" argument.
  lbd.loader::load_package("sae.shared")
  lbd.loader::load_package("ushd.dbr")
} else {
  lbd.loader::load_package(path = sae.shared_repo)
}

library(ushd.dbr, lib.loc = "FILEPATH")

# these are the distribution functions with updated approaches to solve parameters
source(paste0("FILEPATH"))

# construct dlist based on the pdf_families defined in "FILEPATH"
#   NOTE that the dlist constructed if we source "FILEPATH/pdf_families.R" does not include
#   all of the distributional families used in GBDs
dlist <- c(classA, classB, classM) 

XMAX <- 50 # must be in the global environment for get_density to work
XMIN <- 10

obj_method <- "analytical" # or numerical (ideally set this in the settings file and pass as a variable to the function

calc_metab_bmi_adult_exp_sd_ushd <- function(exp_mean_dir, location_id = NULL, year_id, sex_id,
                                        draws_keep = NULL,
                                        imp, extra_dim, extra_dim_values,
                                        threshold_weights = list(uw = 0.16, ov = 0.42, ob = 0.42),
                                        fx_length = 1000,
                                        control = NULL,
                                        round_digits = list(exp_mean = 6,
                                                         under_prev = 6,
                                                         over_prev = 6,
                                                         obes_prev = 6),
                                        save_score = F,
                                        ens_weight_version_name = "GBD2020_weights",
                                        ensemble_weight_version_ids = NULL,
                                        mc_cores = 6,
                                        opt_raked_prev = NULL,
                                        opt_raked_exp_mean = NULL
                                        ) {
  # to protect against possibility that the centrally-managed script is updated to at some point,
  # save some of the distributions that we modified on USHD and check that they are unchanged
  check_classA <- classA
  source("FILEPATH/edensity.R") 
  stopifnot(identical(check_classA, classA))
  
  source(paste0("FILEPATH/ushd_get_edensity_prevs.R"))
  Rcpp::sourceCpp("FILEPATH/scale_density_simpson.cpp")
  Rcpp::sourceCpp(paste0("FILEPATH/ushd_integ_bmi.cpp"))
  
  my.ushd.add_age_group_id <- function(dt) {
    ages <- sort(unique(dt$age))
    lookup <- data.table(age = ages, age_group_id = suppressWarnings(sae.shared::translate_ages(ages, "age_group_id")))
    dt[lookup, on = .(age)]
  }

  # LOAD MEAN BMI & OBESITY/OVERWEIGHT PREV----------------------------------
  
  # get versions of obesity/overweight prev corresponding to mean BMI
  exp_mean_settings <- fread(paste0(exp_mean_dir, "settings.csv"), header = F, stringsAsFactors = F) # load settings corresponding to mean BMI to get the corresponding prevs

  if(extra_dim == "edu") stop("cannot currently process edu with how prev files read b/c imp number is where education is expected to be in file path")
  
  for(arg in c("in_underweight_est", 
               "in_overweight_est",
               "in_obese_est")){
    arg_value <- gsub("\\\"", "", exp_mean_settings[V1 == arg, V2])
    # remove the "est" directory to put it at the model base dir
    arg_value <- gsub("/est/", "/", arg_value)
    assign(arg, arg_value)
  }
  
  if(length(in_underweight_est) == 0 | threshold_weights$uw == 0){ # skip loading underweight if the underweight file is not specified or if no weight is put on underweight in optimization
    skip_uw = T
  } else{
    skip_uw = F
  }
  
  if(threshold_weights$uw > 0 & skip_uw){
    stop("Non-zero weight put on underweight threshold, but no underweight file located in settings file")
  }
  if(skip_uw){
    if(sum(grepl("under", names(round_digits))>0)){
      temp <- grep("under", names(round_digits), value = T)
      message(sprintf("Underweight prev is not used in optimization either because threshold_weights$uw == 0 or an underweight prevalence model was not specified. 
                      However, %s is an item in round_digits.\n%s will be removed from round_digits.", temp, temp))
      round_digits <- round_digits[which(!grepl("under", names(round_digits))>0)]
    }
  }
  
  
  files_prev <- sae.shared::expand_file_template(
    sprintf("draws/draws_mcnty_%s_%s_%s_%s%s.rds", year, sex, race, edu, raked),
    year = year_id,
    sex = sex_id,
    race = if (identical(extra_dim, "race")) extra_dim_values else sae.shared::race_default,
    edu =  if (identical(extra_dim, "edu"))  extra_dim_values else sae.shared::edu_default,
    raked = ifelse(opt_raked_prev, "_raked", "")
  )
  # if opt_raked_exp_mean and opt_raked_prev are both TRUE or both FALSE, 
  # then files_mean will equal files_prev. If they are not the same,
  # modify files_prev to set whether or not to load the raked mean BMI files
  if(opt_raked_prev == opt_raked_exp_mean){
    files_mean <- files_prev
  } else if (opt_raked_mean) {
    # if using raked mean but unraked prev, add raked to the filenames
    files_mean <- gsub(".rds", "_raked.rds", files_prev)
  } else {
    # if using unraked mean but raked prev, remove raked from the filenames
    files_mean <- gsub("_raked.rds", ".rds", files_prev)
  }
    

  lookup_loc <- NULL
  locid <- location_id # avoid collision with data.table env cleverness
  load.file <- function(f, 
                        model_dir, 
                        imp
                        ) {
    if(!is.null(imp) && imp > 0){ # if imp is zero, treate it like no imp
      message("Loading ", model_dir, "imputation", imp, "/", f)
      draws <- readRDS(file.path(model_dir, paste0("imputation", imp), f))
      setnames(draws, "sim", "draw")
      # renumber draws based on imputation
      # note that this only works if there are 100 draws per imputation
      
      # ii <- stringr::str_extract(f, pattern = "[:digit:]*.rds")  
      # ii <- as.numeric(gsub(".rds", "", ii))
      # update draw numbers to deduplicate across imputations, if not already done
      if(draws[, min(draw)] == 1 & draws[, max(draw) == 100] & imp != 1){ 
        draws[, draw := (imp-1)*100 + draw]  
      } 
      draws[, imp := imp]
    } else { # load all imp when imp is passed as NULL (only in the case of exp_mean model, which is not saved by imputation)
      message("Loading ", model_dir, "/", f)
      draws <- readRDS(file.path(model_dir, f))
      setnames(draws, "sim", "draw")
      draws[, imp := 0] # imp is needed for merging below
    }
    
    # filter draws
    if(is.null(draws_keep)){
      draws_keep <- draws[, unique(draw)] # if draws_keep is null, use all draws; otherwise, filter to specified draw in the next step
    }
    # NOTE: USHD numbers draws 1:N, but GBD does 0:N-1
    draws <- draws[draw %in% draws_keep]
    
    # drop aggregate age groups (all age/age standardized)
    draws <- draws[!age %in% c(98, 99)]

    # if the column name "source_v2" is present, filter
    # to source_v2 == BRFSS_LLCP ("gold standard"), remove the column,
    # and provide a warning message that notes that
    # we have assumed this is the gold standard source
    if("source_v2" %in% names(draws)){
      draws <- draws[source_v2 == "BRFSS_LLCP"]
      draws[, source_v2 := NULL]
      warning("Assuming source_v2 == BRFSS_LLCP is the gold standard source")
    }
    
    # get location ids
    if(is.null(lookup_loc)){ # only run once
      locs <- unique(draws[, .(area, level)])
      lookup_loc <- locs[, location_id := ushd.dbr::translate_area(level, area)][, level := NULL]
    }
    draws <- draws[lookup_loc, on = .(area)]
    
    # get age ids
    draws <- my.ushd.add_age_group_id(draws)
    
    if(!is.null(locid)){
      draws <- draws[sex %in% sex_id & location_id %in% locid]
    } else{
      draws <- draws[sex %in% sex_id]
    }
    return(draws)
  }
  
  exp_dt <- rbindlist(lapply(files_mean, load.file, model_dir = exp_mean_dir, imp = NULL)) # passing imp = NULL means read all draws
  # if all imp == 0, remove the column
  if("imp" %in% names(exp_dt) & all(exp_dt$imp == 0)) exp_dt[, imp := NULL]
  ov <- rbindlist(lapply(imp, function(ii) {rbindlist(lapply(files_prev, load.file, model_dir = in_overweight_est, imp = ii))}))
  ob <- rbindlist(lapply(imp, function(ii) {rbindlist(lapply(files_prev, load.file, model_dir = in_obese_est, imp = ii))}))
  if(skip_uw){
    message("No in_underweight_est in settings file -- skipping")
  } else{
    un <- rbindlist(lapply(imp, function(ii) {rbindlist(lapply(files, load.file, model_dir = in_underweight_est, imp = ii))}))
  }

  setnames(ov, "pred", "over_prev")
  setnames(ob, "pred", "obes_prev")
  if(!skip_uw){
    setnames(un, "pred", "under_prev")  
  }
  
  # prep columns for merging
  prev_merge_cols <- c("level", "area", "year", "sex", extra_dim, "age", "age_group_id", "location_id", "draw")
  # if imp and edu are in ov, include them in the merge cols.
  if("imp" %in% names(ov)) prev_merge_cols <- c(prev_merge_cols, "imp")
  if("edu" %in% names(ov)) prev_merge_cols <- c(prev_merge_cols, "edu")

  # if "acuase" is in ov, ob, un, or exp_mean, remove it
  if("acause" %in% names(ov)) ov[, acause := NULL]
  if("acause" %in% names(ob)) ob[, acause := NULL]
  if(!skip_uw) if("acause" %in% names(un)) un[, acause := NULL]
  if("acause" %in% names(exp_dt)) exp_dt[, acause := NULL]

  prevs <- merge(ov, ob, by = prev_merge_cols)
  
  if(!skip_uw){
    prevs <- merge(prevs, un, by = prev_merge_cols)  
  }
  
  mean_merge_cols <- c("level", "area", "year", "sex", extra_dim, "age", "age_group_id", "location_id", "draw")
  # if imp and edu are in exp_dt, include them in the merge cols.
  if("imp" %in% names(exp_dt)) mean_merge_cols <- c(mean_merge_cols, "imp")
  if("edu" %in% names(exp_dt)) mean_merge_cols <- c(mean_merge_cols, "edu")

  setnames(exp_dt, "pred", "exp_mean")
  dt <- merge(exp_dt, prevs, by = mean_merge_cols)
  
  if(!skip_uw){
    if(length(unique(c(nrow(dt), nrow(exp_dt), nrow(ob), nrow(ov), nrow(un)))) != 1) stop("THE LENGTHS OF INPUT FILES ARE NOT THE SAME. CHECK ISSUE WITH MERGING")
    rm(prevs, exp_dt, un, ov, ob)
  } else{
    if(length(unique(c(nrow(dt), nrow(exp_dt), nrow(ob), nrow(ov)))) != 1) stop("THE LENGTHS OF INPUT FILES ARE NOT THE SAME. CHECK ISSUE WITH MERGING")
    rm(prevs, exp_dt, ov, ob)
  }
  
  # Load ensemble weights ---------------------------------------------------

  # To avoid repeatedly querying database, cache result for other tasks
  cache_filename <- sprintf("%s/ens_wt_cache.rds", output_dir)
  if(file.exists(cache_filename)){
    wlist <- readRDS(cache_filename)
  } else{
    if(is.numeric(ensemble_weight_version_ids)) must_be_best <- F else must_be_best <- T
    tryCatch(
      {
        
        call_string <- sprintf(
          "get_ensemble_weights(model_run_name = NULL,  version_name = %s,version_type = NULL,ensemble_weight_version_ids = %s, get_best = %s)", 
          ifelse(!is.null(ens_weight_version_name), ens_weight_version_name, "NULL"), 
          ifelse(!is.null(ensemble_weight_version_ids), ensemble_weight_version_ids, "NULL"), 
          must_be_best
        )
        
        if(!is.null(ensemble_weight_version_ids)) ensemble_weight_version_ids <- as.list(ensemble_weight_version_ids) # expects a list of numbers (or a NULL object)
        wlist <- get_ensemble_weights(
          model_run_name = NULL,
          version_name = ens_weight_version_name,
          version_type = NULL,
          ensemble_weight_version_ids = ensemble_weight_version_ids,
          get_best = must_be_best
        )
        
        if(nrow(wlist) == 0){
          warning("get_ensemble_weights returned DT with length 0 -- something went wrong")
          wlist <- paste("get_ensemble_weights returned DT with length 0 -- check db call", call_string, sep = "\n\n")
          
        }
        
        if(wlist[, uniqueN(ensemble_weight_version_id)] != 1){
          wlist <- paste("get_ensemble_weights returned multiple sets of ensemble weights -- check db call", call_string, sep = "\n\n")
        } 
      },
      error = function(e){
        message(e)
        wlist <- paste("get_ensemble_weights failed -- check db call", call_string, sep = "\n\n")
        print(wlist)
      },
      finally = {
        saveRDS(wlist, cache_filename) # write some output even if DB call fails so that we don't repeatedly use the (non-working criteria)
        print(call_string)
      }
    )
  }
  
  stopifnot(is.data.table(wlist))
  
  # Round values in DT ------------------------------------------------------
  dt[, row_id := .I] # create an ID to map rows in the original data to rows in the rounded data (before deduplication) in case row order changes
  dt_rounded <- copy(dt)
  # round each exposure value by the round_digits specified
  dt_rounded[, (names(round_digits)) := lapply(names(round_digits), function(col) round(x = get(col), digits = round_digits[[col]]))]
  
  #### from wlist, deduce which demographic variables have unique weights
  
  # if a column that database expects is missing, it is filled with a list of NULL -- remove those rows
  null_col <- names(which(sapply(wlist, function(c) is.null(unlist(c)))))
  if(length(null_col > 0)){
    wlist[, ((null_col)) := NULL]
  }
  
  setkeyv(wlist, unique(grep("_id", names(wlist), invert = T, value = T)))
  # determine unique strata
  wlist[, ens_weight_strata := .GRP, by = key(wlist)]
  
  # note currently not considering education
  # merge the indicators of unique strata onto the data we're rounding
  dt_rounded <- merge(dt_rounded, 
        unique(wlist[, .(sex_id, age_group_id, population_group_id, ens_weight_strata)]),
        by.x = c("sex", "age_group_id", "race"), by.y = c("sex_id", "age_group_id", "population_group_id"),
        all.x = T)
  stopifnot(dt_rounded[is.na(ens_weight_strata), .N] == 0)
  
  full_rounded <- dt_rounded[, c("ens_weight_strata", names(round_digits), "row_id"), with = F]
  # create a key representing each unique combination of values (rounded exposure vals and strata ID)
  # This will be used to merge exp_sd vals from the reduced dataset to the original DT
  full_rounded[, combo_key := .GRP, by = setdiff(names(full_rounded), "row_id")] # Keep row_id so can merge the combo_key to original data
  dt_rounded <- unique(full_rounded[, c("ens_weight_strata", names(round_digits), "combo_key"), with = F]) # reduce rows to unique prev/mean values and ensemble weight strata
  
  dt <- full_rounded[, .(row_id, combo_key)][dt, on = "row_id"]
  stopifnot(sort(dt[, unique(combo_key)]) == sort(dt_rounded[, unique(combo_key)]))
  rm(full_rounded)
  
  print("Set rounding digits as:")
  print(round_digits)
  print(sprintf("Rounded DT had %i rows, a %#.1f percent reduction over original (%i)", dt_rounded[, .N], (1- dt_rounded[, .N]/dt[, .N])*100, dt[, .N]))
  
  if(skip_uw){
    dt_rounded[, under_prev := -100] # cannot leave NULL or else optimizer fails, but it will be given weight of zero in the optimizer, so can give a wild value
  }
  #--CALC EXPOSURE SD----------------------------------------------------------
  
  fit_bmi_ensemble <- function(b, over, obese, under, weights, mean, ll, which_method) {
    stopifnot(which_method %in% c("analytical", "numerical"))
    tryCatch({
      if(which_method == "analytical"){
        out <- NULL
        out <- get_edensity_prevs(weights = weights, mean = mean, sd = b, .min = XMIN, .max = XMAX, scale = T)          
      } else if (which_method == "numerical"){
        fx <- NULL
        fx <- get_edensity(weights = weights, mean = mean, sd = b, .min = XMIN, .max = XMAX, scale = T, .length = ll) # I used `b` instead of `Vectorize(b)` to avoid an error (vectorize used in code in PAF calc); I don't think it makes a difference in the results b/c b is not a function as far as I can tell
        out <- NULL
        out <- integ_bmi(fx$x, fx$fx) # calculates the prevalence of underweight, overweight, obese
      }
      threshold_weights$ov*(out$over-over)^2 + threshold_weights$ob*(out$obese-obese)^2 + threshold_weights$uw*(out$under-under)^2
    }, error=function(e){
      # write out fit_bmi_ensemble contents associated with error:
      issue_settings <- data.table(
        date = Sys.time(),
        b = b, 
        mean = mean,
        over = over,
        under = under,
        under = under,
        ll = ll,
        which_method = which_method,
        weights = weights
      )
      fwrite(x = issue_settings,
             file = sprintf("%s/ISSUES_fit_bmi_ensemble.csv", output_dir),
             append = T)
      cat("ERROR :",conditionMessage(e), "\n")

      # to read in...
      row = 1
      for(var in grep("weights|date", names(issues), invert = T, value = T)){
        assign(var, issues[row, get(var)])
      }
      weights <- issues[row, grep("weights", names(issues), value = T), with = F]
      names(weights) <- gsub("weights.", "", names(weights))
      # to see the unique weight sets with issues:
      # unique(issues[, grep("weights", names(issues), value = T), with = F])
      
      })
  }
  fit_bmi_ensemble <- compiler::cmpfun(fit_bmi_ensemble)
  calc_exp_sd <- function(i, fx_length, control_settings, my_method){
    weights <- merge(dt_rounded[i, .(ens_weight_strata)], wlist, by = "ens_weight_strata")
    weights[, c("ens_weight_strata", grep("_id", names(weights), value = T)) := NULL]
    weights <- unique(weights)
    
    stopifnot(weights[, .N] == 1)
    
    optPARAMS=list()
    optVALS=list()
    for(p in c(5)){ # after updates to PDF families and objective function, one starting value is sufficient in large majority of cases.
      SOPT <- nlminb(start=dt_rounded[i, ]$exp_mean/p,objective = fit_bmi_ensemble,
                     over=dt_rounded[i, ]$over_prev, obese=dt_rounded[i, ]$obes_prev, under=dt_rounded[i, ]$under_prev, 
                     weights=weights, mean=dt_rounded[i, ]$exp_mean, ll = fx_length, which_method = my_method,
                     lower=pmax(2,dt_rounded[i, ]$exp_mean*.06), upper=pmin(dt_rounded[i, ]$exp_mean*1.5, 30), # upper and lower values of SD to test -- set limits relative to mean BMI & some limits that are outside the plausible range/cause issues with ensemble distribution
                     control=control_settings)
      optPARAMS = rbind(optPARAMS, SOPT$par)
      optVALS = rbind(optVALS, SOPT$objective)
    }
    # ensures that it's not returning a valid exp_sd if optimizer errors out
    if(!is.infinite(min(unlist(optVALS))) & is.numeric(min(unlist(optVALS)))){
      if(save_score){ # saves the min objective function value when TRUE & the implied prevs associated with optimal exp_sd
        opt_sd <- optPARAMS[which.min(optVALS),][[1]]
        if(my_method == "analytical"){
          out <- get_edensity_prevs(weights = weights, mean = dt_rounded[i,]$exp_mean, sd = opt_sd, .min = 10, .max = 50, scale = T) # recalc ens dist for the optimal exp_sd
        } else if(my_method == "numerical"){
          fx <- get_edensity(weights = weights, mean = dt_rounded[i,]$exp_mean, sd = opt_sd, .min = 10, .max = 50, scale = T, .length = fx_length) # recalc ens dist for the optimal exp_sd
          out <- integ_bmi(fx$x, fx$fx) # calc the prevalences  
        } else{
          stop("need to provide methods to evaluate prevalences of edensity")
        }
        
        return(list(optPARAMS[which.min(optVALS),][[1]],
                    min(unlist(optVALS)),
                    out$under,
                    out$over,
                    out$obese))
      } else{
        return(optPARAMS[which.min(optVALS),][[1]])  
      }
        
    } else{
      return(-1)
    }
    
    
  }
  calc_exp_sd <- compiler::cmpfun(calc_exp_sd)
  t1 <- Sys.time()
  message("---- starting optimization: ", t1, "/n")
  exp_sds <- mclapply(1:nrow(dt_rounded), calc_exp_sd, mc.cores = mc_cores, fx_length = fx_length, control_settings = control, my_method = obj_method)
  t2 <- Sys.time()
  message("---- finished optimization: ", t2, "/n")
  message("---- time to complete: ")
  print(t2- t1)
  
  if(save_score) {
    exp_sds = as.data.table(lapply(1:length(exp_sds[[1]]), function(x) unlist(lapply(exp_sds, `[`, x))))
    names(exp_sds) <- c("pred", "score", "implied_under", "implied_over", "implied_obese") # "implied" refers to implied by ensemble dist
    if(exp_sds[, .N] != dt_rounded[, .N]) stop("ERROR: LENGTH OF EXP_SD != LENGTH OF EXP_MEAN, SO THE OUTPUT CANNOT BE TRUSTED")
    dt_rounded <- cbind(dt_rounded, exp_sds)    
  } else{
    exp_sds <- unlist(lapply(exp_sds, `[`, 1))
    if(length(exp_sds) != dt_rounded[, .N]) stop("ERROR: LENGTH OF EXP_SD != LENGTH OF EXP_MEAN, SO THE OUTPUT CANNOT BE TRUSTED")
    dt_rounded <- cbind(dt_rounded, pred = exp_sds)
  }
  
  # merge the rounded DT back to the full data.table with all rows
  cc <- c("pred", "combo_key")  
  if(save_score) cc <- c(cc, "score",  "implied_under", "implied_over", "implied_obese")
  nrow_1 <- dt[,.N]
  dt <- merge(dt, dt_rounded[, ..cc], by = "combo_key")  
  nrow_2 <- dt[,.N]
  stopifnot(nrow_1 == nrow_2)
  stopifnot(dt[is.na(combo_key), .N] == 0)
  dt[, combo_key := NULL]
  
  rm(exp_sds)
  return(dt)
}
