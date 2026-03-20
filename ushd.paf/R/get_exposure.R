#' Get data from exposure files, either mean or standard deviation (sd)
#'
#' @param model_exp_run_id (int) exposure model id to get data for.
#' @param year_id (int) vector of years to return data for.
#' @param sex_id (int) vector of sex_ids to return data for. Probably 1:2, occasionally just 1 OR 2
#' @param location_id (int) location id to return data for
#' @param n_draws (int) number of draws to return. If fewer draws exist in the
#' model than requested this will error, and if more draws exist than requested
#' the extra draws will simply be dropped.
#' @param extra_dim character name of extra dimension of data. This should be "race" or "edu"
#' @param extra_dim_values identifier to load relative to extra_dim. E.g., if
#' extra_dim is "race" and extra_dim_values is 1:3 this loads data for races 1-3
#' @param level (character) The geography level to pull files for e.g., mcnty, state, natl
#' @param model_type (character) Either "exposure" or "sae" to specify which type of model data to pull
#'
#' @details This function underlies both get_exp() and get_exp_sd(). Most
#' parameters are used to filter the data provided in model_dir to the specified
#' dimensions.  In current usage the draw/sd files being pulled are always mcnty level.
#'
#' @return Returns data.table with columns age_group_id, sex_id, location_id, year_id, draw, and pred, and extra_dim
ushd.get_exposure <- function(model_exp_run_id, year_id, sex_id, location_id, n_draws, extra_dim, extra_dim_values, level = "mcnty", model_type = "exposure") {
  
  if (model_type == "exposure") {
    # Use the standard exposure model draw files
    draws.files <- ushd.dbr::get_risk_exp_model_draws_file(
      model_exp_run_ids = as.list(model_exp_run_id),
      level = level,
      years = as.list(year_id),
      sex = as.list(sex_id),
      race = if (extra_dim == 'race') as.list(extra_dim_values) else NULL,
      edu = if (extra_dim == 'edu') as.list(extra_dim_values) else NULL
    )
  } else if(model_type == "sae") {
    # Use SAE model prediction draw files
    draws.files <- rbindlist(lapply(year_id, function(yr) ushd.dbr::get_sae_model_prediction_draw_file(
      sae_model_run_id  = model_exp_run_id,
      level = level,
      year_id = yr,
      sex_id = sex_id
    )), use.names = T)
    draws.files <- draws.files[population_group_id %in% extra_dim_values]
    setnames(draws.files, "filepath", "file_path") # match outcome of get_risk_exp_model_draws_file
  } else {
    stop("Invalid model_type: must be either 'exposure' or 'sae'")
  }
  
  # Check if optimized draws directory exists
  has.optimized.draws <- function(draws_files) {
    draw.file <- draws_files[1, ]$file_path
    draw.dir <- dirname(draw.file)
    model.dir <- dirname(draw.dir)
    optimized.draw.dir <- file.path(model.dir, "draws_mcnty")
    return(dir.exists(optimized.draw.dir))
  }
  
  if (has.optimized.draws(draws.files)) {
    message("Using optimized draws_mcnty dir")

    # reformat draw.files to use the new paths via a find/replace
    mc <- ushd_client$load_data$get_translate_area_map()$mcnty
    #reverse lookup the mcnty index from the location_id
    mcnty_id <- names(mc)[mc == location_id]
    # handle duplicate '/'
    files <- gsub('/+', '/', draws.files$file_path)
    # MODEL_ROOT/draws/draws_mcnty_2009_1_3_1.rds -> MODEL_ROOT/draws_mcnty/MCNTY/draws_MCNTY_2009_1_3_1.rds
    files <- sub(
      'draws/draws_mcnty',
      sprintf("draws_mcnty/%s/draws_%s", mcnty_id, mcnty_id),
      files
    )
    message("Reading \n", paste(files, collapse = "\n"))
    l_dt <- lapply(files, readRDS)
    draws <- rbindlist(l_dt, use.names = TRUE)
  } else {
    message("No optimized draws_mcnty dir exists. Loading draws directly. This may take a while.")
    
    if(model_type == "exposure") {
      # Get the full draws data for exposure models
      draws <- ushd.dbr::get_risk_exp_model_draws_file(
        model_exp_run_ids = as.list(model_exp_run_id),
        level = level,
        years = as.list(year_id),
        sex = as.list(sex_id),
        race = if (extra_dim == 'race') as.list(extra_dim_values) else NULL,
        edu = if (extra_dim == 'edu') as.list(extra_dim_values) else NULL,
        return_draws = TRUE
      )
    } else if(model_type == "sae") {
      # For SAE models, we need to read the files manually
      message("Reading SAE model draw files")
      l_dt <- lapply(draws.files$file_path, readRDS)
      draws <- rbindlist(l_dt, use.names = TRUE)
    }
  }
  
  message("Done reading")
  # sim not draw?
  if ("sim" %in% colnames(draws)) {
    setnames(
      draws,
      c("sim"),
      c("draw")
    )
  }
  if (draws[, .N] == 0) {
    stop(sprintf("No data found for model %i -- check code for lodaing draws", model_exp_run_id))
  }
  max.draws <- draws[, max(draw)]
  if (max.draws < n_draws) {
    stop(sprintf("User requested %i draws but the data for model %i has only %i draws", n_draws, model_exp_run_id, max.draws))
  } else if (max.draws > n_draws) {
    draws <- draws[draw <= n_draws, ]
  }

  # Translate area and remove unnecessary rows
  draws[, loc_id := ushd.dbr::translate_area(level, area)]
  draws <- draws[loc_id %in% location_id, ]

  # 2. Remove excess data not necessary for later operations
  suppressWarnings(draws[, `:=`(modelable_entity_name = NULL, state = NULL, rei_id = NULL)])

  # 3. Transform draws identifiers
  draws[, draw := draw - 1] # re-number to be 0-based
  
  if(draws[age > 85, .N] > 0){
    # remove age groups above 85 b/c we use age_group_id 160 for 85+ age group. ushd.add_age_group_id excepts that 85 is the largest age group.
    warning("draw file for model_exp_run_id ", model_exp_run_id, " contains age groups ", paste0(draws[age > 85, unique(age)], sep = ","), " which are not recognized and will be removed.")
    draws <- draws[age <= 85]
  }
  draws <- ushd.add_age_group_id(draws)

  setnames(
    draws,
    c("sex",    "year",    "loc_id"),
    c("sex_id", "year_id", "location_id")
  )

  # 4. Remove draws
  draws[, `:=`(age = NULL, level = NULL, area = NULL)]

  # if it's an SAE model, then subset to source == "BRFSS"
  # and delete the source column.
  # Otherwise, error if there is a source (or source_v2) column
  if (model_type == "sae") {
    if(any(grepl("source_v2", colnames(draws)))) {
      stop("not expecting a source_v2 column in SAE model data")
    } else if (any(grepl("source", colnames(draws)))) {
      draws <- draws[source == "BRFSS"]
      draws[, source := NULL]
      warning("Filtering to source == 'BRFSS' for SAE model run ", model_exp_run_id)
      # write a warning message as a text file in the PAF output directory.
      # (only if such a warning has not been written before)
      warn_file <- file.path(out_dir, "/sae_model_run_warning.txt")
      if (!file.exists(warn_file)) {
        writeLines(
          paste("Filtering to source == 'BRFSS' for SAE model run", model_exp_run_id, 
                "because the source column was present in the data. 
                Make sure this is expected (better yet, filter files to gold-standard source in the SAE step)."),
          warn_file
        )
      }
    }
  } else {
    if (any(grepl("source|source_v2", colnames(draws)))) { 
      stop("Exposure model data should not have a source column")
    }
  }

  # 5. Order columns to match
  data.table::setcolorder(draws, c("age_group_id", "sex_id", "location_id", "year_id", "draw", "pred", extra_dim))

  return(draws)
}

ushd.get_exp <- function(model_exp_run_id, year_id, sex_id, location_id, n_draws, extra_dim, extra_dim_values) {
  if(rei_id != 105) { # if not FPG
    res <- ushd.get_exposure(
      model_exp_run_id = model_exp_run_id,
      year_id = year_id,
      sex_id = sex_id,
      location_id = location_id,
      n_draws = n_draws,
      extra_dim = extra_dim,
      extra_dim_values = extra_dim_values)
    # get_exp() in paf/utils/data.R supplies this if there is no "parameter" name in the exp value
    res[, parameter := 'continuous']
  } else { # If FPG
    res <- ushd.get_exp_fpg(
      model_exp_run_id = model_exp_run_id,
      year_id = year_id,
      sex_id = sex_id,
      location_id = location_id,
      n_draws = n_draws,
      extra_dim = extra_dim,
      extra_dim_values = extra_dim_values)
    
    # get_exp() in paf/utils/data.R supplies this if there is no "parameter" name in the exp value
    
      # add a column called "parameter" that explains the "level" (diagnosed/treated),
    # based on db_diagnosis and db_treated
    # sr_prev_no
    # sr_prev_yes_no_tx
    # sr_prev_yes_tx
    res[db_diagnosis == 0 & db_treated == 0, parameter := "sr_prev_no"]
    res[db_diagnosis == 1 & db_treated == 0, parameter := "sr_prev_yes_no_tx"]
    res[db_diagnosis == 1 & db_treated == 1, parameter := "sr_prev_yes_tx"]

    # Create a four level that is TMREL (with prevalence 0); this is used for PAF
    # calculation
    tmp <- copy(res[db_diagnosis == 0 & db_treated == 0])
    tmp[, `:=`(parameter = "tmrel", pred = 0, db_diagnosis = -1, db_treated = -1)] # add dummy values for db_diagnosis and db_treated
    # combine the two data.tables
    res <- rbindlist(list(res, tmp), use.names = TRUE)

    # check that there are no NAs in parameter
    if (any(is.na(res$parameter))) {
      print(res[, .N, .(parameter, db_diagnosis, db_treated)])
      stop("Parameter has NAs")
    }
  }

  setnames(res, 'pred', 'exp_mean')
  

  return(res)
}

ushd.get_exp_sd <- function(model_exp_run_id, year_id, sex_id, location_id, n_draws, extra_dim, extra_dim_values) {
  res <- ushd.get_exposure(
    model_exp_run_id = model_exp_run_id,
    year_id = year_id,
    sex_id = sex_id,
    location_id = location_id,
    n_draws = n_draws,
    extra_dim = extra_dim,
    extra_dim_values = extra_dim_values)

  setnames(res, 'pred', 'exp_sd')
  return(res)
}

# Create a function to get the custom exposure for FPG in USHD
# Pull the diabetes prevalence & treatment prevalence estimates upstream of the
# exposure model (model_exp_run_id). 
# Calculate the prevalence of the categories:
# sr_prev_no
# sr_prev_yes_no_tx
# sr_prev_yes_tx
# (Similar to how implemented in risk_factors/2_exposure/fpg/dm_case_def_pred/calc_diabetes_prev.R)
ushd.get_exp_fpg <- function(model_exp_run_id, year_id, sex_id, location_id, n_draws, extra_dim, extra_dim_values) {
  # Get the sae_model_ids upstream of the exposure model
  tmp_id <- model_exp_run_id
  exp_run <- ushd.dbr::get_risk_exp_model_run(
        rei_id = rei_id,
        model_focus_type = ushd_extra_dim,
        get_best = FALSE)[model_exp_run_id == tmp_id]
  stopifnot(nrow(exp_run) == 1)
  sae_model_ids <- eval(parse(text = sprintf("c(%s)", exp_run$sae_model_run_id)))
  if(length(sae_model_ids) != 4) {
    stop(
      "There should be four sae_model_run_ids upstream of the exposure model. Found ", 
      length(sae_model_ids), " sae_model_run_ids (", 
      paste(sae_model_ids, collapse = ", "), ")")
  }
  
  
  # Determine ID for self-reported diagnosis (MEI 32609)
  sr_prev_model_id <- get_sae_model_run(modelable_entity_id = 32609, is_best = FALSE)[sae_model_run_id %in% sae_model_ids]$sae_model_run_id
  # Determine ID for treatment prevalence (MEI 32611)
  tx_prev_model_id <- get_sae_model_run(modelable_entity_id = 32611, is_best = FALSE)[sae_model_run_id %in% sae_model_ids]$sae_model_run_id
  
  message("Getting self-reported prevalence draws from SAE model ID: ", sr_prev_model_id)
  # Use ushd.get_exposure to get the SAE model data
  sr_prev <- ushd.get_exposure(
    model_exp_run_id = sr_prev_model_id,
    year_id = year_id,
    sex_id = sex_id,
    location_id = location_id,
    n_draws = n_draws,
    extra_dim = extra_dim,
    extra_dim_values = extra_dim_values,
    model_type = "sae"
  )
  # Save the original column name for later
  setnames(sr_prev, "pred", "sr_prev")
  
  message("Getting treatment prevalence draws from SAE model ID: ", tx_prev_model_id)
  # Use ushd.get_exposure to get the treatment SAE model data
  tx_prev <- ushd.get_exposure(
    model_exp_run_id = tx_prev_model_id,
    year_id = year_id,
    sex_id = sex_id,
    location_id = location_id,
    n_draws = n_draws,
    extra_dim = extra_dim,
    extra_dim_values = extra_dim_values,
    model_type = "sae"
  )
  # Save the original column name for later
  setnames(tx_prev, "pred", "tx_prev")
  
  # Calculate the prevalence of the categories
  message("Calculating prevalence categories")
  
  # Merge the two datasets on common fields
  # if extra_dim is race, delete column "edu" (if it exists)

  if (extra_dim == "race" && "edu" %in% names(sr_prev)) {
    sr_prev[, edu := NULL]
    tx_prev[, edu := NULL]
  }
  merge_cols <- c("age_group_id", "sex_id", "location_id", "year_id", "draw", extra_dim)
  all_prev <- merge(sr_prev, tx_prev, by = merge_cols, all = TRUE)
  # check for missing values
  if (anyNA(all_prev)) {
    stop("Missing values in merged data. Check the input data.")
  }
  
  # 1. Create a copy of the data for category 1: No diagnosis (sr_prev_no)
  sr_prev_no <- copy(all_prev)[, `:=`(
    pred = 1 - sr_prev,
    db_diagnosis = 0, 
    db_treated = 0
  )]
  
  # 2. Create a copy for category 2: Diagnosed but not treated (sr_prev_yes_no_tx)
  sr_prev_yes_no_tx <- copy(all_prev)[, `:=`(
    pred = sr_prev - tx_prev,
    db_diagnosis = 1, 
    db_treated = 0
  )]
  
  # 3. Create a copy for category 3: Diagnosed and treated (sr_prev_yes_tx)
  sr_prev_yes_tx <- copy(all_prev)[, `:=`(
    pred = tx_prev,
    db_diagnosis = 1, 
    db_treated = 1
  )]
  
  # Combine all three categories
  # First, drop the original prevalence columns
  sr_prev_no[, c("sr_prev", "tx_prev") := NULL]
  sr_prev_yes_no_tx[, c("sr_prev", "tx_prev") := NULL]
  sr_prev_yes_tx[, c("sr_prev", "tx_prev") := NULL]
  
  # Combine the three categories
  result <- rbindlist(list(sr_prev_no, sr_prev_yes_no_tx, sr_prev_yes_tx), use.names = TRUE)
   
  # Return the final data
  return(result)
}
