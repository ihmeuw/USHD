####################################################################################################
## Description: Functions to make it easier to get estimates from the database for USHD,
##                especially to retrieve a set of estimates (e.g., attributable burden and the
##                input PAFs)
##                
##              Functions are documented internally.
##              List of all functions in this document:
##              
##              1. `add_demographic_label`: Adds demographic labels to a given data table based on a specified demographic parameter.
##              2. `get_states_loc`: Returns a data table with state names, FIPS, abbreviated names, and location ids.
##              3. `get_ab_path_from_version_db`: Returns the path for attributable burden data based on the given version.
##              4. `load_ab`: Loads attributable burden results based on specified parameters.
##              5. `get_ab_settings`: Returns a list of settings for attributable burden based on the given version.
##              6. `get_ab_causes`: Returns a vector of cause IDs associated with an attributable burden run.
##              7. `load_paf`: Loads population attributable fraction (PAF) results based on specified parameters.
##              8. `get_all_models_from_ab`: Returns consolidated data table of upstream models with the information: run_id, run_root, and run_name.
##              ** especially useful ^
##              9. `get_sae_run_helper`: Returns SAE model run information based on provided SAE model name or SAE model id.
##              10. `get_cw_version_from_sae`: Returns Crosswalk run table based on provided SAE model name or SAE model id.
##              11. `get_direct_ests_from_sae`: Returns the direct estimates from SAE models based on provided SAE model name or SAE model id.
##              12. `get_compiled_microdata_from_cw`: Returns compiled microdata from Crosswalk version.
##              13. `get_survey_extraction_versions_from_compile_microdata`: Returns survey extraction versions from compiled microdata.
##              14. `get_bmi_exp_model_name_from_id`: Returns BMI exposure model name from model id.
##              15. `get_sae_root_path_from_id`: Returns the SAE model root path from SAE model id.
##              16. `get_sae_model_from_exp_model`: Returns the SAE model runs associated with the exposure model.
##              17. `get_paf_id_from_ab`: Returns a list containing run_id, run_root for AB, PAF compile, and PAF.
##              18. `get_fatal_model_from_ab`: Returns list with fatal model ID and run name.
##              19. `get_bmi_exp_model_ids_from_paf`: Returns a list with run_id, run_name, and run_root for exp_mean and exp_sd models.
##              20. `glimpse_burden_versions`: Returns a data table with burden versions and associated metadata not returned by existing functions.
##                  
##
####################################################################################################

source(paste0("FILEPATH"))
lbd.loader::load_package("sae.shared")
funcs <- list.files('FILEPATH')
for (func in funcs) {
  source(paste0('FILEPATH', func))
}
library(data.table)

source(paste0("FILEPATH"))


###########################################################################
# Add demographic labels --------------------------------------------------
###########################################################################

#' Add column with demographic labels
#' 
#' @param DT data.table with demographic IDs 
#' @param var string: sex_id, age_group_id, race. Specifies which demographic
#' labels to add.
#'
#' @return original DT with extra column with demographic labels
#'
#' @export
#' 
#' @examples
add_demographic_label <- function(DT, var){
  # make demographic keys
  if(var == "sex_id"){
    sex_key <- as.data.table(list(sex_id = 1:3,
                                  sex = c("male", "female", "both")))
    
  }
  if(var == "age_group_id"){
    age_key <- get_ids('age_group')
  }
  
  if(var == "race"){
    race_code <- sort(unique(DT$race))
    
    if(setequal(race_code, c(1,2,3,4,7))){ # old race codes
      race_key <- as.data.table(list(race = c(1,                   2,                3,        4,          7),
                                     race_name =   c("NH White",       "NH Black",     "NH AIAN", "NH API",  "Hispanic")))  
    } else if(setequal(race_code, c(2,4,5,6,7))){ # new race codes
      race_key <- as.data.table(list(race = race_code,
                                     race_name = c("Latino", "Black", "White", "AIAN", "Asian")))  
    } else if(setequal(race_code, c(1,2,4,5,6,7))){ # new race codes + all race
      race_key <- as.data.table(list(race = race_code,
                                     race_name = c("Total", "Latino", "Black", "White", "AIAN", "Asian")))  
    } else{
      stop("Race codes aren't recognized as a set: ", paste(race_code, collapse = ","))
    }
  }
  
  # choose appropriate key
  
  key <- switch(var,
                "age_group_id" = age_key,
                "sex_id" = sex_key,
                "race" = race_key,
                stop("var must be age_group_id, sex_id, or race. ", var, " was provided."))
  DT <- key[DT, on = var]
  return(DT)
}

#' get a list of state abbreviation and shared location IDs
#'
#' @return DT with state names, FIPS, abbrev. name, and location_id
#' @export
#'
#' @examples 
get_states_loc <- function(){
  state_abb <- fread("FILEPATH")
  state_abb <- state_abb[fips <= 56 & fips > 0]
  state_abb$location_id <- translate_area(level = "state", area = state_abb$fips)
  return(state_abb)
}

###########################################################################
# Get attributable burden  ------------------------------------------------
###########################################################################


get_ab_path_from_version_db <- function(ab_version_db = NULL){
  burden_meta <- fromJSON(txt = get_burden_risk_metadata(burden_risk_run_id = ab_version_db))[["cli-args"]]
  burden_path <- paste0(burden_meta$out_dir, burden_meta$output_version, "/")  
  return(burden_path)
}

#' load AB results
#'
#' @param ab_version AB version, corresponding to version on disk at the location of ab_root argument
#' FILEPATH/{AB_VERSION}
#' @param level  state, natl, mncty
#' @param year_ids Years to pull. If NULL, will load all years available
#' @param loc_ids # location IDs to load. If NULL, loads all for the level
#' @param type draws or ests
#' @param with_labels # attach demographic labels
#' @param use_db read from database instead of from disk -- NOT CURRENTLY IMPLEMENTED
#' @param ab_root path to AB results on disk. Default value is FILEPATH
#'  for backward compatability with code before argument added
#'
#' @return DT with AB collapsed ests
#' @export
#'
#' @examples
#'  load_ab(ab_version = 26,
#'    level = "state",
#'    year_ids = 2015,
#'    loc_ids = 523,
#'    type = "ests",
#'    with_labels = T,
#'    use_db = F,
#'    ab_root = "FILEPATH"
#'  )
load_ab <- function(ab_version,
                    level,
                    year_ids = NULL,
                    loc_ids,
                    type,
                    with_labels = T, 
                    use_db,
                    ab_root = "FILEPATH"  
)
{
  stopifnot(type %in% c("draws", "ests"))
   
  # config ------------------------------------------------------------------
  
  settings <- fromJSON(txt = paste0(ab_root, "/", ab_version, "/metadata.json"))
  settings <- settings$`cli-args`
  
  if(is.null(loc_ids)){
    locs <- get_location_metadata(location_set_id = settings$location_set_ids, location_set_version_id = 1133, release_id = 15)
    loc_type <- fcase(level == "natl", 2,
                      level == "state", 3,
                      level == "mcnty", 4)
    loc_ids <- locs[location_type_id == loc_type, location_id]
  }
  
  measures <- get_ids("measure")
  measure_name <- measures[measure_id == settings$measure_ids, measure_name]
  
  if(use_db){
    stop("Function only supports use_db == F. Make sure the ab_version corresponds to the output dir version, not the DB version")
  } else{
    get_burden_files <- function(loc, level, measure_id = 4, type, year_ids){
      if(type == "ests"){
        pattern = sprintf("%s/%s/draws/%s/upload/%s/single_year", ab_root,
                          ab_version,
                          loc,
                          measure_id)
        ff <- list.files(path = pattern,
                         pattern = "^upload_risk",
                         full.names = T,
                         recursive = T)
      } else if(type == "draws"){
        pattern = sprintf("%s/%s/draws/%s", ab_root,
                          ab_version,
                          loc)
        ff <- list.files(path = pattern,
                         pattern = ".h5$",
                         full.names = T)
      }
      if(!is.null(year_ids)){
        # filter years to just year_ids
        ff <- grep(paste(paste0(year_ids, "\\."), collapse = "|"), ff, value  =T )
        # catch bug if the grep command doesn't work as expected -- 
        #   before 10/12/23, this command did not work as expected when the location_id was between 2000-2019
        #   resulting in duplicate files loaded
        stopifnot(length(year_ids) == length(ff)) 
      }
      
      return(ff)
    }
    
    load_ab_files <- function(f, type){
      if(type == "draws"){
        DT <- setDT(readHDF(f))  
        DT <- melt(DT, measure.vars = grep("draw", names(DT), value = T), variable.name = "draw")
      } else{
        DT <- fread(f)
        summary_cols <- c("mean", "upper", "lower")
        setnames(DT, c("mean", "upper", "lower"), c("ab_mean", "ab_ub", "ab_lb"))
      }
      
      return(DT)
    }
    
    ab <- rbindlist(lapply(loc_ids, function(i) {
      ff <- get_burden_files(loc = i, level = level, measure_id = settings$measure_ids, type = type, year_ids = year_ids)
      print(ff)
      
      out <- rbindlist(lapply(ff, function(f) load_ab_files(f, type = type)))
      return(out)
    }))
    
  }
  # add age groups using USHD groups
  age_groups <- ushd.add_age_group_id(data.table(age = c(0, 1, seq(5,85, by = 5))))
  ab <- age_groups[ab, on = "age_group_id"]
  
  # remove unneeded age groups
  remove_age_groups <- setdiff(unique(ab$age_group_id), c(age_groups$age_group_id, 22, 27, 37, 38))
  if(length(remove_age_groups) > 0){
    print("Removing the following age groups from AB ests:")
    all_age_groups <- get_ids('age_group')
    print(all_age_groups[age_group_id %in% remove_age_groups])
    ab <- ab[!age_group_id %in% remove_age_groups]
  }
  
  if(with_labels){
    ab <- add_demographic_label(ab, "age_group_id")
    ab <- add_demographic_label(ab, "sex_id")
    ab <- add_demographic_label(ab, "race")
    if(level == "state"){
      state_abb <- get_states_loc()
      ab <- state_abb[, .(fips, alpha, name, location_id)][ab, on = "location_id"]
    }
  }
  
  return(ab)
  
}

#' get the AB settings file
#'
#' @param ab_version AB version, corresponding to version on disk
#' @param ab_root path to AB results on disk. Default value is FILEPATH
#'  for backward compatability with code before argument added
#' 
#' @return Named list with all of the attributable burden settings
#' @export
#'
#' @examples
get_ab_settings <- function(ab_version,
                            ab_root = "FILEPATH"){
  settings <- fromJSON(txt = paste0(ab_root, "/", ab_version, "/metadata.json"))
  settings <- settings$`cli-args`
  return(settings)
}

#' get the cause_ids associated with AB version
#'
#' @param ab_version AB version, corresponding to version on disk, e.g.,
#' FILEPATH/{AB_VERSION}
#' @param ab_root path to AB results on disk. Default value is FILEPATH
#'  for backward compatability with code before argument added
#'
#' @return vector of cause IDs assoicated with an AB run
#' @export
#'
#' @examples 
#' get_ab_causes(26)
#' get_ab_causes(26, ab_root = "/FILEPATH")
get_ab_causes <- function(ab_version, ab_root = "FILEPATH"){
  settings <- get_ab_settings(ab_version, ab_root)
  # load the metadata files associated with an arbitrary mcnty; take the first file
  ff <- list.files(sprintf("%s/draws/1001/metadata/", settings$versioned_out_dir), recursive = T, full.names = T)[1]
  meta <- fromJSON(txt = ff)
  cid <- meta$dimensions$cause_id
  return(cid)
}


###########################################################################
# Get PAFs  ------------------------------------------------
# NOTE that we rarely need to use this function. For most purposes, we want
#   to use back-calculated PAFs (from the AB results, metric_id 2) because
#   they are available for aggregates. 
###########################################################################

#' Load PAF results
#'
#' NOTE that we rarely need to use this function. For most purposes, we want
#   to use back-calculated PAFs (from the AB results, metric_id 2) because
#   they are available for aggregates
#   
#  This function is pretty slow since can only load draws.
#   
#' @param paf_compile_version paf compile version ID (cam get from AB settings).
#' Must be in the database. 
#' CAUTION: that PAF compile IDs are what are in the database
#' and they DO NOT necessarily correspond to the ID in 
#' FILEPATH
#' although those are the paf_compile results
#' 
#' @param years years to get PAFs
#' @param loc_ids location IDs for PAFs to retrieve. Note that PAFs are only computed
#' for most-detailed geographic level
#' @param type draws or ests. If ests, will collapse draws.
#' @param with_labels add demographic labels to output (e.g., race name)
#' @param allow_many_files If FALSE, funciton will only load PAFs is they come from
#' less than 100 different files. Loading more than this is very slow, but can 
#' override with by setting to TRUE.
#' @param only_yll_pafs if FALSE, will also load YLD pafs
#'
#' @return
#' @export
#'
#' @examples
load_paf <- function(paf_compile_version,
                     years,
                     loc_ids,
                     type,
                     with_labels,
                     allow_many_files = F,
                     only_yll_pafs = T
){
  
  locs <- get_location_metadata(location_set_id = 128, location_set_version_id = 1133, release_id = 15) # USHD release ID
  if(locs[location_id %in% loc_ids & location_type_id != 4, .N] != 0) stop("location ids can ONLY correspond to counties. PAFs only calculated at county level.")
  
  # get file paths
  paf_compile_files <- as.vector(sapply(years, function(yr) sapply(loc_ids, function(ll) ushd_client$paf_compile$get_paf_compile_draw_file(paf_compile_run_id = paf_compile_version,
                                                                                                                                           year_id = yr, location_id = ll, db_conf = db_conf))))
  
  if(length(paf_compile_files) > 100 & !allow_many_files) stop("Are you sure you want to load ", length(paf_compile_files), " draw files? If so, set allow_many_files to TRUE")
  
  load_paf_files <- function(f, type){
    DT <- setDT(fread(f))  
    if(only_yll_pafs){
      DT <- DT[measure_id == 4]
    }
    
    DT <- melt(DT, measure.vars = grep("draw", names(DT), value = T), variable.name = "draw")
    if(type == "ests"){
      DT <- sae.shared::collapse_draws(DT, var = "value", id_vars = setdiff(names(DT), c("draw", "value", "V1")))
    }
    
    return(DT)
  }
  
  paf <- rbindlist(lapply(paf_compile_files, function(f){
    print(f)
    
    out <- load_paf_files(f, type = type)
    return(out)
  }))
  
  # add age groups using USHD groups
  age_groups <- ushd.add_age_group_id(data.table(age = seq(20,85, by = 5)))
  paf <- age_groups[paf, on = "age_group_id"]
  
  if(with_labels){
    paf <- add_demographic_label(paf, "age_group_id")
    paf <- add_demographic_label(paf, "sex_id")
    paf <- add_demographic_label(paf, "race")
  }
  summary_names <- grep(names(paf), pattern = "value", value = T)
  setnames(paf, summary_names, gsub("value", "paf", summary_names))
  
  return(paf)
}

###########################################################################
# Get model_ids -------------------------------------------------
# These functions make it easy to get the parent model ID associated with 
#   each step of the pipeline
###########################################################################

#' Get PAF compile, PAF, and AB run results given an AB version
#'
#' The PAF compile and PAF models are parents of the AB model.
#' 
#' @param burden_path path to AB version on disk, e.g., 
#' FILEPATH
#' @param burden_risk_run_id database id for AB
#' NOTE: Provide ONE of burden_path or burden_risk_run_id, not both -- 
#'   burden_risk_run_id is preferred, but can use burden_path if the run isn't 
#'   in the database (it will give a warning)
#'   
#' @return Returns nested list with run_id, run_root for AB, PAF compile, and PAF.
#' @export
#'
#' @examples
#' get_paf_id_from_ab(burden_path = NULL,
#'     burden_risk_run_id = 5
#' )
get_paf_id_from_ab <- function(burden_path = NULL,
                               burden_risk_run_id = NULL
){
  if(!is.null(burden_path) & !is.null(burden_risk_run_id)) stop("provide ONE of burden_path or burden_risk_run_id, not both -- burden_risk_run_id is preferred, but can use burden_path if the run isn't in the database")
  
  if(!is.null(burden_risk_run_id)){
    # use DB
    burden_run <- get_burden_risk_run(burden_risk_run_id = burden_risk_run_id)
    burden_meta <- fromJSON(txt = get_burden_risk_metadata(burden_risk_run_id = burden_risk_run_id))[["cli-args"]]
    burden_path <- paste0(burden_meta$out_dir, burden_meta$output_version, "/")  
  } else{
    # load from disk
    burden_meta <- get_ab_settings(basename(burden_path), dirname(burden_path))
    
    # recreate the relevant parts of burden_run from metadata  
    burden_run <- data.table(paf_compile_run_id = burden_meta$paf_compile_run_id)
    warning("Provided path to AB results rather than database ID -- using database ID is preferred if it is available")
  }
  
  # get paf compile
  paf_compile_run_id <- burden_run$paf_compile_run_id
  paf_compile_run <- get_paf_compile_model_run(paf_compile_run_id = paf_compile_run_id)
  
  # get just one file path file paths
  # FIXME -- this is a temporary fix b/c get_paf_compile_draw_file is not working
  paf_compile_files <- ushd_client$paf_compile$get_paf_compile_draw_file(paf_compile_run_id = paf_compile_run_id,
                                                                         year_id = 2019, location_id = 1000, db_conf = db_conf)
  paf_compile_path <- dirname(paf_compile_files)
  
  # get paf
  paf_run_id <- paf_compile_run$paf_run_id  # note that this might break if we have multiple PAF run IDs
  paf_run <- get_paf_run(paf_run_id = paf_run_id)
  # no need to have the PAF path...should be reading from PAF compile
  
  results <- list(
    burden = list(
      run_id = burden_risk_run_id,
      run_root = burden_path
    ),
    paf_compile = list(
      run_id = paf_compile_run_id,
      run_root = paf_compile_path
    ),
    paf = list(run_id = paf_run_id,
               run_root = "use paf compile path to read results")
  )
  
  return(results)
}

#' Get fatal model version that is used for attributable burden run
#'
#' @param burden_path path to AB version on disk, e.g., 
#' FILEPATH
#' @param burden_risk_run_id database id for AB
#' NOTE: Provide ONE of burden_path or burden_risk_run_id, not both -- 
#'   burden_risk_run_id is preferred, but can use burden_path if the run isn't 
#'   in the database (it will give a warning)
#'
#' @return list with fatal model ID and run name
#' @export
#'
#' @examples
get_fatal_model_from_ab <- function(burden_path = NULL,
                                    burden_risk_run_id = NULL
){
  if(!is.null(burden_path) & !is.null(burden_risk_run_id)) stop("provide ONE of burden_path or burden_risk_run_id, not both -- burden_risk_run_id is preferred, but can use burden_path if the run isn't in the database")
  
  if(!is.null(burden_risk_run_id)){
    # use DB
    burden_run <- get_burden_risk_run(burden_risk_run_id = burden_risk_run_id)
  } else{
    # load from disk
    burden_meta <- get_ab_settings(basename(burden_path))
    
    # recreate the relevant parts of burden_run from metadata  
    burden_run <- data.table(fatal_model_run_id = burden_meta$fatal_run_id)
    warning("Provided path to AB results rather than database ID -- using database ID is preferred if it is available")
  }
  
  fatal_model_name <- get_fatal_model_version(model_version_id = burden_run$fatal_model_run_id)$model_version_name
  
  return(list(fatal = list(
    run_id = burden_run$fatal_model_run_id,
    run_name = fatal_model_name
  )))
}

#' Get BMI exposure model name from ID
#' A lot of the database functions require model_run_name, but we often only have
#' model_run_id. This function queries the database to return the model_run_name
#' based on ID
#'
#' @param exp_model_id 
#'
#' @return model run name, as saved in the database
#' @export
#'
#' @examples get_bmi_exp_model_name_from_id(10)
#' 
get_bmi_exp_model_name_from_id <- function(exp_model_id){
  return(get_risk_exp_model_run(model_exp_run_id = exp_model_id)$model_run_name)
}

#' get mean and exp_sd results from PAF ID
#' 
#' The exp_mean and exp_sd models are parents of the PAF model.
#'
#' @param paf_run_id PAF run ID in the database. NOTE that this is different from 
#' the PAF compile run ID.
#'
#' @return Returns a nested list with run_id, run_name, and run_root for exp_mean and
#' exp_sd models.
#' @export
#'
#' @examples get_bmi_exp_model_ids_from_paf(5)
get_bmi_exp_model_ids_from_paf <- function(paf_run_id){ 
  # get exp_sd model ID from PAF run
  paf_run <- get_paf_run(paf_run_id = paf_run_id)
  exp_sd_model_id = paf_run$model_exp_run_id 
  
  # get exp_sd run NAME from model ID
  exp_sd_model_name <- get_bmi_exp_model_name_from_id(exp_sd_model_id)
  
  exp_sd_run <- get_risk_exp_model_run(model_run_name = exp_sd_model_name)
  exp_sd_meta <- get_risk_exp_model_metadata(model_exp_metadata_id = exp_sd_run$model_exp_metadata_id)
  # exp_sd_path <- exp_sd_meta[key == "draws_sd_output_dir"]
  
  exp_sd_path <- dirname(dirname(get_risk_exp_model_draws_file(model_run_name = exp_sd_model_name, draw_type = "exp_sd")$file_path[1]))
  
  # get ensemble weight version from exp_sd model
  
  ens_wt_id <- exp_sd_run$ensemble_weight_version_id
  all_ens_wt_meta <- get_ensemble_weights_metadata(version_type = "race", get_best = F)  
  ens_wt_name <- all_ens_wt_meta[ensemble_weight_version_id == exp_sd_run$ensemble_weight_version_id, version_name]
  
  # get mean model from exp_sd model
  exp_mean_model_id = exp_sd_run$parent_run_id
  
  exp_mean_model_name <- get_bmi_exp_model_name_from_id(exp_mean_model_id)
  
  exp_mean_run <- get_risk_exp_model_run(model_run_name = exp_mean_model_name)
  exp_mean_path <- dirname(dirname(get_risk_exp_model_draws_file(model_run_name = exp_mean_model_name)$file_path[1]))
  exp_mean_rake <- gsub("exp_", "", exp_mean_run$model_output_type)
  
  
  results <- list(
    exp_sd = list(
      run_id = exp_sd_model_id,
      run_name = exp_sd_model_name,
      run_root = exp_sd_path,
      raked_type = NA # exp_sd never raked
    ),
    exp_mean = list(
      run_id = exp_mean_model_id,
      run_name = exp_mean_model_name,
      run_root = exp_mean_path,
      raked_type = exp_mean_rake
    ),
    ensemble_weight = list(
      run_id = ens_wt_id,
      run_name = ens_wt_name
    )
  )
  
  return(results)
}

#' Get SAE model root path from sae_model_id
#' 
#' we can't get the  SAE model directory from the database directly, so this function
#'  wraps DB function to get the path from sae_model_id
#'  
#' In the database, we can get the path to the limited use directory associated 
#' with model run (which contains model objects, etc.)
#' 
#'
#' @param sae_model_id 
#'
#' @return The path to the directory that contains SAE estimates
#' @export
#'
#' @examples get_sae_root_path_from_id(3)
get_sae_root_path_from_id <- function(sae_model_id = NULL){
  stopifnot(length(sae_model_id) == 1) # only designed to work for one ID at a time

  draw_filepath <- get_sae_model_run(sae_model_run_id = sae_model_id, is_best = F)$sae_model_run_filepath[1]
  
  return(draw_filepath)
}


#' Get SAE model run info from exposure model
#' 
#' get the SAE models (prevalence underweight, overweight, obese) from exposure (mean or SD) model
#' The SAE models are parents of the exp_mean and exp_sd models
#'
#' @param exp_model_name name of exposure model in DB
#' @param exp_model_id exposure model ID
#' Can provide one or both of the arguments.
#'
#' @return Returns a data.table containing information about the SAE model runs 
#' associated with exposure model, including:
#'  sae_model_run_id
#'  modelable_entity_id,
#'  sae_model_run_name,
#'  run_root, 
#'  mei_name, 
#'  ...etc. 
#' @export
#'
#' @examples  get_sae_model_from_exp_model(exp_model_id = 10)
get_sae_model_from_exp_model <- function(exp_model_name = NULL,
                                         exp_model_id = NULL){
  stopifnot(!is.null(exp_model_name) | !is.null(exp_model_id))
  
  if(is.null(exp_model_name)){
    exp_model_name = get_bmi_exp_model_name_from_id(exp_model_id)
  }
  
  exp_model_run <- get_risk_exp_model_run(model_run_name = exp_model_name)
  
  if(!is.null(exp_model_id) && exp_model_run$model_exp_run_id != exp_model_id) stop(paste(
    sprintf("Provided exp_model_name %s and exp_model_run_id %s, which do not match in database table model_exp_run:\n", exp_model_name, exp_model_id), 
    paste0(capture.output(print(exp_model_run, row.names = T)),  collapse = "\n")), collapse = "\n")
  
  sae_model_ids <- eval(parse(text = sprintf("c(%s)", exp_model_run$sae_model_run_id)))

  sae_model_runs <- rbindlist(lapply(sae_model_ids, function(sae_model_run_id) get_sae_model_run(sae_model_run_id = sae_model_run_id)))
  sae_model_runs <- sae_model_runs[, .(
    sae_model_run_id, final_risk_run_id, measure_id, modelable_entity_id, 
    parent_run_id, model_output_type, survey_crosswalk_version_id, 
    sae_model_run_name
  )]
  
  mei <- get_ids("modelable_entity")
  setnames(mei, "modelable_entity_id", "ref_modelable_entity_id")
  sae_model_runs[, mei_name := mei[ref_modelable_entity_id == modelable_entity_id, modelable_entity_name], by = 1:nrow(sae_model_runs)]
  sae_model_runs[, mei_name_short := fcase(
    modelable_entity_id == 27261, "underweight",
    modelable_entity_id == 27262, "overweight",
    modelable_entity_id == 27263, "obese"
  )]
  
  setnames(sae_model_runs, "model_output_type", "raked_type")
  # get the roots of the ESTIMATE directories, which are not the sae_model_run_filepath from DB
  sae_model_runs[, run_root := get_sae_root_path_from_id(sae_model_id = sae_model_run_id), by = 1:nrow(sae_model_runs)]
  
  return(sae_model_runs)
}

# 
#' Get crosswalk version from SAE model
#'
#' @param sae_model_name SAE model name
#' @param sae_model_id SAE model ID
#' 
#' At least one of sae_model_name and sae_model_id must be provided. If both
#' are provided, will check that they match.
#'
#' @return Crosswalk run table
#' @export
#'
#' @examples
#' 
#' get_cw_version_from_sae(sae_model_name = "2022_09_11_08_16_40model60")
#' get_cw_version_from_sae(sae_model_id = 3)
#' get_cw_version_from_sae(sae_model_name = "2022_09_11_08_16_40model60", sae_model_id = 3)
#' 
get_sae_run_helper <- function(sae_model_name = NULL,
                               sae_model_id = NULL){
  if(is.null(sae_model_name)){
    sae_run <- get_sae_model_run(sae_model_run_id = sae_model_id)
  } else{
    sae_run <- get_sae_model_run(sae_model_run_name = sae_model_name)  
    if(!is.null(sae_model_id)){
      if(sae_run$sae_model_run_id != sae_model_id){
        mm <- sprintf("sae_model_name %s and sae_model_id %s were both provided. sae_model_name %s
                is associated with sae_model_id %s. Check discrepancy!",
                      sae_model_name,
                      sae_model_id,
                      sae_model_name,
                      sae_run$sae_model_run_id)
        stop(mm)
      }
    }
  }
  return(sae_run)
}

get_cw_version_from_sae <- function(sae_model_name = NULL,
                                    sae_model_id = NULL){
  
  sae_run <- get_sae_run_helper(sae_model_name = sae_model_name,
                     sae_model_id = sae_model_id)
  
  cw_run <- ushd.dbr::get_crosswalk_version(survey_crosswalk_version_id = sae_run$survey_crosswalk_version_id)
  stopifnot(cw_run[, .N] == 1)
  return(cw_run)
}

get_direct_ests_from_sae <- function(sae_model_name = NULL,
                                     sae_model_id = NULL){
  
  sae_run <- get_sae_run_helper(sae_model_name = sae_model_name,
                                sae_model_id = sae_model_id)
  final_risk_run <- get_final_risk_run(final_risk_run_id = sae_run$final_risk_run_id)
  direct_est_ids <- eval(parse(text = sprintf("c(%s)", final_risk_run$survey_direct_estimate_collapse_microdata_run_id)))
  de_tab <- rbindlist(
    lapply(
      direct_est_ids, function(ID) get_survey_direct_estimate_collapse_microdata_run(survey_direct_estimate_collapse_microdata_run_id = ID)
    )
  )

  # add source name to de_tab 
  survey_source <- get_survey_source()
  de_tab <- merge(de_tab, survey_source, by = "survey_source_id", all.x = T)
  setnames(de_tab, "name", "survey")

  de_tab <- de_tab[, .(survey, direct_est_path = direct_estimate_file_path, used_in_sae_model = sae_run$sae_model_run_id, survey_direct_estimate_collapse_microdata_run_id)]
  if(sae_run$sae_model_run_id <= 7) {
    # sae_model_id <= 7 are assocaited with a final_risk_run (48) that was 
    # missing the Gallup direct estimtes in the database table, even though BRFSS 
    # and Gallup were both used. Just be aware  
  de_tab <- rbindlist(list(
      de_tab, 
      data.table(survey = "gallup", direct_est_path = "not in database", used_in_sae_model = sae_run$sae_model_run_id, survey_direct_estimate_collapse_microdata_run_id  = NA)))
  }
  return(de_tab)
}


get_compiled_microdata_from_cw <- function(cw_version_id = NULL){
  cw_run <- ushd.dbr::get_crosswalk_version(survey_crosswalk_version_id = cw_version_id)
  comp_microdata <- get_compile_microdata(get_best = F)
  return(comp_microdata[survey_compile_microdata_version_id == cw_run$survey_compile_microdata_version_id, .(survey_compile_microdata_version_id, output_file_path)])
}

get_survey_extraction_versions_from_compile_microdata <- function(compile_microdata_version_id = NULL){
  comp_microdata <- get_compile_microdata(get_best = F)
  # return vector of survey_extraction_version_ids
  survey_extract_ids <- gsub(" ", "", strsplit(comp_microdata[survey_compile_microdata_version_id == compile_microdata_version_id, survey_extraction_version_id], split = ",")[[1]])
  
  # get extraction info
  get_extr <- function(ID){
    unique(
      data.table(
        get_survey_extraction(survey_extraction_version_id = ID)
        )[, .(survey_extraction_version_id, input_type, output_file_path = as.character(output_file_path))]
    )
  }
  
  extr <- rbindlist(lapply(survey_extract_ids, get_extr))
  return(extr)
}


#' Get all models upstream of AB model
#' 
#' This function calls get_paf_id_from_ab(), get_bmi_exp_model_ids_from_paf(),
#' and get_sae_model_from_exp_model() in sequence in order to get all of the models
#' upstream of a set of attributable burden results
#'
#' @param burden_path path to AB version on disk, e.g., 
#' FILEPATH
#' @param burden_risk_run_id database id for AB
#' NOTE: Provide ONE of burden_path or burden_risk_run_id, not both -- 
#'   burden_risk_run_id is preferred, but can use burden_path if the run isn't 
#'   in the database (it will give a warning)
#'
#' @return Returns consolidated DT of upstream models with the information: run_id,
#' run_root, and run_name. Values are not applicable for all models and will be NULL.
#' run_id for burden will be NULL if burden_path is used instead of burden_risk_run_id
#'   burden
#'   paf_compile
#'   paf
#'   fatal
#'   exp_sd
#'   exp_mean
#'   overweight
#'   obese
#'   underweight (if used)
#' @export
#'
#' @examples 
#' get_all_models_from_ab(burden_path = "FILEPATH")
#' get_all_models_from_ab(burden_risk_run_id = 5)
get_all_models_from_ab <- function(burden_path = NULL,
                                   burden_risk_run_id = NULL
){
  # get AB and PAF info
  ab_paf_runs <- get_paf_id_from_ab(burden_path = burden_path,
                                    burden_risk_run_id = burden_risk_run_id)
  
  # Get fatal model info
  yll_runs <- get_fatal_model_from_ab(burden_path = burden_path,
                                      burden_risk_run_id = burden_risk_run_id)
  
  # Get exp_mean and exp_sd info
  bmi_exp_runs <- get_bmi_exp_model_ids_from_paf(paf_run_id = ab_paf_runs$paf$run_id)
  
  # get SAE model info
  sae_runs <- get_sae_model_from_exp_model(exp_model_name = bmi_exp_runs$exp_mean$run_name) 
  # check that you get the same SAE runs from the exp_mean and exp_sd models
  sae_runs_from_exp_sd <- get_sae_model_from_exp_model(exp_model_name = bmi_exp_runs$exp_sd$run_name) 
  stopifnot(all.equal(sae_runs, sae_runs_from_exp_sd))
  
  ## Get CW versions that went into SAE models
  # get from each SAE model version used
  # cw_runs <- lapply(sae_runs_from_exp_sd$sae_model_run_name, get_cw_version_from_sae, sae_model_id = NULL)
  cw_runs <- lapply(sae_runs_from_exp_sd$sae_model_run_id,function(id) get_cw_version_from_sae(sae_model_id = id))
  # Check that they're all the same (they should be!)
  stopifnot(length(unique(cw_runs)) == 1)
  cw_runs <- cw_runs[[1]]
  
  ## Get compiled microdata for CW
  compiled_micro <- get_compiled_microdata_from_cw(cw_version_id = cw_runs$survey_crosswalk_version_id)
  
  ## Get survey extractions
  extractions <- get_survey_extraction_versions_from_compile_microdata(compile_microdata_version_id = compiled_micro$survey_compile_microdata_version_id)
  
  ## Get direct ests from SAE models
  direct_est_list <- lapply(sae_runs$sae_model_run_id, function(x) get_direct_ests_from_sae(sae_model_id = x))
  # make sure that all models use the same direct ests
  if(rbindlist(direct_est_list)[, .(files_per_survey = uniqueN(direct_est_path)), survey][files_per_survey > 1, .N]){
    message(paste(capture.output(rbindlist(direct_est_list)), collapse = "\n"))
    stop("SAE models did not use the same direct ests")
  }
  direct_ests <- direct_est_list[[1]]
  
  
  
  # consolidate outputs
  ab_paf_runs <- rbindlist(ab_paf_runs, idcol =T, fill = T)
  yll_runs <- rbindlist(yll_runs, idcol = T, fill = T)
  bmi_exp_runs <- rbindlist(bmi_exp_runs, idcol =T, fill = T)
  sae_runs <- sae_runs[, .(.id = mei_name_short, run_id = sae_model_run_id, run_name = sae_model_run_name, raked_type, run_root = run_root)]
  cw_runs <- cw_runs[, .(.id = "crosswalk", run_id = survey_crosswalk_version_id, run_name = basename(output_file_path), run_root = output_file_path)]
  compiled_micro <- compiled_micro[, .(.id = "compiled_microdata", run_id = survey_compile_microdata_version_id, run_name = basename(output_file_path), run_root = output_file_path)]
  extractions <- extractions[, .(.id = "extraction", run_id = survey_extraction_version_id, run_name = input_type, run_root = output_file_path)]
  direct_ests <- direct_ests[, .(.id = "direct_ests", run_id = survey_direct_estimate_collapse_microdata_run_id, run_name = paste(survey, "direct_ests_no_strat", sep = "_"), run_root = direct_est_path)]
  results <- rbind(ab_paf_runs,
                   yll_runs,
                   bmi_exp_runs,
                   sae_runs,
                   direct_ests,
                   cw_runs,
                   compiled_micro,
                   extractions,
                   fill = T)
  setnames(results, ".id", "model")
  
  return(results)
}

#' Function to get information about all AB models in the database
#'
#' This function pulls information about all AB models in the database, 
#'  including the burden root, 
#'  PAF compile root,
#'  and REI ID.
#' 
#' @return A data table with information about all AB models in the database
#' @export
glimpse_burden_versions <- function(){
  burden <- get_burden_risk_run()
  # lapply through all the AB models in burden to pull the PAF info
  version_info <- rbindlist(lapply(burden$burden_risk_run_id, function(b_id){
    # get the PAF info
    paf_meta <- get_paf_id_from_ab(burden_risk_run_id = b_id)
    # get REI ID from PAF
    rei_id <- get_paf_metadata(paf_run_id = paf_meta$paf$run_id)$rei_id
    # Get any issues about the PAF
    paf_issues <- get_paf_run(paf_run_id = paf_meta$paf$run_id)$status_issues
    # paf_issues is a list of variable length. Unlist, take the unique values, and concatinate into one string.
    paf_issues <- paste(unique(unlist(paf_issues)), collapse = ", ")
    # get_paf_run(paf_run_id = paf_meta$paf$run_id)
    # get_paf_metadata(paf_run_id = paf_meta$paf$run_id)
    # get_paf_compile_model_run(paf_compile_run_id = paf_meta$paf_compile$run_id)
    as.data.table(list(burden_risk_run_id = b_id, 
                       burden_root = paf_meta$burden$run_root, 
                       paf_compile_root = paf_meta$paf_compile$run_root, 
                       rei_id = rei_id,
                       paf_issues = unlist(paf_issues)))
  }))
  # merge version_info with burden on the burden_risk_run id
  burden <- burden[version_info, on = "burden_risk_run_id"]
  return(burden)
}

