####################################################################################################
## Description: Define two functions related to model settings:
##
##              "get_settings":
##                load settings from the "settings" CSV into the R global environment and evaluate
##                to the appropriate data type.
##
##              "check_settings":
##                run checks on the "settings" CSV file. That is, check that everything that needs
##                to be specified is there and in the right format. Similarly, check that all files
##                listed in settings exist and are in the right format and that all files and
##                settings are self-consistent. Note that "check_settings" runs "get_settings" so
##                all settings are loaded into the global environment as a side-effect.
##
## Inputs:      dir [character] -- the file path to the directory where "settings.csv" is
##                located
##              validation [logical] -- is this a validation model?
####################################################################################################

require(data.table)
require(Matrix)
require(sp)


check_settings <- function(dir, validation = F) {

  # check that the main directory exists and is in the correct location
  if (!dir.exists(dir)) stop(paste(dir, "does not exist"))
  if (!grepl("^'FILEPATH'", dir)) stop(paste("'dir' must be on 'FILEPATH' (equivalently, 'FILEPATH'"))

  # check that the settings file exists
  if (!file.exists(paste0(dir, "/settings.csv"))) stop(paste0(dir, "/settings.csv does not exist"))

  # load settings
  get_settings(dir)
  
  if(!is.null(pop_file) & !is.null(pop_version)) stop("You have provided both pop_file and pop_version!")

  # in get_settings, any setting in the "optional" vector that wasn't in
  # settings.csv will be created and set to NULL
  if (!is.null(pop_file) & !is.null(pop_version)) stop("You have provided both pop_file and pop_version!")

  # check that all necessary settings are present
  all_settings <- c("model_class", "model", "area_var", "raking_area_var",
                    "years", "ages", "sexes",
                    "by_race", "races", "race_labels",
                    "by_edu", "edu_groups", "edu_labels",
                    "covars", "covars_as", "covars_trans", "n.sims",
                    "adjmat_file", "deaths_file", "pop_file", "pop_version", "covar_versions", "covar_as_versions",
                    "geoagg_files", "shape_file", "age_std_file", "ref_lt_file",
                    "raking_draws_dir", "raking_est_dir", "LU_folder")

  if (validation) all_settings <- c(all_settings, "val_sizes", "val_iter", "val_types", "val_dir", "gs_file")
  if (fixed_age_time) all_settings <- c(all_settings, "ref_dir", "ref_raked", "ref_level", "ref_area")
  miss <- all_settings[!all_settings %in% ls(envir = .GlobalEnv)]
  if (length(miss)) stop(paste("Settings missing:", paste(miss, collapse = "; ")))

  # check that all settings are the right type
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    if (class(x) == "integer") return(TRUE)
    else if (class(x) == "numeric" & max(abs(x - round(x))) < tol) return(TRUE)
    else return(FALSE)
  }

  if (class(area_var) != "character") stop("'area_var' should be type character")
  if (!is.null(raking_area_var)) if (class(raking_area_var) != "character") stop("'raking_area_var' should be type character")
  if (!is.wholenumber(years)) stop("'years' should be a whole number of type integer or numeric")
  if (!is.wholenumber(ages)) stop("'ages' should be a whole number of type integer or numeric")
  if (!is.wholenumber(sexes)) stop("'sexes' should be a whole number of type integer or numeric")
  if (class(by_race) != "logical") stop("'by_race' should be a logical")
  if (class(by_edu) != "logical") stop("'by_edu' should be a logical")
  if (!is.wholenumber(races)) stop("'races' should be a whole number of type integer or numeric")
  if (class(race_labels) != "character") stop("'race_labels' should be type character")
  if (!is.null(covars)) if (class(covars) != "character") stop("'covars' should be type character or NULL")
  if (!is.null(covars_as)) if (class(covars_as) != "character") stop("'covars_as' should be type character or NULL")
  if (!is.null(covars_trans)) if (class(covars_trans) != "character") stop("'covars_trans' should be type character or NULL")
  if (!is.wholenumber(n.sims)) stop("'n.sims' should be a whole number of type integer or numeric")
  if (class(adjmat_file) != "character") stop("'adjmat_file' should be type character")
  if (class(deaths_file) != "character") stop("'deaths_file' should be type character")

  if (!is.null(pop_file)) {
    if (class(pop_file) != "character") {
      stop("'pop_file' should be type character")
    }
  } else if (!is.null(pop_version)) {
    if (class(pop_version) != "numeric") {
      stop("'pop_version' should be type numeric")
    }

  }

  if (!is.null(covar_versions)) if (class(covar_versions) != "numeric") stop("'covar_versions' should be type numeric or NULL")
  if (!is.null(covar_as_versions)) if (class(covar_as_versions) != "numeric") stop("'covar_as_versions' should be type numeric or NULL")
  if (!is.null(geoagg_files)) if (class(geoagg_files) != "character") stop("'geoagg_files' should be type character or NULL")
  if (class(shape_file) != "character") stop("'shape_file' should be type character")
  if (class(age_std_file) != "character") stop("'age_std_file' should be type character")
  if (class(ref_lt_file) != "character") stop("'ref_lt_file' should be type character")
  if (!is.null(raking_draws_dir)) if (class(raking_draws_dir) != "character") stop("'raking_draws_dir' should by type character")
  if (!is.null(raking_est_dir)) if (class(raking_est_dir) != "character") stop("'raking_est_dir' should by type character")
  if (validation) {
    if (!is.wholenumber(val_sizes)) stop("'val_sizes' should be a whole number of type integer or numeric")
    if (!is.wholenumber(val_iter)) stop("'val_iter' should be a whole number of type integer or numeric")
    if (class(val_types) != "character") stop("'val_types' should be type character")
    if (class(val_dir) != "character") stop("'val_dir' should be type character")
    if (class(gs_file) != "character") stop("'gs_file' should be type character")
  }
  if (fixed_age_time) {
    if (class(ref_dir) != "character") stop("'ref_dir' should be type character")
    if (class(ref_raked) != "logical") stop("'ref_raked' should be type logical")
    if (class(ref_level) != "character") stop("'ref_level' should be type character")
    if (!is.wholenumber(ref_area)) stop("'ref_area' should be type integer or numeric")
  }

  # check that all settings are the right length
  if (length(model) != 1) stop("'model' should be length 1")
  if (length(area_var) != 1) stop("'area_var' should be length 1")
  if (!is.null(raking_area_var)) if (length(raking_area_var) != 1) stop("'raking_area_var' should be length 1")
  if (length(years) <= 1 & model != "one_year_re_covs") stop("'years' should be length >2")
  if (length(ages) < 1) stop("'ages' should be length >= 1")
  if (length(sexes) < 1 | length(sexes) > 2) stop("'sexes' should be length 1 or 2")
  if (length(by_race) != 1) stop("'by_race' should be length 1")
  if (length(by_edu) != 1) stop("'by_edu' should be length 1")
  if (length(races) < 1) stop("'races' should be length >= 1")
  if (length(race_labels) < 1) stop("'race_labels' should be length >= 1")
  if (!is.null(covars)) if (length(covars) < 1) stop("'covars' should be length >= 1 or NULL")
  if (!is.null(covars_as)) if (length(covars_as) < 1) stop("'covars_as' should be length >= 1 or NULL")
  if (!is.null(covars_trans)) if (length(covars_trans) < 1) stop("'covars_trans' should be length >= 1 or NULL")
  if (length(n.sims) != 1) stop("'n.sims' should be length 1")
  if (length(adjmat_file) != 1) stop("'adjmat_file' should be length 1")
  if (length(deaths_file) != 1) stop("'deaths_file' should be length 1")
  if (!is.null(pop_file)) {
    if (length(pop_file) != 1) {
      stop("'pop_file' should be length 1")
    }
  } else if (!is.null(pop_version)) {
    if (length(pop_version) != 1) {
      stop("'pop_version' should be length 1")
    }
  }

  if (!is.null(covar_versions)) if (length(covar_versions) == 0) stop("'covar_versions' should be > length 1 or NULL")
  if (!is.null(covar_as_versions)) if (length(covar_as_versions) == 0) stop("'covar_as_versions' should be > length 1 or NULL")
  if (!is.null(geoagg_files)) if (length(geoagg_files) < 1) stop("'geoagg_files' should be length >= 1 or NULL")
  if (length(shape_file) != 1) stop("'shape_file' should be length 1")
  if (length(age_std_file) != 1) stop("'age_std_file' should be length 1")
  if (length(ref_lt_file) != 1) stop("'ref_lt_file' should be length 1")
  if (!is.null(raking_draws_dir)) if (length(raking_draws_dir) != 1) stop("'raking_draws_dir' should be length 1")
  if (!is.null(raking_est_dir)) if (length(raking_est_dir) != 1) stop("'raking_est_dir' should be length 1")
  if (validation) {
    if (length(val_sizes) < 1) stop("'val_sizes' should be length >= 1")
    if (length(val_iter) != 1) stop("'val_iter' should be length 1")
    if (length(val_types) < 1) stop("'val_types' should be length >= 1")
    if (length(gs_file) != 1) stop("'val_dir' should be length 1")
    if (length(gs_file) != 1) stop("'gs_file' should be length 1")
  }
  if (fixed_age_time) {
    if (length(ref_dir) != 1) stop("'ref_dir' should be length 1")
    if (length(ref_raked) != 1) stop("'ref_raked' should be length 1")
    if (length(ref_level) != 1) stop("'ref_level' should be length 1")
    if (length(ref_area) != 1) stop("'ref_area' should be length 1")
  }
  if (length(LU_folder) != 1) stop("'LU_folder' should be length 1")

  # check that n.sims > 0 but not too big (which will cause memory problems)
  if (n.sims <= 0) stop("'n.sims' must be >= 1")
  if (n.sims > 1e4) stop("setting 'n.sims' too high which may cause memory problems")

  # check that all necessary model code exists for the selected model
  all_mod_files <- c(paste0("mod_", model, ".cpp"), paste0("fit_mod_", model, ".r"))
  miss <- setdiff(all_mod_files, dir(model_class))
  if (length(miss)) stop(paste("Model code missing:", paste(miss, collapse = "; ")))

  # check that "by_race", "races", and "race_labels" are self-consistent
  if (!by_race & length(races) > 1) stop("'by_race' is FALSE, but multiple races are specified")
  if (!by_race & races[1] != all_pop_id) stop(paste0("'by_race' is FALSE, but races does not equal ",all_pop_id))
  if (by_race & length(races) < 2) stop("'by_race' is TRUE, but only one race is specified")
  if (length(races) != length(race_labels)) stop("'races' and 'race_labels' should have same length")

  # check that "by_edu", "edu_groups, and "edu_labels" are self-consistent
  if (!by_edu & length(edu_groups) > 1) stop("'by_edu' is FALSE, but multiple edu_groups are specified")
  if (!by_edu & edu_groups[1] != all_pop_id) stop(paste0("'by_edu' is FALSE, but edu_groups does not equal ", all_pop_id))
  if (by_edu & length(edu_groups) < 2) stop("'by_edu' is TRUE, but only one education is specified")
  if (length(edu_groups) != length(edu_labels)) stop("'edu_groups' and 'edu_labels' should have same length")

    # check that all input files exist
  all_files <- list("adjmat_file" = adjmat_file, "deaths_file" = deaths_file,
                    "shape_file" = shape_file, "age_std_file" = age_std_file, "ref_lt_file" = ref_lt_file)
  if (!is.null(geoagg_files)) all_files[["geoagg_files"]] <- geoagg_files
  if (!is.null(subset_hisp_mr_path)) all_files[["subset_hisp_mr_path"]] <- subset_hisp_mr_path
  if (validation) all_files[["gs_file"]] <- gs_file
  if(!is.null(pop_file)) all_files[["pop_file"]] <- pop_file
  miss <- sapply(all_files, function(x) sum(!file.exists(x)))
  miss <- miss[miss > 0]
  if (length(miss)) stop(paste("Input files missing/nonexistent:", paste(names(miss), collapse = "; ")))

  # check that all input files are the correct file type
  rds_files <- c("deaths_file", "adjmat_file", "geoagg_files", "shape_file",
                 "age_std_file")
  if(!is.null(pop_file)) rds_files <- c(rds_files,"pop_file")
  csv_files <- c("ref_lt_file")

  for (ff in names(all_files)) {
    if (ff %in% rds_files) type <- "rds"
    if (ff %in% csv_files) type <- "csv"

    if (sum(!grepl(paste0("\\.", type, "$"), tolower(all_files[[ff]])))) {
      stop(paste("'", ff, "' should be type:", type))
    }
  }

  # check that the validation directories exist and that they have the appropriate files in them
  if (validation) {
    if (!file.exists(gsub("/$", "", val_dir))) stop("'val_dir' does not exist")
    for (type in val_types) {
      if (!file.exists(paste0(val_dir, "/", type))) stop("'val_types' does not exist in 'val_dir'")
      for (size in format(val_sizes, scientific = F, trim = T)) {
        for (it in 1:val_iter) {
          if (!file.exists(paste0(val_dir, "/", type, "/deaths_", size, "_", it, ".rds"))) stop("deaths files mising from 'val_dir'/'val_types'")
          if (!file.exists(paste0(val_dir, "/", type, "/pop_", size, "_", it, ".rds"))) stop("pop files mising from 'val_dir'/'val_types'")
        }
      }
    }
  }

  # check that the reference directories exist and that they have the appropriate files in them
  if (fixed_age_time) {
    if (!file.exists(ref_dir)) stop("'ref_dir' does not exist")
    ref_est_file <- paste0("mx_est_all", if (ref_raked) "_raked", ".rds")
    if (!file.exists(paste0(ref_dir, "/", ref_est_file))) stop(paste0("'", ref_est_file, "' does not exist in 'ref_dir'"))

    ref_draw_files <- CJ(year = years, sex = sexes)[, paste0("mx_draws_", ref_level, "_", year, "_", sex, if (ref_raked) "_raked", ".rds")]
    if (sum(!file.exists(paste0(ref_dir, "/", ref_draw_files)))) stop(paste0("one or more draw files missing from 'ref_dir' (expecting: ", paste(ref_draw_files, collapse = ","), ")"))
  }

  # check that the adjacency matrix is correctly formatted
  adjmat <- readRDS(adjmat_file)
  if (!class(adjmat)[1] == "dgTMatrix") stop("object in 'adjmat_file' should be a dgTMatrix")
  if (!isSymmetric(adjmat)) stop("adjacency matrix in 'adjmat_file' should be symmetric")
  num_j <- nrow(adjmat)
  rm(adjmat)

  # check that deaths file is formatted correctly and consistent with the settings for area_var, sexes, and races and with the adjacency matrix
  deaths <- readRDS(deaths_file)
  if (!class(deaths)[1] == "data.table") stop("object in 'deaths_file' should be a data.table")
  all_vars <- c(area_var, "year", "sex", "age", if (by_race) "race", if (by_edu) "edu", "deaths")
  miss <- setdiff(all_vars, names(deaths))
  if (length(miss)) stop(paste("Variables missing from 'deaths_file':", paste(miss, collapse = "; ")))
  if (length(unique(deaths[[area_var]])) > num_j) stop("there are more counties in 'deaths_file' than in 'adjmat_file'")
  if (sum(!unique(deaths[[area_var]]) %in% 0:(num_j - 1))) stop("area variable is not coded correctly in 'deaths_file' (i.e., sequentially from 0)")
  if (sum(!sexes %in% deaths[,unique(sexes)])) stop("sexes missing from 'deaths_file'")
  if (by_race) if (sum(!races %in% deaths[, unique(race)])) stop("races missing from 'deaths_file'")
  if (by_edu) if (sum(!edu_groups %in% deaths[, unique(edu)])) stop("education groups (column: edu) missing from 'deaths_file'")
  if (sum(is.na(deaths[, all_vars, with = F]))) stop("NAs in 'deaths_file'")
  rm(deaths); gc()

  # check that population file is formatted correctly and consistent with the settings for area_var, years, sexes, ages, and races and with the adjacency matrix
  if(!is.null(pop_file)) {
    pop <- readRDS(pop_file)
    if (!class(pop)[1] == "data.table") stop("object in 'pop_file' should be a data.table")
    all_vars <- c(area_var, "year", "sex", "age", if (by_race) "race", if (by_edu) "edu", "pop")
    miss <- setdiff(all_vars, names(pop))
    if (length(miss)) stop(paste("Variables missing from 'pop_file':", paste(miss, collapse = "; ")))
    if (length(unique(pop[[area_var]])) != num_j) stop("there are a different number of counties in 'pop_file' than in 'adjmat_file'")
    if (sum(!unique(pop[[area_var]]) %in% 0:(num_j - 1))) stop("area variable is not coded correctly in 'pop_file' (i.e., sequentially from 0)")
    if (sum(!years %in% pop$year)) stop("not all years in 'years' are present in 'pop_file'")
    if (sum(!sexes %in% pop[,unique(sexes)])) stop("sexes missing from 'pop_file'")
    if (sum(!ages %in% pop[,unique(ages)])) stop("ages missing from 'pop_file'")
    if (by_race) if (sum(!races %in% pop[, unique(race)])) stop("races missing from 'pop_file'")
    if (by_edu) if (sum(!edu_groups %in% pop[, unique(edu)])) stop("education groups (column: edu) missing from 'pop_file'")
    if (sum(is.na(pop[, all_vars, with = F]))) stop("NAs in 'pop_file'")
    rm(pop); gc()
  }


  # check that covariates file is formatted correctly and consistent with the settings for area_var, years, covars, and with the adjacency matrix
  if (!is.null(covars) & is.null(covar_versions)) stop("covariates specified but there is not a covariates version list specified")
  if (!is.null(covars)) {

    for(cc in covars) {

      message(paste0("Validating mcnty covariate ",cc, " from the DB"))
      covar <- get_covariate_data(covariate_dataset_id = as.numeric(covar_versions[cc]))

      if (!class(covar)[1] == "data.table") stop(paste0("object in DB for ",cc," should be a data.table"))
      all_vars <- c(area_var, "year", cc)
      if (sum(!years %in% covar$year)) stop(paste0("not all years in 'years' are present in ",cc," in DB"))
      if (length(unique(covar[[area_var]])) != num_j) stop(paste0("there are a different number of counties in ",cc," from the DB than in 'adjmat_file'"))
      if (sum(!unique(covar[[area_var]]) %in% 0:(num_j - 1))) stop(paste0("area variable is not coded correctly in ",cc,
                                                                          " from DB (i.e., sequentially from 0)"))
      if (sum(is.na(covar[year %in% years, all_vars, with = F]))) stop(paste0("NAs in ",cc," from DB for specified years"))

    }

    rm(covar); gc()
  }

  # check that age-sex covariates file is formatted correctly and consistent with the settings for area_var, years, covars_as, and with the adjacency matrix
  if (!is.null(covars_as) & is.null(covar_as_versions)) stop("age-sex covariates specified but there is not a age-sex covariate version list specified")
  if (!is.null(covars_as)) {

    for(cc in covars_as) {

      message(paste0("Validating age-sex covariate ",cc, " from the DB"))
      covar <- get_covariate_data(covariate_dataset_id = as.numeric(covar_as_versions[cc]))

      if (!class(covar)[1] == "data.table") stop(paste0("object in DB for age-sex ",cc," should be a data.table"))
      all_vars <- c(area_var, "year", "sex", "age", cc)
      if (sum(!years %in% covar$year)) stop(paste0("not all years in 'years' are present in age-sex ",cc," in DB"))
      if (sum(!sexes %in% covar$sex)) stop(paste0("not all sexes in 'sexes' are present in age-sex ",cc," in DB"))
      if (sum(!ages %in% covar$age)) stop(paste0("not all ages in 'ages' are present in age-sex ",cc," in DB"))
      if (length(unique(covar[[area_var]])) != num_j) stop(paste0("there are a different number of counties in age-sex ",cc," from the DB than in 'adjmat_file'"))
      if (sum(!unique(covar[[area_var]]) %in% 0:(num_j - 1))) stop(paste0("area variable is not coded correctly in age-sex ",cc,
                                                                          " from DB (i.e., sequentially from 0)"))
      if (sum(is.na(covar[year %in% years, all_vars, with = F]))) stop(paste0("NAs in ",cc," from DB for specified years"))

    }

    rm(covar); gc()
  }

  if (!is.null(covars_subpop) & is.null(covar_subpop_versions)) stop("race-ethnicity or education covariates specified but there is not a race-ethnicity/education covariates versions list specified")
  if (!is.null(covars_subpop) & is.null(covar_subpop_hyperpriors_settings)) stop("race-ethnicity or education covariates specified but there is not a race-ethnicity/education covariates file specified")
  if (!is.null(covars_offset) & is.null(covar_offset_file)) stop("offset covariates specified but there is not an offset covariates file specified")

  # tests and prep for subpop specific covariates
  if (!is.null(covars_subpop)) {

    for (cc in covars_subpop) {

      message(paste0("Validating subpop covariate ",cc, " from the DB"))
      covar <- get_covariate_data(covariate_dataset_id = as.numeric(covar_subpop_versions[cc]))

      if (!class(covar)[1] == "data.table") stop(paste0("object in DB for subpop ",cc," should be a data.table"))
      all_vars <- c(area_var, "year", ifelse(by_race, "race", "edu"), cc)
      if (sum(!years %in% covar$year)) stop(paste0("not all years in 'years' are present in subpop ",cc," in DB"))
      if (sum(!races %in% covar$race)) stop(paste0("not all race/ethnicity groups in 'races' are present in subpop ",cc," in DB"))
      if (length(unique(covar[[area_var]])) != num_j) stop(paste0("there are a different number of counties in subpop ",cc," from the DB than in 'adjmat_file'"))
      if (sum(!unique(covar[[area_var]]) %in% 0:(num_j - 1))) stop(paste0("area variable is not coded correctly in subpop ",cc,
                                                                          " from DB (i.e., sequentially from 0)"))
      if (sum(is.na(covar[year %in% years, all_vars, with = F]))) stop(paste0("NAs in ",cc," from DB for specified years"))
      rm(covar); gc()
    }
  }

  # tests and prep for offset covariate
  if(!is.null(covars_offset)) {
    covar <- readRDS(covar_offset_file)
    if (!class(covar)[1] == "data.table") stop("object in 'covar_offset_file' should be a data.table")
    all_vars <- c(area_var, "year", covars_offset)
    miss <- setdiff(all_vars, names(covar))
    if (length(miss)) stop(paste("Variables missing from 'covar_offset_file':", paste(miss, collapse = "; ")))
    if (sum(!years %in% covar$year)) stop("not all years in 'years' are present in 'covar_offset_file'")
    if (length(unique(covar[[area_var]])) != num_j) stop("there are a different number of counties in 'covar_offset_file' than in 'adjmat_file'")
    if (sum(!unique(covar[[area_var]]) %in% 0:(num_j - 1))) stop("area variable is not coded correctly in 'covar_offset_file' (i.e., sequentially from 0)")
    if (sum(is.na(covar[year %in% years, all_vars, with = F]))) stop("NAs in 'covar_offset_file' for specified years")
    rm(covar); gc()
  }


  # check that if both covars, and any of covars_subpop, covars_as, and covars_offset are specified such that there is no overlap
  if (length(intersect(covars, covars_as))) stop("'covars' and 'covars_as' cannot contain any of the same items")
  if (length(intersect(covars, covars_subpop))) stop("'covars' and 'covars_subpop' cannot contain any of the same items")
  if (length(intersect(covars_subpop, covars_as))) stop("'covars_subpop' and 'covars_as' cannot contain any of the same items")
  if (length(intersect(covars, covars_offset))) stop("'covars' and 'covars_offset' cannot contain any of the same items")
  if (length(intersect(covars_subpop, covars_offset))) stop("'covars_subpop' and 'covars_offset' cannot contain any of the same items")

  # check that covars_trans is appropriately specified and consistent with covars, covars_as, covar_file, and covar_as_file
  if (!is.null(covars_trans)) {
    if (is.null(names(covars_trans))) stop("'covars_trans' must be a named character vector")
    if (sum(!names(covars_trans) %in% c(covars, covars_as, covars_subpop))) stop("the name of each item in 'covars_trans' must also be in 'covars','covars_as', or 'covars_subpop'")

    covar <- list()
    cov_merge_vars <- c("year","edu","sex","race","race_set","mcnty","state","age","race_label","edu_label")

    # mcnty-level covariates
    if(!is.null(covars)) {

      cov_all <- data.table()
      for(cc in covars) {
        message(paste0("Reading in mcnty covariates ",cc, " from the DB"))
        cov_tmp <- get_covariate_data(covariate_dataset_id = as.numeric(covar_versions[cc]))

        if(nrow(cov_all) == 0) {
          cov_all <- copy(cov_tmp)
        } else {
          cov_all <- merge(cov_all, cov_tmp, by = cov_merge_vars)
        }

        if(nrow(cov_all) == 0) stop("Covar has zero rows")

      }

      covar[[1]] <- cov_all[,(c("mcnty","year",covars)), with=F]
    }

    # age-sex level covariates
    if(!is.null(covars_as)) {

      cov_all <- data.table()
      for(cc in covars_as) {
        message(paste0("Reading in age-sex covariate ",cc, " from the DB"))
        cov_tmp <- get_covariate_data(covariate_dataset_id = as.numeric(covar_as_versions[cc]))

        if(nrow(cov_all) == 0) {
          cov_all <- copy(cov_tmp)
        } else {
          cov_all <- merge(cov_all, cov_tmp, by = cov_merge_vars)
        }

        if(nrow(cov_all) == 0) stop("Covar has zero rows")

      }

      covar[[length(covar) + 1]] <- cov_all[,(c("mcnty","year","age","sex",covars_as)), with=F]
    }

    # subpop level covariates
    if(!is.null(covars_subpop)) {

      cov_all <- data.table()
      for(cc in covars_subpop) {
        message(paste0("Reading in subpop covariate ",cc, " from the DB"))
        cov_tmp <- get_covariate_data(covariate_dataset_id = as.numeric(covar_subpop_versions[cc]))

        if(nrow(cov_all) == 0) {
          cov_all <- copy(cov_tmp)
        } else {
          cov_all <- merge(cov_all, cov_tmp, by = cov_merge_vars)
        }

        if(nrow(cov_all) == 0) stop("Covar has zero rows")

      }

      covar[[length(covar) + 1]] <- cov_all[,(c("mcnty","year","race","edu",covars_subpop)), with=F]
    }

    if(!is.null(covars_offset)) {
      covar[[length(covar) + 1]] <- readRDS(covar_offset_file)[,(c("mcnty","year",covars_offset)), with=F]
    }


    if(length(covar) > 2) {
      covar <- merge(covar[[1]], merge(covar[[2]], covar[[3]], by = c(area_var, "year")),
                     by = c(area_var, "year"))
    } else if (length(covar) > 1) {
      covar <- merge(covar[[1]], covar[[2]], by = c(area_var, "year"))
    } else covar <- covar[[1]]



    for (var in names(covars_trans)) {
      fun <- try(eval(parse(text = paste("function (x)", covars_trans[var]))), silent = T)
      if (class(fun) == "try-error") stop("each item in 'covars_trans' should be a string specifying a function of x")
      temp <- try(fun(covar[[var]]))
      if (class(temp) == "try-error") stop("each item in 'covars_trans' should be a string specifying a function of x")
      if (sum(is.na(temp))) stop("'covars_trans' settings introduce NAs in 'covar_file' or 'covar_as_file'")
    }
    rm(covar); gc()
  }

  # If the Horiuchi-Coale life table method is specified, make sure that there is a population growth file
  if(lt_hc) {

    if("pop_growth_file" %in% ls(envir = .GlobalEnv)) {

      if(!file.exists(pop_growth_file)) stop("lt_hc is TRUE, but the pop_growth_file you specified does not exist")

      pop_growth <- readRDS(pop_growth_file)
      if(nrow(pop_growth) == 0) stop("The pop_growth_file has no rows")
      expected_cols <- c("mcnty", "year", "sex", "age", "race", "edu", "pop", "state")
      if(!all(expected_cols %in% names(pop_growth))) stop(paste0("One of: ", paste(expected_cols,collapse=", "), " - is not in the pop_growth_file"))
      if (by_race) {
        if (!all(c(years, c(min(years):(min(years)-10))) %in% pop_growth$year)){
          stop("In pop_growth file: Missing the specified years and the 10 previous years before the minimum year that are necessary for calculating population growth")
        }
      }

      if(by_race & !all(races %in% pop_growth$race)) stop("The model is by_race but pop_growth_file has the incorrect race values")
      if(by_edu & !all(edu_groups %in% pop_growth$edu)) stop("The model is by_edu but pop_growth_file has the incorrect edu values")
      if(!max(ages) %in% pop_growth$age) stop("The terminal age group is missing from the pop_growth_file")
      rm(pop_growth); gc()

    } else {
      message("pop_growth_file is not specified; pop_version will be used")
    }

  }

  # check that the geoagg files are formatted correctly, consistent with the settings for area_var, years, sexes, ages, and races
  if (!is.null(geoagg_files)) {
    if (is.null(names(geoagg_files))) stop("geoagg_files must be a named vector")
    for (level in names(geoagg_files)) {
      weights <- readRDS(geoagg_files[level])
      if (!class(weights)[1] == "data.table") stop("object in 'geoagg_files' should be a data.table")
      miss <- setdiff(c(area_var, level, "wt"), names(weights))
      if (length(miss)) stop(paste("'geoagg_files' is missing variables:", paste(miss, collapse = ", ")))
      if (sum(is.na(weights))) stop("NAs in 'geoagg_files'")
      if (sum(!unique(weights[[area_var]]) %in% 0:(num_j - 1))) stop("area variable is not coded correctly in 'geoagg_files' (i.e., sequentially from 0)")
      if ("year" %in% names(weights)) {
        if (sum(!years %in% weights$year)) stop("years missing from 'geoagg_files'")
      }
      if ("sex" %in% names(weights)) {
        if (sum(!sexes %in% weights$sex)) stop("sexes missing from 'geoagg_files'")
      }
      if ("age" %in% names(weights)) {
        if (sum(!ages %in% weights$age)) stop("ages missing from 'geoagg_files'")
      }
      if ("race" %in% names(weights)) {
        if (sum(!races %in% weights$race)) stop("races missing from 'geoagg_files'")
      }
      all_vars <- intersect(c(area_var, "year", "sex", "age", "race"), names(weights))
      weights <- weights[, list(wt = sum(wt)), by = all_vars]
      if (weights[, max(abs(wt - 1)) > 1e-10]) stop(paste("weights should sum to 1 within", paste(vars, collapse = "-"), "in 'geoagg_files'"))
      rm(weights)
    }
  }

  # check that the shapefiles are formatted correctly and consistent with the adjacency matrix
  area_map <- readRDS(shape_file)
  if (class(area_map)[1] != "SpatialPolygonsDataFrame") stop("Object in 'shape_file' should be a SpatialPolygonsDataFrame")
  if (length(area_map) != num_j) stop("Object in in 'shape_file' does not have the same number of areas as the object in 'adjmat_file'")
  if (sum(sapply(area_map@polygons, function(x) x@ID) != 0:(num_j - 1))) stop("polygon IDs in 'shape_file' do not have the correct area variable coding (i.e., sequentially from 0) or are not sorted properly")

  # check that the age standard file is correctly formatted and consistent with the "ages" setting
  age_std <- readRDS(age_std_file)
  all_vars <- c("age", "wt")
  miss <- setdiff(all_vars, names(age_std))
  if (length(miss)) stop(paste("Variables missing from 'age_std_file':", paste(miss, collapse = "; ")))
  if (length(setdiff(ages, age_std$age))) stop("ages missing from 'age_std_file'")
  if (sum(is.na(age_std[, all_vars, with = F]))) stop("NAs in 'age_std_file'")
  rm(age_std)

  # check that the format of the race subset file is correct
  if(!is.null(subset_hisp_mr_path) & !is.null(subset_hisp_mr_path)){
    sub_race_dt <- readRDS(subset_hisp_mr_path)
    all_vars <- c("year","has_hispanic","has_multiracial","area","state")

  }

  # check that the reference life table file is correctly formatted and consistent with the "ages" setting
  ref_lt <- fread(ref_lt_file)
  all_vars <- c("death_age", "ex")
  miss <- setdiff(all_vars, names(ref_lt))
  if (length(miss)) stop(paste("Variables missing from 'ref_lt_file':", paste(miss, collapse = "; ")))
  if (sum(is.na(ref_lt[, all_vars, with = F]))) stop("NAs in 'ref_lt_file'")
  rm(ref_lt); gc()

  # check that the gold standard file is formatted correctly
  if (validation) {
    gs_mx <- readRDS(gs_file)
    gs_ex <- readRDS(file.path(dirname(gs_file), "validation_set_gs_ex.rds"))
    if (!class(gs_mx)[1] == "data.table" | !class(gs_ex)[1] == "data.table") stop("objects in 'gs_file' should be a data.table")
    all_vars <- c(area_var, "year", "sex", "age", "mx_mean", "mx_lb", "mx_ub")
    miss <- setdiff(all_vars, names(gs_mx))
    if (length(miss)) stop(paste("Variables missing from gs_mx in 'gs_file':", paste(miss, collapse = "; ")))
    if (sum(!unique(gs_mx[[area_var]]) %in% 0:(num_j - 1))) stop("area variable is not coded correctly in gs_mx in 'gs_file' (i.e., sequentially from 0)")
    if (sum(is.na(gs_mx[, all_vars, with = F]))) stop("NAs in gs_mx in 'gs_file'")
    all_vars <- c(area_var, "year", "sex", "age", "ex_mean", "ex_lb", "ex_ub")
    miss <- setdiff(all_vars, names(gs_ex))
    if (length(miss)) stop(paste("Variables missing from gs_ex in 'gs_file':", paste(miss, collapse = "; ")))
    if (sum(!unique(gs_ex[[area_var]]) %in% 0:(num_j - 1))) stop("area variable is not coded correctly in gs_ex in 'gs_file' (i.e., sequentially from 0)")
    if (sum(is.na(gs_ex[, all_vars, with = F]))) stop("NAs in gs_ex in 'gs_file'")
    rm(gs_mx, gs_ex); gc()
  }

  # check that the reference estimates file is properly formatted
  if (fixed_age_time) {
    est <- readRDS(paste0(ref_dir, "/", ref_est_file))
    if (!class(est)[1] == "data.table") stop("object in 'ref_est_file' should be a data.table")
    all_vars <- c("level", "area", "year", "sex", "age", if (by_race) "race", "mx_mean")
    miss <- setdiff(all_vars, names(est))
    if (length(miss)) stop(paste("Variables missing from 'ref_est_file':", paste(miss, collapse = "; ")))
    if (est[level == ref_level & area == ref_area, .N] == 0) stop("'ref_level' and 'ref_area' combination do not exist in 'ref_est_file'")
    if (length(setdiff(years, est$year))) stop("'ref_est_file' does not include all years in 'years'")
    if (length(setdiff(sexes, est$sex))) stop("'ref_est_file' does not include all sexes in 'sexes'")
    if (length(setdiff(ages, est[mx_mean > 0, unique(age)]))) stop("'ref_est_file' does not include all ages in 'ages'")
    if (by_race) if (length(setdiff(races, est$race))) stop("'ref_est_file' does not include all races in 'races'")
    rm(est); gc()
  }

  # check that the prior information is present and correct if prior type is specified
  if (!is.null(prior_type)) {
    if (!(prior_type) %in% c("pc","loggamma", "half_normal")) stop("Prior_type is not pc, loggamma, or half_normal.")
    if (is.null(prior_list)) stop("Prior type is specified but there are no prior values.")
    if(!is.list(prior_list)) stop("The prior arguments are not in a list format.")
    if(length(unique(lengths(prior_list))) > 1) stop("The list of prior arguments have different specifications.")
    if(unique(lengths(prior_list)) != 3) stop("The list of prior arguments does not have 3 parameters per random effect.")
    if(prior_type == "half_normal" & length(unique(unlist(lapply(prior_list, `[[`, 1)))) != 1) stop("Half normal specified but there are variable values for the mean of the distribution")
    if(prior_type == "half_normal" & unique(unlist(lapply(prior_list, `[[`, 1))) != 0) stop("Half normal specified but mean of the distribution is not 0")

  }

  # covar_subpop_hyperpriors_settings = list(re_cov_par1 = 5, re_cov_par2 = 0.05, re_cov_log_sigma = -5)
  if (!is.null(covars_subpop)) {
    if (!is.list(covar_subpop_hyperpriors_settings)) stop("covar_subpop_hyperpriors_settings is not in a list format.")
    if (length(covar_subpop_hyperpriors_settings) != 3) stop("covar_subpop_hyperpriors_settings does not have 3 parameters per random effect.")
    if (!("re_cov_par1" %in% names(covar_subpop_hyperpriors_settings))) stop("covar_subpop_hyperpriors_settings must have an entry named 're_cov_par1'")
    if (!("re_cov_par2" %in% names(covar_subpop_hyperpriors_settings))) stop("covar_subpop_hyperpriors_settings must have an entry named 're_cov_par2'")
    if (!("re_cov_log_sigma" %in% names(covar_subpop_hyperpriors_settings))) stop("covar_subpop_hyperpriors_settings must have an entry named 're_cov_log_sigma'")
  }

  return("checks on settings passed")
}
