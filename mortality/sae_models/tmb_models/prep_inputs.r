####################################################################################################
## Description: Prep inputs for small area models for area-year-sex-race-age all-cause or cause-
##              specific mortality rates. Specifically, collapse and merge deaths and population
##              data and then combine with covariates data. Recode age groups and years to run
##              sequentially from zero. Also, prep CAR structure matrices for spatial, temporal,
##              and age effects.
##
## Passed args: dir [character] -- home directory for settings and final output
##
## Requires:    deaths data (deaths_file)
##              population data (pop_file)
##              covariates data, if used (covars, covars_as, covars_trans, covar_file, covar_as_file)
##              neighborhood adjacency matrix (adjmat_file)
##
## Outputs:     prepped data file ("[limited_use_dir]/data.rds")
##              graphs for age, time, and year random effects ("[dir]/re_graphs.rdata")
##
####################################################################################################

library(R.utils)
library(data.table)
library(Matrix)

sourceDirectory("functions/", modifiedOnly = FALSE)

## Get settings ------------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]

get_settings(dir)

if (!(dir %like% "FILEPATH")) stop("Model is not being run from the expected directory: FILEPATH")
current_dir <- gsub("FILEPATH", "", dir)
lu_root <- paste0("FILEPATH",LU_folder,"FILEPATH")
lu_modeldir <- paste0(lu_root,"/",current_dir)
dir.create(lu_modeldir, recursive = T)

if (!exists("race_together") | is.null(race_together)) race_together <- F
if (!exists("edu_together") | is.null(edu_together)) edu_together <- F

loc <- fread("FILEPATH")

## Load and prep inputs ----------------------------------------------------------------------------
# combine deaths and population
deaths <- readRDS(deaths_file)

message("Done with deaths")

# subset to cause specified in settings
subset_cause <- F
if("cause_id" %in% names(deaths)) {
  message("Subsetting")
  subset_cause <- T
  deaths <- deaths[cause_id == get("cause_id", .GlobalEnv), ]
}

if (!nrow(deaths) > 0) {
  stop(paste0("There are no rows in the deaths data.table. cause_id is ", cause_id, "."))
}

setnames(deaths, area_var, "area")

if ("sex_id" %in% names(deaths)) {
  setnames(deaths, "sex_id", "sex")
}

if (!by_race) deaths[, race := all_pop_id]
if (!by_edu) deaths[, edu := all_pop_id]
deaths <- deaths[year %in% years & age %in% ages, list(deaths = sum(deaths)), by = "area,year,sex,race,edu,age"]

stopifnot(nrow(deaths) > 0)

message("Read in pop")
if (tolower(tools::file_ext(pop_file)) == "rds") {
  pop <- readRDS(pop_file)
} else if (tolower(tools::file_ext(pop_file)) == "csv") {
  pop <- fread(pop_file)
} else {
  stop(glue::glue("Unhandled file extension: {tolower(tools::file_ext(pop_file))}"))
}

if (!("state" %in% names(pop))) {
  pop <- merge(pop, unique(loc[, .(mcnty, state)]), by = "mcnty", all.x = T)
}

if ("births" %in% names(pop) & ("pop" %in% names(pop))) {
  stop("both 'births' and 'pop' are columns in pop data.table")
}
if ("births" %in% names(pop)) {
  setnames(pop, "births", "pop")
}

setnames(pop, area_var, "area")
if (!by_race) pop[, race := all_pop_id]
if (!by_edu) pop[, edu := all_pop_id]
pop <- pop[year %in% years & age %in% ages, list(pop = sum(pop)), by = "area,year,sex,race,edu,age,state"]

stopifnot(nrow(pop) > 0)

message("Merging pop and deaths together")
data <- merge(pop, deaths, by = c("area", "year", "sex", "race", "edu", "age"), all = T)
if (all(is.na(data[, deaths]))) stop("All rows of deaths are NA") # don't want every row to be null before proceeding with a bunch of zeros

## Prep growth rates for using the Horiuchi-Coale method
if (lt_hc) {
  calc_growth_rate(dir, test_by_race = by_race, test_by_edu = by_edu, n_years = 10)

}

# Define variables used in fitting the models
if(race_together) {
  data[, race_fit := 99]
} else {
  data[, race_fit := race]
}

if(edu_together) {
  data[,edu_fit := 99]
} else {
  data[,edu_fit := edu]
}

if(sex_together) {
  data[, sex_fit := 99]
} else {
  data[, sex_fit := sex]
}

rm(pop, deaths); gc()


# prepare misclassification ratios draws
if (!is.null(misclassification_correction)) {
  if (misclassification_correction) {

    expanded <- prep_misclass( # just copies the misclassification draws into the directory
      n.sims,
      dir = dir,
      filepath = "FILEPATH"
    )
  }
}

# load covariates
cov_merge_vars <- c("year","edu","sex","race","race_set","mcnty","state","age","race_label","edu_label")

if (!is.null(covars) | !is.null(covars_as) | !is.null(covars_subpop) | !is.null(covars_offset)) {

  if(!is.null(covars_subpop)) {

    covar <- data.table()
    for(cc in covars_subpop) {
      message(paste0("Reading in subpop covariate ",cc, " from the DB"))
      cov_tmp <- get_covariate_data(covariate_dataset_id = as.numeric(covar_subpop_versions[cc]))

      if(nrow(covar) == 0) {
        covar <- copy(cov_tmp)
      } else {
        covar <- merge(covar, cov_tmp, by = cov_merge_vars)
      }

      if(nrow(covar) == 0) stop("Covar has zero rows")

    }

    if(unique(covar$age) == 98) covar[,age := NULL]

    if("age" %in% names(covar)) {
      covar <- covar[year %in% years, c(area_var, "year", "age", "race", "edu", covars_subpop), with = F]
    } else {
      covar <- covar[year %in% years, c(area_var, "year", "race", "edu", covars_subpop), with = F]
    }

    assign("covar_subpop", covar)

  } else {
    assign("covar_subpop", data.table(expand.grid(area_var = unique(data$area), year = years, race = races, edu = edu_groups)))
    setnames(covar_subpop, "area_var", area_var)
  }


  if(!is.null(covars_as)) {

    covar <- data.table()
    for(cc in covars_as) {
      message(paste0("Reading in age-sex covariate ",cc, " from the DB"))
      cov_tmp <- get_covariate_data(covariate_dataset_id = as.numeric(covar_as_versions[cc]))

      if(nrow(covar) == 0) {
        covar <- copy(cov_tmp)
      } else {
        covar <- merge(covar, cov_tmp, by = cov_merge_vars)
      }

      if(nrow(covar) == 0) stop("Covar has zero rows")

    }

    covar <- covar[year %in% years, c(area_var, "year", "age", "sex", covars_as), with = F]

    assign("covar_as_data", covar)

  } else {
    assign("covar_as_data", data.table(expand.grid(area_var = unique(data$area), year = years, sex = sexes, age = ages)))
    setnames(covar_as_data, "area_var", area_var)
  }

  if(!is.null(covars)) {

    covar <- data.table()
    for(cc in covars[!(covars %in% c("indic","indic_1","indic_2"))]) {
      message(paste0("Reading in mcnty covariates ",cc, " from the DB"))
      cov_tmp <- get_covariate_data(covariate_dataset_id = as.numeric(covar_versions[cc]))

      if(nrow(covar) == 0) {
        covar <- copy(cov_tmp)
      } else {
        covar <- merge(covar, cov_tmp, by = cov_merge_vars)
      }

      if(nrow(covar) == 0) stop("Covar has zero rows")

    }

    # subset to columns of interest
    covar <- covar[year %in% years, c(area_var, "year", covars[!(covars %in% c("indic","indic_1","indic_2"))]), 
                   with = F]

    assign("covar_data", covar)

  } else {
    assign("covar_data", data.table(expand.grid(area_var = unique(data$area), year = years)))
    setnames(covar_data, "area_var", area_var)
  }
  
  # Merge everything together
  potential_merge_vars <- c(area_var, "year", "sex", "race", "edu", "age")
  level1_vars <- potential_merge_vars[potential_merge_vars %in% intersect(names(covar_as_data), names(covar_data))]
  level1 <- merge(covar_as_data, covar_data, by = level1_vars, allow.cartesian = T)
  if (nrow(level1) == 0) stop("merge of covar_as_data and covar_data resulted in 0 rows.")

  level2_vars <- potential_merge_vars[potential_merge_vars %in% intersect(names(level1), names(covar_offset))]

  level3_vars <- potential_merge_vars[potential_merge_vars %in% intersect(names(level2), names(covar_subpop))]
  covar <- merge(level2, covar_subpop, by = level3_vars, allow.cartesian = T)

  # test for duplicates
  if (nrow(covar) != nrow(unique(covar[, potential_merge_vars, with = F]))) {
    stop(glue::glue("There are duplicated rows in covar among columns {paste(potential_merge_vars, collapse=', ')}"))
  }

  rm(covar_subpop, covar_as_data, covar_data, level1, level2); gc()

  # transform and standardize covariates
  if (!is.null(covars_trans)) {
    for (var in names(covars_trans)) {
      fun <- eval(parse(text = paste("function(x)", covars_trans[var])))
      covar[[var]] <- fun(covar[[var]])
    }
  }

  # this standardizes the covariates that were transformed above.
  # that covars_offset is omitted, that is intentional. We don't want to transform it.
  for (var in c(covars[!(covars %in% c("cohort","low_mx_age","indic","indic_1","indic_2"))], 
                covars_as, covars_subpop)) {
    if (covar[, uniqueN(get(var))] == 2) next # don't standardize indicator covariates
    covar[[var]] <- (covar[[var]] - mean(covar[[var]])) / sd(covar[[var]])
  }
  
  ## Function to process various indicators
  process_indicator <- function(indicator_data, indic_name) {
    
    if ("race" %in% names(indicator_data)) {
      if ("age" %in% names(indicator_data)) {
        indicator_data[, age := as.integer(as.character(age))]
        covar <-
          merge(
            covar,
            indicator_data,
            by = c(area_var, "race", "age"),
            all = T
          )
      } else {
        covar <- merge(covar,
                       indicator_data,
                       by = c(area_var, "race"),
                       all = T)
      }
    } else {
      covar <- merge(covar, indicator_data, by = area_var, all = T)
    }
    
    stopifnot(nrow(covar[is.na(indic)]) == 0)
    stopifnot(nrow(covar[is.na(year)]) == 0)
    
    setnames(covar, c("indic"), c(indic_name))
    
    return(covar)
    
  }

  if (exists("indicator_file")) {
    if (!is.null(indicator_file)) {
      indicator_data <- readRDS(indicator_file)
      covar <- process_indicator(indicator_data, "indic")
    }
  }
  
  if (exists("indicator_file_1")) {
    if (!is.null(indicator_file_1)) {
      indicator_data <- readRDS(indicator_file_1)
      covar <- process_indicator(indicator_data, "indic_1")
    }
  }
  
  if (exists("indicator_file_2")) {
    if (!is.null(indicator_file_2)) {
      indicator_data <- readRDS(indicator_file_2)
      covar <- process_indicator(indicator_data, "indic_2")
    }
  }

  setnames(covar, area_var, "area")

  # merge covariates onto the dataset
  common_names <- intersect(names(data), names(covar))
  data <- merge(data, covar, by = common_names, all.x = T)

  rm(covar); gc()
}

if (exists("indicator_file")) {
  if (!is.null(indicator_year) & !is.null(indicator_file)) {
    if ("indic" %in% names(data)) {
      # make sure the non-indicator years all have 0s
      data[year != indicator_year, indic := 0]
    } else {
      data[, indic := ifelse(year == indicator_year, 1, 0)]
    }
  }
}

## Now for the model with 2 indicator files
if (exists("indicator_file_1") & exists("indicator_file_2")) {
  if (!is.null(indicator_year_1)) {
    if ("indic_1" %in% names(data)) {
      # make sure the non-indicator years all have 0s
      data[year != indicator_year_1, indic_1 := 0]
    } else {
      data[, indic_1 := ifelse(year == indicator_year_1, 1, 0)]
    }
  } else {
    stop("indicator year 1 is missing!")
  }
  
  
  # Now for the second indicator
  if (!is.null(indicator_year_2)) {
    if ("indic_2" %in% names(data)) {
      # make sure the non-indicator years all have 0s
      data[year != indicator_year_2, indic_2 := 0]
    } else {
      data[, indic_2 := ifelse(year == indicator_year_2, 1, 0)]
    }
  } else {
    stop("indicator year 2 is missing!")
  }
} else {
  indicator_year_1 <- NULL
  indicator_year_2 <- NULL
}


## If the cause is cvd_pah, get rid of data before 2004 because those data need to be outliered due to definitional issues
if(subset_cause) {
  if(get("cause_id", .GlobalEnv) == 1004) {

    data[year < 2004, deaths := NA]

  }
}


# encode / recode year from 0
data[, year := as.integer(year - years[1])]

# encode / recode age from 0
data[, age := as.integer(factor(age, levels = ages)) - 1L]

# create structure matrices for space, time, and age effects
adjmat <- readRDS(adjmat_file)
graph_j <- diag(apply(adjmat, 1, sum)) - adjmat
graph_j <- as(graph_j, "dgTMatrix")
rm(adjmat)

if(length(years) > 1) {
  num_t <- max(data$year) + 1
  graph_t <- matrix(rep(0, num_t^2), nrow = num_t, ncol = num_t)  # matrix of zeros
  graph_t[abs(row(graph_t) - col(graph_t)) == 1] <- 1
  graph_t <- diag(apply(graph_t, 1, sum)) - graph_t
  graph_t <- as(graph_t, "dgTMatrix")
} else {
  graph_t <- NULL
}


if (length(ages) > 1) {
  num_a <- max(data$age) + 1
  graph_a <- matrix(rep(0, num_a^2), nrow = num_a, ncol = num_a)
  graph_a[abs(row(graph_a) - col(graph_a)) == 1] <- 1
  graph_a <- diag(apply(graph_a, 1, sum)) - graph_a
  graph_a <- as(graph_a, "dgTMatrix")
} else {
  graph_a <- NULL
}

# save prepped data

if (!is.null(indicator_year)) {
  setkeyv(data, c("area", "year", "sex", "race", "edu", "age", "indic"))
} else if (!is.null(indicator_year_1) & !is.null(indicator_year_2)) {
  setkeyv(data, c("area", "year", "sex", "race", "edu", "age", "indic_1", "indic_2"))
} else {
  setkeyv(data, c("area", "year", "sex", "race", "edu", "age"))

}

saveRDS(data, file = paste0(lu_modeldir, "/data.rds"))
save(graph_j, graph_t, graph_a, file = paste0(dir, "/re_graphs.rdata"))

cat("DONE.")
