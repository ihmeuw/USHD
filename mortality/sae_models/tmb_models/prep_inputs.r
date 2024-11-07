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
## Outputs:     prepped data file ("[FILEPATH]/data.rds")
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

# this file contains an indicator column that will allow state-years that are missing edu
# information to be dropped.
if (by_edu) {
  infants_non_reporting <-
    fread(
      "[FILEPATH]"
    )
}

loc <- fread("[FILEPATH]")
# reference file for year when the Multiracial option was added to the death certificate in each state
multiracial_implementation <- fread("[FILEPATH]")

## Load and prep inputs ----------------------------------------------------------------------------
# combine deaths and population
deaths <- readRDS(deaths_file)

message("Done with deaths")

# subset to cause specified in settings
subset_cause <- F
if("cause_id" %in% names(deaths)) {
  message("Subsetting")
  subset_cause <- T

  if(cause_id != 294) {
    allcause <- deaths[cause_id == 294]
    stopifnot(nrow(allcause) > 0)
    setnames(allcause, area_var, "area")
    allcause <- allcause[year %in% years]

    if (!by_race) allcause[, race := all_pop_id]
    if (!by_edu) allcause[, edu := all_pop_id]
    allcause <- allcause[year %in% years, list(deaths = sum(deaths)), by = "area,year,sex,race,edu,age"]
  }

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

if(subset_cause) {

  if(get("cause_id", .GlobalEnv) != 294) {
    # get population to merge onto all-cause mortality for the purposes of figuring out where deaths > pop but pop = 0.
    pop_for_allcause <- copy(pop)[year %in% years, list(pop = sum(pop)), by = "area,year,sex,race,edu,age,state"]
  }

}

pop <- pop[year %in% years & age %in% ages, list(pop = sum(pop)), by = "area,year,sex,race,edu,age,state"]

stopifnot(nrow(pop) > 0)

message("Merging pop and deaths together")
data <- merge(pop, deaths, by = c("area", "year", "sex", "race", "edu", "age"), all = T)
if (all(is.na(data[, deaths]))) stop("All rows of deaths are NA")

## Prep growth rates for using the Horiuchi-Coale method
if (lt_hc) {
  calc_growth_rate(dir, test_by_race = by_race, test_by_edu = by_edu, n_years = ifelse(by_edu, 5, 10),
                   years, pop_growth_file, area_var)
  message("Done with calculating growth rates!")

}

data[is.na(deaths), deaths := 0]
data[is.na(pop), pop := 0]


if (nrow(data[pop == 0 | deaths == 0]) == nrow(data)) {
  stop("Every single row has either a zero in deaths or pop, suggesting a bad merge of deaths with pop.")
}

## Make the all-cause data, if we are not already modeling for all-cause
if(subset_cause) {

  if(get("cause_id", .GlobalEnv) != 294) {
    # get population to merge onto all-cause mortality for the purposes of figuring out where deaths > pop but pop = 0.
    allcause <- merge(pop_for_allcause, allcause, by = c("area", "year", "sex", "race", "edu", "age"), all = T)
    stopifnot(nrow(allcause[is.na(pop)]) == 0)
    allcause[is.na(deaths), deaths := 0]
    # Determine where deaths > pop and pop == 0
    allcause[,adjust_pop := as.integer(pop == 0 & deaths > pop)]
    allcause <- allcause[,.(area, year, sex, race, edu, age, adjust_pop)]
    # merge onto the data
    data <- merge(data, allcause, by=c("area", "year", "sex", "race", "edu", "age"), all.x=T)
    stopifnot(nrow(data[is.na(adjust_pop)]) == 0)
  } else {
    # otherwise, assume that you are modeling all-cause mortality
    data[,adjust_pop := as.integer(pop == 0 & deaths > pop)]
  }


} else {
  # otherwise, assume that you are modeling all-cause mortality
  data[,adjust_pop := as.integer(pop == 0 & deaths > pop)]
}


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
    expanded <-
      prep_misclass(
        # just copies the misclassification draws into the directory
        n.sims,
        dir = dir,
        filepath = ifelse(
          test = by_edu,
          yes = ifelse(
            test = n.sims == 1000,
            yes = "[FILEPATH]",
            no = "[FILEPATH]"
          ),
          no = "[FILEPATH]"
        )
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
        start_nrow <- nrow(covar)
        covar <- merge(covar, cov_tmp, by = cov_merge_vars) 
        end_nrow <- nrow(covar)
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

  # offset for education modeling
  if (!is.null(covars_offset)) {
    covar <- readRDS(covar_offset_file)

    if("age" %in% names(covar)) {
      covar <- covar[year %in% years, c(area_var, "year", "age", covars_offset), with = F] 
    } else {
      covar <- covar[year %in% years, c(area_var, "year", covars_offset), with = F]
    }
    assign("covar_offset", covar)

  } else {
    assign("covar_offset", data.table(expand.grid(area_var = unique(data$area), year = years, race = races, edu = edu_groups)))
    setnames(covar_offset, "area_var", area_var)
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

    if(length(covars[!(covars %in% c("indic","indic_1","indic_2"))]) > 0) {

      covar <- data.table()
      for(cc in covars[!(covars %in% c("indic","indic_1","indic_2"))]) {
        message(paste0("Reading in mcnty covariates ", cc, " from the DB"))
        cov_tmp <- get_covariate_data(covariate_dataset_id = as.numeric(covar_versions[cc]))

        if (cc == "ethn_hisp") {
          if (max(cov_tmp$ethn_hisp > 1)) {
            print("ethn_hisp summary before any changes:")
            print(summary(cov_tmp$ethn_hisp))
            cov_tmp[, ethn_hisp := ethn_hisp / 100]
            print("ethn_hisp summary after rescaling:")
            print(summary(cov_tmp$ethn_hisp))
          }
        }

        if (nrow(covar) == 0) {
          covar <- copy(cov_tmp)
        } else {
          covar <- merge(covar, cov_tmp, by = cov_merge_vars)
        }

        if (nrow(covar) == 0)
          stop("Covar has zero rows")

      }

      # subset to columns of interest
      covar <- covar[year %in% years, c(area_var, "year", covars[!(covars %in% c("indic","indic_1","indic_2"))]),
                     with = F]

      assign("covar_data", covar)

    } else {

      assign("covar_data", data.table(expand.grid(area_var = unique(data$area), year = years)))
      setnames(covar_data, "area_var", area_var)

    }


  } else {
    assign("covar_data", data.table(expand.grid(area_var = unique(data$area), year = years)))
    setnames(covar_data, "area_var", area_var)
  }

  
  potential_merge_vars <- c(area_var, "year", "sex", "race", "edu", "age")
  level1_vars <- potential_merge_vars[potential_merge_vars %in% intersect(names(covar_as_data), names(covar_data))]
  level1 <- merge(covar_as_data, covar_data, by = level1_vars, allow.cartesian = T)
  if (nrow(level1) == 0) stop("merge of covar_as_data and covar_data resulted in 0 rows.")

  level2_vars <- potential_merge_vars[potential_merge_vars %in% intersect(names(level1), names(covar_offset))]
  level2 <- merge(level1, covar_offset, by = level2_vars, allow.cartesian = T)
  if (nrow(level2) == 0) stop("merge of covar_offset and level1 resulted in 0 rows.")

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

  for (var in c(covars[!(covars %in% c("cohort","low_mx_age","indic","indic_1","indic_2","indic_20","indic_21"))],
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


  if(model %in% c("spline_indicator_covid")) {

    stop("Modeling not currently set up to run")

  }

  setnames(covar, area_var, "area")

  # merge covariates onto the dataset
  common_names <- intersect(names(data), names(covar))
  data <- merge(data, covar, by = common_names, all.x = T)

  rm(covar); gc()
}

if(model %in% c("spline_indicator_covid")) {

  stop("Code not currently set up to create this indicator")

}


if (exists("indicator_file")) {
  if (!is.null(indicator_year) & !is.null(indicator_file)) {
    if ("indic" %in% names(data)) {
      data[year != indicator_year, indic := 0]
    } else {
      stop("indic not already a column in data")
    }
  }
}

## Now for the model with 2 indicator files
if (exists("indicator_file_1") & exists("indicator_file_2")) {
  if (!is.null(indicator_year_1)) {
    if ("indic_1" %in% names(data)) {
      data[year != indicator_year_1, indic_1 := 0]
    } else {
      stop("indic_1 not already a column in data")
    }
  } else {
    stop("indicator year 1 is missing!")
  }

  # Now for the second indicator
  if (!is.null(indicator_year_2)) {
    if ("indic_2" %in% names(data)) {
      data[year != indicator_year_2, indic_2 := 0]
    } else {
      stop("indic_2 not already a column in data")
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

    # we don't want to remove the rows because this will cause prediction issues, but we don't want to use the data either
    data[year < 2004, deaths := NA]

  }
}


# encode / recode year from 0
data[, year := as.integer(year - years[1])]

# encode / recode age from 0
data[, age := as.integer(factor(age, levels = ages)) - 1L]

# test for duplicated column names
if(!(length(names(data)) == length(unique(names(data))))) {
  stop("There are duplicated column names in data.")
}

# create structure matrices for space, time, and age effects
adjmat <- readRDS(adjmat_file)

graph_j <- diag(apply(adjmat, 1, sum)) - adjmat
graph_j <- as(graph_j, "dgTMatrix")
rm(adjmat)

num_e <- length(unique(data[edu != edu_default & edu != unknown_edu, edu])) # shouldn't include all-edu or unknown

graph_e <- matrix(rep(0, num_e^2), nrow = num_e, ncol = num_e)
graph_e[abs(row(graph_e) - col(graph_e)) == 1] <- 1 
graph_e <- diag(apply(graph_e, 1, sum), nrow = nrow(graph_e), ncol = ncol(graph_e)) - graph_e
graph_e <- as(graph_e, "dgTMatrix")

if(length(years) > 1) {
  num_t <- max(data$year) + 1
  graph_t <- matrix(rep(0, num_t^2), nrow = num_t, ncol = num_t)  # matrix of zeros
  graph_t[abs(row(graph_t) - col(graph_t)) == 1] <- 1

  if(detach_2019_2020) {
    message("Detaching 2019 and 2020")
    index_19 <- which(years == 2019)
    index_20 <- which(years == 2020)
    graph_t[index_19, index_20] <- 0
    graph_t[index_20, index_19] <- 0
  }

  if(detach_2020_2021) {
    message("Detaching 2020 and 2021")
    index_20 <- which(years == 2020)
    index_21 <- which(years == 2021)
    graph_t[index_20, index_21] <- 0
    graph_t[index_21, index_20] <- 0
  }


  if(detach_2021_2022) {
    message("Detaching 2021 and 2022")
    index_21 <- which(years == 2021)
    index_22 <- which(years == 2022)
    graph_t[index_21, index_22] <- 0
    graph_t[index_22, index_21] <- 0
  }


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

key_vars <- c("area", "year", "sex", "race", "edu", "age")

if (!is.null(indicator_year)) {
  key_vars <- c(key_vars, "indic")
  if("race_agg" %in% names(data)) key_vars <- c(key_vars, "race_agg")
} else if (!is.null(indicator_year_1) & !is.null(indicator_year_2)) {
  key_vars <- c(key_vars, "indic_1", "indic_2")
  if("race_agg" %in% names(data)) key_vars <- c(key_vars, "race_agg")

} else if ("indic_20" %in% names(data)) {

  key_vars <- c(key_vars, "indic_20", "indic_21")
  if("race_agg" %in% names(data)) key_vars <- c(key_vars, "race_agg")

} else {
  if("race_agg" %in% names(data)) key_vars <- c(key_vars, "race_agg")
}

setkeyv(data, key_vars)

saveRDS(data, file = paste0(lu_modeldir, "/data.rds"))
save(graph_j, graph_t, graph_a, graph_e, file = paste0(dir, "/re_graphs.rdata"))

cat("DONE.")
