####################################################################################################
## Description: Prep inputs for small area models for prevalence of fair or poor general health. 
##              Specifically, combine with covariates data. Recode age groups and years to run 
##              sequentially from zero. Also, prep CAR structure matrices for spatial, temporal, and 
##              age effects.
##
## Passed args: repo [character] -- location of nonfatal repository
##              output_dir [character] -- location for model outputs
##              settings_loc [character] -- file path of settings file
##
## Requires:    covariates data, if used (covars, covars_as, covars_trans, covar_file, covar_as_file)
##              neighborhood adjacency matrix (adjmat_file)
##
## Outputs:     prepped data file ("[output_dir]/data.rds")
##              graphs for age, time, and year random effects ("[output_dir]/re_graphs.rdata")
##
####################################################################################################

###### Load required libraries
pacman::p_load(R.utils, data.table, TMB, car, splines, stringr, survival)
library(lbd.loader, lib.loc = sprintf("FILEPATH", 
                                      R.version$major, 
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))

library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (output_dir_draws_est <- commandArgs(TRUE)[[3]])
  (settings_loc <- commandArgs(TRUE)[[4]])
  (imp <- commandArgs(TRUE)[[5]])
  (n_folds <- commandArgs(TRUE)[[6]])
} else {
  imp <- 0
}

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

message(repo)
message(output_dir)
message(settings_loc)

###### Assign settings from settings file
get_settings(settings_loc)

##### Process data_file object
if (length(data_file) > 1 | !is.null(names(data_file))) {
  data_file <- data.table("name" = names(data_file), "location" = data_file)
} else {
  data_file <- data.table("name" = "Single source", "location" = data_file)
}

##### Process outcome object
if (length(outcome) > 1 | !is.null(names(outcome))) {
  outcome <- data.table("name" = names(outcome), "outcome" = outcome)
} else {
  outcome <- data.table("name" = "Single outcome", "outcome" = outcome)
}

##### Process geo_crosswalks object
if (length(geo_crosswalks) > 1 | !is.null(names(geo_crosswalks))) {
  geo_crosswalks <- data.table("level" = names(geo_crosswalks), "run_date" = geo_crosswalks)
} else {
  geo_crosswalks <- data.table("level" = "Single level", "run_date" = geo_crosswalks)
}

##### Now loop through data files
combined_dt <- data.table()

###### Get mcnty:state mapping
locs <- fread("FILEPATH")

if (type == "validation" & n_folds == 1) {
  combined_dt <- readRDS(paste0(output_dir, "/resampled_data.rds"))
} else {
  for (data_set in 1:nrow(data_file)) {
    print(paste0("Processing ", data_file[data_set, name], "..."))
    
    ##### Load and prep inputs
    dt <- readRDS(data_file[data_set, location])
    
    if (imp == 0) {
      ###### Update some fields
      if (data_file[data_set, name] == "HCUP") {
        setnames(dt, c("pop", "year_id", "sex_id"), c("sample_size", "year", "sex"), skip_absent = TRUE)
      } else if (data_file[data_set, name] %in% c("mdcr", "max")) {
        setnames(dt, c("year_id", "sex_id", "FIPS_state"), c("year", "sex", "state"), skip_absent = TRUE)
        dt$state <- as.integer(dt$state)
        dt <- dt[!(source == "max" & is.na(cases))]
      } else if (data_file[data_set, name] %in% c("mdcr_eligibility")) {
        setnames(dt, c("pop"), c("sample_size"), skip_absent = TRUE)
        dt[, state_name := NULL]
      } else if (data_file[data_set, name] %in% c("NHANES")) {
        dt[, c("mcnty", "year")] <- NA
        setnames(dt, c("edu_ushd", "marital_ushd", "age"), c("edu_code", "marital_ushd", "age_bin"), skip_absent = TRUE)
      } else if (data_file[data_set, name] %in% c("MarketScan")) {
        dt[, c("edu", "marital") := NULL]
        setnames(dt, "age", "age_bin")
      }
      
      ###### Rename outcome
      setnames(dt, outcome[data_set, outcome], "cases")
      
      ###### Add a data source field
      dt$source <- data_file[data_set, name]
      
      ###### Add to combined data set
      combined_dt <- rbindlist(list(combined_dt, dt), use.names = TRUE, fill = TRUE)
    } else if (imp > 0) {
      if (!(outcome[data_set, outcome] %like% 'obese')) {
        
        ###### Add a data source field
        dt$source <- data_file[data_set, name]
        dt <- dt[,imp := imp]#add imputation indicator
        setnames(dt, paste0(outcome[data_set, outcome], "_", imp), "cases")
        combined_dt <- rbindlist(list(combined_dt, dt), use.names = TRUE, fill = TRUE)
        
      } else if (outcome[data_set, outcome] %like% 'obese') {#keep only dataset corresponding to imp
        
        ###### Add a data source field
        dt <- setDT(dt[[as.numeric(imp)]])
        dt$source <- data_file[data_set, name]
        dt <- dt[,imp := imp]#add imputation indicator
        ###### Rename outcome
        setnames(dt, c(paste0(outcome[data_set, outcome], "_", imp), paste0("sample_size_", imp), paste0("weights_", imp)), c("cases", "sample_size", "weights"))
        combined_dt <- rbindlist(list(combined_dt, dt), use.names = TRUE, fill = TRUE)
      }
      
      exclude <- grep("cases_", names(combined_dt), value = TRUE)
      combined_dt <- combined_dt[,!..exclude]
    }
  }
  
  ###### Drop rows with 0 or NA sample size
  combined_dt <- combined_dt[!is.na(sample_size) & sample_size > 0 & !is.na(cases)]
  
  ###### Rename column(s)
  setnames(combined_dt, c("age_bin", "mcnty", "sex_id", "year_id"), c("age", "area", "sex", "year"), skip_absent = TRUE)
  
  if (!is.null(combined_dt$edu_code)) {
    setnames(combined_dt, "edu_code", "edu")
  }
  if (!is.null(combined_dt$marital_ushd)) {
    setnames(combined_dt, "marital_ushd", "marital")
  }
  if (!is.null(combined_dt$ushd_phone)) {
    setnames(combined_dt, "ushd_phone", "phone")
  }
  
  stopifnot(nrow(combined_dt) > 0)
  
  ###### Convert age_group_id to age for clinical data
  if (any(c("mdcr", "max", "HCUP") %in% data_file$name)) {
    ## Load and merge age group data
    age_groups <- fread("FILEPATH")
    combined_dt <- merge(combined_dt, age_groups[, list(age_group_id, age_start)], by = "age_group_id", all.x = TRUE)
    if ("age" %in% colnames(combined_dt)) {
      combined_dt[is.na(age) & !is.na(age_start), age := age_start]
      combined_dt[, age_start := NULL]
    } else {
      setnames(combined_dt, "age_start", "age")
    }
  }
  
  ###### Set key and sort
  if (!("area" %in% colnames(combined_dt))) {
    combined_dt[, "area" := NA]
  }
  
  setkeyv(combined_dt, c("area", "year", "sex", "race", "age", strat_vars))
  
  ###### Merge location metadata
  if (!is.null(combined_dt$state)) {
    combined_dt <- merge(combined_dt, unique(locs[, list(mcnty, state)]), by.x = "area", by.y = "mcnty", all.x = TRUE)
    combined_dt[is.na(state.x) & !is.na(state.y), state.x := state.y]
    combined_dt <- merge(combined_dt, unique(locs[, list(state, state_name)]), by.x = "state.x", by.y = "state", all.x = TRUE)
    combined_dt[, state.y := NULL]
    setnames(combined_dt, "state.x", "state")
  } else if ("ACS" %in% source_levels) {
    ###### Merge state names
    puma_mcnty_mapping <- readRDS(paste0("FILEPATH"))
    combined_dt <- merge(combined_dt, unique(puma_mcnty_mapping[, c("puma_mcnty", "puma_version", "year", "state")]), by = c("puma_mcnty", "puma_version", "year"), all.x = TRUE)
    combined_dt <- merge(combined_dt, unique(locs[, list(state, state_name)]), by = "state", all.x = TRUE)
  } else if (any(c("mdcr", "max", "HCUP") %in% data_file$name)) { # Clinical data
    combined_dt <- merge(combined_dt, unique(locs[, list(mcnty, state, state_name)]), by.x = "area", by.y = "mcnty", all.x = TRUE)
    
    #### Set state for state-level data
    if ("FIPS_state" %in% colnames(combined_dt)) {
      combined_dt[is.na(state) & !is.na(FIPS_state), state := as.integer(FIPS_state)]
    }
  } else {
    combined_dt <- merge(combined_dt, unique(locs[, list(mcnty, state, state_name)]), by.x = "area", by.y = "mcnty", all.x = TRUE)
  }
}

###### Check race codes for input data
validate_race_codes(DT = combined_dt,
                    race_set_id = 1, # specifies DB code
                    includes_all_race = F, # says not to expect the all-race variable (1)
                    silent = T, # doesn't print anything if the outputs look okay
                    expect_missing_race = c(3)) # tells the function that it's okay if NH Other is missing

###### Check states object
if (exists("states")) {
  if ((length(states) > 0) & states[1] != "all") { # Specific states were requested
    #### First check whether CBSA-level data are present
    if (!is.null(combined_dt$cbsa_mcnty_code)) {
      if (nrow(combined_dt[!is.na(cbsa_mcnty_code)]) > 0) {
        #### Retain only data for requested states or cbsa_mcnties that at least partially fall in the requested states
        combined_dt <- combined_dt[state_name %in% states | (!is.na(cbsa_mcnty_code) & cbsa_mcnty_code %in% combined_dt[state_name %in% states, cbsa_mcnty_code])]
      } else {
        #### Retain only data for requested states
        combined_dt <- combined_dt[(state_name %in% states)]
      }
    } else {
      #### Retain only data for requested states
      combined_dt <- combined_dt[(state_name %in% states)]
    }
    
    #Create adjacency matrix ----------------------------------------------------------------------------------------------------------------------------------------------------------
    ## Load adjacency file (from the Census Bureau, current to 2010 census counties)
    adj <- fread(paste0("FILEPATH"))
    adj <- adj[, c(2, 4), with = F]
    setnames(adj, c("cnty1", "cnty2"))
    adj[, cnty1 := nafill(cnty1, "locf")]
    adj <- adj[cnty1 < 60000,]
    
    ## Map 2010 county adjacencies to merged county adjacencies
    adj[, cnty1 := car::recode(cnty1, locs[, paste(paste(cnty, mcnty, sep = "="), collapse = ";")])]
    adj[, cnty2 := car::recode(cnty2, locs[, paste(paste(cnty, mcnty, sep = "="), collapse = ";")])]
    original_adj <- copy(adj)
    adj <- rbind(adj, data.table(cnty1 = 1209, cnty2 = 1200))
    adj <- rbind(adj, data.table(cnty1 = 1200, cnty2 = 1209))
    
    adj <- unique(adj)
    
    ## Make all Hawaii counties neighbors (otherwise they have no neighbors except themselves)
    adj <- rbind(adj, CJ(cnty1 = locs[state_name == "Hawaii", unique(mcnty)],
                         cnty2 = locs[state_name == "Hawaii", unique(mcnty)]))
    
    ## Make sure that adjacencies are symmetric
    adj <- unique(rbind(adj, data.table(cnty1 = adj$cnty2, cnty2 = adj$cnty1)))
    
    ## Don't let counties be adjacent to themselves
    adj <- adj[cnty1 != cnty2]
    
    ## Confirm that we made the expected additions
    dplyr::anti_join(adj, original_adj)
    
    ## Restrict to requested states
    adj_final <- adj[cnty1 %in% locs[state_name %in% states, mcnty] & cnty2 %in% locs[state_name %in% states, mcnty]]
    
    ## Reindex from 0
    requested_areas <- unique(locs[state_name %in% states, mcnty])
    adj_final[, cnty1_recode := as.integer(factor(cnty1, levels = requested_areas)) - 1L]
    adj_final[, cnty2_recode := as.integer(factor(cnty2, levels = requested_areas)) - 1L]
    
    ## Convert to an adjacency matrix
    adjmat <- sparseMatrix(i = adj_final$cnty1_recode + 1, j = adj_final$cnty2_recode + 1, x = rep(1, nrow(adj_final)), repr = "T")
  }
}

###### Restrict to requested races, sexes, ages and years
#### Retain all NHANES data here; they will be subset to requested strata after disaggregation
combined_dt <- combined_dt[((race %in% races) & (sex %in% sexes) & (age %in% ages) & (year %in% years)) | source %in% c("NHANES") |
                             (((sex %in% sexes) & (age %in% ages) & (year %in% years)) & source %in% c("MarketScan"))]

#### Load covariates from database
if (length(covars) > 0) {
  library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
  
  cov <- data.table()
  for (current_cov in covars) {
    print(current_cov)
    cov_data <- get_covariate_data(covariate_name = current_cov)
    
    cov_data[, c("race_label", "edu_label", "race_set", "edu") := NULL]
    
    if (current_cov == "pop_density") {
      cov_data[, c("race", "sex", "age") := NULL]
      if (nrow(cov) == 0) {
        cov <- copy(cov_data)
      } else {
        cov <- merge(cov, cov_data, by = c("year", "mcnty", "state"), all = TRUE)
      }
    } else {
      if (nrow(cov) == 0) {
        cov <- copy(cov_data)
      } else {
        cov <- merge(cov, cov_data, all = TRUE)
      }
    }
  }
  
  ### Clean up
  cov <- cov[year %in% years]
  combos <- as.data.table(expand.grid(sex = sexes, age = ages, merger = 1))
  cov$merger <- 1
  cov <- merge(cov, combos, by = "merger", allow.cartesian = TRUE, all = TRUE)
  cov$merger <- NULL
  cov[, c("sex", "age", "sex.x", "age.x", "sex.y", "age.y") := list(sex.y, age.y, NULL, NULL, NULL, NULL)]
  
  ## Standardize covars
  covars <- gsub("_by_race_ethn", "", covars)
  for (var in covars) {
    if (var %in% c("income_pc", "pop_density")) {
      temp <- scale(log(cov[, ..var]))
    } else {
      temp <- scale(cov[, ..var])
    }
    # temp <- scale(log(cov[, ..var]))
    cov[, var] <- temp
  }
  setnames(cov, area_var, "area")
  cov[, state := NULL]
} else {
  cov <- NULL
}

###### Get and save population estimates
if (exists("pop_covariate_dataset_id")) {
  if (!is.null(pop_covariate_dataset_id)) {
    pop <- get_population_data(covariate_dataset_id = pop_covariate_dataset_id)
    pop[, "race_set" := NULL] # Drop race_set to avoid confusion
  }
} else {
  pop <- readRDS(pop_file)
}

###### Check race codes for population data
validate_race_codes(DT = pop,
                    race_set_id = 1, # specifies DB code
                    includes_all_race = F, # says not to expect the all-race variable (1)
                    silent = T, # doesn't print anything if the outputs look okay
                    expect_missing_race = c(3)) # tells the function that it's okay if NH Other is missing


## Restrict to requested races, sexes, ages, and years
pop <- pop[(race %in% races) & (sex %in% sexes) & (age %in% ages) & (year %in% years)]

## Save population data
saveRDS(pop, file = paste0(output_dir_draws_est, "/population.rds"))

###### Load mortality rate covariates, if specified
if (!exists("covars_mx")) {
  covars_mx <- NULL
}

if (!is.null(covars_mx)) { #### Note: This code block has been updated to use db race codes but has not yet been tested
  for (i in 1:length(covars_mx)) {
    cov_temp <- readRDS(covars_mx[[i]][which(names(covars_mx[[i]]) == "location")])
    
    cov_name <- paste0("mx_", covars_mx[[i]][which(names(covars_mx[[i]]) == "acause")])
    
    #### Clean up
    cov_temp <- cov_temp[year %in% years & level == "mcnty" & sex != 3 & race != 1 & age < 98, -c("mx_lb", "mx_ub", "mx_se", "edu", "acause")]
    setnames(cov_temp, "mx_mean", cov_name)
    
    #### Rescale
    temp <- scale(log(cov_temp[, ..cov_name]))
    cov_temp[, cov_name] <- temp
    
    #### Merge to existing cov
    if (!is.null(cov)) {
      cov <- merge(cov, cov_temp, by = c("area", "race", "year", "sex", "age"), all = TRUE)
    } else {
      cov <- copy(cov_temp)
    }
    
    covars <- c(covars, cov_name)
    rm(cov_temp, cov_name)
  }
}

###### Check race codes for covs
validate_race_codes(DT = cov,
                    race_set_id = 1, # specifies DB code
                    includes_all_race = F, # says not to expect the all-race variable (1)
                    silent = T, # doesn't print anything if the outputs look okay
                    expect_missing_race = c(3)) # tells the function that it's okay if NH Other is missing

###### Resort
setkeyv(combined_dt, c("area", "year", "sex", "race", "age", "source", strat_vars))

if (!(type == "validation" & n_folds == 1)) {
  ###### Collapse unrequested stratifying variables in data set
  if ("BRFSS" %in% source_levels) {
    if (grepl("weighted_count", outcome[name == "BRFSS", outcome])) { # Using weighted counts
      combined_dt_brfss <- combined_dt[source == "BRFSS", list(cases = weighted.mean(cases / sample_size, weights) * sum(sample_size), weights = sum(weights), sample_size = sum(sample_size)), by = c("area", "year", "sex", "race", "age", "source", "state", "state_name", "cbsa_mcnty_code", strat_vars)]
    } else { # Using raw counts
      combined_dt_brfss <- combined_dt[source == "BRFSS", list(cases = sum(cases), weights = sum(weights), sample_size = sum(sample_size)), by = c("area", "year", "sex", "race", "age", "source", "state", "state_name", "cbsa_mcnty_code", strat_vars)]
    }
  } else {
    combined_dt_brfss <- NULL 
  }
  
  if ("ACS" %in% source_levels) {
    if (grepl("weighted_count", outcome[name == "ACS", outcome])) { # Using weighted counts
      combined_dt_acs <- combined_dt[source == "ACS", list(cases = weighted.mean(cases / sample_size, weights) * sum(sample_size), weights = sum(weights), count = sum(count), sample_size = sum(sample_size)), by = c("area", "puma_version", "puma_mcnty", "year", "sex", "race", "age", "source", "state", "state_name", strat_vars)]
    } else { # Using raw counts
      combined_dt_acs <- combined_dt[source == "ACS", list(cases = sum(cases), weights = sum(weights), sample_size = sum(sample_size)), by = c("area", "puma_version", "puma_mcnty", "year", "sex", "race", "age", "source", "state", "state_name", strat_vars)]
    }
  } else {
    combined_dt_acs <- NULL
  }
  
  if ("Gallup" %in% source_levels) {
    if (grepl("weighted_count", outcome[name == "Gallup", outcome])) { # Using weighted counts
      combined_dt_gallup <- combined_dt[source == "Gallup", list(cases = weighted.mean(cases / sample_size, weights) * sum(sample_size), weights = sum(weights), sample_size = sum(sample_size)), by = c("area", "year", "sex", "race", "age", "source", "state", "state_name", strat_vars)]
    } else { # Using raw counts
      combined_dt_gallup <- combined_dt[source == "Gallup", list(cases = sum(cases), weights = sum(weights), sample_size = sum(sample_size)), by = c("area", "year", "sex", "race", "age", "source", "state", "state_name", strat_vars)]
    }
  } else {
    combined_dt_gallup <- NULL
  }
  
  combined_dt_clinical <- combined_dt[source %in% c("HCUP", "mdcr", "max", "mdcr_eligibility", "MarketScan")]
  
  if ("NHANES" %in% source_levels) {
    if (grepl("weighted_count", outcome[name == "NHANES", outcome])) { # Using weighted counts
      combined_dt_NHANES <- combined_dt[source == "NHANES", list(cases = weighted.mean(cases / sample_size, weights) * sum(sample_size), weights = sum(weights), sample_size = sum(sample_size)), by = c("area", "svyyear", "sex", "race", "age", "source", "state", "state_name", strat_vars)]
    } else { # Using raw counts
      combined_dt_NHANES <- combined_dt[source == "NHANES", list(cases = sum(cases), weights = sum(weights), sample_size = sum(sample_size)), by = c("area", "svyyear", "sex", "race", "age", "source", "state", "state_name", strat_vars)]
    }
  } else {
    combined_dt_NHANES <- NULL
  }
  
  #### Now combine source-specific data sets
  combined_dt <- rbindlist(list(combined_dt_brfss, combined_dt_acs, combined_dt_gallup, combined_dt_clinical, combined_dt_NHANES), use.names = TRUE, fill = TRUE)
  rm(combined_dt_brfss, combined_dt_acs, combined_dt_gallup, combined_dt_clinical, combined_dt_NHANES)
  
  ###### Set indicators for post-stratification
  combined_dt[, post_stratify := as.integer(age >= 20 & source %in% c("BRFSS", "Gallup", "ACS", "NHANES"))] # Only post-stratify survey data for ages 20+
  combined_dt[, post_stratify_by_phone := as.integer(source %in% c("BRFSS"))] # Only post-stratify by phone for BRFSS
}

###### Save copy of data set with non-recoded data
saveRDS(combined_dt, file = paste0(output_dir, "/data_pre_factor.rds"))

recodes <- list()

###### Resort again
setkeyv(combined_dt, c("area", "year", "sex", "race", "age", "source", strat_vars))

#### Apply data exclusions
if (!is.null(exclusions)) {
  for (current in exclusions) {
    combined_dt <- combined_dt[!(eval(parse(text = current)))]
  }
}

#### Recode vars to start from 0
# Source-race index (only including the races present in the data, and excluding gold-standard)
if (!is.null(gold_standard_source)) {
  combined_dt[source != gold_standard_source, source_race_recode := as.integer(.GRP) - 1L, .(source, race)]
  tmp <- na.omit(unique(combined_dt[, .(source_race_recode, source, race)]))
  if (nrow(tmp) > 0) {
    tmp <- tmp[, .(var = "source_race", source_race = paste0(source, "_", race), source_race_recode = source_race_recode, source, race), by = 1:nrow(tmp)]
    recodes$source_race <- tmp
    combined_dt[, c("source_race", "source_race_recode") := list(source_race_recode, NULL)] 
  }
} else {
  combined_dt[, source_race_recode := as.integer(.GRP)-1L, .(source, race)]
  tmp <- unique(combined_dt[, .(source_race_recode, source, race)])
  tmp <- tmp[, .(var = "source_race", source_race = paste0(source, "_", race), source_race_recode = source_race_recode, source, race), by = 1:nrow(tmp)]
  
  recodes$source_race <- tmp
  combined_dt[, c("source_race", "source_race_recode") := list(source_race_recode, NULL)]
}

# Race
combined_dt[, race_recode := as.integer(factor(race, levels = races)) - 1L]
recodes$race <- data.table(cbind.data.frame(var = "race", "race" = as.integer(races), "race_recode" = as.integer(factor(races, levels = races)) - 1L))
combined_dt[source == "NHANES", "race_NHANES" := race] # Save original NHANES race coding (will be recoded after disaggregation)
combined_dt[, c("race", "race_recode") := list(race_recode, NULL)]

# Source
combined_dt[, source_index_recode := as.integer(factor(source, levels = source_levels)) - 1L]
recodes$source <- data.table(cbind.data.frame(var = "source", "source_index" = source_levels, "source_index_recode" = as.integer(factor(source_levels, levels = source_levels)) - 1L))
combined_dt[, c("source_index", "source_index_recode") := list(source_index_recode, NULL)]

# Sex
combined_dt[, sex_recode := as.integer(factor(sex, levels = sexes)) - 1L]
recodes$sex <- data.table(cbind.data.frame(var = "sex", "sex" = as.integer(sexes), "sex_recode" = as.integer(factor(sexes, levels = sexes)) - 1L))
combined_dt[, c("sex", "sex_recode") := list(sex_recode, NULL)]

# Year
combined_dt[, year_recode := as.integer(factor(year, levels = years, ordered = TRUE)) - 1L]
recodes$year <- data.table(cbind.data.frame(var = "year", "year" = as.integer(years), "year_recode" = as.integer(factor(years, levels = years)) - 1L))
combined_dt[, c("year", "year_recode") := list(year_recode, NULL)]

# Age
combined_dt[, age_recode := as.integer(factor(age, levels = ages, ordered = TRUE)) - 1L]
recodes$age <- data.table(cbind.data.frame(var = "age", "age" = as.integer(ages), "age_recode" = as.integer(factor(ages, levels = ages)) - 1L))
combined_dt[, c("age", "age_recode") := list(age_recode, NULL)]

if ("edu" %in% strat_vars) {
  ###### Recode edu from 0
  combined_dt[, edu_recode := as.integer(factor(edu, levels = edu_levels)) - 1L]
  recodes$edu <- data.table(cbind.data.frame(var = "edu", "edu" = as.integer(edu_levels), "edu_recode" = as.integer(factor(edu_levels, levels = edu_levels)) - 1L))
  combined_dt[, c("edu", "edu_recode") := list(edu_recode, NULL)]
}

if ("marital" %in% strat_vars) {
  ###### Recode marital status from 0
  combined_dt[, marital_recode := as.integer(factor(marital, levels = marital_levels)) - 1L]
  recodes$marital <- data.table(cbind.data.frame(var = "marital", "marital" = as.integer(marital_levels), "marital_recode" = as.integer(factor(marital_levels, levels = marital_levels)) - 1L))
  combined_dt[, c("marital", "marital_recode") := list(marital_recode, NULL)]
}

###### Area (mcnty) and state
if (exists("states")) {
  if ((length(states) > 0) & states[1] != "all") { # Specific states were requested
    area_levels <- unique(locs[state_name %in% states, mcnty])
    state_levels <- unique(locs[state_name %in% states, state])
  } else {
    state_levels <- unique(locs$state)
  }
}

if (!exists("area_levels")) {
  area_levels <- unique(locs$mcnty)
}

if (!exists("state_levels")) {
  state_levels <- unique(locs$state)
}

combined_dt[, area_recode := as.integer(factor(area, levels = area_levels)) - 1L]
recodes$area <- data.table(cbind.data.frame(var = "mcnty", "area" = as.integer(area_levels), "area_recode" = as.integer(factor(area_levels, levels = area_levels)) - 1L))
setnames(recodes$area, "area", "mcnty")
combined_dt[, c("area", "area_recode") := list(area_recode, NULL)]

combined_dt[, state_recode := as.integer(factor(state, levels = state_levels)) - 1L]
recodes$state <- data.table(cbind.data.frame(var = "state", "state" = as.integer(state_levels), "state_recode" = as.integer(factor(state_levels, levels = state_levels)) - 1L))
combined_dt[, c("state", "state_recode") := list(state_recode, NULL)]

###### Save recoding file
saveRDS(recodes, file = paste0(output_dir, "/recode.rds"))

###### Create structure matrices for space, time, and age effects
if (!exists("adjmat")) { # If adjmat was not created above...
  adjmat <- readRDS(adjmat_file)
}

graph_j <- diag(apply(adjmat, 1, sum)) - adjmat
graph_j <- as(graph_j, "dgTMatrix")
rm(adjmat)

num_t <- nrow(recodes$year)
num_a <- nrow(recodes$age)
num_e <- nrow(recodes$edu)

if (is.null(num_e)) {
  num_e <- 0
}

graph_t <- matrix(rep(0, num_t^2), nrow = num_t, ncol = num_t)
graph_t[abs(row(graph_t) - col(graph_t)) == 1] <- 1
graph_t <- diag(apply(graph_t, 1, sum)) - graph_t
graph_t <- as(graph_t, "dgTMatrix")

if (length(ages) > 1) {
  graph_a <- matrix(rep(0, num_a^2), nrow = num_a, ncol = num_a)
  graph_a[abs(row(graph_a) - col(graph_a)) == 1] <- 1
  graph_a <- diag(apply(graph_a, 1, sum)) - graph_a
  graph_a <- as(graph_a, "dgTMatrix")
} else {
  graph_a <- NULL
}

graph_e <- matrix(rep(0, num_e^2), nrow = num_e, ncol = num_e)
graph_e[abs(row(graph_e) - col(graph_e)) == 1] <- 1
graph_e <- diag(apply(graph_e, 1, sum)) - graph_e
graph_e <- as(graph_e, "dgTMatrix")

#### Set up spline bases on age and time
if (is.null(age_knots_spec)) age_knots_spec <- c(0, 20, 50, 85)
if (is.null(year_knots_num)) year_knots_num <- 4

age_knots <- which(ages %in% age_knots_spec) - 1
s_age <- as.matrix(as.data.table(bs((1:num_a) - 1, knots = age_knots[1:(length(age_knots) - 1)], degree = 1, intercept = FALSE)))
num_age_spline <- ncol(s_age)
graph_as <- matrix(rep(0, num_age_spline^2), nrow = num_age_spline, ncol = num_age_spline)
graph_as[abs(row(graph_as) - col(graph_as)) == 1] <- 1
graph_as <- diag(apply(graph_as, 1, sum)) - graph_as
graph_as <- as(graph_as, "dgTMatrix")

year_knots <- seq(1, length(years), length.out = year_knots_num) - 1
s_year <- as.matrix(as.data.table(bs((1:num_t) - 1, knots = year_knots[1:(length(year_knots) - 1)], degree = 1, intercept = FALSE)))
num_year_spline <- ncol(s_year)
graph_ts <- matrix(rep(0, num_year_spline^2), nrow = num_year_spline, ncol = num_year_spline)
graph_ts[abs(row(graph_ts) - col(graph_ts)) == 1] <- 1
graph_ts <- diag(apply(graph_ts, 1, sum)) - graph_ts
graph_ts <- as(graph_ts, "dgTMatrix")

###### Deal with NA cases
combined_dt[is.na(cases), cases := NA_real_]

###### Set holdout folds
#### Create collapsed data set without post_strat vars
if (n_folds > 1) {
  collapsed_temp <- combined_dt[, list(cases = sum(cases), weights = sum(weights), sample_size = sum(sample_size), .N), by = holdout_strat]
  combined_dt_temp <- create_folds(collapsed_temp, n_folds)
  combined_dt_temp <- merge(combined_dt, combined_dt_temp[, c(holdout_strat, "holdout"), with = FALSE], by = holdout_strat)
  stopifnot(nrow(combined_dt_temp) == nrow(combined_dt))
  combined_dt <- combined_dt_temp
}

###### Save prepped data
setkeyv(combined_dt, c("area", "year", "sex", "race", "age", "source", strat_vars))
save(graph_j, graph_t, graph_a, graph_e, graph_as, graph_ts, s_age, s_year, file = paste0(output_dir, "/re_graphs.RData"))

###### Now run code that was previously embedded in fit_mod scripts
#### Add intercept and define num variables
combined_dt[, int := 1L]

#### Calculate num variables
num_s <- nrow(recodes$sex)
num_j <- nrow(recodes$area)
num_t <- nrow(recodes$year)
num_a <- nrow(recodes$age)
num_r <- nrow(recodes$race)
num_d <- nrow(recodes$source)
num_dr <- nrow(recodes$source_race)
num_v <- nrow(recodes$survey_version)
num_dr <- nrow(recodes$source_race)
num_e <- nrow(recodes$edu)
num_m <- nrow(recodes$marital)
num_f <- nrow(recodes$phone)
num_g <- nrow(recodes$gq)
num_w <- nrow(recodes$state)

if (is.null(num_e)) {
  num_e <- 0
}

if (is.null(num_m)) {
  num_m <- 0
}

#### Save num variables
save(num_age_spline, num_year_spline, num_s, num_j, num_t, num_a, num_r, num_d, num_dr, num_v, num_e, num_m, num_f, num_g, num_w, file = paste0(output_dir, "/num_vars.RData"))

#### Set agg_id counter object
last_agg_id <- -1

###### Now deal with aggregate inputs
if ("BRFSS" %in% source_levels) {
  combined_dt_brfss <- combined_dt[source == "BRFSS"]
  
  ###### Separate mcnty and state rows
  dt_mcnty <- combined_dt_brfss[!is.na(area)] # Prioritize cbsa_mcnty_code over area if both are set
  dt_cbsa_mcnty <- combined_dt_brfss[is.na(area) & !is.na(cbsa_mcnty_code)]
  dt_state <- combined_dt_brfss[!is.na(state) & is.na(area) & is.na(cbsa_mcnty_code)]
  if (nrow(dt_mcnty) + nrow(dt_cbsa_mcnty) + nrow(dt_state) != nrow(combined_dt_brfss)) {
    stop("The row counts of mcnty-, puma_mcnty-, and state-level data do not match the total row count of the full data set. Please check this.")
  }
  
  dt_mcnty[, agg_wt := 1]
  dt_mcnty[, level := "mcnty"]
  
  ##########################
  ###### Read in mcnty:state aggregation weights
  if (dt_state[, .N] > 0) { # skip if everyone is assigned an mcnty or cbsa_mcnty_code (note that respondents in the state remainder get a cbsa_mcnty_code)
    mcnty_state_agg_wts <- readRDS(paste0("FILEPATH"))
    
    ###### Check race codes for weights data
    validate_race_codes(DT = mcnty_state_agg_wts,
                        race_set_id = 1, # specifies DB code
                        includes_all_race = F, # says not to expect the all-race variable (1)
                        silent = T, # doesn't print anything if the outputs look okay
                        expect_missing_race = c(3)) # tells the function that it's okay if NH Other is missing
    
    if (!identical(sort(c("edu", "marital")), sort(strat_vars))) {
      ##### Collapse to state-mcnty combinations
      mcnty_state_agg_wts <- mcnty_state_agg_wts[, list(pop = sum(pop)), by = c("state", "mcnty", "year", "sex", "age", "race", strat_vars)]
      mcnty_state_agg_wts[, total_pop := sum(pop), by = c("state", "year", "sex", "age", "race", strat_vars)]
      
      ###### Recalculate aggregation weights
      mcnty_state_agg_wts[, agg_wt := pop / total_pop]
    }
    
    ###### Recode variables in state_mcnty data set to ordered integer (0-indexed)
    mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$sex[, -"var"], on = "sex"]
    mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$year[, -"var"], on = "year"]
    mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$age[, -"var"], on = "age"]
    mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$race[, -"var"], on = "race"]
    mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$state[, -"var"], on = "state"]
    
    if ("edu" %in% strat_vars) {
      mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$edu[, -"var"], on = "edu"]
      mcnty_state_agg_wts[, c("edu", "edu_recode") := list(edu_recode, NULL)]
    }
    if ("marital" %in% strat_vars) {
      mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$marital[, -"var"], on = "marital"]
      mcnty_state_agg_wts[, c("marital", "marital_recode") := list(marital_recode, NULL)]
    }  
    if ("phone" %in% strat_vars) {
      mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$phone[, -"var"], on = "phone"]
      mcnty_state_agg_wts[, c("phone", "phone_recode") := list(phone_recode, NULL)]
    }  
    
    mcnty_state_agg_wts$mcnty_state_uid <- 0:(nrow(mcnty_state_agg_wts) - 1)
    
    ###### Clean up recoded values
    mcnty_state_agg_wts[, c("year", "sex", "age", "race", "state") := list(year_recode, sex_recode, age_recode, race_recode, state_recode)]
    mcnty_state_agg_wts[, c("year_recode", "age_recode", "race_recode", "sex_recode", "state_recode") := NULL]
    
    # ###### Set unique IDs
    dt_state$agg_id <- (last_agg_id + 1):(last_agg_id + nrow(dt_state))
    last_agg_id <- max(dt_state$agg_id)
    
    ###### Derive mapping from dt_state to dt_mcnty
    dt_state_combined <- merge(dt_state, mcnty_state_agg_wts, by = c("state", "year", "sex", "race", "age", strat_vars), all.x = TRUE, allow.cartesian = TRUE)
    dt_state_combined <- merge(dt_state_combined, recodes$area, by = "mcnty", all.x = TRUE)
    dt_state_combined[, c("area", "mcnty") := NULL]
    setnames(dt_state_combined, c("area_recode"), c("area"))
    
    ###### Check that dt_state agg_wts sum to 1 and that there are no NA agg_wts
    stopifnot(nrow(dt_state_combined[is.na(agg_wt)]) == 0)
    stopifnot(dt_state_combined[, sum(agg_wt), by = agg_id][, max(abs(V1 - 1))] < 1e-10)  
  }
  
  ##########################
  ###### Read in mcnty:cbsa-mcnty aggregation weights
  mcnty_cbsa_agg_wts <- readRDS(paste0("FILEPATH"))
  
  ###### Check race codes for weights data
  validate_race_codes(DT = mcnty_cbsa_agg_wts,
                      race_set_id = 1, # specifies DB code
                      includes_all_race = F, # says not to expect the all-race variable (1)
                      silent = T, # doesn't print anything if the outputs look okay
                      expect_missing_race = c(3)) # tells the function that it's okay if NH Other is missing
  
  if (!identical(sort(c("edu", "marital")), sort(strat_vars))) {
    ##### Collapse unused post-stratification variables
    mcnty_cbsa_agg_wts <- mcnty_cbsa_agg_wts[, list(pop = sum(pop)), by = c("cbsa_mcnty_code", "mcnty", "year", "sex", "age", "race", strat_vars)]
    
    ###### Recalculate total_pop
    mcnty_cbsa_agg_wts[, total_pop := sum(pop), by = c("cbsa_mcnty_code", "year", "sex", "age", "race", strat_vars)]
    
    ###### Recalculate aggregation weights
    mcnty_cbsa_agg_wts[, agg_wt := pop / total_pop]
  }
  
  ###### recodes variables in mcnty_cbsa data set to ordered integer (0-indexed)
  mcnty_cbsa_agg_wts <- mcnty_cbsa_agg_wts[recodes$sex[, -"var"], on = "sex"]
  mcnty_cbsa_agg_wts <- mcnty_cbsa_agg_wts[recodes$year[, -"var"], on = "year"]
  mcnty_cbsa_agg_wts <- mcnty_cbsa_agg_wts[recodes$age[, -"var"], on = "age"]
  mcnty_cbsa_agg_wts <- mcnty_cbsa_agg_wts[recodes$race[, -"var"], on = "race"]
  
  if ("edu" %in% strat_vars) {
    mcnty_cbsa_agg_wts <- mcnty_cbsa_agg_wts[recodes$edu[, -"var"], on = "edu"]
    mcnty_cbsa_agg_wts[, c("edu", "edu_recode") := list(edu_recode, NULL)]
  }
  if ("marital" %in% strat_vars) {
    mcnty_cbsa_agg_wts <- mcnty_cbsa_agg_wts[recodes$marital[, -"var"], on = "marital"]
    mcnty_cbsa_agg_wts[, c("marital", "marital_recode") := list(marital_recode, NULL)]
  }  
  if ("phone" %in% strat_vars) {
    mcnty_cbsa_agg_wts <- mcnty_cbsa_agg_wts[recodes$phone[, -"var"], on = "phone"]
    mcnty_cbsa_agg_wts[, c("phone", "phone_recode") := list(phone_recode, NULL)]
  }
  
  mcnty_cbsa_agg_wts$mcnty_cbsa_uid <- 0:(nrow(mcnty_cbsa_agg_wts) - 1)
  
  ###### Clean up recoded values
  mcnty_cbsa_agg_wts[, c("year", "sex", "age", "race") := list(year_recode, sex_recode, age_recode, race_recode)]
  mcnty_cbsa_agg_wts[, c("year_recode", "age_recode", "race_recode", "sex_recode") := NULL]
  
  ###### Set unique IDs
  dt_cbsa_mcnty$agg_id <- (last_agg_id + 1):(last_agg_id + nrow(dt_cbsa_mcnty))
  last_agg_id <- max(dt_cbsa_mcnty$agg_id)
  
  ###### Derive mapping from dt_cbsa_mcnty to dt_mcnty
  dt_cbsa_mcnty_combined <- merge(dt_cbsa_mcnty[, -c("state")], mcnty_cbsa_agg_wts, by = c("cbsa_mcnty_code", "year", "sex", "race", "age", strat_vars), all.x = TRUE)
  dt_cbsa_mcnty_combined <- merge(dt_cbsa_mcnty_combined, recodes$area, by = "mcnty", all.x = TRUE)
  dt_cbsa_mcnty_combined[, c("area", "mcnty") := NULL]
  setnames(dt_cbsa_mcnty_combined, c("area_recode"), c("area"))
  
  ###### Check that agg_wts sum to 1
  stopifnot(dt_cbsa_mcnty_combined[, sum(agg_wt), by = agg_id][, max(abs(V1 - 1))] < 1e-10)
  
  ###### Combine aggregate rows
  if(dt_state[, .N] > 0){
    dt_agg_rows <- rbindlist(list(dt_cbsa_mcnty_combined, dt_state_combined), use.names = TRUE, fill = TRUE)  
  } else{
    dt_agg_rows <- dt_cbsa_mcnty_combined
  }
  
  
  ###### Check that agg_wts in combined data table sum to 1 (this should be redundant, but checks for improperly-replicated agg_ids)
  stopifnot(dt_agg_rows[, sum(agg_wt), by = agg_id][, max(abs(V1 - 1))] < 1e-10)
  
  ###### Sort aggregate data
  setkeyv(dt_agg_rows, "agg_id")
  
  ###### Set levels
  dt_mcnty[, level := "mcnty"]
  dt_agg_rows[, level := "aggregate"]
  
  ###### Combined data sets
  combined_dt_brfss <- rbindlist(list(dt_mcnty, dt_agg_rows), use.names = TRUE, fill = TRUE)
  
  
  ##########################
  ########## Deal with age 80+ observations (from 2013 onward, treat age bin 80 as an aggregate of 80-84 and 85+)
  dt_age_80 <- combined_dt_brfss[source == "BRFSS" & age >= recodes$age[age == 80, age_recode] & year >= recodes$year[year == 2013, year_recode]]
  
  if (nrow(dt_age_80) > 0) {
    if (!exists("post_stratification_wt_file_under20")) {
      pop <- readRDS(post_stratification_wt_file)  
    } else {
      if (!is.null(post_stratification_wt_file_under20) & any(ages < 20)) {
        pop <- rbindlist(list(readRDS(post_stratification_wt_file), readRDS(post_stratification_wt_file_under20)), use.names = TRUE, fill = TRUE)
      } else {
        pop <- readRDS(post_stratification_wt_file)  
      }
    }
    
    ###### Check race codes for weights data
    validate_race_codes(DT = pop,
                        race_set_id = 1, # specifies DB code
                        includes_all_race = F, # says not to expect the all-race variable (1)
                        silent = T, # doesn't print anything if the outputs look okay
                        expect_missing_race = c(3)) # tells the function that it's okay if NH Other is missing
    
    pop <- pop[year %in% recodes$year[year_recode %in% unique(dt_age_80$year), year] & age >= 80]
    setnames(pop, "value", "pop")
    
    #### Add state
    if (is.null(pop$state)) {
      pop <- merge(pop, unique(locs[, list(mcnty, state)]), by = "mcnty", all.x = TRUE)
    }
    
    ##### Collapse across unused stratifying variable combinations and calculate agg_wts among ages
    if (!all.equal(strat_vars, c("edu", "marital"))) {
      pop <- pop[, list(pop = sum(pop)), by = c("state", "mcnty", "year", "sex", "age", "race", strat_vars)]
    }
    pop[, total_pop := sum(pop), by = c("state", "mcnty", "year", "sex", "race", strat_vars)]
    
    ###### Calculate agg_wts
    pop[, agg_wt2 := pop / total_pop]
    
    #### Set post-stratification weights for strata with zero aggregation weight, using cascade strategy
    # Set raw weights
    pop[, wt := NULL]
    pop[, c("wt0", "wt") := list(agg_wt2, agg_wt2)]
    
    # Second level of the cascade (drop year)
    pop[, pop2 := sum(pop), by = c("mcnty", "state", "age", "race", "sex", "edu", "marital")]
    pop[, agg_pop2 := sum(pop), by = c("mcnty", "state", "race", "sex", "edu", "marital")]
    pop[, wt2 := pop2 / agg_pop2]
    
    # Third level of the cascade (drop year and mcnty)
    pop[, pop3 := sum(pop), by = c("state", "age", "race", "sex", "edu", "marital")]
    pop[, agg_pop3 := sum(pop), by = c("state", "race", "sex", "edu", "marital")]
    pop[, wt3 := pop3 / agg_pop3]
    
    # Fourth level of the cascade (drop year and race)
    pop[, pop4 := sum(pop), by = c("state", "sex", "edu", "marital", "age")]
    pop[, agg_pop4 := sum(pop), by = c("state", "sex", "edu", "marital")]
    pop[, wt4 := pop4 / agg_pop4]
    
    # Fifth level of the cascade (drop year, race, and sex)
    pop[, pop5 := sum(pop), by = c("state", "age", "edu", "marital")]
    pop[, agg_pop5 := sum(pop), by = c("state", "edu", "marital")]
    pop[, wt5 := pop5 / agg_pop5]
    
    pop[total_pop >= 20, "use_version" := 1]
    pop[total_pop < 20, c("agg_wt2", "use_version") := list(wt2, 2)]
    pop[agg_pop2 < 20, c("agg_wt2", "use_version") := list(wt3, 3)]
    pop[agg_pop3 < 20, c("agg_wt2", "use_version") := list(wt4, 4)]
    pop[agg_pop4 < 20, c("agg_wt2", "use_version") := list(wt5, 5)]
    
    stopifnot(nrow(pop[is.na(agg_wt2)]) == 0)
    stopifnot(nrow(pop[agg_pop5 < 20]) == 0)
    stopifnot(pop[, sum(agg_wt2), by = c("mcnty", "state", "year", "sex", "race", "edu", "marital")][, max(abs(V1 - 1))] < 1e-10)
    
    ###### Recode variables in pop to ordered integer (0-indexed)
    pop <- pop[recodes$sex[, -"var"], on = "sex"]
    pop <- pop[recodes$year[, -"var"], on = "year"]
    pop <- pop[recodes$age[, -"var"], on = "age"]
    pop <- pop[recodes$race[, -"var"], on = "race"]
    pop <- pop[recodes$area[, -"var"], on = "mcnty"]
    pop <- pop[recodes$state[, -"var"], on = "state"]
    
    if ("edu" %in% strat_vars) {
      pop <- pop[recodes$edu[, -"var"], on = "edu"]
      pop[, c("edu", "edu_recode") := list(edu_recode, NULL)]
    }
    if ("marital" %in% strat_vars) {
      pop <- pop[recodes$marital[, -"var"], on = "marital"]
      pop[, c("marital", "marital_recode") := list(marital_recode, NULL)]
    }
    if ("phone" %in% strat_vars) {
      pop <- pop[recodes$phone[, -"var"], on = "phone"]
      pop[, c("phone", "phone_recode") := list(phone_recode, NULL)]
    }
    
    ###### Clean up recoded values
    pop[, c("year", "sex", "age", "race", "area", "state") := list(year_recode, sex_recode, age_recode, race_recode, area_recode, state_recode)]
    pop[, c("year_recode", "age_recode", "race_recode", "sex_recode", "area_recode", "state_recode") := NULL]
    
    #### Cleanup
    pop[, c("wt", "wt0", "pop_total", "total_pop", "value_age_pooled", "pop2", "agg_pop2", "wt2", "pop3", "agg_pop3", "wt3", "pop4", "agg_pop4", "wt4", "pop5", "agg_pop5", "wt5") := NULL]
    
    #### Set agg_id for mcnty-level rows
    dt_age_80_mcnty <- dt_age_80[level == "mcnty"]
    dt_age_80_mcnty[, agg_id := (last_agg_id + 1):(last_agg_id + nrow(dt_age_80_mcnty))]
    last_agg_id <- max(dt_age_80_mcnty$agg_id)
    dt_age_80 <- rbindlist(list(dt_age_80[level == "aggregate"], dt_age_80_mcnty), use.names = TRUE, fill = TRUE)
    
    ###### Replicate dt_age_80 for age_85
    dt_age_85 <- copy(dt_age_80)
    dt_age_85[, age := recodes$age[age == 85, age_recode]]
    
    ###### Combine ages 80 and 85
    dt_age_80_85 <- rbindlist(list(dt_age_80, dt_age_85), use.names = TRUE, fill = TRUE)
    
    ###### Merge on populations
    dt_age_80_85 <- merge(dt_age_80_85, pop[, agg_wt2, by = c("race", "age", "sex", "year", "area", strat_vars)], by = c("area", "year", "sex", "age", "race", strat_vars), all.x = TRUE)
    
    ###### Combine agg_wt variables
    dt_age_80_85[, agg_wt := agg_wt * agg_wt2]
    dt_age_80_85[, level := "aggregate"]
    
    ###### Add back to remaining data rows
    total <- rbindlist(list(combined_dt_brfss[!(source == "BRFSS" & age >= recodes$age[age == 80, age_recode] & year >= recodes$year[year == 2013, year_recode])], dt_age_80_85[, -c("agg_wt2")]), use.names = TRUE, fill = TRUE)
    stopifnot(nrow(total) == nrow(combined_dt_brfss) + nrow(dt_age_80)) # Check that the number of rows is correct
  } else {
    total <- copy(combined_dt_brfss)
  }
  
  ###### Check that agg_wts sum to 1
  stopifnot(total[!is.na(agg_id), sum(agg_wt), by = agg_id][, max(abs(V1 - 1))] < 1e-10)
  
  ###### Drop rows with agg_wt == 0
  total <- total[agg_wt != 0]
  
  ###### Rename and clean up
  combined_dt_brfss <- copy(total)
  rm(total)
} else {
  combined_dt_brfss <- NULL
}

if ("Gallup" %in% source_levels) {
  combined_dt_gallup <- combined_dt[source == "Gallup"]
  combined_dt_gallup[, agg_wt := 1]
  combined_dt_gallup[!is.na(area), level := "mcnty"]
  combined_dt_gallup$agg_id <- NA
} else {
  combined_dt_gallup <- NULL
}

if ("ACS" %in% source_levels) {
  combined_dt_acs <- combined_dt[source == "ACS"]
  combined_dt_acs[, area := NULL] # Drop area (all NAs)
  
  ###### Separate mcnty, puma_mcnty, and state rows
  dt_puma_mcnty <- combined_dt_acs[!is.na(puma_mcnty)]
  if (nrow(dt_puma_mcnty) != nrow(combined_dt_acs)) {
    stop("The row counts of mcnty- and puma_mcnty-level data do not match the total row count of the full data set. Please check this.")
  }
  
  ###### Load PUMA:mcnty crosswalk
  puma_mcnty_crosswalk <- readRDS(paste0("FILEPATH"))
  
  ###### Check race codes for weights data
  validate_race_codes(DT = puma_mcnty_crosswalk,
                      race_set_id = 1, # specifies DB code
                      includes_all_race = F, # says not to expect the all-race variable (1)
                      silent = T, # doesn't print anything if the outputs look okay
                      expect_missing_race = c(3)) # tells the function that it's okay if NH Other is missing
  
  if (!identical(sort(c("edu", "marital")), sort(strat_vars))) {
    ##### Collapse unused post-stratification variables
    puma_mcnty_crosswalk <- puma_mcnty_crosswalk[, list(pop = sum(pop)), by = c("puma_mcnty", "mcnty", "year", "sex", "age", "race", strat_vars)]
    
    ###### Recalculate total_pop
    puma_mcnty_crosswalk[, total_pop := sum(pop), by = c("puma_mcnty", "year", "sex", "age", "race", strat_vars)]
    
    ###### Recalculate aggregation weights
    puma_mcnty_crosswalk[, agg_wt := pop / total_pop]
  }
  
  ## Restrict to requested races, sexes, ages, and years
  puma_mcnty_crosswalk <- puma_mcnty_crosswalk[(race %in% races) & (sex %in% sexes) & (age %in% ages) & (year %in% years)]
  
  ###### Recode variables in puma_mcnty data set to ordered integer (0-indexed)
  puma_mcnty_crosswalk <- puma_mcnty_crosswalk[recodes$sex[, -"var"], on = "sex"]
  puma_mcnty_crosswalk <- puma_mcnty_crosswalk[recodes$year[, -"var"], on = "year"]
  puma_mcnty_crosswalk <- puma_mcnty_crosswalk[recodes$age[, -"var"], on = "age"]
  puma_mcnty_crosswalk <- puma_mcnty_crosswalk[recodes$race[, -"var"], on = "race"]
  puma_mcnty_crosswalk <- puma_mcnty_crosswalk[recodes$area[, -"var"], on = "mcnty"]

  if ("edu" %in% strat_vars) {
    puma_mcnty_crosswalk <- puma_mcnty_crosswalk[recodes$edu[, -"var"], on = "edu"]
    puma_mcnty_crosswalk[, c("edu", "edu_recode") := list(edu_recode, NULL)]
  }
  if ("marital" %in% strat_vars) {
    puma_mcnty_crosswalk <- puma_mcnty_crosswalk[recodes$marital[, -"var"], on = "marital"]
    puma_mcnty_crosswalk[, c("marital", "marital_recode") := list(marital_recode, NULL)]
  }
  
  ###### Clean up recodes values
  puma_mcnty_crosswalk[, c("mcnty", "year", "sex", "age", "race") := list(area_recode, year_recode, sex_recode, age_recode, race_recode)]
  puma_mcnty_crosswalk[, c("year_recode", "age_recode", "race_recode", "area_recode", "sex_recode") := NULL]
  
  ###### Create a unique PUMA-mcnty ID
  puma_mcnty_crosswalk[, puma_mcnty_uid := paste0(puma_version, "_", puma_mcnty)]
  
  ###### Set unique IDs
  dt_puma_mcnty$agg_id <- (last_agg_id + 1):(last_agg_id + nrow(dt_puma_mcnty))
  last_agg_id <- max(dt_puma_mcnty$agg_id)
  
  ###### Derive mapping from dt_puma_mcnty to dt_mcnty
  dt_puma_mcnty_combined <- merge(dt_puma_mcnty, puma_mcnty_crosswalk, by = c("puma_version", "puma_mcnty", "year", "sex", "race", "age", strat_vars), all.x = TRUE, allow.cartesian = TRUE)
  setnames(dt_puma_mcnty_combined, c("mcnty"), c("area"))
  
  ###### Combine aggregate rows
  dt_agg_rows <- dt_puma_mcnty_combined
  
  ###### Set levels
  # dt_mcnty[, level := "mcnty"]
  dt_agg_rows[, level := "aggregate"]
  
  ###### Combine aggregate rows with mcnty rows
  combined_dt_acs <- dt_agg_rows
  
  ###### Drop rows with agg_wt == 0
  combined_dt_acs <- combined_dt_acs[agg_wt != 0]
  
  ###### Check that agg_wts sum to 1
  stopifnot(combined_dt_acs[, sum(agg_wt), by = agg_id][, max(abs(V1 - 1))] < 1e-10)
} else {
  combined_dt_acs <- NULL
}

if (any(c("mdcr", "max", "HCUP", "mdcr_eligibility") %in% data_file$name)) { # Clinical data, except MarketScan
  combined_dt_clinical <- combined_dt[source %in% c("mdcr", "max", "HCUP", "mdcr_eligibility")]
  
  ###### Check for rows without either mcnty or state identifiers
  stopifnot(nrow(combined_dt_clinical[(is.na(area) & is.na(state))]) == 0)
  
  ###### Separate mcnty and state rows
  dt_mcnty <- combined_dt_clinical[!is.na(area)] # Retain mcnty-level data
  dt_state <- combined_dt_clinical[!is.na(state) & is.na(area)]
  if (nrow(dt_mcnty) + nrow(dt_state) != nrow(combined_dt_clinical)) {
    stop("The row counts of mcnty-, puma_mcnty-, and state-level data do not match the total row count of the full data set. Please check this.")
  }
  
  if (nrow(dt_state) > 0) {
    ###### Read in mcnty:state aggregation weights
    mcnty_state_agg_wts <- readRDS(paste0("FILEPATH"))
    
    ###### Check race codes for weights data
    validate_race_codes(DT = mcnty_state_agg_wts,
                        race_set_id = 1, # specifies DB code
                        includes_all_race = F, # says not to expect the all-race variable (1)
                        silent = T, # doesn't print anything if the outputs look okay
                        expect_missing_race = c(3)) # tells the function that it's okay if NH Other is missing
    
    mcnty_state_agg_wts$age <- as.integer(as.character(mcnty_state_agg_wts$age))
    
    ##### Collapse to state-mcnty combinations
    mcnty_state_agg_wts <- mcnty_state_agg_wts[, list(pop = sum(pop)), by = c("state", "mcnty", "year", "sex", "age", "race")]
    mcnty_state_agg_wts[, total_pop := sum(pop), by = c("state", "year", "sex", "age", "race")]
    
    ###### Recalculate aggregation weights
    mcnty_state_agg_wts[, agg_wt := pop / total_pop]
    # agg_wts fail when the total population in a given mcnty-year-sex-race-age is 0.
    # In these cases, we use the following hierarchy of backups: cbsa_mcnty_code-year-race-age (no sex),
    # mcnty-year-age (no sex or race), mcnty-year (no sex, age, or race), and mcnty (no sex, age, race, or year)
    
    ###### Recode variables in mcnty_state data set to ordered integer (0-indexed)
    mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$sex[, -"var"], on = "sex"]
    mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$year[, -"var"], on = "year"]
    mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$age[, -"var"], on = "age"]
    mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$race[, -"var"], on = "race"]
    mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$state[, -"var"], on = "state"]
    mcnty_state_agg_wts$mcnty_state_uid <- 0:(nrow(mcnty_state_agg_wts) - 1)
    
    ###### Clean up recoded values
    mcnty_state_agg_wts[, c("year", "sex", "age", "race", "state") := list(year_recode, sex_recode, age_recode, race_recode, state_recode)]
    mcnty_state_agg_wts[, c("year_recode", "age_recode", "race_recode", "sex_recode", "state_recode") := NULL]
    
    ###### Set unique IDs
    dt_state$agg_id <- (last_agg_id + 1):(last_agg_id + nrow(dt_state))
    if (nrow(dt_state) > 0) {
      last_agg_id <- max(dt_state$agg_id)
    }
    
    ###### Derive mapping from dt_state to dt_mcnty
    dt_state_combined <- merge(dt_state, mcnty_state_agg_wts, by = c("state", "year", "sex", "race", "age"), all.x = TRUE, allow.cartesian = TRUE)
    dt_state_combined[, c("area") := NULL]
    setnames(dt_state_combined, c("mcnty"), c("area"))
    
    #### Drop state-level data, if requested
    if (exists("drop_state_level_data")) {
      if (drop_state_level_data) {
        dt_state_combined <- NULL
      }
    }
    
    ###### Combine aggregate rows
    dt_agg_rows <- dt_state_combined
    
    ###### Reset pop
    # First set tiny population for zero-population strata
    dt_agg_rows[total_pop == 0, total_pop := 1e-12]
    dt_agg_rows[, pop := total_pop * agg_wt]
    
    ###### Check that agg_wts sum to 1
    if (nrow(dt_agg_rows) > 0) {
      stopifnot(dt_agg_rows[, sum(agg_wt), by = agg_id][, max(abs(V1 - 1))] < 1e-10)
    }
    
    dt_agg_rows[, level := "aggregate"]
  } else {
    dt_agg_rows <- NULL
  }
  
  ###### Set levels
  dt_mcnty[, level := "mcnty"]
  
  ###### Combine aggregate rows with mcnty rows
  combined_dt_clinical <- rbindlist(list(dt_mcnty, dt_agg_rows), use.names = TRUE, fill = TRUE)
  
  ###### Set agg_wt to 1 for mcnty-level data
  combined_dt_clinical[level == "mcnty", agg_wt := 1]
  combined_dt_clinical[level == "mcnty", agg_id := NA]
  
  ###### Drop rows with agg_wt == 0
  combined_dt_clinical <- combined_dt_clinical[agg_wt != 0]
  
  ###### Drop rows with sample_size == 0
  combined_dt_clinical <- combined_dt_clinical[sample_size > 0]
  
  ###### Ensure that sample_size >= cases
  combined_dt_clinical[cases > sample_size, cases := sample_size]
} else {
  combined_dt_clinical <- NULL
}

if (any(c("MarketScan") %in% data_file$name)) { # MarketScan (all-race, state-level)
  combined_dt_marketscan <- combined_dt[source %in% c("MarketScan")]
  
  ###### Check for rows without either mcnty or state identifiers
  stopifnot(nrow(combined_dt_marketscan[(is.na(area) & is.na(state))]) == 0)
  
  ###### Separate mcnty and state rows
  dt_mcnty <- combined_dt_marketscan[!is.na(area)] # Retain mcnty-level data
  dt_state <- combined_dt_marketscan[!is.na(state) & is.na(area)]
  if (nrow(dt_mcnty) + nrow(dt_state) != nrow(combined_dt_marketscan)) {
    stop("The row counts of mcnty-, puma_mcnty-, and state-level data do not match the total row count of the full data set. Please check this.")
  }
  
  if (nrow(dt_state) > 0) {
    ###### Read in mcnty:state aggregation weights
    mcnty_state_agg_wts <- readRDS(paste0("FILEPATH"))
    
    ###### Check race codes for weights data
    validate_race_codes(DT = mcnty_state_agg_wts,
                        race_set_id = 1, # specifies DB code
                        includes_all_race = F, # says not to expect the all-race variable (1)
                        silent = T, # doesn't print anything if the outputs look okay
                        expect_missing_race = c(3)) # tells the function that it's okay if NH Other is missing
    
    mcnty_state_agg_wts$age <- as.integer(as.character(mcnty_state_agg_wts$age))
    
    ##### Collapse to state-mcnty combinations
    mcnty_state_agg_wts <- mcnty_state_agg_wts[, list(pop = sum(pop)), by = c("state", "mcnty", "year", "sex", "age", "race")]
    mcnty_state_agg_wts[, total_pop := sum(pop), by = c("state", "year", "sex", "age")] # Drop race, since MarketScan is all-race
    
    ###### Recalculate aggregation weights
    mcnty_state_agg_wts[, agg_wt := pop / total_pop]
    # agg_wts fail when the total population in a given mcnty-year-sex-race-age is 0.
    # In these cases, we use the following hierarchy of backups: cbsa_mcnty_code-year-race-age (no sex),
    # mcnty-year-age (no sex or race), mcnty-year (no sex, age, or race), and mcnty (no sex, age, race, or year)
    
    ###### Recode variables in mcnty_state data set to ordered integer (0-indexed)
    mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$sex[, -"var"], on = "sex"]
    mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$year[, -"var"], on = "year"]
    mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$age[, -"var"], on = "age"]
    mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$race[, -"var"], on = "race"]
    mcnty_state_agg_wts <- mcnty_state_agg_wts[recodes$state[, -"var"], on = "state"]
    mcnty_state_agg_wts$mcnty_state_uid <- 0:(nrow(mcnty_state_agg_wts) - 1)
    
    ###### Clean up recoded values
    mcnty_state_agg_wts[, c("year", "sex", "age", "race", "state") := list(year_recode, sex_recode, age_recode, race_recode, state_recode)]
    mcnty_state_agg_wts[, c("year_recode", "age_recode", "race_recode", "sex_recode", "state_recode") := NULL]
    
    ###### Set unique IDs
    dt_state$agg_id <- (last_agg_id + 1):(last_agg_id + nrow(dt_state))
    if (nrow(dt_state) > 0) {
      last_agg_id <- max(dt_state$agg_id)
    }
    
    ###### Derive mapping from dt_state to dt_mcnty
    dt_state_combined <- merge(dt_state[, -c("race")], mcnty_state_agg_wts, by = c("state", "year", "sex", "age"), all.x = TRUE, allow.cartesian = TRUE)
    dt_state_combined[, c("area") := NULL]
    setnames(dt_state_combined, c("mcnty"), c("area"))
    
    #### Drop state-level data, if requested
    if (exists("drop_state_level_data")) {
      if (drop_state_level_data) {
        dt_state_combined <- NULL
      }
    }
    
    ###### Combine aggregate rows
    dt_agg_rows <- dt_state_combined
    
    ###### Reset pop
    # First set tiny population for zero-population strata
    dt_agg_rows[total_pop == 0, total_pop := 1e-12]
    dt_agg_rows[, pop := total_pop * agg_wt]
    
    ###### Check that agg_wts sum to 1
    if (nrow(dt_agg_rows) > 0) {
      stopifnot(dt_agg_rows[, sum(agg_wt), by = agg_id][, max(abs(V1 - 1))] < 1e-10)
    }
    
    dt_agg_rows[, level := "aggregate"]
  } else {
    dt_agg_rows <- NULL
  }
  
  ###### Set levels
  dt_mcnty[, level := "mcnty"]
  
  ###### Combine aggregate rows with mcnty rows
  combined_dt_marketscan <- rbindlist(list(dt_mcnty, dt_agg_rows), use.names = TRUE, fill = TRUE)
  
  ###### Set agg_wt to 1 for mcnty-level data
  combined_dt_marketscan[level == "mcnty", agg_wt := 1]
  combined_dt_marketscan[level == "mcnty", agg_id := NA]
  
  ###### Drop rows with agg_wt == 0
  combined_dt_marketscan <- combined_dt_marketscan[agg_wt != 0]
  
  ###### Drop rows with sample_size == 0
  combined_dt_marketscan <- combined_dt_marketscan[sample_size > 0]
  
  ###### Ensure that sample_size >= cases
  combined_dt_marketscan[cases > sample_size, cases := sample_size]
} else {
  combined_dt_marketscan <- NULL
}

if ("NHANES" %in% source_levels) {
  combined_dt_nhanes <- combined_dt[source == "NHANES"]
  dt_natl <- copy(combined_dt_nhanes)
  
  ##########################
  ###### Read in mcnty:natl aggregation weights
  mcnty_natl_agg_wts <- readRDS(paste0("FILEPATH"))
  
  ###### Check race codes for weights data
  validate_race_codes(DT = mcnty_natl_agg_wts,
                      race_set_id = 1, # specifies DB code
                      includes_all_race = F, # says not to expect the all-race variable (1)
                      silent = T, # doesn't print anything if the outputs look okay
                      expect_missing_race = c(3)) # tells the function that it's okay if NH Other is missing
  
  mcnty_natl_agg_wts$age <- as.integer(as.character(mcnty_natl_agg_wts$age))
  
  ##### Restrict to model years
  mcnty_natl_agg_wts <- mcnty_natl_agg_wts[year %in% years]
  
  ###### Create svyyear field (two-year NHANES cycles)
  mcnty_natl_agg_wts[year %in% 2009:2010, svyyear := "2009_2010"]
  mcnty_natl_agg_wts[year %in% 2011:2012, svyyear := "2011_2012"]
  mcnty_natl_agg_wts[year %in% 2013:2014, svyyear := "2013_2014"]
  mcnty_natl_agg_wts[year %in% 2015:2016, svyyear := "2015_2016"]
  mcnty_natl_agg_wts[year %in% 2017:2018, svyyear := "2017_2018"]
  mcnty_natl_agg_wts <- mcnty_natl_agg_wts[!is.na(svyyear)] # Drop years without svyyear (for now this is just 2019)
  
  ###### Create simplified race variable to match NHANES (Other race = NH API + NH AIAN)
  mcnty_natl_agg_wts[race == 1, "race_NHANES" := "NH White Only"]
  mcnty_natl_agg_wts[race == 2, "race_NHANES" := "NH Black Only"]
  mcnty_natl_agg_wts[race == 7, "race_NHANES" := "Hispanic"]
  mcnty_natl_agg_wts[race %in% 3:4, "race_NHANES" := "other"]
  stopifnot(nrow(mcnty_natl_agg_wts[is.na(race_NHANES)]) == 0)
  
  ##### Collapse to natl-mcnty combinations
  mcnty_natl_agg_wts <- mcnty_natl_agg_wts[, list(pop = sum(pop)), by = c("mcnty", "year", "svyyear", "sex", "age", "race", "race_NHANES", strat_vars)]
  
  ###### Combine ages 80 and 85 (NHANES is top-coded as 80+ from 2007 onward)
  mcnty_natl_agg_wts[, "temp_age" := age]
  mcnty_natl_agg_wts[age %in% 80:85, temp_age := 80]
  
  ###### Recalculate aggregation weights
  mcnty_natl_agg_wts[, total_pop := sum(pop), by = c("svyyear", "sex", "temp_age", "race_NHANES", strat_vars)]
  mcnty_natl_agg_wts[, agg_wt := pop / total_pop]
  
  stopifnot(nrow(mcnty_natl_agg_wts[is.na(agg_wt)]) == 0)
  stopifnot(mcnty_natl_agg_wts[, sum(agg_wt), by = c("svyyear", "sex", "temp_age", "race_NHANES", strat_vars)][, max(abs(V1 - 1))] < 1e-10)
  
  ###### Recode variables in natl_mcnty data set to ordered integer (0-indexed)
  mcnty_natl_agg_wts <- mcnty_natl_agg_wts[recodes$sex[, -"var"], on = "sex"]
  mcnty_natl_agg_wts <- mcnty_natl_agg_wts[recodes$year[, -"var"], on = "year"]
  mcnty_natl_agg_wts <- mcnty_natl_agg_wts[recodes$age[, -"var"], on = "age"]
  mcnty_natl_agg_wts <- mcnty_natl_agg_wts[recodes$race[, -"var"], on = "race"]
  if ("edu" %in% strat_vars) {
    mcnty_natl_agg_wts <- mcnty_natl_agg_wts[recodes$edu[, -"var"], on = "edu"]
    mcnty_natl_agg_wts[, c("edu", "edu_recode") := list(edu_recode, NULL)]
  }
  if ("marital" %in% strat_vars) {
    mcnty_natl_agg_wts <- mcnty_natl_agg_wts[recodes$marital[, -"var"], on = "marital"]
    mcnty_natl_agg_wts[, c("marital", "marital_recode") := list(marital_recode, NULL)]
  }
  
  mcnty_natl_agg_wts$mcnty_natl_uid <- 0:(nrow(mcnty_natl_agg_wts) - 1)
  
  ###### Clean up recoded values
  mcnty_natl_agg_wts[, c("year", "sex", "age", "race") := list(year_recode, sex_recode, age_recode, race_recode)]
  mcnty_natl_agg_wts[, c("year_recode", "age_recode", "race_recode", "sex_recode") := NULL]
  
  ###### Repeat for temp_age
  mcnty_natl_agg_wts[, "original_age_recode" := age]
  mcnty_natl_agg_wts[, age := temp_age]
  mcnty_natl_agg_wts <- mcnty_natl_agg_wts[recodes$age[, -"var"], on = "age"]
  mcnty_natl_agg_wts <- mcnty_natl_agg_wts[age != 85] # Drop the one row of age 85 introduced by merge
  mcnty_natl_agg_wts[, c("temp_age") := list(age_recode)]
  mcnty_natl_agg_wts[, c("age_recode") := NULL]
  mcnty_natl_agg_wts[, c("age") := list(temp_age)]
  
  ###### Set unique IDs
  dt_natl$agg_id <- (last_agg_id + 1):(last_agg_id + nrow(dt_natl))
  last_agg_id <- max(dt_natl$agg_id)
  
  ## For strata under age 20, set edu and marital to a false value, just to facilitate the merge (the agg_wts shouldn't be dependent on edu or marital for these ages)
  dt_natl[age < recodes$age[age == 20, age_recode], c("edu", "marital") := list(0, 0)]
  
  ##### Derive mapping from national to dt_mcnty
  dt_natl_combined <- merge(dt_natl[, -c("year", "race")], mcnty_natl_agg_wts, by = c("svyyear", "sex", "race_NHANES", "age", strat_vars), all.x = TRUE, allow.cartesian = TRUE)
  dt_natl_combined <- merge(dt_natl_combined, recodes$area, by = "mcnty", all.x = TRUE)
  dt_natl_combined[, c("area", "mcnty") := NULL]
  setnames(dt_natl_combined, c("area_recode"), c("area"))
  
  ##### Drop rows with NA strat_vars
  if (!is.null(strat_vars)) {
    dt_natl_combined <- dt_natl_combined[complete.cases(dt_natl_combined[, ..strat_vars])] 
  }
  
  ##### Set age to disaggregated age for 80+
  dt_natl_combined[, c("age") := list(original_age_recode)]
  
  ###### Restrict to requested races, sexes, ages and years
  dt_natl_combined <- dt_natl_combined[((race %in% recodes$race[race %in% races, race_recode]) & (sex %in% recodes$sex[sex %in% sexes, sex_recode]) & (age %in% recodes$age[age %in% ages, age_recode]) & (year %in% recodes$year[year %in% years, year_recode]))]
  
  ###### Check that dt_natl agg_wts sum to 1 and that there are no NA agg_wts
  stopifnot(nrow(dt_natl_combined[is.na(agg_wt)]) == 0)
  stopifnot(dt_natl_combined[, sum(agg_wt), by = agg_id][, max(abs(V1 - 1))] < 1e-10)
  
  dt_natl_combined[, level := "aggregate"]
  
  ###### Drop rows with agg_wt == 0
  dt_natl_combined <- dt_natl_combined[agg_wt != 0]
  
  ###### Rename and clean up
  combined_dt_NHANES <- copy(dt_natl_combined)
  rm(dt_natl_combined)
} else {
  combined_dt_NHANES <- NULL
}

#### Combine source-specific data sets
combined_dt <- rbindlist(list(combined_dt_brfss, combined_dt_gallup, combined_dt_acs, combined_dt_clinical, combined_dt_marketscan, combined_dt_NHANES), use.names = TRUE, fill = TRUE)
combined_dt[, c("race_NHANES", "post_stratify_by_phone") := NULL]

###### Check that agg_wts sum to 1
if (nrow(combined_dt[!is.na(agg_id)]) > 0) {
  stopifnot(combined_dt[!is.na(agg_id), sum(agg_wt), by = agg_id][, max(abs(V1 - 1))] < 1e-10)
}

if (!is.null(cov)) {
  ##### Recode covariates
  cov <- merge(cov, recodes$race[, -"var"], by = "race", all.x = TRUE)
  cov <- merge(cov, recodes$age[, -"var"], by = "age", all.x = TRUE)
  cov <- merge(cov, recodes$sex[, -"var"], by = "sex", all.x = TRUE)
  cov <- merge(cov, recodes$year[, -"var"], by = "year", all.x = TRUE)
  cov <- merge(cov, recodes$area[, -"var"], by.x = "area", by.y = "mcnty", all.x = TRUE)
  cov[, c("race", "age", "sex", "year", "area", "race_recode", "age_recode", "sex_recode", "year_recode", "area_recode") := list(race_recode, age_recode, sex_recode, year_recode, area_recode, NULL, NULL, NULL, NULL, NULL)]
  
  #### Save covariates to disk for later use
  saveRDS(cov, file = paste0(output_dir, "/covariates.rds"))
  
  ## Merge covariates onto data set
  combined_dt <- merge(combined_dt, cov, by = c("area", "year", "sex", "race", "age"), all.x = TRUE)
}

###### Make sure that state indicators are present for all rows
combined_dt <- merge(combined_dt, recodes$area[, -c("var")], by.x = "area", by.y = "area_recode", all.x = TRUE)
combined_dt[, c("state", "state_name") := NULL]
combined_dt <- merge(combined_dt, unique(locs[, c("mcnty", "state", "state_name")]), by.x = "mcnty", by.y = "mcnty", all.x = TRUE)
combined_dt[, "mcnty" := NULL]
combined_dt <- merge(combined_dt, recodes$state[, -c("var")], by.x = "state", by.y = "state", all.x = TRUE)
combined_dt[, c("state", "state_recode") := list(state_recode, NULL)]

###### Save data set
saveRDS(combined_dt, file = paste0(output_dir, "/data.rds"))

###### If this is a k-fold validation run, copy prepped inputs to each fold's folder
if (type == "validation" & n_folds > 1) {
  for (i in 1:n_folds) {
    file.copy(paste0(output_dir, "/data.rds"), c(paste0(output_dir, "/fold_", i, "/data.rds")), overwrite = TRUE)
    file.copy(paste0(output_dir, "/covariates.rds"), c(paste0(output_dir, "/fold_", i, "/covariates.rds")), overwrite = TRUE)
    file.copy(paste0(output_dir, "/re_graphs.RData"), c(paste0(output_dir, "/fold_", i, "/re_graphs.RData")), overwrite = TRUE)
    file.copy(paste0(output_dir, "/num_vars.RData"), c(paste0(output_dir, "/fold_", i, "/num_vars.RData")), overwrite = TRUE)
    file.copy(paste0(output_dir, "/recode.rds"), c(paste0(output_dir, "/fold_", i, "/recode.rds")), overwrite = TRUE)
    file.copy(paste0(output_dir, "/data_pre_factor.rds"), c(paste0(output_dir, "/fold_", i, "/data_pre_factor.rds")), overwrite = TRUE)
    file.copy(paste0(output_dir, "/settings.csv"), c(paste0(output_dir, "/fold_", i, "/settings.csv")), overwrite = TRUE)
    file.copy(paste0(output_dir_draws_est, "/population.rds"), c(paste0(output_dir_draws_est, "/fold_", i, "/population.rds")), overwrite = TRUE)
  }
}

###### End
print("Script completed")