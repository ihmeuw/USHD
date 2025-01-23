####################################################################################################
## Description: Generate all draws from the model are simultaneously
##              so that the correlation structure is appropriately reflected. Save the random effect
##              draws, fixed effect draws, and mean object (names/order of the effects).
##
## Passed args: dir [character] -- home directory for settings and final output
##              sex [integer] -- sex to generate predictions for
##              race [integer] -- race to generate predictions for
##              edu [integer] -- educational attainment group to generate predictions for
##              validate [logical] -- is this a validation model? if so, only mx draws for
##                areas in the validation set are created
##              resub [logical] -- are you resubmitting? if yes, it will not re-save files that already
##                exist (or where the final file with all draws exists)
##
## Requires:    fitted model object ("[dir]/model_fit_[sex]_[race].rds")
##              prepped data 
##              file specifying areas in the validation set, if validate is T (gs_file)
##              populations (pop_file)
##              age standard file (age_std_file)
##
## Outputs:     random effect draws: [dir]/initial_sims_[sex]_[race]_[edu].rds
##              fixed effect draws: [dir]/fe_sims_[sex]_[race]_[edu].rds
##              labels for the REs: [dir]/mean_[sex]_[race]_[edu].rds
##
####################################################################################################

stopifnot(grepl("mortality/sae_models$", getwd()))

library(R.utils)
library(data.table)
library(Matrix)
library(splines)
library(dplyr)
sourceDirectory("functions/", modifiedOnly=F)

set.seed(98121)

## Get settings ------------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]
sex <- as.integer(args[2])
race <- as.integer(args[3])
edu <- as.integer(args[4])
validate <- as.logical(args[5])
resub <- as.logical(args[6])

get_settings(dir)

current_dir <- gsub("FILEPATH", "", dir)
lu_root <- paste0("FILEPATH",LU_folder,"FILEPATH")
lu_modeldir <- paste0(lu_root,"/",current_dir)

## Load data and model fit -------------------------------------------------------------------------
data <- readRDS(paste0(lu_modeldir, "/data.rds"))

if("race_agg" %in% names(data)) {
  data <- data[race_agg == "granular"] # get rid of aggregate data in case this messes up this estimation
  
  data[,count := .N, by=c("area","year","sex","race","age","edu")]
  stopifnot(nrow(data[count > 1]) == 0)
  data[,count := NULL]

  # reset the key
  setkeyv(data, c("area", "year", "sex", "race", "edu", "age"))
}


if(is.null(race_together)) race_together <- F
if(is.null(edu_together)) edu_together <- F

# read in the model
out <- readRDS(paste0(dir, "/model_fit_", sex, "_", race, "_", edu, ".rds"))  # all files will have both edu and race

# get the data
if(!("sex_fit" %in% names(data))) {
  # to accomodate cases where we did not set up sex_fit yet (but the models are already running)
  data[,sex_fit := sex]
}


data <- data[sex_fit == get("sex", .GlobalEnv) & race_fit == get("race", .GlobalEnv) & edu_fit == get("edu", .GlobalEnv), ]

# If this is for education, it is necessary to remove unknown education. Non-reporting state-years
# were removed from the data temporarily before model fitting, in order to avoid fitting the model
# to unknown education data. This is is to ensure that we
# still have every county, sex, age, year, and education group that we want to model for. That is,
# we dropped non-reporting state years from fitting, but we still want to predict for non-reporing
# state years, so, we have to have them present. That is why the code below checks for square
# dimensions.
if (by_edu) {
  data <- data[edu != 100, ]

  # check that data is square. ie, it has all the things we want to predict for: all the ages,
  # sexes, years, locations, and education groups that we want results for.

  # naive test:
  num_expected_dims <- length(unique(ages)) * length(unique(years)) * length(unique(sex)) * dim(readRDS(adjmat_file))[1] * length(unique(edu_groups)) 
  num_existing_dims <- nrow(data)
  stopifnot(num_expected_dims == num_existing_dims)

  # full test: (values must have same encoding as data)
  expected_dims <-
    data.table(
      expand.grid(
        age = as.integer(factor(ages, levels = ages)) - 1L,
        year = as.integer(years - years[1]),
        sex = get("sex", .GlobalEnv),
        area = unique(readRDS(geoagg_files["natl"])[[area_var]]),
        edu = edu_groups
      )
    )

  all_equal_result <- all.equal(
    target = expected_dims,
    current =  data[, list(area, year, sex, age, edu)],
    ignore.col.order = T,
    ignore.row.order = T
  )

  message(glue::glue("all.equal() expected dims result: {all_equal_result}"))

  stopifnot(all_equal_result)

}

# the number of races is equal to the unique number of races once we have subsetted race_fit
num_r <- length(unique(data$race))
# and same for education
num_e <- length(unique(data$edu))
# and for sexes
num_s <- length(unique(data$sex))

# Get the rest of the values for the number of variables
num_j <- max(data$area) + 1
num_t <- max(data$year) + 1
num_a <- max(data$age) + 1
num_c <- 2

# modify this specific covariate: prop_1989_revision This covariate is an
# "offset". in the fitting phase, we want to account for the effect of a state
# changing death certificate versions. so, we have this data, which acts like a
# covariate which should pick up that effect in the fitting phase. Here in the
# prediction phase, we want to remove that effect. To do this, we set the data
# to zero. That way the effect will not contribute to the prediction, and the
# effect of changing death certificate versions should be removed. The setting
# zero_covar_offset controls whether or not we want to zero it out.
if (!is.null(covars_offset)) {
  if (covars_offset %in% names(data) & zero_covar_offset) {
    data[, get("covars_offset") := 0]
    message(glue::glue("Zeroing out {covars_offset}"))
  }
  message(glue::glue("{covars_offset} is present but will not be zeroed out."))
}

# If a validation model, subset the data so that predictions are generated only for the validation
# set. However, keep all race/ethnicities or educational attainment groups in the counties that are
# in the validation set. That is, keep all the counties in the validation set.
if (validate) {
  gs_mx <- readRDS(gs_file)
  gs_mx <- gs_mx[year %in% years, ]
  # drop race and edu because we want to keep all races in counties that have at least one race in
  # the validation set
  gs_mx[, `:=`(race = NULL, edu = NULL)]
  # get rid of duplicates in the counties that had more than one race
  gs <- unique(gs_mx[sex == get("sex", .GlobalEnv),
                     list(area = get(area_var))])
  
  data <- merge(gs, data, by = c("area"), all.x = T)
  stopifnot(!any(is.na(data)))
  setkeyv(data, c("area", "year", "sex"))  # spline model 3 fails if the key does not include sex
  rm(gs, gs_mx)
  gc()
}

## Generate mx draws -------------------------------------------------------------------------------
simvars <- paste0("V", 1:n.sims)

if(resub & file.exists(paste0(dir, "/fe_sims_", sex, "_", race, "_", edu, ".rds")) &
   file.exists(paste0(dir, "/initial_sims_", sex, "_", race, "_", edu, ".rds")) &
   file.exists(paste0(dir, "/mean_", sex, "_", race, "_", edu, ".rds"))) {

  message("Skipping simulation because draws already exist and resubmission specified")

} else {
  
  if (fixed_age_time) {
    re1 <- rbindlist(lapply(years, function(this_year) {
      cat(paste0(this_year, "\n"))
      if(race_together) {
        
        draws <- rbindlist(lapply(races, function(this_race) {
          draws <- readRDS(paste0(ref_dir, "/mx_draws_", ref_level, "_", this_year, "_", sex, "_", this_race, if (ref_raked) "_raked", ".rds"))
          draws <- draws[CJ(ref_area, unique(year), unique(sex), ages),
                         list(year = year - min(years), age = as.numeric(as.factor(age)) - 1, sim, mx = log(mx))]
          draws <- dcast.data.table(draws, year + age ~ sim, value.var = "mx")
          # add race variable by indexing the races and then matching this_race to its correct index
          draws[, race := (1:num_r-1)[match(this_race, races)]]
          setnames(draws, c("year", "age", "race", simvars))
        }))
      } else {  # Else, race_together is FALSE
        draws <- readRDS(paste0(ref_dir, "/mx_draws_", ref_level, "_", this_year, "_", sex, "_", race, if (ref_raked) "_raked", ".rds"))
        draws <- draws[CJ(ref_area, unique(year), unique(sex), ages),
                       list(year = year - min(years), age = as.numeric(as.factor(age)) - 1, sim, mx = log(mx))]
        draws <- dcast.data.table(draws, year + age ~ sim, value.var = "mx")
        setnames(draws, c("year", "age", simvars))
      }
      draws
    }))
    setkeyv(re1, c("age", "year"))

    if (model %in% c("1b", "2b")) {
      re1[, age := NULL]
      setkeyv(re1, "year")
    }
  } # end of if(fixed_age_time)

  # extract mean and precision matrix for all model parameters, and align the ordering of the mean
  # vector to match the ordering of the precision matrix
  prec <- out$jointPrecision
  mean <- c(out$par.random, out$par.fixed)
  mean <- unlist(lapply(unique(rownames(prec)), function(x) mean[names(mean) == x]))
  stopifnot(all.equal(rownames(prec), names(mean)))
  
  message("Generating draws from the posterior...")

  # draw from a multivariate normal distribution given the mean and precision matrix for all parameters
  # then subset to those required for constructing predictions
  gen_sims <- function(mu, prec, n.sims) {
    z = matrix(rnorm(length(mu) * n.sims), ncol = n.sims)
    L_inv = Cholesky(prec)
    mu + solve(as(L_inv, "pMatrix"), solve(t(as(L_inv, "Matrix")), z))
  }
  sims <- gen_sims(mu = mean, prec = prec, n.sims = n.sims)
  sims <- sims[grepl("^B|^re|^covar_subpop|^covar_offset", names(mean)), ]
  mean <- mean[grepl("^B|^re|^covar_subpop|^covar_offset", names(mean))]
  rm(prec, gen_sims); gc()

  saveRDS(sims, paste0(dir, "/initial_sims_", sex, "_", race, "_", edu, ".rds"))
  saveRDS(mean, paste0(dir, "/mean_", sex, "_", race, "_", edu, ".rds"))

  # calculate draws for the fixed portion of the model (same for all models)
  message("Working on fixed effects (fe)...")

  # if using a fixed age-time global effect, load the relevant draws for that component of the prediction
  # (note: we're calling this re1, but it is actually the equivalent of B0 + re1, i.e., it includes the intercept)
  if (fixed_age_time) { # in this case, there's no intercept (it's absorbed in re1)
    if (is.null(covars) & is.null(covars_as) & is.null(covars_subpop)) {
      fe <- matrix(0, nrow = nrow(data), ncol = n.sims)
    } else {
      
      # and B_offset will be picked up if they are present.
      B <- sims[grepl("^B", names(mean)), ]
      if (class(B) == "numeric") B <- matrix(B, nrow = 1) else B <- as.matrix(B)

      
      if("B1" %in% names(mean)) {
        if (by_edu) {
          stop("A section of pred_mx.r is running that hasn't been tested. This section is not expected to run.")
        }
        B_0 <- sims[grepl("^B", names(mean)) & !grepl("^B1", names(mean)), ]
        fe_0 <- as.matrix(data[, c( covars, covars_as), with = F]) %*% B_0
        data[, index := .I] # add index to help with merges
        B_1 <- sims[grepl("^B1", names(mean)), ]
        for(r in unique(data$race)) {
          message(paste0("Building matrix for race ", r))
          fe_1_temp <- as.matrix(data[race == r, c(covars_subpop, "index"), with = F]) %*% rbind(B_1[c(0, r+1), ], matrix(0, ncol=n.sims))
          fe_1_temp <- cbind(fe_1_temp, data[race == r]$index)
          if(r == 0) {
            fe_1 <- fe_1_temp
          } else {
            fe_1 <- rbind(fe_1, fe_1_temp)
          }
        }
        # reset the order of the rows based on the index
        fe_1 <- fe_1[order(fe_1[, n.sims+1]), ]
        fe_1 <- fe_1[, -c(n.sims+1)]

        fe <- fe_0 + fe_1
        fe <- as.matrix(fe)

      } else { # Else, "B1" is NOT in the columns of mean
        fe <- as.matrix(data[, c(covars, covars_as, covars_subpop), with = F]) %*% B
      }
    }
  } else { # Else, fixed_age_time is FALSE
    
    
    # and B_offset will be picked up if they are present. There are separate if
    # sections below to handle B1, B_offset. If either of these are present then
    # the variable B will not be used.
    B <- sims[grepl("^B", names(mean)), ] # create / pull out B
    if (class(B) == "numeric") B <- matrix(B, nrow = 1) else B <- as.matrix(B)

    data[, int := 1]

    if("B1" %in% names(mean)) {
      
      if (by_edu) {
        stop("A section of pred_mx.r is running that hasn't been tested. This section is not expected to run.")
      }
      message("B1 is in the names of mean...")

      B_0 <- sims[grepl("^B", names(mean)) & !grepl("^B1", names(mean)), ]

      fe_0 <- as.matrix(data[, c("int", covars, covars_as), with = F]) %*% B_0 # This is analogous to the normal B * X

      data[, index := .I] # add index to help with merges
      B_1 <- sims[grepl("^B1", names(mean)), ]
      for(r in unique(data$race)) {
        message(paste0("Building matrix for race ", r))
        fe_1_temp <- as.matrix(data[race == r, c(covars_subpop, "index"), with = F]) %*% rbind(B_1[c(0, r+1), ], matrix(0, ncol=n.sims))
        fe_1_temp <- cbind(fe_1_temp, data[race == r]$index)
        if(r == 0) {
          fe_1 <- fe_1_temp
        } else {
          fe_1 <- rbind(fe_1, fe_1_temp)
        }
      }
      # reset the order of the rows based on the index
      fe_1 <- fe_1[order(fe_1[, n.sims+1]), ]
      fe_1 <- fe_1[, -c(n.sims+1)]

      fe <- fe_0 + fe_1
      fe <- as.matrix(fe)

      rm(B_1); gc()

    } else if ("B_offset" %in% names(mean) | "log_B_offset" %in% names(mean)) {
      message("B_offset is in the names of mean...")
      message(glue::glue("by_edu is set to {by_edu}. by_race is set to {by_race}."))

      # B_offset is a fixed effect specifically for a offset covariate,
      # representing the average education effect for all states and ages.
      # excluding B1 isn't needed at this point but it protects against future
      # code changes
      
      # This code doesn't need to run per se, except to run tests that everything is the right shape,
      # and to test that fe_offset is in fact fully zero if zero_covar_offset is set to TRUE.

      # Work on the normal fixed effect / B
      # recall that this is for everything (by-county, by-race, by-edu) and it's not edu-specific.
      message("Working on B (a.k.a. B_0)...")

      grepl_index = grepl("^B", names(mean)) &
        !grepl("^B_offset", names(mean)) &
        !grepl("^B1", names(mean)) # make boolean index that can be re-used

      stopifnot(unique(names(mean)[grepl_index]) == "B") # check that the grepl worked
      B_0 <- sims[grepl_index, ]
      rm(grepl_index)

      message("Multiplying B_O with data to create fe_0...")
      fe_0 <- as.matrix(data[, c("int", covars, covars_as, covars_subpop), with = F]) %*% B_0 # includes covars_subpop

      data[, index := .I] # adding this index helps with merges

      # Now work on the Offset fixed effect
      message("Working on B_offset...")

      grepl_index = grepl("^B_offset|^log_B_offset", names(mean))
      if("B_offset" %in% names(mean)) {
        stopifnot(unique(names(mean)[grepl_index]) == "B_offset") # check that the grepl worked
      } else if ("log_B_offset" %in% names(mean)) {
        stopifnot(unique(names(mean)[grepl_index]) == "log_B_offset") # check that the grepl worked
      }

      B_offset <- sims[grepl_index, ] # pull out B_offset or log_B_offset from sims

      if (by_edu) {
        stopifnot(nrow(B_offset) == num_e)

        edu_map = data.table(edu_index = c(1:num_e), edu = sort(edu_groups))

        # loop over rows of B_offset, which correspond to edu groups. Use row index to map to edu
        # value
        for (idx in c(1:num_e)) {
          e = edu_map[edu_index == idx, edu]
          message(glue::glue("Working on B_offset for education group e = {e}, index = {idx}..."))
          message(glue::glue("Multiplying B_offset by data to make fe_offset_temp for education group e = {e}, index = {idx}..."))
          fe_offset_temp <- as.matrix(data[edu == e, c(covars_offset), with = F]) %*% B_offset[idx, ]
          fe_offset_temp <- cbind(fe_offset_temp, data[edu == e]$index) # append fe_offset_temp to the matrix

          # check the index
          stopifnot(length(fe_offset_temp[, n.sims + 1]) == nrow(data) / num_e) # index was placed in the (n.sims + 1)th column

          if (idx == 1) { # handle first loop where fe_offset won't exist already
            fe_offset <- fe_offset_temp
          } else {
            fe_offset <- rbind(fe_offset, fe_offset_temp) # if not on first loop, then fe_offset already exists can can be appended to
          }
          rm(fe_offset_temp)
        } # end loop over edu
        message("Done looping over edu groups for B_offset. fe_offset has been created.")

        # reset the order of the rows based on the index. Need to do this because we subset data to
        # one edu at a time while making fe_offset, so if we didn't re-sort fe_offset its order would
        # not match that of data. order fe_offset according to the column named n.sims + 1 (eg 101,
        # 1001), essentially giving it a row order that matches that of the data
        fe_offset <- fe_offset[order(fe_offset[, n.sims + 1]), ]
        fe_offset <- fe_offset[, -c(n.sims + 1)] # drop the index column

        # create a matrix filled with zeros that is the same shape as fe_offset
        zeros <- matrix(0, nrow = nrow(fe_offset), ncol = ncol(fe_offset))
        # test if the matrix of zeros is the same as fe_offset.
        zeros_test = all.equal(target = zeros, current = fe_offset)
        # only assert that the test passes if the setting zero_covar_offset is TRUE
        if (zero_covar_offset) { 
          stopifnot(zeros_test)
        }
        
        # end by_edu branch 
      } else if (by_race) {

        stopifnot(nrow(B_offset) == num_r-2) # exclude the NH Multiracial and Hispanic groups

        # loop over rows of B_offset, which correspond to races.
        # However, B_offset does not apply to the Multiracial group, so we do not want to evaluate the covariate for that group

        race_map <- data.table(race = races, race_label = race_labels)
        race_map <- setorderv(race_map, "race") # order these because the race IDs may not be in numerical order
        race_map[,r_index := .I]

        for (idx in race_map$r_index) {

          r = race_map[r_index == idx, race]
          message(glue::glue("Working on B_offset for race/ethnicity r = {r}, index = {idx}..."))

          if(race_map[r_index == idx, race_label] %in% c("NH Multiracial", "Hispanic")) {

            message("Creating a matrix of 0s for the offset fixed effect, for NH Multiracial or Hispanic")

            # Now, we have to add on draws of 0 to the end of fe_offset for the Multiracial and Hispanic groups
            other_data <- data[race == r]$index
            matrix_0 <- matrix(0, ncol = n.sims, nrow = length(other_data))
            fe_offset_temp <- cbind(matrix_0, other_data)

          } else {

            message("Not NH Multiracial or Hispanic, so getting draws of offset fixed effect")
            # have to subtract 1 from the index because Hispanic is not included and it is the first index
            
            if(model %like% "beta") {
              fe_offset_temp <- as.matrix(data[race == r, c(covars_offset), with = F]) %*% exp(B_offset[(idx-1), ])
            } else {
              fe_offset_temp <- as.matrix(data[race == r, c(covars_offset), with = F]) %*% B_offset[(idx-1), ]
            }

            fe_offset_temp <- cbind(fe_offset_temp, data[race == r]$index) # append fe_offset_temp to the matrix

          }

          # check the index
          stopifnot(length(fe_offset_temp[, n.sims + 1]) == nrow(data) / num_r) # index was placed in the (n.sims + 1)th column

          if (idx == 1) { # handle first loop where fe_offset won't exist already
            fe_offset <- fe_offset_temp
          } else {
            fe_offset <- rbind(fe_offset, fe_offset_temp) # if not on first loop, then fe_offset already exists can can be appended to
          }
          rm(fe_offset_temp)
        } # end loop over races
        message("Done looping over races groups for B_offset. fe_offset has been created.")

        # reset the order of the rows based on the index. Need to do this because we subset data to
        # one race/ethnicity at a time while making fe_offset, so if we didn't re-sort fe_offset its order would
        # not match that of data. order fe_offset according to the column named n.sims + 1 (eg 101,
        # 1001), essentially giving it a row order that matches that of the data
        fe_offset <- fe_offset[order(fe_offset[, n.sims + 1]), ]
        fe_offset <- fe_offset[, -c(n.sims + 1)] # drop the index column

      } # end by_race branch (there is no else)

      # combine the fixed effect for the offset with the other fixed effect.
      fe <- fe_0 + fe_offset
      fe <- as.matrix(fe)

      # done with all fixed effects for the case when offset was used.
      rm(B_offset, fe_offset); gc()

    } else { # Else, "B1" and "B_offset" is NOT in the columns of mean

      # B was created above
      message("B1 and B_offset are not present. Using B to make fe...")
      fe <- as.matrix(data[, c("int", covars, covars_as, covars_subpop), with = F]) %*% B
    }
  }
  rm(B); gc()
  message("Done making fe.")
  message("saving...")
  saveRDS(fe, paste0(dir, "/fe_sims_", sex, "_", race, "_", edu, ".rds"))
  message("Done.")
}
