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
##              prepped data file ("[FILEPATH]/data.rds")
##              file specifying areas in the validation set, if validate is T (gs_file)
##              populations (pop_file)
##              age standard file (age_std_file)
##
## Outputs:     random effect draws: [dir]/initial_sims_[sex]_[race]_[edu].rds
##              fixed effect draws: [dir]/fe_sims_[sex]_[race]_[edu].rds
##              labels for the REs: [dir]/mean_[sex]_[race]_[edu].rds
##
####################################################################################################

library(R.utils)
library(data.table)
library(Matrix)
library(splines)
library(dplyr)
sourceDirectory("functions/")

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

if(is.null(race_together)) race_together <- F
if(is.null(edu_together)) edu_together <- F

# read in the model
out <- readRDS(paste0(dir, "/model_fit_", sex, "_", race, "_", edu, ".rds"))

# get the data
data <- data[sex == get("sex", .GlobalEnv) & race_fit == get("race", .GlobalEnv) & edu_fit == get("edu", .GlobalEnv), ]

# the number of races is equal to the unique number of races once we have subsetted race_fit
num_r <- length(unique(data$race))
# and same for education
num_e <- length(unique(data$edu))

# Get the rest of the values for the number of variables
num_j <- max(data$area) + 1
num_t <- max(data$year) + 1
num_a <- max(data$age) + 1

# if a validation model, subset the data so that predictions are generated only for the validation set
# However, keep all race/ethnicities in the counties that are in the validation set
if (validate) {
  load(gs_file)
  gs_mx <- gs_mx[year %in% years, ]
  gs_mx[, race := NULL]  # drop race because we want to keep all races in counties that have at least one race in the validation set
  gs_mx <- unique(gs_mx)  # get rid of duplicates in the counties that had more than one race
  gs <- unique(gs_mx[sex == get("sex", .GlobalEnv), list(area = get(area_var), year = year - years[1])])
  data <- merge(gs, data, by = c("area", "year"), all.x = T)
  stopifnot(!any(is.na(data)))
  setkeyv(data, c("area", "year", "sex"))  # spline model 3 fails if the key does not include sex
  rm(gs, gs_ex, gs_mx); gc()
}

## Generate mx draws -------------------------------------------------------------------------------
simvars <- paste0("V", 1:n.sims)

if(resub & file.exists(paste0(dir, "/fe_sims_", sex, "_", race, "_", edu, ".rds")) &
   file.exists(paste0(dir, "/initial_sims_", sex, "_", race, "_", edu, ".rds")) &
   file.exists(paste0(dir, "/mean_", sex, "_", race, "_", edu, ".rds"))) {
  
  message("Skipping simulation because draws already exist and resubmission specified")
  
} else {
  
  # extract mean and precision matrix for all model parameters, and align the ordering of the mean
  # vector to match the ordering of the precision matrix
  prec <- out$jointPrecision
  mean <- c(out$par.random, out$par.fixed)
  mean <- unlist(lapply(unique(rownames(prec)), function(x) mean[names(mean) == x]))
  stopifnot(all.equal(rownames(prec), names(mean)))
  
  # draw from a multivariate normal distribution given the mean and precision matrix for all parameters
  # then subset to those required for constructing predictions
  gen_sims <- function(mu, prec, n.sims) {
    z = matrix(rnorm(length(mu) * n.sims), ncol = n.sims)
    L_inv = Cholesky(prec)
    mu + solve(as(L_inv, "pMatrix"), solve(t(as(L_inv, "Matrix")), z))
  }
  sims <- gen_sims(mu = mean, prec = prec, n.sims = n.sims)
  sims <- sims[grepl("^B|^re|^covar_subpop", names(mean)), ]
  mean <- mean[grepl("^B|^re|^covar_subpop", names(mean))]
  rm(prec, gen_sims); gc()
  
  saveRDS(sims, paste0(dir, "/initial_sims_", sex, "_", race, "_", edu, ".rds"))
  saveRDS(mean, paste0(dir, "/mean_", sex, "_", race, "_", edu, ".rds"))
  
  # Deal with the covariates
  B <- sims[grepl("^B", names(mean)), ]
  if (class(B) == "numeric") B <- matrix(B, nrow = 1) else B <- as.matrix(B)
  data[, int := 1]
  
  fe <- as.matrix(data[, c("int", covars, covars_as, covars_subpop), with = F]) %*% B
  
  rm(B); gc()
  saveRDS(fe, paste0(dir, "/fe_sims_", sex, "_", race, "_", edu, ".rds"))
}