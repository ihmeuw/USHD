####################################################################################################
## Inputs:    Compiled microdata
##            settings: FILEPATH
##              Settings file specifies the model formula, stratification variables,
##              whether to run by-source or not, etc.
##            
## Outputs:   FILEPATH
## 
## Description: 
##
##  Fit a BMI correction model after binning self-reported and measured data by 
##   quantile of self-reported BMI and demographic variables. 
##  Specifically, create matched-pairs of BRFSS/NHANES & Gallup/NHANES data, matched by
##    demographic specified in the settings file and self-reported quantile. Within
##    each subset, calculate mean BMI and standard error for each source and other
##    stratification variables.
##  Each pair is an observation that we can use to fit an INLA model.
##  Apply the model to the self-reported data to predict measured BMI.
##
##  Note that this is loosely similar to the quantile-based correction Ward et al.
##   perform in https://www.nejm.org/doi/full/10.1056/NEJMsa1909301 and the 
##   matched-pair approach used to estimate crosswalk coefficients in GBD (not the
##   GBD 2023 BMI crosswalk, but the general approach to crosswalking on GBD). 
## 
##   A key difference is that we are comparing self-reported and measured BMI, 
##   groups by self-reported BMI quantiles (and other demographics) exclusively. 
##   E.g., we're comparing the BMI of people in the xxth percentile of SR BMI in 
##   BRFSS to the *measured* BMI of people in the 10th percentile of *self-reported*
##   BMI in NHANES. This achieves a similar goal to our quantile-correction CW
##   in that we assume people with the same self-reported BMI percentile are similar
##   across surveys, even though we know that self-reported BMI is different in 
##   NHANES than in the phone surveys b/c of interview mode.
##         
####################################################################################################

################################################################################
# Set up
################################################################################

if (dir.exists(paste0('FILEPATH', Sys.info()['user']))) {
  repo <- paste0('FILEPATH')
} else {
  repo <- paste0('FILEPATH')
}

library(data.table)
library(ggplot2)
require(Hmisc)
library(INLA)
library(splines)
library(survey)

source(paste0(repo, "0_functions/get_db_results.R"))
source(paste0(repo, "../non_fatal/sae_models/functions/settings.R"))
source(paste0(repo, "2_exposure/high_bmi/correction_models/model_based_cw/inla_formulas.R")) # source formulas
## read in from command args
if(interactive()){
  output_dir = "FILEPATH"
} else{
  args <- commandArgs(TRUE)
  output_dir <- args[[1]]
}
print(output_dir)

## read settings
get_settings(paste0(output_dir, "/settings.csv"))

################################################################################
# Load inputs
################################################################################

# load compiled microdata
micro_meta <- get_compile_microdata(get_best = T)
data <- readRDS(paste0(micro_meta$output_file_path, "/prepped_bmi_xwalk_micro.rds"))

# Prep data
make_fct <- unique(c(strat_vars, "race_eth_code", "age10", "sex", "survey"))
data[, (make_fct) := lapply(.SD, factor), .SDcols = make_fct]

# exclude rows with NAs in the predictors (missingness for bmi_measure is okay in not NHANES)
data <- data[!(is.na(bmi_report) | is.na(race_eth_code) | is.na(age) | is.na(svyyear) | is.na(sex))]
# exclude NHANES respondents without measured BMI
data <- data[!(survey == "nhanes" & is.na(bmi_measure))]

# filter to respondents who are not pregnant, or pregnant var is NA (e.g., males)
data <- data[is.na(preg) | preg == 0]

# Create a copy of bmi_report and bmi_measure
# Then, limit the bmi_report and bmi_measure columns to 10-55. Do this b/c
# there are more extreme values in BRFSS/Gallup than in NHANES, which leads
# to large ratios of values. I believe this is a difference in how the data 
# are top-coded, and partially a result of rare outcomes (extreme BMIs) in the 
# larger phone surveys.
data[, `:=`(bmi_report_orig = bmi_report, bmi_measure_orig = bmi_measure)]
data[, `:=`(bmi_report = pmax(10, pmin(55, bmi_report)), bmi_measure = pmax(10, pmin(55, bmi_measure)))]
# add a small amount of noise to the top/bottom coded values to avoid ties with 
# the same value (affects quantiles) and issues calculating SE
set.seed(1)
data[bmi_report_orig < 10 | bmi_report_orig > 55, bmi_report := bmi_report + rnorm(.N, 0, 0.01)]
data[bmi_measure_orig < 10 | bmi_measure_orig > 55, bmi_measure := bmi_measure + rnorm(.N, 0, 0.01)]


##############################################################################
# Adjust survey weights ---------------------------------------------------
##############################################################################

# For combining multiple years of survey data, BRFSS and NHANES recommend
# rescaling the weights. Details here: https://wwwn.cdc.gov/nchs/nhanes/tutorials/weighting.aspx
# In short, when combining N waves, scale the weights by 1/N. (There are also notes
# about using the 4-year weights for combining 1999-2000 NHANES with other waves.
# This is already taken care of in the prep script).
# A similar approach is appropriate for BRFSS.
# I will do the weight-scaling later while looping through each subset of the data.
# For Gallup, we need to take an extra step because the weights sum to the sample
# size, not the population size. In the step below, I will scale the weights for each
# year of Gallup to equal the USHD adult population for that year. 

pop <- get_population_data(population_name = "pop_by_race_ethn_1997",
                           year = as.list(1999:2020))
# if there are no rows for 1999, copy the 2000 data and change the year to 1999
# Otherwise, we won't put enough weight on the 1999-2000 survey waves
if(pop[year == 1999, .N] == 0){
  pop1999 <- copy(pop[year == 2000])
  pop1999[, year := 1999]
  pop <- rbind(pop, pop1999, use.names =T)
  setkey(pop, year)
}
# make sure there is pop data for all years
if(!all(1999:2020 %in% pop[, unique(year)])){
  stop("There is not pop data for all years from 1999 to 2020")
}

# calculate the national adult pop size for each year
annual_pop <- pop[age >= 20, .(natl_pop = sum(pop)), year]

data <- unique(annual_pop[, .(year, natl_pop)])[data, on = "year"]

# save the original weights
data[survey != "nhanes", orig_wt := wt] 
data[survey == "nhanes", orig_wt := mec_wt]

# reweight Gallup weights so that they're consistent with the annual pop size
# only do this for Gallup. Other survey weights are already designed to be combined across years
data[survey == "gallup", wt := wt*natl_pop/sum(wt), by = .(survey, year)]

# compare original and adjusted sum of annual weights for Gallup
tmp <- data[survey == "gallup", .(orig_sum_wt = sum(orig_wt), sum_wt = sum(wt)), by = year]
# melt
tmp <- melt(tmp, id.vars = "year")
ggplot(tmp, aes(x = year, y = value)) +
  geom_point() + geom_line() + 
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Comparison of Gallup weights before and after adjustment",
       y = "Sum of weights",
       x = "Year") +
  theme_minimal()

# to facilitate scaling the weights later, make a column that indicates how many years
# are in each svyyear of the data (NHANES only). 
data[survey == "nhanes", `:=`(num_years_wave = 2)]
data[survey == "nhanes" & svyyear == "2017_2020prp", `:=`(num_years_wave = 3.2)]  # See https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/overviewbrief.aspx?Cycle=2017-2020

################################################################################
# handle sources
################################################################################

# if the object use_source_v2 exists (from settings) AND is TRUE, then we will use the source_v2
# instead of source.
# source_v2 splits BRFSS into BRFSS_LANDLINE_ONLY (year <= 2010) and BRFSS_LLCP (year > 2010)

if(exists("use_source_v2") && use_source_v2){
  data[, source_v2 := survey]
  data[year <= 2010 & survey == "brfss", source_v2 := "BRFSS_LANDLINE_ONLY"]
  data[year > 2010 & survey == "brfss", source_v2 := "BRFSS_LLCP"]
} else {
  # otherwise, just make source_v2 the same as survey. This makes it a little 
  # easier in the code later on b/c we can assume source_v2 is a column
  data[, source_v2 := survey]
}

################################################################################
# Calculate quantiles & pairs
################################################################################

# calculated empirical weighted quantile based on self-reported BMI

# Create wrapper around Hmisc::wtd.Ecdf() b/c this function
# returns the ecdf, but it's not evaluated for each row.
# In this wrapper, calculate ecdf using Hmisc, turn it into
# a function so that we can calculate the quantile for each strata
calc_quantile <- function(.x, .weights, .normwt, .na.rm) {
    # error if there are NAs in .x or .weights
  if(any(is.na(.x)) | any(is.na(.weights))){
    stop("There are NAs in the BMI or weights")
  }
  
  # calculate ecdf with Hmisc -- this returns the unique X values and the 
  # CDF for each of those values.
  ecdf <- Hmisc::wtd.Ecdf(
    x = .x, 
    weights = .weights, 
    normwt = .normwt, 
    na.rm = .na.rm,
    type = "i/n") # I like this method b/c ecdf has range [0,1]. FWIW, our results
      # are not sensitive to this choice b/c we bin quantiles.
  # Turn ecdf into a function by interpolation
  ecdf_fun <- approxfun(
    x = ecdf$x, 
    y = ecdf$ecdf, 
    method = "linear", 
    yleft = 0, 
    yright = 1,
    ties = "mean" # affects duplicates, which should only be found for the first two values of ecdf$x. Not very sensitive to choice b/c we bin values, but this is sensible in case that there are very few unique values 
  )
  # calculate the quantile for each observation
  return(ecdf_fun(.x))
}
# Loop through each subset of the data.
# Within that subset, calculate each respondents' quantile in the empirical, 
# weighted self-reported BMI distribution for that source.
strat_vars <- unique(c(strat_vars, age_var, "age20"))

# check that age_var and age20 nest properly
tmp1 <- data[, .N, age_var]
tmp2 <- data[, .N, c(age_var, "age20")]
stopifnot(tmp1[, .N] == tmp2[, .N])

# loop through each set of years of BRFSS/Gallup we're matching to NHANES. This comes
# from the settings.
data_quant <- lapply(1:length(year_subsets), function(i) {
  print(i)
  sub <- year_subsets[[i]]
  # subset data
  tmp <- copy(data[eval(sub)])
  
  # use NHANES weights for subset that had the in-person measurements
  tmp[survey == "nhanes", wt := mec_wt]

  # scale the weights for the subsets.
  # Do this separately for NHANES and the other surveys. (This is because
  # we group a different number of years for NHANES vs the other surveys)

  tmp[survey != "nhanes", `:=`(wt = wt/uniqueN(year), weight_scale_by = uniqueN(year)), by = .(survey, source_v2)]
  tmp[survey == "nhanes", tmp_sum_num_year_wave := sum(unique(num_years_wave))]
  tmp[survey == "nhanes", `:=`(wt = wt * (num_years_wave/tmp_sum_num_year_wave), weight_scale_by = uniqueN(year)), by = .(survey, source_v2)][, tmp_sum_num_year_wave := NULL]

  # check that the wt and orig_wt are within an order of magnitude of each other, 
  # which is a good sanity check that the weights are scaled correctly
  # stopifnot(all(abs(log10(tmp$wt/tmp$orig_wt)) < 1)) -- this won't be true for Gallup
  # check that weights are never larger after combining years
  # stopifnot(all(tmp$wt <= tmp$orig_wt))


  tmp[, bmi_report_percentile := calc_quantile(
      .x = bmi_report,
      .weights = wt,
      .normwt = TRUE,
      .na.rm = FALSE), # shouldn't have NA's
    by = c("age20", "sex", "survey", "source_v2")] # don't stratify by race when calculating quantiles

  # error if there are NAs in bmi_report_percentile
  stopifnot(!any(is.na(tmp$bmi_report_percentile)))

  # collapse the data within each bin of self-reported quantiles
  # Create bins of percentiles, based on the number of quantiles specified in settings
  tmp[, bmi_report_percentile_bin := cut(
      bmi_report_percentile, 
      breaks = seq(0, 1, length.out = num_quantiles + 1),
      include.lowest = T,
      labels = FALSE)
    ]
  tmp[, bmi_report_percentile_bin := as.integer(as.character(bmi_report_percentile_bin))]
  tmp[, subset := i]
  tmp[, year_center := mean(unique(year), na.rm = T), .(survey,source_v2)]
  return(tmp)

})

# Then, collapse the data within each bin of self-reported quantiles and strata. For 
# NHANES, collapse the measured BMI.

# Set options for allowing a single observation per stratum 
message("Starting to collapse pairs")
options(survey.lonely.psu = "adjust") # per guidelines https://www.cdc.gov/brfss/annual_data/2019/pdf/Complex-Smple-Weights-Prep-Module-Data-Analysis-2019-508.pdf
setOption("survey.adjust.domain.lonely", TRUE)
pairs <- lapply(data_quant, function(tmp){ 
  tab_sr <- tmp[survey != "nhanes", .(
    n = .N,
    sum_wt_report = sum(wt),
    mean_bmi_report = weighted.mean(bmi_report, wt),
    se_bmi_report = sqrt(Hmisc::wtd.var(bmi_report, wt, normwt = TRUE))/sqrt(.N) 
    ), 
    by = c(strat_vars, "bmi_report_percentile_bin", "survey", "source_v2", "subset")
  ]

  # calculate first/last year by survey. Do this separate from collapse above b/c
  # sometimes a specific bin/strata won't have data, but we want consistent start/end years
  tab_sr_years <- tmp[survey != "nhanes", .(
    year_start = min(year, na.rm = T), 
    year_end = max(year, na.rm = T),
    year_center = mean(unique(year), na.rm = T)
    ), 
    by = c("survey", "source_v2")
  ]
  tab_sr[tab_sr_years, on = c("survey", "source_v2"), `:=`(year_start = i.year_start, year_end = i.year_end, year_center = i.year_center)]

  # # calculate mean measured BMI, grouped by self-reported BMI 
  tmp_measured <- tmp[survey == "nhanes"]

  # create the design object for NHANES
  # See https://wwwn.cdc.gov/nchs/nhanes/tutorials/VarianceEstimation.aspx
  # NOTE -- by subsetting data in earlier steps, I may underestimate uncertainty
  # See explaination in link above.
  tmp_measured_design <- svydesign(
    id = ~psu_pseudo,
    strata = ~stra_pseudo,
    weights = ~wt,
    data = tmp_measured,
    nest = TRUE # Per doc: "The nest=TRUE option must be used for continuous NHANES data because the unique PSUs are identified by the combination of the strata and the PSU variables (i.e. the PSU identifiers reuse the same values for the PSUs within each stratum.)"
  )
  # calculate mean and SE using survey package
  tab_meas_mean <- svyby(
    formula = ~bmi_measure,
    by = as.formula(paste0("~", paste(unique(c(strat_vars, "bmi_report_percentile_bin", "survey", "source_v2", "subset")), collapse = "+"))),
    design = tmp_measured_design,
    FUN = svymean,
    na.rm = T,
    keep.var = TRUE, # If FUN returns a svystat object, extract standard errors from it
    vartype = "se",
    deff="replace",
    multicore = FALSE
  )
  
  tab_meas_mean <- as.data.table(tab_meas_mean, keep.rownames = F)
  setnames(tab_meas_mean, c("bmi_measure", "se"), c("mean_bmi_measure", "se_bmi_measure"))
  measured_count <- svyby(
    formula = ~bmi_measure,
    by = as.formula(paste0("~", paste(unique(c(strat_vars, "bmi_report_percentile_bin", "survey", "source_v2", "subset")), collapse = "+"))),
    design = tmp_measured_design,
    FUN = unwtd.count,
  )
  measured_count <- as.data.table(measured_count, keep.rownames = F)
  tab_meas_mean[measured_count, 
    on = c(strat_vars, "bmi_report_percentile_bin", "survey", "source_v2", "subset"),
    `:=`(n_measured = i.counts)
  ]
  tab_meas <- tab_meas_mean
  
  setkeyv(tab_sr, c(strat_vars, "bmi_report_percentile_bin"))
  setkeyv(tab_meas, c(strat_vars, "bmi_report_percentile_bin"))
  
  tab <- tab_sr[tab_meas][!is.na(survey)]  
  return(tab)  
})

# data_quant can be used for prediction
data_quant <- rbindlist(data_quant, use.names = T) 
# remove NHANES from data_quant -- we don't make predictions for NHANES
data_quant <- data_quant[survey != "nhanes"]

# replace wt with orig_wt, to un-do the scaling of the weights implemented to 
# collapse across years of data. This does not affect what we put into the INLA model
data_quant[, wt := orig_wt]

# add a unique ID to data_quant
data_quant[, ID := .I]
pairs <- rbindlist(pairs, use.names = T)
# also make an ID for pairs, but make it negative so it's easy to differentiate 
# from the ID in data_quant
pairs[, ID := -.I]

# apply exclusions to the pairs for model fitting (e.g., dropping AIAN/API from Gallup)
# specified in the settings file
pairs <- pairs[!eval(fit_exclusions)]

# remove pairs with NAs or fewer than & report how many
pairs <- pairs[!is.na(mean_bmi_report) & !is.na(mean_bmi_measure)]

if(exists("use_crw2")){
  if(use_crw2 == TRUE){
    stop("CRW2 is no longer supported.")
  }
} else {
  use_crw2 <- FALSE
}

################################################################################
# Compute differences in pairs
################################################################################

# calculate the log-ratio of the mean BMI measure and mean BMI report within
# the pairs
pairs[, ratio := mean_bmi_measure/mean_bmi_report]
pairs[, log_ratio := log(ratio)] # same as diff in log space

##### calculate the standard error of the log_ratio -- need to delta transform
# Let data X ~ N(mu, SD^2)
# Let M = sample mean of X
# When N is large enough,
# M ~ N(mu, SD^2/n)

# Calculate the standard error of the sample mean of log X, i.e., log(M)
# We can derive the approximate standard error of the log sample mean using the
# delta method, where g(x) = log(x)
# sqrt(n) * [g(M) - g(mu)] ~ N(0, sigma^2 * (g(mu)^2)))
# sqrt(n) * [log(M) - log(mu)] ~ N(0, sigma^2 * [d/dx(log(mu))]^2)
# Recall d/dx(log(mu)) = 1/mu
# log(M) - log(mu) ~ N(0, sigma^2/n / mu^2)
# log(M) ~ N(log(mu), sigma^2/n / mu^2)
# Then, plug in Var(M) = sigma^2/n
# SE(log(M)) = sigma/(sqrt(n)*mu) = SE(X)/mean(X)

# In short, the log SE for the sample mean is sample SE/sample mean
# This is consistent with the finding here: https://github.com/ihmeuw-msca/crosswalk/blob/main/src/crosswalk/utils.py#L139-L159
# For more on delta method, see https://web.stanford.edu/class/archive/stats/stats200/stats200.1172/Lecture17.pdf
# And https://scicomp-docs.ihme.washington.edu/msca/book/current/03-metaregression_concepts.html#transforming-the-dependent-variable
pairs[, `:=`(log_se_bmi_measure = se_bmi_measure/mean_bmi_measure,
             log_se_bmi_report = se_bmi_report/mean_bmi_report)]
# lastly, we need to calculate the SE for log_ratio
# Recall that log_ratio = log(mean_bmi_measure/mean_bmi_report) = log(mean_bmi_measure) - log(mean_bmi_report)
# And Var(A +/- B) = Var(A) + Var(B) when independent.
# So Var(log_ratio) = Var(log(mean_bmi_measure)) + Var(log(mean_bmi_report)), i.e.
# se_log_ratio = sqrt(se_log_bmi_measure^2 + se_log_bmi_report^2)
pairs[, se_log_ratio := sqrt(log_se_bmi_measure^2 + log_se_bmi_report^2)]

make_fct <- c("survey", "source_v2", strat_vars)
pairs[, (make_fct) := lapply(.SD, as.factor), .SDcols = make_fct]

################################################################################
# Model formulas
################################################################################
# Create a list of potential model formulas, and save them in a list. 
# This enables us to select the appropriate model formula based on a setting

# set equally-spaced increments for year_center. This addresses the irregular spacing
# of years in the data

year_values <- unique(seq(round(data_quant[, min(year_center)]), round(data_quant[, max(year_center)]), 1))
bmi_report_percentile_bin_vals <- sort(unique(data_quant[, bmi_report_percentile_bin]))
year_values_index <- sort(as.integer(as.factor(year_values)))

# create a range of "restricted" years for natural spline. Used in model 27. 
# Trends outside of bounds will be linear. Use this for setting
# the boundary knots to exclude the early/late NHANES data, which is more suspect
if(exists("time_knot_boundaries")){
  year_boundaries_restricted <- year_values_index[year_values %in% time_knot_boundaries]
  # assert that year_boundaries_restricted is length 2
  stopifnot(length(year_boundaries_restricted) == 2)
}

form <- formulas[[model_num]] # formulas is sourced from ./inla_formulas.R
print(sprintf("using model %s", model_num))
print(form)

# error if use_crw2 is FALSE, but model uses crw2, or vice versa.
if(use_crw2 & !any(grepl("crw2", form))){
  stop("use_crw2 is TRUE, but model does not use crw2")
} else if (!use_crw2 & any(grepl("crw2", form))){
  stop("use_crw2 is FALSE, but model uses crw2")
}

################################################################################
# Fit model with INLA
################################################################################
if(!exists("weight_inla")) {
  stop("Please specify weight_inla in the settings file")
}

# scale the response & SE by scale_by -- this makes the model more stable.
# Will need to divide the predictions by the same amount
scale_by = 100

if(resub && file.exists(paste0(output_dir, "model.rds"))){
  message("resub is TRUE, but model.rds already exists. Reading fodel fit.")
  inla_fits <- readRDS(paste0(output_dir, "model.rds"))
  
} else {
  # fit separately by sex
  inla_fits <- lapply(1:2, function(s){
    message(sprintf("Fitting INLA model for sex %s", s))

    # error if source_v2 is in all.vars(form), but use_source_v2 does not exist or is FALSE
    # (means there's a mismatch b/w the formula in the settings file and the argument
    # passed for source_v2)
    if("source_v2" %in% all.vars(form) && !exists("use_source_v2")){
      stop("source_v2 is in the formula, but use_source_v2 does not exist. Check settings")
    } else if ("source_v2" %in% all.vars(form) && !use_source_v2){
      stop("source_v2 is in the formula, but use_source_v2 is FALSE. Check settings")
    }

    # construct data:
    # restrict to variables in the model formula form
    keep_vars <- unique(c(all.vars(form), "year_center", "age20", "se_log_ratio", "survey", "sum_wt_report", "ID"))

    # Start with observations to FIT the model on
    inla_data_1 = copy(pairs[sex == s])
    # set response to NA for rows with fewer than 5 observations so we don't 
    # use it in model fitting
    inla_data_1[n < 5  | n_measured < 5, `:=`(log_ratio = NA, se_log_ratio = NA)]
    inla_data_1 <- inla_data_1[, .SD, .SDcols = c(keep_vars[keep_vars %in% names(pairs)])]
    
    # add in the data to PREDICT on -- this is our microdata.
    # (recall that INLA doesn't have predict function, but it will fill in predictions
    # when the input data has an NA response)
    inla_data_2 = data_quant[survey != "nhanes" & sex == s, .SD, .SDcols = c(keep_vars[keep_vars %in% names(data_quant)])]
    inla_data_2 <- na.omit(inla_data_2)
    
    # apply gold-standard to predictions, if it exists
    # update the survey/source columns in ONLY the prediction dataset. sq
    if(exists("gold_standard")){
      message("setting gold standard source for prediction: ", gold_standard)
      inla_data_2[, c("survey", "source_v2") := gold_standard]
    }

    inla_data <- rbind(inla_data_1, inla_data_2, use.names = T, fill = T)

    if(age_var == "age20"){
      inla_data[, age20_2 := age20]
    } else if (age_var == "age10"){
      inla_data[, age10_2 := age10]
      inla_data[, age20_2 := age20]
    } else {
      stop("need to update to handle age_var other than age20 or age10")
    }
    rm(inla_data_1, inla_data_2)
    inla_data[, decade := as.factor(10 * floor(pmax(year_center, 2000) / 10))]  # making 1999 part of the 2000 decade
    # round year center -- this reduces the number of levels for the time effect
    inla_data[, year_center := round(year_center, 0)] # should match whatever rounding we use in year_values
      
    
    # identify all factor variables, and drop un-used levels for those factors
    fct_vars <- lapply(inla_data, is.factor)
    fct_vars <- names(fct_vars)[unlist(fct_vars)]
    # exclude year_center and bmi_report_percentile_bin from dropping levels, if present
    fct_vars <- fct_vars[!fct_vars %in% c("year_center", "bmi_report_percentile_bin", "quantile_scaled")]
    # drop unused levels
    inla_data[, ((fct_vars)) := lapply(.SD, droplevels) , .SDcols = fct_vars]

    if(exists("use_source_v2") && use_source_v2){
      # relevel source_v2 so that gold-standard is the first level. If there is not
      # a gold standard, relevel so that BRFSS_LLCP is the first level
      make_first <- ifelse(exists("gold_standard"), gold_standard, "BRFSS_LLCP")
      if(!is.na(make_first)){ # when the gold-standard is not set as NA
        relevel_source <- levels(inla_data$source_v2)
        # stop if make_first is not in relevel_source
        if(!(make_first %in% relevel_source)){
          stop(sprintf("%s is not in levels of source_v2 (levels are %s)", make_first, paste(relevel_source, collapse = ", ")))
        }
        # reorder relevel_source
        relevel_source <- c(make_first, relevel_source[relevel_source != make_first])
        inla_data[, source_v2 := factor(source_v2, levels = relevel_source)]
      }
    }
    # index year_center from 1 & make sure it's numeric
    inla_data[, year_center := as.integer(factor(year_center, levels = year_values))]

    stopifnot(!any(is.na(inla_data$year_center)))

    stopifnot(!any(is.na(inla_data$bmi_report_percentile_bin)))

    # if year2 is a term, copy year_center to year2
    if("year2" %in% all.vars(form)){
      inla_data[, year2 := year_center]
    }

    # if source_v2_copy2 is a term, copy source_v2 to source_v2_copy2
    if("source_v2_copy2" %in% all.vars(form)){
      inla_data[, source_v2_copy2 := source_v2]
    }

    # if race_eth_code_copy is a term, copy
    if("race_eth_code_copy" %in% all.vars(form)){
      inla_data[, race_eth_code_copy := race_eth_code]
    }

    # if bmi_report_percentile_bin_copy is a term, copy
    if("bmi_report_percentile_bin_copy" %in% all.vars(form)){
      inla_data[, bmi_report_percentile_bin_copy := bmi_report_percentile_bin]
    }

    # rescale the weight column so that it sums to the number of observations with 
    # non-zero weight.
    # This is important for the stability of the results
    # See notes from INLA forum: https://groups.google.com/g/r-inla-discussion-group/c/fKR5TKbA7Yk/m/1yfHpauFDgAJ
    inla_data[, sum_wt_report := sum_wt_report/sum(sum_wt_report, na.rm = T)*sum(!is.na(sum_wt_report))]

    inla_data[, precision := 1/se_log_ratio^2] # inverse-variance (variance of log_ratio) 

    # do the scaling of the response & precision
    # mean * scale
    # var = var * scale^2 --> precision = 1/var = 1/(var*scale^2) = precision/scale^2
    inla_data[, `:=`(log_ratio = log_ratio*scale_by, precision = precision/scale_by^2)]

    # Rename the scale and weights columsn to scale and weights. Addresses a weird
    # scoping issue in INLA explained by Michael Collison
    # Quote from Slack in #research-r
    # In short, the problem may impact you if you -
    # Are using the main inla() function within a function (i.e. not in the global env)
    # Are using any of these params E, offset, scale, weights, Ntrials, strata, lp.scale, or link.covariates
    # The names of the objects being passed to the params above don't match the name of the param
    # You can see the code here - https://github.com/hrue/r-inla/blob/devel/rinla/R/inla.R#L282-L328
    # What INLA is doing is
    # Ignoring the params that are passed in (except for checking they are not NULL)
    # Finding the name of the object that was provided (i.e. if you call inla(..., weights = wgts) it finds wgts
    # Looks for wgts in the environment that the data stack for inla was created in
    # if not in the data environment, looks in the parent frame - however, the parent frame is redefined by default in the inla function https://github.com/hrue/r-inla/blob/devel/rinla/R/inla.R#L268 to be the environment where the INLA formula is created
    # wgts doesn't exist in the data, formula, or global env, so INLA throws a vague warning, ignores the weights you passed in explicitly to the function, and continues on fitting the model as if no weights were provided
    setnames(inla_data, c("precision", "sum_wt_report"), c("scale", "weights"))
    fit_inla <- inla(form,
                    data = inla_data, 
                    family = "gaussian", 
                    control.predictor(link = 1, compute = TRUE), # identity link, don't compute marginals of linear predictor
                    scale = scale,  # scale of prior on the Gaussian distribution, which is loggamma (see https://inla.r-inla-download.org/r-inla.org/doc/likelihood/gaussian.pdf)
                    weights = if(weight_inla) weights else NULL, 
                    control.inla = list(strategy = "gaussian", int.strategy = "eb", h = 1e-3),
                    control.compute = list(config = TRUE, waic = TRUE, return.marginals.predictor=FALSE),
                    control.family = list(hyper = list(prec = list(
                      prior = "loggamma", param = c(1, 0.01), initial = 2
                    ))),
                    verbose = TRUE)
    print(summary(fit_inla))
    return(fit_inla)
  })
  message("model fit!")

}

cat(capture.output(print(micro_meta)), file = paste0(output_dir, "/input_microdata_meta.txt"), sep = "\n")
saveRDS(inla_fits, paste0(output_dir, "model.rds")) 
cat(capture.output(print(form)), file = paste0(output_dir, "/inla_formula.txt")) 
# report how much information was dropped due to sample sizes (used to figure out
# if we've over-stratified)
dropped <- pairs[, .(strata = .N, sample_size_self_report = sum(n), sample_size_measure = sum(n_measured)), .(dropped = !(n >= 5 & n_measured >= 5))]
dropped <- dropped[, c(list(dropped = dropped), lapply(.SD, function(x) sprintf("%.02f%%", x/sum(x)*100))), .SDcols = setdiff(names(dropped), "dropped")]
dropped_by_race <- pairs[, .(strata = .N, sample_size_self_report = sum(n), sample_size_measure = sum(n_measured)), .(dropped = !(n >= 5 & n_measured >= 5), race_eth_code)]
dropped_by_race <- dropped_by_race[, c(list(dropped = dropped), lapply(.SD, function(x) sprintf("%.02f%%", x/sum(x)*100))), .SDcols = setdiff(names(dropped_by_race), c("dropped", "race_eth_code")), by = "race_eth_code"][order(dropped)]
cat(capture.output(print(dropped)), file = paste0(output_dir, "info_dropped_strata.txt"), sep = "\n")
cat(capture.output(print(dropped_by_race)), file = paste0(output_dir, "info_dropped_by_race_strata.txt"), sep = "\n")


################################################################################
# Generate predictions from INLA
################################################################################

# Above, we provide the microdata we want to predict on to the INLA model.
# The response variable for the microdata is blank, so INLA will generate
# predictions.

# Generate n.sims samples of the posterior from the INLA model fit.
# The INLA predictions are log_ratios. Use this to calculate the measured BMI
# from the self-reported BMI.
message("starting prediction")
preds <- lapply(inla_fits, function(mod){
  # Generate n.sims posterior samples. This returns of list, where each element
  # is a sample
  samples <- inla.posterior.sample(n = n.sims, result = mod, verbose = T)
  # We make to make predictors of the log ratio for each respondent.
  # This is analogous to creating a new dataset from the fitted model
  # in the INLA parlance: (that's only true if we include precision of the response)
  # Adapted from https://inla.r-inla-download.org/r-inla.org/doc/vignettes/sample-eval.html,
  # section "Samples of hyper-parameters". 
  pred_response <- function(.scale_by = scale_by) {
    # predictor -- either the linear predictor or the fitted values.
    return(Predictor/.scale_by)
  }
  
  pred <- as.data.table(inla.posterior.sample.eval(fun = pred_response, samples))
  setnames(pred, paste0("sample:", 1:n.sims), paste0("pred_log_ratio_", 1:n.sims))
  # also extract the linear predictor and theta
  mod_summary <- as.data.table(mod$summary.fitted.values[, c("mean", "sd")])[, `:=`(mean = mean / scale_by, sd = sd / scale_by)]
  setnames(mod_summary, c("mean", "sd"), c("fitted_value_mean", "fitted_value_sd"))
  # recombine pred with the input data 
  pred <- cbind(mod$.args$data, pred, mod_summary)
  # un-do transformation from above
  pred[, `:=`(log_ratio = log_ratio / scale_by, scale = scale * scale_by^2)]
  # output just the prediction variables
  out <- pred[, .SD, .SDcols = c("ID", "fitted_value_mean", "fitted_value_sd", paste0("pred_log_ratio_", 1:n.sims))]
  
  return(out)
})

preds <- rbindlist(preds)

# subset to just the training set (ID is negative)
preds_pairs <- preds[ID < 0]
# subset preds to just the prediction set (ID is positive)
preds <- preds[ID > 0]

stopifnot(na.omit(preds)[, .N] == preds[, .N])

# merge preds onto the microdata
setkey(data_quant, ID)
setkey(preds, ID)
pred_cols <- paste0("pred_log_ratio_", 1:n.sims)
data_quant[preds, ((pred_cols)) := mget(paste0("i.", pred_cols))]

# and so the same merge onto pairs
setkey(pairs, ID)
setkey(preds_pairs, ID)
pairs[preds_pairs, ((pred_cols)) := mget(paste0("i.", pred_cols))]

# attach NHANES back onto data_quant
data_quant <- rbind(
  data_quant, 
  data[survey == "nhanes" & svyyear != "2017_2018"][, .SD, .SDcols = names(data_quant)[names(data_quant) %in% names(data)]], 
  use.names = T, fill = T)

# if pred_exclusions is specified in the settings, apply to the prediction dataset (data_quant)
if(exists("pred_exclusions")){
  data_quant <- data_quant[!eval(pred_exclusions)]
}

# estimate measured BMI based on self-reported BMI and log ratio
# log_ratio = log(measured/report), so
# report = measured/exp(log_ratio)
data_quant[, ((paste0("pred_bmi_", 1:n.sims))) := lapply(.SD, function(log_ratio) bmi_report * exp(log_ratio)), .SDcols = pred_cols]
pairs[, ((paste0("pred_bmi_", 1:n.sims))) := lapply(.SD, function(log_ratio) mean_bmi_report * exp(log_ratio)), .SDcols = pred_cols]

# drop NHANES svyyear 2017_2018 b/c duplicated with 2017_2020prp
data_quant <- data_quant[!(survey == "nhanes" & svyyear == "2017_2018")]

################################################################################
# Save microdata and collapsed output
################################################################################
message("starting to save predictions")

# write info about the input versions used 
# (print cw_meta and microdata_meta, and capture output to save as text file)
saveRDS(data_quant, paste0(output_dir, "pred.rds"))
saveRDS(pairs, paste0(output_dir, "pairs.rds"))

## Expand the Gallup data to use OMB 1977 categories with bridged race

# Note that that bridged race output will be exactly the same as the original output
#   because we dropped NH Multiracial (among other) populations from Gallup due
#   to inconsistencies in Gallup survey questions/coding.
# Keeping the following lines in case we decide to keep NH Asian, NH multiracial,
#   NH AIAN, NH NHOPI, and NH other respondents from Gallup in the future
# 
# # There are some limitations in the NCHS bridging algorithm used to produce
# # the race categories in the following file -- specifically related to dropping
# # some Asian respondents in some waves.
# # We're working on updating the model, but the missingness is currently propagated through to this file. 
gallup_meta <- get_survey_extraction(input_type = "gallup", get_best = T)
stopifnot(unique(gallup_meta$survey_extraction_version_id) %in%
            unlist(strsplit(micro_meta$survey_extraction_version_id, ", ")))  # stop if gallup version doesn't match version according to compiled microdata
gallup77 <- readRDS(paste0(unique(gallup_meta$output_file_path), "gallup_microdata_race77.rds"))

# merge on the unique ID in Gallup to add 1997 data with bridged race (and partial race weight)
# keep the Gallup race variable called gallup_race77 instead of filling in the pre-existing race77 column
#   in pred to indicate that the Gallup version of the variable used the race bridging
#   algorithm, so the re_weights must be used.

pred77 <- merge(data_quant, gallup77[, .(gallup_uid = uid, gallup_race77 = race77, re_weight)], by = "gallup_uid", all.x = T)

message("Saving version of bridged 1977 race (Gallup)")
saveRDS(pred77, paste0(output_dir, "pred_bridged_race.rds"))

message("Done saving")
