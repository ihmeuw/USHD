#####################################################################################
## Description: Calculate standard errors manually.
##  This is recommended in this documentation from IPUMS: https://usa.ipums.org/usa/repwt.shtml
## 
## Details: Calculate HDI using the perwt file, and then merge on the replicate weights.
##  Then calculate the standard errors using the replicate weights, but use only
##  the perwt information to get the point estimate.
##  This calculation is mostly done by the below function se_function()
##  Also, make sure that the manual calculation is the same as that provided using the survey() function.
##  Do this by calculating the SEs manually for a subset of replicate weights, and comparing
##  the results to the result of using the survey() function for that subset of replicate weights.
##  Do this for only a subset because this is otherwise computationally infeasible.
#####################################################################################

library(readstata13)
library(haven)
library(survey)
library(ggplot2)
library(RColorBrewer)
library(stats)
library(ggpubr)
library(sf)
library(gridExtra)
library(data.table)
library(mgsub)
library(dplyr)
library(srvyr, lib.loc = "[LIBRARY_PATH]")

##################### Custom functions
##### Assign bins to the continuous percentile values
calc_bins <- function(dt, percentile_name) {
  dt <- copy(dt)
  setnames(dt, percentile_name, "percentile_tmp")
  dt[, pct_bin := cut(percentile_tmp, breaks = seq(0,1,0.1), right = TRUE)]
  
  dt[,pct_label := tstrsplit(pct_bin,",")[[2]]]
  dt[,pct_label := tstrsplit(pct_label,"]")[[1]]]
  dt[,pct_label := as.numeric(pct_label)]
  dt[abs(percentile_tmp - 1) < 0.0001, pct_label := 1]
  
  dt[,pct_bin := NULL]
  setnames(dt, c("percentile_tmp", "pct_label"), c(percentile_name, paste0("pct_label_",percentile_name)))
  
  return(dt)
}

##### Calculate the standard error using the replicate weights
# Formula from: https://usa.ipums.org/usa/repwt.shtml
se_function <- function(dt, rep_vals, byvars, max_val) {
  dt <- dt[, (rep_vals) := lapply(.SD, function(x) (x-perwt)^2), .SDcols = rep_vals]
  dt[,se := sqrt(4/max_val*rowSums(.SD)), by=byvars, .SDcols = rep_vals]
  return(dt)
}

##### Compare the results of manually calculating the standard errors versus using the survey() function
#####   (for a subset of the replicate weights)
compare_manual_with_survey <- function(dt_manual, dt_survey) {
  combined <- rbind(copy(dt_manual)[,version := "manual"],
                    copy(dt_survey)[,version := "from_survey"])
  
  if(!("variable" %in% names(combined))) combined[,variable := "placeholder"]
  
  combined <- dcast.data.table(combined, pct_label_percentile + race + sex + year + age_group + measure + variable ~ version, value.var=c("value","se"))
  stopifnot(nrow(combined[is.na(value_manual)]) == 0) # this should be 0, since we are assessing the entire dataset
  # But, we only calculate the survey values for a subset, so we can remove the rows that were not included in the survey analysis
  combined <- combined[!is.na(value_from_survey)]
  
  stopifnot(nrow(combined) > 0)
  
  # confirm that they are the same
  stopifnot(nrow(combined[(measure == "total" & abs(value_from_survey - value_manual) > 0.000001) |
                            (measure == "proportion" & abs(value_from_survey - value_manual) > 0.0000000001)]) == 0)
  stopifnot(nrow(combined[(measure == "total" & abs(se_from_survey - se_manual) > 0.000001) |
                            (measure == "proportion" & abs(se_from_survey - se_manual) > 0.0000000001)]) == 0)
}

############ Functions for calculating proportions
##### Loop over the strata and variables by which we want to calculate the proportion of each
#####   population in each HDI percentile
calc_proportions <- function(fdata, max_val, byage, rep_vars, number_years, base_cols_arg,
                             prop_types, year_types) {
  fdata <- copy(fdata)
  proportions <- data.table()
  for(proportion_arg in c(T,F)) { # if you want to calculate a proportion (T) or just the total numerator (F)
    
    for(prop_type in prop_types) {
      
      for(yr_type in year_types) {
        
        for(by_pct in c(T,F)) {
          
          message(paste0("Calculating proportions ",yr_type," for: ",prop_type,". Within percentile bin? ",by_pct))
          
          base_cols <- base_cols_arg
          if(byage) base_cols <- c(base_cols, "age_group")
          
          if(prop_type == "prop_of") {
            removed_vars <- c("race","sex")
          } else {
            removed_vars <- c("pct_label_percentile")
          }
          
          if(!by_pct) {
            base_cols <- base_cols[base_cols != "pct_label_percentile"]
          }
          
          if(yr_type == "across years") {
            base_cols <- base_cols[base_cols != "year"]
          }
          
          message("Numerator variables:\n")
          message(paste(base_cols,collapse="\n"))
          
          message("Denominator variables:\n")
          message(paste(base_cols[!(base_cols %in% removed_vars)],collapse="\n"))
          
          if(length(setdiff(base_cols, base_cols[!(base_cols %in% removed_vars)])) == 0) {
            message("Skipping because numerator and denominator are the same")
            next()
          }
          
          proportions <- rbind(proportions, get_proportions(dt = fdata,
                                                            rep_vars = rep_vars,
                                                            byvars = base_cols,
                                                            denom_vars = base_cols[!(base_cols %in% removed_vars)],
                                                            max_val = max_val,
                                                            point_name = "value",
                                                            variable_name = prop_type,
                                                            get_proportion = proportion_arg,
                                                            number_years = number_years))
        }
      }
      gc()
    }
  }
  
  return(proportions)
}

##### Actually calculate the proportion (called by calc_proportions())
get_proportions <- function(dt, rep_vars, byvars, denom_vars, max_val, point_name, variable_name, get_proportion, number_years) {
  dt <- copy(dt)
  dt <- dt[, lapply(.SD, sum), by = byvars, .SDcols = rep_vars]
  
  if(get_proportion) {
    dt <- dt[, (rep_vars) := lapply(.SD, function(x) x/sum(x)), by = denom_vars, .SDcols = rep_vars]
  } else if (!("year" %in% byvars) & max_val == 80) {
    message("Dividing by number of years")
    dt <- dt[, (rep_vars) := lapply(.SD, function(x) x/number_years), .SDcols = rep_vars]
    
  }  # else, we just want to get the variation in the totals
  # this step stays the same
  rep_vars_no_perwt <- rep_vars[rep_vars != "perwt"]
  dt <- se_function(dt = dt, rep_vals = rep_vars_no_perwt, byvars = byvars, max_val = max_val)
  
  # format
  cols <- c(byvars, "perwt", "se")
  dt <- dt[,(cols), with=F]
  gc()
  
  dt[,variable := variable_name]
  setnames(dt, "perwt", point_name)
  
  for(ii in c("pct_label_percentile", "year","race","sex","age_group")) {
    if(!(ii %in% names(dt))) {
      dt[,tmp_val := "ALL"]
      setnames(dt, "tmp_val", ii)
    }
  }
  dt[,measure := ifelse(get_proportion, "proportion", "total")]
  
  return(dt)
}

##### Calculate the proportions using the survey() function
calc_proportions_by_survey <- function(svy_obj, age_arg, race_sex_arg, pct_arg, year_arg, prop_types, by_puma_mcnty = F) {
  proportions <- data.table()
  for(prop_type in prop_types) {
    message(paste0("Working on: ",prop_type))
    if(prop_type == "prop_in" & pct_arg != "ALL") next
    svy_tmp <- copy(svy_obj)
    
    if(age_arg != "ALL") {
      svy_tmp <- svy_tmp %>%
        filter(age_group == age_arg)
    }
    
    if(year_arg != "ALL") {
      svy_tmp <- svy_tmp %>%
        filter(year == year_arg)
    }
    
    if(prop_type == "prop_in") {
      
      if(by_puma_mcnty) {
        if(race_sex_arg != "ALL_ALL") {
          svy_tmp <- svy_tmp %>%
            filter(race_sex == race_sex_arg)
        }
        svy_tmp <- svy_tmp %>%
          group_by(puma_mcnty, pct_label_percentile)
        
      } else {
        svy_tmp <- svy_tmp %>%
          filter(race_sex == race_sex_arg) %>%
          group_by(pct_label_percentile)
      }
    } else {
      if(pct_arg != "ALL") {
        svy_tmp <- svy_tmp %>%
          filter(pct_label_percentile == pct_arg) %>%
          group_by(race_sex)
      } else {
        svy_tmp <- svy_tmp %>%
          group_by(race_sex)
      }
    }
    
    if(nrow(svy_tmp) == 0) {
      message("survey object has 0 rows, so skipping")
      next
    }
    
    svy_tmp <- svy_tmp %>%
      summarize(proportion = survey_mean(vartype = c("se")),
                total = survey_total(vartype = c("se"))) %>%
      as.data.table()
    
    if(!("pct_label_percentile" %in% names(svy_tmp))) svy_tmp[,pct_label_percentile := pct_arg]
    if(!("year" %in% names(svy_tmp))) svy_tmp[,year := year_arg]
    if(!("race_sex" %in% names(svy_tmp))) svy_tmp[,race_sex := race_sex_arg]
    if(!("age_group" %in% names(svy_tmp))) svy_tmp[,age_group := age_arg]
    
    svy_tmp[,variable := prop_type]
    proportions <- rbind(proportions, svy_tmp, fill=T)
  }
  
  return(proportions)
}

############# Functions for calculating averages
##### Analagous function to get_proportions(), but for calculating the weighted averages
get_weighted_average <- function(dt, rep_vars, byvars, max_val, point_name, covariate) {
  dt <- copy(dt)
  setnames(dt, covariate, "covariate_tmp")
  
  dt <- dt[, (rep_vars) := lapply(.SD, function(x) (covariate_tmp*x)/sum(x)), by = byvars, .SDcols = rep_vars] # first weight
  gc()
  dt <- dt[, lapply(.SD, sum), by = byvars, .SDcols = rep_vars] # then sum
  gc()
  
  rep_vars_no_perwt <- rep_vars[rep_vars != "perwt"]
  dt <- se_function(dt = dt, rep_vals = rep_vars_no_perwt, byvars = byvars, max_val = max_val)
  
  # format
  cols <- c(byvars, "perwt", "se")
  dt <- dt[,(cols), with=F]
  gc()
  
  setnames(dt, "perwt", point_name)
  
  for(ii in c("pct_label_percentile", "year","race","sex","age_group")) {
    if(!(ii %in% names(dt))) {
      dt[,tmp_val := "ALL"]
      setnames(dt, "tmp_val", ii)
    }
  }
  
  dt[,measure := covariate]
  
  return(dt)
}

##### Analagous function to calc_proportions(), but for calculating the weighted averages
calc_weighted_vals <- function(fdata, max_val, byage, rep_vars, base_cols_arg, year_types, percentile_calc_options) {
  fdata <- copy(fdata)
  averages <- data.table()
  
  for(cov in c("eduyrs","hh_consumption","ex_mean","hdi")) {
    
    for(yr_type in year_types) {
      
      for(by_pct in percentile_calc_options) {
        
        message(paste0("Calculating averages for ",cov,", ",yr_type,". Within percentile bin? ",by_pct))
        
        base_cols <- base_cols_arg
        if(byage) base_cols <- c(base_cols, "age_group")
        
        if(!by_pct) {
          base_cols <- base_cols[base_cols != "pct_label_percentile"]
        }
        
        if(yr_type == "across years") {
          base_cols <- base_cols[base_cols != "year"]
        }
        
        message("Variables:\n")
        message(paste(base_cols,collapse="\n"))
        
        averages <- rbind(averages, get_weighted_average(dt = fdata, 
                                                         rep_vars = rep_vars,
                                                         byvars = base_cols,
                                                         max_val = max_val,
                                                         point_name = "value",
                                                         covariate = cov))
        
      }
    }
  }
  gc()
  
  return(averages)
}

##### Calculate the weighted averaged usinf the survey() function
calc_averages_by_survey <- function(svy_obj, age_arg, race_sex_arg, pct_arg, year_arg) {
  averages <- data.table()
  svy_tmp <- copy(svy_obj)
  
  if(age_arg != "ALL") {
    svy_tmp <- svy_tmp %>%
      filter(age_group == age_arg)
  }
  
  if(year_arg != "ALL") {
    svy_tmp <- svy_tmp %>%
      filter(year == year_arg)
  }
  
  if(pct_arg != "ALL") {
    svy_tmp <- svy_tmp %>%
      filter(pct_label_percentile == pct_arg)
  }
  
  svy_tmp <- svy_tmp %>%
    group_by(race_sex)
  
  if(nrow(svy_tmp) == 0) {
    message("survey object has 0 rows, so skipping")
    next
  }
  
  svy_tmp <- svy_tmp %>%
    summarize(hdi = survey_mean(hdi, vartype = c("se")),
              eduyrs = survey_mean(eduyrs, vartype = c("se")),
              hh_consumption = survey_mean(hh_consumption, vartype = c("se")),
              ex_mean = survey_mean(ex_mean, vartype = c("se"))) %>%
    as.data.table()
  
  if(!("pct_label_percentile" %in% names(svy_tmp))) svy_tmp[,pct_label_percentile := pct_arg]
  if(!("year" %in% names(svy_tmp))) svy_tmp[,year := year_arg]
  if(!("race_sex" %in% names(svy_tmp))) svy_tmp[,race_sex := race_sex_arg]
  if(!("age_group" %in% names(svy_tmp))) svy_tmp[,age_group := age_arg]
  
  averages <- rbind(averages, svy_tmp, fill=T)
  
  return(averages)
}

#########################################
if(interactive()) {
  save_dir <- "[HDI_OUTPUT_DIRECTORY]"
  date_time_stamp <- "[TIME_STAMP]" 
  yr1 <- 2008
  yr2 <- 2021
  by_percentile <- T
  var <- "hdi_index"
  age_group_labels <- "25-44xx45-64xx65-84xx85+"
  min_weight <- 1
  max_weight <- 80
  testing_max_weight <- 2
  byage <- T
  calculation_type <- "proportion"
} else {
  args <- commandArgs(trailingOnly = TRUE)
  save_dir <- args[1]
  date_time_stamp <- args[2]
  yr1 <- as.numeric(args[3])
  yr2 <- as.numeric(args[4])
  by_percentile <- as.logical(args[5])
  var <- args[6]
  age_group_labels <- args[7]
  min_weight <- args[8]
  max_weight <- as.integer(args[9])
  testing_max_weight <- as.integer(args[10])
  byage <- as.logical(args[11])
  calculation_type <- args[12]
}

####################### Prep data ###########################
stopifnot(min_weight == 1 & max_weight == 80)

age_group_labels <- strsplit(age_group_labels,"xx")[[1]]
rep_vars <- c("perwt",paste0("repwtp",c(min_weight:max_weight)))
rep_vars_reduced <- c("perwt",paste0("repwtp",c(min_weight:testing_max_weight)))

if(byage) {
  hdi_file <- "combined_weights_age_grp_specific_hdi.rds"
  file_ending <- "_by_age.rds"
} else {
  hdi_file <- "combined_weights_all_age_hdi.rds"
  file_ending <- "_all_age.rds"
}

## Read in data
data <- readRDS(file.path(dirname(save_dir),hdi_file))
gc()

total_num_years <- length(yr1:yr2)

if(!byage) {
  data[,age_group := NULL]
}

# reduce size of data
data[,c("age_specific","sample","serial","statefip","pernum","age","total_weight","wt") := NULL]
gc()

### Label the bins
data[,percentile := get(var)]

# Get the deciles
data <- calc_bins(dt = data, percentile_name = "percentile")
gc()

data[,c("percentile") := NULL]
data[,race_sex := paste0(race,"_",sex)]

summary_dir <- file.path(dirname(save_dir),"summaries")
dir.create(summary_dir, recursive = T)

############################################################################################ 
############################################################################################ 
####################### Make survey objects using a subset of the replicate weights ###########################

### Now make some subsets that we can use for testing
message("Making survey objects for smaller datasets")
rep_wgt_vars_not_included <- c(paste0("repwtp",setdiff(c(min_weight:max_weight),c(min_weight:testing_max_weight))))
data_reduced <- copy(data)[,(rep_wgt_vars_not_included) := NULL]

# make survey objects for these reduced sets
# See documentation here for this command suggested by IPUMS: https://usa.ipums.org/usa/repwt.shtml
svy_reduced <- as_survey_rep(data_reduced, weight = perwt , repweights = starts_with("repwtp"),
                             type = "JK1", scale = 4/ testing_max_weight , rscales = rep(1, testing_max_weight ), mse = TRUE)

#### Prepare the puma-mcnty data
# in this scenario, we calculate the puma-mcnty data
# subset to 2012+ for the puma-mcnty analysis, since the PUMA versions change in 2012. Thus, this avoids having to use multiple versions.
if(!byage & calculation_type == "puma_mcnty") {
  data_2012_plus <- copy(data[year >= 2012])
  unique_combos <- readRDS(file.path(dirname(dirname(save_dir)),"puma_mcnty_combined_mapping.rds"))
  ## Now check that all of the PUMAs in the crosswalk file are in our file
  stopifnot(setdiff(unique(data_2012_plus$puma), unique_combos$PUMA_ID) == 0)
  stopifnot(setdiff(unique_combos$PUMA_ID, unique(data_2012_plus$puma)) == 0)
  
  # Now we want to aggregate the PUMAs together that have the same puma_mcnty designation
  data_2012_plus <- merge(data_2012_plus, unique(unique_combos[,.(puma = PUMA_ID, puma_mcnty)]), by="puma", all=T)
  # make sure the merge was successful
  stopifnot(nrow(data_2012_plus[is.na(puma_mcnty)]) == 0)
  stopifnot(nrow(data_2012_plus[is.na(perwt)]) == 0)
  
}

gc()

## Create combinations we will test using the survey function
combos <- as.data.table(expand.grid(race = c("Black","API"),
                                    sex = c(1),
                                    year = c(2010,"ALL"),
                                    pct = c(0.1,"ALL")))
combos[,race_sex := paste0(race,"_",sex)]
combos[,race := as.character(race)]
combos[,year := as.character(year)]
combos[,pct := as.character(pct)]
# remove combination that causes the session to crash
combos <- combos[!(pct == "ALL" & year == "ALL")]
ag <- ifelse(byage, "45-64", "ALL")

############################################################################################ 
############################################################################################ 
####################### Calculate the population proportions within each HDI decile ###########################

#### Now start looping through the data to get the proportions

if(calculation_type == "proportion") {
  ############### First, test that the manual calculation and survey() functions produce the same results
  # First calculate the proportions using the reduced dataset, so we can make sure that the process is comparable to using the survey function
  reduced_props <- calc_proportions(fdata = data_reduced, max_val = testing_max_weight, byage = byage,
                                    rep_vars = rep_vars_reduced,
                                    number_years = total_num_years,
                                    base_cols_arg = c("year","race","sex", "pct_label_percentile"),
                                    prop_types = c("prop_in","prop_of"), year_types = c("by year", "across years"))
  
  # and now for the survey objects using the reduced number of replicate weights
  # check for just a few combinations
  survey_props <- data.table()
  
  for(ii in 1:nrow(combos)) {
    
    message(paste0("Ages: ", ag,"\n"))
    message(paste0("Row ",ii," of ",nrow(combos)))
    
    survey_props <- rbind(survey_props,
                          calc_proportions_by_survey(svy_reduced,
                                                     age_arg = ag,
                                                     race_sex_arg = combos[ii, race_sex],
                                                     pct_arg = combos[ii, pct],
                                                     year_arg = combos[ii, year],
                                                     prop_types = c("prop_in","prop_of"),
                                                     by_puma_mcnty = F))
    gc()
  }
  
  # merge on the combos, so we only keep what we want to look at, and also make sure we created everything
  survey_props <- merge(survey_props, combos[,indic := 1], by.x=c("race_sex","year","pct_label_percentile"), by.y=c("race_sex","year","pct"),
                        all.y=T)
  survey_props <- survey_props[!is.na(proportion)]
  
  # Function to make the survey results look like the results coming from the manual SE calculation
  process_survey_proportions <- function(survey_props) {
    if(!("puma_mcnty" %in% names(survey_props))) {
      survey_props[,puma_mcnty := 1]
    }
    
    survey_props <- melt.data.table(survey_props, measure.vars=c("proportion","proportion_se","total","total_se"),
                                    variable.name = "measure", value.name="value")
    survey_props[,metric := tstrsplit(measure,"_")[2]]
    survey_props[,measure := tstrsplit(measure,"_")[1]]
    survey_props[is.na(metric), metric := "value"]
    survey_props <- unique(survey_props) 
    survey_props <- dcast.data.table(survey_props, puma_mcnty + pct_label_percentile + year + race_sex + age_group + variable + measure ~ metric, value.var="value")
    survey_props[,race := tstrsplit(race_sex,"_")[1]]
    survey_props[,sex := tstrsplit(race_sex,"_")[2]]
    survey_props[,race_sex := NULL]
    
    if(length(unique(survey_props$puma_mcnty)) == 1) {
      survey_props[,puma_mcnty := NULL]
    }
    
    return(survey_props)
  }
  
  survey_props <- process_survey_proportions(survey_props)
  
  # Comparison to make sure they are the same
  compare_manual_with_survey(dt_manual = reduced_props,
                             dt_survey = survey_props)
  
  rm(reduced_props, survey_props) # these are only used for testing; we don't need to keep them
  gc()
  
  ############### End of testing on reduced dataset  ############### 
  
  ############### Now for the full dataset
  full_props <- calc_proportions(fdata = data, max_val = max_weight, byage = byage,
                                 rep_vars = rep_vars,
                                 number_years = total_num_years,
                                 base_cols_arg = c("year","race","sex", "pct_label_percentile"),
                                 prop_types = c("prop_in","prop_of"), year_types = c("by year", "across years"))
  
  # now across races and sexes
  full_props_no_race <- calc_proportions(fdata = data, max_val = max_weight, byage = byage,
                                         rep_vars = rep_vars,
                                         number_years = total_num_years,
                                         base_cols_arg = c("sex", "pct_label_percentile"),
                                         prop_types = c("prop_of"), year_types = c("across years"))
  
  # and also calculate the proportion out of the total population, across percentiles
  full_props_no_percentile <- calc_proportions(fdata = data, max_val = max_weight, byage = byage,
                                               rep_vars = rep_vars,
                                               number_years = total_num_years,
                                               base_cols_arg = c("sex", "race"),
                                               prop_types = c("prop_of"), year_types = c("across years"))
  
  # now across sexes, but by race and percentile
  full_props_no_sex <- calc_proportions(fdata = data, max_val = max_weight, byage = byage,
                                         rep_vars = rep_vars,
                                         number_years = total_num_years,
                                         base_cols_arg = c("race", "pct_label_percentile"),
                                         prop_types = c("prop_of"), year_types = c("across years"))
  
  full_props <- unique(rbind(full_props,
                             full_props_no_race,
                             full_props_no_percentile,
                             full_props_no_sex))
  
  ### Save the proportions
  saveRDS(full_props, file.path(summary_dir,paste0("proportions_by_", var, file_ending)))
  
  rm(full_props)
  gc()
  
} else if(calculation_type == "weighted_avgs" & var == "hdi_index") {
  
  #### Now the same for the datasets with the reduced number of replicate weights
  reduced_averages <- calc_weighted_vals(fdata = data_reduced, max_val = testing_max_weight, 
                                         byage = byage, rep_vars = rep_vars_reduced,
                                         base_cols_arg = c("year","race","sex", "pct_label_percentile"),
                                         year_types = c("by year", "across years"),
                                         percentile_calc_options = c(T,F))
  
  
  survey_averages <- data.table()
  
  for(ii in 1:nrow(combos)) {
    message(paste0("Ages: ", ag,"\n"))
    message(paste0("Row ",ii," of ",nrow(combos)))
    
    survey_averages <- rbind(survey_averages,
                             calc_averages_by_survey(svy_reduced,
                                                     age_arg = ag,
                                                     race_sex_arg = combos[ii, race_sex],
                                                     pct_arg = combos[ii, pct],
                                                     year_arg = combos[ii, year]))
    gc()
  }
  
  # need to format this to look like the results coming from the manual SE calculation
  # merge on the combos, so we only keep what we want to look at, and also make sure we created everything
  survey_averages <- merge(survey_averages, combos[,indic := 1], by.x=c("race_sex","year","pct_label_percentile"), 
                           by.y=c("race_sex","year","pct"),
                           all.y=T)
  survey_averages <- survey_averages[!is.na(hdi)]
  
  survey_averages <- melt.data.table(survey_averages, measure.vars=c(c("eduyrs","hh_consumption","ex_mean","hdi"),
                                                                     paste0(c("eduyrs","hh_consumption","ex_mean","hdi"),c("_se"))),
                                     variable.name = "measure", value.name="value")
  survey_averages[,metric := ifelse(measure %like% "_se$", "se", "value")]
  survey_averages[,measure := ifelse(measure %like% "eduyrs", "eduyrs",
                                     ifelse(measure %like% "hh_consumption","hh_consumption",
                                            ifelse(measure %like% "ex_mean", "ex_mean",
                                                   ifelse(measure %like% "hdi", "hdi", "error"))))]
  
  stopifnot(nrow(survey_averages[measure == "error"]) == 0)
  survey_averages <- unique(survey_averages) # again, not sure what is up with these duplicates
  survey_averages <- dcast.data.table(survey_averages, pct_label_percentile + year + race_sex + age_group + measure ~ metric, value.var="value")
  survey_averages[,race := tstrsplit(race_sex,"_")[1]]
  survey_averages[,sex := tstrsplit(race_sex,"_")[2]]
  survey_averages[,race_sex := NULL]
  
  # Comparison to make sure they are the same
  compare_manual_with_survey(dt_manual = reduced_averages,
                             dt_survey = survey_averages)
  
  rm(reduced_averages, survey_averages) # these are only used for testing; we don't need to keep them
  gc()
  
  ### Now for the full dataset
  full_averages <- calc_weighted_vals(fdata = data, max_val = max_weight, byage = byage, rep_vars = rep_vars,
                                      base_cols_arg = c("year","race","sex", "pct_label_percentile"),
                                      year_types = c("by year", "across years"),
                                      percentile_calc_options = c(T,F))
  
  ### Save the averages
  saveRDS(full_averages, file.path(summary_dir,paste0("averages_by_",var, file_ending)))
  
  rm(full_averages)
  gc()
  
} else if (calculation_type == "puma_mcnty" & var == "hdi_index" & !byage) {
  ###### Now, calculate the puma-mcnty weighted average HDI values and proportions
  num_years_mcnty_puma <- length(unique(data_2012_plus$year))
  
  # get the proportion in each decile within each mcnty-puma, both by race/sex and well as across races and sexes
  full_props_mcnty_puma_by_racesex <- calc_proportions(fdata = data_2012_plus, max_val = max_weight,
                                                       byage = byage, rep_vars = rep_vars,
                                                       number_years = num_years_mcnty_puma,
                                                       base_cols_arg = c("race","sex", "pct_label_percentile", "puma_mcnty"),
                                                       prop_types = c("prop_in"), year_types = c("across years"))
  
  # aggregate across races and sexes
  full_props_mcnty_puma_across_racesex <- calc_proportions(fdata = data_2012_plus, max_val = max_weight,
                                                           byage = byage, rep_vars = rep_vars,
                                                           number_years = num_years_mcnty_puma,
                                                           base_cols_arg = c("pct_label_percentile","puma_mcnty"),
                                                           prop_types = c("prop_in"), year_types = c("across years"))
  
  full_props_mcnty_puma <- rbind(full_props_mcnty_puma_by_racesex,
                                 full_props_mcnty_puma_across_racesex)
  
  ### Save the proportions
  saveRDS(full_props_mcnty_puma, file.path(summary_dir,paste0("proportions_by_mcnty_puma_",var, file_ending)))
  rm(full_props_mcnty_puma); gc()
  
  ##### Now get the average values
  full_averages_mcnty_puma_by_racesex <- calc_weighted_vals(fdata = data_2012_plus, max_val = max_weight, byage = byage, rep_vars = rep_vars,
                                                            base_cols_arg = c("race","sex", "puma_mcnty"),
                                                            year_types = c("across years"),
                                                            percentile_calc_options = F)
  
  full_averages_mcnty_puma_across_racesex <- calc_weighted_vals(fdata = data_2012_plus, max_val = max_weight, byage = byage, rep_vars = rep_vars,
                                                                base_cols_arg = c("puma_mcnty"),
                                                                year_types = c("across years"),
                                                                percentile_calc_options = F)
  
  full_averages_mcnty_puma_ <- rbind(full_averages_mcnty_puma_by_racesex,
                                     full_averages_mcnty_puma_across_racesex)
  
  rm(full_averages_mcnty_puma_by_racesex, full_averages_mcnty_puma_across_racesex); gc()
  
  ### Save the proportions
  saveRDS(full_averages_mcnty_puma_, file.path(summary_dir,paste0("averages_by_mcnty_puma_",var, file_ending)))
  rm(full_averages_mcnty_puma_); gc()
  
} else {
  stop("Invalid options")
}
