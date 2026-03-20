####################################################################################################
## Inputs:    INLA model fit
##            settings: FILEPATH
##            
## Outputs:   Returns table with the correction factors (ratio measured/self-reported BMI)
##              by age, sex, year, source, and BMI quantile.
##            Save to
##            FILEPATH
##            and FILEPATH
##              so we can share with GBD.
## 
## Description: Produce crosswalk ratios that GBD can apply to their microdata.
##              Using INLA model fit, calculate the crosswalk ratios by age, sex,
##              race, source (and no-source), and BMI quantile.
##              Collapse the race-specific corrections to all-race using the population
##                proportions of race by age/sex. This is NOT mathematically equivalent
##                to calculating the all-race ratios directly, so we run
##                sensitivity analyses to ensure the methods are close enough.
##            Creates plots to compare adjusted BMI using the all-race correction ratios we
##              provide to GBD vs the results using race-specific ratios.
##         
####################################################################################################

################################################################################
# Set up
################################################################################

if (dir.exists(paste0('FILEPATH', Sys.info()['user']))) {
  repo <- paste0('FILEPATH', Sys.info()['user'], '/risk_factors/')
} else {
  repo <- paste0('FILEPATH', Sys.info()['user'], '/us_counties/risk_factors/')
}

library(data.table)
library(ggplot2)
library(INLA)

source(paste0(repo, "0_functions/get_db_results.R"))
source(paste0(repo, "../non_fatal/sae_models/functions/settings.R"))
source(paste0(repo, "2_exposure/high_bmi/correction_models/vetting/cw_time_trend_helper_funs.R")) # useful to collapse microdata for vetting

## read in from command args
if(interactive()){
  output_dir = "FILEPATH"
} else{
  args <- commandArgs(TRUE)
  output_dir <- args[[1]]
}
print(output_dir)

# Define the shared output dir
out_share <- "FILEPATH"
# Append the model version to path
out_share <- paste0(out_share, "/", basename(output_dir))
dir.create(out_share, showWarnings = F, recursive = F)

## read settings
get_settings(paste0(output_dir, "/settings.csv"))

## Load INLA model fit
models <- readRDS(paste0(output_dir, "/model.rds"))

## Make the predictions (separately by sex)
preds_orig <- rbindlist(lapply(1:2, function(s){
  fit <- models[[s]]
  
  # Create prediction frame
  pred_frame <- fit$.args$data # (data used to fit model)
  
  scale_by <- 100 # in the model fitting code, we scaled the log-ratio by 100 to improve
  # numerical stability. We need to undo this scaling to get the correct ratio.
  
  # get the fitted values (which are identical to the linear predictors, because
  # we use the identity link function)
  pred_frame$pred <- fit$summary.linear.predictor[, "mean"] # [fit$.args$data[,ID < 0], "mean"]  
  
  # Create predictions without the source effect
  # We need to get the source effect from the model, merge those with pred frame,
  # and subtract from linear predictor.
  # This is implemented differently depending on the specific model.
  if(model_num %in% c("form31", "form34", "form36")){
    # source effects are:
    # main effect "source_v2"
    # and interaction b/w race and source_v2, called "race_eth_code_copy"
    # constructed as follows:
    # f(race_eth_code_copy, model = "iid", replicate = as.integer(as.factor(source_v2)),
    #   hyper = list(prec = list(prior = "pc.prec", param = c(5,0.05))),
    #   constr = TRUE) 
    source_main <- as.data.table(fit$summary.random[["source_v2"]][, c("ID", "mean")])
    setnames(source_main, "ID", "source_v2")
    source_interact <- as.data.table(fit$summary.random[["race_eth_code_copy"]][, c("ID", "mean")])
    # we need to create the merge columns. The ordering of the random effects is 
    # races, repeated by source.
          # now that we know both terms in the interaction, we can figure out which each
      # row of params is
      # We first need to get all the unique levels of both terms in the data
      tmp <- unique(pred_frame[, .SD, .SDcols = c("race_eth_code", "source_v2")])
      levels <- lapply(tmp, function(x) if(is.factor(x)) levels(x) else unique(x))
      ids <- as.data.table(cbind(
        race_eth_code = rep(levels[[1]], times = length(levels[[2]])), 
        source_v2 = rep(levels[[2]], each = length(levels[[1]]))
      ))
      # check that ids is unique
      stopifnot(ids[, .N] == unique(ids)[, .N])
      # check that ids has the same number of rows as params
      stopifnot(nrow(ids) == nrow(source_interact))
      # merge the ids with the params
      source_interact <- cbind(ids, source_interact)
          
      # create a new pred frame for the "no source"
      pred_frame_no_source <- copy(pred_frame)
      # merge source effects with pred_frame
      pred_frame_no_source[source_main, on = "source_v2", source_main := i.mean]
      pred_frame_no_source[source_interact, on = c("race_eth_code", "source_v2"), source_interact := i.mean]
      # calculate the prediction without the source effects
      pred_frame_no_source[, `:=`(source_v2 = "none", survey = "none", pred = pred - source_main - source_interact)]
      # round the predictions to 5 decimal points so that we can check for uniqueness later
      pred_frame_no_source[, pred := round(pred, 5)]
      # remove the source effects
      pred_frame_no_source[, c("source_main", "source_interact") := NULL]

      # bind the two pred frames together
      pred_frame <- rbindlist(list(pred_frame, pred_frame_no_source), use.names = T)
  } else {
    stop("Implement source removal for model ", model_num)
  }
  # Undo the scaling from the modeling script
  pred_frame$pred <- pred_frame$pred/scale_by
  
  # remove the observation values
  pred_frame[, c("log_ratio", "ID", "weights", "se_log_ratio", "scale") := NULL]
  # remove year and decade terms, and any terms with "copy" in the name
  pred_frame <- pred_frame[, !grepl("year|decade|copy|age20_2|age10_2", names(pred_frame)), with = FALSE]
  # make unique
  pred_frame <- unique(pred_frame)

    
  # add sex to the prediction frame
  pred_frame[, sex := s]

  # check that the predictions are unique by strat_vars, age20, race, source, bmi_report_percentile_bin (this is true when we don't include year in the model).
  # If this is not true, I would suspect some issue aligning the source effects with the
  # pred_frame, which would make the source-removed effects on each source different (i.e.
  # the predictor on BRFSS or Gallup should be the same after removing the source effects).
  stopifnot(pred_frame[, .N, c("age20", strat_vars, "source_v2", "survey", "bmi_report_percentile_bin")][N > 1][, .N] == 0)

    
  return(pred_frame)
}), use.names = T)

## Data cleaning

# get a numeric age variable
# load preds to get the values of age and age_var
tmp_data <- readRDS(paste0(output_dir, "/pred.rds"))
tmp_data <- unique(tmp_data[, .SD, .SDcols = unique(c("age", age_var))])
tmp_data[, age := as.numeric(as.character(age))]
age_key <- tmp_data[, .(age_start = min(age), age_end = max(age) + 4), by = age_var]

# for max age group, set the end to 124
age_key[age_start == max(age_start), age_end := 124]
setkey(age_key, age_start)

preds_orig[age_key, on = age_var, `:=`(age_start = i.age_start, age_end = i.age_end)]

# make sure that there are the same number of rows for each source.
stopifnot(preds_orig[, .N, source_v2][, uniqueN(N), .N] == 1)

preds <- copy(preds_orig)

################################################################################
# Collapse across race
################################################################################

##### Get the population proportions by age, sex, year, location

# load pop data
pop <- get_population_data(population_name = "pop_by_race_ethn_1997") # use OMB 1997 b/c the races used here are most similar to OMB 1997
# remove unnecessary columns
pop[, c("race_set", "race_label", "edu_label", "edu") := NULL]
grp_cols <- setdiff(names(pop), c("pop"))

# filter to ages >= 20 
pop <- pop[age >= 20]
# combine Asian, AIAN, NHPI, and multiracial into "other"
pop_sum <- sum(pop$pop)
pop <- rbindlist(list(
  pop[race %in% c(2, 10, 11)], # Latino, White, Black
  pop[race %in% 12:15, .(pop = sum(pop), race = 97), by = setdiff(grp_cols, "race")] # Asian, AIAN, NHPI, Multiracial
), use.names = T)
# make sure pop_sum hasn't changed
stopifnot(abs(pop_sum - sum(pop$pop)) < 0.01)

# get state and national populations
pop <- rbindlist(list(
  pop[, .(pop = sum(pop), level = "state"), by = setdiff(grp_cols, c("mcnty"))],
  pop[, .(pop = sum(pop), level = "natl"), by = setdiff(grp_cols, c("mcnty", "state"))]
), use.names = T, fill = T)
# expect that sum of pop will be 2*pop_sum
stopifnot(abs(2*pop_sum - sum(pop$pop)) < 0.01)
# check that sum of pop is the same for each level
stopifnot(pop[, .(pop = sum(pop)), by = c("level")][, uniqueN(round(pop, 2)), .N] == 1)

# create a location_ID column based on level are state
locs <- unique(pop[, .(level, area = state)])
locs[level == "natl", area := 1]
lookup_loc <- locs[, location_id := ushd.dbr::translate_area(level, area)]

setnames(pop, "state", "area")
pop[level == "natl", area := 1]
pop[lookup_loc, on = c("level", "area"), location_id := i.location_id]

# calculate the proportion of all-race pop in each race group, by age, sex, year, location
grp_cols <- setdiff(names(pop), c("pop"))
pop[, race_prop := pop/sum(pop), by = setdiff(grp_cols, "race")]
# check that sums to 1 by year, sex, age, location_id
stopifnot(pop[, sum(race_prop), by = c("year", "sex", "age", "location_id")][abs(V1 - 1) > 1e-9, .N] == 0)
setkey(age_key, age_start)
# create age-groups for merging with the preds.
pop[, age_var := cut(age, breaks = c(age_key$age_start, 124), labels = as.character(age_key[[age_var]]), right = F)]
setnames(pop, "age_var", age_var)
# Create a race_eth_code column for merging with preds
setnames(pop, "race",  "race_eth_code")
pop[, race_eth_code := as.factor(race_eth_code)]

##### Merge preds with pop proportions
# this will expand preds greatly -- this will create a row for each age group, even
# though preds only vary by age_var group. After collapsing by race, there will
# be slight differences in all-race values by age group b/c of different population
# proportions.
preds <- merge(preds, pop, by = c("sex", "race_eth_code", age_var), allow.cartesian = T)
# remove rows for source_v2 == "BRFSS_LLCP" & year < 2011 or
#  source_v2 == "BRFSS_LANDLINE_ONLY" & year >= 2011
# because these sources are inherently restricted to certain years.
# For Gallup, remove years < 2008 b/c Gallup starts in 2008
preds <- preds[!(source_v2 == "BRFSS_LLCP" & year < 2011) & !(source_v2 == "BRFSS_LANDLINE_ONLY" & year >= 2011) & !(source_v2 == "Gallup" & year < 2008)]

##### Collapse to create all-race ratios

preds[, `:=`(log_ratio = pred, ratio = exp(pred))]

# create the all-race version by taking the weighted mean of ratio 
# using RATIO not LOG_RATIO
grp_col <- setdiff(names(preds), c("pred", "pop", "race_prop", "log_ratio", "ratio"))
preds <- rbindlist(list(
  preds[, .(ratio = weighted.mean(ratio, w = race_prop), race_eth_code = 1), by = setdiff(grp_col, "race_eth_code")],
  preds[, -c("pred", "pop", "race_prop", "log_ratio"), with = F]
), fill = T, use.names = T)

################################################################################
# Save
################################################################################

# save preds to the LU dir and the share dir
saveRDS(preds, paste0(output_dir, "/correction_ratio.rds"))

# clean up for sharing with GBD
preds_shared <- copy(preds)[race_eth_code == 1]
# use ID names
setnames(preds_shared, c("sex", "year"), c("sex_id", "year_id"))
# remove unneeded columns
preds_shared[, c("source_v2", "race_eth_code") := NULL]
saveRDS(preds_shared, paste0(out_share, "/correction_ratio.rds"))

# copy the settings file from the input dir to the share dir
file.copy(paste0(output_dir, "/settings.csv"), paste0(out_share, "/settings.csv"), overwrite = T)

# create a readme message

readme <- sprintf(
      "Ratios are based on the USHD CW model in %s.
   The 'ratio' column is the predicted ratio of measured to self-reported BMI. Predicted BMI = self-reported BMI microdata x ratio.
   Source-specific corrections produced for BRFSS and Gallup. Other source can be corrected using the ratios in rows where the source is 'none'.
   Note that these ratios may not be appropriate to apply to NHANES and other non-telephone surveys.
   Produced on %s.",
  output_dir, format(Sys.time(), "%Y_%m_%d_%H_%M")
)
write(readme, file = paste0(out_share, "/README.txt"))

################################################################################
# Vetting
################################################################################

pdf(paste0(out_share, "/correction_ratios.pdf"), width = 14, height = 9)
# Look at:
# How the ratios change over time, by age, race, location, source, and bmi_report_percentile_bin
# heat map of ratio by age x quantile, facet by source & years 2000, 2010, 2019
for(s in 1:2){
  p <- ggplot(preds[level == "natl" & race_eth_code == 1 & year %in% c(2000, 2010, 2019) & sex == 1], aes(x = age, y = bmi_report_percentile_bin, fill = ratio)) +
    facet_wrap(~year + survey, labeller = "label_both", ncol = 3) +
    geom_tile() +
    scale_fill_viridis_c() +
    labs(title = "Ratio of measured to self-reported BMI by age and quantile",
         subtitle = paste("Sex", s, ". National, all-races"))
  print(p)
}


for(q in sort(unique(preds$bmi_report_percentile_bin))){
  p <- ggplot(preds[level == "natl" & race_eth_code == 1 & bmi_report_percentile_bin == q], aes(x = year, y = ratio, color = age)) +
    geom_point(shape = 1) +
    geom_line(aes(group = age)) +
    facet_wrap(~sex + source_v2, ncol = 4)+
    scale_color_viridis_c() +
    theme_bw() +
    labs(title = sprintf("Ratio of measured to self-reported BMI by year in quantile %s (of %s). National, all-races", q, max(preds$bmi_report_percentile_bin)))
  print(p)

  # same plot, but make age x-axis and color by year
  p <- ggplot(preds[level == "natl" & race_eth_code == 1 & bmi_report_percentile_bin == q], aes(x = age, y = ratio, color = year)) +
    geom_point(shape = 1) +
    geom_line(aes(group = year)) +
    facet_wrap(~sex + source_v2, ncol = 4) +
    scale_color_viridis_c() +
    theme_bw() +
    labs(title = sprintf("Ratio of measured to self-reported BMI by age in quantile %s (of %s). National, all-races", q, max(preds$bmi_report_percentile_bin)))
  print(p)
}
  

# look at the ratio across bmi_report_percentiles
for(yr in c(2000, 2005, 2010, 2015, 2019)){
  p <- ggplot(preds[level == "natl" & race_eth_code == 1 & year == yr], aes(x = as.integer(bmi_report_percentile_bin), y = ratio, color = age)) +
    geom_point(shape = 1) +
    geom_line(aes(group = age)) +
    facet_wrap(~sex + source_v2, ncol = 3)+
    scale_color_viridis_c() +
    theme_bw() +
    labs(title = sprintf("Ratio of measured to self-reported BMI by quantile in %s. National, all-races", yr))
  print(p)
}  

dev.off()

# How do the all-race ratios change over time.
# Start by looking just at natl

tmp <- preds[level == "natl" & race_eth_code == 1]
stopifnot(tmp[, .N, .(year, source_v2, bmi_report_percentile_bin, age, sex)][N != 1, .N] == 0)
# check that there is only one row per age, year, source, bmi_report_percentile_bin, sex
for(a in unique(tmp$age)){
  p <- ggplot(tmp[age == a], aes(x = year, y = bmi_report_percentile_bin, fill = ratio)) +
    facet_wrap(~sex + survey, labeller = "label_both") +
    geom_tile() +
    scale_fill_viridis_c() +
    labs(title = "Ratio of measured to self-reported BMI by year and quantile",
         subtitle = paste("Age group", a, ". National, all-races"))
  print(p)
}  



# Apply these ratios to the microdata and see how the results differ if we use
# the all-race ratios or race-specific ones

# this is saved from the CW script
micro_full <- readRDS(paste0(output_dir, "/pred.rds"))
keep_vars <- c("year", "bridged_race", "race_eth_code", "svyyear", "sex", "age", "age10", "age20", "age_continuous", "wt", "NID", "nid", "bmi_measure", "bmi_report", "survey", "source_v2", "race77", "state", "bmi_report_percentile_bin", "year_center", paste0("pred_bmi_", 1:10))
micro <- micro_full[, ..keep_vars][survey != "nhanes" & year >= 2000]

# remove rows with missing state or NA self-reported BMI
micro <- micro[!is.na(state) & !is.na(bmi_report)]

micro[, sex := as.integer(sex)]
micro[, age := as.integer(as.character(age))]
# calculate mean BMI predicted from the CW script
micro[, pred_bmi_mean_v1 := rowMeans(.SD, na.rm = T), .SDcols = paste0("pred_bmi_", 1:10)]
micro[, paste0("pred_bmi_", 1:10) := NULL]

# create a copy of "micro" for BRFSS, but make the source called "none"
# This allows use to compare the ratio in the generic source using race-specific
# and all-race ratios.
micro <- rbindlist(list(
  micro, 
  copy(micro[survey == "brfss"])[, `:=`(source_v2 = "none", survey = "none", pred_bmi_mean_v1 = NA)]
), use.names = T)

# then recalculate mean BMI use the race-specific ratios and all-race ratios
# merge on the race-specific ratios
micro[preds[level == "state" & race_eth_code != 1], on = c("year", "sex", "age", "source_v2", "bmi_report_percentile_bin", "state" = "area", "race_eth_code"), ratio_race_specific := i.ratio]
# merge on the all-race ratios
micro[preds[level == "state" & race_eth_code == 1], on = c("year", "sex", "age", "source_v2", "bmi_report_percentile_bin", "state" = "area"), ratio_all_race := i.ratio]

# calculate the new version of pred_bmi
micro[, pred_bmi_mean_v2 := bmi_report * ratio_race_specific]
micro[, pred_bmi_mean_v3 := bmi_report * ratio_all_race]

# make sure there are no NAs
micro[is.na(pred_bmi_mean_v2)]
micro[is.na(pred_bmi_mean_v3)]

### then collapse the microdata by the various measures
# use the means_by and prev_by functions defined in cw_time_trend_helper_funs.R
# These scripts assume that the BMI values are in the columns pred_bmi_1, ..., pred_bmi_10
# So I will rename the various pred bmi columns temporarily
setnames(micro, c("pred_bmi_mean_v1", "pred_bmi_mean_v2", "pred_bmi_mean_v3"), c("pred_bmi_1", "pred_bmi_2", "pred_bmi_3"))
micro[, paste0("pred_bmi_", 4:10) := NULL]

collapse_results <- function(data, col_by, n_preds = 3){
  means <- means_by(data, by = col_by, use_svyyear = FALSE, by_version = FALSE, use_raked_weights = F, n_preds = n_preds)
  prev_overwt <- prev_by(data, type = "overweight", by = col_by, use_svyyear = FALSE, by_version = FALSE, use_raked_weights = F, n_preds = n_preds)
  prev_obese <- prev_by(data, type = "obese", by = col_by, use_svyyear = FALSE, by_version = FALSE, use_raked_weights = F, n_preds = n_preds)
  # rbindlist these
  results <- rbindlist(list(
    means[, measure := "mean_bmi"], 
    prev_overwt[, measure := "prev_overwt"], 
    prev_obese[, measure := "prev_obese"]),
    use.names = T)
  
  # rename pred_bmi_1, ..., pred_bmi_3 to reflect their actual meaning
  setnames(results, c("pred_bmi_1", "pred_bmi_2", "pred_bmi_3"), c("pred_bmi_ushd", "pred_bmi_race_ratio", "pred_bmi_mean_all_race_ratio"))
  
  # now melt the results by the type column
  results <- melt(results, id.vars = unique(c(col_by, "survey", "measure", "bmi_report")), measure.vars = patterns("pred_bmi"), value.name = "pred_bmi")

  return(results)
}

# summarize by year, sex, source_v2:
results1 <- collapse_results(micro, col_by = c("year", "sex", "source_v2"))
results1[, `:=`(race_eth_code = 1, level = "natl", area = 1, age = 98)]

# And then do the same, but by state
results2 <- collapse_results(micro, col_by = c("year", "sex", "source_v2", "state"))
results2[, `:=`(race_eth_code = 1, level = "state", area = state, state = NULL, age = 98)]

# And then the same, but by race
results3 <- collapse_results(micro, col_by = c("year", "sex", "source_v2", "race_eth_code"))
results3[, `:=`(level = "natl", area = 1, age = 98)]

# Create all-race, by age
results4 <- collapse_results(micro, col_by = c("year", "sex", "source_v2", "age"))
results4[, `:=`(race_eth_code = 1, level = "natl", area = 1)]

# combine the results
results <- rbindlist(list(results1, results2, results3, results4), use.names = T)
# also calculate the ratio of the pred to self-reported
results[, ratio := pred_bmi/bmi_report]

######### Plots
pdf(paste0(out_share, "/check_ratios_summary.pdf"), width = 14, height = 9)
# plot the time trends
for(meas in unique(results$measure)){
  p <- ggplot(results[level == "natl" & race_eth_code == 1 & measure == meas & age == 98], aes(x = year, y = pred_bmi, color = variable)) +
    geom_line(aes(group = interaction(variable, source_v2))) +
    facet_wrap(~sex + survey, labeller = "label_both") +
    labs(subtitle = meas, title = "Adjusted BMI over time")
  print(p)
}

# plot the ratio
for(meas in unique(results$measure)){
  p <- ggplot(results[measure == meas & level == "natl" & race_eth_code == 1 & age == 98], aes(x = year, y = ratio, color = variable)) +
    geom_line(aes(group = interaction(variable, source_v2))) +
    facet_wrap(~sex + survey, labeller = "label_both")+
    labs(title = "Self-report correction ratios by different methods. Natl, all ages, all races",
      subtitle = meas)
  print(p)
}

# Look at age-specific
for(meas in unique(results$measure)){
  for(s in 1:2){
    p <- ggplot(results[measure == meas & level == "natl" & race_eth_code == 1 & age != 98 & sex == s], aes(x = year, y = ratio, color = variable)) +
      geom_line(aes(group = interaction(variable, source_v2))) +
      facet_wrap(~age + survey, labeller = "label_both")+
      labs(title = "Self-report correction ratios by different methods. Natl, all races",
        subtitle = sprintf("%s sex %s", meas, s))
    print(p)
    p <- ggplot(results[measure == meas & level == "natl" & race_eth_code == 1 & age != 98 & sex == s], aes(x = as.numeric(as.character(age)), y = ratio, color = variable)) +
      geom_line(aes(group = interaction(variable, source_v2))) +
      facet_wrap(~year + survey, labeller = "label_both")+
      labs(title = "Self-report correction ratios by different methods. Natl, all races",
        subtitle = sprintf("%s sex %s", meas, s))
    print(p)
  }
  
}

# Look at race-specific
for(meas in unique(results$measure)){
  for(s in 1:2){
    p <- ggplot(results[measure == meas & level == "natl" & sex == s & age == 98], aes(x = year, y = ratio, color = variable)) +
      geom_line(aes(group = interaction(variable, source_v2))) +
      facet_grid(survey ~ race_eth_code, labeller = "label_both") +
      labs(title = "Self-report correction ratios by different methods -- Natl, all ages, by race",
        subtitle = sprintf("%s, sex %s", meas, s))
    print(p)
  }
}

# look at state level
for(meas in unique(results$measure)){
  for(loc in unique(results$area)){
    p <- ggplot(results[measure == meas & level == "state" & area == loc & race_eth_code == 1 & age == 98], aes(x = year, y = ratio, color = variable)) +
      geom_line(aes(group = interaction(variable, source_v2))) +
      facet_grid(sex ~ survey, labeller = "label_both") +
      labs(title = "Self-report correction ratios by different methods -- all ages, all races: state-level",
        subtitle = sprintf("%s, state %s. Collapsed by age", meas, loc))
    print(p)
  }
}
dev.off()

# Make scatter plots comparing the SR ratio using the pred_bmi_race_ratio and pred_bmi_mean_all_race_ratio methods
# Use all-races combined for these plots
# make results wide
results_wide <- dcast(results[race_eth_code == 1], year + sex + age + source_v2 + survey + measure + race_eth_code + level + area ~ variable, value.var = "ratio")
pdf(paste0(out_share, "/scatter_plots.pdf"), width = 14, height = 9)
for(meas in unique(results_wide$measure)){
  for(s in 1:2){
    p <- ggplot(results_wide[measure == meas & level == "natl" & sex == s],
                  aes(x = pred_bmi_race_ratio, y = pred_bmi_mean_all_race_ratio, color = year)) + 
          geom_point() +
          geom_abline() +
          coord_equal() +
          theme_bw() +
          facet_wrap(~survey) +
          labs(title = "Comparison of self-report correction ratios by different methods,  natl",
               subtitle = sprintf("%s sex %s", meas, s))
    print(p)

    # make state-level plots
    p <- ggplot(results_wide[measure == meas & level == "state" & sex == s], 
                aes(x = pred_bmi_race_ratio, y = pred_bmi_mean_all_race_ratio, color = year)) +
          geom_point() +
          geom_abline() +
          coord_equal() +
          theme_bw() +
          facet_wrap(~survey) +
          labs(title = "Comparison of self-report correction ratios by different methods",
               subtitle = sprintf("%s state-level, sex %s", meas, s))
    print(p)
  }
}
dev.off()
