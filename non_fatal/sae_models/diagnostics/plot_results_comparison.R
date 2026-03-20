####################################################################################################
## Description: Make plots of nonfatal data and estimates. Option to plot just 1 model alone,
##              a comparison of 2 models, or just the comparison.
##
## Passed args: plot_results_other_level -- set in settings by to optionally plot by  
##                puma_mcnty or cbsa_mcnty in addition to natl, state  and mcnty
##              output_dir [character] -- file path to look for settings and estimates for model 1
##              dir2 [character list] -- file path(s) to look for settings and estimates for comparison
##                models()   
##              v1 [character] -- label for model 1; it does not matter what this is
##              v2 [character list] -- label(s) for comparison model(s); it does not matter what this is
##              plot_types [character] -- either standard, compare, or standardxxcompare;
##                theast option gets split into the individual terms, and it indicates
##                that the user wants to plot both types of plots (xx is just there for nice
##                script submission)
##              apply_pop_restrictions [logical] -- whether to apply population restrictions to model estimates
##                based on population restrictions to direct estimates. If selected, this will load
##                the model estimated produced by the raking script (plot_point_estimates_and_data_rake_svyyweights.R),
##                because the appropriate restrictions were applied in that case. It will only take
##                the post-stratification weighted model estimates (not the direct estimates),
##                so it's not actually raked.
##
## Requires:    Cases and population input data (data_file and pop_file)
##              Pre-raking rate estimates ("[dir]/est_all.rds")
##              Post-raking rate estimates ("[dir]/est_all_raked.rds"). Note that
##                pre-raking estimates will be duplicated if post-raking estimates are not
##                available.
##
## Outputs:     If running just standard plots:
##                Data and estimates plots at the national level ("[dir]/est_natl.pdf")
##                Data and estimates plots at the state level ("[dir]/est_state.pdf")
##                Data and estimates by age/aggregated across time plots at the mcnty level for 
##                  all mcnties by population ("[dir]/est_mcnty.pdf")
##                Data and estimates by age/aggregated across time plots at the mcnty level for 
##                  all mcnties by population ("[dir]/est_medium_mcnty.pdf")
##                Data and estimates by year/aggregated across age plots at the mcnty level for 
##                  all mcnties by population ("[dir]/estby_year_mcnty.pdf")
##                Data and estimates by year/aggregated across age plots at the mcnty level for 
##                  all mcnties by population ("[dir]/estby_year_medium_mcnty.pdf")
##
##              If running comparison plots (if more than one comparison model, each of these is created
##                for each comparison model), then same plots are made for comparison model. 
##
##              If running both sets of plots, then all of the above plots are created
####################################################################################################

###### Load required libraries
pacman:::p_load(R.utils, data.table, ggplot2, scales, RColorBrewer, gplots, gridExtra, ggpubr, stringr)
library(ggblend, lib.loc = "FILEPATH")

###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (settings_loc  <- commandArgs(TRUE)[[3]])
  (comparison_dir <- commandArgs(TRUE)[[4]])
  (output_dir_draws_est <- commandArgs(TRUE)[[5]])
  (imp <- commandArgs(TRUE)[[6]])
  (conditional <- commandArgs(TRUE)[[7]])
  (apply_pop_restrictions <- as.logical(commandArgs(TRUE)[[8]]))
}

race <- 99

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs[funcs != "load_sae_shared.R"]) {
  source(paste0(repo, "/functions/", func))
}

###### Assign settings from settings file
get_settings(settings_loc)

#### Get mcnty:state mapping
loc <- fread("FILEPATH")

v1 <- "new_results"

if (!exists("comparison_rundate")) {
  comparison_rundate <- NULL
}

# if apply_pop_restrictions is TRUE, create the alternative directory to load in the results.
if (apply_pop_restrictions) {
  tmp_est_dir <- paste0(output_dir, "/vetting/rake_svy_weights")
  
  # Also set by_source to TRUE. We do this because the restrictions vary by source,
  #   so we need to create sets of the model estimates for each source.
  # HOWEVER, by_source is typically used to generate predictions separately for each source,
  #   which includes predicting out on the non-gold standard random effects. This is
  #   not necessarily the case here, so it's important to proceed cautiously.
  # by_source <- TRUE
} else {
  # otherwise, use the standard location
  tmp_est_dir <- output_dir_draws_est
}

if (exists("mean_model")) {
  new_label <- paste0(run_date, "_model", mean_model, " (new)")
} else {
  new_label <- paste0(run_date, " (new)")
}

if (is.null(comparison_rundate)) {
  runs <- vector(mode = "list", length = 1)
  runs[[1]] <- data.table("run_date" = run_date, "version" = new_label, output_dir_draws_est = tmp_est_dir)
} else {
  # check if modelXXX is in the comparison rundate, otherwise the comparison dir was not correctly specified in the settings file
  if (!grepl("model", comparison_rundate)) {
    stop("The comparison rundate was not correctly specified in the settings file.\nNeed to specify 'rundatemodelXXX'")
  }
  runs <- vector(mode = "list", length = 2)
  runs[[1]] <- data.table("run_date" = run_date, "version" = new_label, output_dir_draws_est = tmp_est_dir)
  if (exists("mean_model")) {
    runs[[2]] <- data.table("run_date" = comparison_rundate, "version" = comparison_rundate, output_dir_draws_est = str_replace(tmp_est_dir, paste0(run_date, "_model", mean_model), comparison_rundate))
  } else {
    runs[[2]] <- data.table("run_date" = comparison_rundate, "version" = comparison_rundate, output_dir_draws_est = str_replace(tmp_est_dir, paste0(run_date), comparison_rundate))
  }
  # if pred_est_all.rds does not exist in the comparison dir, set runs[[2]] to NULL, change comparison_rundate to NULL, and give a warning
  if (!file.exists(paste0(runs[[2]]$output_dir_draws_est, "/pred_est_all.rds"))) {
    warning("Comparison directory does not contain pred_est_all.rds. Will not compare models.")
    runs[[2]] <- NULL
    comparison_rundate <- NULL
  }
}

natl_level <- "natl"
state_level <- "state"
mcnty_level <- "mcnty"

if (!exists("plot_results_other_level")) {
  plot_results_other_level <- NULL
}

# if n.imp > 0 but imp == 0 (i.e., collapse imp), set plot_results_other_level to NULL
# b/c we don't have a data.rds file in the imputation directory
if (n.imp > 0 & imp == 0) {
  plot_results_other_level <- NULL
}

if (!exists("direct_estimates_file")) {
  direct_estimates_file <- NULL
}

if (!exists("family") && outcome[1] == "pred_bmi") {
  y_label <- "Mean BMI"
} else if (outcome[1] == "mds_pc") {
  y_label <- "MDs per Capita"
} else if (family == "Poisson") {
  y_label <- "Incidence Rate"
} else if (family == "binomial") {
  y_label <- "Prevalence"
} else {
  y_label <- outcome_string
}

#### Set plotting palette
type_color <- c("Latino" = hue_pal()(6)[5], "AIAN" = hue_pal()(6)[3], "NH AIAN" = hue_pal()(6)[3], "Black" = hue_pal()(6)[2], "NH Black" = hue_pal()(6)[2], "API" = hue_pal()(6)[4], "Asian" = hue_pal()(6)[4], "NH Asian" = hue_pal()(6)[4], "NH API" = hue_pal()(6)[4], "White" = hue_pal()(6)[1], "NH White" = hue_pal()(6)[1], "All races" = hue_pal()(6)[6])

## Aggregate data to produce direct estimates
agg_data <- function(dt, include_GBD_estimates = FALSE) {
  #### Set indicator for original data
  dt$original <- 1
  
  #### Load and merge population data
  if (file.exists(paste0(output_dir_draws_est, "/population.rds"))) {
    pop <- readRDS(paste0(output_dir_draws_est, "/population.rds"))
  } else {
    pop <- readRDS(pop_file)
  }
  
  #### Deal with mcnty- and state-level data separately
  dt$race <- as.numeric(as.character(dt$race))
  dt[, prev := cases / sample_size]
  
  #### Recode
  recode <- readRDS(paste0(output_dir, "/recode.rds"))
  
  dt <- merge(dt, recode$sex[, -c("var")], by.x = "sex", by.y = "sex_recode", all.x = TRUE)
  dt <- merge(dt, recode$age[, -c("var")], by.x = "age", by.y = "age_recode", all.x = TRUE)
  dt <- merge(dt, recode$race[, -c("var")], by.x = "race", by.y = "race_recode", all.x = TRUE)
  dt <- merge(dt, recode$year[, -c("var")], by.x = "year", by.y = "year_recode", all.x = TRUE)
  dt <- merge(dt, recode$area[, -c("var")], by.x = "area", by.y = "area_recode", all.x = TRUE)
  dt[, c("sex", "age", "race", "year") := list(sex.y, age.y, race.y, year.y)]
  dt[, c("sex.y", "age.y", "race.y", "year.y") := NULL]
  
  dt <- merge(dt[!is.na(mcnty), -c("state")], pop, by = c("mcnty", "year", "sex", "age", "race"), all.x = TRUE)
  
  if ("pop.x" %in% colnames(dt)) {
    dt[, "pop" := pop.x]
    dt[is.na(pop.x), pop := pop.y]
  }
  dt[is.na(agg_wt), "agg_wt" := 1]
  dt[, pop := pop * agg_wt]
  
  # if the source_v2 column in in dt, use that instead of source
  if("source_v2" %in% colnames(dt)){
    dt[, source := source_v2]
  }
  
  #### Aggregate data by sex
  dt <- rbindlist(list(dt, dt[, list(sex = 3, prev = weighted.mean(prev, pop, na.rm = TRUE), cases = sum(cases, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE)), by = c("area", "year", "race", "state", "source", "age", "level")]), use.names = TRUE, fill = TRUE)
  
  #### Aggregate data by race
  dt <- rbindlist(list(dt, dt[, list(race = 1, prev = weighted.mean(prev, pop, na.rm = TRUE), cases = sum(cases, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE)), by = c("area", "year", "sex", "source", "state", "age", "level")]), use.names = TRUE, fill = TRUE)
  
  #### Aggregate data by age
  dt <- rbindlist(list(dt, dt[, list(age = 98, prev = weighted.mean(prev, pop, na.rm = TRUE), cases = sum(cases, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE)), by = c("area", "year", "sex", "source", "state", "race", "level")]), use.names = TRUE, fill = TRUE)
  
  #### Aggregate data by geography (state) for mcnty-level data only
  dt[, level := "mcnty"]
  dt <- rbindlist(list(dt, dt[level == "mcnty", list(area = state, level = "state", prev = weighted.mean(prev, pop, na.rm = TRUE), cases = sum(cases, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE)), by = c("year", "sex", "source", "state", "race", "age")]), use.names = TRUE, fill = TRUE)
  
  #### Aggregate data by geography (national)
  dt <- rbindlist(list(dt, dt[level == "state", list(area = 1, level = "natl", prev = weighted.mean(prev, pop, na.rm = TRUE), cases = sum(cases, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), pop = sum(pop, na.rm = TRUE)), by = c("year", "sex", "source", "race", "age")]), use.names = TRUE, fill = TRUE)
  
  if (include_GBD_estimates) {
    source("FILEPATH")
    source("FILEPATH")
    
    gbd_round_id <- 6
    
    #### Get location metadata
    location_metadata <- get_location_metadata(gbd_round_id = gbd_round_id, location_set_id = 35, decomp_step = "iterative")
    
    #### Restrict location metadata to United States (national and state-level)
    usa_location_metadata <- location_metadata[grep("USA", ihme_loc_id)]
    
    #### Get age groups
    age_groups <- fread("FILEPATH")
    
    #### Get prevalence estimates from GBD
    gbd_outputs <- get_outputs(topic = "cause", gbd_round_id = gbd_round_id, decomp_step = "step5", compare_version_id = 7244, measure_id = measure_id, metric = 3, location_id = unique(usa_location_metadata$location_id), cause_id = cause_id, cause_set_id = 2, year_id = years, age_group_id = c(22, unique(age_groups[age_start %in% ages, age_group_id])), sex_id = c(sexes, 3))
    
    #### Merge location metadata
    gbd_outputs <- merge(gbd_outputs, unique(loc[, c("state_name", "state")]), by.x = "location_name", by.y = "state_name", all.x = TRUE)
    
    #### Merge age metadata
    gbd_outputs <- merge(gbd_outputs, age_groups[, c("age_group_id", "age_start")], by = "age_group_id", all.x = TRUE)
    gbd_outputs[age_group_id == 22, age_start := 98] # Set all age estimates to age group 98
    
    #### Set area, level, and other variables
    gbd_outputs[location_id == 102, c("area", "level") := list(1, "natl")]
    gbd_outputs[location_id != 102, c("area", "level") := list(state, "state")]
    gbd_outputs[, c("race", "source", "sex", "sex_id") := list(1, "GBD", sex_id, NULL)]
    
    #### Harmonize format with dt
    setnames(gbd_outputs, c("val", "year_id", "age_start"), c("prev", "year", "age"))
    
    #### Append to dt objects
    dt <- rbindlist(list(dt, gbd_outputs), use.names = TRUE, fill = TRUE)
  }
  
  # return the list of all estimates
  return(dt)
}

## Define function to combine/aggregate estimates
agg_est <- function(dir, label, output_dir_current, type = "Full Estimate") {
  # load estimates (both pre- and post-raking, if available)
  if (apply_pop_restrictions) {
    load_est <- function(dir) {
      load(paste0(dir, "/ests_post_raking_aggregation.Rdata"))
      est <- direct_estimates_raked # this is the object loaded in the  .Rdata file that contains estimates
      # subset to all-edu and all-marital
      est <- est[edu == 9 & marital == 9]
      
      # keep the columns we need for merging:
      keep_col <- c("age", "year", "race", "sex", "edu", "marital", "source", "level", "area", "pred")
      est <- unique(est[, ..keep_col])
      # update the prediction names to match what's in pred_est_all
      # We don't have the UI for the results loaded here, so just set them to the mean est
      setnames(est, "pred", "pred_mean")
      est[, c("pred_lb", "pred_ub", "pred_se") := list(pred_mean, pred_mean, 0)]
      
      # also change how the age vars are coded
      est[, age := fcase(age == "age_std_20+", 99L,
                         age == "20+", 98L,
                         age == "80+", 83L,
                         age != "80+", as.integer(age))]
      
      return(est)
    }
    est <- load_est(output_dir_current)
  } else{
    est <- readRDS(paste0(output_dir_current, "/pred_est_all.rds"))
    # if source_v2 is in est, use that instead of source
    if ("source_v2" %in% colnames(est)) {
      est[, source := source_v2]
    }
  }
  
  if (isTRUE(conditional) | conditional == 'TRUE') {
    if (outcome[[1]] %like% c('obese')) {
      if(apply_pop_restrictions) stop("Script not updated yet for obesity results when apply_pop_restrictions is TRUE")
      est_overwt <- readRDS(paste0("FILEPATH", draws_overwt, "/imputation0/est/pred_est_all.rds"))
      setnames(est_overwt, c('pred_mean', 'pred_lb', 'pred_ub'), c('pred_mean_overwt', 'pred_lb_overwt', 'pred_ub_overwt'))
      est <- merge (est, est_overwt[, !c('pred_median', 'pred_se')], by=c('level','area','year','sex','race','age'))
      est[, `:=` (pred_mean=pred_mean/pred_mean_overwt, pred_lb=pred_lb/pred_lb_overwt, pred_ub=pred_ub/pred_ub_overwt)]
      est[, c('pred_mean_overwt', 'pred_lb_overwt', 'pred_ub_overwt'):= NULL]
    }
  }
  
  if (outcome[1] != "pred_bmi") {
    #### Load recode mapping
    recode <- readRDS(paste0(output_dir, "/recode.rds"))$area
    
    #### Convert area to mcnty
    est_mcnty <- merge(est[level == "mcnty"], recode, by.x = "area", by.y = "area_recode")
    est_mcnty[, c("area", "mcnty", "var") := list(mcnty, NULL, NULL)]
    
    est <- rbindlist(list(est_mcnty, est[level != "mcnty"]), use.names = TRUE, fill = TRUE)
  }
  
  #### Sort
  setkeyv(est, c("level", "area", "year", "sex", "race", "age"))
  
  #### Set type and version
  est[, c("type", "version", "estimate_type") := list("Estimates (Unraked)", label, type)]
  
  # return the list of all estimates
  return(est)
}


## Define functions for data and estimates plots ---------------------------------------------------

# Plot all ages estimates for a given area-race, by sex and year
plot_by_sex_year <- function(fest, fdirect, title, line_shape_setting) {
  fest[, Sex := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both"))]
  fest[, age := factor(age, levels = c("98", "99"), labels = c("All ages", "Age-standardized"))]
  fdirect[, Sex := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both"))]
  fdirect[, age := factor(age, levels = c("98", "99"), labels = c("All ages", "Age-standardized"))]
  
  if (!(length(unique(direct_estimates_stand$sex)) == 1 & unique(direct_estimates_stand$sex)[1] == 3)) {
    fest <- fest[sex != 3]
    fdirect <- fdirect[sex != 3]
  }
  
  g <- ggplot() + theme_bw() +
    geom_ribbon(data = fest[version == new_label], aes(x = year, ymin = pred_lb, ymax = pred_ub, fill = Sex), size = 0.1, alpha = 0.1) +
    geom_point(data = fdirect[source != "GBD" & !is.na(sex) & (imp == 0 | is.na(imp))], aes(x = year, y = prev, color = Sex, shape = as.factor(source), group = imp, size = sample_size), alpha = 0.5) +
    geom_line(data = fest, aes(x = year, y = pred_mean, color = Sex, linetype = version)) +
    facet_grid(source ~ age, scales = "free_y") +
    scale_size_area() +
    labs(x = "Year", y = y_label, color = "Sex", fill = "Sex", linetype = "Type", shape = "Source",
         title = title) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) + xlim(min(years), max(years))
  g
}

# Plot all ages estimates for a given area, by race and year
plot_by_race_year <- function(fest, fdirect, title, line_shape_setting) {
  fest[, race := factor(race, levels = c(races, 1), labels = c(race_labels, "All races"))]
  fest[, age := factor(age, levels = c(98, 99), labels = c("All ages", "Age-standardized"))]
  fest[, Sex := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both"))]
  
  fdirect[, race := factor(race, levels = c(races, 1), labels = c(race_labels, "All races"))]
  fdirect[, age := factor(age, levels = c(98, 99), labels = c("All ages", "Age-standardized"))]
  fdirect[, Sex := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both"))]
  
  g <- ggplot() + theme_bw() +
    facet_grid(source ~ Sex + age, scales = "free_y") +
    geom_ribbon(data = fest[version == new_label], aes(x = year, ymin = pred_lb, ymax = pred_ub, fill = race), size = 0.05, alpha = 0.1) +
    geom_point(data = fdirect[source != "GBD" & !is.na(sex) & (imp == 0 | is.na(imp))], aes(x = year, y = prev, color = race, shape = as.factor(source), size = sample_size), alpha = 0.5)  +
    geom_line(data = fest, aes(x = year, y = pred_mean, color = race, linetype = version)) +
    labs(x = "Year", y = y_label, color = "Race", fill = "Race", linetype = "Estimate Type", shape = "Source",
         title = title) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) + xlim(min(years), max(years)) +
    scale_size_area() + 
    scale_color_manual(values = type_color) + scale_fill_manual(values = type_color)
  g
}

# Plot all ages estimates for a given area, by race and year
plot_by_race_age_year <- function(fest, fdirect, title, line_shape_setting) {
  fest[, race := factor(race, levels = c(races, 1), labels = c(race_labels, "All races"))]
  fest[, Sex := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both"))]
  fdirect[, race := factor(race, levels = c(races, 1), labels = c(race_labels, "All races"))]
  fdirect[, Sex := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both"))]
  
  if (!(length(unique(direct_estimates_stand$sex)) == 1 & unique(direct_estimates_stand$sex)[1] == 3)) {
    fest <- fest[sex != 3]
    fdirect <- fdirect[sex != 3]
  }
  
  g <- ggplot() + theme_bw() +
    facet_grid(source ~ Sex + year, scales = "free_y") +
    geom_ribbon(data = fest[age != "80+" & version == new_label], aes(x = as.numeric(as.character(age)), ymin = pred_lb, ymax = pred_ub, fill = race), size = 0.05, alpha = 0.1) +
    geom_point(data = fdirect[source != "GBD" & !is.na(sex) & age != "80+" & (imp == 0 | is.na(imp))], aes(x = as.numeric(as.character(age)), y = prev, color = race, shape = as.factor(source), size = sample_size), alpha = 0.5) +
    geom_line(data = fest[age != "80+"], aes(x = as.numeric(as.character(age)), y = pred_mean, color = race, linetype = version)) +
    labs(x = "Age", y = y_label, color = "Race", fill = "Race", linetype = "Estimate Type", shape = "Source",
         title = title, caption = "Points with black borders (age 82.5) represent ages 80+") + theme(plot.title = element_text(hjust = 0.5, face = "bold")) + #xlim(min(ages), max(ages)) +
    scale_size_area() +
    scale_color_manual(values = type_color) + scale_fill_manual(values = type_color)
  
  if (nrow(fdirect[source == "BRFSS" & !is.na(sex) & age == "80+"]) > 0) {
    g <- g + geom_point(data = fdirect[source == "BRFSS" & !is.na(sex) & age == "80+" & (imp == 0 | is.na(imp))], aes(x = 82.5, y = prev, fill = race, size = sample_size), shape = 21, color = 1, alpha = 0.2);
  }
  g
}

# Plot age-specific estimates for a given area-race, by age, sex, and year
plot_by_age_sex_year <- function(fest, fdirect, title, line_shape_setting) {
  fest[, Sex := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both"))]
  fdirect[, Sex := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both"))]
  
  if ("80+" %in% unique(fest$age)) {
    fest[, age := factor(age, levels = c(ages, "80+"))]
  }
  
  fest[, age_backup := age]
  fdirect[, age_backup := age]
  
  if (!(length(unique(direct_estimates_stand$sex)) == 1 & unique(direct_estimates_stand$sex)[1] == 3)) {
    fest <- fest[sex != 3]
    fdirect <- fdirect[sex != 3]
  }
  
  g1 <- ggplot() + theme_bw() +
    facet_wrap(source ~ age, scales = "free_y") +
    geom_ribbon(data = fest[version == new_label], aes(x = year, ymin = pred_lb, ymax = pred_ub, fill = Sex), size = 0.05, alpha = 0.05) +
    geom_point(data = fdirect[source != "GBD" & !is.na(sex) & (imp == 0 | is.na(imp))], aes(x = year, y = prev, color = Sex, shape = as.factor(source), size = sample_size), alpha = 0.5) +
    geom_line(data = fest, aes(x = year, y = pred_mean, color = Sex, linetype = version)) +
    scale_size_area() +
    labs(x = "Year", y = y_label, color = "Sex", fill = "Sex", linetype = "Estimate Type", shape = "Source",
         title = title) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) + xlim(min(years), max(years))# +
  g1
  
  g2 <- ggplot() + theme_bw() +
    facet_wrap(source ~ year) +
    geom_ribbon(data = fest[version == new_label], aes(x = as.numeric(as.character(age_backup)), ymin = pred_lb, ymax = pred_ub, fill = Sex), size = 0.05, alpha = 0.05) +
    geom_point(data = fdirect[source != "GBD" & !is.na(sex) & (imp == 0 | is.na(imp))], aes(x = as.numeric(as.character(age_backup)), y = prev, color = Sex, shape = as.factor(source), size = sample_size), alpha = 0.5) +
    geom_line(data = fest, aes(x = as.numeric(as.character(age_backup)), y = pred_mean, color = Sex, linetype = version)) +
    scale_size_area() +
    labs(x = "Age", y = y_label, color = "Sex", fill = "Sex", linetype = "Estimate Type", shape = "Source",
         title = title, caption = "Points with black borders (age 82.5) represent ages 80+") + theme(plot.title = element_text(hjust = 0.5, face = "bold"))# + xlim(min(ages), max(ages))# +
  if (nrow(fdirect[source == "BRFSS" & !is.na(sex) & age_backup == "80+"]) > 0) {
    g2 <- g2 + geom_point(data = fdirect[source == "BRFSS" & !is.na(sex) & age_backup == "80+" & (imp == 0 | is.na(imp))], aes(x = 82.5, y = prev, fill = Sex, size = sample_size), shape = 21, color = 1, alpha = 0.5)
  }
  
  g2
  
  ggarrange(g1, g2, nrow = 1, common.legend = T, legend = "right")
}

# Plot age-specific estimates for a given area-race, by age, sex, and year, for intermediate geographic levels (e.g., CBSA-mcnty or PUMA-mcnty)
plot_intermediate_level_by_age_sex_year <- function(fest, fdirect, title, line_shape_setting) {
  fest[, Sex := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both"))]
  fest[, race_name := factor(race, levels = c(races, 1), labels = c(race_labels, "All"))]
  
  fdirect[, Sex := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both"))]
  fdirect[, race_name := factor(race, levels = c(races, 1), labels = c(race_labels, "All"))]
  
  if ("80+" %in% unique(fest$age)) {
    fest[, age := factor(age, levels = c(ages, "80+"))]
  }
  
  if (!(length(unique(direct_estimates_stand$sex)) == 1 & unique(direct_estimates_stand$sex)[1] == 3)) {
    fest <- fest[sex != 3]
    fdirect <- fdirect[sex != 3]
  }
  
  fest[, age_backup := age]
  fdirect[, age_backup := age]
  
  if ("85" %in% unique(fest$age)) {
    fest[age == 85, age := "85+"]
    fest[, age := factor(age, levels = c(ages[ages != 85], "80+", "85+"))]
    fdirect[age == 85, age := "85+"]
  }
  
  if (by_source) {
    p <- ggplot() + theme_bw() + 
      facet_grid(source ~ Sex + race_name, scales = "free_y") +
      geom_point(data = fdirect[age != "80+" & source != "GBD" & !is.na(sex) & !is.na(race) & (imp == 0 | is.na(imp))], aes(x = as.numeric(as.character(age)), y = prev, shape = as.factor(source), size = sample_size, color = as.factor(year)), alpha = 0.5) +
      labs(x = "Age", y = y_label, color = "Year", linetype = line_shape_setting,
           shape = "Source", title = title) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      scale_size_area() +
      guides(fill = "none") + xlim(min(ages), max(ages))
    if (nrow(fdirect[source == "BRFSS" & !is.na(sex) & age == "80+"]) > 0) {
      p <- p + geom_point(data = fdirect[source == "BRFSS" & !is.na(sex) & age == "80+" & (imp == 0 | is.na(imp))], aes(x = 82.5, y = prev, fill = as.factor(year), shape = as.factor(source), size = sample_size), shape = 21, color = 1, alpha = 0.5)
    }
    p <- p + geom_line(data = fest[age != "80+"], aes(x = as.numeric(as.character(age)), y = pred_mean, color = as.factor(year), linetype = as.factor(source))) +
      geom_ribbon(data = fest[age != "80+" & version == new_label], aes(x = as.numeric(as.character(age)), ymin = pred_lb, ymax = pred_ub, fill = as.factor(year), linetype = as.factor(source)), alpha = 0.05)
  } else {
    p <- ggplot() + theme_bw() + 
      facet_grid(Sex ~ race_name, scales = "free_y") +
      geom_point(data = fdirect[age != "80+" & source != "GBD" & !is.na(sex) & !is.na(race) & (imp == 0 | is.na(imp))], aes(x = as.numeric(as.character(age)), y = prev, shape = as.factor(source), size = sample_size, color = as.factor(year)), alpha = 0.5) +
      labs(x = "Age", y = y_label, color = "Year", linetype = line_shape_setting,
           shape = "Source", title = title) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      scale_size_area() +
      guides(fill = "none") + xlim(min(ages), max(ages))
    if (nrow(fdirect[source == "BRFSS" & !is.na(sex) & age == "80+"]) > 0) {
      p <- p + geom_point(data = fdirect[source == "BRFSS" & !is.na(sex) & age == "80+" & (imp == 0 | is.na(imp))], aes(x = 82.5, y = prev, fill = race, size = sample_size), shape = 21, color = 1, alpha = 0.5);
    }
    p <- p + geom_line(data = fest[age != "80+"], aes(x = as.numeric(as.character(age)), y = pred_mean, color = as.factor(year), linetype = as.factor(source))) +
      geom_ribbon(data = fest[age != "80+" & version == new_label], aes(x = as.numeric(as.character(age)), ymin = pred_lb, ymax = pred_ub, fill = as.factor(year), linetype = as.factor(source)), alpha = 0.05)
  }
  p
}

plot_mcnty_trends <- function(fest, fdirect, title, line_shape_setting) {
  fest[, Sex := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both"))]
  fest[, race_name := factor(race, levels = c(races, 1), labels = c(race_labels, "All"))]
  fest <- fest[age < 98]
  
  fdirect[, Sex := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both"))]
  fdirect[, race_name := factor(race, levels = c(races, 1), labels = c(race_labels, "All"))]
  fdirect <- fdirect[age < 98]
  fdirect <- fdirect[level == "mcnty"]
  
  if ("80+" %in% unique(fest$age)) {
    fest[, age := factor(age, levels = c(ages, "80+"))]
  }
  
  fest[, age_backup := age]
  fdirect[, age_backup := age]
  
  if ("85" %in% unique(fest$age)) {
    fest[, age := factor(age, levels = c(ages[ages != 85], "80+", "85"))]
  }
  
  # if agg_wt is not a column, create a new column and set the value to 1
  if(!"agg_wt" %in% colnames(fdirect)) {
    fdirect[, agg_wt := 1]
  } else {
    # otherwise, just fill in the NA values
    fdirect[is.na(agg_wt), agg_wt := 1]
  }
  
  
  
  if (by_source) {
    if ("GBD" %in% unique(fdirect$source)) {
      p <- ggplot() + theme_bw() + 
        facet_grid(source ~ Sex + race_name, scales = "free_y") +
        geom_point(data = fdirect[age != "80+" & source != "GBD" & !is.na(sex) & !is.na(race) & (imp == 0 | is.na(imp))], aes(x = as.numeric(as.character(age)), y = prev, shape = as.factor(source), size = sample_size, color = as.factor(year)), alpha = 0.5) +
        labs(x = "Age", y = y_label, color = "Year", linetype = version,
             shape = "Source", title = title) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        scale_size_area() +
        guides(fill = "none") + xlim(min(ages), max(ages))
      if (nrow(fdirect[source == "BRFSS" & !is.na(sex) & age == "80+"]) > 0) {
        p <- p + geom_point(data = fdirect[source == "BRFSS" & !is.na(sex) & age == "80+" & (imp == 0 | is.na(imp))], aes(x = 82.5, y = prev, fill = race, size = sample_size), shape = 21, color = 1, alpha = 0.5);
      }
      p <- p + geom_line(data = fest[age != "80+"], aes(x = as.numeric(as.character(age)), y = pred_mean, color = as.factor(year), linetype = version)) +
        geom_ribbon(data = fest[age != "80+" & version == new_label], aes(x = as.numeric(as.character(age)), ymin = pred_lb, ymax = pred_ub, fill = as.factor(year)), alpha = 0.05) +
        geom_line(data = fdirect[age != "80+" & source == "GBD" & !is.na(sex) & !is.na(race)], aes(x = as.numeric(as.character(age)), y = prev, color = 1), linetype = 6, alpha = 0.5)
    } else {
      p <- ggplot() + theme_bw() + 
        facet_wrap(source ~ Sex + race_name, scales = "free_y") +
        geom_ribbon(data = fest[age != "80+" & version == new_label], aes(x = as.numeric(as.character(age)), ymin = pred_lb, ymax = pred_ub, fill = as.factor(year)), alpha = 0.05) +
        geom_point(data = fdirect[age != "80+" & source != "GBD" & !is.na(sex) & !is.na(race) & (imp == 0 | is.na(imp))], aes(x = as.numeric(as.character(age)), y = prev, shape = as.factor(source), size = sample_size, color = as.factor(year), alpha = agg_wt)) +
        labs(x = "Age", y = y_label, color = "Year", linetype = "Version",
             shape = "Source", title = title) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        scale_size_area() +
        guides(fill = "none") + xlim(min(ages), max(ages))
      if (nrow(fdirect[source == "BRFSS" & !is.na(sex) & age == "80+"]) > 0) {
        p <- p + geom_point(data = fdirect[source == "BRFSS" & !is.na(sex) & age == "80+" & (imp == 0 | is.na(imp))], aes(x = 82.5, y = prev, fill = as.factor(year), shape = as.factor(source), size = sample_size, alpha = agg_wt), shape = 21, color = 1)
      }
      p <- p + geom_line(data = fest[age != "80+"], aes(x = as.numeric(as.character(age)), y = pred_mean, color = as.factor(year), linetype = version))
    }
  } else {
    if ("GBD" %in% unique(fdirect$source)) {
      p <- ggplot() + theme_bw() + 
        facet_grid(cols = vars(race_name), rows = vars(sex)) +
        geom_point(data = fdirect[source != "GBD" & !is.na(sex) & !is.na(race) & (imp == 0 | is.na(imp))], aes(x = age, y = prev, shape = as.factor(source), size = sample_size, color = as.factor(year)), alpha = 0.5) +
        labs(x = "Age", y = y_label, color="Location", linetype = version,
             shape = "Source", title = title) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        scale_size_area() +
        guides(fill = "none") + xlim(min(ages), max(ages))
      if (nrow(fdirect[source == "BRFSS" & !is.na(sex) & age == "80+"]) > 0) {
        p <- p + geom_point(data = fdirect[source == "BRFSS" & !is.na(sex) & age == "80+" & (imp == 0 | is.na(imp))], aes(x = 82.5, y = prev, fill = as.factor(year), size = sample_size), shape = 21, color = 1, alpha = 0.5);
      }
      p <- p + geom_line(data = fest[type != "Data" & level == "mcnty" & estimate_type == "Full Estimate", ], aes(x = age, y = pred_mean, color = as.factor(year), linetype = version)) +
        geom_ribbon(data = fest[type != "Data" & level == "mcnty" & estimate_type == "Full Estimate" & version == new_label,], aes(x = age, ymin = pred_lb, ymax = pred_ub, fill = "County"), alpha = 0.05) +
        geom_line(data = fdirect[source == "GBD" & !is.na(sex) & !is.na(race)], aes(x = age, y = prev, color = 1), linetype = 6, alpha = 0.5)
    } else {
      p <- ggplot() + theme_bw() + 
        facet_grid(Sex ~ race_name, scales = "free_y") +
        geom_point(data = fdirect[age != "80+" & source != "GBD" & !is.na(sex) & !is.na(race) & (imp == 0 | is.na(imp))], aes(x = as.numeric(as.character(age)), y = prev, shape = as.factor(source), size = sample_size, color = as.factor(year)), alpha = 0.5) +
        labs(x = "Age", y = y_label, color = "Year", linetype = version,
             shape = "Source", title = title) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        scale_size_area() +
        guides(fill = "none") + xlim(min(ages), max(ages))
      if (nrow(fdirect[source == "BRFSS" & !is.na(sex) & age == "80+"]) > 0) {
        p <- p + geom_point(data = fdirect[source == "BRFSS" & !is.na(sex) & age == "80+" & (imp == 0 | is.na(imp))], aes(x = 82.5, y = prev, fill = as.factor(year), size = sample_size), shape = 21, color = 1, alpha = 0.5);
      }
      p <- p + geom_line(data = fest[age != "80+"], aes(x = as.numeric(as.character(age)), y = pred_mean, color = as.factor(year), linetype = version)) +
        geom_ribbon(data = fest[age != "80+" & version == new_label], aes(x = as.numeric(as.character(age)), ymin = pred_lb, ymax = pred_ub, fill = as.factor(year)), alpha = 0.05)
    }
  }
  
  print(p)
}

plot_mcnty_trends_year <- function(fest, fdirect, title, line_shape_setting, cap = NULL) {
  fest[, Sex := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both"))]
  fest[, race_name := factor(race, levels = c(races, 1), labels = c(race_labels, "All"))]
  
  fdirect[, Sex := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both"))]
  fdirect[, race_name := factor(race, levels = c(races, 1), labels = c(race_labels, "All"))]
  
  if ("80+" %in% unique(fest$age)) {
    fest[, age := factor(age, levels = c(ages, "80+"))]
  }
  
  fest[, age_backup := age]
  fdirect[, age_backup := age]
  
  if ("85" %in% unique(fest$age)) {
    fest[age == 85, age := "85+"]
    fest[, age := factor(age, levels = c(ages[ages != 85], "80+", "85+"))]
    fdirect[age == 85, age := "85+"]
  }
  
  if (by_source) {
    if ("GBD" %in% unique(fdirect$source)) {
      p <- ggplot(fest, aes()) + theme_bw() + 
        facet_grid(source ~ Sex + race_name) +
        geom_ribbon(data = fest[age == 98 & type != "Data" & level == "mcnty" & estimate_type == "Full Estimate" & version == new_label,], aes(x = year, ymin = pred_lb, ymax = pred_ub, fill = "County"), alpha = 0.05) +
        geom_line(data = fest[age == 98 & type != "Data" & level == "mcnty" & estimate_type == "Full Estimate", ], aes(x = year, y = pred_mean, color = "aaaCounty", linetype = version)) +
        geom_line(data = fdirect[source == "GBD" & !is.na(sex) & !is.na(race)], aes(x = year, y = prev, color = 1), linetype = 6, alpha = 0.5) +
        geom_point(data = fdirect[source != "GBD" & !is.na(sex) & !is.na(race) & (imp == 0 | is.na(imp))], aes(x = year, y = prev, shape = as.factor(source), size = sample_size), color = 1, alpha = 0.5) +
        labs(x = "Year", y = y_label, color="Location", linetype = version,
             shape = "Source", title = title) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        scale_size_area() +
        guides(fill = "none") + xlim(min(years), max(years)) +
        scale_size_continuous(range = c(1, 4))
    } else {
      p <- ggplot(fest, aes()) + theme_bw() + 
        facet_grid(source ~ Sex + race_name) +
        geom_ribbon(data = fest[age == 98 & type != "Data" & level == "mcnty" & estimate_type == "Full Estimate" & version == new_label,], aes(x = year, ymin = pred_lb, ymax = pred_ub, fill = "County"), alpha = 0.2) +
        geom_line(data = fest[age == 98 & type != "Data" & level == "mcnty" & estimate_type == "Full Estimate", ], aes(x = year, y = pred_mean, color = "aaaCounty", linetype = version)) +
        geom_point(data = fdirect[source != "GBD" & !is.na(sex) & !is.na(race) & (imp == 0 | is.na(imp))], aes(x = year, y = prev, shape = as.factor(source), size = sample_size), color = 1, alpha = 0.5) +
        labs(x = "Year", y = y_label, color="Location", linetype = version,
             shape = "Source", title = title) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        scale_color_hue(labels = c("County")) + guides(fill = "none") + xlim(min(years), max(years)) +
        scale_size_area() +
        scale_size_continuous(range = c(1, 4))
    }
  } else {
    if ("GBD" %in% unique(fdirect$source)) {
      p <- ggplot(fest, aes()) + theme_bw() + 
        facet_grid(source ~ Sex + race_name) +
        geom_ribbon(data = fest[age == 98 & type != "Data" & level == "mcnty" & estimate_type == "Full Estimate" & version == new_label,], aes(x = year, ymin = pred_lb, ymax = pred_ub, fill = "County"), alpha = 0.05) +
        geom_line(data = fest[age == 98 & type != "Data" & level == "mcnty" & estimate_type == "Full Estimate", ], aes(x = year, y = pred_mean, color = "aaaCounty", linetype = version)) +
        geom_line(data = fdirect[source == "GBD" & !is.na(sex) & !is.na(race)], aes(x = year, y = prev, color = 1), linetype = 6, alpha = 0.5) +
        geom_point(data = fdirect[source != "GBD" & !is.na(sex) & !is.na(race) & (imp == 0 | is.na(imp))], aes(x = year, y = prev, shape = as.factor(source), size = sample_size), color = 1, alpha = 0.5) +
        labs(x = "Year", y = y_label, color="Location", linetype = version,
             shape = "Source", title = title) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        guides(fill = "none") + xlim(min(years), max(years)) +
        scale_size_area() +
        scale_size_continuous(range = c(1, 4))
      # }
    } else {
      if (outcome %in% c("income_pc")) {
        fest <- fest[sex == 3]
        fdirect <- fdirect[sex == 3]
        fdirect[source == "outlier", c("source", "source2") := list("ACS", "outlier")]
      }
      
      if (is.null(fdirect$source2)) {
        fdirect$source2 <- NA
      }
      
      if (outcome == "mds_pc") {
        fest <- fest[race == 1 & sex == 3]
      }
      
      p <- ggplot(fest, aes()) + theme_bw() + 
        facet_grid(source ~ Sex + race_name) +
        geom_ribbon(data = fest[age == 98 & type != "Data" & level == "mcnty" & estimate_type == "Full Estimate" & age_backup != "80+" & version == new_label,], aes(x = year, ymin = pred_lb, ymax = pred_ub, fill = "County"), alpha = 0.2) +
        geom_line(data = fest[age == 98 & type != "Data" & level == "mcnty" & estimate_type == "Full Estimate" & age_backup != "80+", ], aes(x = year, y = pred_mean, color = "aaaCounty", linetype = version)) +
        geom_point(data = fdirect[source != "GBD" & is.na(source2) & !is.na(sex) & !is.na(race) & age_backup != "80+" & (imp == 0 | is.na(imp))], aes(x = year, y = prev, shape = as.factor(source), size = sample_size), color = 1, alpha = 0.5) +
        geom_point(data = fdirect[source != "GBD" & source2 == "outlier" & !is.na(sex) & !is.na(race) & age_backup != "80+" & imp == 0], aes(x = year, y = prev, size = sample_size, shape = as.factor(source)), color = 4, alpha = 1) +
        labs(x = "Year", y = y_label, color = "Location", linetype = version, shape = "Source", title = title, caption = "Points are direct estimates; blue points represent outliered data.") + 
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        scale_color_hue(labels = c("County")) + guides(fill = "none") + xlim(min(years), max(years)) +
        scale_size_area() +
        scale_size_continuous(range = c(1, 4))
    }
  }
  p <- p + labs(caption = cap)
  print(p)
}

create_plots <- function(est_stand, direct_estimates_stand) {
  # if apply_pop_restrictions is TRUE, need to use a different file name
  file_suffix <- ifelse(apply_pop_restrictions, "_pop_restrictions", "")
  
  line_shape_setting = "type"
  p <- "standard"
  ############# Top level plots
  if (isTRUE(conditional) | conditional == 'TRUE') {
    pdf(paste0(output_dir, "/est_", natl_level, "_", p, "_conditional", file_suffix, ".pdf"), width = 17, height = 8)
  } else {
    pdf(paste0(output_dir, "/est_", natl_level, "_", p, file_suffix, ".pdf"), width = 17, height = 8)
  }
  
  if (outcome[1] == "income_pc") {
    title <- paste0("Income per Capita: Observed and Estimated (",  capitalize(natl_level), ")")
  } else if (outcome[1] == "edu_hs") {
    title <- paste0("Educational Attainment (HS): Observed and Estimated (",  capitalize(natl_level), ")")
  } else if (length(outcome) == 1 &&  outcome[1] == "pred_bmi") {
    title <- paste0("Mean BMI: Observed and Estimated (",  capitalize(natl_level), ")")
  } else if (family == "Poisson") {
    title <- paste0(outcome_string, ": Observed and Estimated Incidence Rates (",  capitalize(natl_level), ")")
  } else if (family == "binomial") {
    title <- paste0(outcome_string, ": Observed and Estimated Prevalence (",  capitalize(natl_level), ")")
  }
  
  fest <- est_stand[.(natl_level), , on = "level"]
  fdirect <- direct_estimates_stand[.(natl_level), , on = "level"]
  
  print("Starting national-level plots...")
  print(plot_by_sex_year(fest = fest[race == 1 & age >= 98,], fdirect = fdirect[race == 1 & age >= 98,], title, line_shape_setting))
  
  if (by_source) {
    for (src in unique(fest[source != "GBD", source])) {
      print(plot_by_sex_year(fest = fest[source %in% c(src, "GBD") & race == 1 & age >= 98,], fdirect = fdirect[source %in% c(src, "GBD") & race == 1 & age >= 98,], title = paste0(title, " (", src, ")"), line_shape_setting))
    }
  }
  
  if (by_race) {
    print(plot_by_race_year(fest = fest[age %in% c(98, 99),], fdirect = fdirect[age %in% c(98, 99),], title, line_shape_setting))
    
    for (rr in unique(c(1, races))) {
      print(plot_by_race_year(fest = fest[age >= 98 & race == rr,], fdirect = fdirect[age >= 98 & race == rr,],
                              title = paste0(title, " (", race_map[race == rr, race_labels], ")"), line_shape_setting))
    }
    
    if (by_source) {
      for (src in unique(fest[source != "GBD", source])) {
        print(plot_by_race_year(fest = fest[source %in% c(src, "GBD") & age %in% c(98, 99),], fdirect = fdirect[source %in% c(src, "GBD") & age %in% c(98, 99),], title = paste0(title, " (", src, ")"), line_shape_setting))
      }
    } 
    
    if (!exists("plot_years")) {
      plot_years <- c(min(years), max(years))
    } else if (is.null(plot_years)) {
      plot_years <- c(min(years), max(years))
    }
    
    print(plot_by_race_age_year(fest = fest[!(age %in% c(98, 99, "80+")) & year %in% plot_years,], fdirect = fdirect[!(age %in% c(98, 99)) & year %in% plot_years,],
                                title = paste(title), line_shape_setting))
    
    for (rr in unique(c(1, races))) {
      print(plot_by_race_age_year(fest = fest[age < 98 & race == rr & year %in% plot_years,], fdirect = fdirect[age < 98 & race == rr & year %in% plot_years,],
                                  title = paste(title, if (rr != 1) paste0("(", race_map[race == rr, race_labels], ")")), line_shape_setting))
    }
    
    if (by_source) {
      for (src in unique(fest[source != "GBD", source])) {
        print(plot_by_race_age_year(fest = fest[source %in% c(src, "GBD") & !(age %in% c(98, 99, "80+")) & year %in% plot_years,], fdirect = fdirect[source %in% c(src, "GBD") & !(age %in% c(98, 99)) & year %in% plot_years,], title = paste0(title, " (", src, ")"), line_shape_setting))
      }
    }
    
    for (rr in unique(c(1, races))) {
      print(plot_by_age_sex_year(fest = fest[race == rr & age < 98,], fdirect = fdirect[race == rr & age < 98,],
                                 title = paste0(title, " (", race_map[race == rr, race_labels], ")"),
                                 line_shape_setting))
      if (by_source) {
        for (src in unique(fest[source != "GBD", source])) {
          if(fest[source %in% c(src, "GBD") & race == rr & !(age %in% c(98, 99)), .N] == 0){
            # skip if there are no estimates for this source/race combination
            next
          }
          print(plot_by_age_sex_year(fest = fest[source %in% c(src, "GBD") & race == rr & !(age %in% c(98, 99)),], fdirect = fdirect[source %in% c(src, "GBD") & race == rr & !(age %in% c(98, 99)),],
                                     title = paste0(title, " (", race_map[race == rr, race_labels], ", ", src, ")"),
                                     line_shape_setting))
        }
      }
    }
  }
  
  dev.off()
  
  ############# Mid-level plots
  if (!is.null(state_level)) {
    if (isTRUE(conditional) | conditional=='TRUE') {
      pdf(paste0(output_dir, "/est_", state_level, "_", p, "_conditional", file_suffix, ".pdf"), width = 17, height = 8)
    } else {
      pdf(paste0(output_dir, "/est_", state_level, "_", p, file_suffix, ".pdf"), width = 17, height = 8)
    }
    
    print("Starting state-level plots...")
    for (aa in est_stand[level == state_level, unique(area)]) {
      if (outcome[1] == "income_pc") {
        title <- paste0("Income per Capita: Observed and Estimated (",  capitalize(state_level), " ", aa, " (", est_stand[level == state_level & area == aa, state_name][1], ")")
      } else if (outcome[1] == "pred_bmi") {
        title <- paste0("Mean BMI: Observed and Estimated (",  capitalize(state_level), " ", aa, " (", est_stand[level == state_level & area == aa, state_name][1], "))")
      } else if (family == "Poisson") {
        title <- paste0(outcome_string, ": ", capitalize(state_level), " ", aa, " (", est_stand[level == state_level & area == aa, state_name][1], "))")
      } else if (family == "binomial") {
        title <- paste0(outcome_string, ": ", capitalize(state_level), " ", aa, " (", est_stand[level == state_level & area == aa, state_name][1], "))")
      }
      
      fest <- est_stand[.(state_level), , on = "level"]
      fdirect <- direct_estimates_stand[.(state_level), , on = "level"]
      
      print(plot_by_sex_year(fest = fest[area == aa & race == 1 & age %in% c(98, 99),], fdirect = fdirect[area == aa & race == 1 & age %in% c(98, 99),], title = title, line_shape_setting))
      
      if (by_source) {
        for (src in unique(fest[source != "GBD", source])) {
          print(plot_by_sex_year(fest = fest[area == aa & source %in% c(src, "GBD") & race == 1 & age %in% c(98, 99),], fdirect = fdirect[area == aa & source %in% c(src, "GBD") & race == 1 & age %in% c(98, 99),], title = paste0(title, " (", src, ")"), line_shape_setting))
        }
      }
      
      if (by_race) {
        print(plot_by_race_year(fest = fest[area == aa & age >= 98,], fdirect = fdirect[area == aa & age >= 98,], title = paste(title), line_shape_setting))
        
        if (by_source) {
          for (src in unique(fest[source != "GBD", source])) {
            print(plot_by_race_year(fest = fest[area == aa & source %in% c(src, "GBD") & age >= 98,], fdirect = fdirect[area == aa & source %in% c(src, "GBD") & age >= 98,], title = paste0(title, " (", src, ")"), line_shape_setting))
          }
        }
        
        for (rr in unique(c(1, races))) {
          print(plot_by_race_year(fest = fest[area == aa & age >= 98 & race == rr,], fdirect = fdirect[area == aa & age >= 98 & race == rr,],
                                  title = paste0(title, " (", race_map[race == rr, race_labels], ")"), line_shape_setting))
        }
        
        if (!exists("plot_years")) {
          plot_years <- c(min(years), max(years))
        } else if (is.null(plot_years)) {
          plot_years <- c(min(years), max(years))
        }
        
        print(plot_by_race_age_year(fest = fest[area == aa & age < 98 & year %in% plot_years,], fdirect = fdirect[area == aa & age < 98 & year %in% plot_years,],
                                    title = paste(title), line_shape_setting))
        
        for (rr in unique(c(1, races))) {
          print(plot_by_race_age_year(fest = fest[area == aa & age < 98 & race == rr & year %in% plot_years,], fdirect = fdirect[area == aa & age < 98 & race == rr & year %in% plot_years,],
                                      title = paste(title, if (rr != 1) paste0("(", race_map[race == rr, race_labels], ")")), line_shape_setting))
        }
        
        if (by_source) {
          for (src in unique(fest[source != "GBD", source])) {
            # determine plot years again b/c the range determined above might not exist for all sources
            tmp_plot_years <- c(unlist(fest[area == aa & source %in% c(src, "GBD") & age < 98, .(min(year), max(year))]))
            print(plot_by_race_age_year(fest = fest[area == aa & source %in% c(src, "GBD") & age < 98 & year %in% tmp_plot_years,], fdirect = fdirect[area == aa & source %in% c(src, "GBD") & age < 98 & year %in% tmp_plot_years,], title = paste0(title, " (", src, ")"), line_shape_setting))
          }
        }
        
        for (rr in unique(c(1, races))) {
          print(plot_by_age_sex_year(fest = fest[area == aa & race == rr & age < 98,], fdirect = fdirect[area == aa & race == rr & age < 98,],
                                     title = paste0(title, " (", race_map[race == rr, race_labels], ")"),
                                     line_shape_setting))
          if (by_source) {
            for (src in unique(fest[area == aa & source != "GBD", source])) {
              if(fest[area == aa & source %in% c(src, "GBD") & race == rr & age < 98, .N] == 0){
                # skip if there are no preds for this race/source combination
                next
              }
              print(plot_by_age_sex_year(fest = fest[area == aa & source %in% c(src, "GBD") & race == rr & age < 98,], fdirect = fdirect[area == aa & source %in% c(src, "GBD") & race == rr & age < 98 & sex != 3,],
                                         title = paste0(title, " (", race_map[race == rr, race_labels], ", ", src, ")"),
                                         line_shape_setting))
            }
          }
        }
      }
    }
    
    dev.off()
  }
  
  ############# Other-level plots (cbsa_mcnties or puma_mcnties)
  if (!is.null(plot_results_other_level)) {
    if(isTRUE(conditional) | conditional=='TRUE') {
      pdf(paste0(output_dir, "/est_", plot_results_other_level, "_", p, "_conditional.pdf"), width = 17, height = 8)
    } else {
      pdf(paste0(output_dir, "/est_", plot_results_other_level, "_", p, ".pdf"), width = 17, height = 8)
    }
    
    print("Starting mid-level plots...")
    for (aa in est_stand[level == plot_results_other_level, unique(area)]) {
      if (family == "Poisson") {
        title <- paste0(outcome_string, ": ", capitalize(plot_results_other_level), " ", aa, " (in ", est_stand[level == state_level & area == aa, state_name][1], ")")
      } else if (family == "binomial") {
        title <- paste0(outcome_string, ": ", capitalize(plot_results_other_level), " ", aa, " (in ", est_stand[level == plot_results_other_level & area == aa, state_name][1], ")")
      }
      
      print(plot_by_race_age_year(fest = fest[area == aa & age < 98 & year %in% plot_years,], fdirect = fdirect[area == aa & age < 98 & year %in% plot_years,],
                                  title = paste(title), line_shape_setting))
      
      for (rr in unique(c(1, races))) {
        print(plot_by_race_age_year(fest = fest[area == aa & age < 98 & race == rr & year %in% plot_years,], fdirect = fdirect[area == aa & age < 98 & race == rr & year %in% plot_years,],
                                    title = paste(title, if (rr != 1) paste0("(", race_map[race == rr, race_labels], ")")), line_shape_setting))
      }
      
      if (by_source) {
        for (src in unique(fest[source != "GBD", source])) {
          print(plot_by_race_age_year(fest = fest[area == aa & source %in% c(src, "GBD") & age < 98 & year %in% plot_years,], fdirect = fdirect[area == aa & source %in% c(src, "GBD") & age < 98 & year %in% plot_years,], title = paste0(title, " (", src, ")"), line_shape_setting))
        }
      }
      
      for (rr in unique(c(1, races))) {
        print(plot_by_age_sex_year(fest = fest[area == aa & race == rr & age < 98,], fdirect = fdirect[area == aa & race == rr & age < 98,],
                                   title = paste0(title, " (", race_map[race == rr, race_labels], ")"),
                                   line_shape_setting))
        if (by_source) {
          for (src in unique(fest[area == aa & source != "GBD", source])) {
            print(plot_by_age_sex_year(fest = fest[area == aa & source %in% c(src, "GBD") & race == rr & age < 98,], fdirect = fdirect[area == aa & source %in% c(src, "GBD") & race == rr & age < 98,],
                                       title = paste0(title, " (", race_map[race == rr, race_labels], ", ", src, ")"),
                                       line_shape_setting))
          }
        }
      }
    }
    
    dev.off()
  }
  
  ############# Low level plots
  # plot all mcounties in order
  print("Starting mcnty-level plots...")
  if (!exists("model") & exists("mean_model")) {
    model <- mean_model
  }
  
  if (model %like% "covariate_mod") {
    ############# Low level plots
    if (!apply_pop_restrictions) { # the mcnty-level results are saved in the raking script, so these aren't available when apply_pop_restrictions is TRUE
      if (isTRUE(conditional) | conditional=='TRUE') {
        pdf(paste0(output_dir, "/est_", mcnty_level, "_", p, "_conditional", file_suffix, ".pdf"), width = 17, height = 8)
      } else {
        pdf(paste0(output_dir, "/est_", mcnty_level, "_", p, file_suffix, ".pdf"), width = 17, height = 8)
      }
      
      # plot all mcounties in order
      print("Starting mcnty-level plots...")
    }
    
    est_stand <- est_stand[!(type %in% c("Estimate (Raked)", "Variance (Raked)"))]
    
    # plot mcounties in descending order by sample size
    for (aa in unique(loc$mcnty)) {
      ti <- paste0(outcome_string, ": ", "Merged county ", aa, " (", paste(loc[mcnty == aa, cnty_name], collapse = ", "), ")")
      plot_mcnty_trends_year(fest = est_stand[age == 98 & level == "mcnty" & area == aa,], fdirect = direct_estimates_stand[age == 98 & level == "mcnty" & area == aa,], ti, line_shape_setting)
    }
    
    dev.off()
  } else if (outcome[1] == "pred_bmi") {
    ############# Low level plots
    if (!apply_pop_restrictions) { # the mcnty-level results are saved in the raking script, so these aren't available when apply_pop_restrictions is TRUE
      if (isTRUE(conditional) | conditional == 'TRUE') {
        pdf(paste0(output_dir, "/est_by_year_", mcnty_level, "_", p, "_conditional.pdf"), width = 17, height = 8)
      } else {
        pdf(paste0(output_dir, "/est_by_year_", mcnty_level, "_", p, ".pdf"), width = 17, height = 8)
      }
      
      # plot all mcounties in order
      print("Starting mcnty-level plots...")
    }
    
    est_stand <- est_stand[!(type %in% c("Estimate (Raked)", "Variance (Raked)"))]
    
    # plot mcounties in order
    loc <- fread("FILEPATH")
    for (aa in unique(loc$mcnty)) {
      ti <- paste0("Mean BMI", ": ", "Merged county ", aa, " (", paste(loc[mcnty == aa, cnty_name], collapse = ", "), ")")
      plot_mcnty_trends_year(fest = est_stand[sex != 3 & age == 98 & level == "mcnty" & area == aa,], fdirect = direct_estimates_stand[sex != 3 & age == 98 & level == "mcnty" & area == aa,], ti, line_shape_setting)
    }
    
    dev.off()
  } else { # everything else
        if (isTRUE(conditional) | conditional == 'TRUE') {
      pdf(paste0(output_dir, "/est_by_year_", mcnty_level, "_", p, "_conditional", file_suffix, ".pdf"), width = 17, height = 8)
    } else {
      pdf(paste0(output_dir, "/est_by_year_", mcnty_level, "_", p, file_suffix, ".pdf"), width = 17, height = 8)
    }

    # plot mcounties in order (mcnty)
    for (aa in unique(loc$mcnty)) {
      ti <- paste0(outcome_string, ": ", "Merged county ", aa, " (", paste(loc[mcnty == aa, cnty_name], collapse = ", "), ")")
      plot_mcnty_trends_year(fest = est_stand[age == 98 & level == "mcnty" & area == aa & sex != 3,], fdirect = direct_estimates_stand[age == 98 & level == "mcnty" & area == aa & sex != 3,], ti, line_shape_setting)
    }

    dev.off()  

    # plot mcnties in order by population size (descending)
    if (isTRUE(conditional) | conditional == 'TRUE') {
      pdf(paste0(output_dir, "/est_by_year_", mcnty_level, "_", p, "_conditional", file_suffix, "descending_pop.pdf"), width = 17, height = 8)
    } else {
      pdf(paste0(output_dir, "/est_by_year_", mcnty_level, "_", p, file_suffix, "descending_pop.pdf"), width = 17, height = 8)
    }

    # plot mcounties in order of 2019 pop (mcnty)
    tmp <- pop[year == 2019 & race != 1 & sex == 3 & age <= 85]
    # check that 3110*5 rows per age
    stopifnot(all(tmp[,.N, age]$N == 3110*5))
    tmp <- rbindlist(list(
      tmp[, .(pop = sum(pop)), .(mcnty, state, race)][order(-pop)],
      tmp[, .(pop = sum(pop), race = 1), .(mcnty, state)][order(-pop)]
      ), use.names = T
    )
    print(tmp)
    # sum of pop should be between 200 and 300 million (adult population); if not, something
    # went wrong collapsing pop
    stopifnot(sum(tmp[race == 1,pop]) > 2e8 & sum(tmp[race == 1, pop]) < 3e8)
    
    
    for (aa in tmp[race == 1 & pop >= 1000][order(-pop),mcnty]) {
      ti <- paste0(outcome_string, ": ", "Merged county ", aa, " (", paste(loc[mcnty == aa, cnty_name], collapse = ", "), "), State ", tmp[mcnty == aa, unique(state)],  ", Population: ", tmp[mcnty == aa & race == 1, format(round(pop, 0), big.mark = ",")])
      # make a caption with the pop by race
      cap <- paste("2019 pop by race:", tmp[mcnty == aa, paste(race, format(round(pop, 0), big.mark = ","), sep = "=", collapse = " // ")])
      p <- plot_mcnty_trends_year(fest = est_stand[age == 98 & level == "mcnty" & area == aa & sex != 3,], fdirect = direct_estimates_stand[age == 98 & level == "mcnty" & area == aa & sex != 3,], ti, line_shape_setting, cap = cap)    
    }
    dev.off()

  }
}

######## Prepare direct estimates
### Load location mapping
locations <- fread("FILEPATH")

##### Produce direct estimates for covariate data
if (exists("covariate_name")) {
  if (!is.null(covariate_name)) {
    if (file.exists(paste0(output_dir, "/p_preds.csv")) & !(covariate_name %in% c("income_pc_by_race_ethn"))) {
      direct_estimates <- fread(paste0(output_dir, "/p_preds.csv"))
    } else {
      direct_estimates <- readRDS(paste0(output_dir, "/data_pre_factor.rds"))
    }
    
    if (covariate_name %in% c("income_pc_by_race_ethn")) {
      direct_estimates <- direct_estimates[, c("area", "year", "income_pc", "sample_size", "race", "state")]
    } else if (covariate_name %in% c("ahrf_mds_pc")) {
      direct_estimates[, race := 1]
    } else {
      cols <- c("mcnty", "year", "race", "state", paste0("raw_", outcome[1]), "raw_pop", "pop") # pop (which is actually effective sample size) is used only for scaling the size of data points in the plots
      direct_estimates <- direct_estimates[, ..cols]
    }
    
    direct_estimates[, c("sex", "age", "level") := list(3, 98, "mcnty")]
    setnames(direct_estimates, c(paste0("raw_", outcome[1]), "cases", "mcnty", "pop"), c("count", "count", "area", "sample_size"), skip_absent = TRUE)
    
    if (covariate_name %in% c("poverty_by_race_ethn", "education_by_race_ethn", "unemployment_by_race_ethn", "foreign_born_by_race_ethn")) {
      direct_estimates[, prev := count / raw_pop]
      direct_estimates_state_natl <- readRDS(direct_estimates_file)
      setnames(direct_estimates_state_natl, c(eval(outcome[1]), "total"), c("prev", "sample_size"))
      direct_estimates_state_natl[, c("sex", "age") := list(3, 98)]
      direct_estimates <- rbindlist(list(direct_estimates, direct_estimates_state_natl), use.names = TRUE, fill = TRUE)
      direct_estimates <- direct_estimates[race != 0]
    } else if (covariate_name %in% c("ahrf_mds_pc")) {
      direct_estimates[, level := "mcnty"]
      direct_estimates_state <- direct_estimates[, list(level = "state", area = state, count = sum(count), sample_size = sum(sample_size)), by = c("year", "state", "state_name", "sex", "age", "race")]
      direct_estimates_natl <- direct_estimates[, list(level = "natl", area = 1, count = sum(count), sample_size = sum(sample_size)), by = c("year", "sex", "age", "race")]
      direct_estimates <- rbindlist(list(direct_estimates, direct_estimates_state, direct_estimates_natl), use.names = TRUE, fill = TRUE)
      direct_estimates[, prev := count / sample_size]
    } else if (covariate_name %in% c("income_pc_by_race_ethn")) {
      setnames(direct_estimates, c("income_pc"), c("prev"), skip_absent = TRUE)
      direct_estimates_state_natl <- readRDS(direct_estimates_file)
      setnames(direct_estimates_state_natl, c(eval(outcome[1])), c("prev"))
      direct_estimates_state_natl[, c("sex", "age") := list(3, 98)]
      direct_estimates <- rbindlist(list(direct_estimates, direct_estimates[, list(level = "state", area = state, sample_size = sum(sample_size), prev = weighted.mean(prev, sample_size)), by = c("year", "race", "state", "sex", "age")]), use.names = TRUE, fill = TRUE)
      direct_estimates <- rbindlist(list(direct_estimates, direct_estimates[level == "mcnty", list(level = "natl", area = 1, sample_size = sum(sample_size), prev = weighted.mean(prev, sample_size)), by = c("year", "race", "sex", "age")]), use.names = TRUE, fill = TRUE)
      direct_estimates <- rbindlist(list(direct_estimates, direct_estimates[, list(race = 1, sample_size = sum(sample_size), prev = weighted.mean(prev, sample_size)), by = c("year", "sex", "age", "level", "area")]), use.names = TRUE, fill = TRUE)
      
      direct_estimates <- merge(direct_estimates, direct_estimates_state_natl, by = c("year", "area", "level", "race", "sex", "age"), all = TRUE)
      direct_estimates[!is.na(prev.y), prev.x := prev.y]
      direct_estimates[, c("prev", "prev.x", "prev.y") := list(prev.x, NULL, NULL)]
    } else {
      direct_estimates[, c("prev") := list(count / raw_pop)]
      direct_estimates <- rbindlist(list(direct_estimates, direct_estimates[, list(level = "state", area = state, count = sum(count), raw_pop = sum(raw_pop), sample_size = sum(sample_size), prev = weighted.mean(prev, raw_pop)), by = c("year", "race", "state", "sex", "age")]), use.names = TRUE, fill = TRUE)
      direct_estimates <- rbindlist(list(direct_estimates, direct_estimates[level == "mcnty", list(level = "natl", area = 1, count = sum(count), raw_pop = sum(raw_pop), sample_size = sum(sample_size), prev = weighted.mean(prev, raw_pop)), by = c("year", "race", "sex", "age")]), use.names = TRUE, fill = TRUE)
      direct_estimates <- rbindlist(list(direct_estimates, direct_estimates[, list(race = 1, count = sum(count), raw_pop = sum(raw_pop), sample_size = sum(sample_size), prev = weighted.mean(prev, raw_pop)), by = c("year", "sex", "age", "level", "area")]), use.names = TRUE, fill = TRUE)
    }
    
    direct_estimates[, c("state") := list(NULL)]
    
    direct_estimates_stand <- copy(direct_estimates)
    rm(direct_estimates)
    
    direct_estimates_stand.mcnty <- merge(direct_estimates_stand[level == "mcnty"], unique(locations[, list(mcnty, state_name)]), by.x = "area", by.y = "mcnty", all.x = TRUE)
    direct_estimates_stand.state <- merge(direct_estimates_stand[level == "state"], unique(locations[, list(state, state_name)]), by.x = "area", by.y = "state", all.x = TRUE)
    direct_estimates_stand <- rbindlist(list(direct_estimates_stand.mcnty, direct_estimates_stand.state, direct_estimates_stand[!level %in% c("state", "mcnty")]), use.names = TRUE, fill = TRUE)
    
    direct_estimates_stand[, source := gold_standard_source]
    setnames(direct_estimates_stand, "raw_pop", "pop", skip_absent = TRUE)
    if (!is.null(direct_estimates_stand$sample_size)) {
      direct_estimates_stand[, weights := sample_size]
    }
    
    #### Append outliers
    if (file.exists(paste0(output_dir, "/data_outliered.RDS")) & outcome == "income_pc") {
      outliers <- readRDS(paste0(output_dir, "/data_outliered.RDS"))
      setnames(outliers, c("mcnty", "neff", "income_pc"), c("area", "sample_size", "prev"))
      outliers[, c("source", "weights", "sex", "age") := list("outlier", sample_size, 3, 98)]
      same_cols <- colnames(outliers)[colnames(outliers) %in% colnames(direct_estimates_stand)]
      outliers <- outliers[, ..same_cols]
      direct_estimates_stand <- rbindlist(list(direct_estimates_stand, outliers), use.names = TRUE, fill = TRUE)
    }
  }
} else {
  ##### Process data_file object
  if (length(direct_estimates_file) > 1 | !is.null(names(direct_estimates_file))) {
    direct_estimates_file <- data.table("name" = names(direct_estimates_file), "location" = direct_estimates_file)
  }
  
  #### Load or generate direct estimates
  direct_estimates_stand_list <- vector(mode = "list", length = length(data_file))
  direct_estimates_agg_list <- vector(mode = "list", length = length(data_file))
  
  for (i in 1:length(data_file)) {
    print(paste0("Preparing direct estimates for ", names(data_file)[i], "..."))
    if (!is.null(direct_estimates_file)) {
      if (names(data_file)[i] %in% unique(direct_estimates_file$name)) {
        direct_estimates <- data.table()
        
        ##### Load and prep inputs
        direct_estimates <- readRDS(direct_estimates_file[name == names(data_file)[i], location])
        
        ###### Add a data source field
        direct_estimates$source <- names(data_file)[i]
        
        if (outcome[1] == "edu_hs") {
          direct_estimates[, prev := edu_hs]
        } else if (outcome[1] == "pred_bmi") {
          direct_estimates[, pred_bmi_0 := rowMeans(.SD), .SDcols = paste0("pred_bmi_", 1:n.imp)]
          direct_estimates <- melt(direct_estimates, id.vars = c("age", "year", "sex", "race", "state", "level", "area", "source", "final_weight", "weights", "sample_size", "pop"), measure.vars = c(paste0("pred_bmi_", 0:n.imp)), variable.name = "imp_string", value.name = "prev")
          direct_estimates[imp_string == "pred_bmi_0", imp := 0]
        } else if (outcome[names(outcome) == names(data_file)[i]] %in% c("gen_health_45_count", "gen_health_45_weighted_count")) {
          direct_estimates[, prev := gen_health_45]
        } else if (outcome[names(outcome) == names(data_file)[i]] %in% c("asthma_count", "asthma_weighted_count")) {
          direct_estimates[, prev := asthma]
        } else if (outcome[names(outcome) == names(data_file)[i]] %in% c("frequent_activity_limitations_count", "frequent_activity_limitations_weighted_count")) {
          direct_estimates[, prev := frequent_activity_limitations]
        } else if (outcome[names(outcome) == names(data_file)[i]] %in% c("depression_count", "depression_weighted_count")) {
          direct_estimates[, prev := depression]
        } else if (outcome[names(outcome) == names(data_file)[i]] %in% c("treated_depression_count", "treated_depression_weighted_count")) {
          direct_estimates[, prev := treated_depression]
        } else if (outcome[names(outcome) == names(data_file)[i]] %in% c("exp_stress_count", "exp_stress_weighted_count")) {
          direct_estimates[, prev := stress]
        } else if (outcome[names(outcome) == names(data_file)[i]] %in% c("exp_worry_count", "exp_worry_weighted_count")) {
          direct_estimates[, prev := worry]
        } else if (outcome[names(outcome) == names(data_file)[i]] %in% c("exp_physical_pain_count", "exp_physical_pain_weighted_count")) {
          direct_estimates[, prev := pain]
        } else if (outcome[names(outcome) == names(data_file)[i]] %in% c("tooth_loss_count", "tooth_loss_weighted_count")) {
          direct_estimates[, prev := tooth_loss]
        } else if (outcome[names(outcome) == names(data_file)[i]] %in% c("copd_count", "copd_weighted_count")) {
          direct_estimates[, prev := copd]
        } else if (outcome[names(outcome) == names(data_file)[i]] %in% c("arthritis_count", "arthritis_weighted_count")) {
          direct_estimates[, prev := arthritis]
        } else if (outcome[names(outcome) == names(data_file)[i]] %in% c("diabetes_count", "diabetes_weighted_count")) {
          direct_estimates[, prev := diabetes]
        } else if (grepl("DIFFREM", data_file[[i]])) {
          direct_estimates[, prev := DIFFREM]
        } else if (grepl("DIFFPHYS", data_file[[i]])) {
          direct_estimates[, prev := DIFFPHYS]
        } else if (grepl("DIFFMOB", data_file[[i]])) {
          direct_estimates[, prev := DIFFMOB]
        } else if (grepl("DIFFCARE", data_file[[i]])) {
          direct_estimates[, prev := DIFFCARE]
        } else if (grepl("DIFFSENS", data_file[[i]])) {
          direct_estimates[, prev := DIFFSENS]
        } else if (grepl("DIFFANY", data_file[[i]])) {
          direct_estimates[, prev := DIFFANY]
        } else if (outcome[[i]] %like% c('overwt','underwt','obese')& n.imp == 0){
          direct_estimates[, prev := get(outcome[[i]])]
        } else if (outcome[[i]] %like% 'weighted_count' & n.imp > 0 & imp != 0){
          direct_estimates[, prev := get(paste0(gsub('_weighted_count','',outcome[[i]]),'_', imp))]
        } else if (outcome[[i]] %like% 'weighted_count' & n.imp > 0 & imp == 0){ 
          # if imp == 0, combine imputations (collapse across) 
          tmp_outcome <- gsub('_weighted_count','',outcome[[i]])
          direct_estimates[, prev := rowMeans(.SD), .SDcols = paste0(tmp_outcome,'_', 1:n.imp)]
        } else if (outcome[[i]] %like% '_count' & !outcome[[i]] %like% 'overwt'& !outcome[[i]] %like% 'underwt'& !outcome[[i]] %like% 'obese' & n.imp > 0){
          direct_estimates[, prev := get(paste0(gsub('_count','',outcome[[i]],'_', imp)))]
        } else if (outcome[[i]] %in% c('overwt','underwt','obese') & n.imp > 0){
          direct_estimates[, prev := get(paste0(outcome[[i]],'_', imp))]
        } else if (names(data_file)[i] == "NHANES" & "has_diabetes" %in% colnames(direct_estimates)) {
          direct_estimates[, prev := has_diabetes]
        }
        
        if (isTRUE(conditional) | conditional == 'TRUE') {
          if (outcome[[1]] %like% c('obese')){
            direct_estimates[, prev := prev / get(paste0('overwt_', imp))]
          }
        }
        
        #### Replicate svvyyear for NHANES
        if (names(data_file)[i] == "NHANES") {
          direct_estimates_1 <- copy(direct_estimates)
          direct_estimates_2 <- copy(direct_estimates)
          direct_estimates_1[, "year" := substr(svyyear, 1, 4)]
          direct_estimates_2[, "year" := substr(svyyear, 6, 9)]
          direct_estimates <- rbindlist(list(direct_estimates_1, direct_estimates_2), use.names = TRUE, fill = TRUE)
          direct_estimates[, final_weight := final_weight / 2]
          direct_estimates[, sample_size := sample_size / 2]
          direct_estimates[, "pop" := final_weight]
          
          direct_estimates[race == "NH White Only", race := "5"]
          direct_estimates[race == "NH Black Only", race := "4"]
          direct_estimates[race == "Hispanic", race := "2"]
          direct_estimates[race == "all races", race := "1"]
          
          direct_estimates_api <- copy(direct_estimates[race == "other"])
          direct_estimates_aian <- copy(direct_estimates[race == "other"])
          direct_estimates_api[, race := "7"]
          direct_estimates_aian[, race := "6"]
          
          direct_estimates <- rbindlist(list(direct_estimates[race != "other"], direct_estimates_api, direct_estimates_aian), use.names = TRUE, fill = TRUE)
          direct_estimates$race <- as.integer(as.character(direct_estimates$race))
          
          direct_estimates <- direct_estimates[marital_ushd == 9 & edu_ushd == 9]
        }
        
        #### Restrict to modeled years plus all years
        direct_estimates <- direct_estimates[year %in% c(years)]
        
        #### Set final_weight to weights if NA (or null?)
        if ("final_weight" %in% names(direct_estimates)) {
          direct_estimates[is.na(final_weight), final_weight := weights]
        } else if (is.null(direct_estimates$weights)) {
          direct_estimates$weights <- 1
          direct_estimates$final_weight <- 1
        } else {
          direct_estimates[, final_weight := weights]
        }
        
        if (!(outcome[1] %in% c("pred_bmi", "edu_hs"))) {
          if (!outcome[names(outcome) == names(data_file)[i]] %like% 'overwt' &
              !outcome[names(outcome) == names(data_file)[i]] %like% 'underwt' &
              !outcome[names(outcome) == names(data_file)[i]] %like% 'obese') {
            
            #### Restrict to modeled age groups (this will also drop age 98, which will be recomputed later to represent the age groups that were actually modeled)
            direct_estimates <- direct_estimates[age %in% ages]
            
            if (source_levels[1] != "mdcr_eligibility" & (names(data_file)[i] != "NHANES")) {
              #### Aggregate across post-stratification vars
              direct_estimates <- direct_estimates[, list(weights = sum(weights, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), prev = weighted.mean(prev, final_weight, na.rm = TRUE)), by = c("area", "level", "year", "race", "sex", "age", "source")]
              
              #### Aggregate by age
              direct_estimates <- rbindlist(list(direct_estimates, direct_estimates[, list(age = 98, weights = sum(weights, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), prev = weighted.mean(prev, final_weight, na.rm = TRUE)), by = c("area", "level", "year", "race", "sex", "source")]), use.names = TRUE, fill = TRUE)
              
              #### Compute age-standardized direct estimates
              std_wt <- readRDS(age_std_file)#[, list(age, wt = wt / sum(wt))]
              
              std_wt$age <- as.character(std_wt$age)
              direct_estimates$age <- as.character(direct_estimates$age)
              
              direct_estimates[source == "BRFSS" & age == 80 & year >= 2013, age := "80+"]
              std_wt <- rbindlist(list(std_wt, data.table(age = "80+", wt = std_wt[age %in% c("80", "85"), sum(wt)])), use.names = TRUE, fill = TRUE)
              
              direct_estimates_99 <- merge(direct_estimates[age != 98], std_wt, by = "age", all.x = TRUE)
              
              direct_estimates <- rbindlist(list(direct_estimates, direct_estimates_99[, list(age = 99, weights = sum(weights, na.rm = TRUE), final_weight = sum(final_weight, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), prev = weighted.mean(prev, wt, na.rm = TRUE)), by = c("area", "level", "year", "race", "sex", "source")]), use.names = TRUE, fill = TRUE)
            } else {
              #### Aggregate by age
              direct_estimates <- rbindlist(list(direct_estimates, direct_estimates[, list(age = 98, pop = sum(pop, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), prev = weighted.mean(prev, pop, na.rm = TRUE)), by = c("area", "level", "year", "race", "sex", "source")]), use.names = TRUE, fill = TRUE)
            }
          }
          
          #### Drop rows with missingness
          if (!(outcome[1] %in% c("edu_hs"))) {
            direct_estimates <- direct_estimates[!is.na(year) & !is.na(sex) & !is.na(race) & !is.na(age) & !is.na(source) & !is.na(prev)]  
          }
          
          # Update the source variable to source_v2 if prediction is by_source and if the
          #   gold standard is BRFSS_LLCP
          if (by_source & gold_standard_source %in% c("BRFSS_LLCP", "BRFSS_LANDLINE_ONLY")) {
            # only if reading the BRFSS file
            if (names(data_file)[i] == "BRFSS") {
              # This will need to be updated if source_v2 is something other than BRFSS_LLCP, BRFSS_LANDLINE_ONLY, or Gallup
              direct_estimates[year < 2011, source := "BRFSS_LANDLINE_ONLY"]
              direct_estimates[year >= 2011, source := "BRFSS_LLCP"]
            }
          }
        }        
        
        direct_estimates_stand_list[[i]] <- direct_estimates[year != -1]
        rm(direct_estimates)
      }
      
      #### Produce mcnty-level "direct estimates" from input data set if necessary (when geographically-aggregated inputs were used)
      if (!is.null(plot_results_other_level)) {
        mcnty_direct_estimates <- agg_data(dt = readRDS(paste0(output_dir, "/data.rds"))[source == names(data_file)[i]], include_GBD_estimates = FALSE)[level == "mcnty"]
        direct_estimates_stand_list[[i]] <- rbindlist(list(direct_estimates_stand_list[[i]], mcnty_direct_estimates), use.names = TRUE, fill = TRUE)
      }
      
      if (is.null(direct_estimates_stand_list[[i]])) {
        #### Produce "direct estimates" from input data set if necessary
        ## Note that the input data are not necessarily square and are unweighted, and that these "direct estimates" are therefore not necessarily representative.
        ## Interpret them accordingly!
        
        if (exists("cause_id")) {
          if (!is.null(cause_id)) {
            direct_estimates_stand_list[[i]] <- agg_data(dt = readRDS(paste0(output_dir, "/data.rds"))[source == names(data_file)[i]], include_GBD_estimates = TRUE)
          } else {
            direct_estimates_stand_list[[i]] <- agg_data(dt = readRDS(paste0(output_dir, "/data.rds"))[source == names(data_file)[i]], include_GBD_estimates = FALSE)
          }
        } else {
          direct_estimates_stand_list[[i]] <- agg_data(dt = readRDS(paste0(output_dir, "/data.rds"))[source == names(data_file)[i]], include_GBD_estimates = FALSE)
        }
      }
    }
  }
  
  direct_estimates_stand <- rbindlist(direct_estimates_stand_list, use.names = TRUE, fill = TRUE)
  rm(direct_estimates_stand_list); gc()
  
  if (!is.null(direct_estimates_stand) & !is.null(direct_estimates_file)) {
    direct_estimates_stand.mcnty <- merge(direct_estimates_stand[level == "mcnty", -c("state", "state_name")], unique(locations[, list(mcnty, state_name)]), by.x = "area", by.y = "mcnty", all.x = TRUE)
    direct_estimates_stand.state <- merge(direct_estimates_stand[level == "state", -c("state", "state_name")], unique(locations[, list(state, state_name)]), by.x = "area", by.y = "state", all.x = TRUE)
    direct_estimates_stand <- rbindlist(list(direct_estimates_stand.mcnty, direct_estimates_stand.state, direct_estimates_stand[!level %in% c("state", "mcnty")]), use.names = TRUE, fill = TRUE)
  }
  
  direct_estimates_stand$year <- as.integer(direct_estimates_stand$year)
}

run_outputs <- vector(mode = "list", length = length(runs))

#### Get model estimates
for (run in 1:length(runs)) {
  output_dir_draws_est_temp <- runs[[run]]$output_dir_draws_est
  if (!apply_pop_restrictions) {
    output_dir_draws_est_temp <- paste0(output_dir_draws_est_temp, "/est/")
  }
  data_stand <- NULL
  
  #### Get estimates
  all_est_list <- list()
  all_est_list[[length(all_est_list) + 1]] <- agg_est(dir = output_dir, label = v1, output_dir_current = output_dir_draws_est_temp, type = "Full Estimate")
  
  est_stand <- rbindlist(all_est_list, use.names = TRUE, fill = TRUE)
  rm(all_est_list); gc()
  
  race_map <- data.table(race = c(races, 1), race_labels = c(race_labels, "All races/ethnicities"))
  
  #### Update race codes for old model runs
  if (!is.null(comparison_rundate)) {
    if (runs[[run]]$run_date == comparison_rundate) {
      if (!tryCatch(validate_race_codes(est_stand), error = function(e) FALSE)) {
        if (dplyr::setequal(unique(est_stand$race), c(1:4, 7, 9))) {
          mapping <- fread("FILEPATH")
          est_stand <- merge(est_stand, mapping, by.x = "race", by.y = "old_race", all.x = TRUE)
          est_stand[, c("race", "new_race") := list(new_race, NULL)]
        }
      }
    }
  }
  
  ## Merge state names onto datasets
  est_stand.mcnty <- merge(est_stand[level == "mcnty"], unique(locations[, list(mcnty, state_name)]), by.x = "area", by.y = "mcnty", all.x = TRUE)
  est_stand.state <- merge(est_stand[level == "state"], unique(locations[, list(state, state_name)]), by.x = "area", by.y = "state", all.x = TRUE)
  
  if (!is.null(plot_results_other_level)) {
    if (plot_results_other_level == "puma_mcnty") {
      est_stand.other <- est_stand[level == "puma_mcnty"]
    } else if (plot_results_other_level == "cbsa_mcnty") {
      est_stand.other <- est_stand[level == "cbsa_mcnty"]
    }
  } else {
    est_stand.other <- NULL
  }
  
  est_stand <- rbindlist(list(est_stand.mcnty, est_stand.state, est_stand.other, est_stand[level == "natl"]), use.names = TRUE, fill = TRUE)
  
  est_stand$age <- as.character(est_stand$age)
  
  if ("BRFSS" %in% source_levels & (outcome[1] != "pred_bmi")) {
    if (!is.null(est_stand$source)) {
      est_stand_temp <- copy(est_stand)[age %in% 80:85 & year >= 2013 & source == "BRFSS"]
    } else if (gold_standard_source %like% "BRFSS") {
      est_stand$source <- "BRFSS"
      est_stand_temp <- copy(est_stand)[age %in% 80:85 & year >= 2013]  
      
      est_stand$age <- as.character(est_stand$age)
      
      if ("BRFSS" %in% source_levels | "BRFSS_LLCP" %in% source_levels) {
        if (!is.null(est_stand$source)) {
          est_stand_temp <- copy(est_stand)[age %in% 80:85 & year >= 2013 & source %in% c("BRFSS_LANDLINE_ONLY","BRFSS", "BRFSS_LLCP")]
        } else if (gold_standard_source == "BRFSS") {
          est_stand$source <- "BRFSS"
          est_stand_temp <- copy(est_stand)[age %in% 80:85 & year >= 2013]  
        } else if (gold_standard_source == "BRFSS_LLCP"){
          est_stand$source <- "BRFSS_LLCP"
          est_stand_temp <- copy(est_stand)[age %in% 80:85 & year >= 2013]  
        }
      }
    }
    
    if (file.exists(paste0(output_dir_draws_est, "/population.rds"))) {
      pop <- readRDS(paste0(output_dir_draws_est, "/population.rds"))
    } else {
      pop <- readRDS(pop_file)
    }
    
    pop$age <- as.character(pop$age)
    
    #### Aggregate pops
    if ('id_source' %in% colnames(pop)){
      pop <- rbindlist(list(pop, pop[, list(race = 1, pop = sum(pop)), by = c("mcnty", "state", "year", "sex", "age", "id_source", "race_set")]), use.names = TRUE, fill = TRUE)
      pop <- rbindlist(list(pop, pop[, list(sex = 3, pop = sum(pop)), by = c("mcnty", "race", "state", "year", "age", "id_source", "race_set")]), use.names = TRUE, fill = TRUE)
      
      state_pop <- pop[year %in% years, list(pop = sum(pop)), by = c("race", "state", "year", "sex", "age", "id_source", "race_set")]
      natl_pop <- pop[year %in% years, list(pop = sum(pop)), by = c("race", "year", "sex", "age", "id_source", "race_set")]
    } else {
      pop <- rbindlist(list(pop, pop[, list(race = 1, pop = sum(pop)), by = c("mcnty", "state", "year", "sex", "age")]), use.names = TRUE, fill = TRUE)
      pop <- rbindlist(list(pop, pop[, list(sex = 3, pop = sum(pop)), by = c("mcnty", "race", "state", "year", "age")]), use.names = TRUE, fill = TRUE)
      
      state_pop <- pop[year %in% years, list(pop = sum(pop)), by = c("race", "state", "year", "sex", "age")]
      natl_pop <- pop[year %in% years, list(pop = sum(pop)), by = c("race", "year", "sex", "age")]
    }
    est_stand_temp_mcnty <- merge(est_stand_temp[source == "BRFSS" & level == "mcnty"], pop, by.x = c("area", "year", "sex", "race", "age"), by.y = c("mcnty", "year", "sex", "race", "age"))
    est_stand_temp_state <- merge(est_stand_temp[source == "BRFSS" & level == "state"], state_pop, by.x = c("area", "year", "sex", "race", "age"), by.y = c("state", "year", "sex", "race", "age"))
    est_stand_temp_natl <- merge(est_stand_temp[source == "BRFSS" & level == "natl"], natl_pop, by.x = c("year", "sex", "race", "age"), by.y = c("year", "sex", "race", "age"))
    
    est_stand_temp_combined <- rbindlist(list(est_stand_temp_mcnty, est_stand_temp_state, est_stand_temp_natl), use.names = TRUE, fill = TRUE)
    est_stand_temp_combined_collapsed <- est_stand_temp_combined[pop > 0, list(age = "80+", pred_mean = weighted.mean(pred_mean, pop), pred_se = sqrt(sum(pred_se^2 * pop) / sum(pop))), by = c("area", "year", "sex", "race", "level", "state_name", "source", "type", "version", "estimate_type")]
    est_stand_temp_combined_collapsed[, c("pred_lb", "pred_ub") := list(pred_mean - 1.96 * pred_se, pred_mean + 1.96 * pred_se)]
    
    est_stand <- rbindlist(list(est_stand, est_stand_temp_combined_collapsed), use.names = TRUE, fill = TRUE)
    
    # When apply_pop_restrictions is TRUE, we load the estimates produced by another script that already 
    #   created the 80+ age group. Earlier, we recoded 80+ as age 83. Here we can 
    #   label is as 80+
    if(apply_pop_restrictions & 83 %in% est_stand[, unique(age)]){
      est_stand[age == 83, age := "80+"]
    }
  }
  
  #### Move GBD estimates to est object rather than direct_estimates
  if (!by_source) {
    if ("source" %in% colnames(est_stand)) {
      est_stand[is.na(source), source := "current"]
    } else {
      est_stand[, source := "current"]
    }
  }
  
  if (!is.null(gold_standard_source)) {
    est_stand[source == "current", source := gold_standard_source]
  } else if (length(unique(direct_estimates_stand$source)) == 1) {
    est_stand$source <- unique(direct_estimates_stand$source)
  }
  
  est_stand$version <- runs[[run]]$version
  run_outputs[[run]] <- est_stand
  rm(est_stand)
}

est_stand <- rbindlist(run_outputs, use.names = TRUE, fill = TRUE)

gbd_est <- direct_estimates_stand[source == "GBD"]
if (nrow(gbd_est) > 0) {
  setnames(gbd_est, c("prev", "upper", "lower"), c("pred_mean", "pred_lb", "pred_ub"))
  est_stand <- rbindlist(list(est_stand, gbd_est), use.names = TRUE, fill = TRUE)
  direct_estimates_stand <- direct_estimates_stand[source != "GBD"]
}

if ("pop" %in% colnames(direct_estimates_stand)) {
  direct_estimates_stand[is.na(weights), final_weight := as.numeric(pop)]
} else {
  direct_estimates_stand[, final_weight := sample_size]
}
direct_estimates_stand[!is.na(weights), final_weight := weights]

#### Apply data exclusions
if (tryCatch(exists("exclusions"), error = function(e) FALSE)) {
  if (!is.null(exclusions)) {
    for (current in exclusions) {
      direct_estimates_stand <- direct_estimates_stand[!(eval(parse(text = current)))]
    }
  }
}

if (outcome[1] == "pred_bmi" & gold_standard_source == "BRFSS_LLCP" & length(unique(direct_estimates_stand$source)) == 1 && unique(direct_estimates_stand$source) == "BRFSS") {
  direct_estimates_stand$source <- "BRFSS_LLCP"
} else if (outcome[1] == "pred_bmi" & gold_standard_source == "BRFSS_LANDLINE_ONLY" & length(unique(direct_estimates_stand$source)) == 1 && unique(direct_estimates_stand$source)[1] == "BRFSS") {
  direct_estimates_stand$source <- "BRFSS_LANDLINE_ONLY"
}

# Make versions an ordered factor so that "new" is first (solid line)
tmp_versions <- est_stand[version != new_label, unique(version)]
tmp_versions <- c(new_label, tmp_versions)
est_stand[, version := factor(version, levels = tmp_versions)]

if (is.null(direct_estimates_stand$imp)) {
  direct_estimates_stand$imp <- 0
}

est_stand[, version_linetype := as.integer(version)]

# Produce plots
saveRDS(est_stand, file = paste0(output_dir, "/plot_est_stand_data.rds"))
saveRDS(direct_estimates_stand, file = paste0(output_dir, "/plot_direct_estimates_stand_data.rds"))

create_plots(est_stand = est_stand, direct_estimates_stand = direct_estimates_stand)
