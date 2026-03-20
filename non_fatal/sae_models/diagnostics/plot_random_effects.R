####################################################################################################
## Description:   Plot random effects from one of the all-cause SAE models.
####################################################################################################

###### Load required libraries
required_packages <- c("R.utils", "data.table", "ggplot2", "scales", "RColorBrewer", "gplots", 
             "gridExtra", "ggpubr", "sf", "boot", "splines", "forcats", "stringr", 
             "car", "scattermore", "pacman")
# Install missing packages in the specified library
rvers <- R.Version() # get R version in format 422 if 4.2.2, etc.
rvers <- gsub("\\.", "", paste0(rvers$major, rvers$minor))
ushd_lib <- paste0("/snfs1/Project/us_counties/rlibs/rlibs", rvers, "/")
# Check which packages are not installed
missing_packages <- required_packages[!required_packages %in% 
  c(installed.packages()[, "Package"],
  list.dirs(ushd_lib, recursive = FALSE, full.names = FALSE))]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, lib = ushd_lib)
}
.libPaths(c(.libPaths(), ushd_lib))
# Load all required packages
pacman::p_load(char = required_packages)


###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (settings_loc  <- commandArgs(TRUE)[[3]])
  (output_dir_draws_est <- commandArgs(TRUE)[[4]])
  (sex <- commandArgs(TRUE)[[5]])
  (imp <- commandArgs(TRUE)[[6]])
} else if (interactive()) {
  repo <- "FILEPATH"
  sex <- 1
}

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  print(paste0("Loading ",func))
  source(paste0(repo, "/functions/", func))
}

###### Assign settings from settings file
get_settings(settings_loc)

# load & plot model parameters ---------------------------------------------------------------------
model_name <- paste0(output_dir, "/model_fit_", sex, ".rds")
param_f <- paste0(output_dir_draws_est, "/re_param_", sex, ".rds")
if (file.exists(param_f)) {
  message(sprintf("Loading random effects from\n%s\nrather than reconstructing the RE in this script", param_f))
  re_param <- readRDS(param_f)
} else {
  re_param <- NULL
}

# Load recode mapping
recode <- readRDS(paste0(output_dir, "/recode.rds"))
load(paste0(output_dir, "/num_vars.RData"))

settings <- fread(paste0(settings_loc), header = FALSE, stringsAsFactors = FALSE)
years <- eval(parse(text = settings[V1 == "years", V2]))
ages <- eval(parse(text = settings[V1 == "ages", V2]))
model <- settings[V1 == "model", V2]
marital_labels <- c("Current", "Former", "Never Married")
a_spline <- 1:length(eval(parse(text = settings[V1 == "age_knots_spec", V2])))
t_spline <- 1:eval(parse(text = settings[V1 == "year_knots_num", V2]))

sex_label <- c("Males", "Females", "Both Sexes")[which(sex %in% 1:3)]
  
# all REs ------------------------------------------------------------------------------------------
## From plot_random_and_fixed_effects.R
## To retrieve data.table of random effects with appropriate metadata
## Note that this function is deprecated for models produced since mid 2023. The RE
##   is constructed and saved in complete_pred_subdraw.R
## Random effects vary by model
get_re <- function(model_name) {
  
  mod <- get_params(model_name)
  mod <- mod[part != "fixed"] # get rid of the values of rho, sigma, etc. because these will not be used in prediction
  
  random_effects <- unique(mod$param)
  
  re_ls <- list()
  message("RE1 creation")
  re1 <- mod[param == "re1"]
  
  # Most models have the same u1 so won't use if statement
  if (nrow(re1) > 0) {
    combos <- as.data.table(expand.grid(year_spline =  paste("year spline", t_spline), a_spline =  paste("age spline", a_spline), area = seq(0, num_j-1L), race = race_labels))
    
    if (!race_together) {
      combos[, race := get("race", .GlobalEnv)]
    }
    
    re1 <- cbind(re1, combos)
    setkeyv(re1, c("year_spline", "a_spline", "area", "race"))
    re_ls[[length(re_ls) + 1]] <- re1
  } else {
    re_ls[[length(re_ls) + 1]] <- NULL
  }
  
  message("RE2 creation")
  re2 <- mod[param == "re2"]
  
  if (nrow(re2) > 0) {
    if (model %in% c("model78")) {
      # get the combinations of areas and races
      combos <- as.data.table(expand.grid(area = seq(0, num_j - 1L), race = 1:num_r - 1L))
      if (!race_together) {
        combos[, race := get("race", .GlobalEnv)]
      }
      
      re2 <- cbind(re2, combos)
      
      setkeyv(re2, c("area", "race"))
    } else if (model %in% c("model84")) {
      re2 <- NULL
    } else {
      # generally always u2_{j}
      re2[, area := 1:num_j - 1L]
      setkeyv(re2, "area")
    }
    re_ls[[length(re_ls) + 1]] <- re2  
  } else {
    re_ls[[length(re_ls) + 1]] <- NULL
  }
  
  message("RE3 creation")
  re3 <- mod[param == "re3"]
  
  if (nrow(re3) > 0) {
    if (model %in% c("model35","model36", "model37", "model38", "model39", "model40","model41", "model42", "model57", "model79", "model93")) {
      # u3_{a,t,r}
      combos <- as.data.table(expand.grid(year = years, age = ages, race = race_labels))
      if(!race_together) {
        combos[, race := get("race", .GlobalEnv)]
      }
      
      re3 <- cbind(re3, combos)
      
      setkeyv(re3, c("year", "race", "age"))
    } else if (model %in% c("model44", "model45", "model46", "model47", "model48", "model49", "model51", "model81", "model52", "model53", "model55", "model64", "model73", "model80", "model78")) {
      # u3_{a,t,r,e,m}
      # age-year-race-edu-marital-level random intercept
      
      # Marital and edu get recoded later
      
      combos <- as.data.table(expand.grid(year = years, 
                                          age = ages, 
                                          race = race_labels, 
                                          edu = 1:num_e-1, 
                                          marital = 1:num_m-1))
      
      if(!race_together) {
        combos[, race := get("race", .GlobalEnv)]
      }
      
      re3 <- cbind(re3, combos)
      re3 <- re3[age >= 20]
      
      setkeyv(re3, c("year", "race", "age", "edu", "marital"))
      
    } else if (model %in% c("model103")) {
      # u3_{t,r,e,m}
      # age-year-race-edu-marital-level random intercept
      
      # Marital and edu get recoded later
      
      combos <- as.data.table(expand.grid(year = years, 
                                          race = race_labels, 
                                          edu = 1:num_e-1, 
                                          marital = 1:num_m-1))
      
      if(!race_together) {
        combos[, race := get("race", .GlobalEnv)]
      }
      
      re3 <- cbind(re3, combos)
      
      setkeyv(re3, c("year", "race", "edu", "marital"))
    } else if (model %in% c("model106", "model107")){
      # u3_{a,r,e,m}
      # age-race-edu-marital-level random intercept
      
      # Marital and edu get recoded later
      
      combos <- as.data.table(expand.grid(age = ages, 
                                          race = race_labels, 
                                          edu = 1:num_e-1, 
                                          marital = 1:num_m-1))
      
      if(!race_together) {
        combos[, race := get("race", .GlobalEnv)]
      }
      
      re3 <- cbind(re3, combos)
      
      setkeyv(re3, c("age", "race", "edu", "marital"))
      
    } else if (model %in% c("model54")) {
      # u3_{a,t',r,e,m}
      # age-year-race-edu-marital-level random intercept
      
      # Marital and edu get recoded later
      
      combos <- as.data.table(expand.grid(year_spline =  paste("year spline", t_spline), 
                                          age = ages, 
                                          race = race_labels, 
                                          edu = 1:num_e-1, 
                                          marital = 1:num_m-1))
      
      if(!race_together) {
        combos[, race := get("race", .GlobalEnv)]
      }
      
      re3 <- cbind(re3, combos)
      re3 <- re3[age >= 20]
      
      setkeyv(re3, c("year_spline", "race", "age", "edu", "marital"))
    } else if (model %in% c("model50")) {
      # u3_{a,t,r,e,m,g}
      # age-year-race-edu-marital-group quarters-level random intercept
      
      # Marital, edu and gq get recoded later
      
      combos <- as.data.table(expand.grid(year = years, 
                                          age = ages, 
                                          race = race_labels, 
                                          edu = 1:num_e-1, 
                                          marital = 1:num_m-1,
                                          gq = 1:num_g-1))
      
      if(!race_together) {
        combos[, race := get("race", .GlobalEnv)]
      }
      
      re3 <- cbind(re3, combos)
      re3 <- re3[age >= 20]
      
      setkeyv(re3, c("year", "race", "age", "edu", "marital", "gq"))
    } else if (model %in% c("model56")) {
      # u3_{a,t,r}
      # age-year-race-level random intercept
      
      combos <- as.data.table(expand.grid(year = years, 
                                          age = ages, 
                                          race = race_labels))
      
      if(!race_together) {
        combos[, race := get("race", .GlobalEnv)]
      }
      
      re1 <- cbind(re1, combos)
      setkeyv(re1, c("a_spline", "area", "race"))
    }
    re_ls[[1]] <- re1
  }
  
  
  message("RE2 creation")
  re2 <- mod[param == "re2"]
  
  # generally always u2_{j}
  
  if ("re2" %in% random_effects) {
    # u2_{j}
    # area effect
    re2[, area := 1:num_j - 1L]
    setkeyv(re2, "area")
    re_ls[[2]] <- re2
  }
  
  message("RE3 creation")
  re3 <- mod[param == "re3"]
  
  if (model %in% c("model31","model35","model36", "model37", "model38", "model39","model40","model41",
                   "model42","model43")) {
    # u3_{a,t,r}
    combos <- as.data.table(expand.grid(year = years, 
                                        age = ages, 
                                        race = race_labels))
    if(!race_together) {
      combos[, race := get("race", .GlobalEnv)]
    }
    
    re3 <- cbind(re3, combos)
    
    setkeyv(re3, c("year", "race", "age"))
    re_ls[[3]] <- re3
  } else if(model %in% c("model44", "model45", "model46", "model47", "model48", "model49", "model51")) {
    # u3_{a,t,r,e,m}
    # age-year-race-edu-marital-level random intercept
    
    # Marital and edu get recoded later
    
    combos <- as.data.table(expand.grid(year = years, 
                                        age = ages, 
                                        race = race_labels, 
                                        edu = 1:num_e-1, 
                                        marital = 1:num_m-1))
    
    if(!race_together) {
      combos[, race := get("race", .GlobalEnv)]
    }
    
    re3 <- cbind(re3, combos)
    re3 <- re3[age >= 20]
    
    setkeyv(re3, c("year", "race", "age", "edu", "marital"))
    re_ls[[3]] <- re3
    
  } else if(model %in% c("model54")) {
    # u3_{a,t',r,e,m}
    # age-year-race-edu-marital-level random intercept
    
    # Marital and edu get recoded later
    
    combos <- as.data.table(expand.grid(year_spline =  paste("year spline", t_spline), 
                                        age = ages, 
                                        race = race_labels, 
                                        edu = 1:num_e-1, 
                                        marital = 1:num_m-1))
    
    if(!race_together) {
      combos[, race := get("race", .GlobalEnv)]
    }
    
    re3 <- cbind(re3, combos)
    re3 <- re3[age >= 20]
    
    setkeyv(re3, c("year_spline", "race", "age", "edu", "marital"))
    re_ls[[3]] <- re3
    
  } else if (model %in% c("model50")) {
    # u3_{a,t,r,e,m,g}
    # age-year-race-edu-marital-group quarters-level random intercept
    
    # Marital, edu and gq get recoded later
    
    combos <- as.data.table(expand.grid(year = years, 
                                        age = ages, 
                                        race = race_labels, 
                                        edu = 1:num_e-1, 
                                        marital = 1:num_m-1,
                                        gq = 1:num_g-1))
    
    if(!race_together) {
      combos[, race := get("race", .GlobalEnv)]
      re3 <- cbind(re3, combos)
      re3 <- re3[age >= 20]
      
      setkeyv(re3, c("year", "race", "age"))
      
    } else if (model %in% c("model84", "model88")) {
      re3 <- NULL
    }
    re_ls[[length(re_ls) + 1]] <- re3
  } else {
    re_ls[[length(re_ls) + 1]] <- NULL
  }
  
  if("re4" %in% random_effects) {
    message("RE4 creation")
    
    re4 <- mod[param == "re4"]
    
    if (model == "model35") {
      # u4_{v}
      # Will need to be recoded
      re4[, survey_version := 1:num_v - 1L]
      setkeyv(re4, "survey_version")
    } 
    
    if (model %in% c("model36", "model45", "model56")) {
      # u4_{d} i.i.d effect on data source
      # Will need to be recoded
      re4[, source := 1:num_d - 1L]
      setkeyv(re4, "source")
    }
    
    if (model %in% c("model37", "model38", "model39", "model41")) {
      # u4_{e} education
      # Will need to be recoded
      re4[, edu := 1:num_e - 1L]
      setkeyv(re4, "edu")
    }
    
    if (model %in% c("model42")) {
      # u4_{m}
      # Will need to be recoded
      re4[, marital := 1:num_m - 1L]
      setkeyv(re4, "marital")
    }
    
    if (model == "model43") {
      # u4_{a,r,e,m}
      # Marital and edu get recoded later
      combos <- as.data.table(expand.grid(area = seq(0, num_j-1L),
                                          race = race_labels,
                                          year = years, 
                                          age = ages, 
                                          edu = 1:num_e-1,
                                          marital = 1:num_m-1
      ))
      
      if (!race_together) {
        combos[, race := get("race", .GlobalEnv)]
      }
      
      re4 <- cbind(re4, combos)
      
      setkeyv(re4, c("area","year", "race", "age"))
    }
    
    if (model %in% c("model46","model47", "model48", "model49", "model50", "model51", "model81", "model55", "model64", "model73", "model80", "model84", "model78", "model88", "model91", "model94")) {
      # u4_{t,a,r} but model46 has indicator: u4_{t,a,r}*(U=1)
      combos <- as.data.table(expand.grid(year = years, 
                                          age = ages, 
                                          race = race_labels))
      
      if (!race_together) {
        combos[, race := get("race", .GlobalEnv)]
      }
      
      re4 <- cbind(re4, combos)
      
      if (model == "model46") {
        re4 <- re4[age < 20]
      }
      
      setkeyv(re4, c("year", "race", "age"))
    }
    
    if (model %in% c("model54")) {
      # u4_{t,a,r} but model46 has indicator: u4_{t,a,r}*(U=1)
      combos <- as.data.table(expand.grid(year_spline =  paste("year spline", t_spline), 
                                          age = ages, 
                                          race = race_labels))
      
      if(!race_together) {
        combos[, race := get("race", .GlobalEnv)]
      }
      re4 <- cbind(re4, combos)
      
      setkeyv(re4, c("year_spline", "race", "age"))
    }
    re_ls[[length(re_ls) + 1]] <- re4
  } else {
    re_ls[[length(re_ls) + 1]] <- NULL
  }
  
  if("re5" %in% random_effects) {
    message("RE5 creation")
    re5 <- mod[param == "re5"]
    
    if(model %in% c("model35", "model36")) {
      # u5_{e} 
      # Will need to be recoded
      re5[, edu := 1:num_e - 1L]
      setkeyv(re5, "edu")
    }
    
    if (model %in% c("model37", "model38")) {
      # Will need to be recoded
      re5[, marital := 1:num_m - 1L]
      setkeyv(re5, "marital")
    }
    
    if (model %in% c("model49", "model55", "model88")) {
      # u5_{d} i.i.d effect on data source
      # Will need to be recoded
      re5[, source := 1:num_d - 1L]
      setkeyv(re5, "source")
    } else if (model %in% c("model73", "model80", "model84", "model78", "model91", "model94", "model106", "model107")) {
      # u5_{r,d} i.i.d effect on data source
      # Will need to be recoded
      combos <- as.data.table(expand.grid(race = 0:(num_r - 1L), source = 0:(num_d - 1L)))
      if (!race_together) {
        combos[, race := get("race", .GlobalEnv)]
      } else if (model %in% c("model120", "model125", "model126", "model127")){
        stop("should not get to this line -- newer models have parameters saved in re_param_[sex].rds")
      }
      re5 <- cbind(re5, combos)
      
      # I think that this ordering is correct
      setkeyv(re5, c("race", "source"))
    }
    
    re_ls[[length(re_ls) + 1]] <- re5
  } else {
    re_ls[[length(re_ls) + 1]] <- NULL
  }
  
  if("re6" %in% random_effects) {
    message("RE6 creation")
    re6 <- mod[param == "re6"]
    
    if (model %in% c("model35", "model36")) {
      # u6_{m} 
      # Will need to be recoded
      re6[, marital := 1:num_m - 1L]
      setkeyv(re6, "marital")
    } else if (model %in% c("model37")) {
      # Will need to be recoded
      re6[, gq := 1:num_g - 1L]
      setkeyv(re6, "gq")
    } else if (model %in% c("model91")) {
      # Will need to be recoded
      re6[, state := 1:num_w - 1L]
      setkeyv(re6, "state")
    } else if (model %in% c("model103", "model106", "model107")) {
      # Will need to be recoded
      re6[, year := 1:num_t - 1L]
      setkeyv(re6, "year")
    }
    
    re_ls[[length(re_ls) + 1]] <- re6
  } else {
    re_ls[[length(re_ls) + 1]] <- NULL
  }
  
  if ("re7" %in% random_effects) {
    message("RE7 creation")
    re7 <- mod[param == "re7"]
    if (model %in% c("model35")) {
      # Will need to be recoded
      re7[, gq := 1:num_g - 1L]
      setkeyv(re7, "gc")
    } else if (model %in% c("model103", "model106", "model107")) {
      # Will need to be recoded
      re7[, age := 1:num_a - 1L]
      setkeyv(re7, "age")
    }    
    re_ls[[length(re_ls) + 1]] <- re7
  } else {
    re_ls[[length(re_ls) + 1]] <- NULL
  }
  
  if ("re8" %in% random_effects) {
    message("RE8 creation")
    re8 <- mod[param == "re8"]
    if (model %in% c("model103", "model106", "model107")) {
      # Will need to be recoded
      re8[, race := 1:num_r - 1L]
      setkeyv(re8, "race")
    }
    re_ls[[length(re_ls) + 1]] <- re8
  } else {
    re_ls[[length(re_ls) + 1]] <- NULL
  }
  
  if ("re9" %in% random_effects){
    message("RE9 creation")
    re9 <- mod[param == "re9"]
    if (model %in% c("model107")){
      # u9_{t,r,e,m}
      # year-race-edu-marital-level random intercept
      
      # Marital and edu get recoded later
      
      combos <- as.data.table(expand.grid(year = years, 
                                          race = race_labels, 
                                          edu = 1:num_e-1, 
                                          marital = 1:num_m-1))
      
      if(!race_together) {
        combos[, race := get("race", .GlobalEnv)]
      }
      stopifnot(re9[, .N] == combos[,.N])
      re9 <- cbind(re9, combos)
      
      setkeyv(re9, c("year", "race", "edu", "marital"))
    }
    re_ls[[length(re_ls) + 1]] <- re9
  } else{
    re_ls[[length(re_ls) + 1]] <- NULL
  }
  ### Combine the random effects
  
  message("Combine random effects")
  
  for (i in 1:length(re_ls)) {
    if (is.null(re_ls[[i]])) {
      next
    }
    re_dt <- re_ls[[i]]
    setnames(re_dt, "est", "mean")
    re_dt[, c("lower", "upper") := list(mean - 1.96 * se, mean + 1.96 * se)]
    re_dt[, sex := get("sex", .GlobalEnv)]
    message("Start recoding vars")
    
    # Recode if needed
    if (!is.null(re_dt$edu)) {
      re_dt <- merge(re_dt, recode$edu, by.x = "edu", by.y = "edu_recode")
      re_dt[, c("edu", "edu.y", "var") := list(edu.y, NULL, NULL)]
      re_dt[edu == 1, edu_lab := "Less than HS"]
      re_dt[edu == 2, edu_lab := "HS"]
      re_dt[edu == 3, edu_lab := "Some college"]
      re_dt[edu == 4, edu_lab := "BA or higher"]
      re_dt$edu_lab <- fct_reorder(re_dt$edu_lab, re_dt$edu)
    }
    
    if (!is.null(re_dt$source)) {
      re_dt <- merge(re_dt, recode$source, by.x = "source", by.y = "source_index_recode")
      re_dt[, c("source", "source_index", "var") := list(source_index, NULL, NULL)]
    }
    
    if (!is.null(re_dt$marital)) {
      re_dt <- merge(re_dt, recode$marital, by.x = "marital", by.y = "marital_recode")
      re_dt[, c("marital", "marital.y", "var") := list(marital.y, NULL, NULL)]
      re_dt[marital == 1, marital_lab := "current"]
      re_dt[marital == 2, marital_lab := "former"]
      re_dt[marital == 3, marital_lab := "never married"]
    }
    if (!is.null(re_dt$state)) {
      re_dt <- merge(re_dt, recode$state, by.x = "state", by.y = "state_recode")
      re_dt[, c("state", "state.y", "var") := list(state.y, NULL, NULL)]
    }
    
    # save to list 
    re_ls[[i]] <- re_dt
  }
  return(re_ls)
}

if (is.null(re_param)) {
  mod_ls <- get_re(model_name)
  mod <- rbindlist(mod_ls, fill = T, use.names = T)
  mod[, race_lab := race]
} else { # format re_param object produced by complete_pred_subdraw
  mod_ls <- re_param
  mod_ls[["re1"]] <- unique(mod_ls[["re1"]][, -c("year", "age", "y_spline_value", "a_spline_value")])
  
  # if RE3 has splines, remove repeated values similar to RE1
  if(any(grepl("spline", names(mod_ls[["re3"]])))){
    mod_ls[["re3"]] <- unique(mod_ls[["re3"]][, -c("year", "age", "y_spline_value", "a_spline_value")])
  }
  
  # in some models (so far, model145 and model146), I save re_mean and re_param for 
  #   re6 and re7...remove those from mod_ls for now 
  if("re6_param" %in% names(mod_ls)){
    mod_ls[["re6_param"]] <- NULL
  }
  if("re7_param" %in% names(mod_ls)){
    mod_ls[["re7_param"]] <- NULL
  }

  mod <- rbindlist(mod_ls, fill = TRUE, use.names = TRUE)
  setnames(mod, "est", "mean")
  mod[, c("lower", "upper") := list(mean - 1.96 * se, mean + 1.96 * se)]
  mod[, id := .I]
  
  if (!is.null(mod$edu)) {
    mod <- merge(mod, recode$edu, by.x = "edu", by.y = "edu_recode", all.x = TRUE)
    mod[, c("edu", "edu.y", "var") := list(edu.y, NULL, NULL)]
    mod[edu == 1, edu_lab := "Less than HS"]
    mod[edu == 2, edu_lab := "HS"]
    mod[edu == 3, edu_lab := "Some college"]
    mod[edu == 4, edu_lab := "BA or higher"]
    mod$edu_lab <- fct_reorder(mod$edu_lab, mod$edu)
  }
  
  if (!is.null(mod[["source"]])) { # need to check for exact name source otherwise DT will return the source_race column as a partial match
    mod <- merge(mod, recode$source, by.x = "source", by.y = "source_index_recode", all.x = TRUE)
    mod[, c("source", "source_index", "var") := list(source_index, NULL, NULL)]
  }

  if(!is.null(mod[["source_v2"]])){
    mod <- merge(mod, recode$source_v2, by.x = "source_v2", by.y = "source_v2_index_recode", all.x = TRUE)
    mod[, c("source_v2", "source_v2_index", "var") := list(source_v2_index, NULL, NULL)]
  }
  
  if (!is.null(mod$marital)) {
    mod <- merge(mod, recode$marital, by.x = "marital", by.y = "marital_recode", all.x = TRUE)
    mod[, c("marital", "marital.y", "var") := list(marital.y, NULL, NULL)]
    mod[marital == 1, marital_lab := "Current"]
    mod[marital == 2, marital_lab := "Former"]
    mod[marital == 3, marital_lab := "Never married"]
  }
  
  if (!is.null(mod$race)) {
    mod <- merge(mod, recode$race, by.x = "race", by.y = "race_recode", all.x = T)
    mod[, c("race", "race.y", "var") := list(race.y, NULL, NULL)]
    race_labs <- data.table(race = as.integer(races), race_lab = race_labels)
    mod <- merge(mod, race_labs, by = "race", all.x = TRUE)
  }
  
  if (!is.null(mod[["source_race"]])) {
    mod <- mod[param != 0] # Gold standard source
    mod[!is.na(source_race), source_race_tmp := paste0(source, "_", race)]
    mod[, c("source_race", "source_race_tmp") := list(source_race_tmp, NULL)]
  }

  if(!is.null(mod[["source_race_v2"]])){
    mod[!is.na(source_race_v2), source_v2_race_tmp := paste0(source_v2, "_", race)]
    mod[, c("source_race_v2", "source_v2_race_tmp") := list(source_v2_race_tmp, NULL)]
  }

  if (!is.null(mod$state)) {
    mod <- merge(mod, recode$state, by.x = "state", by.y = "state_recode", all.x = T)
    mod[, c("state", "state.y", "var") := list(state.y, NULL, NULL)]
  }
  
  if (!is.null(mod$area)){
    mod <- merge(mod, recode$area, by.x = "area", by.y = "area_recode", all.x = T)
    mod[, c("area", "mcnty", "var") := list(mcnty, NULL, NULL)]
  }
  
  if (!is.null(mod$age)){
    mod <- merge(mod, recode$age, by.x = "age", by.y = "age_recode", all.x = T)
    mod[, c("age", "var", "age.y") := list(age.y, NULL, NULL)]
  }
  
  setnames(mod, c("y_spline"), c("year_spline"))
}

pdf(paste0(output_dir, "/random_effects_", sex, "_", imp, ".pdf"), width = 16, height = 8)

mod$param <- factor(mod$param, levels = str_sort(unique(mod$param), numeric = TRUE)) # Make sure that params are ordered correctly (e.g., re10 should come after re9, not re1)
# save the random effects that we plot
if(!dir.exists(paste0(output_dir, "/vetting"))){
  dir.create(paste0(output_dir, "/vetting"))
}
saveRDS(mod, paste0(output_dir, "/vetting/random_effects_for_plotting_", sex, ".rds"))

# create plot with scattermore, which renders much faster geom_point()
print(ggplot(mod, aes(x = id, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
        geom_scattermore(color = "gray20") +
        geom_hline(yintercept = 0) +
        facet_wrap(~ param, scales = "free") +
        labs(title = "All random effects", y = "gamma"))

# plot the fixed effects
tmp <- mod[grepl("B", param)]
tmp <- tmp[!is.na(se)]
print(ggplot(tmp, aes(x = id, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
        geom_errorbar( color = "gray50") +
        geom_point(color = "gray20") +
        geom_hline(yintercept = 0) +
        facet_wrap(~ param, scales = "free") +
        labs(title = "Fixed effects", y = "beta"))

# create plot with scattermore, which renders much faster geom_point()
print(ggplot(mod, aes(x = id, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
  geom_scattermore(color = "gray20") +
  geom_hline(yintercept = 0) +
  facet_wrap(~ param, scales = "free") +
  labs(title = paste0("All random effects, ", sex_label), y = "gamma"))

# area-level random effect -------------------------------------------------------------------------
tmp <- mod[param == "re2",]

loc <- fread("FILEPATH")
# filtering to states in data -- useful when only modeling one state
states_in_tmp <- loc[tmp, on  = c(mcnty = "area"), .(min_state = min(state), max_state = max(state))]
loc_tmp <- loc[state >= states_in_tmp$min_state & state <= states_in_tmp$max_state, list(min_mcnty = min(mcnty)), state_name]

if (model %in% c("model78")) {
  for (r in unique(tmp$race)) {
    tmp_current <- tmp[race == r]
    ggplot() + theme_bw() + 
      geom_errorbar(data = tmp, aes(x = area, ymin = lower, ymax = upper),
                    size = 0.1, color = "gray50") +
      geom_point(data = tmp, aes(x = area, y = mean), size = 0.1, color = "gray20") +
      geom_hline(yintercept = 0) +
      geom_vline(data = loc_tmp, aes(xintercept = min_mcnty), size = 0.1, col = "gray20") +
      geom_text(data = loc_tmp, aes(x = min_mcnty, label = state_name, y = max(tmp$upper)),
                angle = -90, hjust = 0, vjust = 0, size = 3, col = "gray20") +
      labs(title = paste0("Area-level random effect, race ", r), y = "gamma")
    
    shp <- readRDS("FILEPATH")
    shp <- as(shp, "sf")
    
    tmp_current <- merge(shp, tmp_current, by.x = "mcnty", by.y = "area")
    
    ggplot() + theme_bw() + 
      geom_sf(data = tmp_current, aes(fill = mean), color = "transparent", lwd = 0) +
      scale_fill_gradientn(colors = rev(brewer.pal(9, "Spectral")),
                           limits = max(abs(range(tmp$mean) - 1)) * c(-1.01, 1.01) + 1) +
      coord_sf(expand = FALSE, datum = NA) +
      labs(title = paste0("Area-level random effect, race ", r), fill = "gamma")
  }
} else if (!(model %in% c("model84"))) {
  p <- ggplot() + theme_bw() + 
    geom_scattermore(data = tmp, aes(x = area, y = mean), size = 0.1, color = "gray20") + # geom_scattermore much faster than geom_point()
    geom_hline(yintercept = 0) +
    geom_vline(data = loc_tmp, aes(xintercept = min_mcnty), size = 0.1, col = "gray20") +
    geom_text(data = loc_tmp, aes(x = min_mcnty, label = state_name, y = max(tmp$upper)),
              angle = -90, hjust = 0, vjust = 0, size = 3, col = "gray20") +
    labs(title = "Area-level random effect", y = "gamma")
  print(p) # not sure why this doesn't print otherwise
  
  shp <- readRDS("FILEPATH")
  shp <- as(shp, "sf")
  
  tmp <- merge(shp, tmp, by.x = "mcnty", by.y = "area")
  
  p <- ggplot() + theme_bw() + 
    geom_sf(data = tmp, aes(fill = mean), color = "transparent", lwd = 0) +
    scale_fill_gradientn(colors = rev(brewer.pal(9, "Spectral")),
                         limits = max(abs(range(tmp$mean) - 1)) * c(-1.01, 1.01) + 1) +
    coord_sf(expand = FALSE, datum = NA) +
    labs(title = "Area-level random effect", fill = "gamma")
  print(p)
}

# RE 3 ---------------------------------------------------------------------------------
tmp <- mod[param == "re3",]

if (model %in% c(paste("model", c(44:49,51:53, 55, 60:61, 81, 64, 73, 80, 78, 121, 122, 124, 125, 131, 132, 133, 135, 136), sep = ""))) {
  # u3_{a,t,r,e,m}
  # age-year-race-edu-marital-level random intercept
  number_ticks <- function(n) {function(limits) pretty(limits, n)}
  
  print(ggplot(tmp, aes(x = factor(age), y = factor(year), fill = mean)) + theme_bw() + 
          geom_raster() +
          facet_grid(race ~ edu_lab + marital_lab) +
          scale_fill_gradientn(colors = brewer.pal(9, "PuOr")) +
          coord_equal() +
          labs(title = "Age-year-race-edu-marital-level random effect", x = "age group",
               y = "year", fill = "gamma") +
          scale_x_discrete(breaks=number_ticks(4)) +
          scale_y_discrete(breaks=number_ticks(4)) +
          theme(axis.text.x = element_text(angle=90, hjust=1, size = 8),
                axis.text.y = element_text(size = 8))) 
  
  # Make for loop for plots 
  edu_marital <- as.data.table(expand.grid(edu_lab = unique(tmp$edu_lab), marital_lab = unique(tmp$marital_lab)))
  for (z in 1:nrow(edu_marital)) {
    marital_z <- edu_marital[z, marital_lab]
    edu_z <- edu_marital[z, edu_lab]
    tmp2 <- tmp[edu_lab == edu_z & marital_lab == marital_z]
    print(ggplot(tmp2, aes(x = year, color = race_lab, y = mean, ymin = lower, ymax = upper)) + 
            theme_bw() + 
            geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
            geom_line(size = 0.1) +
            facet_wrap( ~ paste("Age group:", age), scales = "free") +
            labs(title = "Age-year-race-edu-marital-level random effect",
                 subtitle = paste0(str_to_sentence(edu_z), " & marital = ", marital_z), 
                 x = "year",
                 y = "gamma", 
                 color = "race/ethnicity"))
    
    print(ggplot(tmp2, aes(x = age, color = race_lab, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
            geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
            geom_line(size = 0.1) +
            facet_wrap(~ paste("Year:", year), scales = "free") +
            labs(title = "Age-year-race-edu-marital-level random effect",
                 subtitle = paste0(str_to_sentence(edu_z), " & marital = ", marital_z), 
                 x = "age",
                 y = "gamma", color = "race/ethnicity"))
    
  }
  
  print(ggplot(tmp, aes(x = year, color = age, group = age, y = mean, ymin = lower, ymax = upper), alpha = 0.5) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_grid(race_lab ~ edu_lab + marital_lab, scales = "free") +
          scale_color_gradientn(colors = brewer.pal(9, "PuRd")[4:9]) +
          labs(title = "Age-year-race-edu-marital-level random effect", x = "year",
               y = "gamma", color = "age") +
          theme(axis.text.x = element_text(angle=45, hjust=1, size = 5))) 
  
  print(ggplot(tmp, aes(x = age, color = year, group = year, y = mean, ymin = lower, ymax = upper), alpha = 0.5) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_grid(race_lab ~ edu_lab + marital_lab) +
          scale_color_gradientn(colors = brewer.pal(9, "PuRd")[4:9]) +
          labs(title = "Age-year-race-edu-marital-level random effect", x = "age",
               y = "gamma", color = "year"))
  
} else if (model %in% c("model106", "model107", "model115", "model120", "model137", "model138", "model139", "model145", "model146", "model147", "model148", "model149", "model151", "model152", "model154")){
  # u3_{a,r,e,m}
  # age-race-edu-marital-level random effect
  # Make for loop for plots 
  edu_marital <- as.data.table(expand.grid(edu_lab = unique(tmp$edu_lab), marital_lab = unique(tmp$marital_lab)))
  for (z in 1:nrow(edu_marital)) {
    marital_z <- edu_marital[z, marital_lab]
    edu_z <- edu_marital[z, edu_lab]
    tmp2 <- tmp[edu_lab == edu_z & marital_lab == marital_z]
    print(ggplot(tmp2, aes(x = age, color = race_lab, y = mean, ymin = lower, ymax = upper)) + 
            theme_bw() + 
            geom_pointrange(size = 0.5, position = position_dodge(width = 0.2)) +
            geom_line(size = 0.5) +
            labs(title = "Age-race-edu-marital-level random effect",
                 subtitle = paste0(str_to_sentence(edu_z), " & marital = ", marital_z), 
                 x = "age",
                 y = "gamma", 
                 color = "race/ethnicity"))
  }
} else if (model %in% c("model103")) {
  # Make for loop for plots 
  edu_marital <- as.data.table(expand.grid(edu_lab = unique(tmp$edu_lab), marital = unique(tmp$marital)))
  for (z in 1:nrow(edu_marital)) {
    marital_z <- edu_marital[z, marital]
    edu_z <- edu_marital[z, edu_lab]
    tmp2 <- tmp[edu_lab == edu_z & marital == marital_z]
    print(ggplot(tmp2, aes(x = year, color = race, y = mean, ymin = lower, ymax = upper)) + 
            theme_bw() + 
            geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
            geom_line(size = 0.1) +
            labs(title = "Age-year-race-edu-marital-level random effect",
                 subtitle = paste0(str_to_sentence(edu_z), " & marital = ", marital_z), 
                 x = "year",
                 y = "gamma", 
                 color = "race/ethnicity"))
  }
  
  print(ggplot(tmp, aes(x = year, y = mean, ymin = lower, ymax = upper), alpha = 0.5) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_grid(race_lab ~ edu_lab + marital_lab, scales = "free") +
          scale_color_gradientn(colors = brewer.pal(9, "PuRd")[4:9]) +
          labs(title = "Year-race-edu-marital-level random effect", x = "year",
               y = "gamma") +
          theme(axis.text.x = element_text(angle=45, hjust=1, size = 5))) 
} else if (model %in% c("model128")) {
  # u3_{t',a',r,e,m,w}
  
  # create a column for combinations of a_spline and year_splint
  tmp[, spline_combo := paste0(a_spline, ",", year_spline)]

  # Want to see how the effects of marital & edu vary by state and time
  # Make a heatmap with:
  # X axis: state
  # Y axis: age & year spline
  # facets: edu & marital
  # plot separately by race
  for(r in unique(tmp$race_lab)){
    print(ggplot(tmp[race_lab == r], aes(x = factor(state), y = factor(spline_combo), fill = mean)) + theme_bw() + 
          geom_raster() +
          facet_grid(edu_lab ~ marital_lab) +
          scale_fill_gradientn(colors = brewer.pal(9, "PuOr")) +
          labs(title = "Year-age-edu-marital-state random effect, by race", 
               subtitle = r, 
               y = "Age/year spline combo (a', y')",
               fill = "gamma"))
    # Show how the age pattern varied by year and state
    # plot age spline on x, effect on y, separate line for each state; facet by year spline
    print(ggplot(tmp[race_lab == r], aes(x = a_spline, y = mean, color = state)) +
          geom_point(size = 0.2, alpha = 0.5) + 
          geom_line(aes(group = as.factor(state)), alpha = 0.5) +
          scale_color_gradientn(colors = brewer.pal(9, "PuRd")[2:9]) +
          facet_grid(year_spline ~ edu_lab + marital_lab, labeller = labeller(year_spline = function(x) paste0("y' ", x)))+
          labs(x = "Age spline", y = "gamma", 
              title = "Year-age-edu-marital-state random effect, by race",
              subtitle = r)
    )
    # similar plot, but make year spline the x-axis & a_spline the y-axis
    print(ggplot(tmp[race_lab == r], aes(x = year_spline, y = mean, color = state)) +
          geom_point(size = 0.2, alpha = 0.5) + 
          geom_line(aes(group = as.factor(state)), alpha = 0.5) +
          facet_grid(a_spline ~ edu_lab + marital_lab, labeller = labeller(a_spline = function(x) paste0("a' ", x)))+
          scale_color_gradientn(colors = brewer.pal(9, "PuRd")[2:9]) +
          labs(x = "Year spline", y = "gamma", 
              title = "Year-age-edu-marital-state random effect, by race",
              subtitle = r)
    )
  }
  
  

} else if (!(model %in% c("model84", "model88", "model94", "model127"))) {
  # u3_{a,t,r}
  # most models have the above u3 so not going to specify models with an if/statement for now
  print(ggplot(tmp, aes(x = factor(age), y = factor(year), fill = mean)) + theme_bw() + 
          geom_raster() +
          facet_grid(~ race) +
          scale_fill_gradientn(colors = brewer.pal(9, "PuOr")) +
          coord_equal() +
          labs(title = "Age-year-race/ethnicity-level random effect", x = "age group",
               y = "year", fill = "gamma"))
  
  print(ggplot(tmp, aes(x = year, color = race, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_wrap(~ paste("Age group:", age), scales = "free") +
          labs(title = "Age-year-race/ethnicity-level random effect", x = "year",
               y = "gamma", color = "race/ethnicity"))
  
  print(ggplot(tmp, aes(x = year, color = age, group = age, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_wrap(~ race, scales = "free") +
          scale_color_gradientn(colors = brewer.pal(9, "PuRd")[4:9]) +
          labs(title = "Age-year-race/ethnicity-level random effect", x = "year",
               y = "gamma", color = "age"))
  
  print(ggplot(tmp, aes(x = age, color = race, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_wrap(~ paste("Year:", year), scales = "free") +
          labs(title = "Age-year-race/ethnicity-level random effect", x = "age",
               y = "gamma", color = "race/ethnicity"))
  
  print(ggplot(tmp, aes(x = age, color = year, group = year, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_wrap(~ race) +
          scale_color_gradientn(colors = brewer.pal(9, "PuRd")[4:9]) +
          labs(title = "Age-year-race/ethnicity-level random effect", x = "age",
               y = "gamma", color = "year"))
}

# age(spline)-year(spline)-county-race or age(spline)-county-race RE ----------------------------------------------------------
# u1_{j,a',r}*S(a') 
if (!model %in% c("model39", "model40", "model51", "model81", "model54", "model55", "model56", "model64", "model73", "model80", "model84", "model78", "model88", "model93", "model91", "model94", "model106", "model107", "model115", "model120", "model121", "model122", "model124", "model125", "model127", "model128", "model131", "model132", "model133", "model135", "model136", "model137", "model138", "model139", "model145", "model146", "model147", "model148", "model149", "model151", "model152", "model154")) {
  tmp <- mod[param == "re1", ]
  
  print(ggplot(tmp, aes(x = area, color = race_lab, y = mean)) + theme_bw() +
          geom_point(size = 0.1) +
          geom_point(data = tmp, shape = 1, size = 0.5, stroke = 1) +
          geom_hline(yintercept = 0) +
          geom_vline(data = loc_tmp, aes(xintercept = min_mcnty), size = 0.1, col = "gray20") +
          geom_text(data = loc_tmp, aes(x = min_mcnty, label = state_name, y = max(tmp$mean)),
                    angle = -90, hjust = 0, vjust = 0, size = 2, col = "gray20") +
          facet_wrap(~ a_spline, nrow = 1) +
          guides(color = guide_legend(override.aes = list(size = 3))) +
          labs(title = paste0("Age(spline)-area-race/ethnicity-level random effect, ", sex_label),
               y = "gamma", color = "race/ethnicity"))
  
  for (r in race_labels) {
    tmp2 <- merge(shp, tmp[race == r, ], by.x = "mcnty", by.y = "area")
    
    gg <- ggplot() + theme_bw() +
      geom_sf(data = tmp2, aes(fill = mean), color = "transparent", lwd = 0) +
      facet_wrap(~ a_spline, nrow = 1) +
      scale_fill_gradientn(colors = rev(brewer.pal(9, "Spectral")),
                           limits = max(abs(range(tmp2$mean) - 1)) * c(-1.01, 1.01) + 1) +
      coord_sf(expand = F, datum = NA) +
      labs(title = paste0("Age(spline)-area-race/ethnicity-level random effect, ", sex_label),
           subtitle = r, fill = "gamma")
    print(gg)
  }
} else if (model %in% c("model51", "model81", "model54", "model55", "model56", "model64", "model57", "model79", "model73", "model80", "model78",  "model106", "model107", "model115", "model120", "model121", "model122", "model124", "model125", "model127", "model128", "model131", "model132", "model133", "model135", "model136", "model137", "model138", "model139", "model145", "model146", "model147", "model148", "model149", "model151", "model152", "model154")) {
  tmp <- mod[param == "re1",]
  
  print(ggplot(tmp, aes(x = area, color = race_lab, y = mean)) + theme_bw() +
          geom_point(data = tmp, shape = 1, size = 0.5, stroke = 0.2) +
          geom_hline(yintercept = 0) +
          geom_vline(data = loc_tmp, aes(xintercept = min_mcnty), size = 0.1, col = "gray20") +
          geom_text(data = loc_tmp, aes(x = min_mcnty, label = state_name, y = max(tmp$mean)),
                    angle = -90, hjust = 0, vjust = 0, size = 2, col = "gray20") +
          facet_grid(year_spline ~ a_spline, labeller = label_both) +
          guides(color = guide_legend(override.aes = list(size = 3))) +
          labs(title = "Year(spline)-age(spline)-area-race/ethnicity-level random effect",
               y = "gamma", color = "race/ethnicity"))
  
  for (r in races) {
    tmp2 <- merge(shp, tmp[race == r,], by.x = "mcnty", by.y = "area")
    race_z <- race_labels[races == r]
    gg <- ggplot() + theme_bw() +
      geom_sf(data = tmp2, aes(fill = mean), color = "transparent", lwd = 0) +
      facet_grid(year_spline ~ a_spline, labeller = label_both) +
      scale_fill_gradientn(colors = rev(brewer.pal(9, "Spectral")),
                           limits = max(abs(range(tmp2$mean) - 1)) * c(-1.01, 1.01) + 1) +
      coord_sf(expand = F, datum = NA) +
      labs(title = "Year(spline)-age(spline)-area-race/ethnicity-level random effect",
           subtitle = race_z, fill = "gamma")
    print(gg)
  }
}

if (model %in% c("model30", "model38", "model39", "model41")) {
  tmp <- mod[param == "re4", ]
  
  print(ggplot() + theme_bw() + 
          geom_errorbar(data = tmp, aes(x = survey_version, ymin = lower, ymax = upper),
                        size = 0.1, color = "gray50") +
          geom_point(data = tmp, aes(x = survey_version, y = mean), size = 0.1, color = "gray20") +
          geom_hline(yintercept = 0) +
          geom_vline(data = loc_tmp, aes(xintercept = min_mcnty), size = 0.1, col = "gray20") +
          geom_text(data = loc_tmp, aes(x = min_mcnty, label = state_name, y = max(tmp$upper)),
                    angle = -90, hjust = 0, vjust = 0, size = 3, col = "gray20") +
          labs(title = "Survey version-level random effect", y = "gamma"))  
  
}

if (model %in% c("model46", "model47", "model48", "model49", "model50", "model51", "model81", "model55", "model64", "model78")) {
  # u4_{t,a,r,e,m}
  tmp <- mod[param == "re4", ]
  print(ggplot(tmp, aes(x = factor(age), y = factor(year), fill = mean)) + theme_bw() + 
          geom_raster() +
          facet_grid(edu_lab ~ race_lab) +
          scale_fill_gradientn(colors = brewer.pal(9, "PuOr")) +
          coord_equal() +
          labs(title = "Age-year-race/ethnicity-level random effect", x = "age group",
               y = "year", fill = "gamma"))
  
  print(ggplot(tmp, aes(x = year, color = race_lab, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_wrap(~ paste("Age group:", age), scales = "free") +
          labs(title = "Age-year-race/ethnicity-level random effect", x = "year",
               y = "gamma", color = "race/ethnicity"))
  
  print(ggplot(tmp, aes(x = year, color = age, group = age, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_wrap(~ race_lab, scales = "free") +
          scale_color_gradientn(colors = brewer.pal(9, "PuRd")[4:9]) +
          labs(title = "Age-year-race/ethnicity-level random effect", x = "year",
               y = "gamma", color = "age"))
  
  print(ggplot(tmp, aes(x = age, color = race_lab, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_wrap(~ paste("Year:", year), scales = "free") +
          labs(title = "Age-year-race/ethnicity-level random effect", x = "age",
               y = "gamma", color = "race/ethnicity"))
  
  print(ggplot(tmp, aes(x = age, color = year, group = year, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_wrap(~ race_lab) +
          scale_color_gradientn(colors = brewer.pal(9, "PuRd")[4:9]) +
          labs(title = "Age-year-race/ethnicity-level random effect", x = "age",
               y = "gamma", color = "year"))
} else if (model %in% c("model84", "model88", "model91", "model94","model73", "model80", "model122", "model125", "model127", "model128", "model131", "model132")) {
  # u4_{t,a,r}
  tmp <- mod[param == "re4",]
  print(ggplot(tmp, aes(x = factor(age), y = factor(year), fill = mean)) + theme_bw() + 
          geom_raster() +
          facet_grid(~ race_lab) +
          scale_fill_gradientn(colors = brewer.pal(9, "PuOr")) +
          coord_equal() +
          labs(title = "Age-year-race/ethnicity-level random effect", x = "age group",
               y = "year", fill = "gamma"))
  
  print(ggplot(tmp, aes(x = year, color = race_lab, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_wrap(~ paste("Age group:", age), scales = "free") +
          labs(title = "Age-year-race/ethnicity-level random effect", x = "year",
               y = "gamma", color = "race/ethnicity"))
  
  print(ggplot(tmp, aes(x = year, color = age, group = age, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_wrap(~ race_lab, scales = "free") +
          scale_color_gradientn(colors = brewer.pal(9, "PuRd")[4:9]) +
          labs(title = "Age-year-race/ethnicity-level random effect", x = "year",
               y = "gamma", color = "age"))
  
  print(ggplot(tmp, aes(x = age, color = race_lab, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_wrap(~ paste("Year:", year), scales = "free") +
          labs(title = "Age-year-race/ethnicity-level random effect", x = "age",
               y = "gamma", color = "race/ethnicity")+
          scale_x_continuous())
  
  print(ggplot(tmp, aes(x = age, color = year, group = year, y = mean, ymin = lower, ymax = upper)) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_wrap(~ race_lab) +
          scale_color_gradientn(colors = brewer.pal(9, "PuRd")[4:9]) +
          labs(title = "Age-year-race/ethnicity-level random effect", x = "age",
               y = "gamma", color = "year"))
}

if (model %in% c("model36", "model45", "model56", "model58")) {
  tmp <- mod[param == "re4", ]
  print(ggplot() + theme_bw() + 
          geom_errorbar(data = tmp, aes(x = source, ymin = lower, ymax = upper, color = source)) +
          geom_point(data = tmp, aes(x = source, y = mean, color = source)) +
          geom_hline(yintercept = 0) +
          labs(title = "Data source-level random effect", y = "gamma")) 
}

if (model %in% c("model36", "model45", "model56")) {
  tmp <- mod[param == "re4", ]
  print(ggplot() + theme_bw() + 
          geom_errorbar(data = tmp, aes(x = source, ymin = lower, ymax = upper, color = source)) +
          geom_point(data = tmp, aes(x = source, y = mean, color = source)) +
          geom_hline(yintercept = 0) +
          labs(title = "Data source-level random effect", y = "gamma")) 
}

if (model %in% c("model133", "model136")) {
  # plot u4 (time/race( and u9 (age/race)) next to each other)
  # u4_{t,r}
  tmp <- mod[param == "re4",]
  # plot year vs gamma, color/facet by race
  p <- ggplot(tmp, aes(x = year, y = mean, ymin = lower, ymax = upper, color = race_lab)) + theme_bw() +
    geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
    geom_line(size = 0.3) +
    labs(title = "Year/race random effect",
        x = "year",
        y = "gamma")+
        scale_x_continuous()
  print(p)
  print(p + facet_wrap(~race_lab))
}

if (model %in% c("model35", "model36")) {
  tmp <- mod[param == "re5", ]
  print(ggplot() + theme_bw() + 
          geom_errorbar(data = tmp, aes(x = edu_lab, ymin = lower, ymax = upper, color = edu_lab)) +
          geom_point(data = tmp, aes(x = edu_lab, y = mean, color = edu_lab)) +
          geom_hline(yintercept = 0) +
          labs(title = "Education-level random effect", y = "gamma", color = "edu"))  
}

if (model %in% c("model37", "model38", "model39")) {
  tmp <- mod[param == "re5", ]
  print(ggplot() + theme_bw() + 
          geom_errorbar(data = tmp, aes(x = marital_lab, ymin = lower, ymax = upper, color = marital_lab)) +
          geom_point(data = tmp, aes(x = marital_lab, y = mean, color = marital_lab)) +
          geom_hline(yintercept = 0) +
          labs(title = "Marital-level random effect", y = "gamma", color = "marital"))  
}

if (model %in% c("model49", "model55", "model88")) {
  tmp <- mod[param == "re5", ]
  print(ggplot() + theme_bw() + 
          geom_errorbar(data = tmp, aes(x = source, ymin = lower, ymax = upper, color = source)) +
          geom_point(data = tmp, aes(x = source, y = mean, color = source)) +
          geom_hline(yintercept = 0) +
          labs(title = "Data source-level random effect", y = "gamma")) 
} else if (model %in% c("model73", "model80", "model78", "model84", "model91", "model94")) {
  tmp <- mod[param == "re5", ]
  print(ggplot() + theme_bw() + 
          facet_wrap(~ race) +
          geom_errorbar(data = tmp, aes(x = source, ymin = lower, ymax = upper, color = source)) +
          geom_point(data = tmp, aes(x = source, y = mean, color = source)) +
          geom_hline(yintercept = 0) +
          labs(title = "Data source-race-level random effect", y = "gamma")) 
  print(ggplot() + theme_bw() + 
          facet_wrap(~ source) +
          geom_errorbar(data = tmp, aes(x = race, ymin = lower, ymax = upper, color = race)) +
          geom_point(data = tmp, aes(x = race, y = mean, color = race)) +
          geom_hline(yintercept = 0) +
          labs(title = "Data source-race-level random effect", y = "gamma"))
} else if (model %in% c("model115", "model121", 'model122')) {
  # u5_{r,d|d != "BRFSS"} 
  tmp <- mod[param == "re5",]
  print(ggplot(data = tmp, aes(x = source_race)) + theme_bw() + 
          geom_errorbar(aes(ymin = lower, ymax = upper)) +
          geom_point(aes(y = mean)) +
          geom_hline(yintercept = 0) +
          labs(title = "Data source-race-level random effect", y = "gamma",
               caption = 'u5_{r,d|d != "BRFSS"} (among races present in non-BRFSS sources)')) 
} else if (model %in% c("model120", "model125", "model127", "model128", "model131")) {
  # u5_{r,a,d|d != "BRFSS"} 
  tmp <- mod[param == "re5",]
  print(ggplot(data = tmp, aes(x = age, color = race_lab)) + theme_bw() + 
          geom_pointrange(aes(y = mean, x = age, ymin = lower, ymax = upper, shape = source), size = 0.5, position = position_dodge(width = 0.2)) +
          geom_line(aes(y = mean)) +
          geom_hline(yintercept = 0) +
          facet_wrap(~source) +
          labs(title = "Age-source-race-level random effect", y = "gamma",
               caption = 'u5_{r,a,d|d != "BRFSS"} (among races present in non-BRFSS sources)')) 
} else if (model %in% c("model132", "model133", "model135", "model136", "model137", "model138", "model139", "model145", "model146", "model147", "model148", "model149", "model152")){
  # u5_{r, a, d_v2|d_v2 != "BRFSS_LLCP"}
  tmp <- mod[param == "re5",]
  p <- ggplot(data = tmp, aes(x = age, color = race_lab)) + theme_bw() +
    geom_pointrange(aes(y = mean, x = age, ymin = lower, ymax = upper, shape = source_v2), size = 0.5, position = position_dodge(width = 0.2)) +
    geom_line(aes(y = mean)) +
    geom_hline(yintercept = 0) +
    labs(title = "Age-source (version 2)-race-level random effect", y = "gamma",
         caption = "u5_{r, a, d_v2|d_v2 != 'BRFSS_LLCP'}\n(among races present in non-gold standard sources)\nSources are BRFSS LLCP (gold-standard; combined landline/cellphone sample);BRFSS landline only; Gallup")
  print(p + facet_wrap(~source_v2))
  print(p + facet_grid(source_v2~race_lab))
} else if (model %in% c("model151", "model154")){
  # u5_{r, d_v2|d_v2 != "BRFSS_LLCP"}
  tmp <- mod[param == "re5",]
  p <- ggplot(data = tmp, aes(x = race_lab, color = source_v2)) + theme_bw() +
    geom_pointrange(aes(y = mean, ymin = lower, ymax = upper, shape = source_v2), size = 0.5, position = position_dodge(width = 0.2)) +
    geom_line(aes(y = mean)) +
    geom_hline(yintercept = 0) +
    labs(title = "Source (version 2)-race level random effect", y = "gamma",
         caption = "u5_{r, d_v2|d_v2 != 'BRFSS_LLCP'}\n(among races present in non-gold standard sources)\nSources are BRFSS LLCP (gold-standard; combined landline/cellphone sample);BRFSS landline only; Gallup")
  print(p + facet_wrap(~source_v2))
}

if(model %in% c("model154")){
  # u8_{a, d_v2|d_v2 != "BRFSS_LLCP"}
  tmp <- mod[param == "re8",]
  p <- ggplot(data = tmp, aes(x = age, color = source_v2)) + theme_bw() +
    geom_pointrange(aes(y = mean, ymin = lower, ymax = upper, shape = source_v2), size = 0.5, position = position_dodge(width = 0.2)) +
    geom_line(aes(y = mean)) +
    geom_hline(yintercept = 0) +
    labs(title = "Age-source (version 2) level random effect", y = "gamma",
         caption = "u8_{a, d_v2|d_v2 != 'BRFSS_LLCP'")
  print(p)
  print(p + facet_wrap(~source_v2))
}

if (model %in% c("model35", "model36")) {
  tmp <- mod[param == "re6", ]
  print(ggplot() + theme_bw() + 
          geom_errorbar(data = tmp, aes(x = marital_lab, ymin = lower, ymax = upper, color = marital_lab)) +
          geom_point(data = tmp, aes(x = marital_lab, y = mean, color = marital_lab)) +
          geom_hline(yintercept = 0) +
          labs(title = "Marital-level random effect", y = "gamma", color = "marital"))  
} else if (model %in% c("model91")) {
  tmp <- mod[param == "re6", ]
  print(ggplot() + theme_bw() + 
          geom_errorbar(data = tmp, aes(x = as.factor(state), ymin = lower, ymax = upper)) +
          geom_point(data = tmp, aes(x = as.factor(state), y = mean)) +
          geom_hline(yintercept = 0) +
          labs(title = "State-level random effect", y = "gamma")) 
} else if (model %in% c("model103", "model106", "model107", "model115", "model120", "model121", "model137", "model138", "model139", "model145", "model146", "model147", "model148", "model149", "model151", "model152", "model154")) {
  # u6_{t}
  tmp <- mod[param %in% c("re6", "B6"), ]
  tmp <- tmp[!is.na(year)]
  p <- ggplot() + theme_bw() + 
          geom_errorbar(data = tmp, aes(x = as.factor(year), ymin = lower, ymax = upper)) +
          geom_point(data = tmp, aes(x = as.factor(year), y = mean)) +
          geom_hline(yintercept = 0) +
          labs(title = "Year-level effect", y = "gamma")
  if(model %in% c("model145", "model146", "model147", "model148", "model149", "model151", "model152", "model154")){
    p <- p + labs(caption = "Year main effect constructed from a natural spline")
  }
  print(p)

  if(model %in% c("model145", "model146", "model147", "model148", "model149", "model151", "model152", "model154")){
    # also plot re6_param
    tmp <- re_param$re6_param
    tmp <- unique(tmp[, .(y_spline, param, est, se)])
    tmp[, `:=`(lower = est - 1.96 * se, upper = est + 1.96 * se)]
    p <- ggplot() + theme_bw() + 
          geom_errorbar(data = tmp, aes(x = as.factor(y_spline), ymin = lower, ymax = upper)) +
          geom_point(data = tmp, aes(x = as.factor(y_spline), y = est)) +
          geom_hline(yintercept = 0) +
          labs(title = "y_spline bases for time", y = "gamma")
    print(p)
  }

}

if (model %in% c("model103", "model106", "model107", "model115", "model120", "model121", "model139", "model146", "model148", "model149", "model151", "model152", "model154")) {
  # u7_{a}
  tmp <- mod[param %in% c("re7", "B7"),]
  tmp <- tmp[!is.na(age)]
  p <- ggplot() + theme_bw() + 
          geom_errorbar(data = tmp, aes(x = as.factor(age), ymin = lower, ymax = upper)) +
          geom_point(data = tmp, aes(x = as.factor(age), y = mean)) +
          geom_hline(yintercept = 0) +
          labs(title = "Age-level effect", y = "gamma")
  if(model %in% c("model146", "model148", "model149", "model151", "model152", "model154")){
    p <- p + labs(caption = "Age main effect constructed from a natural spline")
  }
  print(p)
  if(model %in% c("model146", "model148", "model149", "model151", "model152", "model154")){
    # also plot re7_param
    tmp <- re_param$re7_param
    tmp <- unique(tmp[, .(a_spline, param, est, se)])
    tmp[, `:=`(lower = est - 1.96 * se, upper = est + 1.96 * se)]
    p <- ggplot() + theme_bw() + 
          geom_errorbar(data = tmp, aes(x = as.factor(a_spline), ymin = lower, ymax = upper)) +
          geom_point(data = tmp, aes(x = as.factor(a_spline), y = est)) +
          geom_hline(yintercept = 0) +
          labs(title = "a_spline bases for age", y = "gamma")
    print(p)
  }
}

if (model %in% c("model103", "model106", "model107", "model115", "model120", "model121", "model139")) {
  # u8_{r}
  tmp <- mod[param == "re8",]
  print(ggplot() + theme_bw() + 
          geom_errorbar(data = tmp, aes(x = as.factor(race_lab), ymin = lower, ymax = upper)) +
          geom_point(data = tmp, aes(x = as.factor(race_lab), y = mean)) +
          geom_hline(yintercept = 0) +
          labs(title = "Race-level random effect", y = "gamma")) 
}

if (model %in% c("model107", "model115", "model120")) {
  # u9_{t,r,e,m}
  # year-race-edu-marital-level random intercept
  tmp <- mod[param == "re9",]
  # Make for loop for plots 
  edu_marital <- as.data.table(expand.grid(edu_lab = unique(tmp$edu_lab), marital_lab = unique(tmp$marital_lab)))
  for (z in 1:nrow(edu_marital)) {
    marital_z <- edu_marital[z, marital_lab]
    edu_z <- edu_marital[z, edu_lab]
    tmp2 <- tmp[edu_lab == edu_z & marital_lab == marital_z]
    print(ggplot(tmp2, aes(x = year, color = race_lab, y = mean, ymin = lower, ymax = upper)) + 
            theme_bw() + 
            geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
            geom_line(size = 0.1) +
            labs(title = "Year-race-edu-marital-level random effect",
                 subtitle = paste0(str_to_sentence(edu_z), " & marital = ", marital_z), 
                 x = "year",
                 y = "gamma", 
                 color = "race/ethnicity"))
    
  }
  
  print(ggplot(tmp, aes(x = year, y = mean, ymin = lower, ymax = upper), alpha = 0.5) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_grid(race_lab ~ edu_lab + marital_lab, scales = "free") +
          scale_color_gradientn(colors = brewer.pal(9, "PuRd")[4:9]) +
          labs(title = "Year-race-edu-marital-level random effect", x = "year",
               y = "gamma") +
          theme(axis.text.x = element_text(angle=45, hjust=1, size = 5))) 
  
}

if(model %in% c("model133", "model135", "model137", "model138", "model145", "model147")){
  # u9_{a,r}
  # make same plots, but use age instead of year as x variable
  tmp <- mod[param == "re9",]
  p <- ggplot(tmp, aes(x = age, y = mean, ymin = lower, ymax = upper, color = race_lab)) + theme_bw() +
    geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
    geom_line(size = 0.3) +
    labs(title = "Age/race random effect",
        x = "Age",
        y = "gamma")+
        scale_x_continuous()
  print(p)
  print(p + facet_wrap(~race_lab))
}


if (model %in% c("model120")) {
  # u10_{w,r,e,m}
  # state-race-edu-marital-level random intercept
  tmp <- mod[param == "re10",]
  # Make for loop for plots 
  edu_marital <- as.data.table(expand.grid(edu_lab = unique(tmp$edu_lab), marital_lab = unique(tmp$marital_lab)))
  for (z in 1:nrow(edu_marital)) {
    marital_z <- edu_marital[z, marital_lab]
    edu_z <- edu_marital[z, edu_lab]
    tmp2 <- tmp[edu_lab == edu_z & marital_lab == marital_z]
    print(ggplot(tmp2, aes(x = state, color = race_lab, y = mean, ymin = lower, ymax = upper)) + 
            theme_bw() + 
            geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
            facet_wrap( ~ race) +
            labs(title = "State-race-edu-marital-level random effect",
                 subtitle = paste0(str_to_sentence(edu_z), " & marital = ", marital_z), 
                 x = "state",
                 y = "gamma", 
                 color = "race/ethnicity"))
    
    print(ggplot(tmp2, aes(x = race, color = race_lab, y = mean, ymin = lower, ymax = upper)) + 
            theme_bw() + 
            geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
            facet_wrap( ~ state) +
            labs(title = "State-race-edu-marital-level random effect",
                 subtitle = paste0(str_to_sentence(edu_z), " & marital = ", marital_z), 
                 x = "race",
                 y = "gamma", 
                 color = "race/ethnicity"))
  }
  
  print(ggplot(tmp, aes(x = state, y = mean, ymin = lower, ymax = upper), alpha = 0.5) + theme_bw() + 
          geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
          geom_line(size = 0.1) +
          facet_grid(race_lab ~ edu_lab + marital_lab, scales = "free") +
          scale_color_gradientn(colors = brewer.pal(9, "PuRd")[4:9]) +
          labs(title = "State-race-edu-marital-level random effect", x = "state",
               y = "gamma") +
          theme(axis.text.x = element_text(angle=45, hjust=1, size = 5))) 
  
}

if (model %in% "model128"){
  # u11_{a,r,e,m,c|d == “BRFSS”} 
  tmp <- mod[param == "re11" & source == "BRFSS",]

  print(ggplot(tmp, aes(x = age, y = mean, ymin = lower, ymax = upper, color = as.factor(combined))) +
    theme_bw() +
    geom_pointrange(size = 0.1, position = position_dodge(width = 0.2)) +
    geom_line(linewidth = 0.1)+
    geom_hline(yintercept = 0) +
    facet_grid(race_lab ~ edu_lab + marital_lab) +
    labs(title = "Effect of change in BRFSS sample design on age trends", 
          y = "Gamma", 
          color = "Combined",
          caption = "RE only applies to BRFSS.\ncombined == 0 when year < 2011 & source == BRFSS, otherwise 1\nAlways predict out on combined = 1")       
  )
  # Separate plots by race
  for(r in tmp[, unique(race_lab)]){
    # Plot age trends, faceted by marital & edu, colored by combined.
    print(ggplot(tmp[race_lab == r], aes(x = age, y = mean, ymin = lower, ymax = upper, color = as.factor(combined))) +
      theme_bw() +
      geom_pointrange(size = 0.2, position = position_dodge(width = 0.2)) +
      geom_line(linewidth = 0.2)+
      geom_hline(yintercept = 0) +
      facet_grid(edu_lab ~ marital_lab) +
      labs(title = "Effect of change in BRFSS sample design on age trends", 
           subtitle = r, 
           y = "Gamma", 
           color = "Combined",
           caption = "RE only applies to BRFSS.\ncombined == 0 when year < 2011 & source == BRFSS, otherwise 1\nAlways predict out on combined = 1")       
    )
  }
}

if(model %in% c("model131")){
  # u13_{t,m,w}
  tmp <- mod[param == "re13",]
  # Facet by state, make time x-axis, color by marital
  print(ggplot(tmp, aes(x = year, y = mean, ymin = lower, ymax = upper, color = marital_lab)) +
    theme_bw() +
    geom_pointrange(size = 0.1, position = position_dodge(width = 0.2), alpha = 0.5) +
    geom_line(linewidth = 0.2)+
    geom_hline(yintercept = 0) +
    facet_wrap(~state) +
    labs(title = "Year-marital-state random effect", 
         y = "Gamma", 
         color = "Marital",
         caption = "year-marital-state random intercept (LCAR:IID:IID)")       
  )
  # Facet by marital, make time x-axis, color by state
  print(ggplot(tmp, aes(x = year, y = mean, ymin = lower, ymax = upper, color = as.factor(state))) +
    theme_bw() +
    geom_line(linewidth = 0.3)+
    geom_hline(yintercept = 0) +
    facet_wrap(~marital_lab) +
    labs(title = "Year-marital-state random effect", 
         y = "Gamma", 
         color = "State",
         caption = "year-marital-state random intercept (LCAR:IID:IID)")       
  )
} else if(model %in% c("model135", "model136", "model137")){
  # model 135, 137: u13_{t,r,w}
  # model 136: u13_{a,r,w}
  x_var <- if(model %in% c("model135", "model137")) "year" else if (model == "model136") "age"
  tmp <- mod[param == "re13",]
  setnames(tmp, x_var, "x_var")
  # Facet by state, make time/age x-axis, color by race
  print(ggplot(tmp, aes(x = x_var, y = mean, color = race_lab)) +
    theme_bw() +
    geom_hline(yintercept = 0, linewidth = 0.2) +
    geom_line(linewidth = 0.35)+
    facet_wrap(~state) +
    labs(title = sprintf("%s-race-state random effect", x_var),
         y = "Gamma", 
         x = x_var,
         color = "Race",
         caption = sprintf("%s-race-state random intercept (LCAR:IID:IID)", x_var))       
  )
  # Print the time/age trends, grouped by state. Facet by race_lab
  p <- ggplot(tmp, aes(x = x_var, y = mean, color = as.factor(state))) + 
    theme_bw() + 
    geom_line(linewidth = 0.3)+
    geom_hline(yintercept = 0) +
    facet_wrap(~race_lab) +
    labs(title = sprintf("%s-race-state random effect", x_var), 
         y = "Gamma", 
         x = x_var,
         color = "State",
         caption = sprintf("%s-race-state random intercept (LCAR:IID:IID)", x_var))       
  print(p)
  # add the min/max bars
  print(p + geom_pointrange(aes(ymin = lower, ymax = upper), size = 0.1, position = position_dodge(width = 0.2)))
  # Print boxplots of state effects, with race as x axis, color is year/age.
  print(ggplot(tmp, aes(x = as.factor(race_lab), y = mean, color = as.factor(x_var))) +
    theme_bw() + 
    geom_boxplot() + 
    geom_hline(yintercept = 0) +
    labs(title = sprintf("%s-race-state random effect (boxplot where each point is state-effect)", x_var),
         y = "Gamma",
         color = x_var,
         caption = sprintf("%s-race-state random intercept (LCAR:IID:IID)", x_var)))

  # collapse across states and plot the mean time effect by race
  collapse_tmp <- tmp[, .(mean = mean(mean)), .(x_var, race_lab)]
  print(ggplot(collapse_tmp, aes(x = x_var, y = mean, color = race_lab)) +
    theme_bw() +
    geom_hline(yintercept = 0, linewidth = 0.2) +
    geom_line(linewidth = 0.35)+
    facet_wrap(~race_lab) + 
    labs(title = sprintf("%s-race-state random effect, collapsed across state", x_var), 
         y = "Gamma", 
         x = x_var,
         color = "Race",
         caption = sprintf("%s-race-state random intercept (LCAR:IID:IID) (simple average to collapse across states)", x_var))
  )
} else if (model %in% c("model139")){
  # u13_{t, w} year-state effect
  tmp <- mod[param == "re13",]
  # Facet by state, make time x-axis
  p <- ggplot(tmp, aes(x = year, y = mean, ymin = lower, ymax = upper, color = as.factor(state))) +
    theme_bw() +
    geom_pointrange(size = 0.1, position = position_dodge(width = 0.2), alpha = 0.5) +
    geom_line(linewidth = 0.2)+
    geom_hline(yintercept = 0) +
    labs(title = "Year-state random effect", 
         y = "Gamma", 
         color = "State",
         caption = "year-state random intercept (LCAR:IID)")       
  print(p)
  print(p + facet_wrap(~state))

}

if(model %in% c("model131")){
  # u14_{t,e,w}
  tmp <- mod[param == "re14",]
  # Facet by state, make time x-axis, color by edu
  print(ggplot(tmp, aes(x = year, y = mean, ymin = lower, ymax = upper, color = edu_lab)) +
    theme_bw() +
    geom_pointrange(size = 0.1, position = position_dodge(width = 0.2), alpha = 0.5) +
    geom_line(linewidth = 0.2)+
    geom_hline(yintercept = 0) +
    facet_wrap(~state) +
    labs(title = "Year-edu-state random effect", 
         y = "Gamma", 
         color = "Edu",
         caption = "year-edu-state random intercept (LCAR:LCAR:IID)")       
  )
  # Facet by edu, make time x-axis, color by state
  print(ggplot(tmp, aes(x = year, y = mean, ymin = lower, ymax = upper, color = as.factor(state))) +
    theme_bw() +
    geom_line(linewidth = 0.3)+
    geom_hline(yintercept = 0) +
    facet_wrap(~edu_lab) +
    labs(title = "Year-edu-state random effect", 
         y = "Gamma", 
         color = "State",
         caption = "year-edu-state random intercept (LCAR:LCAR:IID)")       
  )
}

dev.off()
