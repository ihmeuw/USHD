####################################################################################################
## Description: Prepare folder structure for cause-specific models and then submit
##              "_submit_single_cause.r" for each cause of death. The settings specified in
##              "[dir]/settings.csv" are updated as necessary for each specific cause -- this
##              includes the age and sex arguments (based on cause-specific restrictions), and,
##              optionally, the covars and model arguments.
##
## Passed args: dir [character] -- home directory for settings and final output
##              type [character] -- what type of run is this? options:
##                "prep_only" -- just set up the cause list and folder structure and generate
##                    deaths and settings files
##                "models_only" -- just fit models and generate initial area-sex-year-level ASMRs
##                "post_models" -- just run aggregation, collapse, and compile steps required after
##                    area-sex-year-level ASMRs are available (i.e., after running with type =
##                    "models_only")
##                "post_raking" -- the same as "post_models", but using raked area-sex-year-level
##                    ASMRs instead of directly estimated ASMRs.
##                "all" -- run all steps, assuming raking == F where relevant
##                "agg_raked_yll" -- After raking YLLs specifically, run just the aggregation
##                    steps in post-processing on these raked YLLs
##              resub [logical] -- should jobs where the relevant output file already exists be skipped?
##              testing [logical] -- is this a testing job? if T, runs on a reduced set of causes
##                and logs all output.
##
## Requires:    a settings file ("[dir]/settings.csv")
##              prepped death records with cause of death mapped to GBD causes at the appropriate
##                level of the cause hierarchy (deaths_file)
##
## Optional:    a file specifying cause-specific additions or deletions to the covar argument
##                ("[dir]/cause_covariates.csv").
##              a file specifying alternate models for specific causes
##                ("[dir]/cause_models.csv")
##              a file specifying causes to be excluded from the analysis
##                ("[dir]/cause_exclusions.csv")
##
## Outputs:     a separate dataset of death records for each cause of death
##              a folder structure for cause of death models with one folder per cause of death
##                containing a "settings" CSV pointing to the appropriate inputs
##              a submitted job for each cause of death which will run "_submit_single_cause.r" in
##                order to submit and run all required sub-components for each cause of death
##
####################################################################################################

stopifnot(grepl("mortality/sae_models$", getwd()))

suppressWarnings(suppressMessages({
  library(R.utils)
  library(data.table)
  library(TMB)
  library(tidyr)
  sourceDirectory("functions/", modifiedOnly = F)
}))

## Get and check settings --------------------------------------------------------------------------
parser <- argparse::ArgumentParser()
add_dir_argument(parser)
parser$add_argument("type", choices = c("prep_only", "models_only", "post_models", "post_raking", "all", "agg_raked_yll"), help = "Determine type of run")
parser$add_argument("resub", choices = c("TRUE", "FALSE"), help = "Use this flag if this is a resubmission to skip completed jobs")
parser$add_argument("testing", choices = c("TRUE", "FALSE"), help = "Set as testing job. Log all output")
parser$add_argument("all_cause_dependency", choices = c("TRUE", "FALSE"), help = "True if you want to run all-cause, and False if not")
parser$add_argument("--project", default = "PROJ")
parser$add_argument("--queue", default = "QUEUE")
add_aggregation_skip_flags(parser)
parser$add_argument("write_estimates_with_issues", choices = c("TRUE", "FALSE"), help = "Write estimates files even if they have issues.")

args <- parser$parse_args(get_args())

dir <- args$dir
type <- args$type
resub <- as.logical(args$resub)
testing <- as.logical(args$testing)
all_cause_dependency <- as.logical(args$all_cause_dependency)

project <- args$project 
queue <- args$queue
skip_mx_aggregation <- args$skip_mx_aggregation
skip_yll_aggregation <- args$skip_yll_aggregation
skip_yld_aggregation <- args$skip_yld_aggregation
skip_pred_aggregation <- args$skip_pred_aggregation
write_estimates_with_issues <- as.logical(args$write_estimates_with_issues)

if (is.na(testing)) testing <- F

print("Settings passed are:")
for (setting in names(args)){
  print(paste0(setting, ": ", get(setting)))
}

# settings file
check_settings(dir)
get_settings(dir)
settings <- read.csv(paste0(dir, "/settings.csv"), stringsAsFactors = F, header = F)

## Prep deaths data, the cause list, and model settings --------------------------------------------
if (!resub & !type %in% c("post_models", "post_raking", "agg_raked_yll")) { # this section is skipped for resub or post_models/post_raking because it will have already been run

  # Read in population version and save if we have specified the database
  if (!is.null(pop_version)) {
    message(glue::glue("Using pop_version {pop_version} from database"))
    # save original settings
    write.table(
      settings,
      file = file.path(dir, "settings_OG.csv"),
      row.names = F,
      col.names = F,
      sep = ",",
      qmethod = "double"
    )

    pop <- get_population_data(covariate_dataset_id = pop_version)

    if (lt_hc) {
      if (by_race) {
        if (min(pop$year) > (min(years) - 10)) {
          stop("H-C method specified, but population version does not go back 10 previous years")
        }
      }
    }

    # Format
    pop <- pop[, .(race, mcnty, year, sex, age, state, edu, pop)]
    saveRDS(pop, file.path(
      dir,
      paste0("pop_file_cov_dataset_id_", pop_version, ".rds")
    ))

    # make a new row called pop_file in the settings and remove any old row
    if ("pop_file" %in% settings$V1) {
      message("Removing pop_file from settings.csv in order to replace it")
      settings <- settings[settings$V1 != "pop_file", ]
    }

    if ("pop_growth_file" %in% settings$V1) {
      message("Removing pop_growth_file from settings.csv in order to replace it")
      settings <- settings[settings$V1 != "pop_growth_file", ]
    }

    # Now add on new rows to the csv
    settings <- rbind(settings,
                      data.frame(
                        V1 = c("pop_file", "pop_growth_file"),
                        V2 = c(file.path(
                          dir,
                          paste0("pop_file_cov_dataset_id_", pop_version, ".rds")
                        ),
                        file.path(
                          dir,
                          paste0("pop_file_cov_dataset_id_", pop_version, ".rds")
                        ))
                      ))

    # remove the pop_version row: if we re-run error will raise if both are specified!
    if ("pop_version" %in% settings$V1) {
      message("Removing pop_version from settings.csv so that settings won't have both pop_version and pop_file. pop_version will still be in settings_OG.csv.")
      settings <- settings[settings$V1 != "pop_version", ]
    }

    # save updated settings
    write.table(
      settings,
      file = file.path(dir, "settings.csv"),
      row.names = F,
      col.names = F,
      sep = ",",
      qmethod = "double"
    )

    # load the settings again to make sure it's usable
    settings <-
      read.csv(paste0(dir, "/settings.csv"),
               stringsAsFactors = F,
               header = F)

  }

  # load the full deaths dataset
  deaths <- readRDS(deaths_file)
  deaths <- deaths[year %in% years]

  stopifnot(nrow(deaths) > 0)

# load all the cause hierarchy for all fatal causes
  causes <- get_cause_metadata(cause_set_id = 3, gbd_round_id = 7) # reporting causes
  causes_internal <- get_cause_metadata(cause_set_id = 4, gbd_round_id = 7) # internal causes

  causes <- causes[is.na(yld_only), ]
  causes <- causes[!(is.na(yll_age_end) & is.na(yll_age_end))] # get rid of causes with no age information

  # code sexes to model
  causes[male == 1 & female == 1, sexes := "c(1,2)"] # code sexes to model
  causes[male == 1 & female == 0, sexes := "c(1)"]
  causes[male == 0 & female == 1, sexes := "c(2)"]

  # code ages to model
  causes[yll_age_start < 1, yll_age_start := 0] # collapse infant age groups

  causes[, path_to_top_parent_copy := path_to_top_parent]
  causes <- separate(causes, path_to_top_parent_copy, paste0("level_", 0:5), sep = ",", convert = T, fill = "right")
  causes[cause_id == 558, c("yll_age_start","yll_age_end") := list(5,45)] # mental disorders (_mental)
  # Make some specific adjustments for drug and alcohol use disorders
  causes[cause_id %in% c(563, 564, 566), yll_age_start := 15]

  level_cols <- names(causes)[names(causes) %like% "level_"]
  causes[,(level_cols) := NULL]

  ## Match the age restrictions with the USHD ages
  if (by_edu) {
    age_dt <- data.table(age_start = c(0, 1, seq(5, 85, 5)))
  } else {
    age_dt <- data.table(age_start = ages)
  }
  
  age_dt[, age_end := ifelse(
    test = age_start == 0,
    yes = 1,
    no = ifelse(
      test = age_start == 1,
      yes = 4,
      no = ifelse(
        test = age_start == max(ages),
        yes = 110,
        no = age_start + 4
      )
    )
  )]

  stopifnot(nrow(causes) == length(unique(causes$acause)))
  
  for (ac in unique(causes$acause)) {
    x = causes[acause == ac, yll_age_start]
    y = causes[acause == ac, yll_age_end]
    a = age_dt[x >= age_start & x < age_end, age_start]
    b = age_dt[y >= age_start & y < age_end, age_start]
    causes[acause == ac, age_start := a]
    causes[acause == ac, age_end := b]
  }

  stopifnot(nrow(causes[is.na(age_start)]) == 0)
  stopifnot(nrow(causes[is.na(age_end)]) == 0)

  causes[, ages := paste0("c(", paste(ages[between(ages, age_start, age_end)], collapse = ","), ")"), by = "cause_id"] # code ages to model

  # get rid of unnecessary columns
  causes[,c("age_start", "age_end") := NULL]

  # skip models for causes where the children don't add up to the parent and child deaths are not used directly for estimation in GBD
  causes[, run_model := 1]

  causes[parent_id %in% causes[acause %in% c("tb", "cirrhosis", "neo_liver", "neo_other_benign", "ckd"),
                               cause_id], run_model := 0]

  # skip models for causes with less than "min_deaths" deaths
  total <- deaths[, list(deaths = sum(deaths)), by = c("cause_id")]
  total_bysex <- deaths[, list(deaths = sum(deaths)), by = c("cause_id","sex")]
  min_deaths <- 10000 
  min_death_sex <- 1000 # sex-specific cutoff for deaths - require that both sexes have a min death count > 1000
  causes[cause_id %in% total[deaths < min_deaths, cause_id] | cause_id %in% unique(total_bysex[deaths < min_death_sex]$cause_id),
         run_model := 0]

  # skip models for causes that don't appear in deaths at all
  causes[(!cause_id %in% unique(deaths[, cause_id])), run_model := 0]

  # skip all-cause if specified
  if (!all_cause_dependency) causes[cause_id == 294, run_model := 0]

  causes <- causes[, list(level, cause_id, cause_outline, acause, parent_id, path_to_top_parent, sexes, ages, run_model)]
  setkey(causes, "cause_id")


  # get cause-specific covariate information [optional]
  if ("cause_covariates.csv" %in% dir(dir)) {
    stop("This has not yet been transitioned over to use the database. Code is out of date!!")

    cause_covars <- fread(paste0(dir, "/cause_covariates.csv"))
    setkey(cause_covars, "acause")

    covar_fun <- function(this_cause) {
      # remove dropped covariates from the base set
      temp_covars <- setdiff(covars, cause_covars[this_cause, ][add == 0, covar])
      temp_covars_as <- setdiff(covars_as, cause_covars[this_cause, ][add == 0, covar])
      temp_trans <- covars_trans[names(covars_trans) %in% c(temp_covars, temp_covars_as)]

      # add in additional covariates
      temp_covars <- na.omit(c(temp_covars, cause_covars[this_cause, ][add == 1 & as == 0, covar]))
      temp_covars_as <- na.omit(c(temp_covars_as, cause_covars[this_cause, ][add == 1 & as == 1, covar]))
      temp_trans <- c(temp_trans, cause_covars[this_cause, ][add == 1 & !is.na(covar_trans), tapply(covar_trans, covar, function(x) x)])

      # format as an expression and return
      if (length(temp_covars) >= 1) temp_covars <- paste0("c(", paste(paste0("\"", temp_covars, "\""), collapse = ","), ")") else temp_covars <- "NULL"
      if (length(temp_covars_as) >= 1) temp_covars_as <- paste0("c(", paste(paste0("\"", temp_covars_as, "\""), collapse = ","), ")") else temp_covars_as <- "NULL"
      if (length(temp_trans) >= 1) temp_trans <- paste0("c(", paste(paste0(names(temp_trans), "=\"", temp_trans, "\""), collapse = ", "), ")") else temp_trans <- "NULL"
      return(list(temp_covars, temp_covars_as, temp_trans))
    }
    causes[, c("covars", "covars_as", "covars_trans") := covar_fun(acause), cause_id]
    rm(cause_covars, covar_fun)

  } else {
    if ("covars" %in% settings$V1) causes[, covars := settings[settings$V1 == "covars", "V2"]] else causes[, covars := "NULL"]
    if ("covars_as" %in% settings$V1) causes[, covars_as := settings[settings$V1 == "covars_as", "V2"]] else causes[, covars_as := "NULL"]
    if ("covars_trans" %in% settings$V1) causes[, covars_trans := settings[settings$V1 == "covars_trans", "V2"]] else causes[, covars_trans := "NULL"]
    if ("covars_subpop" %in% settings$V1) causes[, covars_subpop := settings[settings$V1 == "covars_subpop", "V2"]] else causes[, covars_subpop := "NULL"]

  }

  # get cause-specific model information [optional]
  if ("cause_models.csv" %in% dir(dir)) {
    cause_models <- fread(paste0(dir, "/cause_models.csv"))
    causes <- merge(causes, cause_models, by = "acause", all.x = T)
    causes[is.na(model), model := get("model", .GlobalEnv)]
    rm(cause_models)

  } else {
    causes[, model := get("model", .GlobalEnv)]

  }
  
  ## Change some of the covariates specified when certain models are run
  for (this_cause in causes[order(level, cause_id), cause_id]) {
    
    if(causes[cause_id == this_cause, model] %in% 
       c("spline_iid_race_one_indic_flexible_re_covs",
         "spline_iid_race_two_indics_flexible_re_covs",
         "spline_iid_year_two_indicators",
         "spline_iid_year_age_indicator")) {
      
      if(this_cause == 696) {

        causes[cause_id == this_cause, 
               covars :=  paste0("c(", paste(paste0("\"", c(get("covars",.GlobalEnv),"indic"), "\""), collapse = ","), ")")]
        
      } else if (this_cause == 717) {
        
        causes[cause_id == this_cause, 
               covars :=  paste0("c(", paste(paste0("\"", c(get("covars",.GlobalEnv),"indic"), "\""), collapse = ","), ")")]
               
      } else if (this_cause %in% c(294, 687)) {
        
        causes[cause_id == this_cause, 
               covars :=  paste0("c(", paste(paste0("\"", c(get("covars",.GlobalEnv),"indic_1","indic_2"), "\""), collapse = ","), ")")]
        
      }
      
      
    }
  }


  # save list of causes (and associated arguments) to be submitted
  setkey(causes, "cause_id")
  write.csv(causes, file = paste0(dir, "/submitted_cause_list.csv"), row.names = F)


  # save settings files for each cause of death
  for (this_cause in causes[order(level, cause_id), cause_id]) {

    cause_level <- causes[J(this_cause), level]
    cause_name <- causes[J(this_cause), acause]

    message(cause_name)

    # create directories for this cause of death
    new_dir <- paste0(dir, "/", cause_name, "/")
    dir.create(new_dir, recursive = T)

    # create a settings CSV for that cause
    new_settings <- as.data.table(copy(settings))

    new_settings$V2[new_settings$V1 == "sexes"] <- causes[J(this_cause), sexes]
    new_settings$V2[new_settings$V1 == "ages"] <- causes[J(this_cause), ages]
    new_settings$V2[new_settings$V1 == "model"] <- causes[J(this_cause), model]
    new_settings <- rbind(new_settings, data.table(V1 = "cause_id", V2 = causes[J(this_cause), cause_id]))
    new_settings <- new_settings[!new_settings$V1 %in% c("covars", "covars_as", "covars_trans"), ]
    new_settings <- rbind(new_settings,
                          data.frame(V1 = c("covars", "covars_as", "covars_trans"),
                                     V2 = unlist(causes[J(this_cause), list(covars, covars_as, covars_trans)])))

    # If not dealing with all-cause (_all), do not include the pop_growth_file
    # this is because we only need this file for calculating life expectancy, so it is not necessary for the cause-specific models
    if (this_cause != 294) {
      levels(new_settings$V2) <- c(levels(new_settings$V2), FALSE, "") # add FALSE and "" (empty string) as possible levels
      new_settings$V2[new_settings$V1 == "lt_hc"] <- FALSE
      new_settings$V2[new_settings$V1 == "pop_growth_file"] <- ""
    }

    # for particular indicator models and causes, add a new line to the settings.csv
    if(new_settings$V2[new_settings$V1 == "model"] %in% 
       c("spline_iid_race_one_indic_flexible_re_covs",
         "spline_iid_race_two_indics_flexible_re_covs",
         "spline_iid_year_two_indicators",
         "spline_iid_year_age_indicator")) {
      
      if(this_cause == 696) {
        # _unintent
        new_settings <- rbind(new_settings, data.table(V1 = "indicator_file", V2 = "FILEPATH"))
        # add a line for the indicator_year
        new_settings <- rbind(new_settings, data.table(V1 = "indicator_year", V2 = 2005))
        # add on another prior because there is an extra random effect on county
        new_settings$V2[new_settings$V1 == "prior_list"] <- "list(c(5,0.05,-5), c(5,0.05,-5),c(5,0.05,-5),c(5,0.05,-5),c(5,0.05,-5))"
          
      } else if (this_cause == 717) {
        
        new_settings <- rbind(new_settings, data.table(V1 = "indicator_file", V2 = "FILEPATH"))
        
        # add a line for the indicator_year
        new_settings <- rbind(new_settings, data.table(V1 = "indicator_year", V2 = 2001))
        # add on another prior because there is an extra random effect on county
        new_settings$V2[new_settings$V1 == "prior_list"] <- "list(c(5,0.05,-5), c(5,0.05,-5),c(5,0.05,-5),c(5,0.05,-5),c(5,0.05,-5))"
        
      } else if (this_cause %in% c(294, 687)) {
        
        ## Apply this indicator to all-cause, _intent, and _inj for experimentation purposes
        new_settings <- rbind(new_settings, data.table(V1 = "indicator_file_1", V2 = "FILEPATH"))
        
        new_settings <- rbind(new_settings, data.table(V1 = "indicator_file_2", V2 = "FILEPATH"))
        
        # add a line for the indicator_year
        new_settings <- rbind(new_settings, data.table(V1 = "indicator_year_1", V2 = 2001))
        new_settings <- rbind(new_settings, data.table(V1 = "indicator_year_2", V2 = 2005))
        # add on two more priors because there is an extra random effect on county
        new_settings$V2[new_settings$V1 == "prior_list"] <- "list(c(5,0.05,-5), c(5,0.05,-5),c(5,0.05,-5),c(5,0.05,-5),c(5,0.05,-5),c(5,0.05,-5),c(5,0.05,-5))"
        
      }

    }


    # alter the age spline knots based on the age restrictions for the cause
    for (age_spline_type in c("age_knots_spec")) {
      if (age_spline_type %in% new_settings$V1) {
        current_age_spline <- eval(parse(text=as.character(new_settings$V2[new_settings$V1 == age_spline_type])))
        current_ages <- eval(parse(text=as.character(new_settings$V2[new_settings$V1 == "ages"])))

        if (is.na(eval(parse(text = current_ages)))) {
          message("Ages are all NA!!")
          if (causes[cause_id == this_cause, run_model] == 1) stop("A cause you are trying to model has invalid ages")
          next
        }

        current_age_spline[1] <- min(current_ages)
        current_age_spline[length(current_age_spline)] <- max(current_ages)
        # deal with any cases where the age group starts at an age larger than the next value in the age spline
        # we assume that there are at least 2 age knots specified in the settings.csv
        for(i in 2:(length(current_age_spline)-1)) {
          if(current_age_spline[1] < current_age_spline[i]) break

          if(current_age_spline[1] > current_age_spline[i]) {
            closest_age <- current_ages[which(abs(current_ages-current_age_spline[i])==min(abs(current_ages-current_age_spline[i])))]
            current_age_spline[i] <- closest_age
          }

        }

        current_age_spline <- unique(current_age_spline)

        # Now deal with if the max age spline is greater than the max age
        for(i in rev(2:(length(current_age_spline)-1))) {
          if(max(current_ages) > current_age_spline[i]) break # if max ages is larger than the entry, break out of if()

          # if not, then take the average of the elements
          if(max(current_ages) < current_age_spline[i]) {
            # find the age in the ages that is >= min age and < that this age knot in the spline
            closest_age <- current_ages[which(abs(current_ages-current_age_spline[i])==min(abs(current_ages-current_age_spline[i])))]
            current_age_spline[i] <- closest_age
          }


        }

        # get unique entries because it is possible that we have duplicates now
        current_age_spline <- sort(unique(current_age_spline))

        # if a cause starts at age 20 or age 40, this method will cause there to be adjacent age knots, which isn't
        # beautiful, so let's shift em a bit
        if(all(current_age_spline == c(20,25,45,65,85))) current_age_spline <- c(20,35,45,65,85)
        if(all(current_age_spline == c(40,45,65,85))) current_age_spline <- c(40,55,65,85)

        message(paste(current_ages, collapse=", "))
        message(paste(current_age_spline, collapse=", "))

        # save the new age spline in the settings
        new_settings[V1 == age_spline_type, V2 := paste0("c(",paste0(current_age_spline,collapse=","),")")]

        # change the age and time knots for maternal level 3 (cause_id = 366)
        if(this_cause == 366) {
          new_settings[V1 == age_spline_type, V2 := "c(10, 50)"]
          # these will currently be repeated for each time in the loop but that's fine...
          new_settings[V1 == "year_knots_num", V2 := "2"]
          new_settings[V1 == "year_knots_num_re3", V2 := "2"]
        }
      }
    }

  
    write.table(new_settings, file = paste0(new_dir, "settings.csv"), row.names = F, col.names = F, sep = ",", qmethod = "double")

    rm(new_dir, new_settings); gc()
  }

  ## create the YAML file
  write_consolidated_settings(dir, copy_J = TRUE)

  message("Done with prep.")

} else {
  causes <- fread(paste0(dir, "/submitted_cause_list.csv"))
  setkey(causes, "cause_id")
}

jids = data.table()
# loop through causes and submit
if (type != "prep_only") { # models_only, post_models, post_raking, all

  sub_all_hold <- 1

  for (this_cause in causes[order(level, cause_id), cause_id]) {
    subset = data.table()
    if (causes[J(this_cause), run_model] == 0 & type %in% c("models_only", "all")) next 

    cause_name <- causes[J(this_cause), acause]

    if (causes[J(this_cause), run_model == 0] & type != "post_raking") {
      next
    }

    job_name <- sprintf("sub_%s", cause_name)
    subset[, jid:=sbatch(code = "_submit_single_cause.r",
                         name = job_name,
                         arguments = c(
                           paste0(dir, "/", cause_name, "/"), cause_name == "_all", type, resub, TRUE, all_cause_dependency,
                           "--queue", queue, "--project", project,
                           if (skip_yll_aggregation) CLI.Flags$skip_yll_aggregation,
                           if (skip_mx_aggregation) CLI.Flags$skip_mx_aggregation,
                           CLI.Flags$skip_yld_aggregation,
                           CLI.Flags$skip_pred_aggregation,
                           write_estimates_with_issues
                         ),
                         hold = if (cause_name != "_all" & all_cause_dependency) sub_all_hold, # need to wait for the "_all" pred_lt job to submit (so we can hold yll jobs)
                         fthread = 1, m_mem_free = "64G", h_rt = "01:00:00", archive = T,
                         project = project, queue = queue,
                         sgeoutput = paste0(dir, "/", cause_name, "/"))]

    if(cause_name == "_all") sub_all_hold <- unique(subset$jid)

    subset$cause_name = cause_name
    Sys.sleep(5) # pause 
    jids = rbind(jids, subset, fill=TRUE)
    message(sprintf("Submitted job '%s' (%s)", job_name, subset$jid))
  }
}

saveRDS(jids, paste0(dir, "/submit_all_causes_jids_get_ylls_", type, ".rds"))
