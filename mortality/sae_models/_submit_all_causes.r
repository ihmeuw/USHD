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

suppressWarnings(suppressMessages({
  library(R.utils)
  library(data.table)
  library(TMB)
  library(tidyr)
  sourceDirectory("functions/", modifiedOnly = F)
}))

## Get and check settings --------------------------------------------------------------------------
# passed arguments
parser <- argparse::ArgumentParser()
add_dir_argument(parser)
parser$add_argument("type", choices = c("prep_only", "models_only", "post_models", "post_raking", "all", "agg_raked_yll"), help = "Determine type of run")
parser$add_argument("resub", choices = c("TRUE", "FALSE"), help = "Use this flag if this is a resubmission to skip completed jobs")
parser$add_argument("testing", choices = c("TRUE", "FALSE"), help = "Set as testing job. Log all output")
parser$add_argument("all_cause_dependency", choices = c("TRUE", "FALSE"), help = "True if you want to run all-cause, and False if not")
parser$add_argument("--project", default ="QUEUE")

add_aggregation_skip_flags(parser)

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
if (!resub & !type %in% c("post_models", "post_raking", "agg_raked_yll")) { 

# load the full deaths dataset
  deaths <- readRDS(deaths_file)
  deaths <- deaths[year %in% years]

  stopifnot(nrow(deaths) > 0)

# load all the cause hierarchy for all fatal causes
  causes <- get_cause_metadata(cause_set_id = 3, gbd_round_id = rake_to_gbd_version[["gbd_round_id"]]) # reporting causes
  causes_internal <- get_cause_metadata(cause_set_id = 4, gbd_round_id = rake_to_gbd_version[["gbd_round_id"]]) # internal causes
  causes <- causes[is.na(yld_only), ]
  causes <- causes[!(is.na(yll_age_end) & is.na(yll_age_end))] # get rid of causes with no age information

# code sexes to model
  causes[male == 1 & female == 1, sexes := "c(1,2)"] # code sexes to model
  causes[male == 1 & female == 0, sexes := "c(1)"]
  causes[male == 0 & female == 1, sexes := "c(2)"]

# code ages to model
  causes[yll_age_start < 1, yll_age_start := 0] # collapse infant age groups

  causes[, path_to_top_parent_copy := path_to_top_parent]
  causes <- separate(causes, path_to_top_parent_copy, paste0("level_", 0:5), sep = ",", convert = T)
  level_cols <- names(causes)[names(causes) %like% "level_"]
  causes[,(level_cols) := NULL]
  
  ## Match the age restrictions with the USHD ages
  age_dt <- data.table(age_start = ages)
  age_dt[,age_end := ifelse(age_start == 0, 1, ifelse(age_start == 1, 4, ifelse(age_start == max(ages), 110, age_start + 4)))]
  
  for(cc in 1:nrow(causes)) {
    causes[cc, age_start := age_dt[causes[cc, yll_age_start] >= age_start & causes[cc, yll_age_start] < age_end, age_start]]
    causes[cc, age_end := age_dt[causes[cc, yll_age_end] >= age_start & causes[cc, yll_age_end] < age_end, age_start]]
    
  }
  
  stopifnot(nrow(causes[is.na(age_start)]) == 0)
  stopifnot(nrow(causes[is.na(age_end)]) == 0)

  causes[, ages := paste0("c(", paste(ages[between(ages, age_start, age_end)], collapse = ","), ")"), by = "cause_id"] # code ages to model
  
  # get rid of unnecessary columns
  causes[,c("age_start", "age_end") := NULL]

  causes[, run_model := 1]

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
  if(!all_cause_dependency) causes[cause_id == 294, run_model := 0]

  causes <- causes[, list(level, cause_id, cause_outline, acause, parent_id, path_to_top_parent, sexes, ages, run_model)]
  setkey(causes, "cause_id")

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
    
    if(this_cause != 294) {
      levels(new_settings$V2) <- c(levels(new_settings$V2), FALSE, "") 
      new_settings$V2[new_settings$V1 == "lt_hc"] <- FALSE
      new_settings$V2[new_settings$V1 == "pop_growth_file"] <- ""
    }

    # alter the age spline knots based on the age restrictions for the cause
    for(age_spline_type in c("age_knots_spec")) {
      if(age_spline_type %in% new_settings$V1) {
        current_age_spline <- eval(parse(text=as.character(new_settings$V2[new_settings$V1 == age_spline_type])))
        current_ages <- eval(parse(text=as.character(new_settings$V2[new_settings$V1 == "ages"])))

        if(is.na(eval(parse(text=current_ages)))) {
          message("Ages are all NA!!")
          if(causes[cause_id == this_cause, run_model] == 1) stop("A cause you are trying to model has invalid ages")
          next
        }

        current_age_spline[1] <- min(current_ages)
        current_age_spline[length(current_age_spline)] <- max(current_ages)
        # deal with any cases where the age group starts at an age larger than the next value in the age spline
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

        current_age_spline <- sort(unique(current_age_spline))

        if(all(current_age_spline == c(20,25,45,65,85))) current_age_spline <- c(20,35,45,65,85)
        if(all(current_age_spline == c(40,45,65,85))) current_age_spline <- c(40,55,65,85)

        # save the new age spline in the settings
        new_settings[V1 == age_spline_type, V2 := paste0("c(",paste0(current_age_spline,collapse=","),")")]

        # change the age and time knots for maternal level 3 (cause_id = 366)
        if(this_cause == 366) {
          new_settings[V1 == age_spline_type, V2 := "c(10, 50)"]
          new_settings[V1 == "year_knots_num", V2 := "2"]
          new_settings[V1 == "year_knots_num_re3", V2 := "2"]
        }
      }
    }


    if (fixed_age_time) {
      new_settings$V2[new_settings$V1 == "ref_dir"] <- gsub("FILEPATH", "", paste0(ref_dir, "/", cause_name, "/"))
    }
    write.table(new_settings, file = paste0(new_dir, "settings.csv"), row.names = F, col.names = F, sep = ",", qmethod = "double")

    rm(new_dir, new_settings); gc()
  }
  
  ## create the YAML file
  write_consolidated_settings(dir, copy_J = TRUE)

} else {
  causes <- fread(paste0(dir, "/submitted_cause_list.csv"))
  setkey(causes, "cause_id")

}

jids = data.table()
## _submit_single_cause.r for each cause ------------------------------------------------------
# loop through causes and submit
if (type != "prep_only") { 
  
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
           if (skip_mx_aggregation) CLI.Flags$skip_mx_aggregation
         ),
         hold = if (cause_name != "_all" & all_cause_dependency) sub_all_hold, 
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
