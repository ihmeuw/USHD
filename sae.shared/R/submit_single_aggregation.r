#' @title submit_single_aggregation
#' @description A function version of the _submit_single_cause.r script used for
#'              submitting jobs to generate life tables and ylls, and aggregate
#'              based on the type passed in.
#'
#' @param jobmon_workflow a jobmon workflow object created with jobmonr::workflow. The
#' jobmon tasks are bound to this object which is used to construct a dag and launch the jobs.
#' @param jobmon_tool a jobmon tool object created with jobmonr::tool.
#' @param dir character -- home directory for settings and final output
#' @param type character -- what type of run is this? options:
#'                "post_raking" -- Aggregate raked area-year-sex-race ASMRs instead of directly estimated ASMRs.
#'                "post_raking_lt" -- Same as post_raking, but add pred_lt. Run Life Tables (for _all)
#'                   on raked area-year-sex-race ASMRs, then aggregate the Life Tables and the raked ASMRs
#'                "post_raking_lt_yll" -- Same as post_raking_lt, but add pred_yll. Run Life Tables, then
#'                   run pred_yll, then aggregate the Life Tables, the YLLS, and the raked ASMRs.
#'                "agg_raked_yll" -- After raking YLLs specifically, run just the aggregation
#'                    steps in post-processing on these raked YLLs
#' @param testing logical -- is this a testing job? if T, logs all output.
#' @param project character -- which project to launch qsubs on.
#' @param queue character -- which queue to launch qsubs on. Default "..."
#' @param skip_mx_aggregation logical -- skip mx aggregation? Default F
#' @param skip_yll_aggregation logical -- skip yll aggregation? Default F
#' @param skip_yld_aggregation logical -- skip mx aggregation? Default F
#' @param skip_pred_aggregation logical -- skip yll aggregation? Default F
#' @param lt_hold list of jobmon tasks -- pred_ylls for child causes need to hold on
#' all cause lt jobs to finish. lt_hold is used to pass these through. When not needed
#' defaults to NULL.
#'
#' @details Due to the fact that some tasks hold on multiple others, and the tasks cannot
#' be duplicated (because only one task can be bound to the jobmon workflow), it is
#' necessary to split out the jobmon tasks from the jids tables (which contains the
#' permutations of all demographics that can get jobs). E.g. pred_lt is duplicated over
#' area (mcnty, state, natl), so 3 identical jobmon tasks are produced as a result.
#' In order to deal with this, the jobmon tasks are placed in a separate deduplicated
#' table for each type of job (pred_lt/pred_yll etc.) and referenced through an id that is
#' saved in the jids object.
#'
#' @return jids, a data.table containing the job_ids of all the jobs launched
submit_single_aggregation <- function(jobmon_workflow,
                                      jobmon_tool,
                                      dir,
                                      type,
                                      testing,
                                      project = "PROJECT",
                                      queue = "...",
                                      skip_mx_aggregation = FALSE,
                                      skip_yll_aggregation = FALSE,
                                      skip_yld_aggregation = FALSE,
                                      skip_pred_aggregation = FALSE,
                                      lt_hold = NULL,
                                      verbose = TRUE) {
  
  tock <- function(msg, start_time) {
    end_time <- Sys.time()
    time_taken <- round(end_time - start_time)
    print(paste(msg, time_taken))
    return(Sys.time())
  }
  if (verbose) start_time <- tock("start me up", Sys.time())
  
  raked <- TRUE
  validate <- FALSE
  lt <- FALSE
  
  # load settings in to separate environment to avoid global pollution
  # each cause may have slightly different settings
  # we want to load the settings.yaml file from my_model/ and then get _inj-specific settings
  settings_env <- ModelSettings$from_dir(dirname(dir))$settings_for_cause(basename(dir))
  
  if(is.null(settings_env$race_together)) race_together <- F
  
  # create a table for holding job ids
  # sex == 3 is both sexes, race_default is all race, edu_default is all edus
  jids <- CJ(level = c(settings_env$area_var, names(settings_env$settings$geoagg_files)),
             year = settings_env$years,
             sex = c(settings_env$sexes, 3),
             race = unique(c(settings_env$races, race_default)),
             edu = unique(c(settings_env$edu_groups, edu_default)))
  
  if (!is.null(settings_env$geoagg_files)) { # we may not be able to aggregate in all years, so year-level combinations with no crosswalk need to be removed.
    for (this_level in names(settings_env$geoagg_files)) {
      weights <- readRDS(settings_env$geoagg_files[[this_level]])
      if ("year" %in% names(weights)) {
        jids <- jids[level != this_level | year %in% unique(weights$year), ]
      }
      rm(weights)
    }
  }
  ## Submit stage 2 jobs (aggregate by geography and sex, collapse and compile) ------------
  cause <- basename(dir)
  parent_dir <- dirname(dir)
  
  # process post_raking, post_raking_lt and post_raking_lt_yll - add lt tasks for both
  if (grepl("post_raking", type)) {
    # Life tables (by year, sex, race, edu)
    # process post_raking_lt and post_raking_lt_yll
    if (grepl("post_raking_lt", type) & (cause == "_all")) {
      lt = TRUE
      mem_lt <- 30
      s_run_lt <- 36000
      jids[sex != 3 & (!settings_env$by_race | race != race_default) & (!settings_env$by_edu | edu != edu_default),
           `:=` (pred_lt_id = .GRP),
           by=c("year", "sex", "race", "edu")]
      
      pred_lt_tasks <- unique(jids[sex != 3 & (!settings_env$by_race | race != race_default) & (!settings_env$by_edu | edu != edu_default),
                                   .(year, sex, race, edu, pred_lt_id)])
      pred_lt_tasks[, hold_id := pred_lt_id]
      # Load task template
      pred_lt_template <- load_jobmon_task_templates(jobmon_tool)$pred_lt_template
      pred_lt_tasks[,
                    `:=` (pred_lt = list(jobmon_add_task(jobmon_task_template = pred_lt_template,
                                                         tool = jobmon_tool,
                                                         name = paste("pred_lt",year,sex,race,edu,raked,basename(dir),sep="_"),
                                                         code = fs::path_package("sae.shared", "/scripts/pred_lt.r"),
                                                         fthread = 8,
                                                         m_mem_free = paste0(mem_lt,"G"),
                                                         s_rt = s_run_lt,
                                                         archive = T,
                                                         sgeoutput = if (testing) dir,
                                                         queue = queue,
                                                         dir = dir,
                                                         year = as.integer(year),
                                                         sex = as.integer(sex),
                                                         race = as.integer(race),
                                                         raked = as.character(raked),
                                                         edu = as.integer(edu)))),
                    by=c("year", "sex", "race", "edu")]
      # tasks are duplicated by level, get unique tasks and add them
      # use task name to find unique tasks because task_id only works after
      # being bound to a workflow
      if (!is.null(get_unique_jobmon_tasks(pred_lt_tasks$pred_lt))) {
        jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, get_unique_jobmon_tasks(pred_lt_tasks$pred_lt))
      }
      if (verbose) start_time <- tock("lt", start_time)
      
    } else { # ignore lt tasks
      jids[, pred_lt_id := 1L] # For 'agg_raked_yll', only run from agg_races on.
      pred_lt_tasks <- unique(jids[sex != 3 & (!settings_env$by_race | race != race_default) & (!settings_env$by_edu | edu != edu_default),
                                   .(year, sex, race, edu, pred_lt_id)])
      pred_lt_tasks[, hold_id := pred_lt_id]
      pred_lt_tasks[, pred_lt := 1]
      if (verbose) start_time <- tock("skip lt", start_time)
    }
    
    # process post_raking_lt_yll - add yll tasks
    if (grepl("post_raking_lt_yll", type)) {
      mem_yll <- "20G"
      
      jids[sex != 3 & (!settings_env$by_race | race != race_default) & (!settings_env$by_edu | edu != edu_default),
           `:=` (pred_yll_id = .GRP),
           by=c("year", "sex", "race", "edu")]
      
      pred_yll_tasks <- unique(jids[sex != 3 & (!settings_env$by_race | race != race_default) & (!settings_env$by_edu | edu != edu_default),
                                    .(year, sex, race, edu, pred_yll_id, pred_lt_id)])
      setnames(pred_yll_tasks, "pred_lt_id", "hold_id")
      # Load task template
      pred_yll_template <- load_jobmon_task_templates(jobmon_tool)$pred_yll_template
      # Predict area-sex-level YLL Rates (by year, sex, race)
      # This needs to hold on the all-cause pred_lt job for both all cause and CoD.
      pred_yll_tasks[sex != 3 & (!settings_env$by_race | race != race_default) & (!settings_env$by_edu | edu != edu_default),
                     `:=` (pred_yll = list(jobmon_add_task(jobmon_task_template = pred_yll_template,
                                                           tool = jobmon_tool,
                                                           name = paste("pred_yll", year, sex, race, edu, raked, basename(dir), sep="_"),
                                                           code = fs::path_package("sae.shared", "/scripts/pred_yll.r"),
                                                           hold = if (lt) pred_lt_tasks[pred_lt_id %in% hold_id]$pred else lt_hold,
                                                           fthread = 8,
                                                           m_mem_free = mem_yll,
                                                           s_rt = 7200,
                                                           archive = T,
                                                           sgeoutput = if (testing) dir,
                                                           queue = queue,
                                                           dir = dir,
                                                           year = as.integer(year),
                                                           sex = as.integer(sex),
                                                           race = as.integer(race),
                                                           raked = as.character(raked),
                                                           edu = as.character(edu)))),
                     by=c("year", "sex", "race", "edu")]
      if (!is.null(get_unique_jobmon_tasks(pred_yll_tasks$pred_yll))){
        jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, get_unique_jobmon_tasks(pred_yll_tasks$pred_yll))
      }
      if (verbose) start_time <- tock("pred_yll", start_time)
    } else { # ignore yll tasks
      jids[, pred_yll_id := 1L] # For 'agg_raked_yll', only run from agg_races on.
      pred_yll_tasks <- unique(jids[sex != 3 & (!settings_env$by_race | race != race_default) & (!settings_env$by_edu | edu != edu_default),
                                    .(year, sex, race, edu, pred_yll_id)])
      pred_yll_tasks[, pred_yll := 1]
      if (verbose) start_time <- tock("skip pred_yll", start_time)
    }
  } else if (type == "agg_raked_yll") { # - we don't predict because we're raking YLLs, so fill in 1s for placeholders for pred_lt and pred_yll
    jids[, pred_lt_id := 1L] # For 'agg_raked_yll', only run from agg_races on.
    pred_lt_tasks <- unique(jids[sex != 3 & (!settings_env$by_race | race != race_default) & (!settings_env$by_edu | edu != edu_default),
                                 .(year, sex, race, edu, pred_lt_id)])
    pred_lt_tasks[, pred_lt := 1]
    
    jids[, pred_yll_id := 1L] # For 'agg_raked_yll', only run from agg_races on.
    pred_yll_tasks <- unique(jids[sex != 3 & (!settings_env$by_race | race != race_default) & (!settings_env$by_edu | edu != edu_default),
                                  .(year, sex, race, edu, pred_yll_id)])
    pred_yll_tasks[, pred_yll := 1]
    if (verbose) start_time <- tock("wtf pred_lt", start_time)
  } else {
    stop(glue::glue("Type = {type} not supported"))
  }
  # allow skipping of MX and/or YLL aggregation based off of flags
  agg_skip_flags <- c(
    if (skip_yll_aggregation) CLI.Flags$skip_yll_aggregation,
    if (skip_yld_aggregation) CLI.Flags$skip_yld_aggregation,
    if (skip_mx_aggregation) CLI.Flags$skip_mx_aggregation,
    if (skip_pred_aggregation) CLI.Flags$skip_pred_aggregation
  )
  # collapse flags to a single character and not a vector of character, which jobmon will interpret incorrectly
  agg_skip_flags <- paste(agg_skip_flags, collapse = " ")
  
  
  # Aggregate races (by year, sex)
  # Unnecessary to include education as a by-dimension here, because if data is by-race then it must be all-education.
  if (settings_env$by_race) {
    mem_race <- "75G"
    s_race <- 21600
    
    jids[sex != 3,
         `:=` (agg_races_id = .GRP),
         by=c("year", "sex")]
    
    agg_races_tasks <- unique(jids[sex != 3,
                                   .(year, sex, agg_races_id)])
    
    agg_races_tasks[, races_id_temp := agg_races_id]
    agg_races_tasks[, yll_id_hold := list(list(unique(jids[agg_races_id %in% races_id_temp]$pred_yll_id))), by=agg_races_id]
    agg_races_tasks[, lt_id_hold := list(list(unique(jids[agg_races_id %in% races_id_temp]$pred_lt_id))), by=agg_races_id]
    agg_races_tasks[, races_id_temp := NULL]
    
    # Load task template
    agg_races_template <- load_jobmon_task_templates(jobmon_tool)$agg_races_template
    agg_races_tasks[sex != 3,
                    `:=` (agg_races = list(jobmon_add_task(jobmon_task_template = agg_races_template,
                                                           tool = jobmon_tool,
                                                           name = unique(paste("agg_races", year, sex, raked, basename(dir), sep="_")),
                                                           code = fs::path_package("sae.shared", "/scripts/agg_races.r"),
                                                           hold = get_unique_jobmon_tasks(
                                                             c(lt_hold,
                                                               pred_lt_tasks[pred_lt_id %in% unlist(lt_id_hold)]$pred_lt,
                                                               pred_yll_tasks[pred_yll_id %in% unlist(yll_id_hold)]$pred_yll)),
                                                           fthread = 8,
                                                           m_mem_free = mem_race,
                                                           s_rt = s_race,
                                                           archive = T,
                                                           sgeoutput = if (testing) dir,
                                                           queue = queue,
                                                           dir = dir,
                                                           year = as.integer(year),
                                                           sex = as.integer(sex),
                                                           edu = as.integer(edu_default),
                                                           raked = as.character(raked),
                                                           lt = as.character(lt),
                                                           agg_skip_flags = agg_skip_flags))),
                    by=c("year", "sex")]
    
    # tasks are duplicated by level, so only add tasks in the first level of jids
    if (!is.null(get_unique_jobmon_tasks(agg_races_tasks$agg_races))){
      jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, get_unique_jobmon_tasks(agg_races_tasks$agg_races))
    }
    if (verbose) start_time <- tock("races", start_time)
  } else {
    jids[, agg_races_id := 1]  # For 'agg_raked_yll', only run from agg_races on.
    
    agg_races_tasks <- unique(jids[sex != 3, .(year, sex, edu, agg_races_id)])
    agg_races_tasks[, agg_races := 1]
    if (verbose) start_time <- tock("skip races", start_time)
  }
  
  # Aggregate education (by year, sex)
  # Unnecessary to include race as a by-dimension here, because if data is by-education, then it must be all-race.
  if (settings_env$by_edu) {
    mem_edu <- "75G"
    s_edu <- 21600
    
    jids[sex != 3,
         `:=` (agg_edus_id = .GRP),
         by=c("year", "sex")]
    
    agg_edus_tasks <- unique(jids[sex != 3,
                                  .(year, sex, agg_edus_id)])
    
    agg_edus_tasks[, edus_id_temp := agg_edus_id]
    agg_edus_tasks[, yll_id_hold := list(list(unique(jids[agg_edus_id %in% edus_id_temp]$pred_yll_id))), by=agg_edus_id]
    agg_edus_tasks[, lt_id_hold := list(list(unique(jids[agg_edus_id %in% edus_id_temp]$pred_lt_id))), by=agg_edus_id]
    agg_edus_tasks[, edus_id_temp := NULL]
    # Load task template
    agg_edus_template <- load_jobmon_task_templates(jobmon_tool)$agg_edus_template
    agg_edus_tasks[sex != 3,
                   `:=` (agg_edus = list(jobmon_add_task(jobmon_task_template = agg_edus_template,
                                                         tool = jobmon_tool,
                                                         name = unique(paste("agg_edus", year, sex, raked, basename(dir), sep="_")),
                                                         code = fs::path_package("sae.shared", "/scripts/agg_edu.r"),
                                                         hold = get_unique_jobmon_tasks(
                                                           c(lt_hold,
                                                             pred_lt_tasks[pred_lt_id %in% unlist(lt_id_hold)]$pred_lt,
                                                             pred_yll_tasks[pred_yll_id %in% unlist(yll_id_hold)]$pred_yll)),
                                                         fthread = 8,
                                                         m_mem_free = mem_edu,
                                                         s_rt = s_edu,
                                                         archive = T,
                                                         sgeoutput = if (testing) dir,
                                                         queue = queue,
                                                         dir = dir,
                                                         year = as.integer(year),
                                                         sex = as.integer(sex),
                                                         raked = as.character(raked),
                                                         lt = as.character(lt),
                                                         agg_skip_flags = agg_skip_flags))),
                   by=c("year", "sex")]
    
    # tasks are duplicated by level, so only add tasks in the first level of jids
    if (!is.null(get_unique_jobmon_tasks(agg_edus_tasks$agg_edus))){
      jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, get_unique_jobmon_tasks(agg_edus_tasks$agg_edus))
    }
    if (verbose) start_time <- tock("edu", start_time)
  } else {
    jids[, agg_edus_id := 1]  # For 'agg_raked_yll', only run from agg_edus on.
    
    agg_edus_tasks <- unique(jids[sex != 3, .(year, sex, agg_edus_id)])
    agg_edus_tasks[, agg_edus := 1]
    if (verbose) start_time <- tock("skip edu", start_time)
  }
  
  # Aggregate geographies (by level, year, sex, race)
  if (!is.null(settings_env$settings$geoagg_files)) {
    mem_geo <- "20G"
    s_geos <- 21600
    
    jids[sex != 3 & level != settings_env$area_var,
         `:=` (agg_geos_id = .GRP),
         by=c("level", "year", "sex", "race", "edu")]
    
    agg_geos_tasks <- unique(jids[sex != 3 & level != settings_env$area_var,
                                  .(level, year, sex, race, edu, agg_geos_id, pred_lt_id, pred_yll_id, agg_races_id, agg_edus_id)])
    
    agg_geos_tasks[, geos_id_temp := agg_geos_id]
    agg_geos_tasks[, yll_id_hold := list(list(unique(jids[agg_geos_id %in% geos_id_temp]$pred_yll_id))), by=agg_geos_id]
    agg_geos_tasks[, lt_id_hold := list(list(unique(jids[agg_geos_id %in% geos_id_temp]$pred_lt_id))), by=agg_geos_id]
    agg_geos_tasks[, races_id_hold := list(list(unique(jids[agg_geos_id %in% geos_id_temp]$agg_races_id))), by=agg_geos_id]
    agg_geos_tasks[, edus_id_hold := list(list(unique(jids[agg_geos_id %in% geos_id_temp]$agg_edus_id))), by=agg_geos_id]
    agg_geos_tasks[, geos_id_temp := NULL]
    
    
    # Load task template
    agg_geos_template <- load_jobmon_task_templates(jobmon_tool)$agg_geos_template
    
    agg_geos_tasks[sex != 3 & level != settings_env$area_var,
                   `:=` (agg_geos = list(jobmon_add_task(jobmon_task_template = agg_geos_template,
                                                         tool = jobmon_tool,
                                                         name = unique(paste("agg_geos", level, year, sex, race, edu, raked,
                                                                             lt, basename(dir), sep="_")),
                                                         code = fs::path_package("sae.shared", "/scripts/agg_geos.r"),
                                                         hold = get_unique_jobmon_tasks(
                                                           c(lt_hold,
                                                             pred_lt_tasks[pred_lt_id %in% unlist(lt_id_hold)]$pred_lt,
                                                             pred_yll_tasks[pred_yll_id %in% unlist(yll_id_hold)]$pred_yll,
                                                             agg_races_tasks[agg_races_id %in% unlist(races_id_hold)]$agg_races,
                                                             agg_edus_tasks[agg_edus_id %in% unlist(edus_id_hold)]$agg_edus)),
                                                         fthread = 8,
                                                         m_mem_free = mem_geo,
                                                         s_rt = s_geos,
                                                         archive = T,
                                                         sgeoutput = if (testing) dir,
                                                         queue = queue,
                                                         dir = dir,
                                                         level = level,
                                                         year = as.integer(year),
                                                         sex = as.integer(sex),
                                                         race = as.integer(race),
                                                         edu = as.integer(edu),
                                                         raked = as.character(raked),
                                                         lt = as.character(lt),
                                                         agg_skip_flags = agg_skip_flags))),
                   by=c("level", "year", "sex", "race", "edu")]
    
    if (!is.null(get_unique_jobmon_tasks(agg_geos_tasks$agg_geos))){
      jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, get_unique_jobmon_tasks(agg_geos_tasks$agg_geos))
    }
    if (verbose) start_time <- tock("geog", start_time)
    
  } else {
    jids[, agg_geos_id := 1]
    
    agg_geos_tasks <- unique(jids[sex != 3 & level != settings_env$area_var, .(level, year, sex, agg_geos_id)])
    agg_geos_tasks[, agg_geos := 1]
    if (verbose) start_time <- tock("skip geog", start_time)
  }
  
  # Aggregate sexes (by level, year, race, edu)
  mem_sex_mcnty <- "30G"
  mem_sex_oth <- "4G"
  s_sex_mcnty <- 18000
  s_sex_oth <- 7200
  
  jids[, `:=` (agg_sex_id = .GRP),
       by=c("level", "year", "race", "edu")]
  
  agg_sex_tasks <- unique(jids[, .(level, year, race, edu, agg_sex_id)])
  
  # have to get hold ids manually because each sex_id needs to hold on multiple ylls, agg_geos
  # and agg_races tasks.
  agg_sex_tasks[, sex_id_temp := agg_sex_id]
  agg_sex_tasks[, yll_id_hold := list(list(unique(jids[agg_sex_id %in% sex_id_temp]$pred_yll_id))), by=agg_sex_id]
  agg_sex_tasks[, races_id_hold := list(list(jids[agg_sex_id %in% sex_id_temp]$agg_races_id)), by=agg_sex_id]
  agg_sex_tasks[, edus_id_hold := list(list(jids[agg_sex_id %in% sex_id_temp]$agg_edus_id)), by=agg_sex_id]
  agg_sex_tasks[, geos_id_hold := list(list(jids[agg_sex_id %in% sex_id_temp]$agg_geos_id)), by=agg_sex_id]
  agg_sex_tasks[, lt_id_hold := list(list(jids[agg_sex_id %in% sex_id_temp]$pred_lt_id)), by=agg_sex_id]
  agg_sex_tasks[, sex_id_temp := NULL]
  
  # Load task template
  agg_sex_template <- load_jobmon_task_templates(jobmon_tool)$agg_sex_template
  
  agg_sex_tasks[, `:=` (agg_sex = list(
    jobmon_add_task(jobmon_task_template = agg_sex_template,
                    tool = jobmon_tool,
                    name = unique(paste("agg_sexes", level, year, race, edu,
                                        raked, lt, basename(dir), sep="_")),
                    code = fs::path_package("sae.shared", "/scripts/agg_sex.r"),
                    hold = c(lt_hold,
                             pred_lt_tasks[pred_lt_id %in% unlist(lt_id_hold)]$pred_lt,
                             pred_yll_tasks[pred_yll_id %in% unlist(yll_id_hold)]$pred_yll,
                             agg_races_tasks[agg_races_id %in% unlist(races_id_hold)]$agg_races,
                             agg_edus_tasks[agg_edus_id %in% unlist(edus_id_hold)]$agg_edus,
                             agg_geos_tasks[agg_geos_id %in% unlist(geos_id_hold)]$agg_geos),
                    fthread = 8,
                    m_mem_free = ifelse(level[1] == "mcnty", mem_sex_mcnty, mem_sex_oth),
                    s_rt = ifelse(level[1] == "mcnty", s_sex_mcnty, s_sex_oth),
                    archive = T,
                    sgeoutput = if (testing) dir,
                    queue = queue,
                    dir = dir,
                    level = level,
                    year = as.integer(year),
                    race = as.integer(race),
                    edu = as.integer(edu),
                    raked = as.character(raked),
                    lt = as.character(lt),
                    agg_skip_flags = agg_skip_flags))),
    by=c("level", "year", "race", "edu")]
  
  if (!is.null(get_unique_jobmon_tasks(agg_sex_tasks$agg_sex))){
    jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, get_unique_jobmon_tasks(agg_sex_tasks$agg_sex))
  }
  if (verbose) start_time <- tock("sex", start_time)
  # Compile estimates
  # If any inputs have changed, re-run compile script.
  # Otherwise, skip these jobs.
  mem_comp <- "15G"
  
  jids[, compile_id := 1]
  
  compile_tasks <- unique(jids[, .(compile_id)])
  compile_tasks[, sex_id_hold := list(list(unique(jids$agg_sex_id)))]
  
  # Load task template
  agg_compile_template <- load_jobmon_task_templates(jobmon_tool)$agg_compile_template
  
  compile_tasks[, `:=` (compile = list(
    jobmon_add_task(jobmon_task_template = agg_compile_template,
                    tool = jobmon_tool,
                    name = unique(paste("agg_compile", basename(dir), sep="_")),
                    code = fs::path_package("sae.shared", "/scripts/compile_estimates.r"),
                    hold = agg_sex_tasks[agg_sex_id %in% unlist(sex_id_hold)]$agg_sex,
                    fthread = 2,
                    m_mem_free = mem_comp,
                    s_rt = 7200,
                    archive = T,
                    sgeoutput = if (testing) dir,
                    queue = queue,
                    dir = dir,
                    raked = as.character(raked),
                    lt = as.character(lt),
                    validate = as.character(validate),
                    agg_skip_flags = agg_skip_flags)))]
  
  if (!is.null(get_unique_jobmon_tasks(compile_tasks$compile))){
    jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, get_unique_jobmon_tasks(compile_tasks$compile))
  }
  if (verbose) start_time <- tock("compile", start_time)
  if (lt) {
    lt_hold <- pred_lt_tasks$pred_lt
  }
  if (verbose) start_time <- tock("DONE", start_time)
  return(list("lt_hold" = lt_hold, "bound_workflow" = jobmon_workflow))
  
}