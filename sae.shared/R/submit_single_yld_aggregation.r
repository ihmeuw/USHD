#' @title submit_single_aggregation
#' @description A function version of the _submit_single_cause.r script used for submitting jobs to generate life tables and ylls, and aggregate based on the type passed in.
#'
#' @param jobmon_workflow a jobmon workflow object created with jobmonr::workflow. The
#'        jobmon tasks are bound to this object which is used to construct a dag and launch the jobs.
#' @param jobmon_tool a jobmon tool object created with jobmonr::tool.
#' @param dir character -- home directory for settings and final output
#' @param testing logical -- is this a testing job? if T, logs all output.
#' @param project character -- which project to launch qsubs on.
#' @param queue character -- which queue to launch qsubs on. Default "..."
#'
#' @note Due to the fact that some tasks hold on multiple others, and the tasks cannot
#' be duplicated (because only one task can be bound to the jobmon workflow), it is
#' necessary to split out the jobmon tasks from the jids tables (which contains the
#' permutations of all demographics that can get jobs).
#'
#' @return jids, a data.table containing the job_ids of all the jobs launched
#'
#' @export
submit_single_yld_aggregation <- function(jobmon_workflow,
                                          jobmon_tool,
                                          dir,
                                          testing,
                                          model_settings = NULL,
                                          project = "PROJECT",
                                          queue = "...") {
  raked <- TRUE
  validate <- FALSE
  lt <- FALSE

  settings_env <- model_settings$settings_for_cause(basename(dir))$settings

  # create a table for holding job ids
  # sex == 3 is both sexes, race == race_default is all race
  jids <- CJ(
    level = c(settings_env$area_var, names(settings_env$geoagg_files)),
    year = settings_env$years,
    sex = c(settings_env$sexes, 3),
    race = unique(c(settings_env$races, race_default)),
    edu = unique(c(settings_env$edu_races, edu_default))
  )

  cause <- basename(dir)
  parent_dir <- dirname(dir)

  # skip both mx and yll aggregation
  agg_skip_flags <- c(CLI.Flags$skip_yll_aggregation, CLI.Flags$skip_mx_aggregation, CLI.Flags$skip_pred_aggregation)

  # Aggregate races (by year, sex)
  if (settings_env$by_race) {
    mem_race <- "50G"
    s_race <- 21600

    jids[sex != 3,
      `:=`(agg_races_id = .GRP),
      by = c("year", "sex")
    ]

    # Load jobmon task template
    agg_races_template <- load_jobmon_task_templates(jobmon_tool)$agg_races_template

    agg_races_tasks <- unique(jids[
      sex != 3,
      .(year, sex, agg_races_id)
    ])
    agg_races_tasks[sex != 3,
      `:=`(agg_races = list(jobmon_add_task(
        jobmon_task_template = agg_races_template,
        tool = jobmon_tool,
        name = unique(paste("agg_races", year, sex, raked, basename(dir), sep = "_")),
        code = fs::path_package("sae.shared", "/scripts/agg_races.r"),
        hold = NULL,
        fthread = 8,
        m_mem_free = mem_race,
        s_rt = s_race,
        archive = T,
        sgeoutput = if (testing) dir,
        queue = queue,
        # task args
        dir = dir,
        year = as.integer(year),
        sex = as.integer(sex),
        edu = as.integer(edu_default),
        raked = as.character(raked),
        lt = as.character(lt),
        agg_skip_flags = paste(agg_skip_flags, collapse = " ")
      ))),
      by = c("year", "sex")
    ]

    # tasks are duplicated by level, so only add tasks in the first level of jids
    if (!is.null(get_unique_jobmon_tasks(agg_races_tasks$agg_races))) {
      jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, get_unique_jobmon_tasks(agg_races_tasks$agg_races))
    }
  } else {
    jids[, agg_races_id := 1]

    agg_races_tasks <- unique(jids[sex != 3, .(year, sex, agg_races_id)])
    agg_races_tasks[, agg_races := 1]
  }

  # Aggregate geographies (by level, year, sex, race)
  if (!is.null(settings_env$geoagg_files)) {
    mem_geo <- "10G"
    s_geos <- 21600

    jids[sex != 3 & level != settings_env$area_var,
      `:=`(agg_geos_id = .GRP),
      by = c("level", "year", "sex", "race", "edu")
    ]

    # Load task template
    agg_geos_template <- load_jobmon_task_templates(jobmon_tool)$agg_geos_template

    agg_geos_tasks <- unique(jids[
      sex != 3 & level != settings_env$area_var,
      .(level, year, sex, race, edu, agg_geos_id, agg_races_id)
    ])

    setnames(agg_geos_tasks, c("agg_races_id"), c("races_id_hold"))

    agg_geos_tasks[sex != 3 & level != settings_env$area_var,
      `:=`(agg_geos = list(jobmon_add_task(
        jobmon_task_template = agg_geos_template,
        tool = jobmon_tool,
        name = unique(paste("agg_geos", level, year, sex, race, edu, raked,
          "FALSE", basename(dir),
          sep = "_"
        )),
        code = fs::path_package("sae.shared", "/scripts/agg_geos.r"),
        hold = get_unique_jobmon_tasks(
          c(agg_races_tasks[agg_races_id %in% races_id_hold]$agg_races)
        ),
        fthread = 8,
        m_mem_free = mem_geo,
        s_rt = s_geos,
        archive = T,
        sgeoutput = if (testing) dir,
        queue = queue,
        # task args
        dir = dir,
        level = level,
        year = as.integer(year),
        sex = as.integer(sex),
        race = as.integer(race),
        edu = as.integer(edu_default),
        raked = as.character(raked),
        lt = as.character(lt),
        agg_skip_flags = paste(agg_skip_flags, collapse = " ")
      ))),
      by = c("level", "year", "sex", "race")
    ]

    if (!is.null(get_unique_jobmon_tasks(agg_geos_tasks$agg_geos))) {
      jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, get_unique_jobmon_tasks(agg_geos_tasks$agg_geos))
    }
  } else {
    jids[, agg_geos_id := 1]

    agg_geos_tasks <- unique(jids[sex != 3 & level != settings_env$area_var, .(level, year, sex, agg_geos_id)])
    agg_geos_tasks[, agg_geos := 1]
  }

  # Aggregate sexes (by level, year, race)
  mem_sex_mcnty <- "30G"
  mem_sex_oth <- "4G"
  s_sex_mcnty <- 18000
  s_sex_oth <- 7200

  jids[, `:=`(agg_sex_id = .GRP),
    by = c("level", "year", "race")
  ]

  # Load task template
  agg_sex_template <- load_jobmon_task_templates(jobmon_tool)$agg_sex_template

  agg_sex_tasks <- unique(jids[, .(level, year, race, agg_sex_id)])
  # have to get hold ids manually because each sex_id needs to hold on multiple ylls, agg_geos
  # and agg_races tasks.
  agg_sex_tasks[, sex_id_temp := agg_sex_id]
  agg_sex_tasks[, races_id_hold := list(list(jids[agg_sex_id %in% sex_id_temp]$agg_races_id)), by = agg_sex_id]
  agg_sex_tasks[, geos_id_hold := list(list(jids[agg_sex_id %in% sex_id_temp]$agg_geos_id)), by = agg_sex_id]
  agg_sex_tasks[, sex_id_temp := NULL]

  agg_sex_tasks[, `:=`(agg_sex = list(
    jobmon_add_task(
      jobmon_task_template = agg_sex_template,
      tool = jobmon_tool,
      name = unique(paste("agg_sexes", level, year, race,
        raked, "FALSE", basename(dir),
        sep = "_"
      )),
      code = fs::path_package("sae.shared", "/scripts/agg_sex.r"),
      hold = c(
        agg_races_tasks[agg_races_id %in% unlist(races_id_hold)]$agg_races,
        agg_geos_tasks[agg_geos_id %in% unlist(geos_id_hold)]$agg_geos
      ),
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
      edu = as.integer(edu_default),
      raked = as.character(raked),
      lt = as.character(lt),
      agg_skip_flags = paste(agg_skip_flags, collapse = " ")
    )
  )),
  by = c("level", "year", "race")
  ]

  if (!is.null(get_unique_jobmon_tasks(agg_sex_tasks$agg_sex))) {
    jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, get_unique_jobmon_tasks(agg_sex_tasks$agg_sex))
  }

  # Compile estimates
  # If any inputs have changed, re-run compile script.
  # Otherwise, skip these jobs.
  mem_comp <- "15G"

  jids[, compile_id := 1]

  # Load task template
  agg_compile_template <- load_jobmon_task_templates(jobmon_tool)$agg_compile_template

  compile_tasks <- unique(jids[, .(compile_id)])
  compile_tasks[, sex_id_hold := list(list(unique(jids$agg_sex_id)))]

  compile_tasks[, `:=`(compile = list(
    jobmon_add_task(
      jobmon_task_template = agg_compile_template,
      tool = jobmon_tool,
      name = unique(paste("agg_compile", basename(dir), sep = "_")),
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
      agg_skip_flags = paste(agg_skip_flags, collapse = " ")
    )
  ))]

  if (!is.null(get_unique_jobmon_tasks(compile_tasks$compile))) {
    jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, get_unique_jobmon_tasks(compile_tasks$compile))
  }

  return(list("bound_workflow" = jobmon_workflow))
}
