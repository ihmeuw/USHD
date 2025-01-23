#' @title compute_raking_jobs
#' @description Calculates the lists of raking jobs and compile jobs to submit
#'
#' @param to_dir [character] Upper model directory
#' @param from_dir [character] Lower model directory
#' @param children_of [character] If specified, subset to direct children of this cause
#' @param initial_level [integer] Beginning level of the cause hierarchy to submit.
#' @param terminal_level [integer] Ending level of the cause hierarchy to submit.
#' @param years [integer vector] List of years to run
#' @param sexes [integer vector] List of sexes to run
#' @param ages [integer vector] List of ages to run
#' @param submitted_causes  data.table of causes, reads from submitted_cause_list.csv if NULL, Default: NULL
#' @param measure mx, yll or yld, Default: NULL
#'
#' @return list(raking_jobs = raking_jobs, compile_jobs = compile_jobs, causes = causes)
#' @rdname compute_raking_jobs
#' @export
compute_raking_jobs <- function(to_dir, from_dir, children_of, initial_level, terminal_level, years, sexes, ages,
                                submitted_causes = NULL, measure = NULL) {

  ## Load the cause list -----------------------------------------------------------------------------
  if (is.null(submitted_causes)) {
    causes <- ModelSettings$get_cause_datatable(dir = from_dir)
  } else {
    causes <- data.table()
    for (cause in names(submitted_causes)) {
      causes <- rbind(causes, c("acause" = cause, submitted_causes[[cause]]), fill = TRUE)
    }
  }

  stop.if.not.expected.cols(causes, c("acause", "level", "cause_id", "parent_id", "run_model"), extra.ok = TRUE)
  causes[cause_id == 294, parent_id := 0]

  # If 'children_of' is specified, subset down to just this parent
  # and its children.
  if (!is.null(children_of)) {
    if (!children_of %in% unique(causes$acause)) {
      lsae.utils::stop_or_quit(paste0("children_of argument is invalid! Was passed: ", children_of), status = 2)
    }
    parent_id_to_subset <- causes[acause == children_of, cause_id]
    causes <- causes[cause_id == parent_id_to_subset | parent_id == parent_id_to_subset]
    initial_level <- causes[acause == children_of, level]
    terminal_level <- initial_level + 1
  }

  
  # Instead we include all causes from root (_all) to terminal level and subset later
  causes <- annotate_post_model_workflow(causes)
  causes <- causes[level %in% 0:terminal_level, list(cause_id, acause, level, parent_id, sexes, ages, rake_model, compile_model, run_model)]
  setkeyv(causes, "cause_id")

  # Create jobs: raking, compiling ------------------------------------------
  # Methodology:
  #   * 1 row per job detailed by relevant demographics
  #   * Each row (job) has a predetermined job name
  #   * Because job names and dependencies are deterministic, we can tack on a
  #       list of parent job names each individual job depends on
  #   * raking jobs are in perspective of the parent cause
  #   * compile jobs are in perspecitve of a single cause for which results are compiled
  dimensions <- data.table()[, CJ(year = years, sex = sexes, age = ages)]
  dimensions <- cross(causes, dimensions)

  # Filter out age and sex restrictions based on cause
  # Have to use mapply here due to data.table semantics
  dimensions <- dimensions[mapply(function(x, y) {
    x %in% eval(parse(text = y))
  }, x = sex, y = sexes)]
  dimensions <- dimensions[mapply(function(x, y) {
    x %in% eval(parse(text = y))
  }, x = age, y = ages)]

  # Create raking jobs: cause_id is the parent cause children are raked to
  all_cause_raking_jobs <- data.table()[, CJ(job_type = "all_cause_rake", cause_id = 0, acause = "_all", level = -1, year = years, sex = sexes)]
  
  # if the directories are equivalent, then that means we are just raking in one dimension across cause,
  # so raking is not done by age and therefore we don't want to include age as a dimension
  if(!identical(normalizePath(from_dir), normalizePath(to_dir))) {
    
    message("Submitting by age")
    
    raking_jobs <- dimensions[rake_model == TRUE, list(job_type = "rake", cause_id, acause, level, parent_id, year, sex, age)]
    # Make an exception for mater_neonat because of the unique age restrictions by sex
    if("mater_neonat" %in% raking_jobs$acause) {
      raking_jobs <- raking_jobs[!(acause == "mater_neonat" & sex == 1 & age > 1)]
    }
  } else {
    
    if(to_dir == "gbd") stop()
    
    message("Submitting not be age (should only be when to_dir and from_dir are identical)")
    
    raking_jobs <- unique(dimensions[rake_model == TRUE, list(job_type = "rake", cause_id, acause, level, parent_id, year, sex)])
  }

  raking_jobs <- rbind(all_cause_raking_jobs, raking_jobs, fill = TRUE)
  # For memory usage optimization: the more children, the more memory requested
  num_child_causes <- causes[, length(cause_id), by = c("parent_id")]
  setnames(num_child_causes, c("parent_id", "V1"), c("cause_id", "num_child_causes"))
  raking_jobs <- merge(raking_jobs, num_child_causes, by = "cause_id")

  
  if(!identical(normalizePath(from_dir), normalizePath(to_dir))) {
    
    message("Submitting by level")
    
    raking_jobs[level == -1, job_name := paste("rake", cause_id, year, sex, sep = "_")]
    raking_jobs[level >= 0, job_name := paste("rake", cause_id, year, sex, age, sep = "_")]
    
    raking_jobs[level == 0, parent_job_name := paste("rake", parent_id, year, sex, sep = "_")]
    raking_jobs[level >= 1, parent_job_name := paste("rake", parent_id, year, sex, age, sep = "_")]
  } else {
    
    if(to_dir == "gbd") stop()
    
    message("Not submitting by level (should only be when to_dir and from_dir are identical)")
    
    # do not want to rake by age when using the experimental raking code
    raking_jobs[, job_name := paste("rake", cause_id, year, sex, sep = "_")]
    raking_jobs[, parent_job_name := paste("rake", parent_id, year, sex, sep = "_")]
  }


  # subset raking jobs to initial/terminal level. this is very unintuitive
  valid.range <- seq(initial_level - 1, terminal_level) # Raking jobs need to decremented by one, because they save jobs at the child-cause level.
  raking_jobs <- raking_jobs[level %in% valid.range]

  if(identical(measure, "yld") || identical(measure, "pred")) {
    compile_jobs <- NULL
  } else {
    # Create compile jobs: cause_id is the cause to compile
    # First, include age dimension to create list of parent job names and then drop that dimension
    # However, if the directories are equivalent, then that means we are just raking in one dimension across cause,
    # so raking is not done by age and therefore we don't want to include age as a dimension
    if(!identical(normalizePath(from_dir), normalizePath(to_dir))) {
      compile_jobs <- dimensions[compile_model == TRUE, list(job_type = "compile", cause_id, acause, level, parent_id, year, sex, age)]
    } else {
      compile_jobs <- unique(dimensions[compile_model == TRUE, list(job_type = "compile", cause_id, acause, level, parent_id, year, sex)])
    }

    compile_jobs <- compile_jobs[level %in% initial_level:terminal_level]

    # Doing both to_dir and from_dir compiles
    if (from_dir == "NONE") {
      crd_compile_jobs <- NULL
    } else {
      crd_compile_jobs <- copy(compile_jobs)
      crd_compile_jobs[, dir := from_dir][, dir_type := "from_dir"]
    }
    compile_jobs[, dir := to_dir][, dir_type := "to_dir"]
    compile_jobs <- rbind(compile_jobs, crd_compile_jobs)

    # Job names: detailed by same demographics as raking jobs EXCEPT age
    compile_jobs[, job_name := paste("compile", cause_id, year, sex, dir_type, sep = "_")]
    
    # if the directories are equivalent, then that means we are just raking in one dimension across cause,
    # so raking is not done by age and therefore we don't want to include age as a dimension
    if(!identical(normalizePath(from_dir), normalizePath(to_dir))) {
      
      message("Submitting and removing age")
      
      compile_jobs[, parent_job_name := paste("rake", parent_id, year, sex, age, sep = "_", collapse = ","), by = job_name]
      compile_jobs <- unique(compile_jobs[, -"age"])
    } else {
      if(to_dir == "gbd") stop()
      message("Submitting and not removing age")
      compile_jobs[, parent_job_name := paste("rake", parent_id, year, sex, sep = "_", collapse = ","), by = job_name]
    }


  }

  return(list(raking_jobs = raking_jobs, compile_jobs = compile_jobs, causes = causes))
}
