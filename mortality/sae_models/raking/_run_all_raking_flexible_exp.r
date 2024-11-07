####################################################################################################
## Description: Script to submit all raking jobs for a particular model run.
##
##              Specifically:
##              1. submit jobs to run "rake_by_geography.r" for all-cause mortality (by sex, year)
##              2. submit jobs to run "rake_by_geography_and_cause.r" for cause-specific mortality
##                 (by sex, year, age, and parent cause)
##              3. submit jobs to run "compile_raked.r" for cause-specific mortality (by sex,
##                 year, and child cause)
##
## Passed args: dir [character] -- the top-level directory for a given model run
##              initial_level [int, 0-3] -- the shallowest level of the hierarchy for which you want
##                  raked results
##              terminal_level [int, 0-4] -- the deepest level of the hierarchy for which you want
##                  raked results.
##              measure [character] -- which measure to rake, "mx" for mortality rates, "yll" for
##                  ylls
##
####################################################################################################

stopifnot(grepl("mortality/sae_models$", getwd()))

suppressMessages(suppressWarnings({
  library(R.utils)
  library(data.table)
  library(stringr)
  sourceDirectory("functions/", modifiedOnly=FALSE)
}))

library(jobmonr)

## Get settings ------------------------------------------------------------------------------------
parser <- argparse::ArgumentParser()

# Rake operation arguments
parser$add_argument("--from_dir", type = "character", help = "Lower model directory")
parser$add_argument("--to_dir", type = "character", help = "Upper model directory")
parser$add_argument("--initial_level", type="integer", choices=sae.shared::cause_choices, help="Initial level of cause hierarchy") 
parser$add_argument("--terminal_level", type="integer", choices=sae.shared::cause_choices, help="Terminal level of cause hierarchy")
parser$add_argument("--measure", type = "character", help="Which measure to rake.", choices=c('mx', 'yll'))
parser$add_argument("--from_geo", type = "character",
                    help="Geography level for lower model.", choices=sae.shared::geo_choices)
parser$add_argument("--to_geo", type = "character",
                    help="Geography level for upper model.", choices=sae.shared::geo_choices)
parser$add_argument("--draws", type = "integer", help="Number of draws to rake at.")
parser$add_argument("--common_raking_vars", type = "character", help="Variables at the same level of granularity between the two models.")
parser$add_argument("--lower_raking_vars", type = "character", help="Variables to rake by for the lower model.")
parser$add_argument("--upper_raking_vars", type = "character", help="Variables to rake by for the upper model.")
parser$add_argument("--children_of", type="character", default=NULL, help = "Parent cause to subset to. Can be helpful for rakes over many levels of the cause hierarchy.")

# Cluster workflow arguments
parser$add_argument("--project", default = "PROJ")
parser$add_argument("--queue", default = "QUEUE")
parser$add_argument("--use_verbose", choices=c("TRUE", "FALSE"))
parser$add_argument("--jobmon_resume", choices = c("TRUE", "FALSE"), default=F)

args <- parser$parse_args(get_args())
for (key in names(args)) {
  assign(key, args[[key]])
}

print("Arguments passed were:")
for (arg in names(args)){
  print(paste0(arg, ": ", get(arg)))
}

from_settings = ModelSettings$from_dir(from_dir)

validate_cause_subset_args(children_of, initial_level, terminal_level)

# Load in jobmon tool, workflow, and task templates
jobmon_tool <- jobmonr::tool(name='USHD_postmodeling')
task_templates <- load_jobmon_task_templates(jobmon_tool)
jobmon_workflow_name <- get_jobmon_workflow_name(jobmon_resume = jobmon_resume, 
                                                 measure = measure, 
                                                 type = "raking", 
                                                 from_dir = from_dir,
                                                 initial_level = initial_level,
                                                 terminal_level = terminal_level,
                                                 children = children_of)

jobmon_workflow <- jobmonr::workflow(tool = jobmon_tool, 
                                     workflow_args=jobmon_workflow_name, 
                                     default_compute_resources_set = list(
                                       "slurm" = list(queue = queue, 'project' = project,
                                                      stderr=paste0(from_dir, "/job-output"),
                                                      stdout=paste0(from_dir, "/job-output")
                                                      )
                                     ),
                                     default_cluster_name = "slurm",
                                     name = paste0(measure, "_raking_lvl_", initial_level, "_", terminal_level))

# make flat file directory for storing jobmon workflow info if it doesn't exist
make_output_dir(paste0(from_dir, "/jobmon_ids"))

# save out workflow name for resumes
jobmon_ids <- data.table("workflow_name" = jobmon_workflow_name, "workflow_id" = NA_integer_, workflow_run_id = NA_integer_, "resume" = jobmon_resume)
jobmon_id_path = get_jobmon_csv_filepath(measure, "raking", from_dir, initial_level, terminal_level, children_of)

warning(sprintf("NOTE: all logs are being written into %s", from_dir))

if (draws < 1000) {
  warning("WARNING\ndraws is set to a value less than 1000. GBD results used in raking will be down-sampled and raked results will not match GBD results.")
}

# Compute raking jobs list 
jobs_list = compute_raking_jobs(
  to_dir,
  from_dir,
  children_of,
  initial_level,
  terminal_level,
  from_settings$years,
  from_settings$sexes,
  from_settings$ages
)

raking_jobs = jobs_list$raking_jobs
causes = jobs_list$causes

# We only run compile jobs if we're iterating down the cause hierarchy.
if (!is.null(children_of)) {
  initial_level <- terminal_level <- causes[acause==children_of, level+1]
}

if (terminal_level>0) {
  common_raking_vars_2d <- 'year,age,sex,sim'
  target_dims = paste(c(
    if (from_geo != to_geo) "geo",
    if (grepl("race", lower_raking_vars)) "race",
    if (grepl("edu", lower_raking_vars)) "edu",
    "cause"), collapse = ',')
  
  message(paste0("Target dims: ",paste(target_dims,collapse=", ")))
  
  dimensions <- data.table()[, CJ(year = from_settings$years, sex = from_settings$sexes, age = from_settings$ages)]
  dimensions <- cross(causes, dimensions)
  
  # Filter out age and sex restrictions based on cause
  # Have to use mapply here due to data.table semantics
  dimensions <- dimensions[mapply(function(x, y) { x %in% eval(parse(text = y)) }, x = sex, y = sexes)]
  dimensions <- dimensions[mapply(function(x, y) { x %in% eval(parse(text = y)) }, x = age, y = ages)]
  
  # Create compile jobs: cause_id is the cause to compile
  # First, include age dimension to create list of parent job names and then drop that dimension
  if(!identical(normalizePath(from_dir), normalizePath(to_dir))) {
    compile_jobs <- dimensions[compile_model == TRUE, list(job_type = "compile", cause_id, acause, level, parent_id, year, sex, age)]
  } else {
    compile_jobs <- unique(dimensions[compile_model == TRUE, list(job_type = "compile", cause_id, acause, level, parent_id, year, sex)])
  }

  compile_jobs[, dir := from_dir]
  
  # Job names: detailed by same demographics as raking jobs EXCEPT age
  compile_jobs[, job_name := paste("compile", cause_id, year, sex, sep = "_")]
  
  if(!identical(normalizePath(from_dir), normalizePath(to_dir))) {
    compile_jobs[, parent_job_name := paste("rake", parent_id, year, sex, age, sep = "_", collapse = ","), by = job_name]
    compile_jobs <- unique(compile_jobs[, -"age"])
  } else {
    compile_jobs[, parent_job_name := paste("rake", parent_id, year, sex, sep = "_", collapse = ","), by = job_name]
  }
} 

## Submit raking jobs ------------------------------------------------------------------------------
for (lvl in initial_level:terminal_level) {
  message(sprintf("Adding jobs for level %i", lvl))
  if (lvl == 0) {
    if (draws == 1000) {
      mem_rake_geo <- "200G"
      s_rake_geo <- 21600
    } else {
      mem_rake_geo <- "12G"
      s_rake_geo <- 3600
    }
    
    # remove cause from 1D rakes 
    lower_raking_vars <- str_remove(lower_raking_vars, ",cause")
    
    if (nrow(raking_jobs[level == lvl - 1])>0){
      # submit (one-dimensional) raking for all-cause mortality
      # this is one job per year-sex-race
      print(paste0("Submitting raking jobs for level: 0, and cause: _all"))
      raking_jobs[
        level == lvl - 1, 
        `:=` (job_id = list(jobmon_add_task(jobmon_task_template = task_templates$flexible_raking_template,
                                            tool = jobmon_tool,
                                            name = job_name, 
                                            code = fs::path_package("sae.shared", "scripts", "flexible_raking.r"),
                                            fthread = 1, m_mem_free = mem_rake_geo, s_rt = s_rake_geo, 
                                            archive = T,
                                            sgeoutput = paste0(from_dir, "/_all/"), queue = queue,
                                            # task args
                                            from_dir = from_dir,
                                            to_dir = to_dir,
                                            measure = measure,
                                            from_geo = from_geo,
                                            to_geo = to_geo,
                                            common_raking_vars = common_raking_vars,
                                            lower_raking_vars = lower_raking_vars,
                                            upper_raking_vars = upper_raking_vars,
                                            year = as.integer(year),
                                            sex = as.integer(sex)))), 
        by=c("year", "sex")]
      
      print(paste0("Raked files will be saved to: ", from_dir))
      
      jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, unlist(raking_jobs$job_id))
      # Merge these jobs IDs as holds for lower-level causes
      all_cause_raking_ids = raking_jobs[level == lvl - 1, .(job_name, job_id)]
      names(all_cause_raking_ids) <- c('parent_job_name', 'hold_jid')
      raking_jobs = merge(raking_jobs, all_cause_raking_ids, by='parent_job_name', all.x=TRUE)
    } else {
      raking_jobs[parent_id == 0, hold_jid := NA_integer_] # A stand-in job hold, if no all-cause jobs were launched. 
    }
  } else if (!identical(normalizePath(from_dir), normalizePath(to_dir))) {
    message("Running 2D raking")
    
    # Need to get parent_acause, year, age, sex from data table.
    # create (two-dimensional) raking tasks. one job per year-sex-race-age AND parent cause, holding on
    # the corresponding raking job for the parent's parent

    print(paste0("Submitting raking jobs for level: ", lvl, ", and children of:", paste(causes[level == lvl-1]$acause, collapse = ", ")))
    holds_specified = 'hold_jid' %in% names(raking_jobs)
    raking_jobs[
      level == lvl - 1, 
      `:=` (job_id = list(jobmon_add_task(jobmon_task_template = task_templates$flexible_2d_raking_template,
                                          tool = jobmon_tool,
                                          name = job_name, 
                                          code = fs::path_package("sae.shared", "scripts", "flexible_2d_raking.r"),
                                          hold = if (holds_specified) unlist(hold_jid),
                                          fthread = 1,
                                          m_mem_free = paste0(
                                            max(5 * num_child_causes * if (draws == 1000) 2 else 1, if (measure == 'mx') 120 else 40),"G"),
                                          s_rt = 72000,
                                          archive = T,
                                          sgeoutput =  paste0(from_dir, "/", acause), 
                                          queue = queue,
                                          # task args
                                          from_dir = from_dir,
                                          to_dir = to_dir,
                                          measure = measure,
                                          to_geo = to_geo,
                                          target_dims = target_dims,
                                          common_raking_vars = common_raking_vars_2d, 
                                          draws = as.integer(draws),
                                          use_verbose = as.character(use_verbose), 
                                          year = as.integer(year),
                                          sex = as.integer(sex),
                                          age = as.integer(age),
                                          parent_acause = acause))), 
      by=c("year", "sex", "age", "acause")]
    
    print(paste0("Raked files will be saved to: ", from_dir))
    
    # Merge these jobs IDs as holds for compile_estimates
    geo_and_cause_ids = raking_jobs[level == lvl - 1, .(job_name, job_id)]
    
    # Add the tasks to the jobmon workflow
    jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, unlist(geo_and_cause_ids$job_id))
    
    # Where geo_and_cause_ids$job_name is in compile_jobs$parent_job_name,
    # Merge on a vector of geo_and_cause_ids$job_id.
    compile_jobs_subset = unique(compile_jobs[level == lvl, .(parent_job_name)])
    if (nrow(compile_jobs)>0){
      for (i in 1:nrow(compile_jobs_subset)){
        parent_job_names = compile_jobs_subset$parent_job_name[i]
        parent_job_names = unlist(strsplit(parent_job_names, ","))
        job_ids = list(geo_and_cause_ids[job_name %in% parent_job_names, job_id])
        compile_jobs_subset[i, hold_jid := job_ids]
      }
      compile_jobs_with_holds = merge(compile_jobs, compile_jobs_subset, by="parent_job_name", all.x=TRUE)
      
      if (nrow(compile_jobs_subset) == 0){ 
        compile_jobs_with_holds[, hold_jid := 1]
      }
      
      # queue up tasks to compile age-specific files. one job per year-sex-race AND child cause, holding
      # on the raking jobs for the parent
      print(paste0("Submitting Compile jobs for level:", lvl, " , and child causes :", paste(causes[level == lvl]$acause, collapse = ", ")))
      compile_jobs_with_holds[
        level == lvl, 
        `:=` (job_id = list(jobmon_add_task(jobmon_task_template = task_templates$raking_compile_template,
                                            tool = jobmon_tool,
                                            name = job_name, 
                                            code = fs::path_package("sae.shared", "scripts", "compile_raked.r"),
                                            hold = unlist(hold_jid),
                                            fthread = 1, m_mem_free = if (draws == 1000) "90G" else "10G", s_rt = 3600,
                                            archive = T,
                                            sgeoutput = paste0(from_dir, "/", acause), 
                                            queue = queue,
                                            # task args
                                            dir = dir,
                                            year = as.integer(year),
                                            sex = as.integer(sex),
                                            acause = acause,
                                            measure = measure))), 
        by=c("year", "sex", "acause")]
      
      print(paste0("Compiled draws and estimates will be saved to cause dirs in: ", from_dir))
      
      jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, unlist(compile_jobs_with_holds$job_id))
    } else {
      message("No compile jobs created given these settings. Skipping compile step.")
    }
  } else {
    
    ## this is for the cause where from_dir == to_dir (raking within the USHD paths)
    print("Using experimental raking code!!")
    print(paste0("Submitting raking jobs for level: ", lvl, ", and children of:", paste(causes[level == lvl-1]$acause, collapse = ", ")))
    holds_specified = 'hold_jid' %in% names(raking_jobs)
    
    raking_jobs[
      level == lvl - 1, 
      `:=` (job_id = list(jobmon_add_task(jobmon_task_template = task_templates$flexible_raking_template_exp,
                                          tool = jobmon_tool,
                                          name = job_name, 
                                          code = fs::path_package("sae.shared", "scripts", "flexible_raking_without_gbd.r"),
                                          hold = if (holds_specified) unlist(hold_jid),
                                          fthread = 1, 
                                          m_mem_free = paste0(
                                            # min out at 120G for mx, 40 for yll
                                            max(5 * num_child_causes * if (draws == 1000) 4 else 1, if (measure == 'mx') 120 else 40),"G"), 
                                          s_rt = 259200, 
                                          archive = T,
                                          sgeoutput = paste0(from_dir, "/_all/"), queue = queue,
                                          # task args
                                          from_dir = from_dir,
                                          to_dir = to_dir,
                                          measure = measure,
                                          from_geo = from_geo,
                                          to_geo = to_geo,
                                          common_raking_vars = common_raking_vars,
                                          lower_raking_vars = lower_raking_vars,
                                          upper_raking_vars = upper_raking_vars,
                                          parent_acause = acause,
                                          year = as.integer(year),
                                          sex = as.integer(sex)))), 
      by=c("year", "sex", "acause")]
    
    print(paste0("Raked files will be saved to: ", from_dir))
    
    # Merge these jobs IDs as holds for compile_estimates
    geo_and_cause_ids = raking_jobs[level == lvl - 1, .(job_name, job_id)]
    
    # Add the tasks to the jobmon workflow
    jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, unlist(geo_and_cause_ids$job_id))
    
    # Where geo_and_cause_ids$job_name is in compile_jobs$parent_job_name,
    # Merge on a vector of geo_and_cause_ids$job_id.
    compile_jobs_subset = unique(compile_jobs[level == lvl, .(parent_job_name)])
    if (nrow(compile_jobs)>0){
      for (i in 1:nrow(compile_jobs_subset)){
        parent_job_names = compile_jobs_subset$parent_job_name[i]
        parent_job_names = unlist(strsplit(parent_job_names, ","))
        job_ids = list(geo_and_cause_ids[job_name %in% parent_job_names, job_id])
        compile_jobs_subset[i, hold_jid := job_ids]
      }
      compile_jobs_with_holds = merge(compile_jobs, compile_jobs_subset, by="parent_job_name", all.x=TRUE)
      
      if (nrow(compile_jobs_subset) == 0){ # No compile jobs have been run! Create a dummy hold_jid.
        compile_jobs_with_holds[, hold_jid := 1]
      }
      
      # queue up tasks to compile age-specific files. one job per year-sex-race AND child cause, holding
      # on the raking jobs for the parent
      print(paste0("Submitting Compile jobs for level:", lvl, " , and child causes :", paste(causes[level == lvl]$acause, collapse = ", ")))
      compile_jobs_with_holds[
        level == lvl, 
        `:=` (job_id = list(jobmon_add_task(jobmon_task_template = task_templates$raking_compile_template,
                                            tool = jobmon_tool,
                                            name = job_name, 
                                            code = fs::path_package("sae.shared", "scripts", "compile_raked.r"),
                                            hold = unlist(hold_jid),
                                            fthread = 1, m_mem_free = if (draws == 1000) "90G" else "10G", s_rt = 3600,
                                            archive = T,
                                            sgeoutput = paste0(from_dir, "/", acause), 
                                            queue = queue,
                                            # task args
                                            dir = dir,
                                            year = as.integer(year),
                                            sex = as.integer(sex),
                                            acause = acause,
                                            measure = measure))), 
        by=c("year", "sex", "acause")]
      
      jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, unlist(compile_jobs_with_holds$job_id))
    }
  }
}

# Bind workflow to get workflow ID
jobmon_workflow$bind()

# save out workflow info for jobmon db queries and resumes
jobmon_ids[.N, `:=` (workflow_id = jobmon_workflow$workflow_id, workflow_run_id = NA)]
write_csv(jobmon_ids, jobmon_id_path, update = T, row.names = F, lock = T)

# run jobmon workflow and wait for completion (up to 1 day)
wfr_status <- jobmonr::run(jobmon_workflow, resume=jobmon_resume, seconds_until_timeout=86400)

message("workflow status: ", wfr_status)
