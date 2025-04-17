####################################################################################################
## Description: Script to submit all yld-raking jobs for a particular model run.
##
##              Specifically:
##              1. submit jobs to run "yld_raking_prototype.r" for all_cause yld (by year, sex)
##
## Passed args: from_dir [character] -- Model run dir separated by race/ethnicity
##              to_dir [character] --  Model run dir for all races (NOT separated by race/ethnicity)
##              from_geo [character] -- Geography level of the lower model.
##              to_geo [character] -- Geography level of the upper model.
##              jobmon_resume [logical] -- When set to TRUE, resumes with previously submitted workflow_ID.
##              fail_fast [logical] -- When set to TRUE, jobmon will stop submitting new jobs when
##                  current workflow jobs have failed for maximum retries.
##
####################################################################################################

setwd("FILEPATH")
stopifnot(grepl("sae_models$", getwd()))

# Loads the sae.shared package
source("functions/load_sae_shared.R")

suppressMessages (suppressWarnings({
  library(stringr)
  library(jobmonr)
}))


if (!exists('jobmon_client')) {
  detach("package:jobmonr", unload = TRUE)
  library(jobmonr)
}

## Get settings ------------------------------------------------------------------------------------
if (interactive()) {
  from_dir <- "FILEPATH"
  to_dir <- "FILEPATH"
  project <- "PROJECT"
  queue <- "..."
  from_geo <- "mcnty"
  to_geo <- "state"
  jobmon_resume <- FALSE
  fail_fast <- TRUE
} else {
  parser <- argparse::ArgumentParser()
  parser$add_argument("--from_dir", help = "Model run dir separated by race/ethnicity")
  parser$add_argument("--to_dir", help = "Model run dir for all races (NOT separated by race/ethnicity)")
  parser$add_argument("--project", default = "PROJECT")
  parser$add_argument("--queue", default = "...")
  parser$add_argument("--from_geo",
                      type = "character",
                      help="Geography level for lower model.", choices=geo_choices)
  parser$add_argument("--to_geo",
                      type = "character",
                      help="Geography level for upper model.", choices=geo_choices)
  parser$add_argument("--jobmon_resume", choices = c("TRUE", "FALSE"), default=F)
  parser$add_argument("--fail_fast", choices = c("TRUE", "FALSE"), default=F)

  args <- parser$parse_args(get_args())
  for (key in names(args)) {
    assign(key, args[[key]])
  }

  logical_args = c('jobmon_resume', 'fail_fast')
  for (arg in logical_args){
    assign(arg, as.logical(get(arg)))
  }

  print("Arguments passed were:")
  for (arg in names(args)){
    print(paste0(arg, ": ", get(arg)))
  }
}

measure <- "yld"
initial_level <- 0
terminal_level <- 0
children_of <- NULL
common_raking_vars <- "year,sex,age,sim"
lower_raking_vars <- "year,sex,age,sim,mcnty,race"
upper_raking_vars <- "year,sex,age,sim,state"

# Load in jobmon tool, workflow, and task templates
jobmon_tool <- jobmonr::tool(name = "USHD_postmodeling")
jobmon_workflow_name <- get_jobmon_workflow_name(jobmon_resume = jobmon_resume,
                                                 measure = measure,
                                                 type = "raking",
                                                 from_dir = from_dir,
                                                 initial_level = initial_level,
                                                 terminal_level = terminal_level,
                                                 children_of = children_of)

wfname <- paste0(measure, "_raking_all_cause")
jobmon_workflow <- jobmonr::workflow(tool = jobmon_tool,
                                     workflow_args=jobmon_workflow_name,
                                     default_compute_resources_set = list(
                                       "slurm" = list(queue = queue, 'project' = project,
                                                      stderr=paste0(from_dir, "/job-output"),
                                                      stdout=paste0(from_dir, "/job-output")
                                                      )
                                     ),
                                     default_cluster_name = "slurm",
                                     name = wfname)


# for now have to set the executor directly to set project
make_group_writable_dir(file.path(from_dir, "jobmon_ids"))

# save out workflow name for resumes
jobmon_ids <- data.table("workflow_name" = jobmon_workflow_name, "workflow_id" = NA_integer_, workflow_run_id = NA_integer_, "resume" = jobmon_resume)
jobmon_id_path = get_jobmon_csv_filepath(measure, "raking", from_dir, initial_level, terminal_level, children_of)

write_csv(jobmon_ids, jobmon_id_path, update = T, row.names = F)

# Loading model settings.
from_settings <- ModelSettings$from_dir(from_dir)

warning(sprintf("NOTE: all logs are being written into %s", from_dir))

if (from_settings$n.sims < 1000) {
  warning("WARNING\nn.sims is set to a value less than 1000. GBD results used in raking will be down-sampled and raked results will not match GBD results.")
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
  from_settings$ages,
  submitted_causes = from_settings$submitted_causes,
  measure = measure
)

raking_jobs = jobs_list$raking_jobs

# load task templates
flexible_raking_template <- load_jobmon_task_templates(jobmon_tool)$flexible_raking_template

## Submit raking jobs ------------------------------------------------------------------------------
# submit (one-dimensional) raking for all-cause mortality
# this is one job per year-sex-race
if (initial_level == 0) {
  if (from_settings$n.sims == 1000) {
    mem_rake_geo <- "200G"
    s_rake_geo <- 7200
  } else {
    mem_rake_geo <- "100G"
    s_rake_geo <- 3600
  }
  raking_jobs[
    level == initial_level - 1,
    `:=` (job_id = list(jobmon_add_task(jobmon_task_template = flexible_raking_template,
                                        tool = jobmon_tool,
                                        name = job_name,
                                        code = fs::path_package("sae.shared", "scripts", "flexible_raking.r"),
                                        fthread = 2, m_mem_free = mem_rake_geo, s_rt = s_rake_geo,
                                        archive = TRUE,
                                        sgeoutput = paste0(from_dir,"/all_cause/"),
                                        queue = queue,
                                        from_dir = from_dir,
                                        to_dir = to_dir,
                                        year = as.integer(year),
                                        sex = as.integer(sex),
                                        measure = measure,
                                        from_geo = from_geo,
                                        to_geo = to_geo,
                                        common_raking_vars = common_raking_vars,
                                        lower_raking_vars = lower_raking_vars,
                                        upper_raking_vars = upper_raking_vars))),
    by=c("year", "sex")]
  jobmon_workflow <- jobmonr::add_tasks(jobmon_workflow, unlist(raking_jobs$job_id))
}

wfr <- jobmonr::run(jobmon_workflow, resume=jobmon_resume, fail_fast = fail_fast, seconds_until_timeout = 9000)
message("workflow status: ", wfr$status)

# save out workflow info for jobmon db queries and resumes
jobmon_ids[.N, `:=` (workflow_id = jobmon_workflow$workflow_id, workflow_run_id = wfr$workflow_run_id)]
write.csv(jobmon_ids, file = jobmon_id_path, row.names = F)

# change group ownership to 'Domain Users'
if (to_dir != "gbd") {
  set_group_ownership(group_name = "Domain Users", dest_dir = to_dir)
}
set_group_ownership(group_name = "Domain Users", dest_dir = from_dir)

# Send email notification that job is complete
send_email(paste0("Years Lived with Disability (YLD) rate raking. Status: ", wfr_status), project, queue)
