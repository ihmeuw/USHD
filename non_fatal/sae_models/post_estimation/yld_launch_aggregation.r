####################################################################################################
## Description: Script to launch "submit_single_yld_aggregation" for the postestimation process.
##                Waits for all aggregation jobs to finish before exiting the script
##
## Passed args: dir [character] -- Model run dir (separated by race/ethnicity)
##              testing [logical] -- is this a testing job? if T, logs all output.
##              project [character] -- which project to use on the cluster
##              queue [character] -- which queue to use on the cluster, e.g. "..."
##
## Outputs:     submitted jobs for all processes required to aggregate and compile
##                ylds.
##
####################################################################################################

setwd("FILEPATH")
stopifnot(grepl("sae_models$", getwd()))
# Loads the sae.shared package
source("functions/load_sae_shared.R")

suppressWarnings(suppressMessages({
  library(R.utils)
  library(data.table)
  library(TMB)
  library(jobmonr)
}))

if (!exists('jobmon_client')){ #If you've cleared the global environment with rm(list=ls()), jobmon doesn't reload.
  detach("package:jobmonr", unload=T)
  library(jobmonr)
}

## Get and check settings --------------------------------------------------------------------------
# passed arguments
if (interactive()){
  dir <- "FILEPATH"
  testing <- TRUE
  project <- "PROJECT"
  queue <- "..."
  jobmon_resume <- FALSE
} else {
  parser <- argparse::ArgumentParser()
  parser$add_argument("--dir", type = "character", help = "model directory to aggregate")
  parser$add_argument("--testing",
                      choices = c("TRUE", "FALSE"),
                      default = TRUE,
                      help = "Set as testing job. Log all output")
  parser$add_argument("--jobmon_resume", default = FALSE)
  parser$add_argument("--project", default = "PROJECT")
  parser$add_argument("--queue", default = "...")

  args <- parser$parse_args(get_args())
  for (key in names(args)) {
    assign(key, args[[key]])
  }

  logical_args = c("testing", "jobmon_resume")
  for (arg in logical_args){
    assign(arg, as.logical(get(arg)))
  }

  print("Arguments passed were:")
  for (arg in names(args)){
    print(paste0(arg, ": ", get(arg)))
  }
}

## settings and validation ----------------------------------------
measure <- "yld"
initial_level <- 0
terminal_level <- 0
children_of <- NULL

# Load model settings
model_settings <- ModelSettings$from_dir(dir)
submitted_causes <- model_settings$submitted_causes

# assemble all causes in data.table
causes <- data.table()
for (cause in names(submitted_causes)) {
  causes <- rbind(causes, c("acause" = cause, submitted_causes[[cause]]), fill = TRUE)
}

# Subset causes for aggregation to compile_model == TRUE
causes <- annotate_post_model_workflow(causes)
causes <- causes[compile_model == TRUE]
setkey(causes, "acause")

# Subset further to only all_cause
cause_name <- "all_cause"
if (!"all_cause" %in% causes[, acause]) {
  msg <- paste0("all_cause not present in causes. Check settings and/or results of annotate_post_model_workflow().")
  stop_or_quit(msg, status = 1)
}
if (causes[J(cause_name), run_model == 0]) {
  msg <- paste0("run_model for ", cause_name, " is set to 0. It won't be aggregated.")
  stop_or_quit(msg, status = 1)
}

# Load in jobmon tool, workflow, and task templates
jobmon_tool <- jobmonr::tool(name="USHD_postmodeling")
jobmon_workflow_name <- get_jobmon_workflow_name(jobmon_resume = jobmon_resume, 
                                                 measure = measure, 
                                                 type = "aggregation", 
                                                 from_dir = dir,
                                                 initial_level = initial_level,
                                                 terminal_level = terminal_level,
                                                 children_of = children_of)

workflow_label = paste0("yld_agg_all_cause")
jobmon_workflow <- jobmonr::workflow(tool = jobmon_tool, 
                                     workflow_args = jobmon_workflow_name, 
                                     name = workflow_label,
                                     default_compute_resources_set = list(
                                       "slurm" = list(queue = queue, 'project' = project,
                                                      stderr=paste0(dir, "/job-output"),
                                                      stdout=paste0(dir, "/job-output")
                                       )
                                     ),
                                     default_cluster_name = "slurm")

# make flat file directory for storing jobmon workflow info if it doesn't exist
make_group_writable_dir(paste0(dir, "/jobmon_ids"))

# save out workflow name for resumes
jobmon_ids <- data.table("workflow_name" = jobmon_workflow_name, "workflow_id" = NA_integer_, workflow_run_id = NA_integer_, "resume" = jobmon_resume)
jobmon_id_path = get_jobmon_csv_filepath(measure, "aggregation", dir, initial_level, terminal_level, children_of)
write_csv(jobmon_ids, jobmon_id_path, update = T, row.names = F)

# bound workflow to workflow tasks for each cause
message("\nCalling submit_single_yld_aggregation for ", cause_name)

output <- submit_single_yld_aggregation(jobmon_workflow = jobmon_workflow,
                                        jobmon_tool = jobmon_tool,
                                        dir = paste0(dir, "/", cause_name, "/"),
                                        testing = testing,
                                        model_settings = model_settings,
                                        project = project,
                                        queue = queue)

jobmon_workflow <- output$bound_workflow

# Wait for submit_all_causes to finish
wfr <- jobmonr::run(jobmon_workflow, resume=TRUE, seconds_until_timeout=86400)

# save out workflow info for jobmon db queries and resumes
jobmon_ids[.N, `:=` (workflow_id = jobmon_workflow$workflow_id, workflow_run_id = wfr$workflow_run_id)]
write_csv(jobmon_ids, jobmon_id_path, update = T, row.names = F)

# change group ownership to 'Domain Users'
set_group_ownership(group_name = "Domain Users", dest_dir = dir)

# Send email notification that job is complete 
send_email("yld rate aggregation", project, queue)
