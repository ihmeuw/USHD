####################################################################################################
## Description: Wrapper script to launch "submit_all_aggregation" for the postestimation process.
##                Waits for all aggregation jobs to finish before exiting the script
##
## Passed args: dir [character] -- Model run dir (separated by race/ethnicity)
##              type [character] -- what type of run is this? options:
##                "post_raking" -- the same as "post_models", but using raked area-year-sex-race
##                    ASMRs instead of directly estimated ASMRs.
##                "agg_raked_yll" -- After raking YLLs specifically, run just the aggregation steps
##                    in post-processing on these raked YLLs
##              resub [logical] -- should jobs where the relevant output file already exists be
##                skipped?
##              testing [logical] -- is this a testing job? if T, logs all output.
##              project [character] -- which project to use on the cluster, e.g. "PROJECT"
##              queue [character] -- which queue to use on the cluster, e.g. "QUEUE"
##              aggregation skip flags [character] -- "--skip_mx_aggregation" or "--skip_yll_aggregation"
##
## Outputs:     submitted jobs for all processes required to aggregate and compile
##                mx, ylls, and (if lt == T) life tables.
##
####################################################################################################

stopifnot(grepl("mortality/sae_models$", getwd()))

suppressWarnings(suppressMessages({
  library(R.utils)
  library(data.table)
  library(TMB)
  sourceDirectory("functions/", modifiedOnly=FALSE)
}))

library(jobmonr)

## Get and check settings --------------------------------------------------------------------------
# passed arguments
parser <- argparse::ArgumentParser()
parser$add_argument("--dir", type = "character", help = "model directory to aggregate")
parser$add_argument("--type", choices = c("post_raking", "agg_raked_yll"), help = "Determine type of run")
parser$add_argument("--testing", choices = c("TRUE", "FALSE"), default = TRUE, help = "Set as testing job. Log all output")
parser$add_argument("--jobmon_resume", default = F)
parser$add_argument("--project", default = "PROJECT")
parser$add_argument("--queue", default = "QUEUE")
parser$add_argument("--children_of", type="character", default=NULL, help = "Parent cause to subset to. Can be helpful for rakes over many levels of the cause hierarchy.")
add_aggregation_skip_flags(parser)
add_raking_initial_terminal_level(parser)

args <- parser$parse_args(get_args())
for (key in names(args)) {
  assign(key, args[[key]])
}

logical_args = c('testing', 'jobmon_resume')
for (arg in logical_args){
  assign(arg, as.logical(get(arg)))
}

print("Arguments passed were:")
for (arg in names(args)){
  print(paste0(arg, ": ", get(arg)))
}

if (is.null(children_of)) {
  wfname_suffix <- paste0("lvl_", initial_level, "_", terminal_level)
} else {
  initial_level = NULL
  terminal_level = NULL
  wfname_suffix <- paste0("children_of_", children_of)
}

validate_cause_subset_args(children_of, initial_level, terminal_level)

errored <- FALSE
measures = c(if (!skip_mx_aggregation) "mx", if (!skip_yll_aggregation) "yll")
for (measure in measures){
  # Load in jobmon tool, workflow, and task templates
  jobmon_tool <- jobmonr::tool(name='USHD_postmodeling')
  jobmon_workflow_name <- get_jobmon_workflow_name(jobmon_resume = jobmon_resume,
                                                   measure = measure,
                                                   type = "aggregation",
                                                   from_dir = dir,
                                                   initial_level = initial_level,
                                                   terminal_level = terminal_level,
                                                   children_of = children_of)

  workflow_label = ifelse(skip_mx_aggregation,
                          paste0('yll_aggregation_', wfname_suffix),
                          paste0('mx_aggregation_', wfname_suffix))
  jobmon_workflow <- jobmonr::workflow(tool = jobmon_tool,
                                      workflow_args = jobmon_workflow_name,
                                      default_compute_resources_set = list(
                                        "slurm" = list(queue = queue, 'project' = project,
                                                       stderr=paste0(dir, "/job-output"),
                                                       stdout=paste0(dir, "/job-output")
                                        )
                                      ),
                                      default_cluster_name = "slurm",
                                      name = workflow_label)

  # make flat file directory for storing jobmon workflow info if it doesn't exist
  make_output_dir(paste0(dir, "/jobmon_ids"))

  # save out workflow name for resumes
  jobmon_ids <- data.table("workflow_name" = jobmon_workflow_name, "workflow_id" = NA_integer_, workflow_run_id = NA_integer_, "resume" = jobmon_resume)
  jobmon_id_path = get_jobmon_csv_filepath(measure, "aggregation", dir, initial_level, terminal_level, children_of)

  # run function that will launch post-estimation for mortality rates + YLLs
  workflow <- submit_all_aggregation(jobmon_workflow = jobmon_workflow,
                                     jobmon_tool = jobmon_tool,
                                     dir = dir,
                                     type = type,
                                     testing = testing,
                                     queue = queue,
                                     skip_mx_aggregation = skip_mx_aggregation,
                                     skip_yll_aggregation = skip_yll_aggregation,
                                     skip_yld_aggregation = skip_yld_aggregation,
                                     skip_pred_aggregation = skip_pred_aggregation,
                                     children_of = children_of,
                                     initial_level = initial_level,
                                     terminal_level = terminal_level)

  # Bind workflow to get workflow ID
  workflow$bind()

  jobmon_ids[.N, `:=` (workflow_id = workflow$workflow_id, workflow_run_id = NA)]
  write_csv(jobmon_ids, jobmon_id_path, update = T, row.names = F, lock = T)

  # Wait for submit_all_causes to finish
  wfr_status <- jobmonr::run(workflow, resume=TRUE, seconds_until_timeout=86400)
  message("workflow status: ", wfr_status)
  errored <- any(errored, wfr_status != "D")

}

message("Done")

