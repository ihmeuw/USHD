#' @title submit_all_aggregation
#' @description A function version of the _submit_all_cause.r script used specifically for aggregation in the post-estimation process. Calls "_submit_single_cause.r" for each cause of death. 
#'
#' @param jobmon_workflow a jobmon workflow object created with jobmonr::workflow. The
#' jobmon tasks are bound to this object which is used to construct a dag and launch the jobs.
#' @param jobmon_tool a jobmon tool object created with jobmonr::tool.
#' @param dir character -- home directory for settings and final output
#' @param type character -- what type of run is this? options:
#'                "post_raking" -- the same as "post_models", but using raked area-year-sex-race
#'                    ASMRs instead of directly estimated ASMRs.
#'                "agg_raked_yll" -- After raking YLLs specifically, run just the aggregation
#'                    steps in post-processing on these raked YLLs
#' @param testing logical -- is this a testing job? if T, logs all output.
#' @param project character -- which project to launch qsubs on. 
#' @param queue character -- which queue to launch qsubs on.
#' @param skip_mx_aggregation logical -- skip mx aggregation? Default F
#' @param skip_yll_aggregation logical -- skip yll aggregation? Default F
#' @param children_of character -- If not NULL, subset causes to the child-causes of "children_of".
#' 
#' @return A data.table containing the job_ids of all downstream jobs launched by submit_single_cause
submit_all_aggregation <- function(jobmon_workflow,
                                   jobmon_tool,
                                   dir,
                                   type,
                                   testing,
                                   project,
                                   queue,
                                   skip_mx_aggregation = F,
                                   skip_yll_aggregation = F, 
                                   skip_yld_aggregation = F,
                                   initial_level = NULL, 
                                   terminal_level = NULL,
                                   children_of = NULL) {
  
  stopifnot(type %in% c("post_raking", "agg_raked_yll"))

  validate_cause_subset_args(children_of, initial_level, terminal_level) 

  message("\nSettings passed to submit_all_aggregation:")
  arguments <- c("dir", "type", "testing", "project", "queue", "skip_mx_aggregation", "skip_yll_aggregation", "skip_yld_aggregation",
                "initial_level", "terminal_level", "children_of")
  for (arg in arguments) {
    message(paste0(arg, ": ", get(arg)))
  }
  
  causes <- ModelSettings$get_cause_datatable(dir, use.csv = TRUE)
  
  # Subset causes for aggregation to compile_model == TRUE
  causes <- annotate_post_model_workflow(causes)
  causes <- causes[compile_model == TRUE]
  
  setkey(causes, "cause_id")
  
  # Subset to only jobs that need to be aggregated 
  causes = causes[aggregate_model == TRUE]

  # Subset causes using initial_level and terminal_level, if specified 
  if (is.null(initial_level)) initial_level = min(causes$level)
  if (is.null(terminal_level)) terminal_level = max(causes$level)
  causes = causes[level %in% initial_level:terminal_level]
  
  # If 'children_of' is specified, subset down to children of this parent
  # (Aggregation is run at the level of each cause).
  if (!is.null(children_of)){
    message(paste0("children_of was passed. Launching jobs only for  children of :", children_of))
    if (!children_of %in% unique(causes$acause)){
      stop_or_quit(paste0("children_of argument is invalid! Was passed: ", children_of))
    }
    
    parent_id_cause <- causes[acause == children_of, cause_id]
    causes <- causes[parent_id == parent_id_cause]
  }
  
  pred_jids <- NULL
  jobmon_workflow_holder <- jobmon_workflow

  if (! "_all" %in% causes$acause) {
    # initial/terminal level excludes "_all" cause
    # proceed assuming that all life tables have been generated
    lt_jids <- NULL
  }  
  
  for (this_cause in causes[order(level, cause_id), cause_id]) {

    cause_name <- causes[J(this_cause), acause]

    message("\nCalling submit_single_cause for ", cause_name)

    output <- submit_single_aggregation(jobmon_workflow = jobmon_workflow_holder,
                                        jobmon_tool = jobmon_tool,
                                        dir = paste0(dir, "/", cause_name, "/"),
                                        lt = cause_name == "_all" & type != "agg_raked_yll",
                                        type = type,
                                        testing = testing,
                                        queue = queue,
                                        skip_mx_aggregation = skip_mx_aggregation,
                                        skip_yll_aggregation = skip_yll_aggregation,
                                        skip_yld_aggregation = skip_yld_aggregation,
                                        lt_hold = if (cause_name == "_all" | skip_mx_aggregation) NULL else lt_jids)

    jobmon_workflow_holder <- output$bound_workflow

    # For the _all cause mx aggregation, save the life table jobmon tasks
    # these are passed to the rest of the causes during mx aggregation
    # to hold on in submit_single_cause
    if ((!skip_mx_aggregation) & (cause_name == "_all")) {
      lt_jids <- output$lt_hold
    }

  }
  
  return(jobmon_workflow_holder)
}
