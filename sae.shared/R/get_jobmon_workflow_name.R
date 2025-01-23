#' @title Get jobmon csv file
#' @description builds the path to the csv file saved by a jobmon run
#'
#' @param measure String, either "mx" or "yll"
#' @param type String, either "raking" or "aggregation"
#' @param from_dir String, directory where json files are saved
#' @param initial_level Int, top level being raked
#' @param terminal_level Int, lowest level being raked
#' @param children_of String, subset of causes being raked/aggregated. Mutually exclusive with
#' initial level and terminal level, e.g. If initial and terminal level are used this must be NULL.
#'
#' @return A filepath to the jobmon json file
#'
#' @rdname get_jobmon_csv_filepath
#' @export
get_jobmon_csv_filepath <- function(measure,
                                    type,
                                    from_dir,
                                    initial_level,
                                    terminal_level,
                                    children_of) {
  validate_cause_subset_args(children_of, initial_level, terminal_level)

  if (!is.null(children_of)) {
    path <- paste0(from_dir, "FILEPATH", type, "_", measure, "_", children_of, ".csv")
  } else {
    path <- paste0(from_dir, "FILEPATH", type, "_", measure, "_", initial_level, "_", terminal_level, ".csv")
  }
  return(path)
}


#' @title Get jobmon workflow name
#' @description Gets or creates a jobmon workflow name. If wanting to resume a jobmon run, this function
#' looks for the magic csv file (e.g. jobmon_ids/raking_mx_0_0.csv) saved out by a previous run to get
#' the workflow name. Resuming a jobmon workflow requires keeping the same name.
#' If the jobmon run is new, a new workflow name will be created.
#'
#' @param jobmon_resume Boolean, is this workflow being resumed?
#' @param measure String, either "mx" or "yll"
#' @param type String, either "raking" or "aggregation"
#' @param from_dir String, directory where json files are saved
#'
#' @return A workflow name string
#'
#' @rdname get_jobmon_workflow_name
#' @export
get_jobmon_workflow_name <- function(jobmon_resume,
                                     measure,
                                     type,
                                     from_dir,
                                     initial_level,
                                     terminal_level,
                                     children_of) {
  if (jobmon_resume) {
    jobmon_id_file <- get_jobmon_csv_filepath(measure, type, from_dir, initial_level, terminal_level, children_of)
    message("Checking that jobmon id file exists - ", jobmon_id_file)
    if (file.exists(jobmon_id_file)) {
      jobmon_ids <- fread(jobmon_id_file)
      workflow_name <- jobmon_ids[.N, ]$workflow_name
    } else {
      stop("jobmon id file does not exist, did you include --jobmon_resume T by accident?")
    }
  } else {
    cause_subset <- if (!is.null(children_of)) children_of else paste(initial_level, terminal_level, sep = "_")
    workflow_name <- paste(type, measure, cause_subset, gsub("-|:| ", "_", Sys.time()), sep = "_")
  }

  message("Jobmon workflow name: ", workflow_name)
  return(workflow_name)
}
