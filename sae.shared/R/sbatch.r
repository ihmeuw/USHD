####################################################################################################
## Description: Define a function for submitting jobs to the cluster
##
##              priority [integer]      Defines the priority of the job relative to other jobs.
##
## Outputs:     the job ID assigned to the submitted job
####################################################################################################

#' @title sbatch
#'
#' @description Define a function for submitting jobs to the cluster
#'
#' @param       code [character]        filepath to the code to run
#' @param       name [character]        the job name
#' @param       arguments [vector]      a list of arguments to pass
#' @param       hold [numeric vector]   a list of job IDs to hold on
#' @param       queue [character]       queue for submitting jobs 
#' @param       fthread [numeric]       threads
#' @param       m_mem_free [character]  maximum memory (eg, "1G")
#' @param       h_rt [character]        maximum runtime ("HH:MM:SS" format)
#' @param       archive [logical]       does this job need J drive access?

#' @param       mkl_threads [numeric]   number of MKL threads
#' @param       omp_threads [numeric]   number of OMP threads
#' @param       project [character]     cluster project name.
#' @param       sgeoutput [character]   file path where sgeoutput files should be written (if
#'                                       NULL then no sgeoutput files are written)
#' @param       shell [character]       R shell script to use for this job. This is assumed to
#'                                       launch R in a singularity image.
#' @param       sing_image              R singularity image, defaults to latest if not specified
#' @param       skip_if_exists          file path(s) to check for output file. If the output file(s)
#'                                       are found no job will be submitted
#' @param       submit                  Whether or not the job is actually submitted. Set to FALSE
#'                                       for manual checking that the sbatch is well-formed. Default TRUE
#' @param       type                    Character. Either "non-TMB" or anything else.
#' @param       array                   Number of array tasks to run
#' @param       array_throttle          Maximum number of array tasks to run concurrently
#'
#' @return  the job ID assigned to the submitted job
#'
#' @rdname sbatch
#' @export
sbatch <- function(code, name = NULL, arguments = NULL, hold = NULL, queue = "QUEUE",
                   fthread = 1, m_mem_free = "2G", h_rt = "24:00:00", archive = T,
                   mkl_threads = NULL, omp_threads = NULL, project = "PROJECT", sgeoutput = NULL,
                   shell = "FILEPATH",
                   priority = NULL,
                   sing_image = "FILEPATH",
                   skip_if_exists = NULL,
                   submit = T, type = "non-TMB", array = NULL, array_throttle = 50) {

  # check for skip_if_exists file (if not NULL)
  if (!is.null(skip_if_exists)) {
    if (all(file.exists(skip_if_exists))) {
      return(1)
    }
  }

  # format job name, if you don't have a custom name
  if (is.null(name)) {
    name <- gsub(".r$", "", tail(strsplit(code, "/")[[1]], 1))
    name <- paste0(c(name, arguments[-1]), collapse = "_") # skip first argument as this is always dir
  }

  # format arguments and hold IDs
  if (!is.null(arguments)) arguments <- paste(arguments, collapse = " ")

  # handle hold cases
  # 1) hold is NULL - no hold
  # 2) hold is empty - no hold (set to NULL)
  # 3) hold is non-empty: ensure numbers are converted properly to strings; build hold_jid str
  if (!is.null(hold)) {
    if (length(hold) == 0) {
      hold <- NULL # an empty hold due to e.g., na.omit(upstream_jobs) is effectively NULL
    } else {
      if (is.numeric(hold)) {
        # sprintf("%.0f" forces full digits with no scientific notation abbreviations e.g., 6.3e+07
        hold <- sprintf("%.0f", hold)
      }
      hold <- paste(hold, collapse = ",")
    }
  }

  # make sure the sgeoutput folder exists
  if (!is.null(sgeoutput)) {
    if (!file.exists(sgeoutput)) {
      sgeoutput <- NULL
    }
  }

  # make error and output directories
  job.output.dir <- if (!is.null(sgeoutput)) file.path(sgeoutput, "job-output")
  if (!is.null(job.output.dir) && !dir.exists(job.output.dir)) dir.create(job.output.dir)

  had.multi.output.dirs <- length(job.output.dir) > 1
  if (had.multi.output.dirs) {
    message(arguments)
    stop("job has multiple output_dirs - is your 'by' insane?")
  }

  if (!is.null(job.output.dir) && !dir.exists(job.output.dir)) {
    for (jod in job.output.dir) {
      dir.create(jod)
    }
  }

  # construct and submit sbatch command and return the job ID
  if (type == "non-TMB") {
    x <- paste(
      "sbatch -p", queue,
      "-J", name,
      paste0("-c ", fthread),
      paste0("--mem=", m_mem_free),
      paste0("-t ", h_rt),
      if (archive) "-C archive",
      "-A", project,
      if (!is.null(hold)) paste0(" --dependency=afterok:", hold, " "),
      if (!is.null(job.output.dir)) paste0("-o ", job.output.dir, "/'%x.o%j'"),
      if (!is.null(array)) paste0(" --array=", array, "%", array_throttle),
      paste0(" ", shell, " -i ", sing_image, " -c 0 "),
      paste0("-s ", code),
      if (!is.null(arguments)) arguments
    )
  } else {
    x <- paste(
      "sbatch -p", queue,
      "-J", name,
      paste0("-c ", fthread),
      paste0("--mem=", m_mem_free),
      paste0("-t ", h_rt),
      if (archive) "-C archive",
      "-A", project,
      if (!is.null(hold)) paste0(" --dependency=afterok:", hold, " "),
      if (!is.null(job.output.dir)) paste0("-o ", job.output.dir, "/'%x.o%j'"),
      if (!is.null(array)) paste0(" --array=", array, "%", array_throttle),
      paste0(" ", shell, " -s ", sing_image, " "),
      paste0("-m  ", mkl_threads, " -o ", omp_threads, ""),
      paste0("-e s ", code),
      if (!is.null(arguments)) arguments
    )
  }

  x <- gsub("[[:blank:]]+", " ", x) # strip excess white space from if() statements above

  if (!submit) {
    return(x)
  }

  id_str <- system(x, intern = T)
  id <- as.numeric(strsplit(id_str, " ")[[1]][4])
  if (length(id) == 0 || is.na(id)) {
    log <- system("mktemp -p FILEPATH", intern = TRUE)
    write(x, file = log)
    message(sprintf("FAILED AT QSUB - see %s", log))
    stop("Everything is broken")
  }
  if (had.multi.output.dirs) {
    message(sprintf("job %i had multiple output dirs - DEBUG", id))
    message(x)
  }
  return(id)
}
