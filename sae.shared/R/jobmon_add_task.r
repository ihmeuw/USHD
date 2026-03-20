#' @title Create a jobmon task
#' @description The jobmon equivalent of the mortality qsub function. Creates and returns a jobmon task object.
#'
#' @param jobmon_task_template Character, The name of a task template found in post_estimation/jobmon_task_templates.r
#' @param code Character, filepath to the code to run. Can be relative to the cwd
#' @param name Character, the job name
#' @param hold list, default NULL. A list of jobmon task objects to hold on. NULL if no upstream tasks. Will deduplicate the tasks and remove placeholders (1) from list.
#' @param queue character, Queue for submitting jobs
#' @param fthread integer, default 1. number of threads for parallelization
#' @param m_mem_free character, default 2G. maximum memory (eg, "1G")
#' @param s_rt integer, default 3600 (1 hour). maximum run time in seconds.
#' @param archive boolean, default T. Request node with archive access?
#' @param intel boolean, default F. Request intel node?
#' @param mkl_threads integer, default NULL. number of MKL threads
#' @param omp_threads integer, default NULL. number of OMP threads
#' @param sgeoutput string, default NULL. Filepath where sgeoutput should be written.
#' If NULL no files are written
#' @param shell string, default SciComp shell `FILEPATH`. Filepath to R shell
#' script to use for this job. This is assumed to launch R in a singularity image.
#' @param priority numeric, default NULL. Priority of the job for the scheduler
#' @param sing_image string, default "default". This arg is used within the `shell` script to determine which
#' singularity image to launch for the job.
#' @param ... all additional args will be passed as arguments to the jobmon_task object. This is equivalent to
#' the `arguments` param in the USHD `qsub` function. These arguments must exactly match the args specified in
#' the `jobmon_task_template`.
#'
#' @note integers like draws/years/sex need to be cast to int explicitly, as reticulate only considers
#' integers e.g. 1L, 2000L as ints and will cast everything else to floats.
#'
#' Similarly, boolean TRUE/FALSE cannot be used as arguments in jobmon because it gets converted to
#' python True/False before being passed back in to the script. This can be circumvented by using
#' characters and casting to logical in the downstream script or taking advantage of 0/1 == F/T and
#' passing them through as integers.
#'
#' OMP_NUM_THREADS=1 is added before invocation of FILEPATH
#'
#' @return jobmon task object
#'
#' @rdname jobmon_add_task
#' @export
jobmon_add_task <- function(jobmon_task_template,
                            tool,
                            code,
                            name,
                            hold = NULL,
                            queue = "...",
                            fthread = 1,
                            m_mem_free = "24G",
                            s_rt = 3600,
                            archive = T,
                            intel = F,
                            mkl_threads = NULL,
                            omp_threads = NULL,
                            sgeoutput = NULL,
                            shell = "OMP_NUM_THREADS=1 FILEPATH -s",
                            priority = NULL,
                            sing_image = "default",
                            ...) {
  # Filter down to the unique jobmon tasks to hold on
  hold <- get_unique_jobmon_tasks(hold)
  # upstream_tasks should be a list instead of a NULL
  if (is.null(hold)) {
    hold <- list()
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
  # modify resource list
  resources=list(
    'cores'=fthread,
    'queue'=queue,
    'runtime'=s_rt,
    'memory'=m_mem_free,
    'working_directory'=getwd()
  )
  if (archive) {
    resources$constraints <- "archive"
  }
  jobmon_task <- jobmonr::task(
    task_template = jobmon_task_template,
    compute_resources=resources,
    name = name,
    upstream_tasks = hold,
    rshell = shell,
    scriptname = code,
    ...
  )
  return(jobmon_task)
}
