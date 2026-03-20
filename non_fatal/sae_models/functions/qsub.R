####################################################################################################
## Description: Define a function for submitting jobs to the cluster
##
## Inputs:      code [character]        filepath to the code to run
##              name [character]        the jobn
##              arguments [vector]      a list of arguments to pass
##              hold [numeric vector]   a list of job IDs to hold on
##              queue [character]       queue for submitting jobs
##              fthread [numeric]       threads
##              m_mem_free [character]  maximum memory (eg, "1G")
##              h_rt [character]        maximum runtime ("HH:MM:SS" format)
##              archive [logical]       does this job need archive access?
##              mkl_threads [numeric]   number of MKL threads
##              omp_threads [numeric]   number of OMP threads
##              project [character]     cluster project name
##              sgeoutput [character]   file path where sgeoutput files should be written (if
##                                      NULL then no sgeoutput files are written)
##              shell [character]       R shell script to use for this job. This is assumed to
##                                      launch R in a singularity image.
##              sing_image              R singularity image, defaults to latest if not specified
##              skip_if_exists          file path(s) to check for output file. If the output file(s)
##                                      are found no job will be submitted
##              submit                  Whether or not the job is actually submitted. Set to FALSE
##                                      for manual checking that the qsub is well-formed. Default TRUE
##
## Outputs:     the job ID assigned to the submitted job
####################################################################################################

qsub <- function(code, name = NULL, arguments = NULL, hold = NULL, queue = "QUEUE",
                 fthread = 1, m_mem_free = "2G", h_rt = "24:00:00", archive = T, intel = F,
                 mkl_threads = NULL, omp_threads = NULL, project = "PROJECT", sgeoutput = NULL,
                 shell = "FILEPATH",
                 priority = NULL,
                 sing_image = "default",
                 skip_if_exists = NULL,
                 submit = T) {

  # check for skip_if_exists file (if not NULL)
  if (!is.null(skip_if_exists)) {
    if (sum(!file.exists(skip_if_exists)) == 0) {
      return(1) # "1" here is a placeholder and signifies no job submitted
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

  # construct and submit qsub command and return the job ID
  x <- paste("qsub -cwd -q", queue,
             "-N", name,
             paste0("-l fthread=", fthread),
             paste0("-l m_mem_free=", m_mem_free),
             paste0("-l h_rt=", h_rt),
             if (archive) "-l archive",
             if (intel) "-l intel",
             "-P", project,
             paste0("-v sing_image=", sing_image),
             if (!is.null(mkl_threads)) paste0("-v SET_MKL_THREADS=", mkl_threads),
             if (!is.null(omp_threads)) paste0("-v SET_OMP_THREADS=", omp_threads),
             if (!is.null(hold)) paste("-hold_jid", hold),
             # write output to job.output.dir; merge stderr (-e) into stdout (-o)
             if (!is.null(job.output.dir)) paste("-o", job.output.dir, "-j yes"),
             if (!is.null(priority)) paste("-p ", priority),
             shell,
             code,
             if (!is.null(arguments)) arguments)
  x <- gsub("[[:blank:]]+", " ", x) # strip excess white space from if() statements above
  
  if (!submit) {
    return(x)
  }
  
  print(x)
  id <- system(x, intern = T)
  return(as.numeric(strsplit(id, " ")[[1]][3]))
}
