# not an actual job hold, just a sys sleep until all jobs are done
# note i modified this for USHD to use job ID not job name -- this may affect the
# retry_sbatch function
library(magrittr)
job_hold <- function(job_id, cluster_name = "slurm") {
  # this function is only for the slurm cluster now and cluster_name is unused
  if (cluster_name != "slurm") stop("job_hold is supported only on the Slurm cluster.")
  
  start_time <- proc.time()
  Sys.sleep(5)
  flag <- 0
  while (flag == 0) {
    stats <- fread(text=system("squeue --me --format \"%i,%j,%t,%K\"", intern = T))
    if(!"JOBID" %in% names(stats)) print(stats)
    job_stats <- stats[JOBID %like% job_id]
    
    if (nrow(job_stats) == 0) {
      flag <- 1
    } else {
      running_tasks <- job_stats[ST == "R"]$ARRAY_TASK_ID
      array_stats <- job_stats[ST == "PD"]
      if (nrow(array_stats) == 0) {
        queued <- ""
        n_queued <- 0
      } else {
        # parse the number of queued tasks, which can look like "3-10%5" or simply "10"
        queued <- unlist(strsplit(array_stats$ARRAY_TASK_ID, "%"))[1]
        queued_tasks <- unlist(strsplit(queued, "-"))
        n_queued <- ifelse (length(queued_tasks) > 1,
                            as.integer(queued_tasks[2]) - as.integer(queued_tasks[1]) + 1,
                            1
        )
      }
      
      message(sprintf(
        "%s total job(s), %d running, %d queued",
        length(running_tasks) + n_queued,
        length(running_tasks),
        n_queued
      ))
      # for array jobs, list running tasks
      if (job_stats[1]$ARRAY_TASK_ID != "N/A") {
        message(sprintf("Tasks running: %s", running_tasks %>% as.numeric %>% sort %>% paste(., collapse=", ")))
        message(sprintf("Tasks queued: %s", queued))
      }
      Sys.sleep(60)
    }
  }
  job_runtime <- proc.time() - start_time
  job_runtime <- round(job_runtime[3] / 60, 0)
  Sys.sleep(10)
  message("Job %s has completed (elapsed time: %s minutes).", job_id, job_runtime)
}