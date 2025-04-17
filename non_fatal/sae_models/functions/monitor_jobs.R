monitor_jobs <- function(sleep_schedule = 30, max_sleep = 1200, jids, queue, fthread, m_mem_free, h_rt, archive, priority, return_alternate_if_available = TRUE, hold = FALSE, array_jobs_break = TRUE, maxit = NULL) {
  library(stringr)
  iter <- 0
  sleep_schedule <- 30
  flag <- 0
  flag_running <- 0
  jids <- unique(jids)
  message("Monitoring submitted jobs...")
  
  while (flag == 0 & (is.null(maxit))) {
    if (!is.null(maxit)) {
      if (iter >= maxit) {
        break
      }
    }
    iter <- iter + 1
    print(paste0("Iteration ", iter))
    Sys.sleep(sleep_schedule)
    
    stats <- fread(text = system(paste0("squeue --me --format \"%A,%F,%j,%t,%K,%r,%S\""), intern = TRUE))
    job_stats <- stats[JOBID %in% jids]
    if (array_jobs_break & nrow(job_stats[ST != "R" & REASON != "JobArrayTaskLimit"]) == 0 & !hold) {
      return("All jobs are running... moving forward as you requested to not wait for jobs to finish")
    } else if (!array_jobs_break & nrow(job_stats[ST != "R"]) == 0 & !hold) {
      return("All jobs are running... moving forward as you requested not to wait for jobs to finish")
    } else if (nrow(job_stats) == 0) {
      flag <- 1
    } else {
      if (flag_running == 0) {
        if (array_jobs_break) {
          stats_running <- job_stats[ST == "R" | REASON == "JobArrayTaskLimit"]
          job_stats <- job_stats[!(ST == "R" | REASON == "JobArrayTaskLimit")]
        } else {
          stats_running <- job_stats[ST == "R"]
          job_stats <- job_stats[ST != "R"]
        }
        
        stats_PD <- job_stats[ST %in% c("PD") & REASON %in% c("Priority")]
        if (nrow(stats_running) > 0 & nrow(stats_PD) == 0) {
          message("All jobs are running... waiting for jobs to finish, per request")
          flag_running <- 1
        } else if (nrow(stats_running) == 0 & nrow(stats_PD) > 0) {
          if (return_alternate_if_available) {
            auto_queue_new <- suppressMessages(invisible(set_queue_dynamically(queue = queue, fthread = fthread, m_mem_free = m_mem_free, h_rt = h_rt, archive = archive, priority = priority, return_alternate = auto_queue)))
          } else {
            auto_queue_new <- suppressMessages(invisible(set_queue_dynamically(queue = queue, fthread = fthread, m_mem_free = m_mem_free, h_rt = h_rt, archive = archive, priority = priority)))
          }
          message("Checking pending jobs...")
          
          if (nrow(stats_running) == 0 & ((auto_queue_new != auto_queue))) {
            message(paste0("Updating queue for ", nrow(stats_PD), " jobs..."))
            for (i in 1:nrow(stats_PD)) {
              switch_queue <- fread(text = system(paste0("scontrol update Partition=", auto_queue_new, " JobId=", stats_PD[i, JOBID]), intern = TRUE))
            }
            auto_queue <- auto_queue_new
          }
        }
      }
      
      sleep_schedule <- min(sleep_schedule * 5, max_sleep)
      message(paste0("Waiting for ", sleep_schedule), " seconds before checking again...")
    }
  }
  if (flag == 1) {
    return("Monitored jobs have completed")
  } else if (iter == maxit) {
    return("Reached maximum iterations")
  }
}
