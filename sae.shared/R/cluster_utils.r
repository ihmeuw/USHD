#' @title wait_for_job_ids
#'
#' @description Wait for scheduler to finish any running jobs with job_number in job_ids.
#'              Sleeps repeatedly until all jobs are completed.
#'
#' @param job_ids numeric vector of jobs to wait for
#' @param interval numeric number of seconds to wait between checks. Default 30
#' @return None
#'
#' @rdname wait_for_job_ids
#' @export
wait_for_job_ids <- function(job_ids, interval = 30) {
  Sys.sleep(interval) 
  jid_str <- paste(job_ids, collapse = ",")
  cmd <- paste0("qstat -j ", jid_str, " 2>FILEPATH | grep '^job_number:' | awk '{print $2}'")
  status <- system(cmd, intern = TRUE)
  while (length(status) != 0) { # running jobs returned; empty output means all jobs done
    Sys.sleep(interval)
    status <- system(cmd, intern = TRUE)
  }
}

#' @title Send email notification
#' @description Sends email notification of pipeline progress using a qsub.
#'
#' @param notification [character] Message to send in email subject.
#' @param project [character] Cluster project to send job to.
#' @param queue [character] Cluster queue to send job to.
#'
#' @rdname send_email
#' @export
send_email <- function(notification, project, queue) {
  user <- Sys.info()[["user"]]
  email_command <- paste0(
    "sbatch -A ", project, " -p ", queue, " --mem=1g -c 1",
    ' -J "', notification, '" --wrap ', '"FILEPATH -s \'', notification,'\' ', user, 'FILEPATH"'
  )
  system(email_command)
}
