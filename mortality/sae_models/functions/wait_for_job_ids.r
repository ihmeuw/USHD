#----------------------------------------
# PURPOSE: Function to hold until a set of job IDs finishes 
# LAST UPDATED: October 2020 
#----------------------------------------

#' Wait for scheduler to finish any running jobs with job_number in job_ids
#'
#' Waits for the scheduler to finish jobs by sleeping repeatedly until all jobs are completed.
#'
#' @param job_ids numeric vector of jobs to wait for
#' @param interval numeric number of seconds to wait between checks. Default 30
wait_for_job_ids = function(job_ids, interval = 30){
    Sys.sleep(interval) 
    jid_str <- paste(job_ids, collapse=",")
    # This bash command checks qstat for the given job ID, but pipes std err 
    # to /dev/null so it will be ignored. 
    cmd = paste0("qstat -j ", jid_str ," 2>/dev/null | grep '^job_number:' | awk '{print $2}'")
    status = system(SYSTEM_COMMAND)
    while (length(status) != 0) { # running jobs returned; empty output means all jobs done
        Sys.sleep(interval)
        status = system(SYSTEM_COMMAND)
    }
}
