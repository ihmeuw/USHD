####################################################################################################
## Description: Set the queue for job submission based on which queue is least busy.
##
## Inputs:      fthread [numeric]       threads (uses "-c" flag in Slurm)
##              m_mem_free [character]  maximum memory (eg, "1G") (set with "--mem==" in Slurm)
##              h_rt [character]        maximum runtime ("DD-HH:MM:SS" format) (uses "-t" flag in Slurm)
##              archive [logical]       does this job need archive access? (uses "-C archive" in Slurm)
##              priority [character]    prioritize "RAM" or "CPU" availability?
##
## Outputs:     The name of the least busy queue.
####################################################################################################

set_queue_dynamically <- function(queue = ..., num_jobs = 1, fthread = 1, m_mem_free = "2G", h_rt = "1-0:00:00", archive = TRUE, priority = "RAM", return_alternate = NULL) {
  library(stringr)
  
  #### Check whether auto queue is requested; otherwise return the specified queue
  if (queue != "auto") {
    return(queue)
  }
  
  runtime_days <- as.integer(substr(h_rt, 0, unlist(gregexpr("-", h_rt)) - 1)) + 
    as.integer(substr(h_rt, unlist(gregexpr("-", h_rt)) + 1, unlist(gregexpr(":", h_rt))[1] - 1)) / 24 + 
    as.integer(substr(h_rt, unlist(gregexpr(":", h_rt))[1] + 1, unlist(gregexpr(":", h_rt))[2] - 1)) / (24 * 60) +
    as.integer(substr(h_rt, unlist(gregexpr(":", h_rt))[2] + 1, nchar(h_rt))) / (24 * 60 * 60)
  
  if (runtime_days > 3) {
    message("Requested run time is greater than 3 days; must use ...")
    return(...)
  }
  
  #### Get system load
  system_load <- system('sinfo -o "%.27n %.16T %.12P %.13C %.9e %.9m %.5O"', intern = TRUE)
  
  #### Drop header row
  system_length <- length(system_load) - 1
  
  #### Convert to data table
  system_table <- data.table("HOSTNAMES" = character(), "STATE" = character(), "PARTITION" = character(), "CPUS_A_I_O_T" = character(), "FREE_MEM" = character(), "MEMORY" = character(), "CPU_L" = character())
  for (i in 2:(system_length + 1)) {
    temp <- unlist(strsplit(system_load[i], split = " "))
    temp <- t(as.data.table(temp[which(temp != "")]))
    system_table <- rbind(system_table, temp, use.names = FALSE)
  }
  
  #### Restrict to archive nodes if archive is requested
  if (archive) {
    message("Restricting to archive nodes")
    system_table <- system_table[HOSTNAMES %like% "archive"]
  }
  
  #### Extract total and available CPUs from each node
  for (i in 1:nrow(system_table)) {
    available <- as.integer(strsplit(system_table[i, CPUS_A_I_O_T], "/")[[1]][2])
    total <- as.integer(strsplit(system_table[i, CPUS_A_I_O_T], "/")[[1]][4])
    system_table[i, available_cpus := available]
    system_table[i, total_cpus := total]
  }
  
  #### Drop ... and ...
  system_table <- system_table[!(PARTITION %in% c(...))]
  
  #### Drop nodes that are drained or draining
  system_table <- system_table[!(STATE %in% c("drained", "draining", "drained*"))]
  
  #### Drop nodes that are reserved
  system_table <- system_table[!(STATE %in% c("resv", "reserved"))]
  
  #### Drop nodes that are fully allocated
  system_table <- system_table[!(STATE %in% c("allocated"))]
  
  #### Check whether there are enough idle nodes in either queue for all jobs
  ## Note that we only consider nodes whose total CPU and RAM exceed our request
  idle_available <- system_table[STATE == "idle" & PARTITION %in% c(...) & as.integer(available_cpus) >= as.integer(fthread) & as.integer(FREE_MEM) >= as.integer(str_replace(m_mem_free, "G", "")) * 1024, list(available_cpus = sum(available_cpus), free_mem = sum(as.integer(FREE_MEM)), .N), by = PARTITION]
  
  message("Summarizing idle nodes")
  message(idle_available)
  
  #### If there are any idle nodes in a given queue that meet our criteria, return the queue with the highest count of appropriate nodes
  # idle_available <- idle_available[available_cpus >= num_jobs * as.integer(fthread) & free_mem > num_jobs * as.integer(str_replace(m_mem_free, "G", "")) * 1024]
  return_queue <- idle_available[order(-N)][1, PARTITION]
  if (!is.null(return_queue) & !is.na(return_queue)) {
    if(return_queue == ...){
      return_queue <- ...
    }
    message(paste0("Submitting to ", return_queue, " based on count of suitable idle nodes"))
    return(return_queue)
  }
  
  #### Check whether suitable nodes in either queue are sufficient for all jobs
  ## Note that we only consider nodes whose total CPU and RAM exceed our request
  queue_available <- system_table[PARTITION %in% c(...) & as.integer(available_cpus) >= as.integer(fthread) & as.integer(FREE_MEM) >= as.integer(str_replace(m_mem_free, "G", "")) * 1024, list(available_cpus = sum(available_cpus), free_mem = sum(as.integer(FREE_MEM)), .N), by = PARTITION]
  
  message("Summarizing available nodes")
  message(queue_available)
  
  #### If we request a queue other than the specified queue, and it has sufficient resources, return it
  if (!is.null(return_alternate)) {
    if (return_alternate == ... & ... %in% unique(queue_available$PARTITION)) {
      message("Returning ... per request for alternate queue")
      return(...)
    } else if ((return_alternate == ... & (... %in% unique(queue_available$PARTITION) | (... %in% unique(queue_available$PARTITION))))) {
      message("Returning ... per request for alternate queue")
      return(...)
    }
  }
  
  #### If there are available nodes that meet our criteria, return the queue with the highest count of appropriate nodes
  return_queue <- queue_available[order(-N)][1, PARTITION]
  if (!is.null(return_queue) & !is.na(return_queue)) {
    if (return_queue == ...) {
      return_queue <- ...
    }
    message(paste0("Submitting to ", return_queue, " based on count of all suitable nodes"))
    return(return_queue)
  }
  
  ###### Otherwise, determine queue by available total CPU or RAM availability
  #### Calculate total CPU and RAM availability by queue
  ## Note that we only consider nodes whose total CPU and RAM exceed or match our request
  queue_totals <- system_table[PARTITION %in% c(...) & as.integer(total_cpus) >= as.integer(fthread) & as.integer(MEMORY) >= as.integer(str_replace(m_mem_free, "G", "")) * 1024, list(available_cpus = sum(available_cpus), free_mem = sum(as.integer(FREE_MEM))), by = PARTITION]
  
  message("Summarizing total resource availability")
  message(queue_totals)
  
  ## Return the queue with the greatest availability of the priority resource
  if (priority == "RAM") {
    return_queue <- queue_totals[order(-free_mem)][1, PARTITION]
  } else {
    return_queue <- queue_totals[order(-available_cpus)][1, PARTITION]
  }
  if (return_queue == ...) {
    return_queue <- ...
  }
  
  if (!is.null(return_queue) & !is.na(return_queue)) {
    message(paste0("Submitting to ", return_queue, " based on total resource availability"))
    return(return_queue)
  } else {
    message(paste0("No suitable node(s) with resource availability found; defaulting to ..."))
    return(...)
  }
}