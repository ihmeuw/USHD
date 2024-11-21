####################################################################################################
## Description: Helper function for qsub-ing submission scripts.
##
## Inputs:      script [character] -- the R script to be submitted. Currently implemented for
##                "_submit_single_cause.r" and "_submit_all_causes.r".
##              dir [character], lt [logical], type [character], resub [logical],
##                testing [logical] -- arguments that will be passed to the submission script
##                (as appropriate; excess arguments are ignored).
##              log [logical] -- if T, sgeoutput is routed to dir.
##
## Outputs:     submitted cluster job; returns the job ID.
##
####################################################################################################

run_submission_script <- function(script, dir = NULL, lt = NULL, type = NULL, resub = NULL,
                                  testing = NULL, initial_level = NULL, terminal_level = NULL,
                                  queue = NULL, project = NULL, priority = 0,
                                  log = F, all_cause_dependency = T) {
  
  source("functions/load_sae_shared.r")
  
  if (script == "_submit_single_cause.r") {
    if (is.null(dir) | is.null(lt) | is.null(type) | is.null(resub) | is.null(testing)) {
      stop("dir, lt, type, resub, and testing are required for _submit_single_cause.r")
    }
    
    jid <- sbatch(code = "_submit_single_cause.r",
                  name = "submit_single_cause",
                  arguments = c(dir, lt, type, resub, testing, all_cause_dependency),
                  fthread = 1,
                  m_mem_free = ifelse(resub, "64G", "100G"),
                  h_rt = "01:00:00",
                  archive = T,
                  sgeoutput = if (log) dir,
                  queue = queue) 
    
  } else if (script == "_submit_all_causes.r") {
    if (is.null(dir) | is.null(type) | is.null(resub) | is.null(testing)) {
      stop("dir, type, resub, and testing are required for _submit_all_causes.r")
    }
    
    jid <- sbatch(code = "_submit_all_causes.r",
                  name = "submit_all_causes",
                  arguments = c(dir, type, resub, testing, all_cause_dependency),
                  fthread = 1,
                  m_mem_free = "50G",
                  h_rt = "01:00:00",
                  archive = T,
                  sgeoutput = if (log) dir,
                  queue = queue)
    
  } else if (script == "_run_all_raking.r") {
    if (is.null(dir) | is.null(resub) | is.null(initial_level) | is.null(terminal_level) | is.null(testing)) {
      stop("dir, resub, initial_level, terminal_level, and testing are required for _run_all_raking.r")
    }
    
    jid <- sbatch(code = "raking/_run_all_raking.r",
                  name = "run_all_raking",
                  arguments = c(dir, resub, initial_level, terminal_level, testing),
                  fthread = 1,
                  m_mem_free = "16G",
                  h_rt = "3:00:00:00",
                  archive = T,
                  sgeoutput = if (log) dir,
                  project = project, priority = priority,
                  queue = queue)
    
  } else {
    stop(paste("Not yet implemented for:", script))
  }
  
  return(jid)
}