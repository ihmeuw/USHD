####################################################################################################
## Description: Generate a time stamp for a model run based on current time.
##
## Inputs:      None
##
## Outputs:     Formatted time stamp (character)
####################################################################################################

#### Format a time stamp for run dates
make_time_stamp <- function() {
  run_date <- gsub("-", "_", Sys.time())
  run_date <- gsub(":", "_", run_date)
  run_date <- gsub(" ", "_", run_date)
  
  return(run_date)
}