####################################################################################################
## Description: Generate a time stamp for a model run based on current time.
##
## Inputs:      None
##
## Outputs:     Formatted time stamp (character)
####################################################################################################

#### Format a time stamp for run dates
make_time_stamp <- function() {
  current_time <- Sys.time()
  formatted_time <- format(current_time, "%Y_%m_%d_%H_%M_%S")
  return(formatted_time)
}
