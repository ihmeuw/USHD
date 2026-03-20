#' @description Creates a versioned run directory at the given root directory. 
create_run_directory <- function(root){
  # Create a new run ID. These are just incrementing integers. 
  last_version = max(c(0, as.integer(list.files(root, pattern = "\\d+"))))
  
  # Create a new directory
  run_id = last_version + 1
  run_directory = file.path(root, run_id)
  dir.create(run_directory)
  
  message(sprintf("Creating PAF compile directory at %s", run_directory))
  
  return(run_directory)
}
