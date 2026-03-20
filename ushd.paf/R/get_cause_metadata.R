#' @title ushd.get_cause_metadata
#' 
#' @description Pulls cause hierarchy for USHD PAF code. 
#' 
#' @param in_dir Root directory of PAF model. Should contain a file 'submitted_cause_list.csv'
#' @return Cause list dataframe
ushd.get_cause_metadata <- function(in_dir){
  verify_cause_list_exists(in_dir)
  cause_list = file.path(in_dir, 'submitted_cause_list.csv')
  cause_list <- fread(cause_list)
  cause_list[!cause_id %in% cause_list$parent_id, most_detailed := 1]
  
  # Subset down to columns that are present in central function get_cause_metadata
  # This drops sexes, ages, run_model, covars, covars_as, covars_trans, model
  cause_list = cause_list[, .(level, cause_id, cause_outline, acause, parent_id, path_to_top_parent, most_detailed)]
  return(cause_list) 
}


#' @title save_cause_metadata
#' 
#' @description Given a fatal model, saves a copy of its cause list to a run directory.
#' 
#' @param mortality_model [str] A fatal model directory.
#' @param run_directory [str] Directory where PAF compile outputs will be saved.
save_cause_metadata <- function(mortality_model, run_directory){
  verify_cause_list_exists(mortality_model)
  file.copy(file.path(mortality_model, "submitted_cause_list.csv"), run_directory)
}

#' @description Helper function to determine if submitted_cause_list.csv exists at path provided
verify_cause_list_exists <- function(in_dir){
  cause_list = file.path(in_dir, 'submitted_cause_list.csv')
  if (!file.exists(cause_list)){
    stop(sprintf("No cause list found at %s.", in_dir))
  }
}