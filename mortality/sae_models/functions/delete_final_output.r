#' If running raking with the --resub flag, delete "est_all_raked" output files. 
#' Otherwise, delete all "est_all" files, EXCEPT for raking. 
#' 
#' @param dir a run directory.
#' @param type run type. Options are models_only, post_models, post_raking, 
#' all, validation, and agg_raked_yll.
delete_final_output <- function(dir, type){
  if (type %in%c('post_raking', 'agg_raked_yll')) {
    files <- dir(dir, "est_all_raked", full.names = T)
    if (length(files) > 0) file.remove(files)
  } else {
    files <- dir(dir, "est_all", full.names = T)
    raking_files <- dir(dir, "est_all_raked", full.names=T)
    files = files[!files%in%raking_files]
    if (length(files) > 0) file.remove(files)
  }
} 