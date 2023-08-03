# Read in the misclassification draws and write them to the directory of interest.

prep_misclass <- function(n.sims, dir, filepath = "FILEPATH") {
  message(glue::glue("Using: {filepath}"))
  expanded <- readRDS(filepath)

  if (max(expanded$sim) < n.sims) {
    stop("The misclassification file has less draws than the number of specified draws")
  }
  if (max(expanded$sim) != n.sims) {
    warning(paste0("The misclassification ratios have a different number of draws in them than those specified by the user; the script will just take the first ", n.sims))
  }

  message("Copying file..")
  file.copy(filepath, paste0(dir,"/misclassification_draws.rds"))
  message("Done.")

  return(expanded)
}
