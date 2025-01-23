# Read in the misclassification draws and write them to the directory of interest.
# Used in sae_models/tmb_models/prep_inputs.r
# Only works for 100 or 1000 draws
## They are a combination of the total, age/sex, region, and coethnic concentration misclassification draws

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
  file.copy(filepath, paste0(dir,"/FILEPATH"))
  message("Done.")

  return(expanded)
}
