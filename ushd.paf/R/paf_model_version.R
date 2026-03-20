#' Get PAF model version info from PAF calculation dirs.
#'
#' @param dirs list of PAF calculation output directories with names corresponding to the year of data.
#'
#' @return list with data.table values for the PAF model version data and names corresponding to the year of data.
get_paf_model_versions <- function(dirs) {
  files <- file.path(dirs, "paf_model_version.csv")
  # keep year designation associated with files
  names(files) <- names(dirs)

  res <- tryCatch(
    lapply(files, data.table::fread),
    error = function(e) {
      missing.idx <- which(!file.exists(files))
      if (length(missing.idx) > 0) {
        bad.dirs <- dirs[missing.idx]
        msg <- sprintf("The following directories do not have a paf_model_version.csv - cannot get versions: %s\\n\\t", paste(bad.dirs, collapse = "\\n\\t"))
        stop(msg)
      }
    })

  return(res)
}


#' Check that all PAF model version data is consistent and return nested list with details.
#'
#' @param model_versions named list of data.table's with PAF model version data. The names are the year for the data.
#'
#' @return nested list containing the unified PAF model data.
check_paf_model_versions <- function(model_versions) {
  # assume first model version is correct, compare all others to it
  risk_model_version <- model_versions[[1]]

  is.match <- sapply(
    model_versions[-1], # all but first element
    function(mv) {
      # quirk - all.equal() returns either TRUE or a vector of characters explaining differences
      # we want TRUE or FALSE and use identical()
      identical(all.equal(risk_model_version, mv), TRUE)
    },
    USE.NAMES = TRUE)

  issues <- is.match[!is.match]
  if (length(issues) > 0) {
    msg <- sprintf(
      "paf_model_version.csv file for year(s) %s does not match file for year %s",
      paste(names(issues), collapse = "/"),
      names(model_versions)[[1]]
    )
    stop(msg)
  }

  # convert two column data.table of this form into nested list
  return(lapply(
      1:nrow(risk_model_version),
      function(i) {
        as.list(risk_model_version[i, ])
      }
    )
  )
}



