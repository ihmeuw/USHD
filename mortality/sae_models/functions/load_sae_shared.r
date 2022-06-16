# load sae.shared package---------------------------------------------------
library(lbd.loader, lib.loc = sprintf("FILEPATH", R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))

# default library location maintained by LSAE, loads the latest version.
sae.shared_path <- NULL

if (is.null(sae.shared_path)) {
  library(sae.shared,
    lib.loc = lbd.loader::pkg_loc("sae.shared")
  )
} else {
  # load the source code
  devtools::load_all(sae.shared_path)
}
