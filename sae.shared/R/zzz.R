# .onload() in zzz.R file makes sure it's loaded when the package ("sae.shared") loads.
.onLoad <- function(libname, pkgname) {
  options(ushd.use_database_for_population = FALSE)
  options(ushd.use_adjusted_data = TRUE)
  # set ushd.use_edu_paths to TRUE if it has not already been set, otherwise leave it as-is
  options(ushd.use_edu_paths = getOption("ushd.use_edu_paths", default = TRUE))
  Sys.umask("0002")
  load_gbd_shared_functions()
}
