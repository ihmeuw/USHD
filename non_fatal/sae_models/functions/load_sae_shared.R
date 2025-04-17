
# load sae.shared package-------------------------------------------------------------------------------------------------
# sae.shared is dependent on lbd.loader
if (R.version$major == "3") {
  library(lbd.loader, lib.loc = "FILEPATH")  
} else if (R.version$major == "4") {
  library(lbd.loader, lib.loc = "FILEPATH")  
}


sae.shared_repo <- NULL

if (is.null(sae.shared_repo)) {
  # loads the most current library maintained by LSAE. If specific version is required,
  # pass it with "version" argument.
  lbd.loader::load_package("sae.shared")
} else {
  lbd.loader::load_package(path = sae.shared_repo)
}
