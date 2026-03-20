####################################################################################################
## Description: Generate diagnostics information about user R environment
##
##              Specifically:
##              1. check if ushd and jobmon clients exist
##              2. Parse .libPaths for unexpected weirdness
##              3. check RETICULATE_PYTHON env var
##              4. load lbd.loader, sae.shared
##              5. check RETICULATE_PYTHON env var
##              6. load jobmon and stringr
##              7. check ushd and jobmon clients again
##
####################################################################################################

sing_name <- Sys.getenv("SINGULARITY_NAME")
message(glue::glue("Running diagnostics on SINGULARITY_NAME = {sing_name}"))

check_client <- function(client_str, client_obj, expected_str, msg) {
  if (exists(client_str)) {
    if (client_obj$"__name__" != expected_str) {
      message(glue::glue("Unexpected client __name__ for {client_str} = {client_obj$'__name__'}"))
    }
  } else {
    message(msg)
  }
}

message("\nChecking ushd_client and jobmon_client")
check_client(client_str = "ushd_client",
             client_obj = ushd_client,
             expected_str = "dbload",
             msg="ushd_client does not exist - that's okay here")
check_client(client_str = "jobmon_client",
             client_obj = jobmon_client,
             expected_str = "jobmon.client",
             msg="jobmon_client does not exist - that's okay here")

# check for weird entries in .libPaths
lp <- .libPaths()
if (grepl("tmb", sing_name)) {
  expected = c("FILEPATH")
  for (str in expected) {
    lp <- lp[lp != str]
  }
} else {
  expected = c("FILEPATH", 
               "FILEPATH", 
               glue::glue("FILEPATH"))
  for (str in expected) {
    lp <- lp[lp != str]
  }
  #check for lbd.loader package paths
  lbd <- glue::glue("FILEPATH")
  for (lp_str in lp) {   
    if (grepl(lbd, lp_str))  lp <- lp[lp != lp_str]
  }
}
if (length(lp) == 0) {
  message("\nNo extra or unexpected .libPaths()")
} else {
  message("\nextra or unexpected .libPaths()")
  message(paste0(lp, collapse="\n"))
}

ret_py_str = Sys.getenv("RETICULATE_PYTHON")
message(paste0("\npre-lbd.loader RETICULATE_PYTHON = ", ret_py_str))
if (ret_py_str == "FILEPATH") {
  message("USHD conda env already loaded - this is probably okay, but means that your environment isn't clean")
} else if (ret_py_str == "") {
  message("No conda env already loaded, env is clean")
} else {
  message("That conda env is not known, env is polluted - you should  clear workspace, restart")
}

message("\nLoading lbd.loader, and sae.shared - messages and warnings are suppressed - this will take a few minutes")
library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                      R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
suppressWarnings(suppressMessages(lbd.loader::load_package("sae.shared")))

message(paste0("\npost sae.shared RETICULATE_PYTHON = ", Sys.getenv("RETICULATE_PYTHON")))
message("\nLoading stringr, and jobmonr")
library(stringr)
library(jobmonr)

message("\nPost sae.shared - Checking ushd_client and jobmon_client")
check_client(client_str = "ushd_client", 
             client_obj = ushd_client, 
             expected_str = "dbload",
             msg="ushd_client does not exist - clean your cache FILEPATH")
check_client(client_str = "jobmon_client", 
             client_obj = jobmon_client, 
             expected_str = "jobmon.client",
             msg="jobmon_client does not exist - clean your cache FILEPATH")
message(glue::glue("Diagnostics on SINGULARITY_NAME = {sing_name} complete"))