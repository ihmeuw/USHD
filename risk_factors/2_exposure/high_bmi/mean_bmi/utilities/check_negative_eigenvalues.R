####################################################################################################
## Description: Checks the INLA model fits for negative eigenvalues
####################################################################################################

# Arguments

if(interactive()){
    output_dir_draws_est <- "FILEPATH"
} else {
    output_dir_draws_est <- commandArgs(TRUE)[[1]]
}
 

  
# set up ------------------------------------------------------------------
library(data.table)
library(mvtnorm)
rversion <- 422 
INLA_loc <- paste0("FILEPATH")

#### Load INLA package
if (!is.null(INLA_loc)) { # Load INLA version from user-specified path
  library(INLA, lib.loc = INLA_loc)
} else { # Load INLA version from Singularity image
  library(INLA)
}

nonfatal_repo <- paste0("FILEPATH")
funcs <- list.files(paste0(nonfatal_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(nonfatal_repo, "/functions/", func)))
}

# Load settings file
settings_loc <- paste0(output_dir_draws_est, "settings.csv")
get_settings(settings_loc)

# Load models in a list so that it's clear which mod fit goes to which file
# Will sort results of checks in this list

mods_paths <- list.files(output_dir_draws_est, pattern = "mod", full.names = T)
stopifnot(length(mods_paths) == n.imp*2)

mod <- lapply(1:length(mods_paths), function(i){
    # get model name
    mod_name <- gsub(".*/mod(\\d+)sex(\\d+)\\.RDS", "imp\\1_sex\\2", mods_paths[i])
    
    cat(paste0("Checking model ", i, " of ", length(mods_paths), ": ", mod_name, "\n"))

    # load the fit
    fit <- readRDS(mods_paths[i])
    
    # extract the eigenvalues from the logs
    grep("Eigenvalues of the Hessian", fit$logfile) -> eigenvalue_loc_start
    grep("StDev\\/Correlation matrix \\(scaled inverse Hessian\\)", fit$logfile) -> eigenvalue_loc_end
    eigen_values <- fit$logfile[(eigenvalue_loc_start+1):(eigenvalue_loc_end-1)]
    eigen_values <- as.numeric(str_trim(eigen_values))
    eigenvalue_dt <- data.table(eigenvalues = eigen_values)
    
    eigenvalue_dt[, mod := mod_name]
    
    # Check for other issues:
   
    checks <- list(
        # fit$ok not documented -- got from here: # https://groups.google.com/g/r-inla-discussion-group/c/dPFKNwMFxwE/m/1exAOQJAAAAJ
        overall = if (fit$ok) {("The model fit completed successfully.\n")} else {("There was an error during the model fit.\n")},
        # check the logs for the words "warning", "fail", or "check"
        logged_warnings = fit$logfile[grep("warn",ignore.case =T,  fit$logfile)],
        logged_fails = fit$logfile[grep("fail",ignore.case =T,  fit$logfile)],
        logged_checks = fit$logfile[grep("(?<![.])\\bcheck\\b", ignore.case = TRUE, fit$logfile, perl = TRUE)]
    )

    # save the eigenvalues to the mod list
    return(list(DT = eigenvalue_dt, check_logs = checks))
})
names(mod)[i] <- gsub(".*/mod(\\d+)sex(\\d+)\\.RDS", "imp\\1_sex\\2", mods_paths[i])

# check for negative eigenvalues
all_eigen <- rbindlist(lapply(mod, `[[`, 1))
if (any(all_eigen$eigenvalues < 0)) {
    cat("The following mods have negative eigenvalues:\n")
    cat(unique(all_eigen[eigenvalues < 0, mod]), "\n")
} else {
    cat("All mods have non-negative eigenvalues.\n")
}

# display other warnings
lapply(1:length(mod), function(i) {
    cat("\n\n", names(mod)[i], "\n")
    # print log info that is not NULL
    mod[[i]][[2]][sapply(mod[[i]][[2]], length)]
})

