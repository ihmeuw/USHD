####################################################################################################
## Description: Upload attributable burden to database
####################################################################################################

library(R.utils)
library(data.table)
library(yaml)
library(parallel)


## load database central functions
library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                      R.version$major,
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
library("sae.shared", lib.loc = lbd.loader::pkg_loc("sae.shared"))
library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
library("ushd.paf", lib.loc = lbd.loader::pkg_loc("ushd.paf")) # load so that we get useful function retry_with_throttle
# running this line ensures successful database connection and prevents interference from other packages
ushd_client$save_covariate_population

burden_risk_dir <- 'FILEPATH'
burden_risk_metadata_id <- NULL
burden_risk_run_id <- NULL
raked <- FALSE

# burden metadata
burden_settings <- jsonlite::fromJSON(paste0(burden_risk_dir, 'metadata.json'))
burden_settings <- burden_settings[["cli-args"]]

paf_compile_run_id = burden_settings$paf_compile_run_id
fatal_model_run_id = burden_settings$fatal_run_id
nonfatal_model_run_id = NULL #burden_settings$nonfatal_run_id
parent_run_id = NULL
saved_draws <- FALSE
saved_summary <- TRUE # turning off for now

is_best_burden <- TRUE
prev_issues_burden <- "Accidentally used only 100 draws."


if(is.null(burden_risk_metadata_id)){
  burden_risk_metadata_id <- save_burden_risk_metadata(paste0(burden_risk_dir, 'metadata.json'))
  message("burden_risk_metadata_id ", burden_risk_metadata_id)
}

if(is.null(burden_risk_run_id)){
  burden_risk_run_id <- save_burden_risk_run(raked = raked,
                                             burden_risk_metadata_id = burden_risk_metadata_id,
                                             paf_compile_run_id = paf_compile_run_id,
                                             fatal_model_run_id = fatal_model_run_id,
                                             parent_run_id = parent_run_id,
                                             is_best = is_best_burden,
                                             prev_issues = prev_issues_burden)
  message("burden_risk_run_id ", burden_risk_run_id)
}

if(!saved_draws){
  # First, get the list of all the locations
  draw_dirs <- list.files(paste0(burden_risk_dir, 'draws/'), 
                          full.names = TRUE, 
                          recursive = FALSE)

  ### set up parallel back-end
  # get number of cores available (the number we requested, not all threads on node)
  # Get the SLURM job ID from the environment variable
  job_id <- Sys.getenv("SLURM_JOB_ID")

  # Use system() to execute sacct and capture the output
  sacct_output <- system(paste("sacct -j", job_id, "--format=AllocCPUs --noheader"), intern = TRUE)

  # The output can have multiple lines (especially with steps or multiple entries). We'll take the first relevant line.
  # Also, trimming any leading/trailing whitespace for clean processing
  # Assuming the format is consistent, we'll just need the number
  # Directly converting the cleaned-up string to integer should give us the number of allocated CPUs
  alloc_cpus_raw <- as.integer(trimws(sacct_output[1]))
  # check that alloc_cpus_raw is a number, and not NULL or length 0
  if(is.na(alloc_cpus_raw) || length(alloc_cpus_raw) == 0){
    message("Could not determine number of allocated CPUs. Assuming 2")
    alloc_cpus_raw <- 2
  } else {
    message("Allocated CPUs: ", alloc_cpus_raw)
  }
  
  # run the upload in parallel
  out <- mclapply(1:length(draw_dirs), function(i){ # save to "out" in case there are errors
    if(i %% 100 == 0){
      message("Uploaded ", i, " out of ", length(draw_dirs), " locations")
    }

    # adapt with.retries from paf/ushd.R and ushd.paf to handle errors that pop
    # up with the database connection
    .retry.delay <- runif(1, 10, 30) # staggers retry
    with.retries <- function(code) {
      res <- retry_with_throttle(code, n.tries = 5, retry.delay = .retry.delay)
      return(res)
    }

    # for each location, get a list of all the files in the first level
    # directory, raked or unraked as appropriate
    draw_files <- list.files(draw_dirs[i], 
                           '.h5', 
                           recursive = FALSE, 
                           full.names = TRUE)
    # if raked, subset to files with raked.h5 in string.
    # Otherwise, invert
    draw_files <- draw_files[grep('raked.h5', draw_files, invert = !raked)]

    # Upload each file

    lapply(draw_files, function(file){
      file_name <- gsub("^.*/", "", file)
      file_name <- substr(file_name, 1, nchar(file_name) - 3)
      file_name <- strsplit(file_name, '_')[[1]]

      measure <- as.integer(file_name[1])
      location <- as.integer(file_name[2])
      year <- as.integer(file_name[3])
      
      with.retries(ushd.dbr::save_burden_risk_draw_file(burden_risk_run_id = burden_risk_run_id,
                                 path = file,
                                 measure_id = measure,
                                 location_id = location, 
                                 year_id = year))
    })
  }, mc.cores = pmin(alloc_cpus_raw, 4))

  message("Done uploading draw files")
}

# The following code can be used to verify which locations were successfully uploaded
# in case the upload process was interrupted or partially failed.

# upload collapsed ests
if(!saved_summary){
  est_files <- list.files(paste0(burden_risk_dir, 'draws/'), 
                          '^upload_risk_[0-9]+_[0-9]+.csv$', 
                          recursive = TRUE, 
                          full.names = TRUE)
  message("Uploading ", length(est_files), " est files")
  for(i in 1:length(est_files)){
    if(i %% 1000 == 0){
      message("Uploaded ", i, " out of ", length(est_files), " est files")
    }
    
    est_file <- est_files[i]
    save_burden_risk_summary_data(burden_risk_run_id = burden_risk_run_id,
                                  path = est_file,
                                  raked = FALSE)
  }
  message(paste("Loaded", length(est_files), "est files"))
}


