####################################################################################################
## Description: Script to launch "check_raked_result"  from sae.shared package for the postestimation
##                process after aggregation jobs have completed.
##
## Inputs:      dir [character] -- main directory for settings and final output for all causes. If checking 
##                  an r/e model, this will be the county_race_dir
##              plot_level [character] -- level to display data and estimates plots at. Note that mcnty
##                  has not been implemented due to large numbers of plots, but could be by making
##                  a workaround to the geoagg_files logic
##              dont_recalc_validation [logical] -- if TRUE, then if the output already exists, that cause will
##                  be skipped in the loops.
##              threads [integer] -- number of threads to parallelize over
##              queue [character] -- queue on which the job will be run
##              delete_temp [logical] -- If True, on passing checks for raked results, temporary files for the specified
##                  cause/level will be deleted.
##
####################################################################################################

stopifnot(grepl("sae_models$", getwd()))
library(R.utils)
source("functions/load_sae_shared.R")

if (interactive()) {
  dir <- "FILEPATH"
  plot_level <- "state"
  dont_recalc_validation <- FALSE
  delete_temp <- FALSE
  threads <- 5
  queue <- "..."
} else {
  parser <- argparse::ArgumentParser()
  parser$add_argument("--dir", help = "Model run dir")
  parser$add_argument("--plot_level", choices = c("natl", "state"))
  parser$add_argument("--dont_recalc_validation", choices = c("TRUE", "FALSE"))
  parser$add_argument("--delete_temp",
                      default = FALSE, 
                      choices = c("TRUE", "FALSE"),
                      help = paste0("If True, on passing checks for raked results, temporary files for the specified ",
                                    "cause/level will be deleted."))
  default_threads <- as.integer(Sys.getenv("SGE_HGR_fthread", unset = "1"))
  default_threads <- if(default_threads > 1L) default_threads - 1L else default_threads
  parser$add_argument("--threads", type = "integer", default = default_threads, help = "Number of threads to assign for qsub job.")
  parser$add_argument("--queue", type = "character", default = "...", help = "queue on which the job will run.")
  
  args <- parser$parse_args(get_args())
  
  dir <- args$dir
  plot_level <- args$plot_level
  dont_recalc_validation <- as.logical(args$dont_recalc_validation)
  delete_temp <- args$delete_temp
  threads <- args$threads
  queue <- args$queue
}

# path to check_raked_result script
check_script_path <- fs::path_package("sae.shared", "scripts", "check_raked_result.r")

# submit job using sbatch
job_id <- sbatch(code = check_script_path,
               name = "yld_check_raked_result",
               arguments = c(
                 "--dir", dir, 
                 "--plot_level", plot_level, 
                 "--dont_recalc_validation", dont_recalc_validation,
                 "--measures yld",
                 "--initial_level 0",
                 "--terminal_level 0",
                 "--delete_temp", delete_temp),
               fthread = threads, m_mem_free = "60G", h_rt = "05:00:00", archive = TRUE,
               project = "PROJECT", queue = queue,
               sgeoutput = dir)

message("yld_check_raked_result job submitted, ID: ", job_id)

