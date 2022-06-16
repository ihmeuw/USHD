####################################################################################################
## Description: Prepare folder structure for validation models and then submit "_submit_single_cause.r"
##              for each validation models. This code can also be used to resubmit failed models. In
##              this case, preparing the folder structure is skipped, and instead any model without
##              final output is resubmitted.
##
## Passed args: dir [character] -- home directory for settings and final output (not a model run dir)
##              resub [logical] -- is this a resubmission (i.e., you've already run this
##                code once and now you just want to resubmit models that failed on the first pass)
##
## Requires:    a list of years that will be included in the models (years)
##              all validation settings (val_dir, val_types, val_sizes, val_iter)
##
## Outputs:     a folder structure for validation models with one folder per model containing
##                a "settings" CSV pointing to the appropriate inputs
##              a submitted job for each validation model which will run "_submit_single_cause.r"
##                in order to submit and run all required sub-components of each model
##
####################################################################################################

library(R.utils)
sourceDirectory("functions/")

## Get and check settings --------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]
resub <- args[2]
if (is.na(resub)) resub <- F
resub <- as.logical(resub)

check_settings(dir, validation = T)
get_settings(dir)

# pre-compile TMB models here, so that it won't need to when submit_single_cause runs
if (model_class == "tmb_models") TMB::compile(paste0("tmb_models/mod_", model, ".cpp"))

## Submit validation models ------------------------------------------------------------------------
if (!resub) {

  ## Load the settings file
  settings <- read.csv(paste0(dir, "/settings.csv"), stringsAsFactors = F, header = F)
  if (!("cause_id" %in% settings$V1)) {
    settings <- rbind(settings, data.table(V1 = "cause_id", V2 = 294))
  }
  stopifnot("cause_id" %in% settings$V1)
  if (as.integer(settings$V2[settings$V1 == "cause_id"]) != 294) {
    stop(paste0("cause_id should be 294, but it is: ", as.integer(settings$V2[settings$V1 == "cause_id"])))
  }

  ## Create directories for holding all files related to each validation model run
  for (val_type in val_types) {


    for (ss in format(val_sizes, scientific = F, trim = T)) {


      for (it in 1:val_iter) {

        cat(paste0("val_type ", val_type, "\n"))
        cat(paste0("val_size ", ss, "\n"))
        cat(paste0("iteration ", it, "\n"))

        new_dir <- paste0(dir, "/validation_", val_type, "/size_", ss, "_iter_", it, "/_all/")
        if (!file.exists(new_dir)) dir.create(new_dir, recursive = T)

        ## Create a settings CSV for each validation run which uses the appropriate input files
        new_settings <- copy(settings)
        new_settings$V2[new_settings$V1 == "deaths_file"] <- paste0(val_dir, "/", val_type, "/deaths_", ss, "_", it, ".rds")
        new_settings$V2[new_settings$V1 == "pop_file"] <- paste0(val_dir, "/", val_type, "/pop_", ss, "_", it, ".rds")

        stopifnot("cause_id" %in% new_settings$V1)
        if (as.integer(new_settings$V2[new_settings$V1 == "cause_id"]) != 294) {
          stop(paste0("cause_id should be 294, but it is: ", as.integer(new_settings$V2[new_settings$V1 == "cause_id"])))
        }

        write.table(new_settings, file = paste0(new_dir, "settings.csv"), row.names = F, col.names = F, sep = ",", qmethod = "double")

        check_settings(new_dir, validation = T)

        qsub(code = "_submit_single_cause.r",
         name = paste0("sub_val_", ss, "_", it),
         shell = "FILEPATH -s",
         arguments = c(
           new_dir,  # dir
           TRUE,  # lt
           "validation",  # type
           FALSE, # resub
           TRUE, # testing
           TRUE, # all_cause_dependency
           "--queue", "QUEUE",
           "--project", "PROJECT",
           "--priority", 0,
           # YLLs are not made for validation, so don't try to aggregate them
           "--skip-yll-aggregation" 
         ),
         fthread = 1, m_mem_free = "64G", h_rt = "04:00:00", archive = T,
         project = "PROJECT", queue = "QUEUE",
         sgeoutput = new_dir)

        Sys.sleep(2) # pause

      } 
    }
  } 
  cat("Done with primary submission loop \n")
}

## Resubmit failed validation models ---------------------------------------------------------------
if (resub) {

  ## Loop through all validation models
  for (val_type in val_types) {

    for (ss in format(val_sizes, scientific = F, trim = T)) {

      for (it in 1:val_iter) {

        new_dir <- paste0(dir, "/validation_", val_type, "/size_", ss, "_iter_", it, "/_all")

        ## If there isn't final output, resubmit the model
        if (!file.exists(paste0(new_dir, "/mx_est_all.rds")) | !file.exists(paste0(new_dir, "/lt_est_all.rds"))) {
          cat("Resubmitting:\n")
          cat(paste0("val_type ", val_type, ":\n"))
          cat(paste0("val_size ", ss, ":\n"))
          cat(paste0("iteration ", it, "\n"))

          qsub(code = "_submit_single_cause.r",
               name = paste0("sub_val_", ss, "_", it),
               shell = "FILEPATH -s",
               arguments = c(
                 new_dir,  # dir
                 TRUE,  # lt
                 "validation",  # type
                 TRUE, # resub, this is the only difference from the above qsub
                 TRUE, # testing
                 TRUE, # all_cause_dependency
                 "--queue", "QUEUE",
                 "--project", "PROJECT",
                 "--priority", 0,
                 # YLLs are not made for validation, so don't try to aggregate them
                 "--skip-yll-aggregation"  # this flag doesn't want a T/F value given to it
                 # don't do --skip-mx-aggregation, it will also skip compile_estimates
               ),
               fthread = 1, m_mem_free = "64G", h_rt = "04:00:00", archive = T,
               project = "PROJECT", queue = "QUEUE",
               sgeoutput = new_dir)

          Sys.sleep(1) # pause

        } 
      }
    } 
  }
  cat("Done with resubmissiong loop \n")
} 

cat("Done with script\n")
