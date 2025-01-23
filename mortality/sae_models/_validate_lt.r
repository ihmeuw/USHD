####################################################################################################
## Description: Prepare folder structure for validation models and then submit "_submit_sing_cause.r"
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

stopifnot(grepl("mortality/sae_models$", getwd()))
suppressMessages(suppressWarnings({
  library(R.utils)
  library(data.table)
  library(glue)
  R.utils::sourceDirectory('functions', modifiedOnly = FALSE)
}))

## Get and check settings --------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]
resub <- args[2]
if (is.na(resub)) resub <- F
resub <- as.logical(resub)

if (!resub) check_settings(dir, validation = T)
get_settings(dir)

if (misclassification_correction) {
  stop("Misclassification correction shouldn't be set to true.")
}

archive_job_output <- function(dir) {
  # get job-output dir
  job_output_dir <- file.path(dir, "job-output")

  if (dir.exists(job_output_dir)) {
    # get list of all files
    files <- list.files(path = job_output_dir,
                        full.names = FALSE, 
                        include.dirs = FALSE
    )
    files <- files[files != "_archive"]

    # make _archive directory, with a dir named with the date inside it

    todays_date <- format(Sys.time(), '%Y_%b_%d')
    archive_dir <- file.path(job_output_dir, "_archive")
    dir.create(archive_dir)
    archive_dir <- file.path(archive_dir, glue("logs_{todays_date}"))
    dir.create(archive_dir)

    # custom function
    move_one_file <- function(f) {
      file.rename(from = file.path(job_output_dir, f),
                  to = file.path(archive_dir, f))
    }

    # apply the function to all files
    if (length(files) > 0) {
      lapply(files, move_one_file)
    }
  }
}

## Make settings for each model run ------------------------------------------------------------------------
if (!resub) {

  ## Load the settings file
  settings <- read.csv(paste0(dir, "/settings.csv"), stringsAsFactors = F, header = F)
  if (!("cause_id" %in% settings$V1)) {
    message("cause_id is not in settings.csv")
    settings <- rbind(settings, data.table(V1 = "cause_id", V2 = 294))
  }
  stopifnot("cause_id" %in% settings$V1)
  if (as.integer(settings$V2[settings$V1 == "cause_id"]) != 294) {
    stop(paste0("cause_id should be 294, but it is: ", as.integer(settings$V2[settings$V1 == "cause_id"])))
  }

  if (as.logical(settings$V2[settings$V1 == "misclassification_correction"])) {
    stop("Misclassification correction shouldn't be set to true.")
  }

  ## Create directories for holding all files related to each validation model run
  for (val_type in val_types[1]) {
    for (size in format(val_sizes, scientific = F, trim = T)) {
      for (iter in 1:val_iter) {

        message(glue("val_type: {val_type}"))
        message(glue("val_size: {size}"))
        message(glue("iteration: {iter}"))

        new_dir <- file.path(dir, glue("validation_{val_type}"), glue("size_{size}_iter_{iter}"), "_all")
        if (!file.exists(new_dir)) dir.create(new_dir, recursive = T)

        ## Create a settings CSV for each validation run which uses the appropriate input files
        new_settings <- copy(settings)
        this_deaths_file <- file.path(val_dir, val_type, glue("deaths_{size}_{iter}.rds"))
        this_pop_file <- file.path(val_dir, val_type, glue("pop_{size}_{iter}.rds"))
        stopifnot(file.exists(this_deaths_file))
        stopifnot(file.exists(this_pop_file))
        new_settings$V2[new_settings$V1 == "deaths_file"] <- this_deaths_file
        new_settings$V2[new_settings$V1 == "pop_file"] <- this_pop_file

        stopifnot("cause_id" %in% new_settings$V1)
        if (as.integer(new_settings$V2[new_settings$V1 == "cause_id"]) != 294) {
          stop(paste0("cause_id should be 294, but it is: ", as.integer(new_settings$V2[new_settings$V1 == "cause_id"])))
        }

        write.table(new_settings, file = file.path(new_dir, "settings.csv"), row.names = F, col.names = F, sep = ",", qmethod = "double")
        write.table(new_settings, file = file.path(gsub("\\/_all", "", new_dir), "settings.csv"), row.names = F, col.names = F, sep = ",", qmethod = "double")

        file.copy(from = file.path(dir, "submitted_cause_list.csv"), to = file.path(gsub("\\/_all", "", new_dir), "submitted_cause_list.csv"))

        check_settings(new_dir, validation = T)

        write_consolidated_settings(gsub("\\/_all", "", new_dir))

      }  # end val_iter loop
    }  # end val_sizes loop
  }  # end val_types loop
  message("Done with creation of dirs and settings loop.")
}  # end resub if statement



# Initial submission for all validation models ------------------------------------------------
if (!resub) {
  for (val_type in val_types[1]) {
    for (size in format(val_sizes, scientific = F, trim = T)[8]) {
      for (iter in 1:val_iter) {
        message(glue("val_type: {val_type}"))
        message(glue("val_size: {size}"))
        message(glue("iteration: {iter}"))

        this_dir <- file.path(dir, glue("validation_{val_type}"), glue("size_{size}_iter_{iter}"), "_all")

        # archive any job output files
        archive_job_output(this_dir)

        sbatch(
          code = "_submit_single_cause.r",
          name = glue("sub_val_{size}_{iter}"),
          arguments = c(
            this_dir, # dir
            TRUE, # lt
            "validation", # type
            FALSE, # resub DON'T CHANGE THIS FROM FALSE
            TRUE, # testing
            TRUE, # all_cause_dependency
            FALSE, # write_estimates_with_issues
            "--queue", "QUEUE",
            "--project", "PROJECT",
            # YLLs are not made for validation, so don't try to aggregate them
            "--skip_yll_aggregation",  # this flag doesn't want a T/F value given to it
            "--skip_yld_aggregation",
            "--skip_pred_aggregation"
          ),
          fthread = 1,
          m_mem_free = "40G",
          h_rt = "04:00:00",
          archive = T,
          project = "proj_tobacco",
          queue = "QUEUE",
          sgeoutput = this_dir
        )

        Sys.sleep(10) # pause so as not to overload the scheduler
      }
    }
  }
  message("Done with initial submission loop.")
}


## Resubmit failed validation models ---------------------------------------------------------------
if (resub) {

  ## Loop through all validation models
  for (val_type in val_types[1]) {
    for (size in format(val_sizes, scientific = F, trim = T)[8]) {
      for (iter in 1:val_iter) {

        this_dir <- file.path(dir, glue("validation_{val_type}"), glue("size_{size}_iter_{iter}"), "_all")

        ## If there isn't final output, resubmit the model
        if (!(file.exists(file.path(this_dir, "mx_est_all.rds"))) | !(file.exists(file.path(this_dir, "lt_est_all.rds")))) {
          message(
            glue(
              "Resubmitting:",
              "val_type: {val_type}",
              "val_size: {size}",
              "iteration: {iter}",
              "this_dir: {this_dir}",
              .sep = "\n"
            )
          )

          # archive any job output files
          archive_job_output(this_dir)

          sbatch(
            code = "_submit_single_cause.r",
            name = glue("sub_val_{size}_{iter}"),
            arguments = c(
              this_dir, # dir
              TRUE, # lt
              "validation", # type
              TRUE, # resub DON'T CHANGE THIS FROM TRUE
              TRUE, # testing
              TRUE, # all_cause_dependency
              FALSE, # write_estimates_with_issues
              "--queue", "QUEUE",
              "--project", "PROJECT",
              # YLLs are not made for validation, so don't try to aggregate them
              "--skip_yll_aggregation",  # this flag doesn't want a T/F value given to it
              "--skip_yld_aggregation",
              "--skip_pred_aggregation"
            ),
            fthread = 1,
            m_mem_free = "40G",
            h_rt = "04:00:00",
            archive = T,
            project = "proj_tobacco",
            queue = "QUEUE",
            sgeoutput = this_dir
          )

          Sys.sleep(1) # pause so as not to overload the scheduler

        }  # end if lt_est_all doesn't exist if statement
      }  # end val_iter loop
    }  # end val_sizes loop
  }  # end val_types loop
  message("Done with resubmissiong loop.")
}  # end resub if statement




# check for  outputs -------------------------------------------------

# dir <- ""
# get_settings(dir)
if (interactive()) {
  message("Checking validation directories for missing final files.")

  missing_files_mx <- c()
  missing_files_lt <- c()

  for (val_type in val_types) {
    for (size in format(val_sizes, scientific = F, trim = T)) {
      for (iter in 1:val_iter) {

        this_dir <- file.path(dir, glue("validation_{val_type}"), glue("size_{size}_iter_{iter}"), "_all")

        if (!file.exists(file.path(this_dir, "mx_est_all.rds"))) {
          missing_files_mx <- c(missing_files_mx, glue("size_{size}_iter_{iter}"))
        }

        if (!file.exists(file.path(this_dir, "lt_est_all.rds"))) {
          missing_files_lt <- c(missing_files_lt, glue("size_{size}_iter_{iter}"))
        }
      }
    }
  }

  message(glue("There are {length(missing_files_mx)} dirs missing mx_est_all."))
  if (length(missing_files_mx) > 0) {
    message(glue::glue("{paste(missing_files_mx, collapse = '\n')}"))
  }
  message(glue("There are {length(missing_files_lt)} dirs missing lt_est_all."))
  if (length(missing_files_lt) > 0) {
    message(glue::glue("{paste(missing_files_lt, collapse = '\n')}"))
  }
}


if (interactive() & by_edu) {
  message("Checking validation directories for missing model fits BY EDU.")

  missing_files_males <- c()
  missing_files_females <- c()

  for (val_type in val_types) {
    for (size in format(val_sizes, scientific = F, trim = T)) {
      for (iter in 1:val_iter) {

        this_dir <- file.path(dir, glue("validation_{val_type}"), glue("size_{size}_iter_{iter}"), "_all")

        if (!file.exists(file.path(this_dir, "/model_fit_1_1_99.rds"))) {
          missing_files_males <- c(missing_files_males, glue("size_{size}_iter_{iter}"))
        }

        if (!file.exists(file.path(this_dir, "/model_fit_2_1_99.rds"))) {
          missing_files_females <- c(missing_files_females, glue("size_{size}_iter_{iter}"))
        }
      }
    }
  }

  message(glue("There are {length(missing_files_males)} dirs missing model_fit_1_1_99.rds"))
  if (length(missing_files_males) > 0) {
    message(glue::glue("{paste(missing_files_males, collapse = '\n')}"))
  }
  message(glue("There are {length(missing_files_females)} dirs missing model_fit_2_1_99.rds"))
  if (length(missing_files_females) > 0) {
    message(glue::glue("{paste(missing_files_females, collapse = '\n')}"))
  }
}


if (interactive()) {
  message("Checking validation directories for missing model fits BY COUNTY")

  missing_files_males <- c()
  missing_files_females <- c()

  for (val_type in val_types) {
    for (size in format(val_sizes, scientific = F, trim = T)) {
      for (iter in 1:val_iter) {

        this_dir <- file.path(dir, glue("validation_{val_type}"), glue("size_{size}_iter_{iter}"), "_all")

        if (!file.exists(file.path(this_dir, "/model_fit_1_99_1.rds"))) {
          missing_files_males <- c(missing_files_males, glue("size_{size}_iter_{iter}"))
        }

        if (!file.exists(file.path(this_dir, "/model_fit_2_99_1.rds"))) {
          missing_files_females <- c(missing_files_females, glue("size_{size}_iter_{iter}"))
        }
      }
    }
  }

  message(glue("There are {length(missing_files_males)} dirs missing model_fit_1_1_99.rds"))
  if (length(missing_files_males) > 0) {
    message(glue::glue("{paste(missing_files_males, collapse = '\n')}"))
  }
  message(glue("There are {length(missing_files_females)} dirs missing model_fit_2_1_99.rds"))
  if (length(missing_files_females) > 0) {
    message(glue::glue("{paste(missing_files_females, collapse = '\n')}"))
  }
}

message("DONE with script.")
