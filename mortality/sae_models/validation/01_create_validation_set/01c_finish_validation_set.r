####################################################################################################
## Description: This is the third script in a set of three
## (start_validation_set.r, create_validation_set_lifetables.r,
## finish_valiation_set.r). See start_validation_set.r for details.
##
####################################################################################################

stopifnot(grepl("mortality/sae_models$", getwd()))
suppressMessages(suppressWarnings({
  library(R.utils)
  library(data.table)
  library(glue)
  R.utils::sourceDirectory('functions', modifiedOnly = FALSE)
}))


# Script arguments ----------------------------------------------------------------------------

if (interactive()) {
  base_dir <- "FILEPATH" # this is where settings are read from
  output_dirname <- "2023_08_15_validation_set_county"
} else {
  args <- commandArgs(trailingOnly = TRUE)
  base_dir <- args[1]
  output_dirname <- args[2]
}

message(
  glue(
    "Arguments:",
    "base_dir: {base_dir}",
    "output_dirname: {output_dirname}",
    .sep = "\n"
  )
)


# Get settings --------------------------------------------------------------------------------

message(glue("Reading settings from {base_dir}"))
get_settings(base_dir)

base_dir = file.path(paste0(
  "FILEPATH",
  LU_folder,
  "FILEPATH"
))

final_output_dir <- file.path(base_dir, output_dirname)

# Collect lifetables -------------------------------------------------------------------------------

expected_dimensions <- readRDS(file.path(
  final_output_dir,
  "intermediate_validation_set",
  glue("expected_ex_dimensions.rds")
))

expected_dimensions[, filename := glue("ex_output_{year}_{sex}_{race}_{edu}.rds"), by = "year,sex,race,edu"]
files <- unique(expected_dimensions$filename)

files <- file.path(
  final_output_dir,
  "intermediate_validation_set",
  "ex_output",
  files
)


missing_files <- files[!file.exists(files)]

if (length(missing_files) > 0) {
  stop(glue("There are {length(missing_files)} missing files:",
            "{paste(missing_files, collapse = '\n')}",
            .sep = "\n"))
}

if (length(files) == 0) {
  stop("No files.")
}

ex <- rbindlist(lapply(files, readRDS), use.names = T)

# Save files ---------------------------------------------------------------------------------------


saveRDS(ex, file.path(final_output_dir, "validation_set", "validation_set_gs_ex.rds"))
message("DONE.")
