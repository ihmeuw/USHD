#------------------------------------------------------------------------------
# PURPOSE: Launch the PAF compile step (aggregate_and_mediate.R) for every location ID. 
#------------------------------------------------------------------------------
library(lbd.loader, lib.loc = sprintf("FILEPATH", R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
library(ushd.dbr, lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
library(sae.shared, lib.loc = lbd.loader::pkg_loc("sae.shared"))
lbd.loader::load.containing.package()

if (interactive()){
  yaml_file = "FILEPATH"
  extra_dim = "race"
  extra_dim_values = "2,4,5,6,7"
  mortality_model = "FILEPATH"
  n_draws = 100
  year_ids = paste0(2000:2023, collapse = ",")
  root_dir = ''
  partition = 'QUEUE'
} else {
  parser <- argparse::ArgumentParser()
  parser$add_argument("--yaml_file", type = "character", help = "YAML file where keys are risk names and values are paths to modeled directories.")
  parser$add_argument("--extra_dim", type = "character", choices = c("race", "edu"), help = "Extra dimension of PAF values. Must be one of c('race', 'edu')")
  parser$add_argument("--extra_dim_values", type = "character", help = "Values for extra dimension")
  parser$add_argument("--mortality_model", type = "character", help = "Fatal model linked to this run. Used to pull the cause list.")
  parser$add_argument("--n_draws", type = 'integer', help = "Number of draws in the model.")
  parser$add_argument("--year_ids", type = "character", help = "List of year IDs to be used in the model. Needs to be a string separated by commas, e.g. '2001,2002'.")
  parser$add_argument("--root_dir", type = "character", default = "FILEPATH", help = "root directory to store outputs. A run directory will be created inside this")
  parser$add_argument('--partition', '-p', default='QUEUE', help='Partition to use')
  args <- parser$parse_args(get_args())
  for (key in names(args)) {
    assign(key, args[[key]])
  }
  
  print("Arguments passed were:")
  for (arg in names(args)) {
    print(paste0(arg, ": ", paste0(get(arg), collapse = ","), ". Vector length is ", length(get(arg))))
  }
}

year_ids_list <- as.character(unlist(strsplit(year_ids, ",")))
#------------------------------------------------------------------------------
# Step 1: Create a new directory with one sub-folder per risk being compiled.
#------------------------------------------------------------------------------
risks <- yaml::read_yaml(yaml_file)
run_directory = create_run_directory(root_dir)
message(sprintf("Created output directory %s", run_directory))

# This loop might get unbearably long as our outputs get larger...
available_location_ids = list()
rei_paf_model_versions <- list()
for (risk in names(risks)){
  risk_config <- risks[[risk]]

  # confirm PAF model version metadata is consistent across runs
  model_versions <- get_paf_model_versions(risk_config)
  # validate they are consistent. error or return a reformatted version for output
  mv <- check_paf_model_versions(model_versions)
  rei_paf_model_versions[[risk]] <- mv

  for (year in year_ids_list) {
    # Set up directories 
    message(sprintf("Linking files for risk %s year %s", risk, year))
    orig_directory = risks[[risk]][[year]]
    files_to_link = list.files(orig_directory)
    dir.create(file.path(run_directory, risk, year), recursive=TRUE)
  
    # Create symbolic links back to original files - saves disk space
    for (f in files_to_link){
      file.symlink(file.path(orig_directory, f), file.path(run_directory, risk, year, f))
    }
  
    # Pull list of location IDs. The files have the pattern "{location_id}_{sex}.csv"
    location_ids = stringr::str_extract(files_to_link, "\\d+_\\d.csv")
    location_ids = location_ids[!is.na(location_ids)]
    location_ids = unlist(tstrsplit(location_ids, split = "_", keep = 1))
    location_ids = as.integer(unique(location_ids))
    available_location_ids[[risk]] = location_ids
  }
}

# save unified input model versions
yaml::write_yaml(rei_paf_model_versions, file.path(run_directory, "paf_model_versions.yaml"))

# Copy cause list from mortality model
save_cause_metadata(mortality_model, run_directory)

#------------------------------------------------------------------------------
# Step 2: Pull list of location IDs from files saved on disk
#------------------------------------------------------------------------------
for (risk in names(risks)){
  location_ids = available_location_ids[[risk]]
}
#------------------------------------------------------------------------------
# Step 3: Run 'aggregate_and_mediate.R' for each location ID. 
# Reconfigured to use an array job with a CSV of arguments

agg_and_mediate_script <- system.file("scripts/aggregate_and_mediate.R", package = "ushd.paf")

# Build argument table for array job (only location_id varies)
array_args <- data.table(expand.grid(location_id = location_ids, risk = names(risks)))
array_args_csv <- file.path(run_directory, "array_args.csv")
write.csv(array_args, array_args_csv, row.names = FALSE)

num_tasks <- nrow(array_args)
message(sprintf("Submitting %i jobs to aggregate and mediate for years %s as an array job", num_tasks, year_ids))

args = c(
  "-J", paste0("ushd_paf_compile_", year_ids_list[[1]], "_to_", year_ids_list[[length(year_ids_list)]], "_array"),
  "--mem=5G",
  "-c", 1,
  "-t", "1:00:00",
  "-o", "FILEPATH",
  "-A", "PROJECT",
  "-p", partition,
  "--parsable",
  paste0("--array=1-", num_tasks, "%500"), # throttle at 500
  "FILEPATH",
  "-s", agg_and_mediate_script,
  "--array_arg_csv", array_args_csv,
  "--year_ids", year_ids,
  "--n_draws", n_draws,
  "--run_directory", run_directory,
  "--extra_dim", extra_dim,
  "--extra_dim_values", extra_dim_values
)
result = processx::run("sbatch", args = args, stderr_to_stdout = TRUE)
result
if (result$status != 0) stop("Did not submit cluster array job successfully.")
