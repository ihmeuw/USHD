# load libraries and functions
stopifnot(grepl("ushd.paf", getwd()))

library(data.table)
library(magrittr)
library(ini)
library(parallel)
library(readr)

source("FILEPATH/get_restrictions.R")
source("FILEPATH/get_rei_metadata.R")

library(lbd.loader, lib.loc = sprintf("FILEPATH", R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
library(sae.shared, lib.loc = lbd.loader::pkg_loc("sae.shared"))

# Sourcing the package is complicated right now because it depends on sae.shared and ushd.dbr
source(file.path(lbd.loader::package.root(getwd()), 'R/get_demographics.R'))
source(file.path(lbd.loader::package.root(getwd()), 'R/get_cause_metadata.R'))

set.seed(124535)

#-- SET UP ARGS ----------------------------------------------------------------
if (interactive()){
  task_id = 1
  year_ids =  2019
  n_draws = 100
  run_directory = 'FILEPATH'
  extra_dim = 'race'
  extra_dim_values = c(2,4,5,6,7)

  array_args_loc <- paste0(run_directory, "/array_args.csv")
  array_args <- fread(array_args_loc, stringsAsFactors = FALSE)
  task_id = 1
  location_id <- array_args$location_id[task_id]
} else {
  parser <- argparse::ArgumentParser()
    parser$add_argument("--array_arg_csv", type = "character", help = "CSV file with array job arguments", default = NULL)
  parser$add_argument("--year_ids", type = "character", help = "Year IDs to run")
  parser$add_argument("--n_draws", type = "integer", help = "Number of draws")
  parser$add_argument("--run_directory", type = "character", help = "Path to read inputs from")
  parser$add_argument("--extra_dim", type = "character", choices = c("race", "edu"), help = "Dimension to compile PAFs over")
  parser$add_argument("--extra_dim_values", type = "character", help = "Values for extra dimension")

  args <- parser$parse_args(get_args())

  task_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  array_args <- fread(args$array_arg_csv, stringsAsFactors = FALSE)
  if (is.na(task_id) || task_id < 1 || task_id > nrow(array_args)) {
    stop(sprintf("Invalid or missing SLURM_ARRAY_TASK_ID: %s", task_id))
  }
  
  location_id <- array_args$location_id[task_id]
  year_ids <- args$year_ids
  n_draws <- args$n_draws
  run_directory <- args$run_directory
  extra_dim <- args$extra_dim
  extra_dim_values <- args$extra_dim_values
  
  year_ids <- as.integer(unlist(strsplit(year_ids, ",")))
  vector_args <- c("extra_dim_values")
  for (i in seq_along(vector_args)) {
    assign(vector_args[i], unlist(strsplit(get(vector_args[i]), ",")))
  }
  task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID") %>% as.numeric

  print(sprintf("Task ID is %s", task_id))
  print("Arguments passed were:")
  print(sprintf("location_id: %s", location_id))
  print(sprintf("year_ids: %s", year_ids))
  print(sprintf("n_draws: %s", n_draws))
  print(sprintf("run_directory: %s", run_directory))
  print(sprintf("extra_dim: %s", extra_dim))
  print(sprintf("extra_dim_values: %s", extra_dim_values))
}

# Get list of risks to aggregate - should be sub-folders in run_directory
risks = list.dirs(run_directory, recursive = FALSE, full.names = FALSE)
do_aggregation <- length(risks) > 1

# MAPPING FILES ----------------------------------------------------------------
demo <- ushd.get_demographics(extra_dim = extra_dim, extra_dim_values = extra_dim_values) # This is needed for age_group_id and sex_id
draw_cols <- paste0("draw_",0:(n_draws-1))

cause_dt <- ushd.get_cause_metadata(run_directory)

# READ/APPEND/RENAME/CAP -------------------------------------------------------
# Get list of files that match {location_id}_{sex_id}.csv format
# get all risk directories and year directories
dirs_to_list <- do.call(file.path, expand.grid(file.path(run_directory, risks), year_ids))
file_list = list.files(dirs_to_list, pattern = sprintf("^%s_[12].csv", location_id), full.names = TRUE, recursive = TRUE)

# Multiple years can point to the same file. Resolve the symlinks and remove 
#   duplicates before loading files
orig_file_list <- file_list
file_list <- unique(sapply(file_list, Sys.readlink))
if("" %in% file_list) stop("Not all files in file_list are symlinks. file_list:\n", paste(orig_file_list, collapse = "\n"))

if (length(file_list)==0){
  stop(sprintf("Did not find any files matching the pattern {location_id = %s}_{sex = [12]}.csv in the folder %s", location_id, run_directory))
}

# Read all of them in
dt <- rbindlist(lapply(file_list, read.csv))
rows <- nrow(dt)

# PREP FOR PAFS OF 1 -----------------------------------------------------------
# Drop PAFs of 1 if they came through before joint calculation
# This path is just a symlink to the static file maintained by Central Comp
paf_one <- fread("FILEPATH")[, .(rei_id, cause_id)]
paf_one[, paf_one := 1]
dt <- merge(dt, paf_one, by = c("rei_id", "cause_id"), all.x = TRUE)
dt <- dt[is.na(paf_one), ]
dt[, paf_one := NULL]

# drop any causes that aren't actual causes or aren't most detailed but flag them in the logs first!
invalid_causes <- setdiff(unique(dt$cause_id), cause_dt[most_detailed == 1, ]$cause_id)
# Don't remove neo_liver (level 3, not most detailed) when running PAFs for high BMI.

# add exceptions to the list of invalid causes for BMI & FPG
# BMI: Exception: 417 (neo_liver)
# FPG: Exception: 417 (neo_liver), 589 (CKD), 297 (TB)

if(370 %in% unique(dt$rei_id)){
  if(uniqueN(dt$rei_id) != 1) stop("There are multiple REIs in this run. Exception for cause_id 417 only implemented for BMI-only PAFs")
  # remove 417 (neo_liver) from invalid causes
  invalid_causes <- invalid_causes[invalid_causes != 417]
}
if(105 %in% unique(dt$rei_id)){
  if(uniqueN(dt$rei_id) != 1) stop("There are multiple REIs in this run. Exceptions only implemented for FPG-only PAFs")
  # remove 589 (CKD), 417 (neo_liver), and 297 (TB) from invalid causes
  invalid_causes <- invalid_causes[!(invalid_causes %in% c(589, 417, 297))]
}


if (length(invalid_causes) > 0) {
  warning("Dropping invalid causes found in PAFs for the following risk-cause pairs: ",
          paste(unique(dt[cause_id %in% invalid_causes, .(rei_id, cause_id)]), collapse = "-"))
  dt <- dt[!cause_id %in% invalid_causes, ]
}

# check again for missings, or numbers > 1
for (col in draw_cols) dt[is.na(get(col)), (col) := 0]
for (col in draw_cols) dt[get(col) > 1, (col) := .999999]

# APPLY CAUSE RESTRICTIONS -----------------------------------------------------

get_rstr <- function(type, dt) {
  message("Do not have age/sex restrictions yet for USHD, returning early with no changes.")
  return(dt)
  
  rstr <- get_restrictions(restriction_type = type, age_group_id = demo$age_group_id,
                           cause_id = unique(dt$cause_id), sex_id = demo$sex_id,
                           measure_id = c(3,4),
                           release_id = 16, cause_set_id = 2)
  if (type == "age") {
    rstr <- rstr[is_applicable == 1]
    setnames(rstr, "is_applicable", "drop")
    if(nrow(rstr) == 0) return(dt)
    dt <- merge(dt, rstr, by = c("age_group_id", "measure_id", "cause_id"), all.x = TRUE)
  } else {
    rstr[, drop := 1]
    if(nrow(rstr) == 0) return(dt)
    dt <- merge(dt, rstr, by = c(paste0(type, "_id"), "cause_id"), all.x = TRUE)
  }
  dt <- dt[is.na(drop), ]
  dt[, drop := NULL]
  return(dt)
}
message("Applying cause restrictions")
for(type in c("age","measure","sex")) {
  dt <- get_rstr(type, dt)
}

# JOINT PAF CALC ---------------------------------------------------------------
agg_results <- function(data, id_vars, value_vars, gbd_release_id, child_check = TRUE,
                        med = TRUE, mediation = NULL, add_uhc_reis = FALSE) {
  stop("Have not tested aggregation code yet!")
  
  rei_set_id <- 13
  
  # make rei map with parent and children risks
  rei_meta <- get_rei_metadata(rei_set_id = rei_set_id, release_id = gbd_release_id)
 
  # collapse hierarchical tree to two level tree, parent to all most-detailed children
  reis <- rei_meta[, .(parent_id=as.numeric(tstrsplit(path_to_top_parent, ","))),
                   by=c("rei_id", "most_detailed")]
  reis <- reis[most_detailed == 1 | rei_id == parent_id, ]
  
  format_custom_aggregates <- function(rei_lists) {
    return(lapply(names(rei_lists), function(x) {
      data.table(
        rei_id = c(as.integer(x), rei_lists[[x]]),
        most_detailed = c(0, rep(1, times = length(rei_lists[[x]]))),
        parent_id = as.integer(x)
      )
    }) %>% rbindlist(., use.names = TRUE))
  }
  
  # make sure parent doesn't already exist in the dataset
  rei_list <- reis[!(rei_id %in% unique(data$rei_id)), ]$rei_id %>% unique
  # check that all the children are present
  child_list <- reis[parent_id %in% rei_list & most_detailed == 1, ]$rei_id %>% unique
  child_exist <- data[rei_id %in% child_list, ]$rei_id %>% unique
  missing_list <- setdiff(child_list, child_exist)
  if(child_check & length(missing_list) != 0) {
    stop("The child reis [", paste(missing_list, collapse = ", "), "] are missing")
  }
  
  # loop
  calc_parent <- function(parent) {
    message("... aggregating rei ID ", parent)
    child_list <- reis[parent_id == parent & most_detailed == 1, ]$rei_id %>% unique
    agg <- merge(data, reis[rei_id %in% child_list & parent_id == parent, ],
                 by="rei_id", allow.cartesian=TRUE)
    # mediation
    if (med) {
      med_tmp <- mediation[rei_id %in% child_list & med_id %in% child_list, ]
      med_tmp <- merge(med_tmp, unique(agg[,.(rei_id,cause_id,measure_id,sex_id,age_group_id,most_detailed,parent_id)]),
                       by=c("rei_id","cause_id"), all.x=TRUE, allow.cartesian = TRUE)
      setnames(med_tmp, c("most_detailed","parent_id"),c("r_md","r_parent"))
      med_tmp <- merge(med_tmp, unique(agg[,.(rei_id,cause_id,measure_id,sex_id,age_group_id,most_detailed,parent_id)]),
                       by.x=c("med_id","cause_id","measure_id","sex_id","age_group_id"),
                       by.y=c("rei_id","cause_id","measure_id","sex_id","age_group_id"),
                       all.x=TRUE, allow.cartesian = TRUE)
      setnames(med_tmp, c("most_detailed","parent_id"),c("m_md","m_parent"))
      med_tmp <- med_tmp[m_parent == r_parent]
      setnames(med_tmp,"r_parent","parent_id")
      med_tmp <- med_tmp[, list(mean_mediation=1 - prod(1-mean_mediation)),
                         by = c("rei_id", "cause_id", "parent_id", "measure_id", "sex_id", "age_group_id")]
      agg <- merge(agg, med_tmp, by = c("rei_id", "cause_id", "parent_id", "measure_id", "sex_id", "age_group_id"), all.x = TRUE)
      # if 100% mediated, PAF -> 0
      agg[mean_mediation == 1, (draw_cols) := 0]
      # if otherwise mediated, use unmediated PAF
      agg[!is.na(mean_mediation) & mean_mediation != 1,
          (draw_cols) := lapply(1:n_draws, function(draw) (med_cols[draw] %>% get))]
    }
    # aggregation
    if (any(med_cols %in% names(agg))) agg[, (med_cols) := NULL]
    rei_collapse_vars <- c(id_vars[!id_vars %in% "rei_id"], "parent_id")
    agg <- agg[, lapply(.SD, function(x) 1-prod(1-x)), .SDcols = value_vars, by = rei_collapse_vars]
    setnames(agg, "parent_id", "rei_id")
    return(agg)
  }
  agg_full <- rbindlist(lapply(unique(reis[most_detailed == 0, ]$parent_id), calc_parent), use.names = TRUE)
  data <- rbindlist(list(data, agg_full), use.names = TRUE, fill = TRUE)
  data[, rei_id := as.integer(as.character(rei_id))]
  return(data)
}

if (do_aggregation){
  message("Calculating risk aggregate PAFs")
  dt <- agg_results(dt, id_vars = c("rei_id", "cause_id", "measure_id", "location_id",
                                    "year_id", "sex_id", "age_group_id"),
                    value_vars = draw_cols, gbd_release_id = gbd_release_id,
                    child_check = TRUE, mediation = mediation, med = TRUE,
                    add_uhc_reis = add_uhc_reis)
  
} else {
  message(sprintf("Only one risk subfolder found (%s). Skipping aggregation.", risks))
}

dt <- dt[, c("rei_id","cause_id", "location_id", "year_id", "age_group_id", "sex_id", "measure_id", extra_dim, draw_cols), with = F]

#  ADD PAFS OF 1 ---------------------------------------------------------------
message("Adding PAFs of 100%")
if (do_aggregation) {
  paf_one <- agg_results(paf_one, id_vars = c("rei_id", "cause_id"),
                         value_vars = "paf_one", gbd_release_id = gbd_release_id,
                         child_check = FALSE, med = FALSE, add_uhc_reis = add_uhc_reis)
}
paf_one[, location_id := location_id][, paf_one := NULL]
paf_one <- merge(paf_one, data.table(expand.grid(location_id = location_id,
                                                 year_id = year_ids,
                                                 race = extra_dim_values,
                                                 age_group_id = demo$age_group_id,
                                                 sex_id = demo$sex_id,
                                                 measure_id = c(3,4))),
                 by = "location_id", allow.cartesian = TRUE)
paf_one[, (draw_cols) := 1]

# Drop joint pafs of 1 if they slip through
dt <- merge(dt, unique(paf_one[,.(rei_id,cause_id)])[, paf_one :=1], by = c("rei_id", "cause_id"), all.x = TRUE)
dt <- dt[is.na(paf_one), ]
dt[, paf_one := NULL]
rei_ids <- dt$rei_id %>% unique
dt <- rbindlist(list(dt, paf_one[rei_id %in% rei_ids]), use.names =T )

dt <- dt[, c("rei_id","cause_id", "location_id", "year_id", "age_group_id", "sex_id", "measure_id", extra_dim, draw_cols), with = F]

# APPLY CAUSE RESTRICTIONS -----------------------------------------------------
for(type in c("age","measure","sex")) {
  dt <- get_rstr(type, dt)
}

# SAVE -------------------------------------------------------------------------
message("Saving output files")
# check again for missings, or numbers > 1
for (col in draw_cols) dt[is.na(get(col)), (col) := 0]
for (col in draw_cols) dt[get(col) > 1, (col) := .999999]

write_paf <- function(dt, y) {
  message("Saving year ID ", y)
  write.csv(dt[year_id == y, ], file=gzfile(paste0(run_directory, "/", location_id, "_", y, ".csv.gz")))
}
for (year_id in year_ids) write_paf(dt=dt, y=year_id)

if (!file.exists(paste0(run_directory, "/existing_reis.csv.gz"))){
  write_csv(unique(dt[, .(rei_id, cause_id, measure_id, sex_id)]),
            file=gzfile(paste0(run_directory, "/existing_reis.csv.gz")))
}
