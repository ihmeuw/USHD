

###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (sims <- commandArgs(TRUE)[[3]])
  (map_path <- commandArgs(TRUE)[[4]])
}

###### Load libraries and functions
pacman::p_load(data.table, tidyverse, ggplot2, foreach, doParallel, tidyr, broom, INLA, doParallel, boot)
library(lbd.loader, lib.loc = sprintf("FILEPATH", 
                                      R.version$major, 
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))

library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))

##### Get information for task
task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cov_map <- fread(map_path)
cov <- cov_map[task_num == task_id, indicator]
run_date <- cov_map[task_num == task_id, run_date_or_path]
sims <- as.numeric(sims)

##### Create race/ethn map to merge race_group with race var
race_mapping <- data.table(race_group = c("NH White", "NH Black", "NH AIAN", "NH API", "Hispanic"),
                           race = c(5, 4, 6, 7, 2))

##### Read in modeling object and take draws
model_dir <- get_covariate_version(cov)[is_best == 1, description]
model_dir <- substr(model_dir, regexpr("model folder ", model_dir)[1] + 13, nchar(model_dir))
mod_obj <- readRDS(paste0( model_dir, "/model.RDS"))
draws <- inla.posterior.sample(sims, mod_obj, seed = 12345)

##### Format and save each draw
registerDoParallel(3)
pred_draws <- foreach(i = 1:length(draws), .combine = 'cbind') %dopar% {
  # Subset to draws
  draw <- draws[i][[1]]$latent
  
  if (cov == "income_pc_by_race_ethn") {
    draw <- exp(draw)
  } else {
    draw <- inv.logit(draw)
  }
  
  # Subset to predictions
  pred_names <- grep("APredictor", row.names(draw), value = TRUE)
  draw <- as.data.table(draw[pred_names,])
  setnames(draw, "V1", cov)
  
  # Merge meta data (I think this is correct)
  preds <- fread(paste0(model_dir, "/preds.csv"))
  stopifnot(nrow(preds) == nrow(draw))
  draw <- cbind(preds, draw)
  rm(preds)
  
  # Format datatable and save
  setnames(draw, "mcnty", "area")
  draw[, level := "mcnty"]
  draw[, sim := i]
  draw <- merge(draw, race_mapping, by = "race_group", all.x = TRUE)
  draw[, race_group := NULL]
  file_name <- paste0("temp_draw_", i, "_", gsub("_by_race_ethn", "", cov), ".rds")
  retain_cols <- c("year", "area", "race", "level", "sim", cov)
  saveRDS(draw[, ..retain_cols], file = paste0(output_dir, "/", file_name))
}
