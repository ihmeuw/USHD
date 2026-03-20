rm(list=ls())
library(data.table)
library(reshape2)
library(dplyr)

## Load settings -----------------------------------------------------------------------------------
###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (settings_loc <- commandArgs(TRUE)[[2]])
  (output_dir_draws_est <- commandArgs(TRUE)[[3]])
  (map_path <- commandArgs(TRUE)[[4]])
}

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  print(func)
  source(paste0(repo, "/functions/", func))
}

###### Assign settings from settings file
get_settings(settings_loc)
task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
combos <- fread(map_path)

#### Subset to year-race-sex-geo level combo
y <- combos[task_id, year]
r <- combos[task_id, race]
s <- combos[task_id, sex]
l <- as.character(combos[task_id, level])
paste0("\nPulling results for combos (year, race, sex, level): ", paste0(c(y, r, s, l), collapse = ", "), "\n", sep = "")

#### Create file name (all combined files with have 1000 at the end to indicate 1000 draws- NOT an imputation number)
name <- paste0(l, "_", y, "_", s, "_", r, "_", edu, ".rds")
draws_file <- paste0("FILEPATH")

#### Read in files for combo for each imp
draws <- rbindlist(lapply(1:n.imp, function(imp) {
  dt <- readRDS(paste0(output_dir_draws_est, gsub("IMP", imp, draws_file)))
  ## Below -> each file has sims 1-100 -> renumber so it is numbers 1-1000 so each row has a unique sim number
  dt[, sim := (imp - 1)*100 + sim]
}))

#### Save draws
saveRDS(draws, paste0(output_dir_draws_est, "/imputation0/draws/", paste0("draws_", name)))

#### Collapse draws and save est
if (by_source) {
  est <- collapse_draws(draws, "pred", c("level", "area", "year", "sex", "race", "age", "source_v2"))  
} else {
  est <- collapse_draws(draws, "pred", c("level", "area", "year", "sex", "race", "age"))
}

saveRDS(est, file=paste0(output_dir_draws_est, "/imputation0/est/", paste0("est_", name)))

if (length(outcome) > 1 | !is.null(names(outcome))) {
  outcome <- data.table("name" = names(outcome), "outcome" = outcome)
} else {
  outcome <- data.table("name" = "Single outcome", "outcome" = outcome)
}

if (outcome[1, 1] %like% 'pred_bmi' & l=='mcnty'){
  #### Create file name (all combined files with have 1000 at the end to indicate 1000 draws- NOT an imputation number)
  name_mcnty <- paste0("_", y, "_", s, "_", r, "_", edu, ".rds")
  draws_file_mcnty <- paste0("FILEPATH")
  
  #### Read in files for combo for each imp
  draws_mcnty <- rbindlist(lapply(1:n.imp, function(imp) {
    dt <- readRDS(paste0(output_dir_draws_est, gsub("IMP", imp, draws_file_mcnty)))
    ## Below -> each file has sims 1-100 -> renumber so it is numbers 1-1000 so each row has a unique sim number
    dt[, sim := (imp - 1)*100 + sim]
  }))

  #### Save draws  
  for (m in 0:3109){
    draws_sub <- draws_mcnty[area==m]
    dir.create(paste0(output_dir_draws_est, "/draws_mcnty/", m, "/"), recursive = T)
    saveRDS(draws_sub, paste0(output_dir_draws_est, "/draws_mcnty/", m, "/", paste0("draws_", m, name_mcnty)))
  }

}
