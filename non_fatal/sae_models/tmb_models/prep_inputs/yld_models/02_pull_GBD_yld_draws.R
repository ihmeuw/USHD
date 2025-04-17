###############################################################################################################
## Description: Child script to pull draws of YLD outcomes by location for all-cause YLD models
###############################################################################################################

###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (sims <- commandArgs(TRUE)[[3]])
}

###### Set up
pacman::p_load(data.table, tidyverse, ggplot2, foreach, doParallel, tidyr, broom, INLA, doParallel)
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")

funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
sims <- as.integer(sims)
us_states_loc_id = c(523:573)

## Retrieve cause IDs
cause_metadata <- get_cause_metadata(release_id = 9, cause_set_id = 3) # cause_set_id 3: "GBD reporting"; set used in fatal modeling
included_causes <- c("inj_trans", "_unintent", "_intent", "_subs", "diab_ckd",
                     "_neuro", "_comm", "_neo", "cvd", "resp", "digest", "_mental", "skin", "_sense", "msk", "_otherncd")
causes <- cause_metadata[acause %in% included_causes]

icause_id <-  causes[task_id, cause_id]
# get acause_id that will be name of column value
acause_name <- causes[task_id, acause]
acause_name <- gsub("^\\_", "", acause_name) # delete leading underscores to make future operations easier
print(paste0("Loading draws for cause_id ", icause_id, ": ", causes[task_id, cause_name]))

locs <- fread("FILEPATH") # has fips column which = area in USHD metadata for states
locs <- locs[location_parent_id == 102] # subset to US states (location_parent_id is USA which is 102)
setnames(locs, "fips", "state")
setnames(locs, "location_name", "state_name")
locs <- locs[, list(state, state_name, location_id)]

gbd_age_ids <- get_ids("age_group")

start.time <- Sys.time()

dt <- get_draws(gbd_id_type = "cause_id",
                gbd_id = icause_id,
                source = "como",
                version_id = 1471,  # final GBD 2021 annual run, which has 500 draws
                measure_id = 3,
                metric_id = 3,
                location_id = us_states_loc_id,  # only get results for US
                sex_id = 1:2,
                age_group_id = c(28, 5:20, 30, 160), # age group bins for <1, 1-4, 5-10,..., 85+
                release_id = 9,
                year_id = 2009:2019,
                n_draws = sims,
                downsample = TRUE)

draw_cols <- paste0("draw_", (1:sims) - 1)
stopifnot(draw_cols %in% names(dt))

## Convert to long format + change metadata to match USHD data
long <- melt(dt, id.vars = c("age_group_id", "cause_id", "location_id", "year_id", "measure_id", "sex_id", "metric_id", "version_id"), 
             measure.vars = draw_cols, 
             value.name = paste0("yld_rate_", acause_name), 
             variable.name = "sim") 
rm(dt)

long[, sim := as.numeric(gsub("draw_", "", sim)) + 1]

long <- long[!(age_group_id %in% c(22, 27))]
long <- merge(long, gbd_age_ids, by = "age_group_id", all.x = T)
long[!(age_group_id %in% c(22, 27, 28)), age := as.integer(substr(age_group_name, 1, 2))]
long[age_group_id == 28, age := 0]
setnames(long, 
         c("year_id", "sex_id"), 
         c("year", "sex"))

long <- merge(long, locs, by = "location_id", all.x = T)

#### Subset
keep <- c("state", "state_name", "year", "age", "sex", "sim", paste0("yld_rate_", acause_name))
long <- long[, ..keep]

print(Sys.time() - start.time)

### Save mean estimate for mean modeling data set
setnames(long, paste0("yld_rate_", acause_name), "val")
mean <- long[, mean(val), by = c("state", "state_name", "year", "age", "sex")]
setnames(mean, "V1", paste0("yld_rate_", acause_name))
setnames(long, "val", paste0("yld_rate_", acause_name))

saveRDS(mean, file = paste0(output_dir, paste0("temp_mean_", "yld_rate_", acause_name, ".rds")))

yld_draws <- long

##### Save each sim separately to be combined with other covariates in that sim
registerDoParallel(cores = 4)
foreach(i = 1:sims) %dopar% {
  dt_sim <- yld_draws[sim == i]
  file_name <- paste0("temp_draw_", i, "_yld_rate_", acause_name, ".rds")
  saveRDS(dt_sim, file = paste0(output_dir, file_name))
}

