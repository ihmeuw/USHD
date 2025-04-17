###############################################################################################################
## Description: Submit post_estimation/combine_imputations.R, post_estimation/compile_estimates.R, diagnostics/plot_state_model_results_parent.R
##
## Passed args: repo [character] -- location of nonfatal repository
##              output_dir [character] -- location of model outputs (data and plots)
##              output_dir_draws_est [character] -- location for model outputs (draws and est files). same as output_dir for YLD models
##              settings_loc [character] -- file path of settings file
##              queue [character] -- submission queue for sbatch jobs
##              sing_image [character] -- Singularity image for sbatch jobs
##
## Outputs:     submitted jobs for all processes to combine imputations or draw-models, compiling estimates & plots
###############################################################################################################

###### Load required libraries
pacman::p_load(R.utils, data.table, stringr)

###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (output_dir_draws_est <- commandArgs(TRUE)[[3]])
  (settings_loc <- commandArgs(TRUE)[[4]])
  (queue <- commandArgs(TRUE)[[5]])
  (sing_image <- commandArgs(TRUE)[[6]])
}

funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

get_settings(settings_loc)

imp <- 1000


if (!exists("skip_to_aggregation")) {
  skip_to_aggregation <- FALSE
}

###### Create a table for holding job ids
jids <- CJ(level = c(area_var, names(geoagg_files)), year = years, sex = c(sexes, 3), race = unique(c(races, 1)))

###### Launch prediction jobs
## Arguments for the prediction array job
races_submit <- 99
m_mem_free_pred <- 50
if (by_race) m_mem_free_pred <- m_mem_free_pred * 3

## Arguments for the save array jobs
m_mem_free_save <- 15
if (by_race) m_mem_free_save <- m_mem_free_save * 3

## Common arguments across both array jobs
fthread_array <- 10
h_rt_array <- "0-04:00:00"

mem_pred <- 20
if (by_race) mem_pred <- mem_pred * 10

## Set queue
auto_queue <- "..."

jids[sex != 3 & (!by_race | race != 9), plot_random_effects := sbatch(code = paste0(repo, "diagnostics/plot_state_random_effects.R"),
                                                                      arguments = c(repo, output_dir, settings_loc, output_dir_draws_est, sex, 1000),
                                                                      name = paste0("plot_state_random_effects_", 1000),
                                                                      fthread = 8, m_mem_free = "50G",
                                                                      h_rt = "04:00:00", archive = FALSE,
                                                                      project = "PROJECT", queue = auto_queue,
                                                                      sgeoutput = output_dir, sing_image = sing_image),
     by = "sex"]
Sys.sleep(30)

combine_imps <- TRUE
array_sub <- TRUE

if (array_sub & !skip_to_aggregation) {
  # if pred_draws is not already done, then submit the array jobs to complete the prediction process
  if (by_sex) {
    for (s in sexes) {
      for (r in races_submit) {
        prep_draws_array_jobs(repo = repo, settings_loc = settings_loc, dir = output_dir, sex = s, race = r, validate, resub, queue = queue, m_mem_free_pred = paste0(m_mem_free_pred, "G"),
                              m_mem_free_save = paste0(m_mem_free_save,"G"), fthread = fthread_array, h_rt = h_rt_array, draw_width,
                              p_draws_job = NA, by_sex, dir_draws_est = output_dir_draws_est, imp = imp, combine_imps = TRUE)
      }
    }
  }
} else {
  jids[, pred_sub := NA] # when jid = 1 is used as a hold, sbatch will just be submitted
  jids[, save_draws := NA]
}

#### Check that all imps are present
missing_files <- list()
for (i in 1:n.imp) {
  folder <- paste0(dirname(output_dir_draws_est), "/imputation", i, "/")
  file1 <- paste0(folder, "/initial_sims_1_99_", i, "_1.rds")
  file2 <- paste0(folder, "/initial_sims_2_99_", i, "_1.rds")
  file3 <- paste0(folder, "/mean_1_99_", i, "_1.rds")
  file4 <- paste0(folder, "/mean_2_99_", i, "_1.rds")
  if (!file.exists(file1)) {
    missing_files[length(missing_files) + 1] <- file1
  }
  if (!file.exists(file2)) {
    missing_files[length(missing_files) + 1] <- file2
  }
  if (!file.exists(file3)) {
    missing_files[length(missing_files) + 1] <- file3
  }
  if (!file.exists(file4)) {
    missing_files[length(missing_files) + 1] <- file4
  }
}

if (length(missing_files) > 0) {
  warning("The following files are missing:")
  message(missing_files)
  quit()
} else {
  message("All initial_sims and mean files present")
}

###### Submit jobs to aggregate by geography and sex, collapse
# Aggregate races (by year, sex)

var_name <- "pred"

jids <- unique(jids[, list(level, year, sex, race, pred_sub, save_draws)])

jids[, imp := 1000]

jids$agg_races <- as.numeric(NA)
jids$agg_geos <- as.numeric(NA)
jids$agg_sex <- as.numeric(NA)

if (by_race) {
  mem_race <- 75
  if (n.sims == 1000) {
    mem_race <- mem_race*2
  }
  
  h_race <- "0-06:00:00"
  
  ## Set queue
  auto_queue <- "..."
  
  jids[sex != 3,
       agg_races := sbatch(code = paste0(repo, "post_estimation/agg_races.R"),
                           arguments = c(repo, output_dir, settings_loc, sex, year, output_dir_draws_est, var_name, 0),
                           name = paste0("agg_races_", sex, "_", year, "_", imp),
                           hold = na.omit(c(unique(save_draws))),
                           skip_if_exists = if (resub) paste0(output_dir_draws_est, "/est/est_mcnty_", year, "_", sex, "_1_0_", edu, ".rds"),
                           fthread = 8, m_mem_free = paste0(mem_race, "G"), h_rt = h_race, archive = FALSE,
                           project = "PROJECT", queue = auto_queue,
                           sgeoutput = output_dir, sing_image = sing_image),
       by = "year,sex,imp"]
  
  Sys.sleep(30)
}

# Aggregate geographies (by level, year, sex, race)
if (!is.null(geoagg_files)) {
  mem_geo <- 20
  mem_geo <- mem_geo*2
  h_geos <- "0-06:00:00"
  
  ## Set queue
  auto_queue <- "..."
  
  jids[sex != 3 & level != area_var,
       agg_geos := sbatch(code = paste0(repo, "post_estimation/agg_geos.R"),
                          arguments = c(repo, output_dir, settings_loc, level, sex, race, year, output_dir_draws_est, var_name, 0),
                          name = paste0("agg_geo_", level, "_", sex, "_", race, "_", year, "_", imp),
                          hold = na.omit(c(unique(agg_races))),
                          skip_if_exists = if (resub) paste0(output_dir_draws_est, "/est/est_", level, "_", year, "_", sex, "_", race, "_", 0, "_", edu, ".rds"),
                          fthread = 8, m_mem_free = paste0(mem_geo,"G"), h_rt = h_geos, archive = FALSE,
                          project = "PROJECT", queue = auto_queue,
                          sgeoutput = output_dir, sing_image = sing_image),
       by = "level,year,sex,race,imp"]
  
  Sys.sleep(30)
}

# Aggregate sexes (by level, year, race)
mem_sex_mcnty <- 30
mem_sex_mcnty <- mem_sex_mcnty * 2

mem_sex_oth <- 4
h_sex_mcnty <- "0-05:00:00"
h_sex_oth <- "0-02:00:00"

## Set queue
auto_queue <- "..."

jids[, agg_sex := sbatch(code = paste0(repo, "post_estimation/agg_sex.R"),
                         arguments = c(repo, output_dir, settings_loc, level, race, year, output_dir_draws_est, var_name, 0),
                         name = paste0("agg_sex_", level, "_", race, "_", year, "_", imp),
                         hold = na.omit(c(unique(agg_geos))),
                         skip_if_exists = if (resub) paste0(output_dir_draws_est, "/est/est_", level, "_", year, "_3_", race, "_", 0, "_", edu, ".rds"),
                         fthread = 8, m_mem_free = ifelse(level[1] == "mcnty", paste0(mem_sex_mcnty,"G"),
                                                          paste0(mem_sex_oth,"G")),
                         h_rt = ifelse(level[1] == "mcnty", h_sex_mcnty, h_sex_oth), archive = FALSE,
                         project = "PROJECT", queue = auto_queue,
                         sgeoutput = output_dir, sing_image = sing_image),
     by = "level,year,race,imp"]

Sys.sleep(30)

###### Compile estimates
mem_comp <- 15*3

auto_queue <- "..."

jids[, "compile"] <- sbatch(code = paste0(repo, "post_estimation/compile_estimates.R"),
                            arguments = c(repo, output_dir, settings_loc, output_dir_draws_est, FALSE, var_name, 0),
                            name = paste0("compile_estimates_", imp),
                            hold = na.omit(c(unique(jids$agg_sex))),
                            skip_if_exists = if (resub) paste0(output_dir_draws_est, "/est/pred_est_all.rds"),
                            fthread = 2, m_mem_free = paste0(mem_comp,"G"), h_rt = "01:00:00", archive = FALSE,
                            project = "PROJECT", queue = auto_queue,
                            sgeoutput = output_dir, sing_image = sing_image)

Sys.sleep(30)

###### Plot results comparison
## Set queue
auto_queue <- "..."

### line plots
plot_id <- sbatch(code = paste0(repo, "diagnostics/plot_state_model_results_parent.R"),
       arguments = c(repo, output_dir, settings_loc, output_dir_draws_est, imp),
       name = paste0("plot_results_", imp),
       hold = na.omit(unique(jids$compile)),
       fthread = 1, m_mem_free = "2G",
       h_rt = "0-04:00:00", archive = FALSE,
       project = "PROJECT", queue = auto_queue,
       sgeoutput = output_dir, sing_image = sing_image)

Sys.sleep(30)

#### Plot maps and GBD compare
## Set queue
auto_queue <- "..."

jids[, "plot_maps"] <- sbatch(code = paste0(repo, "diagnostics/plot_maps_and_gbd_compare.R"),
                              arguments = c(repo, output_dir, settings_loc, output_dir_draws_est, TRUE, 0),
                              name = "plot_maps",
                              fthread = 8, m_mem_free = "100G",
                              h_rt = "0-04:00:00", archive = FALSE,
                              project = "PROJECT", queue = auto_queue,
                              sgeoutput = output_dir, sing_image = sing_image)
