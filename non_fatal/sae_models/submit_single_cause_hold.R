###############################################################################################################
## Description: Submit nonfatal or risk factor model run.
##
## Passed args: repo [character] -- location of nonfatal repository
##              output_dir [character] -- location for model outputs (data and plots)
##              output_dir_draws_est [character] -- location for model outputs (draws and est files)
##              settings_loc [character] -- file path of settings file
##              queue [character] -- submission queue for sbatch jobs
##              sing_image [character] -- Singularity image for sbatch jobs
##
## Requires:    N/A
##
## Outputs:     submitted jobs for all processes required to estimate nonfatal or risk factor burden.
##              "runtime_info.txt" which includes information about provided settings, system
##                settings, and job IDs for submitted jobs.
##
###############################################################################################################

###############################################################################################################
########## 1. Initial setup ##########
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
  (run_date_hold <- commandArgs(TRUE)[[7]])
  (fold <- commandArgs(TRUE)[[8]])
  (imp <- commandArgs(TRUE)[[9]])
}

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

###### Assign settings from settings file
get_settings(settings_loc)

if (!exists("by_sex")) {
  by_sex <- TRUE
}

if (!exists("n.imp")) {
  n.imp <- 0
}

###### Output git status
get_git_status(repo = paste0("FILEPATH"), repo_name = "non_fatal", show_diff = TRUE)

#### Set output_dir for validation runs
if (!exists("n_folds")) {
  n_folds <- 1
}

###### Create a table for holding job ids
jids <- CJ(level = c(area_var, names(geoagg_files)), year = years, sex = c(sexes, 3), race = unique(c(races, 1)))

if (!is.null(geoagg_files)) { # we may not be able to aggregate in all years, so year-level combinations with no crosswalk need to be removed.
  for (this_level in names(geoagg_files)) {
    weights <- readRDS(geoagg_files[this_level])
    if ("year" %in% names(weights)) {
      jids <- jids[level != this_level | year %in% unique(weights$year),]
    }
    rm(weights)
  }
}

if (type %in% c("models_only", "all", "validation")) {
  ## Arguments for the prediction array job
  m_mem_free_pred <- 100
  if (by_race) m_mem_free_pred <- m_mem_free_pred * 3
  
  ## Arguments for the save array jobs
  m_mem_free_save <- 15
  if (by_race) m_mem_free_save <- m_mem_free_save * 3
  
  ## Common arguments across both array jobs
  fthread_array <- 4
  h_rt_array <- "0-04:00:00"
  
  # Predict area-sex-level (by sex, race)
  if (race_together) {
    # put a dummy in for the race argument
    # and skips if files for all races exist
    mem_pred <- 20
    if (by_race) mem_pred <- mem_pred * 10
    races_submit <- 99 # used for the array jobs
    
    if (by_sex) {
      ## Set queue
      auto_queue <- set_queue_dynamically(queue = queue, num_jobs = length(sexes), fthread = 4, m_mem_free = mem_pred, h_rt = "0-4:00:00", archive = TRUE, priority = "RAM")
      
      jids[sex != 3 & (!by_race | race != 1),
           pred_draws := sbatch(code = paste0(repo, model_class, "/pred_draws.R"),
                                arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, sex, races_submit, validate, resub, by_sex, draw_width, imp),
                                name = paste0("pred_draws_", sex, "_", imp),
                                skip_if_exists = if (resub) paste0(output_dir_draws_est, "/initial_sims_", sex, "_99_", edu, "_", imp, ".rds"),
                                fthread = 4, m_mem_free = paste0(mem_pred, "G"),
                                h_rt = "4:00:00", archive = T,
                                project = "PROJECT", queue = auto_queue,
                                sgeoutput = output_dir, sing_image = sing_image),
           by = "sex"]
      Sys.sleep(30)
    } else {
      ## Set queue
      auto_queue <- set_queue_dynamically(queue = queue, fthread = 4, m_mem_free = mem_pred, h_rt = "0-4:00:00", archive = TRUE, priority = "RAM")
      
      jids[sex != 3 & (!by_race | race != 1), "pred_draws"] <- sbatch(code = paste0(repo, model_class, "/pred_draws.R"),
                                                                      arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, 99, races_submit, validate, resub, by_sex, draw_width, imp),
                                                                      name = paste0("pred_draws_", imp),
                                                                      skip_if_exists = if (resub) paste0(output_dir_draws_est, "/initial_sims_99_", edu, "_", imp, ".rds"),
                                                                      fthread = 4, m_mem_free = paste0(mem_pred, "G"),
                                                                      h_rt = "4:00:00", archive = T,
                                                                      project = "PROJECT", queue = auto_queue,
                                                                      sgeoutput = output_dir, sing_image = sing_image)
      Sys.sleep(30)
    }
  }
  
  ## submit array jobs - two possible situations - one
  
  array_sub <- TRUE
  
  # if there are any pred_draws jobs that still need to be run, then evaluate if re-submission needs to happen
  # the function prep_draws_array_jobs will do this and then submit a job if needed
  if (array_sub) {
    # if pred_draws is not already done, then submit the array jobs to complete the prediction process
    
    if (by_sex) {
      for (s in sexes) {
        for (r in races_submit) {
          p_draws_job <- jids[!is.na(pred_draws) & sex == s]
          if (race_together) p_draws_job <- unique(p_draws_job[, .(pred_draws)])
          if (!race_together) p_draws_job <- unique(p_draws_job[race == r, pred_draws])
          
          if (nrow(p_draws_job) == 0) {
            p_draws_job <- NULL
          }
          
          prep_draws_array_jobs(repo = repo, settings_loc = settings_loc, dir = output_dir, sex = s, race = r, validate, resub, queue = queue, m_mem_free_pred = paste0(m_mem_free_pred, "G"),
                                m_mem_free_save = paste0(m_mem_free_save,"G"), fthread = fthread_array, h_rt = h_rt_array, draw_width,
                                p_draws_job, by_sex, dir_draws_est = output_dir_draws_est, imp = imp)
        }
      }
    } else {
      for (r in races_submit) {
        p_draws_job <- jids[!is.na(pred_draws)]
        if (race_together) p_draws_job <- unique(p_draws_job[, .(pred_draws)])
        if (!race_together) p_draws_job <- unique(p_draws_job[race == r, pred_draws])
        
        if (nrow(p_draws_job) == 0) {
          p_draws_job <- NULL
        }
        
        prep_draws_array_jobs(repo = repo, settings_loc = settings_loc, dir = output_dir, sex = 99, race = r, validate, resub, queue = queue, m_mem_free_pred = paste0(m_mem_free_pred, "G"),
                              m_mem_free_save = paste0(m_mem_free_save,"G"), fthread = fthread_array, h_rt = h_rt_array, draw_width,
                              p_draws_job, by_sex, dir_draws_est = output_dir_draws_est, imp = imp)
      }
    }
  }
}

# Now that draws have been generated, set by_sex to TRUE (even when the model was actually fit to both sexes) to maximize code reuse
by_sex <- TRUE

#### Pre aggregation plots
### Plot point estimates (reported from TMB) vs. input data
## Set queue
auto_queue <- set_queue_dynamically(queue = queue, fthread = 8, m_mem_free = "200G", h_rt = "1-00:00:00", archive = TRUE, priority = "RAM")

jids[, "plot_point_estimates_and_data"] <- sbatch(code = paste0(repo, "diagnostics/plot_point_estimates_and_data.R"),
                                                  arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, by_sex, imp),
                                                  name = "plot_point_estimates_and_data",
                                                  hold = na.omit(c(unique(jids$pred_draws), unique(jids$pred_sub), unique(jids$save_draws))),
                                                  fthread = 8, m_mem_free = "200G",
                                                  h_rt = "24:00:00", archive = TRUE,
                                                  project = "PROJECT", queue = auto_queue,
                                                  sgeoutput = output_dir, sing_image = sing_image)

### Plot point estimates (generated for all strata) vs. direct estimates
## Set queue
auto_queue <- set_queue_dynamically(queue = queue, fthread = 8, m_mem_free = "600G", h_rt = "1-00:00:00", archive = TRUE, priority = "RAM")

jids[, "plot_point_estimates_and_data_all_strata"] <- sbatch(code = paste0(repo, "diagnostics/plot_point_estimates_and_data_all_strata.R"),
                                                             arguments = c(repo, output_dir, output_dir_draws_est, settings_loc, by_sex, imp),
                                                             name = "plot_point_estimates_and_data_all_strata",
                                                             hold = na.omit(c(unique(jids$pred_draws), unique(jids$pred_sub), unique(jids$save_draws))),
                                                             fthread = 8, m_mem_free = "600G",
                                                             h_rt = "24:00:00", archive = TRUE,
                                                             project = "PROJECT", queue = auto_queue,
                                                             sgeoutput = output_dir, sing_image = sing_image)

## Set queue
auto_queue <- set_queue_dynamically(queue = queue, num_jobs = length(sexes), fthread = 8, m_mem_free = "100G", h_rt = "0-04:00:00", archive = TRUE, priority = "RAM")

jids[sex != 3 & (!by_race | race != 1), plot_random_effects := sbatch(code = paste0(repo, "diagnostics/plot_random_effects.R"),
                                                                      arguments = c(repo, output_dir, settings_loc, output_dir_draws_est, sex, imp),
                                                                      name = "plot_random_effects",
                                                                      fthread = 8, m_mem_free = "100G",
                                                                      h_rt = "04:00:00", archive = TRUE,
                                                                      project = "PROJECT", queue = auto_queue,
                                                                      sgeoutput = output_dir, sing_image = sing_image), 
     by = "sex"]
Sys.sleep(30)

## Submit stage 2 jobs (pred lts, aggregate by geography and sex, collapse and compile) ------------
# Aggregate races (by year, sex)
if (by_race) {
  mem_race <- 75
  if (n.sims == 1000) {
    mem_race <- mem_race*2
  }
  
  h_race <- "0-06:00:00"
  
  ## Set queue
  auto_queue <- set_queue_dynamically(queue = queue, num_jobs = nrow(unique(jids[, c("year", "sex")])), fthread = 8, m_mem_free = mem_race, h_rt = h_race, archive = TRUE, priority = "RAM")
  
  jids[sex != 3,
       agg_races := sbatch(code = paste0(repo, "post_estimation/agg_races.R"),
                           arguments = c(repo, output_dir, settings_loc, sex, year, output_dir_draws_est, imp),
                           name = paste0("agg_races_", sex, "_", year, "_", imp),
                           hold = na.omit(c(unique(pred_draws), unique(pred_sub), unique(save_draws))),
                           fthread = 8, m_mem_free = paste0(mem_race, "G"), h_rt = h_race, archive = TRUE,
                           project = "PROJECT", queue = auto_queue,
                           sgeoutput = output_dir, sing_image = sing_image),
       by = "year,sex"]
  Sys.sleep(30)
} else {
  jids[, agg_races := NA]
}

# Aggregate geographies (by level, year, sex, race)
if (!is.null(geoagg_files)) {
  mem_geo <- 20
  if (by_race & n.sims == 1000) mem_geo <- mem_geo*2
  h_geos <- "0-06:00:00"
  
  ## Set queue
  auto_queue <- set_queue_dynamically(queue = queue, num_jobs = nrow(unique(jids[, c("level", "year", "sex", "race")])), fthread = 8, m_mem_free = mem_geo, h_rt = h_geos, archive = TRUE, priority = "RAM")
  
  jids[sex != 3 & level != area_var,
       agg_geos := sbatch(code = paste0(repo, "post_estimation/agg_geos.R"),
                          arguments = c(repo, output_dir, settings_loc, level, sex, race, year, output_dir_draws_est, imp),
                          name = paste0("agg_geo_", level, "_", sex, "_", race, "_", year, "_", imp),
                          hold = na.omit(c(unique(pred_draws), unique(agg_races), unique(pred_sub), unique(save_draws))),
                          fthread = 8, m_mem_free = paste0(mem_geo,"G"), h_rt = h_geos, archive = TRUE,
                          project = "PROJECT", queue = auto_queue,
                          sgeoutput = output_dir, sing_image = sing_image),
       by = "level,year,sex,race"]
  Sys.sleep(30)
}

# Aggregate sexes (by level, year, race)
mem_sex_mcnty <- 30
if (by_race & n.sims == 1000) {
  mem_sex_mcnty <- mem_sex_mcnty * 2
}

mem_sex_oth <- 4
h_sex_mcnty <- "0-05:00:00"
h_sex_oth <- "0-02:00:00"

## Set queue
auto_queue <- set_queue_dynamically(queue = queue, num_jobs = nrow(unique(jids[, c("level", "year", "race")])), fthread = 8, m_mem_free = max(mem_sex_mcnty, mem_sex_oth), h_rt = max(h_sex_mcnty, h_sex_oth), archive = TRUE, priority = "RAM")

jids[, agg_sex := sbatch(code = paste0(repo, "post_estimation/agg_sex.R"),
                         arguments = c(repo, output_dir, settings_loc, level, race, year, output_dir_draws_est, imp),
                         name = paste0("agg_sex_", level, "_", race, "_", year, "_", imp),
                         hold = na.omit(c(unique(pred_draws), unique(agg_races), unique(pred_sub), unique(save_draws), unique(agg_geos))),
                         fthread = 8, m_mem_free = ifelse(level[1] %in% c("mcnty", "cbsa_mcnty", "puma_mcnty"), paste0(mem_sex_mcnty,"G"),
                                                          paste0(mem_sex_oth,"G")),
                         h_rt = ifelse(level[1] %in% c("mcnty", "cbsa_mcnty", "puma_mcnty"), h_sex_mcnty, h_sex_oth), archive = TRUE,
                         project = "PROJECT", queue = auto_queue,
                         sgeoutput = output_dir, sing_image = sing_image),
     by = "level,year,race"]
Sys.sleep(30)

# Compile estimates
# If any inputs have changed, re-run compile script.
# Otherwise, skip these jobs.
mem_comp <- 150
if (by_race & n.sims == 1000) {
  mem_comp <- mem_comp * 3
}

## Set queue
auto_queue <- set_queue_dynamically(queue = queue, fthread = 2, m_mem_free = mem_comp, h_rt = "0-01:00:00", archive = TRUE, priority = "RAM")

jids[, "compile"] <- sbatch(code = paste0(repo, "post_estimation/compile_estimates.R"),
                            arguments = c(repo, output_dir, settings_loc, output_dir_draws_est, FALSE, imp),
                            name = paste0("compile_estimates", "_", imp),
                            hold = na.omit(c(unique(jids$pred_draws), unique(jids$agg_races), unique(jids$agg_geos), unique(jids$agg_sex), unique(jids$pred_sub), unique(jids$save_draws))),
                            fthread = 2, m_mem_free = paste0(mem_comp,"G"), h_rt = "01:00:00", archive = TRUE,
                            project = "PROJECT", queue = auto_queue,
                            sgeoutput = output_dir, sing_image = sing_image)
Sys.sleep(30)

if (!(exists("plot_all_ages_races_only"))) {
  plot_all_ages_races_only <- TRUE
}

if (!grepl("state_model", model)) {
  #### Plot maps and GBD compare
  ## Set queue
  auto_queue <- set_queue_dynamically(queue = queue, fthread = 8, m_mem_free = "100G", h_rt = "0-04:00:00", archive = TRUE, priority = "RAM")
  
  jids[, "plot_maps"] <- sbatch(code = paste0(repo, "diagnostics/plot_maps_and_gbd_compare.R"),
                                arguments = c(repo, output_dir, settings_loc, output_dir_draws_est, plot_all_ages_races_only),
                                name = "plot_maps",
                                hold = na.omit(c(jids$compile)),
                                fthread = 8, m_mem_free = "100G",
                                h_rt = "04:00:00", archive = T,
                                project = "PROJECT", queue = auto_queue,
                                sgeoutput = output_dir, sing_image = sing_image)
  Sys.sleep(30)
  
  #### Plot results comparison
  ## Set queue
  conditional <- 'FALSE'
  
  auto_queue <- set_queue_dynamically(queue = queue, fthread = 8, m_mem_free = "100G", h_rt = "0-08:00:00", archive = TRUE, priority = "RAM")
  
  jids[, "plot_results"] <- sbatch(code = paste0(repo, "diagnostics/plot_results_comparison.R"),
                                   arguments = c(repo, output_dir, settings_loc, "NULL", output_dir_draws_est, imp, conditional),
                                   name = "plot_results",
                                   hold = na.omit(c(unique(jids$compile))),
                                   fthread = 8, m_mem_free = "100G",
                                   h_rt = "08:00:00", archive = T,
                                   project = "PROJECT", queue = auto_queue,
                                   sgeoutput = output_dir, sing_image = sing_image)
  
  if (outcome[[1]] %like% c('obese')){
    conditional <- 'TRUE'
    
    jids[, "plot_results_conditional"] <- sbatch(code = paste0(repo, "diagnostics/plot_results_comparison.R"),
                                                 arguments = c(repo, output_dir, settings_loc, "NULL", output_dir_draws_est, imp, conditional),
                                                 name = "plot_results",
                                                 hold = na.omit(c(unique(jids$plot_results))),
                                                 fthread = 8, m_mem_free = "100G",
                                                 h_rt = "08:00:00", archive = T,
                                                 project = "PROJECT", queue = auto_queue,
                                                 sgeoutput = output_dir, sing_image = sing_image)
  }
  Sys.sleep(30)
}

## save list of job ids
job.ids <- vector("numeric")

# hard-coded list of jid columns created, in the order they're created
jid.cols <- c(
  "plot_mod",
  "pred",
  "agg_races",
  "agg_geos",
  "agg_sex",
  "compile",
  "plot_maps"
)

for (jid.col in jid.cols) {
  if (jid.col %in% colnames(jids)) {
    # both 1 and NA are used as a stand-in for "no job submitted"
    vals <- na.omit(jids[[jid.col]])
    vals <- vals[vals != 1]
    job.ids <- c(job.ids, unique(vals))
  }
}

if (length(job.ids) == 0) {
  stop_or_quit("Queued 0 jobs - all work skipped", status = 0)
}

fwrite(list(jobs = job.ids), file = file.path(output_dir, paste0("runtime_info.latest_jobs.", type, ".txt")))

## Save runtime info to dir -------------------------------------------------------------------
info <- c("", "", "",
          paste("Time Submitted:", Sys.time()),
          paste("Runtime arguments:", output_dir, type, resub),
          "", "")

options(width = 500, max.print = 50000)
sink(file = paste0(output_dir, "/runtime_info.txt"), append = T)
cat(paste(info, collapse = "\n"))
data.frame(jids)
sink()

saveRDS(jids, paste0(output_dir, '/submit_single_cause_jids_', type, '.rds'))
