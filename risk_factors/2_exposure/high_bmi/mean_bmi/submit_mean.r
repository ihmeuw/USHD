###############################################################################################################
## Description: Submit mean BMI model run. 
##
## Passed args: nonfatal_repo [character] -- location of nonfatal repository
##              output_dir [character] -- location for model outputs (data and plots)
##              output_dir [character] -- location for model outputs (draws and est files)
##              settings_loc [character] -- file path of settings file
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
  (nonfatal_repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (output_dir_draws_est <- commandArgs(TRUE)[[3]])
  (settings_file <- commandArgs(TRUE)[[4]])
  (sing_image <- commandArgs(TRUE)[[5]])
}

###### Source functions
funcs <- list.files(paste0(nonfatal_repo, "/functions/"))
for (func in funcs) {
  source(paste0(nonfatal_repo, "/functions/", func))
}

###### Assign settings from settings file
settings_path <- "FILEPATH"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc)

message('code imp')

risks_repo <- paste0('FILEPATH')

if (race_code_set =='old'){
  stopifnot(race_all == 9)
} else if (race_code_set =='db'){
  stopifnot(race_all == 1)
}

if (!exists("prep_inputs_file")) {
  prep_inputs_file <- "prep_data.r"
}

###### Create a table for holding job ids
jids <- CJ(level = c(area_var, names(geoagg_files)), year = years, sex = c(sexes, 3), race = c(races, race_all), imp = 1:n.imp)

###### Submit stage 1 jobs (data prep, models)
fthread <- 5
h_rt <- "03:00:00"
m_mem_free <- '50G'
auto_queue <- "QUEUE,QUEUE"

# Launch data prep
jids[sex != 3 & (race != race_all), prep_inputs := sbatch(code = paste0(risks_repo, "/", prep_inputs_file),
                                                          name = paste0("prep_inputs"),
                                                          arguments = c(output_dir, settings_file),
                                                          skip_if_exists = if (resub) paste0(output_dir, "/combined_dt.RDS"),
                                                          fthread = 3, m_mem_free = "30G", h_rt = "0-00:45:00", archive = TRUE,
                                                          queue = auto_queue, sgeoutput = output_dir,
                                                          sing_image = sing_image)]

Sys.sleep(30)

if (by_sex) {
  jids[sex != 3 & (race != race_all), fit_mod := sbatch(code = paste0(risks_repo, "/model_inla.r"),
                                                        name = paste0("fit_mod_", sex,'_', imp),
                                                        arguments = c(output_dir, output_dir_draws_est, settings_file, imp, sex),
                                                        fthread = 15, m_mem_free = '50G', queue = auto_queue, h_rt = "4:00:00",
                                                        hold = na.omit(c(unique(jids[sex != 3 & (!by_race | race != race_all)]$prep_inputs))),
                                                        skip_if_exists = if (resub) paste0(output_dir_draws_est, "/effects_draws", imp, "sex", sex, ".RDS"),
                                                        archive = FALSE, sing_image = sing_image,
                                                        sgeoutput = output_dir),
       by = 'imp,sex']
} else {
  jids[sex != 3 & (race != race_all), fit_mod := sbatch(code = paste0(risks_repo, "/model_inla.r"),
                                                        name = paste0("fit_mod_", 0,'_', imp),
                                                        arguments = c(output_dir, output_dir_draws_est, settings_file, imp, 0),
                                                        fthread = 15, m_mem_free = '50G', queue = auto_queue, h_rt = "4:00:00",
                                                        hold = na.omit(c(unique(jids[sex != 3 & (!by_race | race != race_all)]$prep_inputs))),
                                                        skip_if_exists = if (resub) paste0(output_dir_draws_est, "/effects_draws", imp, "sex", 0, ".RDS"),
                                                        archive = FALSE, sing_image = sing_image,
                                                        sgeoutput = output_dir),
       by = 'imp']
}

Sys.sleep(30)

#skip later steps when testing model fit at the state level    
if (isFALSE(fit_mod_only)) {
  # Generate draws
  e <- 1
  jids[sex != 3 & (race != race_all), gen_draws := sbatch(code = paste0(risks_repo, "/gen_draws/gen_draws.r"),
                                                          name = paste0("gen_draws_", race, "_", sex, "_", year),
                                                          arguments = c(output_dir_draws_est, settings_file, sex, race, e, year, output_dir),
                                                          hold = na.omit(c(unique(jids[sex != 3 & (race != race_all)]$fit_mod))),
                                                          fthread = 2, m_mem_free = "300G", h_rt = "0-5:00:00", archive = TRUE, # for 2 imputations, 200G and 2 hrs is enough; for all imps, probably need 450G
                                                          skip_if_exists = if (resub)  paste0(output_dir_draws_est, "/draws/draws_mcnty_", year, "_", sex, "_", race, "_0_", e, ".rds"),
                                                          queue = auto_queue, sgeoutput = output_dir,
                                                          sing_image = sing_image),
       by='sex,race,year']
  
  Sys.sleep(30)

  ##### Julienne files
  Sys.sleep(30)
  
  ## Submit stage 2 jobs (pred lts, aggregate by geography and sex, collapse and compile) ------------
  # Set up arguments for aggregation array jobs
  agg_races_args <- unique(jids[sex != 3, c("year", "sex")])
  agg_geos_args <- unique(jids[sex != 3 & level != area_var, c("year", "sex", "level", "race")])
  agg_sex_args <- unique(jids[, c("year", "level", "race")])
  agg_args <- list(agg_races = agg_races_args, agg_geos = agg_geos_args, agg_sex = agg_sex_args)
  
  saveRDS(agg_args, paste0(output_dir_draws_est,"/agg_args.rds"))
  
  # Aggregate races (by year, sex)
  mem_race <- 75
  h_race <- "0-06:00:00"
  
  jids[sex != 3, agg_races := sbatch(code = paste0(nonfatal_repo, "post_estimation/agg_races.R"),
                                     arguments = c(nonfatal_repo, output_dir, settings_loc, output_dir_draws_est, "pred"),
                                     name = paste0("agg_races"),
                                     hold = na.omit(c(unique(gen_draws))),
                                     fthread = 8, m_mem_free = paste0(mem_race, "G"), h_rt = h_race, archive = TRUE,
                                     project = "PROJECT", queue = auto_queue,
                                     sgeoutput = output_dir, sing_image = sing_image, array = paste0("1-", nrow(agg_races_args)), array_throttle = 100)]
  
  Sys.sleep(30)
  
  # Aggregate geographies (by level, year, sex, race)
  if (!is.null(geoagg_files)) {
    mem_geo <- 20
    h_geos <- "0-06:00:00"
    
    jids[sex != 3 & level != area_var, agg_geos := sbatch(code = paste0(nonfatal_repo, "post_estimation/agg_geos.R"),
                                                          arguments = c(nonfatal_repo, output_dir, settings_loc, output_dir_draws_est, "pred"),
                                                          name = paste0("agg_geos"),
                                                          hold = na.omit(c(unique(agg_races))),
                                                          fthread = 8, m_mem_free = paste0(mem_geo,"G"), h_rt = h_geos, archive = TRUE,
                                                          project = "PROJECT", queue = auto_queue,
                                                          sgeoutput = output_dir, sing_image = sing_image, array = paste0("1-", nrow(agg_geos_args)), array_throttle = 100)]
  }
  
  Sys.sleep(30)

  # Aggregate sexes (by level, year, race)
  mem_sex_mcnty <- 30
  mem_sex_oth <- 4
  h_sex_mcnty <- "0-05:00:00"
  h_sex_oth <- "0-02:00:00"
  
  jids[, agg_sex := sbatch(code = paste0(nonfatal_repo, "post_estimation/agg_sex.R"),
                           arguments = c(nonfatal_repo, output_dir, settings_loc, output_dir_draws_est, "pred"),
                           name = paste0("agg_sex"),
                           hold = na.omit(c(unique(agg_geos))),
                           fthread = 8, m_mem_free = ifelse(level[1] == "mcnty", paste0(mem_sex_mcnty,"G"),
                                                            paste0(mem_sex_oth,"G")),
                           h_rt = ifelse(level[1] == "mcnty", h_sex_mcnty, h_sex_oth), archive = TRUE,
                           project = "PROJECT", queue = auto_queue,
                           sgeoutput = output_dir, sing_image = sing_image, array = paste0("1-", nrow(agg_sex_args)), array_throttle = 100)]
  
  Sys.sleep(30)

  # Compile estimates
  # If any inputs have changed, re-run compile script.
  # Otherwise, skip these jobs.
  mem_comp <- 70
  
  jids[, compile := sbatch(code = paste0(nonfatal_repo, "post_estimation/compile_estimates.R"),
                           arguments = c(nonfatal_repo, output_dir, settings_loc, output_dir_draws_est, "pred"),
                           name = paste0("compile_estimates_"),
                           hold = na.omit(c(unique(agg_sex))),
                           fthread = 2, m_mem_free = paste0(mem_comp,"G"), h_rt = "0-02:00:00", archive = TRUE,
                           project = "PROJECT", queue = auto_queue,
                           sgeoutput = output_dir, sing_image = sing_image)]
  
  Sys.sleep(30)

  #### Plot maps and GBD compare
  jids[, plot_maps := sbatch(code = paste0(nonfatal_repo, "diagnostics/plot_maps_and_gbd_compare.R"),
                             arguments = c(nonfatal_repo, output_dir, settings_loc, output_dir_draws_est, 0),
                             name = paste0("plot_maps_"),
                             hold = na.omit(c(unique(jids$compile))),
                             fthread = 8, m_mem_free = "100G",
                             h_rt = "04:00:00", archive = T,
                             project = "PROJECT", queue = auto_queue,
                             sgeoutput = output_dir, sing_image = sing_image)]
  
  Sys.sleep(30)

  #### Plot results comparison
  jids[, plot_results := sbatch(code = paste0(nonfatal_repo, "/diagnostics/plot_results_comparison.R"),
                                arguments = c(nonfatal_repo, output_dir, settings_loc, "NULL", output_dir_draws_est, 0, FALSE, FALSE),
                                name = paste0("plot_results_"),
                                hold = na.omit(c(unique(jids$compile))),
                                fthread = 8, m_mem_free = "100G",
                                h_rt = "2-00:00:00", archive = T,
                                project = "PROJECT", queue = auto_queue,
                                sgeoutput = output_dir, sing_image = sing_image)]
  
  Sys.sleep(30)

  plot <- sbatch(code = paste0(risks_repo, "/plot_parameters_INLA.r"),
                 arguments = c(nonfatal_repo, output_dir, output_dir_draws_est, settings_file),
                 name = paste0("plot_inla"),
                 hold = na.omit(c(unique(jids$compile))),
                 fthread = 2, m_mem_free = "50G", h_rt = "01:00:00", archive = TRUE,
                 project = "PROJECT", queue = auto_queue,
                 sgeoutput = output_dir, 
                 sing_image = 'FILEPATH')
  
  Sys.sleep(30)

  raking_all <- sbatch(code = paste0(nonfatal_repo, "raking/run_risk_raking.r"),
                       arguments = output_dir_draws_est,
                       name = paste0("raking_all"),
                       hold = na.omit(c(unique(jids$compile))),
                       fthread = 2, m_mem_free = "50G", h_rt = "01:00:00", archive = TRUE,
                       project = "PROJECT", queue = auto_queue,
                       sgeoutput = output_dir, 
                       sing_image = 'FILEPATH')
}

## save list of job ids for Mike to build a cheap report with
job.ids <- vector("numeric")

# hard-coded list of jid columns created, in the order they're created
jid.cols <- c(
  "prep_inputs",
  "pred_draws",
  "fit_mod",
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

# Write a data file of just these JIDs so they're easy to work with
saveRDS(jids, paste0(output_dir, '/submit_single_cause_jids_', type, '.rds'))
