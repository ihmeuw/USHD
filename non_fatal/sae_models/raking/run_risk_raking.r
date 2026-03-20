# Goal: launch raking scripts and vetting from sae.shared

if (!interactive()) {
  (model_dir <- commandArgs(TRUE)[[1]])
} else {
  model_dir <- 'FILEPATH'
}

resume_raking_step1 <- FALSE
resume_raking_agg <- FALSE

# Note that we should add rei_id to the settings.yaml file (needed for the save_draws_and_estimates function)

bmi_raking_script <- paste0('FILEPATH')
bmi_agg_script <- paste0('FILEPATH')
bmi_check_raking <- paste0('FILEPATH')
bmi_raking_effect_trends <- paste0('FILEPATH')

###### Source functions
repo <- paste0("FILEPATH")
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

# check that the rake codes in settings files are updated if the model has been recoded to the new races
if(grepl("newrace", model_dir)){
  settings <- fread(paste0(model_dir, "/settings.csv"), header = F)
  stopifnot(setequal(eval(parse(text = settings[V1 == "races", V2])), c(2, 4, 5, 6, 7)))
}

# raking script expects draws/ests here: [model_dir]/draws/, but we save them 
# [model_dir]/imputation0/draws/. Fix this with a symlink:

system(paste0("ln -sfn ", sprintf("%s/imputation0/draws/", model_dir), " ", sprintf("%s/draws", model_dir)))
system(paste0("ln -sfn ", sprintf("%s/imputation0/est/", model_dir), " ", sprintf("%s/est", model_dir)))
dir.create(paste0(model_dir, "/check_raked_results/"))

# load the settings.yaml file

model_settings <- ModelSettings$from_dir(model_dir)

run_raking <- sbatch(code = bmi_raking_script,
                     name = "run_raking",
                     fthread = 5, m_mem_free = "20G", h_rt = "24:00:00", archive = TRUE,
                     queue = 'QUEUE', sgeoutput = model_dir,
                     arguments = c("--from_dir", model_dir, "--to_dir", "gbd", "--project", "PROJECT", "--queue", "QUEUE", "--from_geo", "mcnty", "--to_geo", "state", "--jobmon_resume", resume_raking_step1, "--fail_fast", TRUE),
                     sing_image = 'FILEPATH')

agg_raking <- sbatch(code = bmi_agg_script,
                     name = "agg_raking",
                     fthread = 5, m_mem_free = "20G", h_rt = "24:00:00", archive = TRUE,
                     hold = run_raking,
                     queue = 'QUEUE', sgeoutput = model_dir,
                     arguments = c("--dir", model_dir, "--queue", "QUEUE", "--jobmon_resume", resume_raking_agg),
                     sing_image = 'FILEPATH')

check_raking <- sbatch(code = bmi_check_raking,
                       name = "check_raking",
                       fthread = 5, m_mem_free = "100G", h_rt = "5:00:00", archive = TRUE,
                       hold = agg_raking,
                       queue = 'QUEUE', sgeoutput = model_dir,
                       arguments = c("--dir", model_dir, "--edu_race_comparison_dir", "NULL", "--plot_level", "state", "--initial_level", 0, "--terminal_level", 0, "--dont_recalc_validation", FALSE, 
                                     "--measures", "pred", "--delete_temp", FALSE, "--threads", 4, "--dir_with_data", model_dir),
                       sing_image = 'FILEPATH')

check_raking_trends <- sbatch(code = bmi_raking_effect_trends,
                       name = "raking_effect_trends",
                       fthread = 2, m_mem_free = "30G", h_rt = "1:00:00", archive = TRUE,
                       hold = agg_raking,
                       queue = 'QUEUE', sgeoutput = model_dir,
                       arguments = c("--model_dir", model_dir, 
                                     "--outcome_str", basename(dirname(model_dir)),
                                     "--check_years", model_settings$years),
                       sing_image = 'FILEPATH')
