###############################################################################################################
## Description: Pull draws of each model indicator. Launches separate script for each set of covariates that have
##              different processes for uploading covariates. NOTE: Como version hard-coded in YLD (02) script
## Inputs:      File with best run dates of sae indicators (edit here: FILEPATH);
##              File above also has best run dates for sociodemographic estimates (where mean estimates are stored, separate from run dates)
##              Best run dates for sociodemographic covariates modeling objects (edit this script directly)
##              Sims number (edit this script directly)
###############################################################################################################

###### Set up
pacman::p_load(data.table, tidyverse, ggplot2, foreach, doParallel, tidyr, broom, INLA, doParallel)
source("FILEPATH")
source("FILEPATH")

repo <- paste0("FILEPATH")
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

###### Create directory to save data
run_date <- "2024_02_05_10_58_12"
output_dir <- paste0("FILEPATH")
dir.create(output_dir)

my_log <- file(paste0(output_dir, "/log.txt"))
sink(my_log, append = TRUE, type = "output", split = T) # Writing console output to log file

###### Set sims
sims <- 50

queue <- "auto"


#########################################################################################################
############## Self-reported health and disability vars #################################################
#########################################################################################################

###### Read in best run dates (though GBD data and mcnty ylls follow a different file path format) and launch job for each cause
run_dates <- fread("FILEPATH",  header = TRUE)
fwrite(run_dates, paste0(output_dir, "/best_run_dates.csv"))
sae_indics <- run_dates[indicator %in% c("diffrem", "diffcare", "diffphys", "diffmob", "diffsens", "freq_al", "depression", "asthma", "diabetes", "pain", "stress",
                                         "worry", "tooth_loss", "copd", "arthritis", "medicare_elig")]

###### Save
sae_indics[, task_num := 1:.N]
fwrite(sae_indics, c(paste0(output_dir, "/sae_indics_run_dates.csv")))

###### Array job settings
memory <- "400G"
threads <- 10
script <- paste0(repo, 'tmb_models/prep_inputs/yld_models/01_pull_sae_indic_draws.R')
n_jobs <- nrow(sae_indics)
h_rt <- "1-0:00:00"
archive <- T

###### set queue
auto_queue <- set_queue_dynamically(queue = queue, fthread = threads, m_mem_free = memory, h_rt = h_rt, archive = TRUE, priority = "RAM")

############ Launch array jobs
sae_indics_array <- sbatch(code = script,
       arguments = c(repo, output_dir, sims),
       name = "sae_indics_draws",
       fthread = threads, m_mem_free = memory,
       h_rt = h_rt, archive = archive,
       sgeoutput = output_dir,
       array = paste0("1-", n_jobs), 
       queue = auto_queue)


#########################################################################################################
############## GBD YLDs #################################################################################
#########################################################################################################

###### Array job settings
yld_causes <- c("inj_trans", "_unintent", "_intent", "_subs", "diab_ckd",
                     "_neuro", "_comm", "_neo", "cvd", "resp", "digest", "_mental", "skin", "_sense", "msk", "_otherncd")

memory <- "250G"
threads <- 10
script <- paste0(repo, 'tmb_models/prep_inputs/yld_models/02_pull_GBD_yld_draws.R')
n_jobs <- length(yld_causes)
h_rt <- "1-0:00:00"
archive <- T

###### set queue
auto_queue <- set_queue_dynamically(queue = queue, fthread = threads, m_mem_free = memory, h_rt = h_rt, archive = TRUE, priority = "RAM")

ylds <- sbatch(code = script,
       arguments = c(repo, output_dir, sims),
       name = "yld_draws",
       fthread = threads, m_mem_free = memory,
       h_rt = h_rt, archive = archive,
       sgeoutput = output_dir,
       array = paste0("1-", n_jobs), array_throttle = 20, 
       queue = auto_queue)


#########################################################################################################
############## USHD YLLs ################################################################################
#########################################################################################################

###### Array job settings
yll_causes <- c("inj_trans", "_unintent", "_intent", "_subs", "diab_ckd",
                     "_neuro", "_comm", "_neo", "cvd", "resp", "digest", "skin", "msk", "_otherncd") # make sure same length as in script, otherwise causes will be missed

memory <- "400G"
threads <- 4
script <- paste0(repo, 'tmb_models/prep_inputs/yld_models/03_pull_USHD_yll_draws.R')
n_jobs <- length(yll_causes)
h_rt <- "0-04:00:00"
archive <- T

###### set queue
auto_queue <- set_queue_dynamically(queue = queue, fthread = threads, m_mem_free = memory, h_rt = h_rt, archive = TRUE, priority = "RAM")

ylls <- sbatch(code = script,
       arguments = c(repo, output_dir, sims),
       name = "yll_draws",
       fthread = threads, m_mem_free = memory,
       h_rt = h_rt, archive = archive,
       sgeoutput = output_dir,
       array = paste0("1-", n_jobs), array_throttle = 50, 
       queue = auto_queue)


#########################################################################################################
############## Sociodemographic Covariates ##############################################################
#########################################################################################################

cov_map <- run_dates[!(indicator %in% sae_indics$indicator) & indicator != "pop_density"]
cov_map[, task_num := 1:.N]
map_path <- paste0(output_dir, "/cov_map.csv")
fwrite(cov_map, map_path)

memory <- "100G"
threads <- 3
script <- paste0(repo, 'tmb_models/prep_inputs/yld_models/04_pull_sociodemographic_cov_draws.R')
n_jobs <- nrow(cov_map)
h_rt <- "0-04:00:00"
archive <- T

###### set queue
auto_queue <- set_queue_dynamically(queue = queue, fthread = threads, m_mem_free = memory, h_rt = h_rt, archive = TRUE, priority = "RAM")

sd_covs <- sbatch(code = script,
       arguments = c(repo, output_dir, sims, map_path),
       name = "sd_covs_draws",
       fthread = threads, m_mem_free = memory,
       h_rt = h_rt, archive = archive,
       sgeoutput = output_dir,
       array = paste0("1-", n_jobs), 
       queue = auto_queue)


#########################################################################################################
############## Combine data into n.sims data sets #######################################################
#########################################################################################################

memory <- "30G"
threads <- 4
script <- paste0(repo, 'tmb_models/prep_inputs/yld_models/05_combine_all_covs_by_draw.R')
n_jobs <- copy(sims)
h_rt <- "0-04:00:00"
archive <- T

###### set queue
auto_queue <- set_queue_dynamically(queue = queue, fthread = threads, m_mem_free = memory, h_rt = h_rt, archive = TRUE, priority = "RAM")

sbatch(code = script,
       arguments = c(repo, output_dir, sims),
       name = "combine_draws",
       hold = c(sae_indics_array, ylds, ylls, sd_covs),
       fthread = threads, m_mem_free = memory,
       h_rt = h_rt, archive = archive,
       sgeoutput = output_dir,
       array = paste0("1-", n_jobs), array_throttle = 50, 
       queue = auto_queue)

#########################################################################################################
############## Create mean data set #####################################################################
#########################################################################################################
memory <- "200G"
threads <- 4
script <- paste0(repo, 'tmb_models/prep_inputs/yld_models/06_create_mean_dataset.R')
h_rt <- "0-04:00:00"
archive <- T

###### set queue
auto_queue <- set_queue_dynamically(queue = queue, fthread = threads, m_mem_free = memory, h_rt = h_rt, archive = TRUE, priority = "RAM")

sbatch(code = script,
       arguments = c(repo, output_dir),
       name = "mean_dataset",
       hold = c(sae_indics_array, ylds, ylls, sd_covs),
       fthread = threads, m_mem_free = memory,
       h_rt = h_rt, archive = archive,
       sgeoutput = output_dir,
       queue = auto_queue)
