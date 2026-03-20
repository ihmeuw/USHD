
if (!interactive()) {
    (REI  <- as.numeric(commandArgs(TRUE)[[1]]))#(int) Id of the risk you are running PAFs for.
    (YEAR  <- commandArgs(TRUE)[[2]])# (str) step of decomp to run pafs for. step1-5, or iterative.
    (N_DRAWS  <- as.numeric(commandArgs(TRUE)[[3]]))# (str) what project to run jobs under on the cluster.
} else {
  #!/bin/bash
  
  # REI ID to calculate PAFs for
  #
  # rei_id    rei             rei_name
  #     85    air             Air pollution
  #     86    air_pm          Ambient particulate matter pollution
  #     87    air_hap         Household air pollution from solid fuels
  #     88    air_ozone       Ambient ozone pollution
  #    105    metab_fpg       High fasting plasma glucose
  #    107    metab_sbp       High systolic blood pressure
  #    370    metab_bmi_adult High body-mass index in adults
  #    380    air_pmhap       Particulate matter pollution
  #    404    air_no2         Nitrogen dioxide pollution
  REI=370
  
  # Number of draws in the input risk exposure/exposure sd models
  #   10 (testing)
  #  100 (test model)
  #  500 (new GBD standard)
  # 1000 (old standard)
  N_DRAWS=10
  
  # years to calculate PAFs for. This is in literal R notation
  YEAR=2009
}

# 'race' or 'edu'
EXTRA_DIM='race'


# Directory you want to save to
# NOTE: the PAF calculator *DELETES OLD RESULTS* before creating new ones
SAVE_TO='FILEPATH'

# Project to run jobs under
PROJECT='PROJECT'

# RAM to allocate to each sub-job.
# Mike ran 500 draws on the *optimized* inputs for 3 years and they ran at less than 5G
PAF_JOB_MEM='6G'

code <- "FILPEATH"
source(paste0(code, "/launch_paf.R"), echo=T)

launch_paf(rei_id = REI, year_id = YEAR, n_draws = N_DRAWS, cluster_proj = PROJECT, decomp_step = 'ushd', save_results = F, update_bmi_fpg_exp_sd=F, resume = FALSE, m_mem_free = PAF_JOB_MEM, ushd_extra_dim = EXTRA_DIM, base_dir = SAVE_TO)
