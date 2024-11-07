#####################################################################################
## Description: launcher script to submit jobs that will bring in IPUMS data, merge
##  pre-calculated lifespan estimates, calculate HDI, summarize HDI, and compare to 
##  previous HDI versions (the point estimates should be the same)
##
## Arguments:
##  * mortality_subfolder - location of your code.
##  * reprep_data - TRUE if you want to re-run HDI calculation (creates new time stamp)
##      and you don't want to use old results. THIS DOES NOT RE-RUN LIFESPAN ESTIMATES.
##      If you want to re-calculate lifespan, you need to re-launch launch_lt_hc_kannisto_extension
##  * model_dir - which USHD county/race and ethnicity model you want to use for lifespan.
#####################################################################################

# Load settings file and directories ------------------------------------------------
empir_funcs <- list.files("functions/")
for(func in empir_funcs) {
  source(paste0("functions/", func))
}

library(readstata13)
library(haven)
library(survey)
library(ggplot2)
library(RColorBrewer)
library(stats)
library(ggpubr)
library(sf)
library(gridExtra)
library(data.table)
library(mgsub)
library(dplyr)

library(ltcore, lib.loc = "FILEPATH")

user <- Sys.getenv("USER")
extract_helper <- paste0("[PATH_TO_CODE]/extract_ipums_helper.r")
calc_hdi_script <- paste0("[PATH_TO_CODE]/calculate_hdi_index_by_year.r")
summary_script <- paste0("[PATH_TO_CODE]/calculate_ui_from_individuals.r")
summary_helper <- paste0("[PATH_TO_CODE]/calculate_standard_errors_directly.r")

# if you want to calculate weighted percentiles instead of indices (indices are what are used by the UN)
  # script no longer accomodates both, so this has to be true
by_percentile <- T 
stopifnot(by_percentile == T) 

# re-create all of the data?
reprep_data <- T

if(!reprep_data) {
  date_time_stamp <- "[TIME_STAMP_FOR_VERSION]" 
} else {
  # name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
  date_time_stamp <- strsplit(mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3)),"\\.")[[1]][1]
}

years <- 2008:2021

# model to use for life expectancy
model_dir <- "[DIRECTORY_FOR_SMALL_AREA_MODEL]"
message(model_dir)

proj_dir <- "[PROJECT_DIRECTORY]"
outdir_name <- "[OUTPUT_DIRECTORY_NAME]"
dir.create(file.path(proj_dir, outdir_name, date_time_stamp))

# terminal age in life table
max_age <- 110

# race info
races <- c(2,4,5,6,7)
race_labels <- c("Latino", "Black", "White", "AIAN", "API")

# define age groups within which we want to calculate HDI
age_group_labels <- c("25-44", "45-64", "65-84", "85+")

# Crosswalk information for the mcnty-PUMA units
mc_puma_xwalk <- "[PUMA_TO_PUMA_MCNTY_CROSSWALK]"

message("submitting jobs to re-prep data")
weight_nums <- c(1:80)
rep_wgt_vars <- paste0("repwtp",weight_nums)
jids <- data.table(wgt_name = c("perwt",rep_wgt_vars))

convert_1 <- function(dt, col) {
  setnames(dt, col, "val")
  dt[val == 1, val := NA]
  setnames(dt, "val", col)
  dt
}

## Get IPUMS data and merge on life expectancy
jids[, prep_data := sbatch(code = extract_helper,
                           arguments = c(proj_dir, model_dir, outdir_name, max_age, wgt_name, date_time_stamp),
                           name = paste0("prep_data_", wgt_name),
                           fthread = 2,
                           m_mem_free = "50G",
                           h_rt = "02:00:00",
                           archive = T,
                           skip_if_exists = if (!reprep_data) paste0(proj_dir,"/",outdir_name,"/",date_time_stamp,"/prepped_for_hdi_", wgt_name, ".rds"),
                           project = "PROJECT",
                           queue = "QUEUE",
                           sgeoutput = file.path(proj_dir, outdir_name, date_time_stamp)),
     by = "wgt_name"]

jids <- convert_1(jids, "prep_data")

get_hdi_files <- function(proj_dir, outdir_name, date_time_stamp, years, age_group_labels, wgt_name) {
  files <- c()
  for(yr in years) {
    files <- c(files, paste0(proj_dir,"/",outdir_name,"/",date_time_stamp,"/data_with_hdi/year_specific","/hdi_calc",
                             if(by_percentile) "_using_percentile","_",yr,"_ages_",
                             age_group_labels[length(age_group_labels)],"_",wgt_name, ".rds"))
  }
  return(files)
}

# Calculate HDI
## then use job hold logic so that the HDI calculation holds on this previous job
jids[wgt_name == "perwt", calc_hdi := sbatch(code = calc_hdi_script,
                          arguments = c(proj_dir, date_time_stamp, min(years), max(years), by_percentile, 
                                        paste(age_group_labels, collapse="xx"),
                                        wgt_name, outdir_name),
                          name = paste0("calc_hdi_", wgt_name),
                          fthread = 2,
                          m_mem_free = "50G",
                          h_rt = "03:00:00",
                          archive = T,
                          hold = na.omit(prep_data),
                          skip_if_exists = if (!reprep_data) get_hdi_files(proj_dir, outdir_name, 
                                                                           date_time_stamp, years, age_group_labels, wgt_name),
                          project = "PROJECT",
                          queue = "QUEUE",
                          sgeoutput = file.path(proj_dir, outdir_name, date_time_stamp)),
     by = "wgt_name"]

jids <- convert_1(jids, "calc_hdi")

# Merge on the replicate info to the HDI data, in preparation for summarizing
jids[wgt_name == "perwt", summarize_new := sbatch(code = summary_script,
                          name = paste0("merge_replicates_on"),
                          arguments = c(file.path(proj_dir, outdir_name),
                                        date_time_stamp,
                                        min(years),
                                        max(years),
                                        by_percentile,
                                        paste(age_group_labels, collapse="xx"),
                                        1, # min replicate weight
                                        80, # max replicate weight
                                        mc_puma_xwalk,
                                        reprep_data,
                                        summary_helper,
                                        results_comparison_script),
                          hold = na.omit(calc_hdi),
                          queue = "QUEUE",
                          fthread = 2,
                          m_mem_free = "150G",
                          h_rt = "03:00:00",
                          archive = T,
                          project = "PROJECT",
                          sgeoutput = file.path(proj_dir, outdir_name, date_time_stamp)),
     by='wgt_name']

jids <- convert_1(jids, "summarize_new")

saveRDS(jids, file.path(proj_dir, outdir_name, date_time_stamp, "jids.rds"))
