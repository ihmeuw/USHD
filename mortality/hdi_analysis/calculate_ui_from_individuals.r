#####################################################################################
## Merges on the replicate weights to the HDI results (calculated using just the perwt file)
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

if(interactive()) {
  save_dir <- "[HDI_OUTPUT_DIRECTORY]"
  date_time_stamp <- "[TIME_STAMP]"
  yr1 <- 2008
  yr2 <- 2021
  by_percentile <- T
  age_group_labels <- "25-44xx45-64xx65-84xx85+"
  min_weight <- 1
  max_weight <- 80
  mc_puma_xwalk <- "[CROSSWALK_FILE]"
  reprocess_data <- F
  summary_helper <- "[PATH_TO_CODE]/calculate_standard_errors_directly.r"
} else {
  args <- commandArgs(trailingOnly = TRUE)
  save_dir <- args[1]
  date_time_stamp <- args[2]
  yr1 <- as.numeric(args[3])
  yr2 <- as.numeric(args[4])
  by_percentile <- as.logical(args[5])
  age_group_labels <- args[6]
  min_weight <- args[7]
  max_weight <- args[8]
  mc_puma_xwalk <- args[9]
  reprocess_data <- as.logical(args[10])
  summary_helper <- args[11]
  check_results_helper <- args[12]
}

wgt_name <- "perwt"
rep_wgt_vars <- c(paste0("repwtp",c(min_weight:max_weight)))
age_group_labels <- strsplit(age_group_labels,"xx")[[1]]

save_dir <- file.path(save_dir, date_time_stamp, "data_with_hdi", "year_specific")
hdi_inputs_replicate_root <- dirname(dirname(save_dir))
message(paste0("Directory: ", save_dir))

# read in mcnty-puma crosswalk info
mc_puma_xwalk <- readRDS(mc_puma_xwalk)
# only keep the 2012-2019 version
mc_puma_xwalk <- mc_puma_xwalk[puma_version == "2012_2019"]

# check if the mcnty/PUMA/mcnty_PUMA combos change by year at all
unique_combos <- unique(mc_puma_xwalk[,.(mcnty,PUMA_ID,puma_mcnty)])
unique_combos[,count := .N, by=c("mcnty","PUMA_ID")]
stopifnot(nrow(unique_combos[count != 1]) == 0)

# confirmed that these do not change by year (once subset to a particular version)
# so, just using the unique combos now
unique_combos[,count := NULL]
unique_combos[,PUMA_ID := as.numeric(PUMA_ID)]

# Save mask
saveRDS(unique_combos, file.path(dirname(dirname(save_dir)),"puma_mcnty_combined_mapping.rds"))

#################################### Read in HDI data
if(reprocess_data) {
  # First, get the data with HDI calculated across all data
  hdi_files <- file.path(save_dir,
                         paste0("/hdi_calc", if(by_percentile) "_using_percentile",
                                "_",c(yr1:yr2),"_ages_ALL_",wgt_name, ".rds"))
  missing <- hdi_files[!file.exists(hdi_files)]
  if(length(missing) > 0) {
    message("The following files are missing:")
    cat(paste(missing, collapse="\n"))
    stop()
  }

  data <- rbindlist(lapply(hdi_files, readRDS), use.names=T)

  # Now get the data where we calculate HDI within each age group
  hdi_files_by_age <- c()
  for(ag in age_group_labels) {
    if(ag == "ALL") stop("Not supposed to read in all-age files here")
    hdi_files_by_age <- c(hdi_files_by_age, file.path(save_dir,
                                                      paste0("/hdi_calc", if(by_percentile) "_using_percentile",
                                                             "_",c(yr1:yr2),"_ages_",ag,"_",wgt_name, ".rds")))

  }

  if(length(hdi_files_by_age[!file.exists(hdi_files_by_age)]) > 0) stop("Missing files by age")

  ## read them in and save
  data_by_age <- rbindlist(lapply(hdi_files_by_age, readRDS))

  ### Create age groups for summaries by age group
  data[,age_group := factor(age_group, age_group_labels, age_group_labels)]
  data_by_age[,age_group := factor(age_group, age_group_labels, age_group_labels)]

  read_replicate <- function(wgt_name, root) {
    tmp <- readRDS(file.path(root,paste0("prepped_for_hdi_",wgt_name,".rds")))
    cols <- c("sample", "serial", "pernum", wgt_name, "race")
    tmp <- tmp[,(cols), with=F]
    setkeyv(tmp, c("sample","serial", "pernum", "race"))

    return(tmp)
  }

  # add the replicate information onto the full sample with HDI calculated
  og_rows <- nrow(data)
  stopifnot(nrow(data) == nrow(data_by_age))
  for(wgt_name in rep_wgt_vars) {
    message(wgt_name)
    setkeyv(data, c("sample","serial", "pernum", "race"))
    setkeyv(data_by_age, c("sample","serial", "pernum", "race"))

    rep_dt <- read_replicate(wgt_name, hdi_inputs_replicate_root)

    data <- data[rep_dt]
    stopifnot(nrow(data[is.na(perwt)]) == 0)
    stopifnot(nrow(data[is.na(get(wgt_name))]) == 0)
    stopifnot(nrow(data) == og_rows)

    data_by_age <- data_by_age[rep_dt]
    stopifnot(nrow(data_by_age[is.na(perwt)]) == 0)
    stopifnot(nrow(data_by_age[is.na(get(wgt_name))]) == 0)
    stopifnot(nrow(data_by_age) == og_rows)
  }

  # save out, since this is very computationally expensive
  saveRDS(data, file.path(dirname(save_dir),"combined_weights_all_age_hdi.rds"))
  saveRDS(data_by_age, file.path(dirname(save_dir),"combined_weights_age_grp_specific_hdi.rds"))

  rm(og_rows)
  gc()

} else {
  hdi_files <- file.path(save_dir,
                         paste0("/hdi_calc", if(by_percentile) "_using_percentile",
                                "_",c(yr1:yr2),"_ages_ALL_",wgt_name, ".rds"))
  # just read in 1 to get the variable names
  data <- readRDS(hdi_files[1])

}

races <- unique(data$race)
sexes <- unique(data$sex)

## now we need to summarize for each replicate - but across years
jids <- as.data.table(expand.grid(byage = c(T,F),
                                  var = c("eduyrs_index", "ex_mean_index", "hh_consumption_index","hdi_index"),
                                  calculation_type = c("proportion", "puma_mcnty", "weighted_avgs")))
jids[,var := as.character(var)]
jids[,calculation_type := as.character(calculation_type)]

jids <- jids[(calculation_type == "proportion" & var %in%  c("eduyrs_index", "ex_mean_index", "hh_consumption_index")) |
               (var == "hdi_index")]
jids <- jids[!(calculation_type == "puma_mcnty" & byage == T)]

### Launch calculate_hdi_summaries_helper for each race and sex, and then for each percentile
jids[, summarize := sbatch(code = summary_helper,
                           name = paste0("summarize_by_",var,"_byage_",byage,"_",calculation_type),
                           arguments = c(save_dir,
                                         date_time_stamp,
                                         yr1,
                                         yr2,
                                         by_percentile,
                                         var,
                                         paste(age_group_labels, collapse="xx"),
                                         1, # min replicate weight
                                         80, # max replicate weight
                                         2,# testing weight
                                         byage,
                                         calculation_type),
                           queue = "QUEUE",
                           fthread = 2,
                           m_mem_free = "200G",
                           h_rt = "24:00:00",
                           archive = T,
                           project = "PROJECT",
                           sgeoutput = file.path(dirname(dirname(save_dir)))),
     by='var,byage,calculation_type']
