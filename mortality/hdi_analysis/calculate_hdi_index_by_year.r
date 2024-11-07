#####################################################################################
## Calculate HDI
#####################################################################################

# Load settings file and directories ------------------------------------------------
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
  out_dir <- "[PROJECT_DIRECTORY]"
  date_time_stamp <- "[DATE_TIME_STAMP]"
  yr1 <- 2008
  yr2 <- 2021
  by_percentile <- T
  age_group_labels <- "25-44xx45-64xx65-84xx85+"
  rep_id <- "repwtp12"
  outdir_name <- "[OUTPUT_DIRECTORY]"
} else {
  args <- commandArgs(trailingOnly = TRUE)
  out_dir <- args[1]
  date_time_stamp <- args[2]
  yr1 <- as.numeric(args[3])
  yr2 <- as.numeric(args[4])
  by_percentile <- as.logical(args[5])
  age_group_labels <- args[6]
  rep_id <- args[7]
  outdir_name <- args[8]
}

years <- yr1:yr2
age_group_labels <- strsplit(age_group_labels,"xx")[[1]]
age_map <- data.table()  # make an age map

for(ag_line in age_group_labels) {
  age_map <- rbind(age_map, data.table(age_group = ag_line,
                                       start_age = as.numeric(gsub("\\+","",strsplit(ag_line,"-")[[1]][1])),
                                       end_age = as.numeric(strsplit(ag_line,"-")[[1]][2])))


}

age_map[is.na(end_age), end_age := 99999]

## Make output directory
save_dir <- file.path(out_dir,outdir_name,date_time_stamp,"data_with_hdi/year_specific")
dir.create(save_dir, recursive = T)

## Read in data
data <- readRDS(file.path(out_dir,outdir_name,date_time_stamp,paste0("/prepped_for_hdi_", rep_id, ".rds")))

# save out the number of negative person weights
if(nrow(data[perwt <= 0]) > 0) {
  negative_perwt <- data[perwt <= 0][,list(negative_persons = sum(perwt),
                                          number_negative = .N), by="race"]
  total_wt <- data[,list(total_pop = sum(perwt),
                         total_rows = .N), by="race"]
  negative_perwt <- merge(negative_perwt, total_wt, by="race")
  negative_perwt[,pct_persons := negative_persons/total_pop*100]
  negative_perwt[,pct_rows := number_negative/total_rows*100]
  negative_perwt[,repetition := rep_id]

  dir.create(file.path(out_dir,outdir_name,date_time_stamp,"negative_person_weight_values"))
  saveRDS(negative_perwt, file.path(out_dir,outdir_name,date_time_stamp,"negative_person_weight_values",paste0("/negative_perwt_info_", rep_id, ".rds")))
}

for(ii in 1:nrow(age_map)) {
  as <- age_map[ii, start_age]
  ae <- age_map[ii, end_age]

  data[age_specific >= as & age_specific <= ae, age_group := age_map[ii, age_group]]
}

stopifnot(nrow(data[is.na(age_group)]) == 0)
gc()

# Function to calculate the weights, by year
calc_pop_weights <- function(dt, by_year) {
  if(by_year) {
    dt[,total_weight := sum(perwt), by="year"]
  } else {
    dt[,total_weight := sum(perwt)]
  }

  dt[,wt := perwt/total_weight]

  return(dt)
}

message("Calculating by percentile")
data <- calc_pop_weights(data, by_year = F)
gc()

# Function to get weights:
weight_and_get_percentile <- function(dt, var) {
  dt <- copy(dt)
  dt <- setorderv(dt, var, 1)

  dt[,index := cumsum(wt)] 

  # make sure the percentiles sum for each year
  if(abs(max(dt[, index])  - 1) > 0.000001) {
    stop(paste0("percentiles do not add to 1"))
  }

  # Now deal with ties
  message(paste0("Resolving ties"))

  dt[,count := .N, by = var]

  data_unique <- dt[count == 1]
  data_dup <- dt[count > 1]
  og_rows <- nrow(dt) # to make sure we have the same number of rows after deduplication
  data_dup[,index := median(index), by=var]

  dt <- rbind(data_unique, data_dup)
  stopifnot(nrow(dt) == og_rows)
  dt[,count := NULL]

  return(dt)
}

for(var in c("eduyrs", "hh_consumption", "ex_mean")) {
  message(paste0("Working on ",var))
  data <- weight_and_get_percentile(dt = data, var = var)

  # Label according to the variable
  setnames(data, "index", paste0(var,"_index"))
}

# Now calculate HDI as a geometric mean of the percentiles
data[,hdi := (eduyrs_index*ex_mean_index*hh_consumption_index)^(1/3)]

# remove weights and recalculate by year
data[,wt := NULL]
data[,total_weight := NULL]
data_copy <- copy(data)

for(ag in c("ALL","BY_AGE")) {
  data <- copy(data_copy)
  if(ag == "ALL") {
    message("Calculating HDI weights across ages, by year")
    data <- calc_pop_weights(data, by_year = T)
  } else if (ag == "BY_AGE") {
    message("Calculating HDI weights by age and year")
    total_data <- data.table()
    for(age_choice in age_group_labels) {
      total_data <- rbind(total_data,
                          calc_pop_weights(copy(data[age_group == age_choice]), by_year = T))
    }
    data <- copy(total_data)
    rm(total_data)
    gc()
  } else {
    stop("age option is not valid")
  }

  for(yr in years) {
    if(ag == "ALL") {
      # Now calculate percentile of hdi
      tmp <- weight_and_get_percentile(dt = data[year == yr], var="hdi")
      setnames(tmp, "index", "hdi_index")
      saveRDS(tmp, paste0(save_dir,"/hdi_calc", if(by_percentile) "_using_percentile","_",yr,"_ages_",ag,"_",rep_id, ".rds"))
    } else if (ag == "BY_AGE") {

      # Now calculate percentile of hdi, by age
      for(age_choice in age_group_labels) {
        tmp <- weight_and_get_percentile(dt = data[year == yr & age_group == age_choice], var="hdi")
        setnames(tmp, "index", "hdi_index")
        saveRDS(tmp, paste0(save_dir,"/hdi_calc", if(by_percentile) "_using_percentile","_",yr,"_ages_",age_choice,"_",rep_id, ".rds"))
      }
    }
  }
}
