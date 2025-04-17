###################################################################################################
## Description: Format population counts by education status, age, and sex from ACS IPUMS.
###################################################################################################

# Load settings file and directories ------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("population$", getwd())) setwd("FILEPATH")
library(readstata13)
library(haven)
library(survey)

in_dir <- "FILEPATH"
out_dir <- "FILEPATH"
graph_out_dir <- paste(out_dir, "graph/")
years <- 2005:2023
nids <- data.table(year = years, nid = c(412389, 412414, 412423, 412455, 412461, 106543, 127946,
                                         127951, 412499, 412531, 367726, 412538, 412541, 438795,
                                         520621, 520775, 520820, 567988, 567991))

# Load and prep ACS data --------------------------------------------------------------------------
extract_data <- function(year) {
  cat(paste0(year, "\n")); flush.console()
  
  cols <- c("year", "statefip", "puma", "perwt", "age", "sex", "educd")
  if (year <= 2017){
    file_path <- list.files(path = paste0(in_dir, year), pattern = "FILEPATH", full.names = T)
  }else{
    file_path <- list.files(path = paste0(in_dir, year), pattern = "FILEPATH", full.names = T)
    # two file versions exist for 2019
    if (year == 2019){
      file_path <- file_path[length(file_path)]
    }
  }
  
  dt <- as.data.table(read_dta(file_path, col_select = all_of(cols)))
  dt[, puma := 100000*as.integer(as.character(statefip)) + as.integer(as.character(puma))]
  dt[, agegp := cut(age, breaks = c(seq(15,85,5),105), right = FALSE)]
  dt[, sex := as.numeric(sex)]
  dt[educd %in% unique(dt$educd)[which(unique(dt$educd) > 100)], edu := "college grad"]
  dt[educd %in% unique(dt$educd)[which(unique(dt$educd) > 64 & unique(dt$educd) < 101)], edu := "some college"]
  dt[educd %in% unique(dt$educd)[which(unique(dt$educd) > 61 & unique(dt$educd) < 65)], edu := "HS grad"]
  dt[educd %in% unique(dt$educd)[which(unique(dt$educd) < 62)], edu := "less than HS"]
  dt[educd %in% unique(dt$educd)[which(unique(dt$educd) == 999)], edu := NA]
  dt[, edu := factor(edu, levels = c("less than HS", "HS grad", "some college", "college grad"))]
  
  dt <- dt[, list(pop = sum(perwt)), by = 'puma,year,sex,agegp,edu']

  # drop children and adolescents
  dt <- dt[!is.na(agegp)]
  dt[, list(pop = round(sum(pop)/1000000, 1)), year]
  dcast.data.table(dt[, round(sum(pop)/1000000, 1), by = 'year,edu,sex'], edu ~ year + sex)
  dcast.data.table(dt[, round(sum(pop)/1000000, 1), by = 'year,sex'], sex ~ year)
  
  return(list("data" = dt, "file_path" = file_path))
}

extractions <- lapply(years, extract_data)  # get data extractions + file paths
data <- rbindlist(lapply(1:length(extractions), function(i) {extractions[[i]]$data}), use.names = T)  # rbind all data objects from extractions
data[nids, on = "year", nid := i.nid]  # add NID column to data

# Save output -------------------------------------------------------------------------------------
setkeyv(data, c("puma", "year", "sex", "agegp", "edu"))

# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- lapply(1:length(extractions), function(i) {list(extractions[[i]]$file_path)})
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths

# save output
saveRDS(data, paste0(out_dir, date_time_stamp, ".rds"))