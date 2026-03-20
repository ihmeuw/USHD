###################################################################################################
## Description: Match General and SMART BRFSS files to get the most detailed geographic
##                  information possible from SMART while not dropping any participants
##                  in general BRFSS
##              1) Load SMART and General BRFSS files for each year after 2012
##                    (last year with LU data for all states)
##              2) Match respondents from general and SMART files
##              3) Create crosswalk from state/seqno from general to CBSA info
##                 in SMART
##                 
## Output:  Crosswalk from state/seqno from general to CBSA info in SMART
##           
###################################################################################################
# Set up ------------------------------------------------------------------------------------------
rm(list = ls())
library(data.table)
library(haven)

if (dir.exists(paste0('FILEPATH', Sys.info()['user']))) {
  repo_dir <- paste0('FILEPATH')
} else {
  repo_dir <- paste0('FILEPATH')
}
setwd(paste0(repo_dir, "1_data_prep/counties/survey_data/"))
source(paste0(repo_dir, "/0_functions/helper/_versioning_functions.R"))  # includes make_time_stamp() + get_git_status()
source(paste0(repo_dir, "/0_functions/load_ushd_db.R"))  # load USHD database

# set directories
in_root <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
out_root <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
parent_out_dir <- paste0(out_root, "FILEPATH")
years <- 2013:2021  # specify relevant years

# generate run date and create output directory
run_date <- make_time_stamp()
message(run_date)
out_dir <- paste0(parent_out_dir, run_date, "/")
message(out_dir)
dir.create(out_dir)

# save git history to output directory
cat(get_git_status(repo = repo_dir, "Risk Factor Repo", show_diff = T), file = paste0(out_dir, "git_info.txt"), sep="\n")

# read in BRFSS NID info
nids <- fread("FILEPATH")
smart_nids <- nids[source == "general" & year %in% years, list(year, nid)]  # PUF and SMART files are cataloged together

# read the spreadsheet with info about all files
layout <- suppressWarnings(fread("brfss_files_and_variables.csv", na.strings=""))
layout[layout==""]<-NA
smart_file_paths <- layout[SMART == 1 & year %in% years, list(year, file)]

# Functions ---------------------------------------------------------------------------------------
# load files and figure out extension
load_files <- function(filepath, n_rows = Inf){
  filetype <- sub('.*\\.', '', filepath)
  if (filetype == "csv" | filetype == "CSV") {
    data <- fread(filepath, stringsAsFactors = F, nrows = n_rows)
  } else if (filetype == "sas7bdat" | filetype == "SAS7BDAT") {
    data <- as.data.table(read_sas(filepath, n_max = n_rows))
  } else if (filetype == "DTA" | filetype == "dta") {
    data <- as.data.table(read_dta(filepath, n_max = n_rows))
  } else if (filetype == "XPT" | filetype == "xpt"){
    data <- as.data.table(read_xpt(filepath, n_max = n_rows))
  } else{
    stop(paste0("filetype '", filetype, "' not found"))
  }
  setnames(data, tolower(names(data)))
  return(data)
}

# Loop through years and match general and CBSA SMART  --------------------------------------------
# set up list to store crosswalks for each year
crosswalk <- vector(mode = "list", length = length(years))
names(crosswalk) <- as.character(years)

my_log <- file(paste0(out_dir, "match_smart_log_", format(Sys.time(), format = "%Y-%m-%d_%H:%M"), ".txt"))
sink(my_log, append = TRUE, type = "output", split = T)  # writing console output to log file
merged_years_cbsa <- lapply(years, function(a_year){
  print(paste("###############", a_year, "creating crosswalk ####################"))
  
  # load general and SMART files
  general <- load_files(layout[year == a_year & state_file == 0 & SMART == 0, file])
  smart <- load_files(layout[year == a_year & SMART == 1 & geography == "CBSA", file])
  
  print(general[, .N, by = "iyear"])
  
  # standardize vars in general BRFSS
  recode_general <- layout[SMART == 0 & state_file == 0 & year %in% years, .(year, state, iyear, msa)]
  
  setnames(general, recode_general[year == a_year, state], "state")
  setnames(general, recode_general[year == a_year, iyear], "year")
  setnames(general, recode_general[year == a_year, msa], "metro_status")
  
  if(a_year %in% c(2017, 2019)){  # segmentation fault issue occurred in these years only if used the full DT
    print("removing some cols for 2017 or 2019 (to avoid segmentation fault)")
    ncol <- ifelse(a_year == 2017, 159, 170)
    
    metro_col <- which(colnames(general) == "metro_status")
    general <- general[, c(1:ncol, metro_col), with = F]
  }
  
  # standardize vars in SMART  BRFSS
  recode_smart <- layout[SMART == 1 & year %in% years, .(cbsa, cbsa_wt, cbsa_name, ststr)]
  
  setnames(smart, recode_smart[years == a_year, cbsa], "mmsa")
  setnames(smart, recode_smart[years == a_year, cbsa_wt], "mmsa_wt")
  setnames(smart, recode_smart[years == a_year, cbsa_name], "mmsa_name")
  setnames(smart, recode_smart[years == a_year, ststr], "ststr")
  
  # get list of common variables between the sources (merge on these)
  vars <- intersect(names(general), names(smart))
  vars <- vars[which(!vars %in% c("seqno", "ststr", "_strstr", "_ststr", "_rfpap33", "hadhyst2"))]  # vars that will never be unique, or are occasionally causing differences (e.g., _rfpap33/ hadhyst2 in 2016)
  
  # bphigh4 
  # toldhi2   bphigh4 
  # subset the datasets to just the common vars and identifiers for faster merging
  general <- general[, c(vars, "seqno", "state", "year", "metro_status"), with = F]
  smart <- smart[, c(vars, "mmsa", "mmsa_wt", "mmsa_name", "seqno", "ststr"), with = F]
  setnames(smart, "seqno", "smart_seqno")
  setnames(smart, "ststr", "smart_ststr")
  
  # remove lastden3 var which was giving issues (but investigate the distribution first) -- was giving issues in CA 2016
  #     suspected miscoding of this variable in CA-provided data when compared to publicly available general BRFSS. 
  #     We decided to trust General BRFSS since this issue only arose for CA
  dentist_var <- grep("lastden", names(general), value = T)
  if(!identical(dentist_var, character(0))){
    # print("distribution of last time going to the dentist vars")
    # print(table(general[, dentist_var, with = F]))
    if(a_year == 2016){
      general[, (dentist_var) := NULL]
      vars <- vars[which(vars != dentist_var)]
    }
  }

  # remove BP vars in 2015, which were causing issues for HI
  # remove bphigh4 var which was giving issues (but investigate the distribution first) -- was giving issues in HI 2015
  bp_var <- grep("bphigh", names(general), value = T)
  if(!identical(bp_var, character(0))){
    # print("distribution of high blood pressure vars")
    # print(table(general[, bp_var, with = F]))
    if(a_year == 2015){
      general[, (bp_var) := NULL]
      vars <- vars[which(vars != bp_var)]
    }
  }
  # remove toldhi2  which was giving issues (but investigate the distribution first) -- was giving issues in HI 2015 
  toldhi_var <- grep("toldhi", names(general), value = T)
  if(!identical(toldhi_var, character(0))){
    # print("distribution of `toldhi2` vars")
    # print(table(general[, toldhi_var, with = F]))
    if(a_year == 2015){
      general[, (toldhi_var) := NULL]
      vars <- vars[which(vars != toldhi_var)]
    }
  }
  # remove bpmeds  which was giving issues (but investigate the distribution first) -- was giving issues in HI 2015 
  bpmeds <- grep("bpmeds", names(general), value = T)
  if(!identical(bpmeds, character(0))){
    # print("distribution of `bpmeds` vars")
    # print(table(general[, bpmeds, with = F]))
    if(a_year == 2015){
      general[, (bpmeds) := NULL]
      vars <- vars[which(vars != bpmeds)]
    }
  }
  
  # remove "_rfhype5" -- calculated high BP variable (also 2015)
  calc_bp_var <- grep("_rfhype5", names(general), value = T)
  if(!identical(calc_bp_var, character(0))){
    # print("distribution of `_rfhype` vars")
    # print(table(general[, calc_bp_var, with = F]))
    if(a_year == 2015){
      general[, (calc_bp_var) := NULL]
      vars <- vars[which(vars != calc_bp_var)]
    }
  }
  
  calc_chol_var <- grep("_rfchol", names(general), value = T)
  if(!identical(calc_chol_var, character(0))){
    # print("distribution of `_rfchol` vars")
    # print(table(general[, calc_chol_var, with = F]))
    if(a_year == 2015){
      general[, (calc_chol_var) := NULL]
      vars <- vars[which(vars != calc_chol_var)]
    }
  }
  # other vars to remove b/c they caused merge issues for HI 2015
  if(a_year == 2015){
    general[, c("cholchk", "_cholchk", "checkup1", "bloodcho") := NULL]
    vars <- vars[which(!vars %in% c("cholchk", "_cholchk", "checkup1", "bloodcho"))]
  }
  
  # There's one row in the SMART data that wouldn't merge with general; the difference
  #   was in two imputed age variables. Instead of excluded these vars from the variables
  #   to merge on, I will just update the values for this one row to match; this won't
  #   change the outcome these vars caused issues in Georgia 2015 relative to if
  #   I excluded these vars from the match, and it will prevent an additional 20 
  #   rows from being non-unique in general
  if(a_year == 2015){
    # general[, c("_age80", "_age_g") := NULL]
    # vars <- vars[which(!vars %in% c("_age80", "_age_g"))]
    gen_age80 <- general[state == 13 & seqno == 2015004414, `_age80`][[1]]
    gen_age_g <- general[state == 13 & seqno == 2015004414, `_age_g`][[1]]
    smart[mmsa == 12060  & smart_seqno ==2015001899, `:=`(`_age80` = gen_age80, 
                                                           `_age_g` = gen_age_g)]
  }
  
  # look into how many rows in general are not unique on the merging variables
  not_unique <- nrow(general[, vars, with = F])
  general <- setDT(general)
  unique_dt <- general[!general[duplicated(general[, ..vars])], on = vars] 
  unique <- nrow(unique_dt[, vars, with = F])
  rm(unique_dt)
  print(paste("There are", not_unique - unique, "not-unique rows in general BRFSS. (Just a note...keeping duplicated rows)"))
  
  # merge the datasets to get matches
  setkeyv(general, c(vars)) 
  setkeyv(smart, vars)
  merged <- merge(general[, general := 1], smart[, smart := 1], by=(vars), all = T, mult = "first", allow.cartesian = F)  # all=T is added to keep all respondents in general BRFSS (will have missing MMSA info AND add few SMART respondents that have duplicate info and were dropped in previous step) 
  
  # count how many SMART respondents could not be merged (the number should be small, ideally zero)
  cbsa_unmatched <- merged[smart == 1 & is.na(general), .N]
  print(paste("There are", cbsa_unmatched, "rows in SMART that could not be matched to general."))
  
  # remove all vars except for the row identifiers for general and the CBSA info from SMART
  #   (and which rows from SMART do not have unique matches to general, so we should keep)
  merged <- merged[, c("seqno", "state", "year", "mmsa", "mmsa_wt", "mmsa_name", "smart_seqno", "smart_ststr")]
  merged[, year := a_year]  # change year to survey year to be consistent with the handling of the rest of the dataset
  return(merged) 
})

crosswalk <- rbindlist(merged_years_cbsa)

# Address duplicates ------------------------------------------------------------------------------
# there are some rows in SMART that are identical to other rows (except for seqno)
#   and some rows in general that are identical to other rows (except for seqno).
#   To avoid increasing the overall number of rows in BRFSS after merging with the 
#   crosswalk, delete rows that contain a general respondent that has already been
#   matched with a SMART respondent. This may leave some respondents in SMART with 
#   > 1 match, and some respondents in smart with no matches (since if there are 
#   dups in general, there are also dups in SMART), but it's not a big concern 
#   because the responses of those respondents are identical.

dups <- duplicated(crosswalk[, .(year, mmsa, smart_seqno)])  ## THERE ARE PROBABLY SOME DUPS IN DIFFERENT MSAS -- this is probably why I didn't remove all of them; the reduced the number of rows by a lot b/c all NAs are considered dups
print(paste("removed", sum(dups), "duplicates based on year,mmsa, smart_seqno (the number is large b/c it includes all NAs removed)"))
crosswalk <- crosswalk[!dups]

# next, remove the duplicated general BRFSS respondents; arbitrarily remove the 
#   second instance of the duplicated respondent (based on state, year, seqno) because
#   the info from the SMART respondent its merged on should be equivalent (i.e, their
#   responses to questions won't vary; this could possibly make tiny changes to the 
#   overall number of respondents in certain MMSAs b/c the not-unique gen respondents
#   could theoretically be matched wtih not-unique SMART respondents in different locations)
#   IF we later find that the CBSAs respondents are assigned to, does not match with the state 
#   of the general BRFSS respondent, come back to this and see if there is an alternative approach
dups <- duplicated(crosswalk[, .(year, state, seqno)])
print(paste("removed", sum(dups), "duplicates based on year,state, seqno"))
crosswalk <- crosswalk[!dups]

# Save and upload results -------------------------------------------------------------------------
# upload metadata table to USHD database
nid_path_dict <- lapply(1:nrow(smart_file_paths), function(i) {list(smart_file_paths[i, file])})
names(nid_path_dict) <- smart_nids[, nid]
brfss_location_mapping_version_id <- save_brfss_geo_mapping(input_type = "smart",
                                                            nid_path_dict = nid_path_dict,
                                                            output_file_path = out_dir,
                                                            description = "add 2020 and 2021",
                                                            prev_issues = "none",
                                                            is_best = T)

# save output
saveRDS(crosswalk, file = paste0(out_dir, "general_smart_cbsa_crosswalk.rds"))
print("saved and uploaded crosswalk")
closeAllConnections()  # close connection to log file
