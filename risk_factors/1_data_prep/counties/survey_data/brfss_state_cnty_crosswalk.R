####################################################################################################
## Description: Read in state-provided BRFSS files and create county & imputed county code tables
##              for merging during extractions. County values should be FIPS codes and are converted
##              here if necessary.
##
## Note: sometimes the 'iyear' variable is one year higher than the data year for interviews
##       conducted in December (e.g., an interview conducted in December 2019 may have an iyear
##       value of 2020). This will cause issues in our data, so we re-assign the year value in these
##       cases to match the first four digits of the seqno (which should reflect the data year).
##
####################################################################################################

## Setup --------------------------------------------------------------------------------------------
rm(list = ls())

library(data.table)
library(readxl)
library(haven)

if (dir.exists(paste0('FILEPATH', Sys.info()['user']))) {
  repo_dir <- paste0('FILEPATH', Sys.info()['user'], '/risk_factors/')
} else {
  repo_dir <- paste0('FILEPATH', Sys.info()['user'], 'FILEPATH')
}
setwd(paste0(repo_dir, "1_data_prep/counties/survey_data/"))
source(paste0(repo_dir, "/0_functions/helper/_versioning_functions.R")) # includes make_time_stamp() + get_git_status()
source(paste0(repo_dir, "/0_functions/load_ushd_db.R"))  # load USHD database

# set directories
LU <- ifelse(Sys.info()[["sysname"]] == "Windows", "FILEPATH", "FILEPATH")
parent_dir <- paste0(LU, "FILEPATH")
data_dir <- paste0(LU, "FILEPATH")
data_dir_ident <- paste0(LU, "FILEPATH")
run_date <- make_time_stamp()
message(run_date)
out_dir <- paste0(LU, "FILEPATH")
message(out_dir)
dir.create(out_dir)

# save git history to output directory
cat(get_git_status(repo = repo_dir, "Risk Factor Repo", show_diff = T), file = paste0(out_dir, "git_info.txt"), sep="\n")

# read in BRFSS NID info
nids <- fread("FILEPATH")
state_nids <- nids[!source %in% c("general", "LU_national")]

# load all FIPS codes from tigris package for validating
cnty_fips <- as.data.table(tigris::fips_codes)
cnty_fips[, c("state_code", "county_code") := list(as.numeric(state_code), as.numeric(county_code))]
cnty_fips[, is_valid := 1]  # set validity indicator

## Load data and create tables for each state -------------------------------------------------------
# Alaska 2014-2018
ak_file_paths <- data.table(year = 2014:2019, state = "ALASKA",
                            file = c(rep(paste0(data_dir, "AK/BRFSS_ALASKA_COUNTY_ID_2014_2018_Y2021M06D01.XLSX"), 5),
                                     paste0(data_dir, "AK/2019/USA_ALASKA_BRFSS_2019_COUNTY_Y2023M03D27.DTA")))
alaska <- setDT(read_xlsx(unique(ak_file_paths[year %in% 2014:2018, file])))
ak19 <- setDT(read_dta(ak_file_paths[year == 2019, file]))[, list(year = 2019, seqno, cnty = XCTY2013)]
setnames(alaska, old = c("XCTY2013", "SYEAR", "SEQNO"), new = c("cnty", "year", "seqno"))
alaska <- rbindlist(list(alaska, ak19), use.names = T); rm(ak19)
alaska[, match := as.numeric(substr(seqno, 1, 4)) == year]  # check that data years are accurate
stopifnot(unique(alaska$match == TRUE))  
alaska[, match := NULL]
alaska[, state := 2]  # manually assign state FIPS
alaska <- unique(alaska)  # drop duplicate rows

# Arizona 2009-2019 ---------------------------------------------------------------------------------
# 2011 has unknown FIPS code 510 - set to NA for now
# 2017 has string names for counties, plus some string numbers that don't seem to correspond to county FIPS
az_files <- list.files(paste0(data_dir, "AZ/", 2009:2019), pattern = "*.CSV", recursive = T, full.names = T)
arizona <- rbindlist(lapply(az_files, function(path) {
  yr <- as.numeric(substr(path, 60, 63))
  d <- fread(path)
  if (yr %in% 2011:2016) setnames(d, "CTYCODE1", "CTYCODE")  # make consistent for easy calculation
  if (yr == 2017) {
    d[CTYCODE2 == "Santa Cr", CTYCODE2 := "Santa Cruz"]  # fix county name
    d[, CTYCODE2 := paste(CTYCODE2, "County")]  # align name format with cnty_codes table for merging
    d[cnty_fips[state == "AZ"], on = c("CTYCODE2" = "county"), CTYCODE := i.county_code]  # convert names to FIPS codes
  }
  if (yr > 2017) setnames(d, "CTYCODE2", "CTYCODE")  # make consistent for easy calculation
  d <- unique(d[, list(seqno = as.numeric(SEQNO), year = yr, state = 4, cnty = as.numeric(CTYCODE))])
}), use.names = T)
az_file_paths <- data.table(year = 2009:2019, state = "ARIZONA", file = az_files)
stopifnot(all(az_file_paths[, year == as.numeric(substr(file, 60, 63))]))

# Arkansas 2011-2019 --------------------------------------------------------------------------------
# note: contains unknown county FIPS codes 169, 179, 405
ar_files <- list.files(paste0(data_dir, "AR/"), pattern = "*.DTA", full.names = T)
arkansas <- rbindlist(lapply(ar_files, function(path) {
  yr <- as.numeric(substr(path, 76, 79))
  d <- setDT(read_dta(path))
  if (TRUE %in% grepl("CTYCODE2", names(d))) setnames(d, "CTYCODE2", "CTYCODE1")  # make consistent for easy calculation
  d <- unique(d[, list(seqno = as.numeric(seqno), year = yr, state = 5, cnty = as.numeric(CTYCODE1))])
}), use.names = T)
ar_file_paths <- data.table(year = 2011:2019, state = "AR", file = ar_files)
stopifnot(all(ar_file_paths[, year == as.numeric(substr(file, 76, 79))]))

# California 2013-2019 ------------------------------------------------------------------------------
# note: CA state data doesn't give additional info over national data until 2013, so start there
ca_files <- list.files(paste0(data_dir, "CA/", 2013:2019), pattern = "*SAS7BDAT", recursive = T, full.names = T)
ca_cw_2013_file <- grep("CROSSWALKS", ca_files, value = T)  # isolate 2013 CA-to-CDC seqno crosswalk for merging
ca_files <- grep("CROSSWALKS", ca_files, value = T, invert = T)  # remove 2013 CA-to-CDC seqno crosswalk from data files
california <- rbindlist(lapply(ca_files, function(path) {
  yr <- as.numeric(substr(path, 60, 63))
  print(yr)
  d <- setDT(read_sas(path))
  setnames(d, sort(names(d)), sort(tolower(names(d))))  # make consistent for easy calculation
  d <- unique(d[, list(seqno = as.numeric(seqno), year = yr, state = 6, cnty = as.numeric(county1),
                       imp_cnty = ifelse("_impcty" %in% names(d), as.numeric(`_impcty`), NA_real_))])
}), use.names = T, fill = T)

ca_file_paths <- data.table(year = 2013:2019, state = "CA", file = ca_files)
stopifnot(all(ca_file_paths[, year == as.numeric(substr(file, 60, 63))]))

# match 2013 CA seqnos with their CDC counterparts
ca_cw_2013 <- setDT(read_sas(ca_cw_2013_file))
ca_cw_2013[, CA_SEQNO := as.numeric(CA_SEQNO)]
california <- merge.data.table(california, ca_cw_2013[, -"AGE"], by.x = "seqno", by.y = "CA_SEQNO", all.x = T)  # 506 seqnos do not have a CDC match
california[, seqno := ifelse(!is.na(CDC_SEQNO), CDC_SEQNO, seqno)]  # replace seqno with CDC seqno if present
california <- california[year != 2013 | !is.na(CDC_SEQNO)]  # drop observations not included in CDC crosswalk
california[, CDC_SEQNO := NULL]

# Connecticut 2000-2019 -----------------------------------------------------------------------------
# note: this file contains some FIPS for planning regions (25, 27) in 2011 and 2017
# this file also contains some NA seqnos - drop these to match what we're doing in CA
ct_files <- list.files(paste0(data_dir, "CT/", 2000:2019), pattern = "*.DTA", recursive = T, full.names = T)
connecticut <- rbindlist(lapply(ct_files, function(path){
  yr <- as.numeric(substr(path, 60, 63))
  print(yr)
  d <- setDT(read_dta(path))
  if (yr %in% c(2011:2013, 2015:2016)) setnames(d, "CTYCODE1", "ctycode")
  if (yr == 2014) setnames(d, "_IMPCTY", "ctycode")  # these are mostly real county values, so ok to reassign as cnty
  if (yr %in% 2017:2019) setnames(d, "CTYCODE2", "ctycode")
  d <- unique(d[, list(seqno = as.numeric(seqno), year = yr, state = 9, cnty = as.numeric(ctycode))])
}), use.names = T)
connecticut <- connecticut[!is.na(seqno)]
ct_file_paths <- data.table(year = 2000:2019, state = "CT", file = ct_files)
stopifnot(all(ct_file_paths[, year == as.numeric(substr(file, 60, 63))]))

# Delaware 2004-2019 --------------------------------------------------------------------------------
de_files <- list.files(paste0(data_dir, "DE/", 2004:2019), pattern = "*.CSV", recursive = T, full.names = T)
delaware <- rbindlist(lapply(de_files, function(path) {
  yr <- as.numeric(substr(path, 60, 63))
  d <- fread(path)
  d[, year := yr]
  d[, state := 10]
  setnames(d, "_region", "cnty")
  d[, cnty := car::recode(cnty, "1=1; 2=3; 3=5; else=NA")]  # convert to FIPS
  d <- unique(d[, list(seqno, year, state, cnty)])
}), use.names = T)
de_file_paths <- data.table(year = 2004:2019, state = "DE", file = de_files)
stopifnot(all(de_file_paths[, year == as.numeric(substr(file, 60, 63))]))

# Hawaii 2000-2019 ----------------------------------------------------------------------------------
hi_files <- list.files(paste0(data_dir, "HI/"), pattern = "*.CSV", full.names = T)
hawaii <- rbindlist(lapply(hi_files, function(path) {
  d <- fread(path)
  d <- unique(d[, list(seqno = as.numeric(ANNUAL_SEQ_NUM), year = as.numeric(SURVEY_YEAR), state = 15,
                       cnty = as.numeric(COUNTY_CD))])
}), use.names = T)
hi_file_paths <- data.table(year = 2000:2019, state = "HI", file = c(rep(hi_files[1], 11), rep(hi_files[2], 9)))
stopifnot(all(hi_file_paths[, between(year, as.numeric(substr(file, 78, 81)), as.numeric(substr(file, 83, 86)))]))

# remove HI 2004 - CDC excludes this year from PUF because data was only collected in a few months.
# see this link for more information: https://www.cdc.gov/brfss/annual_data/2004/pdf/compare_04.pdf
hawaii <- hawaii[year != 2004]
hi_file_paths <- hi_file_paths[year != 2004]

# Illinois 1996-2020 --------------------------------------------------------------------------------
# seems to be suppression of some counties in some years
il_files <- list.files(paste0(data_dir, "IL/"), pattern = "*.XLSX", full.names = T)
illinois <- rbindlist(lapply(il_files, function(path) {
  yr <- as.numeric(substr(path, 80, 83))
  d <- setDT(read_xlsx(path))
  if (yr < 1999) setnames(d, "_PSU", "SEQNO")  # make consistent for easy calculation
  if (yr %in% 2011:2016) setnames(d, "CTYCODE1", "CTYCODE")  # make consistent for easy calculation
  if (yr >= 2017) setnames(d, "CTYCODE2", "CTYCODE")  # make consistent for easy calculation
  d <- unique(d[, list(seqno = as.numeric(SEQNO), year = yr, state = 17, cnty = as.numeric(CTYCODE))])
}), use.names = T)
il_file_paths <- data.table(year = 1996:2020, state = "IL", file = il_files)
stopifnot(all(il_file_paths[, year == as.numeric(substr(file, 80, 83))]))

# Kansas 2000-2019 ----------------------------------------------------------------------------------
ks_files <- list.files(paste0(data_dir_ident, "KANSAS/", 2000:2019), pattern = "*.DTA", recursive = T, full.names = T)
kansas <- rbindlist(lapply(ks_files, function(path) {
  yr <- as.numeric(substr(path, 58, 61))
  d <- setDT(read_dta(path))
  if (yr %in% 2011:2016) setnames(d, "CTYCODE1", "ctycode")  # make consistent for easy calculation
  if (yr %in% 2017:2019) setnames(d, "CTYCODE2", "ctycode")  # make consistent for easy calculation
  d <- unique(d[, list(seqno = as.numeric(seqno), year = yr, state = 20, cnty = as.numeric(ctycode))])
}), use.names = T)
ks_file_paths <- data.table(year = 2000:2019, state = "KS", file = ks_files)
stopifnot(all(ks_file_paths[, year == as.numeric(substr(file, 58, 61))]))

# Massachusetts 2000-2020 ---------------------------------------------------------------------------
ma_files <- list.files(paste0(data_dir, "MA/", 2000:2020), pattern = "*.DTA", recursive = T, full.names = T)
massachusetts <- rbindlist(lapply(ma_files, function(path) {
  yr <- as.numeric(substr(path, 60, 63))
  d <- setDT(read_dta(path))
  if (yr %in% 2011:2016) setnames(d, "CTYCODE1", "ctycode")  # make consistent for easy calculation
  if (yr >= 2017) setnames(d, "CTYCODE2", "ctycode")  # make consistent for easy calculation
  d <- unique(d[, list(seqno = as.numeric(seqno), year = yr, state = 25, cnty = as.numeric(ctycode))])
}), use.names = T)
ma_file_paths <- data.table(year = 2000:2022, state = "MA", file = ma_files)
stopifnot(all(ma_file_paths[, year == as.numeric(str_extract(file, "(?<=_)\\d{4}(?=_)"))]))

# Mississippi 2000-2012 -----------------------------------------------------------------------------
ms_files <- list.files(paste0(data_dir_ident, "MISSISSIPPI"), pattern = "*.SAS7BDAT", full.names = T)
mississippi <- rbindlist(lapply(ms_files, function(path) {
  yr <- as.numeric(str_extract(path, "(?<=_)\\d{4}(?=_)"))
  print(paste0("MS ", yr))
  d <- setDT(read_sas(path))
  setnames(d, names(d), tolower(names(d)))  # make all variable names lowercase
  setnames(d, "ctycode1", "ctycode", skip_absent = T)  # set county variable name to 'ctycode'
  d <- unique(d[, list(seqno = as.numeric(seqno), year = yr, state = 28, cnty = as.numeric(ctycode))])
}), use.names = T)
ms_file_paths <- data.table(year = 2000:2012, state = "MISSISSIPPI", file = ms_files)
stopifnot(all(ms_file_paths[, year == as.numeric(str_extract(file, "(?<=_)\\d{4}(?=_)"))]))

# Nebraska 2011-2022 ------------------------------------------------------------------------------
# this is at the local health dept level rather than county: see https://dhhs.ne.gov/Pages/local-health-departments.aspx for county-HD map
# used above map and documentation to create a xwalk from LHD to county (one-to-many) located here:
nebraska <- setDT(read_xlsx(paste0(data_dir_ident,
                                   "NEBRASKA/USA_NEBRASKA_BRFSS_2011_2022_LOCAL_HEALTH_DEPARTMENT_EXTRACT_Y2024M11D12.XLSX"),
                            sheet = "Data"))
nebraska <- unique(nebraska[, list(seqno = as.numeric(`PSU (SEQN Number)`), year = as.numeric(`Survey Year`),
                                           state = 31, ne_lhd = as.numeric(`Local Health Department`))])
ne_file_paths <- data.table(year = 2011:2022, state = "NE",
                            file = paste0(data_dir_ident,
                                          "NEBRASKA/USA_NEBRASKA_BRFSS_2011_2022_LOCAL_HEALTH_DEPARTMENT_EXTRACT_Y2024M11D12.XLSX"))
stopifnot(ne_file_paths[year %in% 2011:2022, between(year, as.numeric(substr(file, 79, 82)), as.numeric(substr(file, 84, 87)))])

# New Hampshire 2001-2022 ---------------------------------------------------------------------------
nh_files <- list.files(paste0(data_dir_ident, "NEW_HAMPSHIRE"), pattern = "*.CSV", full.names = T)
new_hampshire <- rbindlist(lapply(nh_files, function(path) {
  yr <- as.numeric(str_extract(path, "(?<=_)\\d{4}(?=_)"))
  print(paste0("NH ", yr))
  d <- fread(path)
  d <- unique(d[, list(seqno = as.numeric(SEQNO), year = yr, state = 33, cnty = as.numeric(nhcty))])
}), use.names = T)
nh_file_paths <- data.table(year = 2001:2022, state = "NH", file = nh_files)
stopifnot(all(nh_file_paths[, year == as.numeric(str_extract(file, "(?<=_)\\d{4}(?=_)"))]))

# drop 2011 for high frequency of mismatched counties with national data
new_hampshire <- new_hampshire[year != 2011]
nh_file_paths <- nh_file_paths[year != 2011]

# New Mexico 2000-2021 ------------------------------------------------------------------------------
# Note: original 2015-2019 files were missing county detail. We received new ones but the old ones
# are still in the same directory, so be careful not to accidentally load those
# 2020 appears to be missing county detail; reached out to Hannah 11/1/24 for a replacement file containing ctycode2nm
# We have 2022 data but only at the region level, so leave out for now
nm_files <- list.files(paste0(data_dir, "NM/", c(2000:2019, 2021)), pattern = "*.DTA", recursive = T, full.names = T)
nm_files <- grep(paste0(2015:2019, "_COUNTY_Y2021", collapse = "|"), nm_files, invert = T, value = T)  # remove faulty 2015-2019 files
new_mexico <- rbindlist(lapply(nm_files, function(path) {
  yr <- as.numeric(substr(path, 60, 63))
  d <- setDT(read_dta(path))
  if (yr == 2000) {  # seqno variable exists but is empty; use psu instead (identical in other years)
    d[, seqno := NULL]
    setnames(d, "_psu", "seqno")
  }
  if (yr == 2008) setnames(d, "ctycodenm", "ctycode")  # make consistent for easy calculation
  if (yr %in% 2011:2016) setnames(d, "ctycode1", "ctycode")  # make consistent for easy calculation
  if (yr %in% 2017:2020) setnames(d, "ctycode2", "ctycode")  # make consistent for easy calculation
  d <- unique(d[, list(seqno = as.numeric(seqno), year = yr, state = 35, cnty = as.numeric(ctycode))])
}), use.names = T)
nm_file_paths <- data.table(year = 2000:2019, state = "NM", file = nm_files)
stopifnot(all(nm_file_paths[, year == as.numeric(substr(file, 60, 63))]))

# New York 2011-2019 --------------------------------------------------------------------------------
ny_files <- list.files(path = paste0(data_dir, "NY/", 2011:2018), pattern = "*dta", ignore.case = T, full.names = T)
ny_files <- append(ny_files, list.files(path = paste0(data_dir, "NY/2019"), pattern = "*.CSV", full.names = T))
new_york <- rbindlist(lapply(ny_files, function(path) {
  yr <- as.numeric(substr(path, 60, 63))
  if (yr >= 2019) {
    d <- fread(path)
  } else {
    d <- setDT(haven::read_dta(path))
  }
  
  if (yr %in% 2011:2016) setnames(d, "CTYCODE1", "ctycode")  # make consistent for easy calculation
  if (yr %in% 2017:2019) setnames(d, "CTYCODE2", "ctycode")  # make consistent for easy calculation
  d <- unique(d[, list(seqno = as.numeric(`_PSU`), year = yr, state = 36, cnty = as.numeric(ctycode),
                       imp_cnty = as.numeric(`_IMPCTY`))])
}), use.names = T, fill = T)
ny_file_paths <- data.table(year = 2011:2019, state = "NY", file = ny_files)
stopifnot(all(ny_file_paths[, year == as.numeric(substr(file, 60, 63))]))

# Pennsylvania 2000-2019 ----------------------------------------------------------------------------
pa_files <- list.files(paste0(data_dir, "PA/", 2000:2019), pattern = "*.DTA", recursive = T, full.names = T)
pennsylvania <- rbindlist(lapply(pa_files, function(path) {
  yr <- as.numeric(substr(path, 60, 63))
  d <- setDT(read_dta(path))
  if (yr %in% 2011:2016) setnames(d, "CTYCODE1", "ctycode")  # make consistent for easy calculation
  if (yr %in% 2017:2019) setnames(d, "CTYCODE2", "ctycode")  # make consistent for easy calculation
  d <- unique(d[, list(seqno = as.numeric(seqno), year = yr, state = 42, cnty = as.numeric(ctycode))])
}), use.names = T)
pa_file_paths <- data.table(year = 2000:2019, state = "PA", file = pa_files)
stopifnot(all(pa_file_paths[, year == as.numeric(substr(file, 60, 63))]))

# South Dakota 2003-2020 ----------------------------------------------------------------------------
# although the county variable is 'impcty' (implying that values are imputed), we are moving forward
sd_file_paths <- data.table(year = 2003:2020, state = "SD",
                            file = paste0(data_dir, "SD/BRFSS_SOUTH_DAKOTA_2003_2020_COUNTY_Y2021M11D24.DTA"))
south_dakota <- setDT(read_dta(unique(sd_file_paths$file)))
south_dakota[, match := as.numeric(substr(seqno, 1, 4)) == iyear]  # check that data years are accurate
stopifnot(unique(south_dakota$match == TRUE))  
south_dakota[, match := NULL]
south_dakota <- unique(south_dakota[, list(seqno = as.numeric(seqno), year = iyear, state = 46,
                                           cnty = as.numeric(impcty))])

# Texas 2000-2019 -----------------------------------------------------------------------------------
# 2000, 2001, 2006, 2010-12 do not have cfips
#    2000-01 have tdhcnty (coded 1-254 in alphabetical order)
#    2006 has c11q12 (777 = dk, 999 = rf, otherwise fips)
#    2010 has c12q13
#    2011 has c08q13
#    2012 has c07q13
# note: TX dept of health county codes (tdhcnty) are assigned alphabetically just like FIPS, so we calculate FIPS
# with the formula FIPS = 2*tdhcnty-1 (e.g., Gray County has TDH code 90 and FIPS 179)
tx_files <- list.files(paste0(data_dir, "TX/", 2000:2019), pattern = "*.DTA", recursive = T, full.names = T)
texas <- rbindlist(lapply(tx_files, function(path) {
  yr <- as.numeric(substr(path, 60, 63))
  d <- setDT(read_dta(path))
  setnames(d, sort(names(d)), sort(tolower(names(d))))  # make consistent for easy calculation
  
  if (yr %in% 2000:2001) {  # assign FIPS
    d[, cnty := 2*as.numeric(tdhcnty)-1]
  } else if (yr == 2006) {
    d[, cnty := as.numeric(c11q12)]
  } else if (yr == 2010) {
    d[, cnty := as.numeric(c12q13)]
  } else if (yr == 2011) {
    d[, cnty := as.numeric(c08q13)]
  } else if (yr == 2012) {
    d[, cnty := as.numeric(c07q13)]
  } else {
    d[, cnty := as.numeric(cfips)]
  }
  
  d <- unique(d[, list(seqno = as.numeric(seqno), year = yr, state = 48, cnty)])
}), use.names = T)
tx_file_paths <- data.table(year = 2000:2019, state = "TEXAS", file = tx_files)
stopifnot(all(tx_file_paths[, year == as.numeric(substr(file, 60, 63))]))

# Vermont 2000-2019 ---------------------------------------------------------------------------------
vt_file_paths <- data.table(year = 2000:2019, state = "VT")
vt_file_paths[, file := ifelse(year %in% 2000:2010,
                               paste0(data_dir, "VT/BRFSS_VERMONT_2000_2010_COUNTY_DATA_Y2021M07D14.DTA"),
                               paste0(data_dir, "VT/BRFSS_VERMONT_2011_2019_COUNTY_DATA_Y2021M07D14.DTA"))]
vermont_00 <- setDT(read_dta(vt_file_paths[year == 2000, file]))
vermont_00 <- vermont_00[, list(iyear, ctycode, `_PSU`)]
setnames(vermont_00, old = c("iyear", "ctycode", "_PSU"), new = c("year", "cnty", "seqno"))
vermont_11 <- setDT(read_dta(vt_file_paths[year == 2011, file]))
vermont_11 <- vermont_11[, list(iyear, CTYCODE2, `_PSU`)]
setnames(vermont_11, old = c("iyear", "CTYCODE2", "_PSU"), new = c("year", "cnty", "seqno"))
vermont <- rbindlist(list(vermont_00, vermont_11), use.names = T); rm(vermont_00, vermont_11)
vermont[, year := as.numeric(substr(seqno, 1, 4))]  # assure data years are accurate
vermont[, state := 50]  # manually assign state FIPS
vermont <- unique(vermont)

# Virginia 2011-2019 --------------------------------------------------------------------------------
va_files <- list.files(paste0(data_dir, "VA/", 2011:2019), pattern = "*.DTA", recursive = T, full.names = T)
virginia <- rbindlist(lapply(va_files, function(path) {
  yr <- as.numeric(substr(path, 60, 63))
  d <- setDT(read_dta(path))
  if (TRUE %in% grepl("CTYCODE2", names(d))) setnames(d, "CTYCODE2", "CTYCODE1")  # make consistent for easy calculation
  if (yr == 2016) {
    d <- unique(d[, list(seqno = as.numeric(`_PSU`), year = yr, state = 51, cnty = as.numeric(CTYCODE1))])  # seqno is incorrect in 2016, use _PSU instead (same values, but correct)
  } else {
    d <- unique(d[, list(seqno = as.numeric(seqno), year = yr, state = 51, cnty = as.numeric(CTYCODE1))])
  }
}), use.names = T)
va_file_paths <- data.table(year = 2011:2019, state = "VA", file = va_files)
stopifnot(all(va_file_paths[, year == as.numeric(substr(file, 60, 63))]))

# Wisconsin 2010-2019 -------------------------------------------------------------------------------
wi_files <- list.files(paste0(data_dir, "WI/", 2010:2019), pattern = "*.DTA", recursive = T, full.names = T)
wi_ctycodes <- sort(unique(setDT(read_dta(wi_files[[2]]))$ctycode))
wi_impctys <- sort(unique(setDT(read_dta(wi_files[[5]]))$`_IMPCTY`))  # map FIPS code (ctycode) to imputed county (_IMPCTY). have to assume they map in order, we don't have a codebook.
wi_impcty_to_ctycode <- data.table(ctycode = sort(wi_ctycodes[wi_ctycodes < 777]), impcty = sort(wi_impctys))

rm(wi_ctycodes, wi_impctys)
wisconsin <- rbindlist(lapply(wi_files, function(path) {
  yr <- as.numeric(substr(path, 60, 63))
  d <- setDT(read_dta(path))
  if (yr == 2010) {
    setnames(d, "ctycode", "impcty")  # rename for recoding to FIPS codes
    d[wi_impcty_to_ctycode, on = "impcty", ctycode := i.ctycode]  # merge on FIPS codes
  } else if (yr > 2012) {
    setnames(d, "_IMPCTY", "impcty")  # rename for recoding to FIPS codes
    d[wi_impcty_to_ctycode, on = "impcty", ctycode := i.ctycode]  # merge on FIPS codes
  }
  d <- unique(d[, list(seqno = as.numeric(seqno), year = yr, state = 55, cnty = as.numeric(ctycode))])
}), use.names = T)
rm(wi_impcty_to_ctycode)
wisconsin <- rbindlist(list(wi09, wisconsin), use.names = T, fill = T)
wi_file_paths <- data.table(year = 2000:2022, state = "WISCONSIN", file = c(rep(wi_files[1], 10), wi_files[2:14]))
stopifnot(all(wi_file_paths[year %in% 2000:2009, file] == "FILEPATH"))
stopifnot(all(wi_file_paths[year >= 2010, year == as.numeric(str_extract(file, "(?<=_)\\d{4}(?=_)"))]))

# drop 2011 for high frequency of mismatched counties with national data
wisconsin <- wisconsin[year != 2011]
wi_file_paths <- wi_file_paths[year != 2011]

# Wyoming 2000-2022 ---------------------------------------------------------------------------------
wy_files <- list.files(paste0(data_dir_ident, "WYOMING/", 2000:2022), pattern = "*.DTA", recursive = T, full.names = T)
wy_files <- wy_files[!grepl("USA_WYOMING_BRFSS_2021_COUNTY_Y2023M02D15.DTA|USA_WYOMING_BRFSS_2022_COUNTY_Y2024M05D14.DTA", wy_files)]  # remove older 2021-2022 files with missing seqnos
wyoming <- rbindlist(lapply(wy_files, function(path) {
  yr <- as.numeric(substr(path, 59, 62))
  d <- setDT(read_dta(path))
  if (yr == 2004) d[, seqno := `_psu`]  # 2004 seqnos are incorrect; use psu instead
  if (yr %in% 2011:2016) {    # make ctycode names consistent for easy calculation
    setnames(d, "ctycode1", "ctycode")
  } else if (yr >= 2017) {
    setnames(d, "ctycode2", "ctycode")
  }
  if (yr >= 2011) {
    d <- d[substr(orgseqno, 1, 2) == "56"]  # if the first 2 digits of orgseqno are not 56, then the respondent is in a different state and the seqno won't be right
  }
  
  d <- unique(d[, list(seqno = as.numeric(seqno), year = yr, state = 56, cnty = as.numeric(ctycode))])

}), use.names = T)
wy_file_paths <- data.table(year = 2000:2020, state = "WY", file = wy_files)
stopifnot(all(wy_file_paths[, year == as.numeric(substr(file, 59, 62))]))

## Bind together and validate county FIPS -----------------------------------------------------------
all_states <- rbindlist(list(alaska, arizona, arkansas, california, connecticut, delaware, hawaii,
                             illinois, kansas, massachusetts, new_mexico, new_york, pennsylvania,
                             south_dakota, texas, vermont, virginia, wisconsin, wyoming),
                        use.names = T, fill = T)

# validate county FIPS
all_states <- merge.data.table(all_states, cnty_fips[, .(state_code, county_code, is_valid)],
                               by.x = c("state", "cnty"), by.y = c("state_code", "county_code"),
                               all.x = T, allow.cartesian = T)
message(round(100*nrow(all_states[is_valid == 1])/nrow(all_states), 2) ," % of county FIPS were found to be valid.")
message("The following invalid county FIPS will be converted to NA: ")
print(sort(unique(all_states[is.na(is_valid), 1000*state + cnty])))
all_states[, cnty := ifelse(is_valid == 1, cnty, NA_real_)]
all_states[, is_valid := NULL]

# repeat validation for imputed county FIPS
all_states <- merge.data.table(all_states, cnty_fips[, .(state_code, county_code, is_valid)],
                               by.x = c("state", "imp_cnty"), by.y = c("state_code", "county_code"),
                               all.x = T, allow.cartesian = T)
message(round(100*nrow(all_states[is_valid == 1 & !is.na(imp_cnty)])/nrow(all_states[!is.na(imp_cnty)]), 2)
        ," % of imputed county FIPS were found to be valid.")
message("The following invalid imputed county FIPS will be converted to NA: ")
print(sort(unique(all_states[is.na(is_valid), 1000*state + imp_cnty])))
all_states[, imp_cnty := ifelse(is_valid == 1, imp_cnty, NA_real_)]
all_states[, is_valid := NULL]

# Save output --------------------------------------------------------------------------------------
setcolorder(all_states, c("seqno", "year", "state", "cnty", "imp_cnty"))
stopifnot(nrow(all_states[is.na(seqno)]) == 0)  # there should be no missing seqnos
all_states <- unique(all_states)
all_states <- all_states[order(state, cnty, year)]  # sort by values of state, then county, then year
saveRDS(all_states, paste0(out_dir, "state_cnty_codes.rds"))

# gather file paths and upload metadata table to USHD database
state_file_paths <- eval(paste0(c("ak", "az", "ar", "ca", "ct", "de", "hi", "il", "ks", "ma",
                                  "nm", "ny", "pa", "sd", "tx", "vt", "va", "wi", "wy"),
                                "_file_paths"))
state_file_paths <- rbindlist(lapply(state_file_paths, get))
state_file_paths <- merge.data.table(state_file_paths, state_nids, by.x = c("year", "state"),
                                     by.y = c("year", "source"), all.x = T)
stopifnot(nrow(state_file_paths[is.na(nid)]) == 0)  # make sure all NIDs matched correctly
state_file_paths <- state_file_paths[order(state, year)]

nid_path_dict <- lapply(1:nrow(state_file_paths), function(i) {list(state_file_paths[i, file])})
nid_path_dict <- setNames(nid_path_dict, state_file_paths[, nid])
ca_2013_dict <- list(as.list(c(a = ca_files[[1]], b = ca_cw_2013_file)))  # add CDC-to-CA crosswalk file path to 2013 CA NID
ca_2013_dict <- setNames(ca_2013_dict, as.character(nids[source == "CA" & year == 2013, nid]))
names(nid_path_dict[[names(ca_2013_dict)]]) <- "a"  # sublist must be named for modifyList to work properly
nid_path_dict <- modifyList(nid_path_dict, ca_2013_dict)  # update NID dictionary with both 2013 CA file paths

brfss_geo_mapping_version_id <- save_brfss_geo_mapping(input_type = "state",
                                                       nid_path_dict = nid_path_dict,
                                                       output_file_path = out_dir,
                                                       description = "add Alaska 2019",
                                                       prev_issues = "none")
