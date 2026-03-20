####################################################################################################
## Description: 1) Compile all Gallup microdata and recode variables for consistency across years
##
## Inputs:      Spreadsheet  containing path to each Gallup file and what variable codes to pull
##                FILEPATH
##              Gallup data FILEPATH
## Output:  'gallup_microdata_race77.rds' and 'gallup_microdata_race97'-- contains 
##              a data.table with Gallup microdata, identifying and demographic 
##              variables, sample weights, and selected other risk factors or health 
##              data, all consistently defined across years. The files use OMB 
##              1977 and 1997 standards for race/ethnicity respectively. The 1977
##               includes duplicated rows of respondents who are multiracial to account 
##               for different possible "primary" races. The 1997 file only has one 
##               row/respondent.
##          `gallup_var_distribution.pdf` -- plots of variable distribution 
##          'gallup_data77_report.csv' and 'gallup_data97_report.csv'  -- a report on the range and missingness for each variable, by
##              year and overall.
##          
##          Note that this script creates a time-stamped directory within the Gallup 
##            directory, unique for each run of this extraction code
##         [FILEPATH]
##  
####################################################################################################
if (dir.exists(paste0('FILEPATH', Sys.info()['user']))) {
  repo_dir <- paste0('FILEPATH')
} else {
  repo_dir <- paste0('FILEPATH')
}
setwd(paste0(repo_dir, "1_data_prep/counties/survey_data/"))

library(car)
library(data.table)
library(haven)
source("vetting_plots.R") # function for vetting plots
source(paste0(repo_dir, '0_functions/helper/_versioning_functions.R')) # includes make_time_stamp() + get_git_status()
source(paste0(repo_dir, '1_data_prep/counties/survey_data/bridge_multiracial.R')) # bridge_multiracial()
source(paste0(repo_dir, "0_functions/load_ushd_db.R"))  # load USHD database

# Store all results in LIMITED USE directory
root <- ifelse(Sys.info()[1] == "Windows", "FILEPATH", "FILEPATH")
main_output_dir <- paste0(root, "FILEPATH")
# Generate run date and create model output folder
run_date <- make_time_stamp()
message(run_date)
output_dir <- paste0(main_output_dir, run_date, "/")
message(output_dir)
dir.create(output_dir)
# Save git history to output_dir
cat(get_git_status(repo = repo_dir, "Risk Factor Repo", show_diff = T), file=paste0(output_dir, "git_info.txt"), sep="\n")

# make function that cleans the binary vars:  1: Yes, 2: No, 3: (DK), 4: (Refused)
make_binary <- function(data, var){
  data[!(get(var) %in% 1:2), (var) := NA]
  data[, (var) := 2 - get(var)]
}

# function that recodes disease/treated vars at the same time
recode_diseases <- function(data, disease_name, treated_name, svy){
  data[get(disease_name) > 2, (disease_name) := NA]
  data[!is.na(get(disease_name)), (disease_name) := 2 - get(disease_name)]
  
  if(!is.na(layout[svy, get(treated_name)])){
    data[get(treated_name) > 2, (treated_name) := NA]
    data[!is.na(get(treated_name)), (treated_name) := 2- get(treated_name)]
  }
}

# figure out vars to extract
layout <- suppressWarnings(fread("gallup_files_and_variables.csv", na.strings=""))
layout[layout==""]<-NA
file_info <- c("year", "dir", "file")
vars <- names(layout)[!names(layout) %in% file_info]

## Read in variable layout and loop over surveys ---------------------------------------------------

gallup_data <- lapply(X = 1:nrow(layout), FUN = function(svy) {
  cat(paste0(svy, " of ", nrow(layout), ": ", layout[svy, file], "\n")); flush.console()
  
  ## Load files and extract and standardize all variables --------------------------------------------
  filepath = paste0(layout[svy, dir], layout[svy, file])
  filetype <- sub('.*\\.', '', filepath)
  n_rows = Inf # typically, set at Inf so that the whole dataset loaded in; can make shorter to speed up testing
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
  
  # keep relevant variables and standardize names
  included_vars <- vars[!is.na(layout[svy, vars, with=F])]
  data <- data[, as.character(layout[svy, included_vars, with=F]), with=F]
  setnames(data, c(included_vars))
  
  data[, year := layout[svy, year]]
  
  # In 2013-2017 where well-being and politics/economics survey are combined, 
  #   remove PE rows since they don't contain the health information we want
  if(!is.na(layout[svy, survey])){
    data <- data[survey == 1]  
    data[, survey := NULL]
  }
  
# Recode survey vars ------------------------------------------------------
  if(!is.na(layout[svy, sample])){
    data[, sample := factor(sample, levels = 1:2, labels = c("landline", "cellphone"))]  
  }
  
  # Convert Julian date to calendar date
  data[, idate := as.Date(idate, origin = structure(0, class = "Date"))]
  
  
# Recode geography vars ---------------------------------------------------

  if(!is.na(layout[svy, census_region])){
    data[, census_region := factor(census_region, levels = 1:4, labels = c("Northeast", "Midwest", "South", "West"))]
    data[, gallup_region := factor(gallup_region, levels = 1:4, labels = c("East", "Midwest", "South", "West"))]
  }
  
  # make state var from county var 
  data[!is.na(cnty), state := as.numeric(stringr::str_sub(cnty, -5, -4))]
  
# Recode demographic ------------------------------------------------------
  
  # age
  data[age < 18 | age >= 100, age := NA] # age topcoded at 99 (100 = refused)
  data[, age_group := cut(age, c(0,18,25,35,45,55,65,100), labels=F, include.lowest=T, right=F) - 1]
  
  # sex
  data[!sex %in% 1:2, sex := NA]
  
  # edu
  # 2008-2013, recodes D4; 2014+, recodes 'education' which was designed for trending with D4
  if(!is.na(layout[svy, educ])){
    data[, educ := car::recode(educ, "1=1; 2=2; c(3,4)=3; c(5,6)=4; else=NA")] # 1 = < HS diploma, 2 = HS diploma, 3 = technical or vocational school, 4 = some college 5 = college grad, 6 = postgraduate degree/work, 7/8 = DK/RF  
  }
  if(!is.na(layout[svy, educ2])){
    # May 1, 2014 and beyond -- draw from educ2
    data[idate >= "2014-05-01", educ := car::recode(educ2, "1=1; 2=2; c(3,4)=3; c(5,6)=4; else=NA")]
    data[, educ2 := NULL]
  }
  data[, educ := factor(educ, levels=1:4, labels=c("less than HS", "HS grad", "some college", "college grad"))]    
 
  # married
  data[, marital := car::recode(marital, "c(1,8)=3; 2=2; c(3,4,5)=1; else=NA")] # 1 Single/Never been married; 2 Married; 3 Separated;4 Divorced; 5 Widowed;8 Domestic partnership/Living with partner (not legally married); 6 (DK); 7 (Refused)
  data[, marital := factor(marital, levels=1:3, labels=c("former", "current", "never"))]
  
  # own home
  if(!is.na(layout[svy, ownhome])){
    data[, ownhome := car::recode(ownhome, "c(1,3)=0; 2=1; else=NA")] # 1 = rent, 2 = own, 3 = other, 4/5 = DK/RF --> turn into binary (1 = own)
  }
  

  # Race/ethnicity -- recoding takes place after the loop; it makes more sense to 
  #   recode r/e by date than survey year b/c changes in the r/e codes did not 
  #   necessarily happen at beginning/end of svy year

# Risk factor vars --------------------------------------------------------

  # alcohol
  if(!is.na(layout[svy, alc_wk])){
    data[alc_wk > 15, alc_wk := NA]
  }
  
  # smoking
  make_binary(data, "smoker")
  
  if(!is.na(layout[svy, smk_cig])){
    make_binary(data, "smk_cig")
  }
  recode_diseases(data, "highchol", "treated_chol", svy)
  recode_diseases(data, "highbp", "treated_bp", svy)
  recode_diseases(data, "depression", "treated_depression", svy)
  recode_diseases(data, "cancer", "treated_cancer", svy)
  if(!is.na(layout[svy, asthma])){
    recode_diseases(data, "asthma", "treated_asthma", svy)
  }
  
  # diabetes (no variable for treated)
  make_binary(data, "diabetes")
  # heart attack (no variable for treated)
  make_binary(data, "heart_attack")

  # number of active days/week
  data[active_days > 7, active_days := NA] # values over 7 considered unreliable and are removed

# body measurements -------------------------------------------------------
  
  # height
  data[height %in% c(998, 999), height := NA]
  data[, height := height*2.54] # convert to centimeters
  
  # Enforce coding of height in all years
  data[height_feet >= 8 | height_inches > 11, `:=`(height_feet = NA, height_inches = NA)] # height over 8 feet considered biologically implausible. Inches over 11 are considered unreliable
  
  # weight
  data[weight %in% c(998, 999), weight := NA]
  data[, weight := weight/2.2]

# Self-reported health ----------------------------------------------------
  
  data[!genhealth %in% c(1:5), genhealth := NA]
  # Create indicator variables for fair/poor health and for excellent/very good health
  data[genhealth %in% 4:5, genhealth_45 := 1]
  data[!genhealth %in% 4:5 & !is.na(genhealth), genhealth_45 := 0]
  
  data[genhealth %in% 1:2, genhealth_12 := 1]
  data[!genhealth %in% 1:2 & !is.na(genhealth), genhealth_12 := 0]
  
  # perfect health -- 1 = strongly disagree, 5 = strongly agree
  if(!is.na(layout[svy, perfect_health])){
    data[perfect_health > 5, perfect_health := NA]  
  }
  
  # Activity limited days
  data[activity_limited_days > 30, activity_limited_days := NA] # values over 30 considered unreliable
  
  # activity limited, physical pain, worry, stress
  make_binary(data, "activity_limited")
  make_binary(data, "exp_physical_pain")
  make_binary(data, "exp_worry")
  make_binary(data, "exp_stress")
  
# Health insurance coverage -----------------------------------------------
  make_binary(data, "dentist")
  make_binary(data, "hlthplan")
  if (!is.na(layout[svy, sec_insur])) {
    make_binary(data, "sec_insur")
  }
  if (!is.na(layout[svy, personal_doc])) {
    make_binary(data, "personal_doc")
  }
  
  
# Clean -------------------------------------------------------------------

  data <- labelled::remove_labels(data) # remove labels imported from the original files
  
  return(data)
})

data <- rbindlist(gallup_data, use.names=T, fill=T)
data_backup <- copy(data)
# rm(gallup_data)

# Mcnty -------------------------------------------------------------------

# merge with mcnty
data[, cnty := as.integer(cnty)]
data[, msacode := as.integer(msacode)]
load("FILEPATH")
data <- merge(data, loc[, list(cnty, mcnty)], by="cnty", all.x=T)

#### add NIDS
nids <- fread("FILEPATH")
data <- merge(data, nids, by = "year", all = T)

# Race recoding -----------------------------------------------------------
#### Create data blocks, representing dates through which Gallup

## Date block 1: January 1, 2008 through March 31, 2011: Race coded using D6
#   Note that Gallup's provided SPSS code for reconciling race/ethnicity into the RACE variable does not appear to incorporate the D6 variable that was 
#   used for race prior to 4-1-2011; it's therefore unclear how the RACE variable was computed during that time period. However, from examination of the
#   data it appears that Hispanic ethnicity was prioritized in setting the RACE variable during this period.
data[idate >= "2008-01-01" & idate <= "2011-03-31", date_block := 1]
## Date block 2: April 1, 2011 through April 17, 2011: Race coded using D50, with separate variables for each option; multiple races could be selected,
#   but note that there was no option for American Indian. It appears that Hispanic ethnicity was not prioritized during this period.
data[idate >= "2011-04-01" & idate <= "2011-04-17", date_block := 2]
## Date block 3: April 18, 2011 through April 24, 2011: Race coded using D50, with separate variables for each option; multiple races could be selected,
##    and American Indian was asked of everyone but required that tribe be specified. It appears that Hispanic ethnicity was not prioritized during this period.
data[idate >= "2011-04-18" & idate <= "2011-04-24", date_block := 3]
## Date block 4: April 25, 2011 through April 28, 2011: Race coded using D50, with separate variables for each option; multiple races could be selected,
#   and American Indian was asked of everyone; tribe was asked separately. It appears that Hispanic ethnicity was not prioritized during this period.
data[idate >= "2011-04-25" & idate <= "2011-04-28", date_block := 4]
## Date block 5: April 29, 2011 through Feb. 1, 2012: Race coded using D50, with separate variables for each option; multiple races could be selected,
#   and American Indian was asked only of those who said no to White, Black, Asian. It appears that Hispanic ethnicity was not prioritized during this period.
data[idate >= "2011-04-29" & idate <= "2012-02-01", date_block := 5]
## Date block 6: Feb. 2, 2012 through Feb. 12, 2012: Race coded using D50, with separate variables for each option; multiple races could be selected,
#   "Other" was removed, and American Indian or Native American was asked of everyone. It appears that Hispanic ethnicity was not prioritized during this period.
data[idate >= "2012-02-02" & idate <= "2012-02-12", date_block := 6]
## Date block 7: Feb. 13, 2012 through Dec. 31, 2012: Race coded using D50, with separate variables for each option; multiple races could be selected,
#   and AIAN and NHOPI was asked of everyone. It appears that Hispanic ethnicity was not prioritized during this period.
data[idate >= "2012-02-13" & idate <= "2012-12-31", date_block := 7]
## Date block 8: Jan. 1, 2013 through Dec. 31, 2017: Race coded using D69, with separate indicators for each option; multiple races could be selected,
#   and AIAN and NHOPI was asked of everyone. It appears that Hispanic ethnicity was not prioritized during this period. Note that we are restricting this
#   date block to the end of the last year of the Gallup Daily before the survey methodology changed. We will deal with 2018-2019 separately.
data[idate >= "2013-01-01" & idate <= "2017-12-31", date_block := 8]
## Date block 9: Jan. 1, 2018 through Dec. 31, 2019: Race coded using D69A, with separate indicators for each option; multiple races could be selected,
#   and AIAN and NHOPI was asked of everyone. D69A was apparently recoded to the RACE_WHITE, etc., variables. It appears that Hispanic ethnicity was not prioritized 
#   during this period. Note that we are restricting this date block to the end of the last year of the Gallup Daily before the survey methodology changed. 
#   We will deal with 2018-2019 separately.
data[idate >= "2018-01-01", date_block := 9]

### Recode Gallup's variable "RACE", in all date blocks
# Gallup's race variable follows a somewhat arbitrary method of determining primary race when multiple races are reported;
#   It gives priority to certain race groups, but which race groups specificity changed over the years.
#   Details found on page 19 in FILEPATH
# 1 = White, 2 = Other, 3 = Black, 4 = Asian, 5 = Hispanic
data[, race_gallup := factor(race_gallup, levels = 1:5, c("White", "Other", "Black", "Asian", "Hispanic"))] # some of the people in these race groups are also hispanic, FYI

### Recode single race variable (only in date block 1)
if (data[date_block != 1 & !is.na(race_single), .N] > 0) warning("There should not be responses to 'race_single' outside of date_block 1. CHECK")
data[, race_single := as.integer(car::recode(race_single, "6=1; 7=2; 9=3; 1=4; 8=5; else=NA"))] # 1 = Other, 2 = DK, 3= RF, 4/5 = HOLD, 6= White, 7 = Black, 8 = Hispanic, 9 = Asian
data[, race_single := factor(race_single, levels = 1:5, labels = c("NH White", "NH Black", "NH Asian", "NH Other", "Hispanic"))]

print(table(data$race_gallup, data$race_single, useNA = "always"))

### Create Hispanic/Latino indicator
data[, hispanic := car::recode(hispanic, "1=1;2=0;else=NA")]

#### Create indicators of race groups when respondents are allowed to enter multiple races
## Recode the columns that correspond to a specific race
race_indicators = c("race_aian", "race_asian", "race_black", "race_nhopi", "race_other", "race_white") # note that not all of these options are available in each year
data[, (race_indicators) := lapply(.SD, function(x) car::recode(x, "1=1; 2=0; else=NA")),  # 1 = Y, 2 = N, 3/4 = DK/RF
     .SDcols = race_indicators]
# It seems like the race_white etc. vars were not created in a large portion of 2013, despite
#   responses for the race_1 --> race_5 variables. 
#   I will recreate these:
# the options for race_1 - race_5 are:
# value                                label
# 0                  (No more responses)
# 1                                White
# 2            Black or African-American
# 3                                Asian
# 4 American Indian or Alaska Native, or
# 5  Native Hawaiian or Pacific Islander
# 8                                 (DK)
# 9                            (Refused)

# This only affects 2013 codings b/c there is consistency between race_# and race_[group] in the other years
# Create binary indicator that someone seleced in race as one of the races they identify with
data[race_1 == 1 | race_2 == 1 | race_3 == 1 | race_4 == 1 | race_5 == 1, race_white := 1]  # data[1 %in% c(race_1, ...), race_white := 1]   (would a row-wise operation work like this?)  or data[, race_white := as.integer(1 %in% c(race_1, ...))]
data[race_1 == 2 | race_2 == 2 | race_3 == 2 | race_4 == 2 | race_5 == 2, race_black := 1]  
data[race_1 == 3 | race_2 == 3 | race_3 == 3 | race_4 == 3 | race_5 == 3, race_asian := 1]  
data[race_1 == 4 | race_2 == 4 | race_3 == 4 | race_4 == 4 | race_5 == 4, race_aian := 1]  
data[race_1 == 5 | race_2 == 5 | race_3 == 5 | race_4 == 5 | race_5 == 5, race_nhopi := 1] 

## Check for individuals who marked "other" as well as another race; drop "other" category for these individuals
data[race_other == 1 & (race_white == 1 | race_black == 1 | race_asian == 1 | race_aian == 1 | race_nhopi == 1), race_other := 0]

## Create an indicator of multiracial

# For now, we'll leave this and handle "other" race in the modeling step
data[, multiracial := as.numeric((rowSums(.SD, na.rm = TRUE) > 1)), .SDcols = race_indicators]

#### Create a variable that conforms to OMB 1977 standards (with Hispanic), i.e.,
#       NH White, NH Black, NH API, NH AIAN, Hispanic -- no mulitiracial, no NHOPI (use API if possible)
data77 <- copy(data)
# Add Hispanic as a race/eth indicator, and set all other races to 0 as to avoid 
#   other race/ethnicity coding of Hispanic respondents (because code as Hispanic if and only if
#   person identifies as Hispanic/Latinx, so don't need the assignment of "primary race")
data77[hispanic == 1, (race_indicators) := NA]
# Combine Asian and Pacific Islander after NHOPI option is introduced (date_block 7 and greater)
data77[date_block >= 7 & (race_asian == 1 | race_nhopi == 1), race_api := 1] # If 1 or both are marked, categorize as API (unless Hispanic)
data77[date_block >= 7 & (race_asian == 0 & race_nhopi == 0 & !is.na(race_asian) & !is.na(race_nhopi)), race_api := 0] # If both are marked as 0, categorize as not API
# remove Asian and NHOPI values in date_block >= 7  (in this coding, people who responded to Asian and NHOPI only will not be marked as multiracial)
data77[date_block >= 7, c("race_asian", "race_nhopi") := NA]

# recalculate multiracial with the new coding
data77[, multiracial := as.numeric((rowSums(.SD, na.rm = TRUE) > 1)), .SDcols = c("race_aian", "race_black", "race_api", "race_white", "race_other", "hispanic")]
# Create variable of number of races selected
data77[, num_races := rowSums(.SD, na.rm = TRUE), .SDcols = c("race_aian", "race_black", "race_api", "race_white", "race_other", "hispanic")]

# Assign primary race to people when there was no multiracial option
data77[!is.na(race_single), primary_race_ethnicity := race_single]

## Assign primary race for single-race individuals (in years where there is a multiracial option)
data77[race_white == 1 & multiracial != 1, primary_race_ethnicity := "NH White"]
data77[race_black == 1 & multiracial != 1, primary_race_ethnicity := "NH Black"]
data77[race_api == 1 & multiracial != 1, primary_race_ethnicity := "NH API"]
data77[race_aian == 1 & multiracial != 1, primary_race_ethnicity := "NH AIAN"]
data77[race_other == 1 & multiracial != 1, primary_race_ethnicity := "NH Other"]
data77[hispanic == 1, primary_race_ethnicity := "Hispanic"] # assign Hispanic regardless of single race; this overrides coding in race_single

## Probabilistically assign "primary race" to multi-race individuals
# Race proportions are from: “National Center for Health Statistics (U.S.) - 2003 - U.S. Census 2000 Population with Bridged Categorie.Pdf.”
# Accessed May 11, 2021. https://www.cdc.gov/nchs/data/series/sr_02/sr02_135.pdf.
# We create replicate rows for each race selected for a given multiracial individual, and assign the bridged race proportions as the re_weight.

## Create a race/ethnicity weight variable and set it to 1 for individuals with a single primary race/ethnicity
data77[!is.na(primary_race_ethnicity) | multiracial != 1, re_weight := 1]

## Set up a data table for multiracial individuals.
multiracial <- data.table()

## Pass multiracial individuals to bridge_multiracial(), which calculates primary race probabilities for each individual
multiracial_bridged <- bridge_multiracial(dt = data77[multiracial == 1])
multiracial <- multiracial_bridged[["dt_bridged"]]
multiracial_missing <- multiracial_bridged[["dt_missing"]]

## Check that bridged data set is complete
if (nrow(multiracial[is.na(primary_race_ethnicity)]) != 0) stop("There should not be missing `primary_race_ethnicity` after applying bridge_multiracial()") # Should be zero
if (nrow(multiracial[is.na(re_weight)]) != 0) stop("There should not be missing re_weights after applying bridge_multiracial()") # Should be zero

## Define a function for recoding multiracial individuals (applied if and only if multiracial respondent is missing covariates necessary for the bridge_multiracial() algorithm)
recode_multiracial <- function(dt, races, props) {
  fields <- data.table("race_name" = c("NH White", "NH Black", "NH AIAN", "NH API"), "field_name" = c("race_white", "race_black", "race_aian", "race_api"))
  temp <- dt[num_races == length(races), list(uid, temp_sum = rowSums(.SD, na.rm = TRUE)), .SDcols = c(fields[race_name %in% races, field_name])]
  temp <- dt[uid %in% temp[temp_sum == length(races), uid]]
  
  replicates <- data.table()
  for (i in 1:length(races)) {
    current <- copy(temp)
    current[, c("primary_race_ethnicity", "re_weight") := list(races[i], props[i])]
    replicates <- rbindlist(list(replicates, current), use.names = TRUE, fill = TRUE)
  }
  
  return(replicates)
}


#### Use global proportions for those individuals for whom the regression model could not be fit due to missing data. These proportions are from Ingram et al. (2003) since Liebler (2022) does not report updated values.
## White/Black multiracial individuals
multiracial <- rbindlist(list(multiracial, recode_multiracial(dt = multiracial_missing, races = c("NH White", "NH Black"), props = c(0.379, 0.621))), use.names = TRUE, fill = TRUE)

## White/AIAN multiracial individuals
multiracial <- rbindlist(list(multiracial, recode_multiracial(dt = multiracial_missing, races = c("NH White", "NH AIAN"), props = c(0.795, 0.205))), use.names = TRUE, fill = TRUE)

# White/Black/AIAN multiracial individuals
multiracial <- rbindlist(list(multiracial, recode_multiracial(dt = multiracial_missing, races = c("NH White", "NH Black", "NH AIAN"), props = c(0.233, 0.572, 0.195))), use.names = TRUE, fill = TRUE)

# White/API multiracial individuals
multiracial <- rbindlist(list(multiracial, recode_multiracial(dt = multiracial_missing, races = c("NH White", "NH API"), props = c(0.673, 0.327))), use.names = TRUE, fill = TRUE)

# White/Black/API/AIAN multiracial individuals
multiracial <- rbindlist(list(multiracial, recode_multiracial(dt = multiracial_missing, races = c("NH White", "NH Black", "NH API", "NH AIAN"), props = c(0.961, 0.020, 0.009, 0.010))), use.names = TRUE, fill = TRUE)

# White/Black/API multiracial individuals
multiracial <- rbindlist(list(multiracial, recode_multiracial(dt = multiracial_missing, races = c("NH White", "NH Black", "NH API"), props = c(0.782, 0.113, 0.104))), use.names = TRUE, fill = TRUE)

# White/API/AIAN multiracial individuals
multiracial <- rbindlist(list(multiracial, recode_multiracial(dt = multiracial_missing, races = c("NH White", "NH API", "NH AIAN"), props = c(0.933, 0.043, 0.024))), use.names = TRUE, fill = TRUE)

# Black/AIAN multiracial individuals
multiracial <- rbindlist(list(multiracial, recode_multiracial(dt = multiracial_missing, races = c("NH Black", "NH AIAN"), props = c(0.814, 0.186))), use.names = TRUE, fill = TRUE)

# Black/API multiracial individuals
multiracial <- rbindlist(list(multiracial, recode_multiracial(dt = multiracial_missing, races = c("NH Black", "NH API"), props = c(0.630, 0.370))), use.names = TRUE, fill = TRUE)

# Black/API/AIAN multiracial individuals
multiracial <- rbindlist(list(multiracial, recode_multiracial(dt = multiracial_missing, races = c("NH Black", "NH API", "NH AIAN"), props = c(0.461, 0.253, 0.286))), use.names = TRUE, fill = TRUE)

# API/AIAN multiracial individuals
multiracial <- rbindlist(list(multiracial, recode_multiracial(dt = multiracial_missing, races = c("NH API", "NH AIAN"), props = c(0.596, 0.404))), use.names = TRUE, fill = TRUE)

## Now add the multiracial individuals back onto data77
ids <- multiracial[, uid]
data77 <- rbindlist(list(data77[!(uid %in% ids)], multiracial), use.names = TRUE, fill = TRUE)
table(data77[is.na(primary_race_ethnicity), num_races], useNA = "always") # Should all be 0 or NA
setnames(data77, "primary_race_ethnicity", "race77")

if (!all.equal(table(unique(data77$uid), useNA = "always"), table(unique(data$uid), useNA = "always"))) {
  warning("The list of unique IDs in the post-multiracial adjustment data set does not match that from the pre-adjustment data set. Please check this.")  
}
if (nrow(data) != round(sum(data77[, re_weight]), digits = 0)) {
  warning("The sum of re_weight weights does not match the number of rows in the pre-adjustment data set. Please check this.")  
}

# remove NCHS algorithm vars that we don't need for further analysis
data77[, c("region_name", "sex_male", "census_region_northeast", 
           "census_region_midwest", "census_region_south", "census_region_west", 
           "urbanization_large_urban", "urbanization_large_suburban" ,
           "urbanization_med_small_metro", "urbanization_non_metro") := NULL]

table(data77$race77, useNA = "always")

#### Create a variable that conforms to OMB 1997 standards (with Hispanic), i.e.,
#       NH White, NH Black, NH Asian, NH NHOPI, NH AIAN, NH Mulitracial [including a race group + other], Hispanic -- no Other categories
data97 <- copy(data)
data97[!is.na(race_single), race_ethnicity := race_single] # take race variable in date block 1, which only allowed for one race option (i.e, multiracial missing)
# data97[race_ethnicity == "NH Other", race_ethnicity := NA]
data97[multiracial == 1, race_ethnicity := "NH Multiracial"]
## Assign race_ethnicity for single-race individuals
data97[race_white == 1 & multiracial != 1, race_ethnicity := "NH White"]
data97[race_black == 1 & multiracial != 1, race_ethnicity := "NH Black"]
data97[race_asian == 1 & multiracial != 1, race_ethnicity := "NH Asian"]
data97[race_aian == 1 & multiracial != 1, race_ethnicity := "NH AIAN"]
data97[race_nhopi == 1 & multiracial != 1, race_ethnicity := "NH NHOPI"]
data97[race_other == 1 & multiracial != 1, race_ethnicity := "NH Other"]
data97[hispanic == 1, `:=`(race_ethnicity = "Hispanic", multiracial = 0)] # assign hispanic regardless of races selected/multiracial
setnames(data97, "race_ethnicity", "race97")


# clean and save ----------------------------------------------------------

# remove the old race variables used for creating the final categories
data77[, c("multiracial","num_races","race_1","race_2", "race_3","race_4","race_5",
           "race_aian","race_api", "race_asian","race_black","race_gallup","race_nhopi",
           "race_other","race_single","race_white") := NULL]

data97[, c("multiracial", "race_1","race_2","race_3","race_4","race_5","race_aian",
           "race_asian","race_black", "race_gallup","race_nhopi","race_other",
           "race_single","race_white") := NULL]

setkeyv(data77, c("year", "sex", "age", "race77", "educ", "marital"))
setkeyv(data97, c("year", "sex", "age", "race97", "educ", "marital"))
saveRDS(data77, file=paste0(output_dir, "gallup_microdata_race77.rds"))
saveRDS(data97, file=paste0(output_dir, "gallup_microdata_race97.rds"))
print("saved microdata")

## Collect and upload extraction metadata to USHD database -----------------------------------------
# this should be updated with each run!

# collect file paths and map to NIDs
nid_file_paths <- merge.data.table(layout[, .(year, dir, file)], nids, by = "year", all.x = T)
stopifnot(all.equal(sort(unique(data$nid)), sort(unique(nid_file_paths$nid))))  # make sure NIDs align in data and metadata
nid_file_paths[, file := paste0(dir, file)]
nid_path_dict <- lapply(1:nrow(nid_file_paths), function(i) {list(nid_file_paths[i, file])})
names(nid_path_dict) <- nid_file_paths[, nid]

# collect commit hash info
repo_git_cmd <- paste0("cd ", repo_dir, "; git rev-parse --short HEAD")
commit <- system(repo_git_cmd, intern = TRUE)

# upload metadata
survey_extraction_version_id <-
  save_survey_extraction(input_type = "gallup",
                         nid_path_dict = nid_path_dict,
                         output_file_path = output_dir,
                         commit_hash = commit,
                         modeler = Sys.info()[["user"]],
                         prev_issues = "newer extraction available",
                         description = "Added new health care access and utilization variables.",
                         discussion = "Not yet vetted",
                         is_best = F)  # default FALSE to allow vetting before marking best with update_survey_extraction() below

# make reports ------------------------------------------------------------

vetting_plots(data77[, !(colnames(data77) %in% c("cnty_char", "state_name")), with = F], "gallup_race77", output_dir)
vetting_plots(data97, "gallup_race97", output_dir)


report <- function(data) {
  d <- lapply(data, function(x) {
    if (class(x) == "character") temp <- function(x) paste("unique values:", length(unique(x)))
    if (class(x) != "character" & length(unique(na.omit(x))) < 10) temp <- function(x) paste("table:", paste(paste(names(table(x)), " (", table(x), ")", sep=""), collapse=", "))
    if (class(x) != "character" & length(unique(na.omit(x))) >= 10) temp <- function(x) paste("range:", round(min(x, na.rm=T), 2), "to", round(max(x, na.rm=T), 2))
    c(temp(x), paste("num missing:", sum(is.na(x))), paste("% missing:", round(100*mean(is.na(x)), 1)))
  })
  d <- do.call("rbind", d)
  d <- cbind(row.names(d), d)
  colnames(d) <- c("Variable", "Summary", "Number_Missing", "Perc_Missing")
  d
}

gallup_report <- lapply(c(sort(unique(data77$year)), 9999), function(yr) if (yr == 9999) cbind(year="All", report(data77)) else cbind(year=yr, report(data77[data77$year %in% yr,])))
gallup_report <- do.call("rbind", gallup_report)
gallup_report <- gallup_report[order(gallup_report[,2], gallup_report[,1]),]
write.csv(gallup_report, file=paste0(output_dir, "gallup_data77_report.csv"), row.names=F)

gallup_report <- lapply(c(sort(unique(data97$year)), 9999), function(yr) if (yr == 9999) cbind(year="All", report(data97)) else cbind(year=yr, report(data97[data97$year %in% yr,])))
gallup_report <- do.call("rbind", gallup_report)
gallup_report <- gallup_report[order(gallup_report[,2], gallup_report[,1]),]
write.csv(gallup_report, file=paste0(output_dir, "gallup_data97_report.csv"), row.names=F)

# update symbolic link so that output dir is "latest"
system(paste0("ln -sfn ", output_dir, " ", output_dir, "../_LATEST"))

## Update extraction as best upon vetting ---------------------------------------------------------
# uncomment below call to set current extraction as best
# update_survey_extraction(best_status = T,
#                          survey_extraction_version_id = survey_extraction_version_id,
#                          prev_issues = NULL)
