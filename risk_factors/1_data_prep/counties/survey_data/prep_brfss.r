####################################################################################################
## Description: 1) Compile all BRFSS microdata and recode variables for consistency across years
##              2) Remove duplicate observations loaded in from multiple data sources, 
##                  keepings the most detailed geographic information. Note that after 2012, county details
##                  come from State LU files. In some cases, I replace the rows of general BRFSS in those staets
##                  with the extracted/cleaned state files. For other states, we just merge the county details
##                  from the state files onto the general use rows; I would recommend using this approach going forward
##                  because it makes extraction much more simple (you don't need to clean/format each state file 
##                  and does not change the results because the state and national files represent that same
##                  participants (except for some supplemental files for Alaska))
##              3) Generate report and plots of missingness/variable distribution in the compiled dataset 
##                  by variable and by year.
##                  
## Inputs:   Spreadsheet of containing path to each BRFSS file and what variable codes to pull
##              FILEPATH
##           Various data files
##           Lists of county-ids to merge onto the general BRFSS data
##
## Output:  `brfss_temp1.rdata` - brfss_temp4.rdata -- files at different levels [I've commented out the lines that actually saves these files to save storage space, but easy to undo]
##            of the extraction/cleaning process. Useful for vetting or if you want
##            to just look at data from one source (rather than compiled)
##          'brfss_microdata.rds' -- contains a data.table with one row per
##              respondent,identifying any demographic variables, sample weights, and selected other
##              self-report variables, all consistently defined across years.
##          'brfss_data_report.csv' -- a report on the range and missingness for each variable, by
##              year, source file, and overall.
##          'brfss_data_coverage.pdf' -- maps showing state, county, and CBSA data coverage
##              in each year
##          'var_distributions.pdf' -- plots of the distribution of each var/year, for 
##              vetting purposes
##          'var_missingness.pdf' -- show % missing for each var/year, by source  
##          
##          Note that this script creates a time-stamped directory within the BRFSS 
##            directory, unique for each run of this extraction code
##         [FILEPATH]
##
####################################################################################################
if (dir.exists(paste0('FILEPATH', Sys.info()['user']))) {
  repo_dir <- paste0('FILEPATH')
} else if (Sys.info()['user'] == "mcelone") {
  repo_dir <- "FILEPATH"
} else { 
  repo_dir <- "FILEPATH"
}

setwd(paste0(repo_dir, "1_data_prep/counties/survey_data/"))
library(car)
library(data.table)
library(foreign)
library(haven)
library(stringr)
library(tidyr)
library(lubridate)
if("labelled" %in% (.packages())){
  detach("package:labelled", unload=TRUE) # made issues with recode
}

source("vetting_plots.R") # function for vetting plots
source(paste0(repo_dir, "/0_functions/helper/_versioning_functions.R")) # includes make_time_stamp() + get_git_status()
source(paste0(repo_dir, "/0_functions/load_ushd_db.R"))  # load USHD database

# Store all results
root <- ifelse(Sys.info()[1] == "Windows", "FILEPATH", "FILEPATH")
main_output_dir <- paste0(root, "FILEPATH")
# Generate run date and create model output folder 
run_date <- make_time_stamp()
message(run_date)
output_dir <- paste0(main_output_dir, run_date, "/")
message(output_dir)
dir.create(output_dir)
# Save git history to output_dir
cat(get_git_status(repo = repo_dir, "Risk Factor Repo", show_diff = T),file=paste0(output_dir, "git_info.txt"),sep="\n")

## Read in variable layout and loop over surveys ---------------------------------------------------
layout <- suppressWarnings(fread("brfss_files_and_variables.csv", na.strings=""))
layout[layout==""]<-NA
# Just look at the vars to extract from the data file itself (not the file metadata I've added to brfss_files_and_variables.csv)
file_info <- c("file", "landline_file", "year", "state_file", "SMART", "geography", "overlap_natl_file", "supplement")
vars <- names(layout)[!names(layout) %in% file_info]

met_conversion <- fread(paste0(getwd(), "/physical_activity_met.csv"))

my_log <- file(paste0(output_dir, "/prep_brfss_log.txt"))
sink(my_log, append = TRUE, type = "output", split = T) # Writing console output to log file
brfss_data <- parallel::mclapply(X = 1:nrow(layout), mc.cores = 15, FUN = function(svy) { 
  cat(paste0(svy, " of ", nrow(layout), ": ", layout[svy, file], "\n")); flush.console()
  
  ## Load files and extract and standardize all variables ------------------------------------------
  # load files; for 2011+ merge on landline-only file to get landline-only weights
  filepath <- layout[svy, file]
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
  # create indicator for landline and cell samples
  if(!is.na(layout[svy, landline_file])){
    landline_only <- data.table(read_dta(layout[svy, landline_file], n_max = n_rows))
    setnames(landline_only, tolower(names(landline_only)))
    landline_only <- landline_only[, as.character(layout[svy, list(seqno, ll_wt, state)]), with=F]
    data <- merge(data, landline_only, by=as.character(layout[svy, list(seqno, state)]), all=T)
    rm(landline_only); gc()
  }
  
  # keep relevant variables and standardize names
  included_vars <- vars[!is.na(layout[svy, vars, with=F])]
  data <- data[, as.character(layout[svy, included_vars, with=F]), with=F]
  setnames(data, c(included_vars))
  data[, c("year", "supplement") := layout[svy, list(year, supplement)]]
  
  # create indicator for landline and cell samples
  if (layout[svy, year] <= 2010 ) {
    data[, sample := "landline"]
  } else if (layout[svy, year] >= 2011 & !is.na(layout[svy, qstver])) {
    data[qstver >= 10 & qstver < 20, sample := "landline"]
    data[qstver >= 20 & qstver < 30, sample := "cellphone"]
  } else{
    data[, sample := NA]
    print("qstver not available in this survey; check")
  }
  
  # In an MSA county yes/no
  if (!is.na(layout[svy, msa])) {
    data[, msa := car::recode(msa, "c(1,2,3,4)=1; 5=0")] # 1-4 are indicates of living in an MSA, whereas 5 is not in an MSA
  } else if(!is.na(layout[svy, msacode])){
    data[, msa := ifelse(is.na(msacode), # if msacode var is present in dataset, but is NA for a specific row, say that does not live in an MSA: otherwise, say that do live in MSA
                         0,
                         1)] # note that there are respondents who live in micropolitan areas (and maybe be in SMART) who are labeled as 0 by this part
  }
  
  setnames(data, 'msa', 'cbsa.yn', skip_absent = T)
  setnames(data, 'msacode', 'cbsa', skip_absent = T)
  
  # Change DK/RF codes for counties to NA
  if(!is.na(layout[svy, cnty])){
    data[cnty %in% c(777, 888, 999), cnty := NA]
  }
  
  # drop lines that aren't real respondents (something went wrong when merging county codes for IN/MI/OR in 2006 and OR/VT in 2007)   if(!is.na(layout[svy, state])){ # (state is NA in some of the SMART files)
    if (layout[svy, year] == 2006) data <- data[!(is.na(age) & is.na(sex) & is.na(race) & state %in% c(18, 26, 41)),]
    if (layout[svy, year] == 2007) data <- data[!(is.na(age) & is.na(sex) & is.na(race) & state %in% c(41, 50)),]
  }
  
  # prep interview month and day
  # In some cases, the day, month, year variables are missing in state datasets
  if(!is.na(layout[svy, iyear])){
    data[, iyear := as.numeric(iyear)]
    if (max(data$iyear, na.rm = T) %in% 90:99) data[, iyear := 1900 + iyear]
    if (min(data$iyear, na.rm = T) == 1) data[, iyear := layout[svy, year] + (iyear - 1)]
  }
  
  if(!is.na(layout[svy, imonth]) & !is.na(layout[svy, iday])){
    data[, imonth := as.numeric(imonth)]
    setnames(data, "imonth", "month")
    data[, day := weekdays(as.Date(paste(iyear, month, iday, sep="-"), "%Y-%m-%d"), abbreviate = FALSE)]
  }
  suppressWarnings(data[, c("iyear", "iday", "imonth") := NULL]) # it will give warnings if any of these vars are missing
  
  # prep age and sex; some files do not include continuous age as a variable
  if(layout[svy, age] %in% c("age", "c07q01", "c08q01", "c11q01", "c12q01", "c13q01", "c07q02", "c08q02")){
    data[age %in% c(7, 9), age := NA]
    data[, age_continuous := age]
    data[, age := cut(age, c(0,18,25,35,45,55,65,100), labels=F, include.lowest=T, right=F) - 1]
  } else if(layout[svy, age] %in% c("_age_g", "a_age_g")){
    data[, age := age]
  } else{
    print(paste0("no option for", layout[svy, age], "in age ~~ check svy =", svy))
  }
  
  if(!is.na(layout[svy, age_continuous])){ # for years where the variable in the "age" column is grouped age, also capture the continuous age
    data[age_continuous %in% c(7, 9), age_continuous := NA]
    data[, age_continuous := age_continuous]
  }
  
  data[!sex %in% 1:2, sex := NA]
  
  # imputed age
  if(!is.na(layout[svy, imp_age_continuous]) & is.na(layout[svy, imp_age_group])){
    data[imp_age_continuous %in% c(7, 9), age := NA]
    data[, imp_age_group := cut(imp_age_continuous, c(0,18,25,35,45,55,65,100), labels=F, include.lowest=T, right=F) - 1] # use same age cuts as normal age
  }
  setnames(data, "imp_age_group", "imp_age", skip_absent = T)
  
  # create a OMB 1977-style race-ethnicity code (unlike race97 var, does not have a multiracial category -- uses respondents "primary" race; create API category)
  if(!is.na(layout[svy, race_primary])){
    if(layout[svy, year] %in% 1990:2000){
      data[, race77 := car::recode(race_primary, "1=0; 2=1; 3=3; 4=2; 5=5; else=NA")] #1= White ; 2 Black; 3 Asian, Pacific Islander; 4 American Indian, Alaska Native; 5 Other; 7 Don't know/Not sure
    } else if(layout[svy, year] %in% 2001:2012){
      data[, race77 := car::recode(race_primary, "1=0; 2=1; c(3,4)=3; 5=2; 6=5; c(7,8)=6; else=NA")] #1 White; 2 Black or African American ; 3 Asian; 4 Native Hawaiian or other Pacific Islander;  5 American Indian or Alaskan Native; 6 Other race ;7 No preferred race ; 8 Multiracial but preferred race not asked ; 77 Don’t know/Not sure ;   99 Refused 
    } else if(layout[svy, race_primary] == "orace4"){ # only CA 2013
      data[, race77 := car::recode(race, "1=0; 2=1; c(3,4)=3; 5=2; 6=5; else=NA")] # 1 = White, 2 = Black, 3= Asian, 4 = NHOPI, 5 = AIAN, 6 = Other, 7 = EK, 9 = RF
      data[hispanic == 1, race77 := 4] # code as Hispanic if respondent identifies as Hispanic/Latino
    } else if(layout[svy, year] >= 2013){
      data[, race77 := car::recode(race_primary, "1=0; 2=1; 3=2; c(4,5)=3; 6=5; c(7,8)=6; else=NA")] # 1 White; 2 Black or African American; 3 American Indian or Alaskan Native;4 Asian;5 Native Hawaiian or other Pacific Islander;6 Other race; 7 No preferred race (and MR);  8 Multiracial but preferred race not answered;  77 Don’t know/Not sure;99 Refused
    }
    data[hispanic == 1, race77 := 4] # code as Hispanic if respondent identifies as Hispanic/Latino
    data[, race77 := factor(race77, levels=0:6, labels=c("NH White", "NH Black", "NH AIAN", "NH API", "Hispanic", "NH Other", "NH MR w/o main"))]
  }
  
  # standardize race using OMB 1997 standards as much as possible
  if(!is.na(layout[svy, race])){
    if (layout[svy, race] == "race") { # 1990-2000 ('RACE' variable): 1=white non-hispanic; 2=black non-hispanic; 3=white hispanic; 4=black hispanic; 5=other hispanic; 6=asian/pacific islander; 7=AIAN; 8=other; 99=DK; in early years, BRFSS did not make hispanic/non-hispanic AIAN and API categories, but we override this with the hispanic var in next line
      data[, race := car::recode(race, "1=0; 2=1; 7=2; 6=3; c(3,4,5)=4; 8=8; else=NA")] # no separated Asian + NHOPI option this year (only API)
      data[hispanic == 1, race := 4]  # Changed on 5/26 to enforce coding r/e as hispanic if respondents are hispanic/latino (regardless of race) -- i think previously we were not correctly coding API people to Hispanic/non-hispanic, although it's possible the section later down that implements hispanic coding does that
    } else if (layout[svy, race] == "race2") { # 2001-2012 ('RACE2' variable): 1=white only, non-hispanic; 2=black only, non-hispanic; 3=asian only, non-hispanic; 4=native HI or pacific islander, non-hispanic; 5=AIAN only, non-hispanic; 6=other only, non-hispanic; 7=multiracial, non-hispanic; 8=hispanic; 9=DK/NS/R
      data[, race := car::recode(race, "1=0; 2=1; 5=2; 3=6; 4=7; 8=4; 7=5; 6=8; else=NA")]
    } else if (layout[svy, race] %in% c("_race", "_race1")) { # 2013+ ('_race' variable): 1=white only, non-hispanic; 2=black only, non-hispanic; 3=AIAN only, non-hispanic; 4=asian only, non-hispanic; 5=native HI or pacific islander, non-hispanic; 6=other only, non-hispanic; 7=multiracial, non-hispanic; 8=hispanic; 9=DK/NS/R
      data[, race := car::recode(race, "1=0; 2=1; 3=2; 4=6; 5=7; 8=4; 7=5; 6=8; else=NA")]
    } else if (layout[svy, race] == "a_race"){
      data[, race := car::recode(race, "1=0; 2=1; 3=2; 4=6; 5=7; 8=4; 7=5; 6=8; else=NA")] # LU 2013: in codebook, called ('_RACE'): 1=white only, non-hispanic; 2=black only, non-hispanic; 3=AIAN only, non-hispanic; 4=asian only, non-hispanic; 5=native HI or pacific islander, non-hispanic; 6=other only, non-hispanic; 7=multiracial, non-hispanic; 8=hispanic; 9=DK/NS/R
    } else if (layout[svy, race] == "orace3"){ # CA 2013
      data[, race := car::recode(race, "1=0; 2=1; 3=6; 4=7; 5=2; 6=8; else=NA")] # 1 = White, 2 = Black, 3= Asian, 4 = NHOPI, 5 = AIAN, 6 = Other, 7 = EK, 9 = RF
      data[!is.na(race_primary), race := 5] # the primary race var is only NOT NA if someone selected more than 1 race (this is correct all but 6 times in the whole state dataset)
    } else{
      print(paste0("no option for", layout[svy, race], "in race ~~ check svy =", svy))
    }
    if(!is.na(layout[svy, hispanic])){
      data[hispanic == 1, race := 4] # make sure all hispanic respondents coded that way regardless of race  
    }
    data[, race := factor(race, levels=0:8, labels=c("NH White Only", "NH Black Only", "NH AIAN Only", "NH API Only", "Hispanic", "NH Multiracial", "NH Asian Only", "NH NHOPI Only", "NH Oth. Only"))] # API category is used thru 2000; Asian and NHOPI categories are used 2001+
    setnames(data, "race", "race97")
    data[year < 2001, race97 := NA] # we can't create OMB 1997 categories before 2001
    data[, race_primary := NULL]
  }
  
  # standardize imputed race
  if (!is.na(layout[svy, imp_race])){
    data[, imp_race := car::recode(imp_race, "1=0; 2=1; 3=3; 4=2; 5=4; else=NA")] # 1 = NH White, 2 = NH Black, 3= NH Asian; 4 = NH AIAN; 5 = Hispanic, 6 = NH Other
    data[, imp_race := factor(imp_race, levels=0:4, labels=c("NH White", "NH Black", "NH AIAN", "NH Asian", "Hispanic"))] # NHOPI is not an option -- just Asian
  }
  
  # standardize marital status
  if(!is.na(layout[svy, marital])){
    if (layout[svy, marital] == "marital"){
      data[, marital := car::recode(marital, "c(2,3,4)=0; 1=1; c(5,6)=2; else=NA")] # all years: 1=married; 2=divorced; 3=widowed; 4=separated; 5=never married; 6=a member of an unmarried couple; 9=refused
    } else{
      print(paste("no option for", layout[svy, marital], "in marital ~~ svy =", svy))
    }
    data[, marital := factor(marital, levels=0:2, labels=c("former", "current", "never"))]
  }
  
  if (!is.na(layout[svy, imp_marital])){
    data[, imp_marital := car::recode(imp_marital, "c(2,3,4)=0; 1=1; c(5,6)=2; else=NA")] # all years: 1=married; 2=divorced; 3=widowed; 4=separated; 5=never married; 6=a member of an unmarried couple
    data[, imp_marital := factor(imp_marital, levels=0:2, labels=c("former", "current", "never"))]
  }
  
  # standardize education
  if (layout[svy, year] <= 1992 & layout[svy, edu] == "educa") { # 1990-1992: 1=eight grade or less; 2=some HS; 3=HS/GED; 4=Some technical school; 5=Technical school graduate; 6=Some college; 7=College graduate; 8=Post-grad/professional degree; 9=Refused
    data[, edu := car::recode(edu, "c(1,2)=0; 3=1; c(4,5,6)=2; c(7,8)=3; else=NA")]
  } else if (layout[svy, year] >= 1993 & layout[svy, edu] == "educa" & !(layout[svy, file] %like% "/CA")) { #1993-2018, except CA 2013: 
    data[, edu := car::recode(edu, "c(1,2,3)=0; 4=1; 5=2; c(6,8)=3; else=NA")] # 1=none/kindergarten; 2=grades 1-8 (Elementary); 3= grades 9-11 (some HS); 4=grade 12/GED (HS grad); 5=college 1-3 yrs (some college/technical school); 6=College 4yrs+ (College grad); 9=Refused. In alaska, group 6 has been recoded as 8 from 1993 to 1997;
  } else if (layout[svy, year] >= 1993 & layout[svy, edu] == "educa" & (layout[svy, file] %like% "/CA")){ # California 2013 has it's own coding
    data[, edu := car::recode(edu, "c(1,88,2)=0; 3=1; c(4,5,6)=2; c(7,8)=3; else=NA")]  # 1 or 88= less than 9th grade, 2 = some HS, 3 = HS grad, 4 = some technical school, 5 = tech school grad, 6 = some college, 7 = college grad; 8 = post grad, 77  = DK, 99 = RF, 96 = other
  } else {
    print(paste("no option for", layout[svy, edu], "in edu ~~ check svy =", svy))
  }
  data[, edu := factor(edu, levels=0:3, labels=c("less than HS", "HS grad", "some college", "college grad"))]
  
  if(!is.na(layout[svy, imp_edu])){
    data[, imp_edu := car::recode(imp_edu, "c(1,2,3)=0; 4=1; 5=2; c(6,8)=3; else=NA")] #  1=none/kindergarten; 2=grades 1-8 (Elementary); 3= grades 9-11 (some HS); 4=grade 12/GED (HS grad); 5=college 1-3 yrs (some college/technical school); 6=College 4yrs+ (College grad)
    data[, imp_edu := factor(imp_edu, levels=0:3, labels=c("less than HS", "HS grad", "some college", "college grad"))]
  }
  
  # standardize phone
  # phoneown_c (in cellphone survey 2011+): do you also have a landline? 1: yes, 2: no
  # phoneuse_c (in cellphone survey 2011-2013): % calls received on cellphone
  # phoneown_ll	(in landline survey 2011+): do you have a cellphone for personal use? 1: yes, 2: no
  # phoneuse_ll (in cellphone survey 2009-2013): % calls received on cellphone
  if (layout[svy, year] < 2009) {  # years prior to 2009 - landline survey only (all have landline - no info on owning cellphone)
    data[, ownll := 1]  # has landline yes
  } else if (!is.na(layout[svy, phoneown_ll]) & is.na(layout[svy, phoneown_c]) & layout[svy, year] %in% 2009:2010) {  # years 2009-2010 - landline survey only (all have landline AND info on owning cellphone)
    data[, ownll := 1]  # has landline yes
    data[phoneown_ll == 1, owncp := 1]  # owns cellphone yes
    data[phoneown_ll == 2, owncp := 0]  # owns cellphone no
    data[owncp == 1, phone := "dual_phone"]  # owns cellphone AND landline (landline is default since this is landline survey only)
    data[owncp == 0, phone := "landline_only"]  # owns landline only
    data[, phone := factor(phone, levels=c("dual_phone", "landline_only", "cell_only"))]
  } else if (!is.na(layout[svy, phoneown_ll]) & !is.na(layout[svy, phoneown_c]) & layout[svy, year] >= 2011) {#years 2011+ (combined data from landline and cell phone surveys)
    data[sample == "landline" | (sample == "cellphone" & phoneown_c == 1), ownll := 1]  # landline survey OR owns landline from cell phone survey
    data[sample == "cellphone" & phoneown_c == 2, ownll := 0]  # cell phone survey and does not own landline
    data[sample == "cellphone" | (sample == "landline" & phoneown_ll == 1), owncp := 1]  # cellphone survey OR owns cell phone from landline survey
    data[sample == "landline" & phoneown_ll == 2, owncp := 0]  # landline survey and does not own cell phone
    data[ownll == 1 & owncp == 1, phone := "dual_phone"]
    data[ownll == 1 & owncp == 0, phone := "landline_only"]
    data[ownll == 0 & owncp == 1, phone := "cell_only"]
    data[, phone := factor(phone, levels=c("dual_phone", "landline_only", "cell_only"))]
  } else if (!is.na(layout[svy, phoneown_ll]) & layout[svy, phoneown_ll] == "cpdemo1a"){  #  Including phones for business and personal use, do you have a cell phone for personal use? 1 = y, 2 = N, 7/9 = DKRF; blank if QSTVER >= 20 (aka if cell phone survey)
    data[sample == "landline" | !is.na(phoneown_ll), ownll := 1]  # in landline sample if asked this question
    data[sample == "cellphone" | phoneown_ll == 1, owncp := 1]  # in CP sample or owns CP
    data[phoneown_ll == 2, owncp := 0]  # does not own CP
  } else if (!is.na(layout[svy, phoneown_ll]) & layout[svy, phoneown_ll] == "cpdemo1b"){  # Not including cell phones or numbers used for computers, fax machines or security systems, do you have more than one telephone number in your household?
    data[sample == "landline" | !is.na(phoneown_ll), ownll := 1]  # in landline sample if asked this question
    data[sample == "cellphone" | phoneown_ll %in% 1:6, owncp := 1]  # in CP sample or owns CP
    data[phoneown_ll == 8, owncp := 0]  # does not own CP
  } else{
    print(paste0("The phone variable is not being recoded here (svy = ", svy, "). Check (not necessarily an issue)"))
  }
  
  # standardize tenure
  if(!is.na(layout[svy, tenure])){
    if (layout[svy, tenure] == "renthome") { # 1996-1999: 1 = Own, 2 = Rent
      data[, tenure := car::recode(tenure, "2=0; 1=1; else=NA")]
    } else if (layout[svy, tenure] == "renthom1") { # 2009-201X: 1 = Own, 2 = Rent, 3 = Other, 7/9 = DK/RF
      data[, tenure := car::recode(tenure, "c(2,3)=0; 1=1; else=NA")]
    } else if (layout[svy, tenure] == "rent"){ # WA 2010 only.. 1 = rent, 2=own, 7/9=DK/RF
      data[, tenure := car::recode(tenure, "1=0;2=1; else=NA")]
    } else if (layout[svy, tenure] == "ownhome"){
      data[, tenure := car::recode(tenure, "1=1; c(2,3)=0; else=NA")] # 1 = own, 2 = rent, 3 = other; 7/77 DK , 9/99 = RF (ownhome in CA 2013 data; follows format of the RENT var)
    } else{
      warning("did not recode")
    }
  }
  
  # prep pregnancy; coded as pregnt2 in 2001, but same codes as all other years
  data[, preg := car::recode(preg, "2=0; 1=1; else=NA")] # all years: 1 = yes; 2 = no
  
  # standardize height in meters
  if (layout[svy, height] %in% c("height", "height3") & layout[svy, SMART] == 0) { # 1990-2001 (height): reported height in feet and inches//height3 used in SMART BRFSS (CBSA) 2013-2017
    data[height %in% c(777, 999, 7777, 9999), height := NA]
    data[, height := (2.54 * (floor(height/100)*12 + (height - 100*floor(height/100))))/100]
  } else if (layout[svy, height] %in% c("htm", "htm2", "htm3", "htm4", "xhtm")) { # 2000-2012, 2020+ (HTM/HTM2/HTM3/HTM4): computed height in meters (two implied decimal points)
    data[height > 998, height := NA]
    data[, height := height/100]
  } else if (layout[svy, height] == "height3" & layout[svy,SMART] == 1){ # height coded differently in SMART 
    data[height == 7777 | height == 9999, height := NA]
    data[, height := ifelse(nchar(height) == 4, # if 4 digits, metric (metric starts with a 9) \\ else, feet/inches
                            as.numeric(substr(height, 2, 4))/100,
                            (2.54 * (floor(as.numeric(height)/100)*12 + (as.numeric(height) - 100*floor(as.numeric(height)/100))))/100)]
  } else if(!is.na(layout[svy, height])){
    print(paste("no option for", layout[svy, height], "in height ~~ check svy =", svy))
  }
  
  # standardize weight in kg
  if (layout[svy, weight] == "weight") { # 1990-2003 (weight): reported weight in pounds
    data[weight %in% c(777, 999, 7777, 9999), weight := NA]
    data[, weight := weight/2.2046]
  } else if (layout[svy, weight] %in% c("wtkg2", "wtkg3", "xwtkg")) { # 2004-2012 (wtkg2/wtkg3): calculated weight in kg (two implied decimal points)
    data[weight == 99999 | weight < 0, weight := NA]
    data[, weight := weight/100]
  } else if (layout[svy, weight] %in% c("c10q12", "c13q10", "c12q10", "c14q10",
                                        "c11q10", "c12q11", "c08q11", "c07q11",
                                        "c07q19", "c08q19", "c08q18", "c08q17")){
    data[weight == 7777 | weight == 9999, weight := NA]
    data[, weight := ifelse(nchar(weight == 4), # if 4 digits, metric (metric starts with a 9) \\ else, feet/inches
                            as.numeric(substr(weight, 2, 4))/100,
                            as.numeric(weight)/2.2046)]
  } else if (!is.na(layout[svy, height])){
    print(paste("no option for", layout[svy, weight], "in weight ~~ check svy =", svy))
  }
  
  # Standardize smoke100 (~"have you smoked >= 100 cigarettes in your lifetime")
  if (layout[svy, smoke100] %in% c("smoke100", "c06q01", "c07q01","c08q01", "c09q01", "c10q01", "c11q01")){
    data[, smoke100 := car::recode(smoke100, "1=1; 2=0; else=NA")] # 1 = yes, 2 = no (all years)
  } else if (!is.na(layout[svy, smoker100])){
    print(paste("no option for", layout[svy, smoke100], "in smoke100 ~~ check svy = ", svy))
  }
  
  # standardized "smokeday" (~how many cigarettes do you smoke in a day)
  #   vars are smokeday, smokday2, or "c07q02", "c08q02", "c09q02", "c10q02", "c11q02"
  if(!is.na(layout[svy, smokeday])){
    data[, smokeday := car::recode(smokeday, "1=1; 2=2; 3=3; else=NA")] # 1 = everyday, 2 = some days, 3 = Not at all, 9 = RF; in smokday2, 7 = RF (but since it's in the "else" category, can recode all smokeday at once)
    data[, smokeday := factor(smokeday, levels = 1:3, labels=c("daily", "sometimes", "not_at_all"))]
  }
  
  # standardize smoking status
  if(!is.na(layout[svy, smoker])){ # missing in FL 2002
    if (layout[svy, smoker] %in% c("_smoker", "a_smoker") & layout[svy, year] %in% 1990:1993) { # smoker (1990-1993 + 2013 CA): 1=current smoker; 2=former smoker; 3=never smoked; 4=irregular smoker; 9=refused
      data[, smoke := car::recode(smoker, "1=1; 2=3; 3=4; 4=2; else=NA")]
    } else if (layout[svy, smoker] %in% c("_smoker2", "a_smoker2") & layout[svy, year] %in% 1994:1995) { # smoker2 (1994-1995): 1=current smoker, 30 days; 2=current smoker, 1-29 days; 3=current smoker, 0 days; 4=current smoker, unknown days; 5=former smoker; 6=never smoked
      data[, smoke := car::recode(smoker, "1=1; c(2,3,4)=2; 5=3; 6=4; else=NA")]
    } else if (layout[svy, smoker] %in% c("a_smoker", "_smoker2", "smoker2", "smoker2x", "a_smoker2", "_smoker3", "a_smoker3", "asmoker3", "x_smoker3", "smoker3x", "smokstat") & layout[svy, year] >= 1996) { # smoker2/smoker3 (1996-2012): 1=current smoker, everyday; 2=current smoker some days; 3=former smoker; 4=never smoked
      data[smoker <= 4, smoke := smoker]
      data[, smoke := car::recode(smoker, "1=1; 2=2; 3=3; 4=4; else=NA")]
    } else if (layout[svy, smoker] == "_smoker" & (layout[svy, file] %like% "/CA")){ # Only applies to CA 2013
      # There is something irregular about how smoker status is calculated in the CA 2013 file (affecting "current, sometimes" category),
      #   so will just construct it here:
      data[smoke100 == 0, smoke := 4L] # never smoke
      data[smoke100 == 1 & smokeday == "daily", smoke := 1L]
      data[smoke100 == 1 & smokeday == "sometimes", smoke := 2L]
      data[smoke100 == 1 & smokeday == "not_at_all", smoke := 3L]
    } else {
      print(paste("no option for", layout[svy, smoker], "in smoker ~~ check svy = ", svy, "(it might not be an issue; not available in SMART 2009-2016"))
      data[, smoke := NA]
    }
    
    data[, smoke := factor(smoke, levels=1:4, labels=c("current, daily", "current, sometimes", "former", "never"))]
    data[!is.na(smoke), smoke_any := grepl("current", smoke)]
  }
  data[, smoker := NULL]
  
  # prep age first smoked
  # NOTE that the question changed a bit over the years for first/last smoked (from regularly smoking to just a puff or)
  if (!is.na(layout[svy, firstsmoke])) {
    data[firstsmoke >= 77, firstsmoke := NA]
    # From archived version of prep_brfss
    if ("smoke" %in% names(data)) data[as.numeric(smoke) == 4, firstsmoke := NA]
  }
  
  # standardize time since last smoked
  # NOTE I recall (but not certain) that the wording of the question changed a bit between years (from last smoked regularly, to just a puff or two)
  if (!is.na(layout[svy, lastsmoke])) {
    if (layout[svy, lastsmoke] == "lastsmok" & layout[svy, year] == 1990) { # LASTSMOK (1990): 1= < 1 month, 2= 1-3 months, 3= 3-6 months, 4= 6-12 months, 5=1+ years
      data[, lastsmoke := car::recode(lastsmoke, "c(1,2,3,4)=1; 5=2; else=NA")]
    } else if (layout[svy, lastsmoke] == "lastsmok" & layout[svy, year] %in% 1991:1993) { # LASTSMOK (1991-1993): 1= < 1 month, 2= 1-3 months, 3= 3-6 months, 4= 6=12 months, 5= 1-5 years, 6= 5+ years, 8= never smoked regularly
      data[, lastsmoke := car::recode(lastsmoke, "c(1,2,3,4)=1; c(5,6)=2; c(8,88)=3; else=NA")]
    } else if (layout[svy, lastsmoke] == "lastsmok" & layout[svy, year] %in% 1994:2000) { # LASTSMOK (1994-2000): 1= < 1 month, 2= 1-3 months, 3= 3-6 months, 4= 6-12 months, 5= 1-5 years, 6= 5-15 years, 7= 15+ years, 88= never smoked regularly
      data[, lastsmoke := car::recode(lastsmoke, "c(1,2,3,4)=1; c(5,6,7)=2; c(8,88)=3; else=NA")]
    } else if (layout[svy, lastsmoke] %in% c("lastsmk", "m13q03", "m14q03", "m11q03") & layout[svy, year] %in% 2001:2003) { # LASTSMK (2001-2003): 1 = < 1 month, 2 = 1-3 months, 3= 3-6 months, 4= 6-12 months, 5 = 1-5 years, 6 = 5-10 years, 7 = 10+ years,
      data[, lastsmoke := car::recode(lastsmoke, "c(1,2,3,4)=1; c(5,6,7)=2; else=NA")]
    } else if (layout[svy, lastsmoke] %in% c("m15q01", "m21q01", "tx03q01")){ # 1= < 1 month, 2= 1-3 months, 3= 3-6 months; 4= 6-12 months; 5=1-5 years; 6= 5-10 years; 7= 10+ years; 8= never smoked regularly (not an option for tx03q01, but no matter)
      data[, lastsmoke := car::recode(lastsmoke, "c(1,2,3,4)=1; c(5,6,7)=2; c(8,88)=3; else=NA")]
    } else if (layout[svy, lastsmoke] %in% c("lastsmk1", "lastsmk2", "c09q04", "c08q04", "c07q04", "c11q04") & layout[svy, year] >= 2009) { # LASTSMK1/LASTSMK2 (2009-2018): 1= < 1 month, 2= 1-3 months, 3= 3-6 months; 4= 6-12 months; 5=1-5 years; 6= 5-10 years; 7= 10+ years; 8= never smoked regularly
      data[, lastsmoke := car::recode(lastsmoke, "c(1,2,3,4)=1; c(5,6,7)=2; c(8,88)=3; else=NA")]
    } else if (!is.na(layout[svy, lastsmoke])){
      print(paste("no option for", layout[svy, lastsmoke], "in lsatsmoke ~~ check svy =", svy))
    }
    data[, lastsmoke := factor(lastsmoke, levels=1:3, labels=c("< 1 yr", "> 1 yr", "never regularly smoked"))]
  }
  
  data[, c("smoke30", "smokenow", "smokenum") := NULL] # vars only available in early years so removing
  
  # prep self-reported health and emotional support
  if(!is.na(layout[svy, genhlth])) {
    data[, genhlth := factor(genhlth, levels=1:5, labels=c("Excellent", "Very good", "Good", "Fair", "Poor"))]
    
    # these are almost always not NA if genhlth is not NA, but in a few cases they are (e.g., FL 2002)
    if(!is.na(layout[svy, physhlth])){
      data[, physhlth := car::recode(physhlth, "88=0; 31:hi=NA")]
    }
    if(!is.na(layout[svy, menthlth])){
      data[, menthlth := car::recode(menthlth, "88=0; 31:hi=NA")]
    }
    if(!is.na(layout[svy, poorhlth])){
      data[, poorhlth := car::recode(poorhlth, "88=0; 31:hi=NA")]
    }
    if(!is.na(layout[svy, physhlth]) & !is.na(layout[svy, menthlth])){
      data[physhlth == 0 & menthlth == 0, poorhlth := 0] # from 1994 onwards there's a skip pattern that
      # assumes poorhlth is 0 if physhlth and menthlth are 0, so we need to fill that in. For 1993
      # this skip pattern doesn't exist, but we'll make the same assumption for consistency
    }
  }
  
  if(!is.na(layout[svy, emtsuprt])) {
    data[, emtsuprt := factor(emtsuprt, levels=1:5, labels=c("Always", "Usually", "Sometimes", "Rarely", "Never"))]
  }
  
  # prep any, heavy, and binge drinking
  setnames(data, "drnkany", "anydrnk", skip_absent = T) # skip_absent suppresses error message in the cases where drnkany is missing
  if (!is.na(layout[svy, alcday])) {
    
    # convert alcday to units = 'per day'
    data[, alcday := as.double(alcday)]
    data[alcday > 230 & alcday < 300, alcday := 230]
    data[alcday >= 100 & alcday < 200, alcday := (alcday - 100) / 7]
    data[alcday >= 200 & alcday < 300, alcday := (alcday - 200) / 30]
    data[alcday == 888, alcday := 0]
    data[alcday > 1, alcday := NA]
    
    if(!is.na(layout[svy, drnkany])){
      # recode drnkany
      data[, anydrnk := car::recode(anydrnk, "2=0; 1=1; else=NA")] # 1 = yes, 2 = no, 7/9/blank = DK/RF/NA (variation b.w drnkany - drnkany5 is all about slight differences in the types of missing responses)
      
      # correct anydrnk based on response to alcday
      data[alcday == 888, anydrnk := 0]
      
      # set alcday to 0 if anydrnk is 0
      data[anydrnk == 0, alcday := 0]
    } else{
      # Fill in anydrnk with alcday, if anydrnk/drnkany is missing
      data[alcday == 0, anydrnk := 0]
      data[alcday > 0, anydrnk := 1]
    }
    
    # recode avedrnk and set to zero if anydrnk is 0
    data[avedrnk >= 77, avedrnk := NA]
    data[anydrnk == 0, avedrnk := 0]
    
    # calculate average daily consumption and derive the indicator for heavy drinking
    data[, csmp := alcday * avedrnk]
    data[!is.na(csmp), heavydrnk := as.numeric((sex == 1 & csmp > 2) | (sex == 2 & csmp > 1))]
    data[, csmp := NULL]
    
    # recode drnkeg5
    data[, drnkge5 := car::recode(drnkge5, "88=0; 77:hi=NA")]
    data[is.na(drnkge5) & anydrnk == 0, drnkge5 := 0]
    setnames(data, "drnkge5", "bingedrnk")
  }  data[, drnkge5 := NULL] # remove var b/c it gets recoded above (in the not NA cases)
  # standardize max drinks and number of drinks in last binge session
  if (!is.na(layout[svy, maxdrnks])) {
    data[maxdrnks >= 77, maxdrnks := NA]
    if(!is.null(data$anydrnk)){ # in WA 2000, there is no drinking data; otherwise, this column is not null (either directly reported or inferred through alcday var)
      data[anydrnk == 0, maxdrnks := 0]
    }
  }
  
  if (!is.na(layout[svy, drnkbeer])) {
    data[, drnkbeer := car::recode(drnkbeer, "88=0; 77:hi=NA")]
    data[, drnkwine := car::recode(drnkwine, "88=0; 77:hi=NA")]
    data[, drnkliqr := car::recode(drnkliqr, "88=0; 77:hi=NA")]
    if(layout[svy, year] == 2008) {
      data[, drnkpmix := car::recode(drnkpmix, "88=0; 77:hi=NA")]
      data[, numdrnks := drnkbeer + drnkwine + drnkliqr + drnkpmix]
      data[, c("drnkbeer", "drnkwine", "drnkliqr", "drnkpmix") := NULL]
    } else {
      data[, numdrnks := drnkbeer + drnkwine + drnkliqr]
      data[, c("drnkbeer", "drnkwine", "drnkliqr") := NULL]
    }
  }
  
  # prep cholesterol
  #cholesterol checked
  if(!is.na(layout[svy, cholchk])){
    if ((layout[svy, cholchk]) %in% c("cholchk", "c05q02", "c06q02", "c07q02", "c08q02")  & layout[svy, year] < 2017) {
      data[, cholchk := car::recode(cholchk, "1=1; 2=2; 3=3; 4=4; else=NA")] # 1 = within past yr; 2 = 1-2 yrs, 3 = 2-5 yrs, 4 = 5+yrs, 7/9 = DKRF
    } else if (layout[svy, cholchk] %in% c("cholchk1", "c05q01", "tx05q01") & layout[svy, year] >= 2017){
      data[, cholchk := car::recode(cholchk, "1=5; 2=1; 3=2; 4=3; 5=4; else=NA")] # 1 = never, 2 = within past y, 3 = 1-2 yrs, 4 = 2-5 yrs, 5= 5+yrs, 7/9 = DK/RF
    } else if(layout[svy, cholchk] %in% c("cholchk2", "cholchk3")){
      data[, cholchk := car::recode(cholchk, "1=5; 2=1; 3=2; c(4,5,6)=3; 8=4; else=NA")] # 1 = never, 2 = within past yr, 3 = 1-2 yrs, 4 = 2-3 yrs, 5 = 3-4 yrs, 6= 4-5 yrs, 7 = EK, 8 = >5, 9 = RF
    } 
    else{
      print(paste0("Didn't find option for '", layout[svy, cholchk]), "'")
    }
    data[, cholchk := factor(cholchk, levels=1:5, labels=c("< 1 yr", "1-2 yrs", "2-5 yrs", "5+ yrs", "never"))]
    setnames(data, "cholchk", "lastcholcheck")
    
    # bloodcho, highchol, and cholmeds are always NA if cholchk is NA
    # ever had blood cholester checked?
    if(!is.na(layout[svy, bloodcho])){
      data[, bloodcho := car::recode(bloodcho, "2=0; 1=1; else=NA")] # 1 = yes, 2 = no, 7/9 = DK/RF
    }
    # fill in bloodcho from cholchk if possible, if bloodcho missing:
    if(is.na(layout[svy, bloodcho])){
      data[lastcholcheck == "never", bloodcho := 0]
      data[lastcholcheck %in% c("< 1 yr", "1-2 yrs", "2-5 yrs", "5+ yrs"),bloodcho := 1]
    }
    setnames(data, "bloodcho", "evercheckchol")
    
    # high blood chol
    if(!is.na(layout[svy, highchol])){
      data[, highchol := car::recode(highchol, "2=0; 1=1; else=NA")] # 1 = yes, 2 = no, 7/9 = DK/RF; possibility of skip
    }
    
    # taking meds for high blood chol
    if(!is.na(layout[svy, cholmeds])){
      data[, cholmeds := car::recode(cholmeds, "2=0; 1=1; else=NA")]  # 1 = yes, 2 = no, 7 = don't know, 9 = refused
    }
  }
  
  # standardize diabetes
  if (layout[svy, year] %in% 1990:1993 & !is.na(layout[svy, diabetes])) { # "diabetes" 1990-1993: 1 = yes; 2 = no; 7 = DK/NS; 9 = Refused. In alaska, no is coded as 3 instead of 2 for some reason
    data[, diabetes := car::recode(diabetes, "c(2,3)=0; 1=1; else=NA")]
  } else if (layout[svy, year] %in% 1994:2003 & !is.na(layout[svy, diabetes])) { # "diabetes" 1994-2003: 1 = yes; 2 = yes, but female told only during pregnancy; 3 = no; 7 = DK/NS; 9 = Refused
    data[, diabetes := car::recode(diabetes, "c(2,3)=0; 1=1; else=NA")]
  } else if (layout[svy, year] >= 2004 & !is.na(layout[svy, diabetes])) { # [2004-2011 = "diabete2"; 2012-2018 = "diabete3"; 2019+ = "diabete4] 1 = yes; 2 = yes, but female told only during pregnancy; 3 = no; 4 = no, pre-diabetes or borderline diabetes; 7 = DK/NS; 9 = Refused
    if (!is.na(layout[svy, predb])) {
      data[, predb := ifelse(diabetes == 4, 1, predb)]  # mark yes for predb when answer to "diabetes" is 4, per key commented above
    }
    data[, diabetes := car::recode(diabetes, "c(2,3,4)=0; 1=1; else=NA")]
  }
  
  # standardize pre-diabetes (only available 2008+)
  if (layout[svy, year] == 2008 & !is.na(layout[svy, predb])) {
    data[, predb := car::recode(predb, "2=0; 1=1; else=NA")]  # 1 = yes, 2 = no, 7 = don't know, 9 = refused
  } else if (layout[svy, year] >= 2009 & !is.na(layout[svy, predb])) {
    data[, predb := car::recode(predb, "c(2,3)=0; 1=1; else=NA")]  # 1 = yes, 2 = yes but female told only during pregnancy, 3 = no, 7 = don't know, 9 = refused
  }
  
  # standardize insulin
  if (!is.na(layout[svy, db_insulin])) {  # all available years (1993+): 1 = yes, 2 = no, 7 = don't know, 9 = refused
    data[, db_insulin := car::recode(db_insulin, "2=0; 1=1; else=NA")]
  }
  
  # standardize diabetes pills
  if (!is.na(layout[svy, db_pill])) {  # all available years (2000-2007): 1 = yes, 2 = no, 7 = don't know, 9 = refused
    data[, db_pill := car::recode(db_pill, "2=0; 1=1; else=NA")]
  }
  
  # standardize hypertension and hypertension medication
  if (layout[svy, year] %in% 1990:1992 & !is.na(layout[svy, highbp])) { # 1990-1992 (bphigh): 1 = no; 2 = yes, by a doctor; 3 = yes, by a nurse; 4 = yes, by other health profesional; 7 = DK/NS; 9 = refused
    data[, highbp := car::recode(highbp, "1=0; c(2,3,4)=1; else=NA")]
  } else if (layout[svy, year] %in% 1993:2001 & !is.na(layout[svy, highbp])) { # 1993-2001 (bphigh/bphigh2 -- have teh same options): 1 = yes; 2 = no; 7 = DK/NS; 9 = refused
    data[, highbp := car::recode(highbp, "2=0; 1=1; else=NA")]
  } else if (layout[svy, year] %in% 2002:2004 & !is.na(layout[svy, highbp])) { # 2002-2004  (bphigh3): 1 = yes; 2 = yes, but female told only during pregnancy; 3 = no; 7 = DK/NS; 9 = refused
    data[, highbp := car::recode(highbp, "c(2,3)=0; 1=1; else=NA")]
  } else if (layout[svy, year] >= 2005 & !is.na(layout[svy, highbp])) { # 2005-2017 (bphigh3): 1 = yes; 2 = yes, but female told only during pregnancy; 3 = no; 4 = told borderline high or pre-hypertensive; 7 = DK/NS; 9 = refused
    data[, highbp := car::recode(highbp, "c(2,3,4)=0; 1=1; else=NA")]
  }
  
  if (!is.na(layout[svy, bpmeds])) { # all years: 1 = yes; 2 = no; 7 = DK/NS; 9 = refused
    data[, bpmeds := car::recode(bpmeds, "2=0; 1=1; else=NA")]
    data[highbp == 0, bpmeds := NA]
  }
  
  # prep health insurance (hlthplan and hlthplan1 are virtually identical)
  if(!is.na(layout[svy, hlthplan])) { # all years: 1 = yes; 2 = no; 7 = DK/NS; 9 = refused
    data[, hlthplan := car::recode(hlthplan, "2=0; 1=1; else=NA")]
  }
  
  # prep alcohol impaired driving #### aide and dnkdri no longer used in analysis
  if (!is.na(layout[svy, drnkdri])) { # 1-76 = Number of times; 88 = None; 77 = Don't know/Not sure; 99 = Refused  [question same across yrs, except for changes in skip options]
    # episodes of alcohol impaired (AIDE) driving (within past 30 days)
    # data[, aide := car::recode(drnkdri, "88=0; c(77,99)=NA")]
    # binary indicator for committing AIDE (within past 30 days)s
    # data[, drnkdri := car::recode(drnkdri, "1:76=1; 88=0; else=NA")]
    # fix data entry errors where anydrnk=1 & drnkdri>0
    data[anydrnk == 0, drnkdri := car::recode(drnkdri, "1:76=0")]
  }
  data[, drnkdri := NULL]
  
  # prep employment and out of work
  if(!is.na(layout[svy, employ])) { # 1 = Employed for wages; 2 = Self-employed; 3 = Out of work for more than 1 year; 4 = Out of work for less than 1 year; 5 = A homemaker; 6 = A student; 7 = Retired; 8 = Unable to work; 9 = Refused; same question across years (called employ and employ1)
    data[, outwork := car::recode(employ, "c(3,4)=1; c(1,2,5,6,7,8)=0; else=NA")] # out of work (for any time)
    data[, outwork1 := car::recode(employ, "c(1,2,4,5,6,7,8)=0; 3=1; else=NA")] # out of work for >1 yr
    data[, outwork2 := car::recode(employ, "c(1,2,3,5,6,7,8)=0; 4=1; else=NA")] # out of work for <1 yr
    data[, employ := car::recode(employ, "1:2=1; 3:8=0; else=NA")] # employeed y/n
  }
  
  # prep any PA
  # NOTE the wording of this question changes over time but the response options stay the same; Need some post-extraction bias corrections
  if (!is.na(layout[svy, exerany])) { # all years same options: 1=yes; 2=no
    data[, exerany := car::recode(exerany, "1=0; 2=1; else=NA")] # (note that most RF are ordered where 1 = yes, but reversed in this case, I think b/c an affirmative response is protective for health; question reworded accordingly in next line)
    setnames(data, "exerany", "pa_none")
  }
  
  # standardize sufficient PA
  #
  #
  # Useful for planning: layout[!is.na(recpa) & !grepl("a_pacat", recpa), .(year, file, recpa, modpact, modpaday, modpatim, vigpact, vigpaday, vigpatim, exract01, exerhmm1, exerhmm2, exeroft1, exeroft2)]
  #
  if(!is.na(layout[svy, recpa]) & is.na(layout[svy, exract01])){ # these conditions are met in 2001, 2003, 2005, 2007, 2009: derive PA status to match 2011 definition (mod=moderate; vig=vigorous; *pact = participates (1=yes, 2=no); *paday = number of days; *patim = HMM each day)
    # I changed the filtering critiera from the previous version of this doc to be more flexible about what the recpa file is called in state files; recpa and exract01 will not both be present at the same time
    # The parec var in 2001-2009: (for each categories, the calculations are shown in the codebook)
    #   1 = meets recomenation for phys activity
    #   2 = do some phys act, but do not meet rec
    #   3 = physically inactivie
    #   9 = DK/RF
    # For pre 2001, and 2011 onward, the var is a version of pacat and has the following categories:
    #   1 = Highly active
    #   2 = Active
    #   3 = Insufficently Active
    #   4 = Inactive
    #   9 = DK/RF
    
    data[modpact > 2, modpact := NA] # recode missing
    data[modpaday > 7, modpaday := NA]
    data[modpatim %in% c(777, 888, 999), modpatim := NA]
    data[modpact == 2, c("modpaday", "modpatim") := 0] # replace time and days with 0 for no reported activity
    data[, modpatim := floor(modpatim/100)*60 + (modpatim %% 100)] # convert from HHMM to minutes
    data[, mod := modpaday * modpatim] # calculate total minutes of activity
    
    data[vigpact > 2, vigpact := NA] # recode missing
    data[vigpaday > 7, vigpaday := NA]
    data[vigpatim %in% c(777, 888, 999), vigpatim := NA]
    data[vigpact == 2, c("vigpaday", "vigpatim") := 0] # replace time and days with 0 for no reported activity
    data[, vigpatim := floor(vigpatim/100)*60 + (vigpatim %% 100)] # convert from HHMM to minutes
    data[, vig := vigpaday * vigpatim] # calculate total minutes of activity
    
    data[, total := mod + 2*vig] # moderate equivalent is mod + 2 * vigorous
    data[!is.na(total) & total < 150, pa_insufficient := 1]
    data[!is.na(total) & total >= 150, pa_insufficient := 0]
    data[pa_none == 1, pa_insufficient := 1] # force consistency between any and sufficient activity -- this is consistent with 2011 where the any question is used as a screen
    setnames(data, "total", "pa_min")
    
  } else if(layout[svy, recpa] %in% c("_pacat1", "a_pacat1", "a_pacat", "_pacat", "_pacat2", "a_pacat2")){ # BRFSS already calculated 
    data[, pa_none := car::recode(recpa, "4=1; 1:3=0; else=NA")] # 1= highly active, 2 = active, 3 = insufficently active, 4 = inactive, 9 = DK/RF/missing
    data[, pa_insufficient := car::recode(recpa, "3:4=1; 1:2=0; else=NA")]
    
  } else if (is.na(layout[svy, recpa]) & !is.na(layout[svy, exract01])) { # 1995-2000 & 2011+: calculate derived variable according to 2011 codebook ~ calculate when recpa is not available
    # convert HH:MM to minutes of each activity
    data[, padur1 := ifelse(exerhmm1 %in% c(777, 888, 999), NA, (exerhmm1 %/% 100)*60 + (exerhmm1 %% 100))]
    data[, padur2 := ifelse(exerhmm2 %in% c(777, 888, 999), NA, (exerhmm2 %/% 100)*60 + (exerhmm2 %% 100))]
    # convert unit(week or month) and times, to times per week of each activity
    data[, pafreq1 := round(ifelse(exeroft1 >= 101 & exeroft1 <= 199, exeroft1 - 100, ifelse(exeroft1 >= 201 & exeroft1 <= 299, (exeroft1 - 200)/(30/7), NA)), digits=3)]
    data[, pafreq2 := round(ifelse(exeroft2 >= 101 & exeroft2 <= 199, exeroft2 - 100, ifelse(exeroft2 >= 201 & exeroft2 <= 299, (exeroft2 - 200)/(30/7), NA)), digits=3)]
    
    # keep met activity codes from either 2011 survey list or 1995-2000 list
    if (!is.na(layout[svy, recpa]) & grepl("a_pacat|_pacat", layout[svy, recpa])) { # 2011 activity codes
      if(layout[svy, year] >= 2013){
      }
      met <- na.omit(met_conversion[, list(met, code=code_2011)])
    } else { # 1995-2000 activity codes
      met <- na.omit(met_conversion[, list(met, code=code_2000)])
    }
    # Merge on metabolic equivalent of task (met) values for each activity type
    data <- merge(data, met, by.x="exract01", by.y="code", all.x=T)
    setnames(data, "met", "metval1")
    data <- merge(data, met, by.x="exract02", by.y="code", all.x=T)
    setnames(data, "met", "metval2")
    data[, maxvo2 := ifelse(sex==1, 60 - (.55*age), 48 - (.37*age))] # maximum oxygen capacity based on age and sex
    data[, fc60 := round((.60 *maxvo2) / 3.5, digits = 2)] # estimated functional capacity
    # Calculate estimated activity intensity, 2:vigorous, 1:moderate, 0:no activity
    data[, actint1 := ifelse(metval1 >= fc60, 2, ifelse(metval1 >= 3.0, 1, 0))]
    data[, actint2 := ifelse(metval2 >= fc60, 2, ifelse(metval2 >= 3.0, 1, 0))]
    
    # Calculate minutes of physical activity per week for each activity
    data[, minact1 := round(ifelse(actint1 == 0, 0, ifelse(padur1>=10, padur1 * pafreq1, 0)))]
    data[, minact2 := round(ifelse(actint2 == 0, 0, ifelse(padur2>=10, padur2 * pafreq2, 0)))]
    # Calculate minutes of physical activity per week scaled by the intensity of the activity
    data[, pamin1 := round(ifelse(actint1 == 2, minact1 * 2, ifelse(actint1 == 1, minact1, 0)), digits=1)]
    data[, pamin2 := round(ifelse(actint2 == 2, minact2 * 2, ifelse(actint2 == 1, minact2, 0)), digits=1)]
    data[, pamin := ifelse(is.na(pamin1) & is.na(pamin2), NA, rowSums(cbind(pamin1, pamin2), na.rm=TRUE))]
    # Calculate minutes of vigourous physical activity
    data[, pavigm1 := ifelse(actint1 == 2, minact1, 0)]
    data[, pavigm2 := ifelse(actint2 == 2, minact2, 0)]
    data[, pavigm := ifelse(is.na(pavigm1) & is.na(pavigm2), NA, rowSums(cbind(pavigm1, pavigm2), na.rm=TRUE))]
    
    data[, pacat := ifelse(!is.na(pa_none) & pa_none == 1, 4, ifelse(pamin > 300 | pavigm > 150, 1, ifelse(pamin >= 150 | pavigm > 75, 2, ifelse(pamin > 1 | pavigm > 1, 3, ifelse(pamin == 0 & pavigm == 0, 4, 9)))))]
    data[, recpa := car::recode(pacat, "c(1,2)=0; c(3,4)=1; else=NA")]
    setnames(data, "recpa", "pa_insufficient")
    setnames(data, "pamin", "pa_min")
    data[pa_none == 1, pa_insufficient := 1] # force consistency between any and sufficient activity -- this is consistent with 2011 where the any question is used as a screen
  }
  
  # remove physical activity variables after making the summary variable above
  data[, c("modpact", "modpaday", "modpatim", "mod", "vigpact", "vigpaday", "vigpatim", "vig", "recpa") := NULL]
  data[, c("exract02", "exract01", "exeroft1", "exerhmm1", "exeroft2", "exerhmm2", "padur1", "padur2", "pafreq1", "pafreq2", "metval1", "metval2", "maxvo2", "fc60",
           "actint1", "actint2", "minact1", "minact2", "pamin1", "pamin2", "pavigm1", "pavigm2", "pavigm", "pacat") := NULL]
  
  ### standardize health condition variables
  # asthma ever
  if(!is.na(layout[svy, asthma])){
    data[, asthma := car::recode(asthma, "1=1;2=0; else=NA")] # 1 = y, 2 = n, 7/9 = DK/RF (there are slightly different versions of the asthma var, but they have the same options)
  }
  # asthma now
  if(!is.na(layout[svy, asthma_now])){
    data[, asthma_now := car::recode(asthma_now, "1=1;2=0; else=NA")] # 1 = y, 2 = n, 7/9 = DK/RF 
  }
  # arthritis
  if(!is.na(layout[svy, arthritis])){
    data[, arthritis := car::recode(arthritis, "1=1;2=0; else=NA")] # 1 = y, 2 = n, 7/9 = DK/RF 
  }
  # depression
  if(!is.na(layout[svy, depression])){
    data[, depression := car::recode(depression, "1=1;2=0; else=NA")] # (Ever told) you that you have a depressive disorder, including depression, major depression, dysthymia, or minor depression?
  }
  # edentulism
  if(!is.na(layout[svy, edentulism])){
    data[, edentulism := car::recode(edentulism, "1=1; 2=2; 3=3; 8=0; else = NA")] # 1 = 5 or fewer,  2 = 6 or more, but not all; 3 = all; 7 = dk; 8 = none; 9 = RF; the name of the cats change slightly, but the options are fundamentally the same
  }
  # COPD
  if(!is.na(layout[svy, copd])){
    data[, copd := car::recode(copd, "1=1;2=0; else=NA")]  # (Ever told) you have Chronic Obstructive Pulmonary Disease or COPD, emphysema or chronic bronchitis 1 = yes, 2 = NO
  }
  # pain and activity
  if(!is.na(layout[svy, pain_act])){ # either a number of days (1-30), "none", or DK/RF
    data[pain_act == 88, pain_act := 0] # coded @ 88 means no days of activity-limiting pain
    data[pain_act > 30, pain_act := NA] # DK/RF (otherwise)
  }
  # anxiety
  if(!is.na(layout[svy, anxiety])){
    data[, anxiety := car::recode(anxiety, "1=1;2=0; else=NA")] # Ever told you had an anxiety disorder 1 = yes, 2 = no; high levels of missingness
  }
  # joint pain
  if(!is.na(layout[svy, limitjoint])){
    data[, limitjoint := car::recode(limitjoint, "1=1;2=0; else=NA")]
  }
  
  # how bad is joint pain
  # note: in 2023 there are values between 10 and 77 (don't know/refused) with no further explanation. These are currently coded to NA but might be worth monitoring in future surveys.
  if(!is.na(layout[svy, joinpain])){
    data[, joinpain := ifelse(joinpain %in% 0:10, joinpain, NA)]
  }
  
  # days worried, tense, or anxious
  if(!is.na(layout[svy, qlstress])){ # either a number of days (1-30), "none", or DK/RF
    data[qlstress == 88, qlstress := 0] # coded @ 88 means no days of activity-limiting pain
    data[qlstress > 30, qlstress := NA] # DK/RF (otherwise)
  }
  
  # hlthprb 1) Back or neck problem 2) Other health problem or NA
  # 01 Arthritis/Rheumatism
  # 02 Back or neck problem
  # 03 Fractures, bone or joint injury
  # 04 Walking problem 
  # 05 Lung/Breathing problem 
  # 06 Hearing problem 
  # 07 Eye/Vision problem 
  # 08 Heart problem 
  # 09 Stroke problem
  # 10 Hypertension/High blood pressure 
  # 11 Diabetes
  # 12 Cancer 
  # 13 Depression/Anxiety/Emotional problem 
  # 14 Other impairment/Problem 
  # 77 Don't know/Not sure 
  # 99 Refused
  # Blank Question skipped or Module not used "
  
  # check 01 is same as 0
  if (layout[svy, year] %in% 2000:2002 & !is.na(layout[svy, hlthprb])) { # "hlthprb" 2 is Back or neck problems as main health problem
    data[, hlthprb := car::recode(hlthprb, "2 = 1; c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14) = 2; else = NA")]
  }
  
  # fall questions
  # "For all
  # 1 - 76 Number of times [76=76 or more]
  # 88 None
  # 77 Don’t know/Not Sure
  # 99 Refused
  # BLANK Missing"
  #fall3mn	fallinj	fall12mn
  
  
  if (layout[svy, year] %in% 2006:2023 & !is.na(layout[svy, fall3mn])) { # "fall3mn" how many times have you fallen in the last 3 months. avail every other year
    data[fall3mn == 88, fall3mn := 0]
    data[!fall3mn %in% 0:76, fall3mn := NA]
  }
  
  if (layout[svy, year] %in% 2006:2023 & !is.na(layout[svy, fall12mn])) { # "fall3mn" how many times have you fallen in the last 3 months. avail every other year
    data[fall12mn == 88, fall12mn := 0]
    data[!fall12mn %in% 0:76, fall12mn := NA]
  }
  
  if (layout[svy, year] %in% 2006:2023 & !is.na(layout[svy, fallinj])) { # "fall3mn" how many times have you fallen in the last 3 months. avail every other year
    data[fallinj == 88, fallinj := 0]
    data[!fallinj %in% 0:76, fallinj := NA]
  }
  
  # cidiagaz
  # no value given in 2010 other than blank
  # 1 Yes, Alzheimer´s Disease 
  # 2 Yes, some other form of dementia but not Alzheimer´s disease
  # 3 No diagnosis has been given 
  # 7 Don’t know/Not sure
  # 9 Refused
  # BLANK
  
  if (layout[svy, year] %in% 2010:2011 & !is.na(layout[svy, cidiagaz])) { # "cidiagaz" has a health care professional ever said that [If Q1 = 1 (Yes): insert “you have;” otherwise, insert “this person has”] Alzheimer’s disease or some other form of dementia?
    data[, cidiagaz := car::recode(cidiagaz, "3=0; c(1,2)=1; else=NA")]
  }
  
  # crgvalzd
  # 1        Yes
  # 2        No
  # 7        Don’t know/Not Sure        
  # 9        Refused        
  # BLANK        Not asked or Missing
  if (layout[svy, year] %in% 2019:2023 & !is.na(layout[svy, crgvalzd])) { # "crgvalzd" caregiver asked about dementia in person being cared for
    data[, crgvalzd := car::recode(crgvalzd, "2=0; 1=1; else=NA")]
  }
  
  # prostate (same recoding as crgvalzd)
  if (layout[svy, year] %in% 2001:2011 & !is.na(layout[svy, prostate])) { # "prostate" has a doctor ever told you...
    data[, prostate := car::recode(prostate, "2=0; 1=1; else=NA")]
  }
  
  # cncrtyp -> 1) Breast cancer 2) Prostate 3) other cancer
  # other cancers 2-17
  # 18 Prostate cancer
  # other cancers 19-28
  # 29 Other
  # 77 Don’t know/Not Sure
  # 99 Refused 
  # BLANK
  # 
  # 2012-2020
  # 19 Prostate cancer"
  # 30-other
  if (layout[svy, year] %in% 2009:2011 & !is.na(layout[svy, cncrtyp])) { # "cncrtyp" what kind of cancer you had...
    data[, cncrtyp := car::recode(cncrtyp, "1=1; 18=2; c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29)=3; else=NA")]
  } else if (layout[svy, year] %in% 2012:2023 & !is.na(layout[svy, cncrtyp])) {
    data[, cncrtyp := car::recode(cncrtyp,  "1=1; 19=2; c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)=3; else=NA")]
  }
  
  # cancer
  # 1 = yes; 2 = no; 7 = don't know; 9 = refused; blank = not asked/missing
  if (!is.na(layout[svy, hvcncr])) {  # has a doctor ever told you that you have cancer?
    data[, hvcncr := car::recode(hvcncr, "2=0; 1=1; else=NA")]
  } else if (!is.na(layout[svy, hvcncr_skin])) {  # has a doctor ever told you that you have skin cancer?
    data[, hvcncr := car::recode(hvcncr_skin, "2=0; 1=1; else=NA")]
  } else if (!is.na(layout[svy, hvcncr_any])) {  # has a doctor ever told you that you have any other types of cancer (asked after skin cancer)?
    data[, hvcncr := car::recode(hvcncr_any, "2=0; 1=1; else=NA")]
  } else {  # no "told have cancer" variable
    data[, hvcncr := NA]
  }
  data[, c("hvcncr_any", "hvcncr_skin") := NULL]  # don't need these after recoding to hvcncr
  
  ######----- Process variables for childhood asthma
  if (!is.na(layout[svy, child_asthma])) {
    # (1) Process birthday or calculate age
    if (layout[svy, year] %in% 2007:2012) { # Child age reported in months
      data[, age_continuous_child := ifelse(child_age >= 0 & child_age <= 215, child_age/12, NA)] 
    } else if (layout[svy, year] %in% c(2006)) { # this year didn't include calculated age but included birthday
      data[, child_birthday := ifelse(child_birthday == "", NA, child_birthday)]
      # birthday reported in month and year
      data[, `:=` (birth_month = substr(child_birthday, 1, 2), birth_year = substr(child_birthday, 3, 6))]
      # Convert IDATE to Date object
      data[, interview_date := mdy(idate)]
      # Create a birth_date column from birth_month and birth_year
      data[, birth_date := ymd(paste0(birth_year, birth_month, "01"))]
      # Calculate age
      data[, age_continuous_child := interval(birth_date, interview_date) / years(1)]
      
      # Some entries have month missing for birthday, so we can estimate these ages using "age = 2006 - birth year"
      data[, age_continuous_child := ifelse((birth_month == 77 | birth_month == 99) & birth_year != 9999 & birth_year != 7777, 2006-as.numeric(birth_year), age_continuous_child)]
      
      # Set missing birthdays to NA - set the age range to 0-17
      data[, age_continuous_child := ifelse(birth_year == 9999 | birth_year == 7777 | child_birthday == 99 | child_birthday == 8888, NA, age_continuous_child)]
      data[, child_age_lower := ifelse(is.na(age_continuous_child), 0, NA)]
      data[, child_age_upper := ifelse(is.na(age_continuous_child), 17, NA)]
      
    
    } else if (layout[svy, year] %in% 2021:2023) { # for these three years they only included 5 year age categories
      data[, child_age_lower := ifelse(child_age == 1, 0, # 1 = 0 months to 4 yrs
                                       ifelse(child_age == 2, 5, # 2 = 5 yrs to 9 yrs
                                              ifelse(child_age == 3, 10, # 3 = 10 yrs to 14 yrs
                                                     ifelse(child_age == 4, 15, NA))))] # 4 = 15 yrs to 17 yrs
      data[, child_age_upper := ifelse(child_age == 1, 4, # 1 = 0 months to 4 yrs
                                       ifelse(child_age == 2, 9, # 2 = 5 yrs to 9 yrs
                                              ifelse(child_age == 3, 14, # 3 = 10 yrs to 14 yrs
                                                     ifelse(child_age == 4, 17, NA))))] # 4 = 15 yrs to 17 yrs
                                              
    } else if (layout[svy, year] %in% 2013:2020) { # for these years the child age variable isn't included
      data[, child_age_lower := 0]
      data[, child_age_upper := 17]
    }
    
    # (2) Process child sex
    data[!child_sex %in% 1:2, child_sex := NA]
  
    # (3) Process child race/ethnicity
    # create a OMB 1977-style race-ethnicity code (unlike race97 var, does not have a multiracial category -- uses respondents "primary" race; create API category)
   if(layout[svy, year] %in% 2006:2012) {
        data[, child_race77 := car::recode(child_race_calc_mult, "1=0; 2=1; c(3,4)=3; 5=2; 6=5; 7=6; else=NA")] #1 White; 2 Black or African American ; 3 Asian; 4 Native Hawaiian or other Pacific Islander;  5 American Indian or Alaskan Native; 6 Other race; 7 Multiracial; 77 Don't know/not sure; 99 Refused 
        data[child_hisp == 1, child_race77 := 4] # code as Hispanic if respondent identifies as Hispanic/Latino
      } else if(layout[svy, year] %in% 2013:2021) {
        data[, child_race77 := car::recode(child_race_calc_mult, "1=0; 2=1; 3=2; c(4,5)=3; 6=5; 8=6; else=NA")] # 1 White; 2 Black or African American; 3 American Indian or Alaskan Native; 4 Asian; 5 Native Hawaiian or other Pacific Islander; 6 Other race; 8 Multiracial but preferred race not answered;  77 Don’t know/Not sure; 99 Refused
        data[child_hisp == 1, child_race77 := 4] # code as Hispanic if respondent identifies as Hispanic/Latino    
      } else if(layout[svy, year] %in% 2022) {
        data[, child_race77 := car::recode(child_race_calc_mult, "1=0; 2=1; 3=2; c(4,5)=3; 6=6; else=NA")] # 1 White; 2 Black or African American; 3 American Indian or Alaskan Native; 4 Asian; 5 Native Hawaiian or other Pacific Islander; 6 Multiracial but preferred race not answered; 77 Don’t know/Not sure; 99 Refused
        data[child_hisp == 1, child_race77 := 4] # code as Hispanic if respondent identifies as Hispanic/Latino    
      } else if(layout[svy, year] %in% 2023) {
        data[, child_race77 := car::recode(child_race_calc_mult, "1=0; 2=1; 3=2; c(4,5)=3; 6=5; 7=6; else=NA")] # 1 White; 2 Black or African American; 3 American Indian or Alaskan Native; 4 Asian; 5 Native Hawaiian or other Pacific Islander; 6 Other race; 7 Multiracial but preferred race not answered; 77 Don’t know/Not sure; 99 Refused
        data[child_hisp == 1, child_race77 := 4] # code as Hispanic if respondent identifies as Hispanic/Latino    
      }
    
    data[, child_race77 := factor(child_race77, levels=0:6, labels=c("NH White", "NH Black", "NH AIAN", "NH API", "Hispanic", "NH Other", "NH MR w/o main"))]
    
    # (4) Process child asthma variables
    if(!is.na(layout[svy, child_asthma])){ # asthma ever
      data[, child_asthma := car::recode(child_asthma, "1=1;2=0; else=NA")] # 1 = y, 2 = n, 7/9 = DK/RF (there are slightly different versions of the asthma var, but they have the same options)
    }
    if(!is.na(layout[svy, child_asthma_now])){ # asthma now
      data[, child_asthma_now := car::recode(child_asthma_now, "1=1;2=0; else=NA")] # 1 = y, 2 = n, 7/9 = DK/RF 
    }
  }
  data <- labelled::remove_labels(data) # remove labels imported from the original files
  
  # join the file information to each row of the microdata (vars defined at top of script); avoid duplicate columns
  file_meta <- layout[svy, file_info, with = F]
  data <- cbind(file_meta, data[, c("supplement", "year") := NULL])
  
  return(data)
})  # use with mclapply or lapply
# }  # use with for loop

## Combine files and format geography variables ----------------------------------------------------
data <- rbindlist(brfss_data, use.names=T, fill=T)

# Merge state-provided county & imputed county information to fill in missing codes ----------------
keys <- c("seqno", "state", "year")

# merge cnty codes from state-provided data extraction
# note: ne_lhd stands for Nebraska local health department. See brfss_state_cnty_crosswalk.R for details
state_cnty_codes <- readRDS(paste0(unique(get_brfss_geo_mapping(input_type = "state", get_best = T)$output_file_path),
                                   "state_cnty_codes.rds"))
setnames(state_cnty_codes, c("cnty", "imp_cnty"), c("cnty_STATE", "imp_cnty_STATE"))
state_cnty_codes[, `:=`(seqno = as.character(seqno))]
setkeyv(state_cnty_codes, keys)
data <- merge.data.table(data, state_cnty_codes, by = keys, all.x = T)

# IL 2007 and DE 2011 have incorrect FIPS; replace with correct value from state data
data[state == 17 & year == 2007 & cnty %in% c(30, 32), cnty := cnty_STATE]
data[state == 10 & year == 2011 & cnty %in% c(7, 17), cnty := cnty_STATE]

# In some rows of the national file, WI 2008 retains what looks like WI county codes (1-72) instead of county FIPS (1-141 odd numbers)
# assign cnty_STATE_WI from codes 1-141 odds to 1-72 inclusive using info from WI codebook, which lists 1-72 as corresponding with counties in alphabetical order.
# This is the same way that FIPS are assigned, so we can directly map 1-72 inclusive to 1-141 odds to get the official FIPS.
wi_cnty_fips <- sort(unique(state_cnty_codes[state == 55, cnty_STATE]))  # gather fips codes 1-141 odd numbers
wi_cnty_ids <- 1:72  # set WI county IDs 1-72
wi_cnty_id_to_fips <- data.table(year = 2008, state = 55, cnty_STATE = sort(wi_cnty_fips), cnty_STATE_WI = sort(wi_cnty_ids))  # create crosswalk between FIPS and cnty IDs
data <- merge.data.table(data, wi_cnty_id_to_fips, by = c("year", "state", "cnty_STATE"), all.x = T)
data[state == 55 & year == 2008, cnty := ifelse(cnty == cnty_STATE_WI, cnty_STATE, cnty)]  # if national data cnty value matches the WI cnty ID, then assign FIPS from cnty_STATE
data[, cnty_STATE_WI := NULL]

# when we have cnty/imp_cnty from both state and national, assess mismatches
# in the case of mismatches, we will always default to national file county values as they tend to be more stable
data[, cnty_match := ifelse(cnty == cnty_STATE, 1, 0)]
data[, imp_cnty_match := ifelse(imp_cnty == imp_cnty_STATE, 1, 0)]
cat(round(100*mean(data[!is.na(cnty) & !is.na(cnty_STATE), cnty_match] == 0), 2),
    "% of state-provided county codes do not match the corresponding national file codes.")
cat(round(100*mean(data[!is.na(imp_cnty) & !is.na(imp_cnty_STATE), imp_cnty_match] == 0), 2),
    "% of state-provided imputed county codes do not match the corresponding national file codes.")
mismatches <- copy(data[!is.na(cnty) & !is.na(cnty_STATE), list(count = .N), by = "state,year,cnty_match"])
mismatches[, total_count := sum(count), by = "state,year"]
mismatches[, freq := round(count/total_count, 5)]
print(mismatches[cnty_match == 0, -"total_count"][order(state)])

# save intermediate file of actual state-years where state-provided county codes
# were used to fill missing values in general data
tmp_st_provided <- data[is.na(cnty) & !is.na(cnty_STATE), list(n_cases = .N), by = .(year, state)]
loc <- fread("/snfs1/Project/us_counties/locations/counties/locations.csv")
tmp_st_provided <- merge.data.table(tmp_st_provided, loc[, .(fips, location_name)],
                                    by = c("state" = "fips"), all.x = T)
setcolorder(tmp_st_provided, c("year", "location_name", "state", "n_cases"))
tmp_st_provided <- tmp_st_provided[order(location_name)]
fwrite(tmp_st_provided, paste0(output_dir, "/state_years_to_cite.rds"))
rm(tmp_st_provided, loc)

# fill missing cnty/imp_cnty with state-provided values
data[is.na(cnty), cnty := cnty_STATE]
data[is.na(imp_cnty), imp_cnty := imp_cnty_STATE]
data[, c("cnty_STATE", "imp_cnty_STATE", "cnty_match", "imp_cnty_match") := NULL]

rm(wi_cnty_fips, wi_cnty_ids, wi_cnty_id_to_fips)

# Drop territories --------------------------------------------------------------------------------
data <- data[as.integer(state) < 60 | is.na(state)] # add is.na(state) so we don't drop suppressed vars/MMSA vars

# save state population to use in vetting
state_yr_n <- data[SMART == 0 & state_file == 0, .N, by = c("state", "year")]
saveRDS(state_yr_n, file=paste0(output_dir, "../gen_brfss_state_n.rds"))

# save(data, file=paste0(output_dir, "brfss_temp1.rdata"))
# print("Saved intermediate output (temp1)")

################################################################################
# Deduplicate respondents and select rows with the most geographic details--------
################################################################################
# Logic of dedudplicating explained here: https://docs.google.com/document/d/1sV0Sr8TXuBEAnJYa8dYQxxjgSOUK9Z3lI4L31fJF9Pw/edit?usp=sharing
# load(paste0(output_dir, "brfss_temp1.rdata"))

####1985-2009
# Remove rows corresponding to files 2005 or earlier that are  NOT (in LU &
# not SMART & a national file ^ and not supplemental). i.e., for years <= 2005,
# only keep LU national BRFSS because it has full county details
setkey(data, "year")
data <- data[!(year <= 2005 & !(SMART == 0 & state_file == 0 & supplement == 0))]
data <- data[!(state_file == 0 & year <= 2009)] # remove public-use national files through 2009, since state or LU files will have more geog detail

#### 2010-2012: Compare best of (state file vs general BRFSS) to CNTY SMART files
# 6/21/2021: removing the function that compares level of restriction in state files to
#   county SMART files (2012 or earlier) because I found in earlier runs that SMART CNTY
#   is only more detailed than general BRFSS in 2012 Alaska, but we will eventually have 
#   LU data for that state, so no need to go through this process. Can look in commit history
#   for this function.

# load(paste0(output_dir, "brfss_temp2.rdata"))

# remove 2012 and earlier SMART CBSA b/c we choose to use the CNTY SMART or General data in these years
data <- data[!(SMART == 1 & year <= 2012 & geography == "CBSA")]

# remove CNTY SMART BRFSS rows; was previously done when comparing level of suppression
#   in each state, but found that state data always has more details.
data <- data[!(SMART == 1 & year <= 2012 & geography == "CNTY")]

# save(data, file=paste0(output_dir, "brfss_temp3.rdata"))
# print("Saved intermediate output (temp3)")

# Merge SMART/BRFSS -------------------------------------------------------
#### 2012+: Merge SMART and general BRFSS to get the most geographic info
# A crosswalk between the seqno in data and the CBSA or MSA codes of the corresponding
#   respondent in the SMART data was created in match_general_smart_brfss.R
# load(file=paste0(output_dir, "brfss_temp3.rdata"))

# Use the file that links respondents in general BRFSS to CBSA info form SMART
crosswalk <- readRDS(paste0(unique(get_brfss_geo_mapping(input_type = "smart", get_best = T)$output_file_path),
                            "general_smart_cbsa_crosswalk.rds"))

crosswalk[, `:=`(year = as.integer(year), 
                 state = as.integer(state))]
crosswalk[, merge_on := 1]# make a merge_on var to make it easier to only merge the crosswalk to section of data
# crosswalk[, state := as.character(state)]
data[year %in% unique(crosswalk$year), merge_on := 1] # limit the files to merge on to general and SMART BRFSS
data <- merge(data, crosswalk, by = c("seqno", "year", "state", "merge_on"), all = T)
data[, merge_on := NULL]
# replace cbsa-vars in 'data' (which are blank for general BRFSS) with the values merged in, and then remove those values
data[SMART == 0 , `:=`(cbsa = mmsa, # only do this for general BRFSS data
                       cbsa_name = mmsa_name,
                       cbsa_wt = mmsa_wt)]
data[, c("smart_seqno", "mmsa", "mmsa_name", "mmsa_wt") := NULL]

### remove respondents from SMART CBSA files EXCEPT for respondents who were not matched
save_cbsa <- crosswalk[!is.na(mmsa) & is.na(seqno)] # there is a line for CBSA/SMART but not general
save_cbsa[, `:=`(smart_seqno = as.character(smart_seqno))]
# make a DT of the SMART data we DO want to keep; remove the smart data from the main data DT, and then merge together what we want and the rest of the data (cumbersom but easier than doing a complciated subset with a bunch of conditions)

save_cbsa[,  mmsa := as.character(mmsa)]
save_cbsa_dat <- data[save_cbsa, on = c("year" = "year", "seqno" = "smart_seqno", "cbsa" = "mmsa")] # merge on the SMART data
data <- data[!(SMART == 1 & geography == "CBSA")]
data <- rbind(data, save_cbsa_dat[, c("smart_seqno", "i.mmsa_name", "i.seqno", "i.state", "i.mmsa_wt", "merge_on", "mmsa_wt", "mmsa_name", "i.smart_ststr") := NULL])

# save(data, file=paste0(output_dir, "brfss_temp4.rdata"))
# print("saved temp4")

###################################################################################################
# Clean up data -----------------------------------------------------------------------------------
###################################################################################################
# load(paste0(output_dir, "brfss_temp4.rdata"))
# map to merged counties
data[, state := as.integer(state)]
data[!is.na(cnty), cnty := 1000*state + cnty]
loc <- fread("/snfs1/Project/us_counties/locations/counties/merged_counties.csv")
data <- merge(data, loc[, list(cnty, mcnty)], by="cnty", all.x=T)

# make indicator of the broad source
data[state_file == 0 & SMART == 0, source := "general"]
data[SMART == 1, source := "SMART"]

#### add NIDS
nids <- fread("/snfs1/Project/us_counties/survey_extractions/nids/brfss_nids.csv")
data <- merge(data, nids, by = c("year", "source"), all.x = T)  # add primary NIDs
stopifnot(nrow(data[is.na(nid)]) == 0)  # all rows should have a primary NID

# add secondary NIDS when geographic or other information was merged from one file onto another
#   (i.e., state-provided county info onto general files -- only applies to rows with non-missing geo information)
#   NOTE: we don't need secondary NIDs for SMART files b/c they have the same record NID as the national files for the same year
# First, add state codes to NIDS spreadsheet to make merging easie.r
#   Merge on either the state name or expression (the string for source comes from
#   the file path of the state files, so we need to be able to merge on either)
states <- fread("/snfs1/Project/us_counties/locations/counties/state_map.csv")
states <- data.table::melt(states, id.vars = "fips", measure.vars = c("alpha", "name"), value.name = "name")[, variable := NULL]
states[, name := toupper(name)]
setnames(states, "fips", "state")
nids <- merge(nids, states, by.x = "source", by.y = "name", all.x = T)

# create an indicator so we only merge on rows with cnty data
nids[, cnty_dat := 1]
data[!is.na(mcnty) | (state == 31 & year %in% 2011:2022), cnty_dat := 1]  # NE 2011-2022 won't have mcnty assigned yet due to health department-level data at this stage (xwalked to county later)

setnames(nids, "nid", "nid2")  # make a secondary NID associated with the state
extracted_states <- unique(state_cnty_codes[, list(extracted = 1), by = .(state, year)])
extracted_states[, state := as.numeric(state)]
nids <- merge(nids, extracted_states, by = c("state", "year"), all.x = T)
data <- merge(data, nids[extracted == 1, .(year, state, cnty_dat, nid2)], by = c("year", "state", "cnty_dat"), all.x = T)
data[, cnty_dat := NULL]
setnames(nids, "nid2", "nid")  # set name back to original for metadata upload
nids[, c("state", "cnty_dat") := NULL]  # no longer need these columns

# set state-specific source for observations with a secondary (state-provided) nid
data <- merge(data, nids[, list(year, source_state = source, nid2 = nid)], by = c("year", "nid2"), all.x = T)  # get state names to set as sources
data[!is.na(nid2), source := source_state]  # set appropriate source name based on secondary NID
data[, "source_state" := NULL]  # no longer need source state column
stopifnot(nrow(data[is.na(source)]) == 0)  # all rows should have a source

## Format and save --------------------------------------------------------------------------------
data[, c("landline_file", "state_file", "SMART", "geography", "overlap_natl_file", "supplement") := NULL]
setkeyv(data, c("year", "sample", "state", "sex", "age", "race97", "edu", "marital"))
data[, id := 1:.N]

# write output
saveRDS(data, file=paste0(output_dir, "brfss_microdata.rds"))
print("saved microdata")

## Collect and upload extraction metadata to USHD database ----------------------------------------
# this should be checked/updated with each run!
# collect file paths
smart_brfss_metadata <- get_brfss_geo_mapping(input_type = "smart", get_best = T)
smart_file_paths <- smart_brfss_metadata[, list(nid, file2 = as.character(path_list))]
state_brfss_metadata <- get_brfss_geo_mapping(input_type = "state", get_best = T)
state_file_paths <- state_brfss_metadata[, list(nid, file = as.character(path_list[[1]]),
                                                file2 = ifelse(nid == 476672, as.character(path_list[[2]]), NA_character_))]  # CA 2013 has a CDC crosswalk file as well

gen_smart_file_paths <- copy(unique(data[, list(nid, file)]))  # we don't need the nid2 column because these are all accounted for in the steps above
gen_smart_file_paths <- merge(gen_smart_file_paths, smart_file_paths, by = "nid", all.x = T)  # SMART BRFSS has same NIDs as general BRFSS

all_file_paths <- rbind(gen_smart_file_paths, state_file_paths, fill = T)
stopifnot(all.equal(sort(unique(c(data$nid, data$nid2))), sort(unique(c(gen_smart_file_paths$nid, nids[extracted == 1, nid])))))  # make sure NIDs align in data and metadata
nid_path_dict <- lapply(1:nrow(all_file_paths), function(i) {
  if (!is.na(all_file_paths[i, file2])) {
    list(all_file_paths[i, file], all_file_paths[i, file2])  # make sure to catch NIDs that have two file paths and put them in the same list
  } else {
    list(all_file_paths[i, file])
  }
})
names(nid_path_dict) <- all_file_paths[, nid]

# collect commit hash info
repo_git_cmd <- paste0("cd ", repo_dir, "; git rev-parse --short HEAD")
commit <- system(repo_git_cmd, intern = TRUE)

# upload metadata
survey_extraction_version_id <-
  save_survey_extraction(input_type = "brfss",
                         nid_path_dict = nid_path_dict,
                         output_file_path = output_dir,
                         commit_hash = commit,
                         modeler = Sys.info()[['user']],
                         prev_issues = "PSU information not retained prior to 2010",
                         description = paste0("Added primary sampling unit per request from Cesar for estimating corrections for survey design."),
                         discussion = paste0("In the previous extraction, PSU was not retained before 2010 due to deduplication of respondents ",
                                             "PSU was thought to ",
                                             "only be present in public-use data due to misleading documentation, but actually does exist in the ",
                                             "LU data upon examination. This extraction contains the PSU from those files through 2009."),
                         brfss_smart_version_id = unique(smart_brfss_metadata$brfss_geo_mapping_version_id),
                         brfss_state_version_id = unique(state_brfss_metadata$brfss_geo_mapping_version_id),
                         is_best = F)  # default FALSE to allow vetting before marking best with update_survey_extraction() below

## Generate report --------------------------------------------------------------------------------
print("Make diagnostic plots")
source("vet_brfss.R")

print("Generating report...")
report <- function(data) {
  d <- lapply(data, function(x) {
    if (class(x) == "character") temp <- function(x) paste("unique values:", length(unique(x)))
    if (class(x) != "character" & length(unique(na.omit(x))) < 10) temp <- function(x) paste("table:", paste(paste(names(table(x)), " (", table(x), ")", sep=""), collapse=", "))
    if (class(x) != "character" & length(unique(na.omit(x))) >= 10) temp <- function(x) paste("range:", round(min(x, na.rm=T), 2), "to", round(max(x, na.rm=T), 2))
    c(temp(x), paste("num missing:", sum(is.na(x))), paste("% missing:", 100*round(mean(is.na(x)), 3)))
  })
  d <- do.call("rbind", d)
  d <- cbind(row.names(d), d)
  colnames(d) <- c("Variable", "Summary", "Number_Missing", "Perc_Missing")
  d
}

brfss_report <- lapply(c(sort(unique(data$year)), 9999), function(yr) if (yr == 9999) cbind(year="All", report(data)) else cbind(year=yr, report(data[data$year %in% yr,])))
brfss_report <- do.call("rbind", brfss_report)
brfss_report <- brfss_report[order(brfss_report[,2], brfss_report[,1]),]
write.csv(brfss_report, file=paste0(output_dir, "brfss_data_report.csv"), row.names=F)

print("Make maps of data coverage")
source("map_data_coverage.R")
print("END")
closeAllConnections() # Close connection to log file

# update symbolic link so that output dir is "latest"
system(paste0("ln -sfn ", output_dir, " ", output_dir, "../_LATEST"))

## Update extraction as best upon vetting ---------------------------------------------------------
# # uncomment below call to set current extraction as best
# update_survey_extraction(best_status = 1,
#                          survey_extraction_version_id = survey_extraction_version_id,
#                          prev_issues = "new variable available (PSU)")
