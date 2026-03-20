####################################################################################################
## Description: Compile all NHANES microdata and recode variables for consistency across years
##
## Inputs:   Spreadsheet containing path to each NHANES file and what variable codes to pull
##
## Output:  'nhanes_microdata.rds' -- contains a data.table with one row per
##              respondent,identifying and demographic variables, sample weights, and selected other
##              self-report and measured variables, all consistently defined across years. 
##          `nhanes_var_distribution.pdf` -- plots of variable distribution 
##          'nhanes_data_report.csv'  -- a report on the range and missingness for each variable, by
##              year and overall.
##
##          Note that this script creates a time-stamped directory within the NHANES 
##            directory, unique for each run of this extraction code
##         [/snfs1/Project/us_counties/survey_extractions/nhanes]
##
####################################################################################################
if (dir.exists(paste0('FILEPATH', Sys.info()['user']))) {
  repo_dir <- paste0('FILEPATH')
} else {
  repo_dir <- paste0('FILEPATH')
}
setwd(paste0(repo_dir, "1_data_prep/counties/survey_data/"))

library(data.table)
library(foreign)
library(car)
source("_functions.R") # utility functions
source("vetting_plots.R") # function for vetting plots
source(paste0(repo_dir, "0_functions/helper/_versioning_functions.R")) # includes make_time_stamp()
source(paste0(repo_dir, "0_functions/load_ushd_db.R"))  # load USHD database

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

loaddata <- function(layout, svy, file, vars) {
  data <- data.table(read_file(paste(layout[svy, dir], layout[svy, file, with=F], sep="/")))
  setnames(data, tolower(names(data)))
  data <- data[, c("seqn", as.character(layout[svy, vars, with=F])), with=F]
  setnames(data, c("seqn", vars))
  return(data)
}

## Read in variable layout and loop over surveys ---------------------------------------------------
layout <- fread("nhanes_files_and_variables.csv")
data <- lapply(1:nrow(layout), function(svy) { # c(1:8,10), function(svy) { #1:nrow(layout), function(svy) {
  cat(paste(svy, "of", nrow(layout), ", survey year: ", layout[svy, svyyear], "\n")); flush.console()
  ## Load and format demographics file ---------------------------------------------------------------
  if(layout[svy, race2] == ""){
    demo <- loaddata(layout, svy, "demo_file", c("sex", "age", "race", "marital", "edu", "preg", "half", "mec_wt", "int_wt", "psu_pseudo", "stra_pseudo"))  
  } else{
    demo <- loaddata(layout, svy, "demo_file", c("sex", "age", "race", "race2", "marital", "edu", "preg", "half", "mec_wt", "int_wt", "psu_pseudo", "stra_pseudo"))  
  }
  
  # race
  demo[, race := car::recode(race, "1:2=2; 3=0; 4=1; 5=3; else=NA")]  # 1 = Mexican, 2 = Other Hispanic, 3 = NH White, 4 = NH Black, 5 = Other (incl. multiracial)
  demo[, race := factor(race, levels=0:3, labels=c("NH White Only", "NH Black Only", "Hispanic", "other"))]
  
  # race 2, which includes NH Asian (2011 onward)
  if(layout[svy, race2] != ""){
    demo[, race2 := car::recode(race2, "1:2=2; 3=0; 4=1; 7=3; 6=4; else=NA")]  # 1 = Mexican, 2 = Other Hispanic, 3 = NH White, 4 = NH Black, 6 = NH Asian, 7 = Other
    demo[, race2 := factor(race2, levels=0:4, labels=c("NH White Only", "NH Black Only", "Hispanic", "other", "NH Asian Only"))]  
  } else{
    demo[, race2 := NA]
  }
  
  # topcode age at 80 to be consistent across years
  demo[age > 80, age := 80]
  
  # marital
  if (layout[svy, svyyear] %in% c("2017_2020prp", "2021_2023")) {  # different coding as of pre-pandemic data
    demo[, marital := car::recode(marital, "1=1; 2=0; 3=2; else=NA")]
  } else {
    demo[, marital := car::recode(marital, "1=1; 2=0; 3=0; 4=0; 5=2; 6=1; else=NA")]
  }
  demo[, marital := factor(marital, levels=0:2, labels=c("former", "current", "never"))]
  
  # edu
  demo[, edu := car::recode(edu, "1=0; 2=0; 3=1; 4=2; 5=3; else=NA")]
  demo[, edu := factor(edu, levels=0:3, labels=c("less than HS", "HS grad", "some college", "college grad"))]
  
  # pregnancy status
  demo[, preg := car::recode(preg, "1=1; 2=0; else=NA")]
  
  # scale 4-year weights
  # Per the NHANES analytic guidelines, we are using 4-year weights for 1999-2000 
  #   and 2001-2002 waves:
  #   https://wwwn.cdc.gov/nchs/data/nhanes/analyticguidelines/99-10-analytic-guidelines.pdf
  # "The NHANES 1999-2000 sample weights were based on information from the 1990 U.S. census. 
  #   The NHANES 2001–2002 sample weights, and all subsequent 2-year cycles, were based on the 
  #   2000 census. Because different population bases were used, the 2-year weights for 
  #   1999–2000 and 2001–2002 are not directly comparable. For analyses of the combined 1999–2000 
  #   and 2001–2002 survey years, 4-year sample weights (i.e., interview, examination, and all 
  #   subsample weights) were created to account for the two different reference populations. 
  #   Because NHANES 2003–2004 and all subsequent survey cycles used the same 2000 census counts 
  #   that were used for NHANES 2001–2002, no other special 4-year weights were needed."
  if(layout[svy, int_wt] == "wtint4yr"){
    # First check that 4-year weights are used consistently
    stopifnot(all(layout[svy, .(int_wt, mec_wt, mec_fast_wt)] %like% "4yr"))
    demo[, c("mec_wt", "int_wt") := list(mec_wt*2, int_wt*2)]
  }
  
  ## Load and format body measurement module ---------------------------------------------------------
  bmx <- loaddata(layout, svy, "bmx_file", c("bmi", "weight", "height"))
  # convert height to meters
  bmx[, height := height/100]
  
  ## Load and format self-reported height/weight ----------------------------------------------
  
  whq <- loaddata(layout, svy, "whq_file", c("weight_report", "height_report"))
  whq[!weight_report %in% 80:400, weight_report := NA] # range of valid values for NHANES (codebook)
  whq[!height_report %in% 48:83, height_report := NA]# range of valid values for NHANES (codebook)
  whq[, weight_report := weight_report/2.2046] # convert to KG
  whq[, height_report := height_report/39.37] # convert to meters
  whq[, bmi_report := weight_report/height_report^2]
  
  ## Load and format cigarette smoking interview module ----------------------------------------------
  smq <- loaddata(layout, svy, "smq_file", c("smkev", "smknow"))
  
  # smoking status
  smq[, smkev := car::recode(smkev, "1=1; 2=0; else=NA")]
  smq[, smoke := ifelse(smkev == 1, car::recode(smknow, "1=1; 2=2; 3=3; else=NA"), 4)]
  
  # current smokers
  smq[, smoke_any := as.numeric(smoke <= 2)]
  smq <- smq[, list(seqn, smoke_any, smoke)]
  
  ## Load and format alcohol interview module --------------------------------------------------------
  if (layout$alq_file[svy] != "" & layout$alc1yr[svy] != "") {
    alq <- loaddata(layout, svy, "alq_file", c("alc1yr", "alclife", "alc12mn", "alc12mu", "alc12avg", "alc5upn", "alc5upu"))
    alq <- merge(alq, demo[, list(seqn, sex)], by="seqn", all.x=T)
    
    # recode alc1yr, alcever, and alclife and fill in from each other
    alq[, alc1yr := car::recode(alc1yr, "1=1;2=0;7:9=NA")]
    alq[, alclife := car::recode(alclife, "1=1;2=0;7:9=NA")]
    alq[!is.na(alc1yr) & alc1yr == 1, alclife := 1]
    alq[(!is.na(alclife) & alclife == 0), alc1yr := 0]
    
    # calculate drinking days from 'alc12mu' and 'alc12mn'
    alq[alc12mn > 366, alc12mn := NA]
    alq[!is.na(alc12mn) & alc12mn == 0, alcday := 0]
    alq[!is.na(alc12mu) & alc12mu == 1, alcday := pmin(alc12mn, 7)/7]
    alq[!is.na(alc12mu) & alc12mu == 2, alcday := pmin(alc12mn, 30)/30]
    alq[!is.na(alc12mu) & alc12mu == 3, alcday := pmin(alc12mn, 365)/365]
    
    # Set drinking frequency response to "never" for people who have had <12 drinks 
    #  in lifetime even though it's possible they have drank in last year; 
    #  most people who have drank very little in lifetime won't have drinking levels
    #  above TMREL, so it won't bias results and setting as NA loses a lot of info
    alq[!is.na(alclife) & alclife == 0, alcday := 0]
    # create categorical frequency var  that matches 2017_2018 categories (note that these may not line up with what respondent might have said b/c of response type bias)
    alq[!is.na(alc12mn), alc12freq := fcase(alcday == 0, 0L, # never
                                            alcday == 1, 1L, # everyday
                                            alcday > 0.5714286, 2L, # more than 4x/wk = "nearly every day"
                                            alcday > 0.2857143, 3L, # more than 2x/wk = "3-4x/wk"
                                            alcday > 0.1428571, 4L, # more than 1x/wk = "2x/wk"
                                            alcday > 0.1, 5L, # more than 3x/month = "1x/wk"
                                            alcday > 0.03333333, 6L, # more than 1x/month = "2-3x/month"
                                            alcday > 0.03013699, 7L,  # more than 11x/yr = "1x/month"
                                            alcday > 0.01643836, 8L, # more than 6x/yr = "7-11x/yr
                                            alcday > 0.005479452, 9L, # more than 1-2x/yr = "3-6x/yr"
                                            alcday > 0.0000000001, 10L, 
                                            default = 99L
    )]
    alq[!is.na(alclife) & alclife == 0, alc12freq := 0L]
    # recode drinking frequency from alc12freq
    alq[, alc12freq := car::recode(alc12freq, "0=0;1=1;2=2;3=3;4=4;5=5;6=6;7=7;8=8;9=9;10=10;else=NA")]
    alq[, alc12freq := factor(alc12freq, levels = 0:10, 
                              labels = c("Never in last yr","Everyday","Nearly every day","3-4x/wk","2x/wk","1x/wk","2-3x/month","1x/month","7-11x/yr","3-6x/yr","1-2x/yr"))]
    
    # create an indicator for any drinking in the past year
    alq[!is.na(alcday), anydrnk := as.numeric(alcday > 0)]
    
    # calculate average drinks per drinking day from 'alc12avg'
    alq[!is.na(alc12avg) & alc12avg <= 95, avedrnk := alc12avg]
    # topcode alc12avg to 15 (implemented after 2017, so force consistency with previous years)
    alq[!is.na(alc12avg) & alc12avg > 15, alc12avg := 15]
    
    # set average drinks per drinking day to 0 for respondents who have 0 average drinking days
    alq[!is.na(alcday) & alcday == 0, avedrnk := 0]
    
    # calculate daily average consumption and create an indicator for heavy drinking
    alq[!is.na(alcday) & !is.na(avedrnk) & sex == 1, heavydrnk := as.numeric(alcday * avedrnk > 2)]
    alq[!is.na(alcday) & !is.na(avedrnk) & sex == 2, heavydrnk := as.numeric(alcday * avedrnk > 1)]
    
    # calculate number of binge drinking episodes per 30 days from 'alc5uptp' and 'acl5upno'
    alq[alc5upn > 366, alc5upn := NA]
    alq[!is.na(alc5upn) & alc5upn == 0, bingedrnk := 0]
    alq[!is.na(alc5upu) & alc5upu == 1, bingedrnk := (pmin(alc5upn, 7)*(30/7))]
    alq[!is.na(alc5upu) & alc5upu == 2, bingedrnk := (pmin(alc5upn, 30))]
    alq[!is.na(alc5upu) & alc5upu == 3, bingedrnk := (pmin(alc5upn, 365)/(365/30))]
    alq[!is.na(alc5upu) & alc5upu == 3 & alc5upn == 350, bingedrnk := 30] # b/c days binge drnk topcoded at 350/yr
    alq[!is.na(anydrnk) & anydrnk == 0, bingedrnk := 0]
    
    # create categorical frequency var that matches 2017_2018 categories (note that these may not line up with what respondent might have said b/c of response type bias)
    #   Sometimes a direct comparison b/w bingedrnk (the number) and the category 
    #     don't make sense, but it can be explained by looking at alq[as.numeric(bingedrnk_freq) < as.numeric(alc12freq) &bingedrnk_freq!= "Never in last yr" ]
    alq[!is.na(alc12mn), alc5freq := fcase(bingedrnk == 0, 0L, # never
                                           bingedrnk >= 30, 1L, # everyday (ALQ141Q is topcoded at 350)
                                           bingedrnk > 17.2, 2L, # more than 4x/wk = "nearly every day", >17x/month
                                           bingedrnk > 8.6, 3L, # more than 2x/wk = "3-4x/wk", >8x/month
                                           bingedrnk > 4.3, 4L, # more than 1x/wk = "2x/wk", >4x/month
                                           bingedrnk > 3, 5L, # more than 3x/month = "1x/wk"
                                           bingedrnk > 1, 6L, # more than 1x/month = "2-3x/month"
                                           bingedrnk > 0.92, 7L,  # more than 11x/yr = "1x/month", >.9x/month
                                           bingedrnk > 0.5, 8L, # more than 6x/yr = "7-11x/yr, >0.5x/month
                                           bingedrnk > 0.2, 9L, # more than 2x/yr = "3-6x/yr"
                                           bingedrnk > 0, 10L, # 1-2x/yr
                                           default = 99L
    )]
    
    # Set binge drinking response to "never" for people who have not drank in past yer
    #  Includes those who have had <12 drinks (they were not asked drnking frequency Q)
    #  in lifetime even though it's possible they have binge drank in last year; 
    #  most people who have drank very little in lifetime won't have drinking levels
    #  above TMREL, so it won't bias results and setting as NA loses a lot of info
    alq[!is.na(alclife) & anydrnk == 0, alc5freq := 0]
    # calc binge drinking frequency from "alc5freq"
    alq[, alc5freq := car::recode(alc5freq, "0=0;1=1;2=2;3=3;4=4;5=5;6=6;7=7;8=8;9=9;10=10;else=NA")]
    alq[, alc5freq := factor(alc5freq, levels = 0:10, labels = c("Never in last yr","Everyday","Nearly every day","3-4x/wk","2x/wk","1x/wk","2-3x/month","1x/month","7-11x/yr","3-6x/yr","1-2x/yr"))]
    setnames(alq, "alc5freq", "bingedrnk_freq")
    
    alq <- alq[, list(seqn, anydrnk, heavydrnk, bingedrnk, bingedrnk_freq, alcday, alc12freq, avedrnk)]
  } else if (layout$alc12freq[svy] != ""){ # 2017_2018+
    alq <- loaddata(layout, svy, "alq_file", c("alcever", "alc12avg", "alc12freq", "alc5freq"))
    alq <- merge(alq, demo[, list(seqn, sex)], by="seqn", all.x=T)
    
    # recode alcever
    alq[, alcever := car::recode(alcever, "1=1; 2=0; else=NA")]
    
    # recode drinking frequency from alc12freq
    alq[, alc12freq := car::recode(alc12freq, "0=0;1=1;2=2;3=3;4=4;5=5;6=6;7=7;8=8;9=9;10=10;else=NA")]
    # force consistency with ever drink variable
    alq[alcever == 0, alc12freq := 0]
    alq[, alc12freq := factor(alc12freq, levels = 0:10, labels = c("Never in last yr","Everyday","Nearly every day","3-4x/wk","2x/wk","1x/wk","2-3x/month","1x/month","7-11x/yr","3-6x/yr","1-2x/yr"))]
    
    # create an indicator for any drinking in the past year -- this isn't completely consistent with the indicator in previous years
    alq[!is.na(alc12freq), anydrnk := ifelse(alc12freq == "Never in last yr", 0, 1)]
    
    # calculate average drinks per drinking day from 'alc12avg'
    # alq[!is.na(alc12avg) & alc12avg <= 95, avedrnk := alc12avg] ## DO NOT include for 2017_2018 wave because the distribution of responses is pretty different (I think b/c there are different criteria on who is asked the question in 2017_2018 -- specifically, anyone how has had >= 1 drnk in life, rather than >= 12)
    # force consistency with ever drink variable
    alq[alcever == 0, avedrnk := 0]
    
    # calc binge drinking frequency from "alc5freq"
    alq[, alc5freq := car::recode(alc5freq, "0=0;1=1;2=2;3=3;4=4;5=5;6=6;7=7;8=8;9=9;10=10;else=NA")]
    # force consistency with ever drink variable
    alq[anydrnk == 0, alc5freq := 0]
    alq[, alc5freq := factor(alc5freq, levels = 0:10, labels = c("Never in last yr","Everyday","Nearly every day","3-4x/wk","2x/wk","1x/wk","2-3x/month","1x/month","7-11x/yr","3-6x/yr","1-2x/yr"))]
    setnames(alq, "alc5freq", "bingedrnk_freq")
    
    alq <- alq[, list(seqn, alcever, anydrnk, alc12freq, bingedrnk_freq)]
    
  } else{
    alq <- data.table(seqn=demo$seqn, anydrnk=NA, heavydrnk=NA, bingedrnk=NA, alcday=NA, avedrnk=NA)
  }
  ## Load and format self-reported health & medical visits -------------------------------------------
  # From the HSQ file
  if (layout$hsq_file[svy] != "" & layout$poorhlth[svy] != "") {
    hsq <- loaddata(layout, svy, "hsq_file", c("genhlth", "physhlth", "menthlth", "poorhlth"))
    
    hsq[genhlth > 5, genhlth := NA]
    hsq[, genhlth := factor(genhlth, levels=1:5, labels=c("Excellent", "Very good", "Good", "Fair", "Poor"))]
    hsq[physhlth > 30, physhlth := NA]
    hsq[menthlth > 30, menthlth := NA]
    hsq[poorhlth > 30, poorhlth := NA]
    
  } else if(layout$hsq_file[svy] != "" & layout$genhlth[svy] != ""){ # if there is a file for HSQ, but it doesn't have poorhlth question (implies it only has general health)
    # note: this condition was tweaked to accommodate pre-pandemic data, which has an HSQ file but only asks about blood being tested for HIV (has no other health variables)
    hsq <- loaddata(layout, svy, "hsq_file", c("genhlth"))
    hsq[, genhlth := factor(genhlth, levels=1:5, labels=c("Excellent", "Very good", "Good", "Fair", "Poor"))]
    hsq[, c("physhlth", "menthlth", "poorhlth") := NA]
  } else {
    hsq <- data.table(seqn=demo$seqn, genhlth=NA, physhlth=NA, menthlth=NA, poorhlth=NA)
    hsq[, genhlth := factor(genhlth, levels=1:5, labels=c("Excellent", "Very good", "Good", "Fair", "Poor"))]
  }
  
  # From the HUQ file
  if (layout$medvisits[svy] != "") {  # this variable not available in 2021-2023 survey
    huq <- loaddata(layout, svy, "huq_file", c("genhlth2", "medvisits"))
  } else {
    huq <- loaddata(layout, svy, "huq_file", c("genhlth2"))
  }
  
  huq[genhlth2 > 5, genhlth2 := NA]
  huq[, genhlth2 := factor(genhlth2, levels=1:5, labels=c("Excellent", "Very good", "Good", "Fair", "Poor"))]
  
  if(layout[svy, medvisits] == "huq050"){
    huq[medvisits > 5, medvisits := NA]
    huq[, medvisits := factor(medvisits, levels=0:5, labels=c("None", "1", "2-3", "4-9", "10-12", "13+"))]  
  } else if (layout[svy, medvisits] == "huq051"){
    huq[medvisits > 8, medvisits := NA]
    huq[, medvisits := car::recode(medvisits, "0=0; 1=1; 2=2; c(3,4,5)=3; 6=4; c(7,8)=5; else=NA")] 
    huq[, medvisits := factor(medvisits, levels=0:5, labels=c("None", "1", "2-3", "4-9", "10-12", "13+"))]  
  } else if (layout$medvisits[svy] == "") {
    huq[, medvisits := NA]
  } else {
    print("check medvisits")
  }
  
  ## Load and format diabetes modules ----------------------------------------------------------------
  # self-report
  if(layout[svy, predb] == ""){
    diq <- loaddata(layout, svy, "diq_file", c("diq", "db_insulin", "db_pill"))
  } else if (layout[svy, riskdb] == "") {
    diq <- loaddata(layout, svy, "diq_file", c("diq", "predb", "db_insulin", "db_pill"))
    diq[, predb := car::recode(predb, "1=1; 2=0; else=NA")]
  } else {
    diq <- loaddata(layout, svy, "diq_file", c("diq", "predb", "riskdb", "db_insulin", "db_pill"))
    diq[, predb := car::recode(predb, "1=1; 2=0; else=NA")]
    diq[, riskdb := car::recode(riskdb, "1=1; 2=0; else=NA")]
  }
  diq[, diq := car::recode(diq, "1=1; 2=0; 3=2; else=NA")]
  diq[, db_insulin := car::recode(db_insulin, "1=1; 2=0; else=NA")]
  diq[, db_pill := car::recode(db_pill, "1=1; 2=0; else=NA")]
  
  if (layout$glu_file[svy] != "") {
    # fasting plasma glucose
    glu <- loaddata(layout, svy, "glu_file", c("lbxglu", "mec_fast_wt"))
    if (layout$svy[svy] %in% c("1999_2000", "2001_2002", "2003_2004")) {
      glu[, lbxglu := 0.9815*lbxglu + 3.5707] # convert from Roche Cobras Mira method (used 1999-2004) to Roche/Hitachi 911 method
    }
    if (layout$svy[svy] %in% c("1999_2000", "2001_2002", "2003_2004", "2005_2006")) {
      glu[, lbxglu := lbxglu + 1.148] # convert from Roche/Hitachi 911 method (used 2005-2006) to Roche ModP method (used 2007+)
    }
    if (layout$svy[svy] %in% c("1999_2000", "2001_2002", "2003_2004", "2005_2006", "2007_2008", "2009_2010", "2011_2012", "2013_2014")){
      glu[, lbxglu := 1.023 * lbxglu - 0.5108] # convert from Roche ModP method with RocheC501 (used 2007-2014) to Roche C311 instrument #  Y (C311) = 1.023 (95%CI: 1.014 – 1.032) * X (C501) - 0.5108 (95%CI: -1.441 – 0.4197)
    }
    
    if(layout[svy, mec_fast_wt] == "wtsaf4yr"){
      glu[, mec_fast_wt := mec_fast_wt * 2]
    }
    
  } else {
    glu <- data.table(seqn=demo$seqn, lbxglu=NA, mec_fast_wt=NA)
  }
  
  # A1c
  ghb <- loaddata(layout, svy, "ghb_file", "lbxgh")
  
  ## Load and format health insurance ----------------------------------------------------------------
  hiq <- loaddata(layout, svy, "hiq_file", "hiq010")
  hiq[, hlthplan := car::recode(hiq010, "1=1; 2=0; else=NA")]
  hiq <- hiq[, list(seqn, hlthplan)]
  
  ## Load and format blood pressure and cholesterol modules ------------------------------------------
  # self-reported blood pressure and cholesterol
  if (layout$svyyear[svy] == "2021_2023") {  # only some variables available in 2021-2023 survey; check this in subsequent surveys
    bpq <- loaddata(layout, svy, "bpq_file", c("highbp", "bpmeds_current", "highchol", "cholmeds_current"))
    bpq[, c("bpmeds_rec", "cholcheck", "lastcholcheck") := NA]
  } else {
    bpq <- loaddata(layout, svy, "bpq_file", c("highbp", "bpmeds_rec", "bpmeds_current", "cholcheck", "lastcholcheck", "highchol", "cholmeds_current"))
  }
  bpq[, highbp := car::recode(highbp, "1=1; 2=0; else=NA")]
  bpq[, bpmeds := car::recode(bpmeds_current,"1=1; 2=0; else=NA")]
  bpq[bpmeds_rec == 2, bpmeds := 0] # if someone is not prescribed/told to take meds for hypertension, they are not asked if they're currently taking meds. However, we can assume that they are not currently taking meds in this csae
  bpq[, cholcheck := car::recode(cholcheck, "1=1; 2=0; else=NA")]
  bpq[, lastcholcheck := factor(lastcholcheck, levels=1:4, labels=c("< 1 yr", "1-2 yrs", "2-5 yrs", "5+ yrs"))]
  bpq[, highchol := car::recode(highchol, "1=1; 2=0; else=NA")]
  bpq[, cholmeds := car::recode(cholmeds_current,"1=1; 2=0; else=NA")]
  bpq[(!is.na(lastcholcheck) | highchol == 1) & layout$svvyear[svy] != "2021_2023", cholcheck := 1] # the 2011 survey asks about high cholesterol first and assumes you've been checked if you say yes; likewise, lastcholcheck is NA. 2021-2023 survey has highchol but doesn't ask cholcheck, so don't include that wave in this condition
  bpq[!is.na(cholcheck) & cholcheck == 0, highchol := NA] # in 2011 people are asked if they are told they had high cholesterol before they are asked if they've ever been tested; in previous years they're first asked if they've been tested which acts as a skip. We're going to reinstate that skip here for consistency. 
  bpq[!is.na(highchol) & highchol == 0, cholmeds := NA] # in 2011, people are asked if they are currently taking medicine to lower cholesterol even if they've never been diagnosed, in previous years these people were not asked the question. Force consitstency with pre-2011.
  bpq <- bpq[, list(seqn, highbp, bpmeds, cholcheck, lastcholcheck, highchol, cholmeds)]
  
  # measured blood pressure (auscultatory measurement)
  # this module was retired after the 2017-2018 wave in favor of the bpxo (oscillometric measurement) module
  if (layout$bpx_file[svy] != "") {
    bpx <- loaddata(layout, svy, "bpx_file", c("bps1", "bps2", "bps3", "bps4", "bpd1", "bpd2", "bpd3", "bpd4"))
    avgbp <- function(x, diastolic) { 
      # initialize argument of whether to drop first reading
      drop_first <- T
      # remove missing measurements
      x <- na.omit(x) 
      # for diastolic readings, drop 0s when there is at least one non-zero reading
      if(diastolic & (0 %in% x)){
        if(length(x[x!=0])>0){
          if(x[1]==0){
            # if first reading was a 0, this will be removed here, so first reading does not have to be dropped later
            drop_first=F
          }
          x <- x[x!=0]
        }
      }
      if(length(x)==0){
        # return NA if there are no measurements
        return(as.double(NA)) 
      } else if (length(x)==1){
        # if there is only one measurement, return that
        return(x) 
      } else {
        if(drop_first){
          # if there are multiple measurements, drop the first reading (as long as first reading was not a 0, in which case it would have been removed already)
          x <- x[2:length(x)]
        }
        # return mean of readings
        return(mean(x))
      }
    }
    
    # measured blood pressure
    bpx[, bpsys := avgbp(c(bps1, bps2, bps3, bps4), F), by=1:nrow(bpx)]
    bpx[, bpdias := avgbp(c(bpd1, bpd2, bpd3, bpd4), T), by=1:nrow(bpx)]
    bpx <- bpx[, list(seqn, bpsys, bpdias)]
  } else {
    bpx <- data.table(seqn=demo$seqn, bpsys=NA, bpdias=NA)
  }
  
  # measured blood pressure (oscillometric measurement)
  # present beginning with 2017-2018 wave (including 2017-2020 pre-pandemic wave)
  if (layout$bpxo_file[svy] != "") {
    bpxo <- loaddata(layout, svy, "bpxo_file", c("bpos1", "bpos2", "bpos3", "bpod1", "bpod2", "bpod3"))
    # Diagnostic plots show reasonable distributions using this averaging method, but this may change in future waves and encourage us to seek alternative approaches.
    avgbpo <- function(x, diastolic) { 
      # initialize argument of whether to drop first reading
      drop_first <- T
      # remove missing measurements
      x <- na.omit(x) 
      # for diastolic readings, drop 0s when there is at least one non-zero reading
      if(diastolic & (0 %in% x)){
        if(length(x[x!=0])>0){
          if(x[1]==0){
            # if first reading was a 0, this will be removed here, so first reading does not have to be dropped later
            drop_first=F
          }
          x <- x[x!=0]
        }
      }
      if(length(x)==0){
        # return NA if there are no measurements
        return(as.double(NA)) 
      } else if (length(x)==1){
        # if there is only one measurement, return that
        return(x) 
      } else {
        if(drop_first){
          # if there are multiple measurements, drop the first reading (as long as first reading was not a 0, in which case it would have been removed already)
          x <- x[2:length(x)]
        }
        # return mean of readings
        return(mean(x))
      }
    }
    
    # measured blood pressure
    bpxo[, bposys := avgbpo(c(bpos1, bpos2, bpos3), F), by=1:nrow(bpxo)]
    bpxo[, bpodias := avgbpo(c(bpod1, bpod2, bpod3), T), by=1:nrow(bpxo)]
    bpxo <- bpxo[, list(seqn, bposys, bpodias)]
  } else {
    bpxo <- data.table(seqn=demo$seqn, bposys=NA, bpodias=NA)
  }
  
  # measured total cholesterol
  tchol <- loaddata(layout, svy, "tchol_file", "lbxtc")
  
  # measured LDL and triglyceride cholesterol
  if (layout$lcol_file[svy] != "") {  # 2021-2023 survey does not have an lcol file
    lchol <- loaddata(layout, svy, "lcol_file", c("lbxtr", "lbdldl"))
  } else {
    lchol <- data.table(seqn = demo$seqn, lbxtr = NA, lbdldl = NA)
  }
  
  # measured HDL cholesterol
  hchol <- loaddata(layout, svy, "hcol_file", "lbdhdd")
  chol <- Reduce(function(x,y) merge(x, y, by="seqn", all=T), list(tchol, lchol, hchol))
  
  ## Load and format respiratory/asthma modules -------------------------------------------------------------
  
  if (layout$rdq_file[svy] != "") {
    rdq <- loaddata(layout, svy, "rdq_file", c("wheez_12m"))
    rdq$wheez_12m <- car::recode(rdq$wheez_12m , "1=1; 2=0; else=NA")
  } else {
    rdq <- data.table(seqn = demo$seqn, wheez_12m = NA)
  }
  
  if (layout$mcq_file[svy] != "") {
    mcq <- loaddata(layout, svy, "mcq_file", c("asthma_dx_ever", "asthma_curr"))
    mcq$asthma_dx_ever <- car::recode(mcq$asthma_dx_ever , "1=1; 2=0; else=NA")
    mcq$asthma_curr <- car::recode(mcq$asthma_curr , "1=1; 2=0; else=NA")
  } else {
    mcq <- data.table(seqn = demo$seqn, asthma_dx_ever = NA, asthma_curr = NA)
  }
  
  ## Merge files together ----------------------------------------------------------------------------
  temp <- Reduce(function(x,y) merge(x, y, by="seqn", all=T), list(demo, bmx, whq, smq, alq, hsq, huq, diq, glu, ghb, hiq, bpq, bpx, bpxo, chol, rdq, mcq))
  
  temp$seqn <- NULL
  if (nrow(demo) != nrow(temp)) print(paste("warnings: respondents dropped or added in survey year", svy))
  return(cbind(svyyear=layout$svyyear[svy], temp))
})

## Collapse all data, add NIDs, and save ----------------------------------------------------------------------
data <- lapply(data, labelled::remove_labels) # remove labels imported from the original files
data <- rbindlist(data, use.names=T, fill = T)
data[, svyyear := factor(svyyear, levels=unique(svyyear))]
nids <- fread(paste0(output_dir, "../../nids/nhanes_nids.csv"))
data <- merge(data, nids, by = "svyyear", all.x = T)
setkeyv(data, c("svyyear", "sex", "age", "race", "edu", "marital"))
saveRDS(data, file=paste0(output_dir, "nhanes_microdata.rds"))

## Collect and upload extraction metadata to USHD database -----------------------------------------
# this should be updated with each run!

# collect file paths and map to NIDs
file_cols <- c("svyyear", "dir", grep("file", names(layout), value = T))
nid_file_paths <- merge.data.table(layout[, ..file_cols], nids, by = "svyyear", all.x = T)
stopifnot(all.equal(sort(unique(data$NID)), sort(unique(nid_file_paths$NID))))  # make sure NIDs align in data and metadata
file_cols <- file_cols[!file_cols %in% c("svyyear", "dir")]  # don't need these in the vector for next steps
setnames(nid_file_paths, old = file_cols, new = paste0("file", 1:length(file_cols)))
nid_path_dict <- lapply(1:nrow(nid_file_paths), function(i) {
  paths <- list()
  for (j in 1:length(file_cols)) {
    if (nid_file_paths[i, get(paste0("file", j))] != "") {
      paths <- append(paths, nid_file_paths[i, paste0(dir, "/", get(paste0("file", j)))])
    }
  }
  paths
})
names(nid_path_dict) <- nid_file_paths[, NID]

# collect commit hash info
repo_git_cmd <- paste0("cd ", repo_dir, "; git rev-parse --short HEAD")
commit <- system(repo_git_cmd, intern = TRUE)

# # upload metadata
survey_extraction_version_id <-
  save_survey_extraction(input_type = "nhanes",
                         nid_path_dict = nid_path_dict,
                         output_file_path = output_dir,
                         commit_hash = commit,
                         modeler = Sys.info()[['user']],
                         prev_issues = "none",
                         description = "Added 2021-2023 survey data",
                         discussion = "This extraction is experimental; tbd whether continuing with it included",
                         is_best = F)  # default FALSE to allow vetting before marking best with update_survey_extraction() below

## Generate report and save ------------------------------------------------------------------------

vetting_plots(data, "nhanes", output_dir) # makes diagnostic plots

report <- function(data) {
  d <- lapply(data, function(x) {
    if (class(x) == "character" | is.factor(x)) temp <- function(x) paste("unique values:", length(unique(x)))
    if (class(x) != "character" & !is.factor(x) & length(unique(na.omit(x))) < 10) temp <- function(x) paste("table:", paste(paste(names(table(x)), " (", table(x), ")", sep=""), collapse=", "))
    if (class(x) != "character" & !is.factor(x) & length(unique(na.omit(x))) >= 10) temp <- function(x) paste("range:", round(min(x, na.rm=T), 2), "to", round(max(x, na.rm=T), 2))
    c(temp(x), paste("num missing:", sum(is.na(x))), paste("% missing:", round(100*mean(is.na(x)), 1)))
  })
  d <- do.call("rbind", d)
  d <- cbind(row.names(d), d)
  colnames(d) <- c("Variable", "Summary", "Number_Missing", "Perc_Missing")
  d
}

nhanes_report <- lapply(c(levels(data$svyyear), "all"), function(yr) if (yr == "all") cbind(year="All", report(data)) else cbind(year=yr, report(data[data$svyyear %in% yr,])))
nhanes_report <- do.call("rbind", nhanes_report)
nhanes_report <- nhanes_report[order(nhanes_report[,2], nhanes_report[,1]),]
write.csv(nhanes_report, file=paste0(output_dir, "/nhanes_data_report.csv"), row.names=F)

# update symbolic link so that output dir is "latest"
system(paste0("ln -sfn ", output_dir, " ", output_dir, "../_LATEST"))

## Update extraction as best upon vetting ---------------------------------------------------------
# # uncomment below call to set current extraction as best
# update_survey_extraction(best_status = T,
#                          survey_extraction_version_id = survey_extraction_version_id,
#                          prev_issues = "new survey data (2021-2023) extracted")
