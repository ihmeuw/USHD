####################################################################################################
## Description: Compile all BRFSS microdata and recode variables for consistency across years
##
## Output:  'brfss_microdata.rdata' -- contains a data.table named 'data' with one row per
##              respondent,identifying and demographic variables, sample weights, and selected other
##              self-report variables, all consistently defined across years.
##          'brfss_data_report.csv' -- a report on the range and missingness for each variable, by
##              year and overall.
##
## NOTE: Run from within the risk_factors/1_data_prep/CBSAs/survey_data folder in the GIT repo.
##       Also, always rerun 'gen_brfss_cnty_rake_wts.r' after rerunning this scripts.
####################################################################################################
setwd('FILEPATH')
library(data.table)
library(foreign)
library(car)
library("readstata13")

rm(list=ls())
root <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
main_dir <- paste0(root, "FILEPATH")

## Read in variable layout and loop over surveys ---------------------------------------------------
layout <- suppressWarnings(fread("brfss_files_and_variables.csv", na.strings=""))
layout <- layout[!(year %in% c(1990:2001, 2018)),] 
vars <- names(layout)[!names(layout) %in% c("file", "landline_file", "year", "scope", "supplement")]

met_conversion <- fread("physical_activity_met.csv")

brfss_data <- lapply(1:nrow(layout), function(svy) {
  cat(paste0(svy, " of ", nrow(layout), ": ", layout[svy, file], "\n")); flush.console()
## Load files and extract and standardize all variables --------------------------------------------
# load files; for 2011+ merge on landline-only file to get landline-only weights
  data <- data.table(read.dta13(layout[svy, file], convert.factors=F))
  setnames(data, tolower(names(data)))
  if (layout[svy, year] > 2010 & layout[svy, year] < 2014 & layout[svy, scope] == 0) {
    landline_only <- data.table(read.dta(layout[svy, landline_file]))
    setnames(landline_only, tolower(names(landline_only)))
    landline_only <- landline_only[, as.character(layout[svy, list(seqno, ll_wt, state)]), with=F]
    data <- merge(data, landline_only, by=as.character(layout[svy, list(seqno, state)]), all=T)
    rm(landline_only); gc()
  }

# create indicator for landline and cell samples
  if (layout[svy, year] <= 2010 ) { 
    data[, sample := "landline"]
  } else if (layout[svy, year] > 2011) {
    data[qstver >= 10 & qstver < 20, sample := "landline"]
    data[qstver >= 20 & qstver < 30, sample := "cellphone"]
  } else if (layout[svy, supplement] == 0) {
    data[qstver >= 10 & qstver < 20, sample := "landline"]
    data[qstver >= 20 & qstver < 30, sample := "cellphone"]
  } 
  
  # In an MSA county yes/no
  if (!is.na(layout[svy, msa])) {data[, mscode := recode(mscode, "c(1,2,3,4)=1; 5=0")]}
 
# keep relevant variables and standardize names
  included_vars <- vars[!is.na(layout[svy, vars, with=F])]
  data <- data[, c("sample", as.character(layout[svy, included_vars, with=F])), with=F]
  setnames(data, c("sample", included_vars))
  data[, c("year", "scope", "supplement") := layout[svy, list(year, scope, supplement)]]

# drop lines that aren't real respondents (something went wrong when merging county codes for IN/MI/OR in 2006 and OR/VT in 2007)
  if (layout[svy, year] == 2006) data <- data[!(is.na(age) & is.na(sex) & is.na(race) & state %in% c(18, 26, 41)),]
  if (layout[svy, year] == 2007) data <- data[!(is.na(age) & is.na(sex) & is.na(race) & state %in% c(41, 50)),]

# prep interview month and day
  data[, c("iday", "imonth", "iyear") := list(as.numeric(iday), as.numeric(imonth), as.numeric(iyear))]
  if (max(data$iyear) %in% 90:99) data[, iyear := 1900 + iyear]
  if (min(data$iyear) == 1) data[, iyear := layout[svy, year] + (iyear - 1)]

  data[, day := weekdays(as.Date(paste(iyear, imonth, iday, sep="-"), "%Y-%m-%d"))]
  setnames(data, "imonth", "month")
  data[, c("iyear", "iday") := NULL]

# prep age and sex
  if (layout[svy, year] < 2013) {data[, age := cut(age, c(0,18,25,35,45,55,65,100), labels=F, include.lowest=T, right=F) - 1]}#age is only continuous for years <2014
  data[!sex %in% 1:2, sex := NA]

# standardize race
  if (layout[svy, year] < 2011) { # 1990-2000 ('RACE' variable): 1=white non-hispanic; 2=black non-hispanic; 3=white hispanic; 4=black hispanic; 5=other hispanic; 6=asian/pacific islander; 7=AIAN; 8=other; 99=DK
    data[, race := recode(race, "1=0; 2=1; 7=2; 6=3; c(3,4,5)=4; else=NA")]
  } else if (layout[svy, year] %in% 2001:2012) { # 2001-2012 ('RACE2' variable): 1=white only, non-hispanic; 2=black only, non-hispanic; 3=asian only, non-hispanic; 4=native HI or pacific islander, non-hispanic; 5=AIAN only, non-hispanic; 6=other only, non-hispanic; 7=multiracial, non-hispanic; 8=hispanic; 9=DK/NS/R
    data[, race := recode(race, "1=0; 2=1; 5=2; c(3,4)=3; 8=4; else=NA")]
  } else if (layout[svy, year] %in% 2013:2017) { # 2013-2017 ('_race' variable): 1=white only, non-hispanic; 2=black only, non-hispanic; 3=AIAN only, non-hispanic; 4=asian only, non-hispanic; 5=native HI or pacific islander, non-hispanic; 6=other only, non-hispanic; 7=multiracial, non-hispanic; 8=hispanic; 9=DK/NS/R
    data[, race := recode(race, "1=0; 2=1; 3=2; c(4,5)=3; 8=4; else=NA")]
  }
  data[, race := factor(race, levels=0:4, labels=c("NH white", "NH black", "NH AIAN", "NH API", "Hispanic"))]

# standardize marital status
  data[, marital := recode(marital, "c(2,3,4)=0; c(1,6)=1; 5=2; else=NA")] # all years: 1=married; 2=divorced; 3=widowed; 4=separated; 5=never married; 6=a member of an unmarried couple; 9=refused
  data[, marital := factor(marital, levels=0:2, labels=c("former", "current", "never"))]
 
# standardize education
  if (layout[svy, year] <= 1992) { # 1990-1992: 1=eight grade or less; 2=some HS; 3=HS/GED; 4=Some technical school; 5=Technical school graduate; 6=Some college; 7=College graduate; 8=Post-grad/professional degree; 9=Refused
    data[, edu := recode(edu, "c(1,2)=0; 3=1; c(4,5,6)=2; c(7,8)=3; else=NA")]

  } else if (layout[svy, year] >= 1993) { #1993-2012: 1=none/kindergarten; 2=grades 1-8 (Elementary); 3= grades 9-11 (some HS); 4=grade 12/GED (HS grad); 5=college 1-3 yrs (some college/technical school); 6=College 4yrs+ (College grad); 9=Refused. In alaska, group 6 has been recoded as 8 from 1993 to 1997
    data[, edu := recode(edu, "c(1,2,3)=0; 4=1; 5=2; c(6,8)=3; else=NA")]
  }
  data[, edu := factor(edu, levels=0:3, labels=c("less than HS", "HS grad", "some college", "college grad"))]

# standardize phone
  # phoneown_c (in cellphone survey 2011+): do you also have a landline? 1: yes, 2: no
  # phoneuse_c (in cellphone survey 2011-2013): % calls received on cellphone
  # phoneown_ll	(in landline survey 2011+): do you have a cellphone for personal use? 1: yes, 2: no
  # phoneuse_ll (in cellphone survey 2009-2013): % calls received on cellphone
  if (is.na(layout[svy, phoneown_ll])) {# years prior to 2009 - landline survey only (all have landline - no info on owning cellphone)
    data[, ownll := 1]# has landline yes
    
  } else if (!is.na(layout[svy, phoneown_ll]) & is.na(layout[svy, phoneown_c])) {# years 2009-2010 - landline survey only (all have landline AND info on owning cellphone)
    data[, ownll := 1]# has landline yes
    data[phoneown_ll == 1, owncp := 1]# owns cellphone yes
    data[phoneown_ll == 2, owncp := 0]# owns cellphone no

    data[owncp == 1, phone := "dual_phone"]# owns cellphone AND landline (landline is default since this is landline survey only)
    data[owncp == 0, phone := "landline_only"]# owns landline only 
    data[, phone := factor(phone, levels=c("dual_phone", "landline_only", "cell_only"))]

  } else if (!is.na(layout[svy, phoneown_ll]) & !is.na(layout[svy, phoneown_c])) {#years 2011+ (combined data from landline and cell phone surveys)
    data[sample == "landline" | (sample == "cellphone" & phoneown_c == 1), ownll := 1]# landline survey OR owns landline from cell phone survey
    data[sample == "cellphone" & phoneown_c == 2, ownll := 0]# cell phone survey and does not own landline
    data[sample == "cellphone" | (sample == "landline" & phoneown_ll == 1), owncp := 1]# cellphone survey OR owns cell phone from landline survey
    data[sample == "landline" & phoneown_ll == 2, owncp := 0]# landline survey and does not own cell phone

    data[ownll == 1 & owncp == 1, phone := "dual_phone"]
    data[ownll == 1 & owncp == 0, phone := "landline_only"]
    data[ownll == 0 & owncp == 1, phone := "cell_only"]
    data[, phone := factor(phone, levels=c("dual_phone", "landline_only", "cell_only"))]
  }

# standardize tenure
  if (!is.na(layout[svy, tenure]) & layout[svy, year] %in% 1996:1999) { # 1996-1999: 1 = Own, 2 = Rent
    data[, tenure := recode(tenure, "2=0; 1=1; else=NA")]

  } else if (!is.na(layout[svy, tenure]) & layout[svy, year] >= 2009) { # 2009-2013: 1 = Own, 2 = Rent, 3 = Other
    data[, tenure := recode(tenure, "c(2,3)=0; 1=1; else=NA")]
  }

# prep pregnancy
  data[, preg := recode(preg, "2=0; 1=1; else=NA")] # all years: 1 = yes; 2 = no

# standardize height in m
  if (layout[svy, height] == "height") { # 1990-2001 (height): reported height in feet and inches
    data[height == 777 | height == 999, height := NA]
    data[, height := (2.54 * (floor(height/100)*12 + (height - 100*floor(height/100))))/100]

  } else if (layout[svy, height] %in% c("htm", "htm2", "htm3", "htm4", "xhtm")) { # 2000-2012 (HTM/HTM2/HTM3/HTM4): computed height in meters (two implied decimal points)
    data[height > 998, height := NA]
    data[, height := height/100]
  }

# standardize weight in kg
  if (layout[svy, weight] == "weight") { # 1990-2003 (weight): reported weight in pounds
    data[weight == 777 | weight == 999, weight := NA]
    data[, weight := weight/2.2]

  } else if (layout[svy, weight] %in% c("wtkg2", "wtkg3", "xwtkg")) { # 2004-2012 (wtkg2/wtkg3): calculated weight in kg (two implied decimal points)
    data[weight == 99999 | weight < 0, weight := NA]
    data[, weight := weight/100]
  }



# standardize smoking status
  if (layout[svy, smoker] %in% c("_smoker", "a_smoker") & layout[svy, year] %in% 1990:1993) { # smoker (1990-1993): 1=current smoker; 2=former smoker; 3=never smoked; 4=irregular smoker; 9=refused
#FIXME consider adding if years expanded back - but check if 'current' means 'current, daily', first
    # data[, smoke := recode(smoker, "1=1; 2=3; 3=4; 4=2; else=NA")]
    # data[, smoke := factor(smoke, levels=1:4, labels=c("current, daily", "current, sometimes", "former", "never"))]
    data[, smoke_any := recode(smoker, "c(2,3)=0; c(1,4)=1; else=NA")]
    data[, c("smoke100", "smokenow", "smokenum", "smoker") := NULL]

  } else if (layout[svy, smoker] %in% c("_smoker2", "a_smoker2") & layout[svy, year] %in% 1994:1995) { # smoker2 (1994-1995): 1=current smoker, 30 days; 2=current smoker, 1-29 days; 3=current smoker, 0 days; 4=current smoker, unknown days; 5=former smoker; 6=never smoked
    data[, smoke := recode(smoker, "1=1; c(2,3,4)=2; 5=3; 6=4; else=NA")]
    data[, smoke := factor(smoke, levels=1:4, labels=c("current, daily", "current, sometimes", "former", "never"))]
    data[!is.na(smoke), smoke_any := grepl("current", smoke)]
    data[, c("smoke100", "smokenow", "smoke30", "smoker") := NULL]

  } else if (layout[svy, smoker] %in% c("_smoker2", "a_smoker2", "_smoker3", "a_smoker3", "asmoker3", "x_smoker3", "smoker3x") & layout[svy, year] >= 1996) { # smoker2/smoker3 (1996-2012): 1=current smoker, everyday; 2=current smoker some days; 3=former smoker; 4=never smoked
    data[smoker <= 4, smoke := smoker]
    data[, smoke := factor(smoke, levels=1:4, labels=c("current, daily", "current, sometimes", "former", "never"))]
    data[!is.na(smoke), smoke_any := grepl("current", smoke)]
    data[, c("smoke100", "smokeday", "smoker") := NULL]
  }

# prep age first smoked
  if (!is.na(layout[svy, firstsmoke])) {
    data[firstsmoke >= 77, firstsmoke := NA]
    if ("smoke" %in% names(data)) data[as.numeric(smoke) == 4, firstsmoke := NA]
  }

# standardize time since last smoked
  if (!is.na(layout[svy, lastsmoke]) & layout[svy, year] == 1990) { # LASTSMOK (1990): 1= < 1 month, 2= 1-3 months, 3= 3-6 months, 4= 6-12 months, 5=1+ years
    data[, lastsmoke := recode(lastsmoke, "c(1,2,3,4)=1; 5=2; else=NA")]

  } else if (!is.na(layout[svy, lastsmoke]) & layout[svy, year] %in% 1991:1993) { # LASTSMOK (1991-1993): 1= < 1 month, 2= 1-3 months, 3= 3-6 months, 4= 6=12 months, 5= 1-5 years, 6= 5+ years, 8= never smoked regularly
    data[, lastsmoke := recode(lastsmoke, "c(1,2,3,4)=1; c(5,6)=2; c(8,88)=3; else=NA")]

  } else if (!is.na(layout[svy, lastsmoke]) & layout[svy, year] %in% 1994:2000) { # LASTSMOK (1994-2000): 1= < 1 month, 2= 1-3 months, 3= 3-6 months, 4= 6-12 months, 5= 1-5 years, 6= 5-15 years, 7= 15+ years, 88= never smoked regularly
    data[, lastsmoke := recode(lastsmoke, "c(1,2,3,4)=1; c(5,6,7)=2; c(8,88)=3; else=NA")]

  } else if (!is.na(layout[svy, lastsmoke]) & layout[svy, year] %in% 2001:2003) { # LASTSMK (2001-2003): 1 = < 1 month, 2 = 1-3 months, 3= 3-6 months, 4= 6-12 months, 5 = 1-5 years, 6 = 5-10 years, 7 = 10+ years,
    data[, lastsmoke := recode(lastsmoke, "c(1,2,3,4)=1; c(5,6,7)=2; else=NA")]

  } else if (!is.na(layout[svy, lastsmoke]) & layout[svy, year] >= 2009) { # LASTSMK1/LASTSMK2 (2009-2013): 1= < 1 month, 2= 1-3 months, 3= 3-6 months; 4= 6-12 months; 5=1-5 years; 6= 5-10 years; 7= 10+ years; 88= never smoked regularly
    data[, lastsmoke := recode(lastsmoke, "c(1,2,3,4)=1; c(5,6,7)=2; c(8,88)=3; else=NA")]
  }

  if (!is.na(layout[svy, lastsmoke])) {
    data[, lastsmoke := factor(lastsmoke, levels=1:3, labels=c("< 1 yr", "> 1 yr", "never regularly smoked"))]
  }
  return(data)
}) 


## Combine files and format geography variables ----------------------------------------------------
# combine files
databrfss <- rbindlist(brfss_data, use.names=T, fill=T)

save(databrfss, file="FILEPATH")
load("FILEPATH")
databrfss<-databrfss[age>0,]
databrfss[, age := age -1]
table(databrfss$age)

# map to merged counties
databrfss[!is.na(cnty), cnty := 1000*state + cnty]

load(paste0(root, "FILEPATH"))
databrfss <- merge(databrfss, loc[, list(cnty, mcnty)], by="cnty", all.x=T)

# data cleaning
subset<-databrfss[state<57,]
sum(table(table(subset[is.na(mcnty) & !is.na(cnty) & year %in% 2005:2017, c(cnty)])))
subset<-subset[!(is.na(mcnty) & !is.na(cnty)) & year %in% 2005:2017, ]#drop counties with mismatch between cnty and mcnty (most have cnty as 777,888 or 999)

## Format and save ---------------------------------------------------------------------------------
databrfss <- subset[, list(year, seqno, sample, wt, ll_wt, msacode, msa, 
                              design_wt, state, mcnty, cnty, month, day,
                              age, sex, race, marital, edu, ownll, owncp, phone, tenure, preg,
                              height, weight,
                              smoke, smoke_any, firstsmoke, lastsmoke
)]

setnames(databrfss, 'msa', 'msa.yn')
setnames(databrfss, 'msacode', 'cbsa')

#add state abbreviation
statefips<-data.table(read.csv('FILEPATH'))
setnames(statefips, 'STATE', 'state.name')
setnames(statefips, 'STATEA', 'state.fips')
setnames(statefips, 'STUSAB', 'state.abb')

databrfss <- merge(databrfss, statefips[,c('state.name', 'state.abb', 'state.fips')], by.x='state', by.y='state.fips')

setkeyv(databrfss, c("year", "sample", "seqno", "state", "sex", "age", "race", "edu", "marital"))
databrfss[, id := 1:.N]
save(databrfss, file=paste0(main_dir, "survey_data/brfss_microdata.rdata"))

## Generate report ---------------------------------------------------------------------------------
datareport <- databrfss[,-c('state.abb','state.name')]#dropped factor variables with more than 10 classes to avoid error in function below (see 3rd if statement)

report <- function(datareport) {
  d <- lapply(datareport, function(x) {
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

brfss_report <- lapply(c(sort(unique(datareport$year)), 9999), function(yr) if (yr == 9999) cbind(year="All", report(datareport)) 
                       else cbind(year=yr, report(datareport[datareport$year %in% yr,])))
brfss_report <- do.call("rbind", brfss_report)
brfss_report <- brfss_report[order(brfss_report[,2], brfss_report[,1]),]
write.csv(brfss_report, file=paste0(main_dir, "survey_data/brfss_data_report.csv"), row.names=F)

















## OLD unused code for vars other than socio-demographics and smoking ---------------------------------------------------------------------------------
#FIXME add remaining code back
# prep any PA
  if (!is.na(layout[svy, exerany])) { # all years: 1=yes; 2=no
    data[, exerany := recode(exerany, "1=0; 2=1; else=NA")]
    setnames(data, "exerany", "pa_none")
  }

# standardize sufficient PA
  if (!is.na(layout[svy, recpa]) & !grepl("a_pacat", layout[svy, recpa])) { # 2001, 2003, 2005, 2007, 2009: derive PA status to match 2011 definition (mod=moderate; vig=vigorous; *pact = participates (1=yes, 2=no); *paday = number of days; *patim = HMM each day)
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
    data[, c("modpact", "modpaday", "modpatim", "mod", "vigpact", "vigpaday", "vigpatim", "vig", "recpa") := NULL]

  } else if (!is.na(layout[svy, exract01])) { # 1995-2000 & 2011+: calculate derived variable according to 2011 codebook
    # convert HH:MM to minutes of each activity
    data[, padur1 := ifelse(exerhmm1 %in% c(777, 888, 999), NA, (exerhmm1 %/% 100)*60 + (exerhmm1 %% 100))]
    data[, padur2 := ifelse(exerhmm2 %in% c(777, 888, 999), NA, (exerhmm2 %/% 100)*60 + (exerhmm2 %% 100))]
    # convert unit(week or month) and times, to times per week of each activity
    data[, pafreq1 := round(ifelse(exeroft1 >= 101 & exeroft1 <= 199, exeroft1 - 100, ifelse(exeroft1 >= 201 & exeroft1 <= 299, (exeroft1 - 200)/(30/7), NA)), digits=3)]
    data[, pafreq2 := round(ifelse(exeroft2 >= 101 & exeroft2 <= 199, exeroft2 - 100, ifelse(exeroft2 >= 201 & exeroft2 <= 299, (exeroft2 - 200)/(30/7), NA)), digits=3)]

    # keep met activity codes from either 2011 survey list or 1995-2000 list
    if (!is.na(layout[svy, recpa]) & grepl("a_pacat", layout[svy, recpa])) { # 2011 activity codes
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
# FIXME in 2011: not found: actin1_, actin2_
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
data[, recpa := recode(pacat, "c(1,2)=0; c(3,4)=1; else=NA")]
setnames(data, "recpa", "pa_insufficient")
setnames(data, "pamin", "pa_min")
data[pa_none == 1, pa_insufficient := 1] # force consistency between any and sufficient activity -- this is consistent with 2011 where the any question is used as a screen
data[, c("exract02", "exract01", "exeroft1", "exerhmm1", "exeroft2", "exerhmm2", "padur1", "padur2", "pafreq1", "pafreq2", "metval1", "metval2", "maxvo2", "fc60",
         "minact1", "minact2", "pamin1", "pamin2", "pavigm1", "pavigm2", "pavigm", "pacat") := NULL]
}
# prep self-reported health and emotional support
if(!is.na(layout[svy, genhlth])) {
  data[, genhlth := factor(genhlth, levels=1:5, labels=c("Excellent", "Very good", "Good", "Fair", "Poor"))]
  data[, physhlth := recode(physhlth, "88=0; 31:hi=NA")]
  data[, menthlth := recode(menthlth, "88=0; 31:hi=NA")]
  data[, poorhlth := recode(poorhlth, "88=0; 31:hi=NA")]
  data[physhlth == 0 & menthlth == 0, poorhlth := 0] # from 1994 onwards there's a skip pattern that
  # assumes poorhlth is 0 if physhlth and menthlth are 0, so we need to fill that in. For 1993
  # this skip pattern doesn't exist, but we'll make the same assumption for consistency
}

if(!is.na(layout[svy, emtsuprt])) {
  data[, emtsuprt := factor(emtsuprt, levels=1:5, labels=c("Always", "Usually", "Sometimes", "Rarely", "Never"))]
}

# prep any, heavy, and binge drinking
  if (!is.na(layout[svy, drnkany])) {
    # recode drnkany
    data[, drnkany := recode(drnkany, "2=0; 1=1; else=NA")]
    setnames(data, "drnkany", "anydrnk")

    # correct anydrnk based on response to alcday
    data[alcday == 888, anydrnk := 0]

    # convert alcday to units = 'per day'
    data[, alcday := as.double(alcday)]
    data[alcday > 230 & alcday < 300, alcday := 230]
    data[alcday >= 100 & alcday < 200, alcday := (alcday - 100) / 7]
    data[alcday >= 200 & alcday < 300, alcday := (alcday - 200) / 30]
    data[alcday == 888, alcday := 0]
    data[alcday > 1, alcday := NA]

    # set alcday to 0 if anydrnk is 0
    data[anydrnk == 0, alcday := 0]

    # set anydrnk to 1 if alcday is greater than 0 (this only kicks in when anydrnk is missing and alcday isn't)
    data[alcday > 0, anydrnk := 1]

    # recode avedrnk and set to zero if anydrnk is 0
    data[avedrnk >= 77, avedrnk := NA]
    data[anydrnk == 0, avedrnk := 0]

    # calculate average daily consumption and derive the indicator for heavy drinking
    data[, csmp := alcday * avedrnk]
    data[!is.na(csmp), heavydrnk := as.numeric((sex == 1 & csmp > 2) | (sex == 2 & csmp > 1))]
    data[, csmp := NULL]

    # recode drnkeg5
    data[, drnkge5 := recode(drnkge5, "88=0; 77:hi=NA")]
    data[is.na(drnkge5) & anydrnk == 0, drnkge5 := 0]
    setnames(data, "drnkge5", "bingedrnk")
  }

# standardize max drinks and number of drinks in last binge session
  if (!is.na(layout[svy, maxdrnks])) {
    data[maxdrnks >= 77, maxdrnks := NA]
    data[anydrnk == 0, maxdrnks := 0]
  }
#FIXME Error in drnkbeer + drnkwine : non-numeric argument to binary operator
  # if (!is.na(layout[svy, drnkbeer])) {
  #   data[, drnkbeer := recode(drnkbeer, "88=0; 77:hi=NA")]
  #   data[, drnkwine := recode(drnkwine, "88=0; 77:hi=NA")]
  #   data[, drnkliqr := recode(drnkliqr, "88=0; 77:hi=NA")]

  #   if(layout[svy, year] == 2008) {
  #     data[, drnkpmix := recode(drnkpmix, "88=0; 77:hi=NA")]
  #     data[, numdrnks := drnkbeer + drnkwine + drnkliqr + drnkpmix]
  #     data[, c("drnkbeer", "drnkwine", "drnkliqr", "drnkpmix") := NULL]
  # 
  #   } else {
  #     data[, numdrnks := drnkbeer + drnkwine + drnkliqr]
  #     data[, c("drnkbeer", "drnkwine", "drnkliqr") := NULL]
  #   }
  # }

# prep cholesterol
  if (!is.na(layout[svy, cholchk]) & !is.na(layout[svy, bloodcho]) & !is.na(layout[svy, highchol])) {
    data[, bloodcho := recode(bloodcho, "2=0; 1=1; else=NA")]
    setnames(data, "bloodcho", "cholcheck")
    data[, cholchk := factor(cholchk, levels=1:4, labels=c("< 1 yr", "1-2 yrs", "2-5 yrs", "5+ yrs"))]
    setnames(data, "cholchk", "lastcholcheck")
    data[, highchol := recode(highchol, "2=0; 1=1; else=NA")]
  }

# standardize diabetes
  if (layout[svy, year] %in% 1990:1993 & !is.na(layout[svy, diabetes])) { # 1990-1993: 1 = yes; 2 = no; 7 = DK/NS; 9 = Refused. In alaska, no is coded as 3 instead of 2 for some reason
    data[, diabetes := recode(diabetes, "c(2,3)=0; 1=1; else=NA")]

  } else if (layout[svy, year] %in% 1994:2003 & !is.na(layout[svy, diabetes])) { # 1994-2003: 1 = yes; 2 = yes, but female told only during pregnancy; 3 = no; 7 = DK/NS; 9 = Refused
    data[, diabetes := recode(diabetes, "c(2,3)=0; 1=1; else=NA")]

  } else if (layout[svy, year] >= 2004 & !is.na(layout[svy, diabetes])) { # 2004-2013: 1 = yes; 2 = yes, but female told only during pregnancy; 3 = no; 4 = no, pre-diabetes or borderline diabetes; 7 = DK/NS; 9 = Refused
    data[, diabetes := recode(diabetes, "c(2,3,4)=0; 1=1; else=NA")]
  }

# standardize hypertension and hypertension medication
  if (layout[svy, year] %in% 1990:1992 & !is.na(layout[svy, highbp])) { # 1990-1992: 1 = no; 2 = yes, by a doctor; 3 = yes, by a nurse; 4 = yes, by other health profesional; 7 = DK/NS; 9 = refused
    data[, highbp := recode(highbp, "1=0; c(2,3,4)=1; else=NA")]

  } else if (layout[svy, year] %in% 1993:2001 & !is.na(layout[svy, highbp])) { # 1993-2001: 1 = yes; 2 = no; 7 = DK/NS; 9 = refused
    data[, highbp := recode(highbp, "2=0; 1=1; else=NA")]

  } else if (layout[svy, year] %in% 2002:2004 & !is.na(layout[svy, highbp])) { # 2002-2004: 1 = yes; 2 = yes, but female told only during pregnancy; 3 = no; 7 = DK/NS; 9 = refused
    data[, highbp := recode(highbp, "c(2,3)=0; 1=1; else=NA")]

  } else if (layout[svy, year] >= 2005 & !is.na(layout[svy, highbp])) { # 2005-2013: 1 = yes; 2 = yes, but female told only during pregnancy; 3 = no; 4 = told borderline high or pre-hypertensive; 7 = DK/NS; 9 = refused
    data[, highbp := recode(highbp, "c(2,3,4)=0; 1=1; else=NA")]
  }

  if (!is.na(layout[svy, bpmeds])) { # all years: 1 = yes; 2 = no; 7 = DK/NS; 9 = refused
    data[, bpmeds := recode(bpmeds, "2=0; 1=1; else=NA")]
    data[highbp == 0, bpmeds := NA]
  }

# prep health insurance
  if(!is.na(layout[svy, hlthplan])) { # all years: 1 = yes; 2 = no; 7 = DK/NS; 9 = refused
    data[, hlthplan := recode(hlthplan, "2=0; 1=1; else=NA")]
  }

# prep seatbelt
  if(!is.na(layout[svy, seatbelt])) { # 1990-1997, 2002, 2006, 2008, 2010, 2011, 2012: 1 = Always; 2 = Nearly always; 3 = Soemtimes; 4 = Seldom; 5 = Never; 7 = Don't know/Not sure; 8 = Never drive or ride in a car; 9 = Refused
    data[, seatbelt := recode(seatbelt, "c(2,3,4,5)=0; 1=1; else=NA")]
  }

# prep alcohol impaired driving
  if (!is.na(layout[svy, drnkdri])) { # 1-76 = Number of times; 88 = None; 77 = Don't know/Not sure; 99 = Refused
  # episodes of alcohol impaired (AIDE) driving (within past 30 days)
    data[, aide := recode(drnkdri, "88=0; c(77,99)=NA")]
  # binary indicator for committing AIDE (within past 30 days)s
    data[, drnkdri := recode(drnkdri, "1:76=1; 88=0; else=NA")]
  # fix data entry errors where anydrnk=1 & drnkdri>0
  	data[anydrnk == 0, recode(drnkdri, "1:76=0")]
  }

# prep falls
  if(!is.na(layout[svy, falls])) { # (2014,2012):"Past 12 months", (2010,2008,2006):"Past 3 months". 1-76, 88=None, 77=DK/NS, 99=Refused
    data[, falls := recode(falls, "88=0; c(77,99)=NA")]
  }

# prep employment and out of work
  if(!is.na(layout[svy, employ])) { # 1 = Employed for wages; 2 = Self-employed; 3 = Out of work for more than 1 year; 4 = Out of work for less than 1 year; 5 = A homemaker; 6 = A student; 7 = Retired; 8 = Unable to work; 9 = Refused
    data[, outwork := recode(employ, "c(3,4)=1; c(1,2,5,6,7,8)=0; else=NA")]
    data[, outwork1 := recode(employ, "c(1,2,4,5,6,7,8)=0; 3=1; else=NA")]
    data[, outwork2 := recode(employ, "c(1,2,3,5,6,7,8)=0; 4=1; else=NA")]
    data[, employ := recode(employ, "1:2=1; 3:8=0; else=NA")]
  }
  return(data)
#FIXME })

