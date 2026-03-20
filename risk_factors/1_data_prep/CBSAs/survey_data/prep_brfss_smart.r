####################################################################################################
## Description: Compile all BRFSS microdata and recode variables for consistency across years
##
## Output:  'brfss_microdata.rdata' -- contains a data.table named 'data' with one row per
##              respondent,identifying and demographic variables, sample weights, and selected other
##              self-report variables, all consistently defined across years.
##          'brfss_data_report.csv' -- a report on the range and missingness for each variable, by
##              year and overall.
##
## NOTE: Run from within the risk_factors/_data_prep/counties/survey_data folder in the GIT repo.
##       Also, always rerun 'gen_brfss_cnty_rake_wts.r' after rerunning this scripts.
####################################################################################################
#removed CDPDE/phone vars (only in BRFSS)
library(data.table)
library(foreign)
library(car)
library(haven)
library(devtools)
library(labelled)#only load after running lapply (causes errors in car's recode through required dplyr)

rm(list=ls())
root <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
main_dir <- paste0(root, "FILEPATH")


## Read in variable layout and loop over surveys ---------------------------------------------------
layout <- suppressWarnings(fread("smart_brfss_files_and_variables.csv", na.strings=""))

vars <- names(layout)[!names(layout) %in% c("smart_file", "year")]

met_conversion <- fread("physical_activity_met.csv")


all_datasm <- lapply(1:nrow(layout), function(svy) {
  cat(paste0(svy, " of ", nrow(layout), ": ", layout[svy, smart_file], "\n")); flush.console()
  
  ## Load files and extract and standardize all variables --------------------------------------------

    # load files; for 2011+ merge on landline-only file to get landline-only weights
  data <- data.table(read_xpt(layout[svy, smart_file]))
  setnames(data, tolower(names(data)))

  # keep relevant variables and standardize names
  included_vars <- vars[!is.na(layout[svy, vars, with=F])]
  data <- data[, c("sample", as.character(layout[svy, included_vars, with=F])), with=F]
  setnames(data, c("sample", included_vars))
  data[, c("year") := layout[svy, list(year)]]

  # prep interview month and day
  if (!is.na(layout[svy, iday]) & !is.na(layout[svy, imonth]) & !is.na(layout[svy, iyear])){
  data[, c("iday", "imonth", "iyear") := list(as.numeric(iday), as.numeric(imonth), as.numeric(iyear))]
  if (max(data$iyear) %in% 90:99) data[, iyear := 1900 + iyear]
  if (min(data$iyear) == 1) data[, iyear := layout[svy, year] + (iyear - 1)]
  data[, day := weekdays(as.Date(paste(iyear, imonth, iday, sep="-"), "%Y-%m-%d"))]
  setnames(data, "imonth", "month")
  data[, c("iyear", "iday") := NULL]}
  
  # prep sex
  data[!sex %in% 1:2, sex := NA]
  if (layout[svy, year] < 2013) {data[, age := cut(age, c(0,18,25,35,45,55,65,100), labels=F, include.lowest=T, right=F) - 1]}#age is only continuous for years <2014
  

  # standardize race
  if (layout[svy, year] %in% 2002:2012) { # 2001-2012 ('RACE2' variable): 1=white only, non-hispanic; 2=black only, non-hispanic; 3=asian only, non-hispanic; 4=native HI or pacific islander, non-hispanic; 5=AIAN only, non-hispanic; 6=other only, non-hispanic; 7=multiracial, non-hispanic; 8=hispanic; 9=DK/NS/R
    data[, race := recode(race, "1=0; 2=1; 5=2; c(3,4)=3; 8=4; else=NA")]
  } else if (layout[svy, year] %in% 2013:2017) { # 2013-2017 ('_race' variable): 1=white only, non-hispanic; 2=black only, non-hispanic; 3=AIAN only, non-hispanic; 4=asian only, non-hispanic; 5=native HI or pacific islander, non-hispanic; 6=other only, non-hispanic; 7=multiracial, non-hispanic; 8=hispanic; 9=DK/NS/R
    data[, race := recode(race, "1=0; 2=1; 3=2; c(4,5)=3; 8=4; else=NA")]
  }
  data[, race := factor(race, levels=0:4, labels=c("NH white", "NH black", "NH AIAN", "NH API", "Hispanic"))]

  # standardize marital status
  data[, marital := recode(marital, "c(2,3,4)=0; c(1,6)=1; 5=2; else=NA")] # all years: 1=married; 2=divorced; 3=widowed; 4=separated; 5=never married; 6=a member of an unmarried couple; 9=refused
  data[, marital := factor(marital, levels=0:2, labels=c("former", "current", "never"))]
  
  # standardize education
  data[, edu := recode(edu, "c(1,2,3)=0; 4=1; 5=2; 6=3; else=NA")]
  data[, edu := factor(edu, levels=0:3, labels=c("less than HS", "HS grad", "some college", "college grad"))]

  # standardize tenure
if (!is.na(layout[svy, tenure]) & layout[svy, year] >= 2009) { # 2009-2017: 1 = Own, 2 = Rent, 3 = Other
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
  if (!is.na(layout[svy, weight]) & layout[svy, year] %in% 2002:2003) { # 1990-2003 (weight): reported weight in pounds
    data[weight == 777 | weight == 999, weight := NA]
    data[, weight := weight/2.2]
  } else if (!is.na(layout[svy, weight]) & layout[svy, year] %in% 2004:2017) { # 2004-2017 (wtkg2/wtkg3): calculated weight in kg (two implied decimal points)
    data[weight == 99999 | weight < 0, weight := NA]
    data[, weight := weight/100]
  } 
  
  
  # standardize smoking status
  if (layout[svy, smoker] %in% c("_smoker2", "a_smoker", "_smoker3") & layout[svy, year] >= 2002) { # smoker2/smoker3 (1996-2012): 1=current smoker, everyday; 2=current smoker some days; 3=former smoker; 4=never smoked
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
  if (!is.na(layout[svy, lastsmoke]) & layout[svy, year] %in% 2002:2003) { # LASTSMOK (2001-2003): 1 = < 1 month, 2 = 1-3 months, 3= 3-6 months, 4= 6-12 months, 5 = 1-5 years, 6 = 5-10 years, 7 = 10+ years,
    data[, lastsmoke := recode(lastsmoke, "c(1,2,3,4)=1; c(5,6,7)=2; else=NA")]
  } else if (!is.na(layout[svy, lastsmoke]) & layout[svy, year] %in% 2009:2017) { # LASTSMK1/LASTSMK2 (2009-2013): 1= < 1 month, 2= 1-3 months, 3= 3-6 months; 4= 6-12 months; 5=1-5 years; 6= 5-10 years; 7= 10+ years; 88= never smoked regularly
    data[, lastsmoke := recode(lastsmoke, "c(1,2,3,4)=1; c(5,6,7)=2; c(8,88)=3; else=NA")]
  }  
  if (!is.na(layout[svy, lastsmoke])) {
    data[, lastsmoke := factor(lastsmoke, levels=1:3, labels=c("< 1 yr", "> 1 yr", "never regularly smoked"))]
  }
  return(data)
})

## Combine files and format geography variables ----------------------------------------------------
# combine files
datasm <- rbindlist(all_datasm, use.names=T, fill=T)
datasm<-remove_attributes(datasm, "format.sas")
datasm<-remove_attributes(datasm, "label")

save(datasm, file="FILEPATH")

load("FILEPATH")

sum(table(table(subset[is.na(sex) & year %in% 2005:2017, c(sex)])))

table(datasm$sex, exclude=NULL)
table(datasm$race, exclude=NULL)
table(datasm$marital, exclude=NULL)
table(datasm$edu, exclude=NULL)
table(datasm$sample, exclude=NULL)

datasm<-datasm[age>0,]
datasm[, age := age -1]
table(datasm$age)

## Format and save ---------------------------------------------------------------------------------
datasmart <- datasm[, list(year, cbsa_wt, cbsa_name, cbsa, month, day, seqno,
                    age, sex, race, marital, edu, tenure, preg,
                    height, weight,
                    smoke, smoke_any, firstsmoke, lastsmoke 
                    )]

setkeyv(datasmart, c("year", "cbsa", "seqno", "sex", "age", "race", "edu", "marital"))
datasmart[, id := 1:.N]
save(datasmart, file=paste0(main_dir, "/smart_brfss_microdata.rdata"))

data <- datasmart
## Generate report ---------------------------------------------------------------------------------
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

brfss_report <- lapply(c(sort(unique(data$year)), 9999), function(yr) if (yr == 9999) cbind(year="All", report(data)) 
                      else cbind(year=yr, report(data[data$year %in% yr,])))
brfss_report <- do.call("rbind", brfss_report)
brfss_report <- brfss_report[order(brfss_report[,2], brfss_report[,1]),]
write.csv(brfss_report, file=paste0(main_dir, "survey_data/smart_brfss_cbsa_data_report.csv"), row.names=F)

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

# prep cholesterol
if (!is.na(layout[svy, cholchk]) & !is.na(layout[svy, bloodcho]) & !is.na(layout[svy, highchol]) & layout[svy, year] %in% 2002:2016) {
  data[, bloodcho := recode(bloodcho, "2=0; 1=1; else=NA")]
  setnames(data, "bloodcho", "cholcheck")
  data[, cholchk := factor(cholchk, levels=1:4, labels=c("< 1 yr", "1-2 yrs", "2-5 yrs", "5+ yrs"))]
  setnames(data, "cholchk", "lastcholcheck")
  data[, highchol := recode(highchol, "2=0; 1=1; else=NA")]
} else if (!is.na(layout[svy, cholchk]) & !is.na(layout[svy, bloodcho]) & !is.na(layout[svy, highchol])& layout[svy, year] == 2017) {
  data[, bloodcho := recode(bloodcho, "2=0; 1=1; else=NA")]
  setnames(data, "bloodcho", "cholcheck")
  data[, cholchk := factor(cholchk, levels=1:4, labels=c("< 1 yr", "1-2 yrs", "2-5 yrs", "5+ yrs"))]
  setnames(data, "cholchk", "lastcholcheck")
  data[, highchol := recode(highchol, "2=0; 1=1; else=NA")]
}

# standardize diabetes
if (layout[svy, year] %in% 2002:2003 & !is.na(layout[svy, diabetes])) { # 1994-2003: 1 = yes; 2 = yes, but female told only during pregnancy; 3 = no; 7 = DK/NS; 9 = Refused
  data[, diabetes := recode(diabetes, "c(2,3)=0; 1=1; else=NA")]
} else if (layout[svy, year] %in% 2004:2017 & !is.na(layout[svy, diabetes])) { # 2004-2017: 1 = yes; 2 = yes, but female told only during pregnancy; 3 = no; 4 = no, pre-diabetes or borderline diabetes; 7 = DK/NS; 9 = Refused
  data[, diabetes := recode(diabetes, "c(2,3,4)=0; 1=1; else=NA")]
} 

# standardize hypertension and hypertension medication
if (layout[svy, year] %in% 2002:2004 & !is.na(layout[svy, highbp])) { # 2002-2004: 1 = yes; 2 = yes, but female told only during pregnancy; 3 = no; 7 = DK/NS; 9 = refused
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
if (!is.na(layout[svy, drnkdri]) & layout[svy, drnkdri] == 'drnkdri') { # 1-76 = Number of times; 88 = None; 77 = Don't know/Not sure; 99 = Refused
  # episodes of alcohol impaired (AIDE) driving (within past 30 days)
  data[, aide := recode(drnkdri, "88=0; c(77,99)=NA")]
  # binary indicator for committing AIDE (within past 30 days)s
  data[, drnkdri := recode(drnkdri, "1:76=1; 88=0; else=NA")]
  # fix data entry errors where anydrnk=1 & drnkdri>0
  ###data[anydrnk == 0, recode(drinkdri, "1:76=0")]# FIXME different vars by year
}  

# prep falls
if(!is.na(layout[svy, falls])) { # (2014,2012):"Past 12 months", (2010,2008,2006):"Past 3 months". 1-76, 88=None, 77=DK/NS, 99=Refused
  data[, falls := recode(falls, "88=0; c(77,99)=NA")]
}

# prep employment and out of work
if (!is.na(layout[svy, employ])) {  # 1 = Employed for wages; 2 = Self-employed; 3 = Out of work for more than 1 year; 4 = Out of work for less than 1 year; 5 = A homemaker; 6 = A student; 7 = Retired; 8 = Unable to work; 9 = Refused
  data[, outwork := recode(employ, "c(3,4)=1; c(1,2,5,6,7,8)=0; else=NA")]
  data[, outwork1 := recode(employ, "c(1,2,4,5,6,7,8)=0; 3=1; else=NA")]
  data[, outwork2 := recode(employ, "c(1,2,3,5,6,7,8)=0; 4=1; else=NA")]
  data[, employ := recode(employ, "1:2=1; 3:8=0; else=NA")]
} 

return(data)
})
