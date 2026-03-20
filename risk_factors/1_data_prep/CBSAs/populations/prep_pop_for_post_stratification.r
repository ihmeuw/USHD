####################################################################################################
## Description: Prep population files to use for post-stratification
##                (1) Load input files and process as necessary to get the population groups needed.
##                (2) Collapse all population files to the merged county level
##                (3) collapse all population files to CBSA/remaing merged counties level
##                (5) Interpolate to fill out the time series
##                (6) Format data.tables and combine in a list
## Output:      A named list of data.tables for each stratifying dimension (race, education, marital
##                status, phone ownership) with the following variables: 'mcnty', 'year', 'sex',
##                'age', 'race', 'edu', 'marital', phone', 'pop'.
####################################################################################################

library(data.table)
library(reshape2)

rm(list=ls())
root <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
main_dir <- paste0(root, "FILEPATH")
pop_dir <- paste0(root, "FILEPATH")
loc_dir <- paste0(root, "FILEPATH")
out_dir <- paste0(root, "FILEPATH")
years <- c(1990, 2019)

#load area code reference file
load(paste0(main_dir,"locations/area.code_reference_file.rdata"))

#subset to variables needed to collapse
area<-area[, c('mcnty','area.codeint')]

## Load and prep all input files -------------------------------------------------------------------
## Race
pop_race <- readRDS(paste0(pop_dir, "nchs_pop_est_by_race_ethn.rds"))

# combine race/ethnicity groups into a single factor to match what we have from BRFSS
pop_race[hisp == 1, race := "Hispanic"]
pop_race[race == "AIAN" & hisp == 0, race := "NH AIAN"]
pop_race[race == "White" & hisp == 0, race := "NH White"]
pop_race[race == "Black" & hisp == 0, race := "NH Black"]
pop_race[race == "API" & hisp == 0, race := "NH API"]

pop_race[, race := factor(race, levels=c("Hispanic", "NH AIAN", "NH White", "NH Black", "NH API"))]
pop_race <- pop_race[, list(pop = sum(pop)), by='fips,year,sex,age,race']

setnames(pop_race, "fips", "cnty")
setkeyv(pop_race, c("cnty", "year"))

census_edu <- readRDS("FILEPATH")
table(census_edu$age_start, census_edu$age_end, exclude=NULL)

## Education
census_edu <- readRDS(paste0(pop_dir, c("census_pop_by_edu.rds")))
acs_edu <- readRDS(paste0(pop_dir, c("acs_pop_by_edu.rds")))
pop_edu <- rbindlist(list(census_edu, acs_edu))
rm(acs_edu,census_edu);gc()
setnames(pop_edu, "fips", "cnty")
setkeyv(pop_edu, c("cnty", "year"))

## Marital status
census_marital <- readRDS(paste0(pop_dir, c("census_pop_by_marital.rds")))
acs_marital <- readRDS(paste0(pop_dir, c("acs_pop_by_marital.rds")))
pop_marital <- rbindlist(list(census_marital, acs_marital))
rm(acs_marital,census_marital);gc()
setnames(pop_marital, "fips", "cnty")
setkeyv(pop_marital, c("cnty", "year"))

## Phone ownership
# load phone ownership rates from NHSR
data <- readRDS(paste0(root,'FILEPATH'))
table(data$year)
data <- melt(data[fips > 100,], id.vars=c("fips", "year"), variable.name ="phone")


# drop no phone and rescale other groups to sum to one (no phone isn't represented in the BRFSS, so we pretend it doesn't exist)
data <- data[phone != "no_phone",]
data <- data[, value := value/sum(value), by='fips,year']
data[fips == 2158, fips := 2270] # revert Kusilvak and Oglala Lakota back to old names to merge with the NCHS pop file
data[fips == 46102, fips := 46113]

# combine phone ownership rates with NCHS population estimates to get population by phone ownership (assume rates are constant across age and sex)
pop <- readRDS(paste0(pop_dir, "nchs_pop_est.rds"))
pop_phone <- merge(pop, data, by=c("fips", "year"), allow.cartesian=T)
pop_phone[, pop := round(pop * value)]
rm(pop, data); gc()

pop_phone <- pop_phone[, list(fips, year, sex, age, phone, pop)]
pop_phone[, phone := factor(phone, levels=c("dual_phone", "landline_only", "cell_only"))]
setnames(pop_phone, "fips", "cnty")
setkeyv(pop_phone, c("cnty", "year"))

## Collapase populations to the merged county level ------------------------------------------------
load(paste0(loc_dir, "counties/merged_counties.rdata"))
loc <- loc[, list(cnty, mcnty)]
setkeyv(loc, "cnty")

pop_race <- loc[pop_race,]
pop_race <- pop_race[age >= 18 & between(year, years[1], years[2]), list(pop = sum(pop)), by='mcnty,year,sex,age,race']
pop_edu <- loc[pop_edu,]
pop_edu <- pop_edu[age_start >= 18, list(pop = sum(pop)), by='mcnty,year,sex,age_start,age_end,edu']
pop_marital <- loc[pop_marital,]
pop_marital <- pop_marital[age_start >= 18, list(pop = sum(pop)), by='mcnty,year,sex,age_start,age_end,marital']
pop_phone <- loc[pop_phone,]
pop_phone <- pop_phone[age >= 18, list(pop = sum(pop)), by='mcnty,year,sex,age,phone']

##create age categories
pop_edu[, age:=ifelse(age_start==18 , 0, 
                   ifelse(age_start==25 , 1, 
                          ifelse(age_start==35 , 2, 
                                 ifelse((age_start==45 & age_end %in% c(54,64))|(age_start==55 & age_end ==59)|(age_start==60 & age_end ==64), 3,
                                               ifelse(age_start==65 | age_start==75, 4, NA)))))]

pop_marital[, age:=ifelse(age_start==18 | age_start==20, 0, 
                      ifelse(age_start==25 | age_start==30, 1, 
                             ifelse(age_start==35 | age_start==40 , 2, 
                                    ifelse(age_start==45 | age_start==50, 3,
                                           ifelse(age_start==55 | age_start==60, 4,
                                                  ifelse(age_start==65 | age_start==75 | age_start==85, 5, NA))))))]

pop_marital <- pop_marital[!is.na(age),]#dropped age marital 15 to 18
## Interpolate populations where necessary to get a complete time series ---------------------------
# by education
pop_edu_complete <- data.table(expand.grid(age = unique(pop_edu$age), mcnty = unique(pop_edu$mcnty),
                                           sex = unique(pop_edu$sex), edu = unique(pop_edu$edu),
                                           year = unique(pop_edu$year)))

pop_edux <- merge(pop_edu_complete, pop_edu[,!c('age_start','age_end')],
                 by = c('year','mcnty','sex','edu','age'), all = T)

pop_edux[is.na(pop), pop := 0]

table(pop_edu$year)

pop_edu <- pop_edux[, list(year = years[1]:years[2], pop = round(approx(year, pop, xout=years[1]:years[2])$y)),by='mcnty,sex,age,edu']

#by marital status
pop_marital_complete <- data.table(expand.grid(age = unique(pop_marital$age), mcnty = unique(pop_marital$mcnty),
                                           sex = unique(pop_marital$sex), marital = unique(pop_marital$marital),
                                           year = unique(pop_marital$year)))

pop_maritalx <- merge(pop_marital_complete, pop_marital[,!c('age_start','age_end')],
                 by = c('year','mcnty','sex','marital','age'), all = T)

pop_maritalx[is.na(pop), pop := 0]

pop_marital <- pop_maritalx[, list(year = years[1]:years[2], pop = round(approx(year, pop, xout=years[1]:years[2])$y)),by='mcnty,sex,age,marital']

pop_marital[year==2017,]$pop <- pop_marital[year==2016,]$pop

rm(pop_edux, pop_maritalx, pop_edu_complete, pop_marital_complete)

#change order to match brfss data
levels(pop_marital$marital)
pop_marital$marital <- factor(pop_marital$marital, levels = c("current","former","never"))

## Collapse populations to the CBSA/remaining merged county level ------------------------------------------------
pop_race <- merge(pop_race, area, by='mcnty', all.y=F) 
pop_race <- pop_race[, list(pop = sum(pop)), by='area.codeint,year,sex,age,race']
pop_edu <- merge(pop_edu, area, by='mcnty', all.y=F) 
pop_edu <- pop_edu[, list(pop = sum(pop)), by='area.codeint,year,sex,age,edu']
pop_marital <- merge(pop_marital, area, by='mcnty', all.y=F) 
pop_marital <- pop_marital[, list(pop = sum(pop)), by='area.codeint,year,sex,age,marital']
pop_phone <- merge(pop_phone, area, by='mcnty', all.y=F) 
pop_phone <- pop_phone[, list(pop = sum(pop)), by='area.codeint,year,sex,age,phone']

## Format output and save --------------------------------------------------------------------------
pop_race <- pop_race[, lapply(.SD, function(x) if (is.factor(x)) x else as.integer(x))]
setkeyv(pop_race, c("area.codeint", "year", "sex", "age", "race"))
pop_edu <- pop_edu[, lapply(.SD, function(x) if (is.factor(x)) x else as.integer(x))]
setkeyv(pop_edu, c("area.codeint", "year", "sex", "age", "edu"))
pop_marital <- pop_marital[, lapply(.SD, function(x) if (is.factor(x)) x else as.integer(x))]
setkeyv(pop_marital, c("area.codeint", "year", "sex", "age", "marital"))
pop_phone <- pop_phone[, lapply(.SD, function(x) if (is.factor(x)) x else as.integer(x))]
setkeyv(pop_phone, c("area.codeint", "year", "sex", "age", "phone"))

#save as a list -----------------------------
stratpop <- list(race = pop_race, edu = pop_edu, marital = pop_marital, phone = pop_phone)



save(stratpop, file=paste0(out_dir, "pop_for_post_stratification.rdata"))
