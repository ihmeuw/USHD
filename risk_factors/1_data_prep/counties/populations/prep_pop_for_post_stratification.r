####################################################################################################
## Description: Prep population files to use for post-stratification
##                (1) Load input files and process as necessary to get the population groups needed.
##                (2) Collapse all population files to the merged county level
##                (3) Interpolate to fill out the time series
##                (4) Format data.tables and combine in a list
## Output:      A named list of data.tables for each stratifying dimension (race, education, marital
##                status, phone ownership) with the following variables: 'mcnty', 'year', 'sex',
##                'age', 'race', 'edu', 'marital', phone', 'pop'.
####################################################################################################

library(data.table)
library(reshape2)

rm(list=ls())
root <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
pop_dir <- paste0(root, "FILEPATH")
cov_dir <- paste0(root, "FILEPATH")
loc_dir <- paste0(root, "FILEPATH")
out_dir <- paste0(root, "FILEPATH")

years <- c(2000,2019)
ages <- c(20, 25,35, 45, 55, 65, 75, 85, 100)

## Load and prep all input files -------------------------------------------------------------------
## Race
files <- file.info(list.files(paste0(pop_dir, "raw/nchs_pop_est_by_race_ethn"), full.names = T))
file <- rownames(files)[which.max(files$mtime)]# grab newest version
pop_race <- readRDS(file)

# combine race/ethnicity groups into a single factor to match what we have from BRFSS
pop_race[hisp == 1, race := "Hispanic"]
pop_race[, race := factor(race, levels=c("White", "Black", "AIAN", "API", "Hispanic"))]
pop_race <- pop_race[, list(pop = sum(pop)), by='fips,year,sex,age,race']

setnames(pop_race, "fips", "cnty")
setkeyv(pop_race, c("cnty", "year"))

## Education
pop_edu <- readRDS('FILEPATH')

## Marital status
files <- paste0(pop_dir, c("raw/census_pop_by_marital.rds", "raw/acs_pop_by_marital.rds"))
pop_marital <- rbindlist(lapply(files, function(x) { readRDS(x)}))
setnames(pop_marital, "fips", "cnty")
setkeyv(pop_marital, c("cnty", "year"))

## Phone ownership
# load phone ownership rates from NHSR
data <- readRDS(paste0(cov_dir, "raw/nhsr_phone_usage.rds"))
data <- melt(data[fips > 100,], id.vars=c("fips", "year"), variable.name ="phone")

# drop no phone and rescale other groups to sum to one (no phone isn't represented in the BRFSS, so we pretend it doesn't exist)
data <- data[phone != "no_phone",]
data <- data[, value := value/sum(value), by='fips,year']

# combine phone ownership rates with NCHS population estimates to get population by phone ownership (assume rates are constant across age and sex)
pop <- readRDS(paste0(pop_dir, "raw/nchs_pop_est.rds"))
pop_phone <- merge(pop, data, by=c("fips", "year"), allow.cartesian=T)
pop_phone[, pop := round(pop * value)]
rm(pop, data); gc()

pop_phone <- pop_phone[, list(fips, year, sex, age, phone, pop)]
pop_phone[, phone := factor(phone, levels=c("dual_phone", "landline_only", "cell_only"))]
setnames(pop_phone, "fips", "cnty")
setkeyv(pop_phone, c("cnty", "year"))

## Collapase populations to the merged county level ------------------------------------------------
load(paste0(loc_dir, "merged_counties.rdata"))
loc <- loc[, list(cnty, mcnty)]
setkeyv(loc, "cnty")

##create age categories
pop_edu[, age:= ifelse(age %in% c(20), 0,
                      ifelse(age %in% c(25,30), 1, 
                            ifelse(age %in% c(35,40), 2, 
                                   ifelse(age %in% c(45,50), 3, 
                                          ifelse(age %in% c(55,60), 4,
                                                 ifelse(age %in% c(65,70), 5, 
                                                        ifelse(age %in% c(75,80), 6,
                                                               ifelse(age %in% c(85), 7,NA))))))))]
#table(pop_marital$age_start, pop_marital$age_end)
pop_marital[, age:=ifelse(age_start==20, 0, 
                          ifelse(age_start==25 | age_start==30, 1, 
                                 ifelse(age_start==35 | age_start==40 , 2, 
                                        ifelse(age_start==45 | age_start==50, 3,
                                               ifelse(age_start==55 | age_start==60, 4,
                                                      ifelse(age_start==65 , 5,
                                                             ifelse(age_start==75, 6,
                                                                    ifelse(age_start==85, 7,NA))))))))]
pop_race <- loc[pop_race,]
pop_race <- pop_race[age %in% min(ages):max(ages) & between(year, years[1], years[2]),]
pop_race[, age := cut(age, ages, labels=F, include.lowest=T, right=F) - 1]
pop_race <- pop_race[, list(pop = sum(pop)), by='mcnty,year,sex,age,race']
pop_edu <- pop_edu[!is.na(age), list(pop = sum(pop)), by='mcnty,year,sex,age,edu']
pop_marital <- loc[pop_marital,]
pop_marital <- pop_marital[!is.na(age), list(pop = sum(pop)), by='mcnty,year,sex,age,marital']
pop_phone <- loc[pop_phone,]
pop_phone <- pop_phone[age %in% min(ages):max(ages) & between(year, years[1], years[2]),]
pop_phone[, age := cut(age, ages, labels=F, include.lowest=T, right=F) - 1]
pop_phone <- pop_phone[, list(pop = sum(pop)), by='mcnty,year,sex,age,phone']




#label race categories to be consistent with data
levels(pop_race$race) <- c("NH white", "NH black", "NH AIAN", "NH API", "Hispanic")

## Interpolate populations where necessary to get a complete time series ---------------------------
pop_edu <- pop_edu[, list(year = years[1]:years[2], pop = round(approx(x=year, y=pop, xout=years[1]:years[2], rule=2)$y)),
                   by='mcnty,sex,age,edu']
pop_marital <- pop_marital[, list(year = years[1]:years[2], pop = round(approx(x=year, y=pop, xout=years[1]:years[2], rule=2)$y)),
                           by='mcnty,sex,age,marital']


## Format output and save --------------------------------------------------------------------------
for (var in c('race','edu','marital','phone')){
  assign('pop',get(paste0('pop_',var)))
  pop <- pop[, lapply(.SD, function(x) if (is.factor(x)) x else as.integer(x))]
  setkeyv(pop, c("mcnty", "year", "sex", "age", var))  
  assign(paste0('pop_',var), pop)
}

stratpop <- list(race = pop_race, edu = pop_edu, marital = pop_marital, phone = pop_phone)
save(stratpop, file=paste0(out_dir, "pop_for_post_stratification.rdata"))
rm(pop_phone, pop_race, pop_edu, pop_marital, loc, pop)
