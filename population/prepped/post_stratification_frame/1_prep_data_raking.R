### Prep data PS frame
### Goal: compile joint distribution from survey/IPUMS by age/race/sex/edu/mcnty/year
###       to create post-stratification frame through raking to mcnty margins

###### Set up objects from command args
(non_fatal_repo <- commandArgs(TRUE)[[1]])
(settings_file <- commandArgs(TRUE)[[2]])
(date_time_stamp <- commandArgs(TRUE)[[3]])


library(data.table)
library(reshape2)

load("FILEPATH")

pop_dir <- "FILEPATH"
cov_dir <- "FILEPATH"
joint_dir <- "FILEPATH"

## Import settings
funcs <- list.files(paste0(non_fatal_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(non_fatal_repo, "/functions/", func)))
}

settings_path <- "FILEPATH"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc)

message('prepping mcnty marginal distributions')
##### Load marginal pop distribution ######################
pop_race <- readRDS(paste0(pop_dir, "/pop_by_race_ethn_1977/", pop_race_version, ".rds"))
pop_edu <- readRDS(paste0(pop_dir, "/pop_by_edu/", pop_edu_version, ".rds"))
pop_marital <- readRDS(paste0(pop_dir, "/pop_by_marital_status/", pop_marital_version, ".rds"))

stopifnot(uniqueN(pop_marital$year)*uniqueN(pop_marital$mcnty)*uniqueN(pop_marital$marital)*uniqueN(pop_marital$sex)*uniqueN(pop_marital$age)==nrow(pop_marital))
stopifnot(uniqueN(pop_edu$year)*uniqueN(pop_edu$mcnty)*uniqueN(pop_edu$edu)*uniqueN(pop_edu$sex)*uniqueN(pop_edu$age)==nrow(pop_edu))
stopifnot(uniqueN(pop_race$year)*uniqueN(pop_race$mcnty)*uniqueN(pop_race$race)*uniqueN(pop_race$sex)*uniqueN(pop_race$age)==nrow(pop_race))

#add state column to population files (marginal)
pop_race <- merge(pop_race[age>19], unique(loc[,c('mcnty','state_name')]), by='mcnty', allow.cartesian = T)
pop_marital  <- merge(pop_marital[age>19], unique(loc[,c('mcnty','state','state_name')]), by='mcnty', allow.cartesian = T)
pop_edu <- merge(pop_edu[age>19], unique(loc[,c('mcnty','state','state_name')]), by='mcnty', allow.cartesian = T)

if (joint_dist == 'BRFSS'){
  pop_marital[age==85, age := 80]
  pop_edu[age==85, age := 80]
  pop_race[age==85, age := 80]
}

pop_marital <- pop_marital[year %in% years, list(pop = sum(pop)), by = c("year", "state","mcnty","marital","sex","age")]
pop_edu <- pop_edu[year %in% years, list(pop = sum(pop)), by = c("year", "state","state_name","mcnty","edu","sex","age","edu_label")]
pop_race <- pop_race[year %in% years, list(pop = sum(pop)), by = c("year", "state","mcnty","race","sex","age")]

pop_edu[, edu := factor(edu_label, levels=c("Less than HS", "HS graduate", "Some college", "College graduate"), 1:4)]
pop_race[, race_label := factor(race, levels=c(1:4,7), c("NH White", "NH Black", "NH AIAN", "NH API", "Hispanic"))]
pop_marital[, marital_label := factor(marital, levels=c(1:3), c("current", "former", "never"))]

pop_race$race <- as.integer(pop_race$race)
pop_marital$marital <- as.integer(pop_marital$marital)
pop_edu$edu <- as.integer(pop_edu$edu)

if (withphone==T){
  ##### Load/format phone usage data
  # load phone ownership rates from NHSR
  phone <- readRDS(pop_phone_file)
  pop <- pop_race[year %in% years, list(pop = sum(pop)), by = c("year", "state","mcnty")]
  
  pop_phone <- merge(pop, phone, by=c("year", "state"), allow.cartesian=T)
  setnames(pop_phone, 'pop', 'total')
  pop_phone[, pop := total*value]
  pop_phone[, c('total','value') := c(NULL,NULL)]
  setnames(pop_phone, 'variable', 'phone_label')
  
  stopifnot(uniqueN(pop_phone$year)*uniqueN(pop_phone$mcnty)*uniqueN(pop_phone$phone)==nrow(pop_phone))
  #add state column to population files (marginal)
  pop_phone <- merge(pop_phone, unique(loc[,c('mcnty','state','state_name')]), by=c('mcnty','state'), allow.cartesian = T)
  pop_phone[, phone := factor(phone_label, c("cell_only", "landline_only", "dual_phone","no_phone"), 1:4)]
  pop_phone$phone <- as.integer(pop_phone$phone)
  
  joint_complete_phone_st <- data.table(expand.grid(age = as.factor(unique(pop_edu$age)), sex=unique(pop_edu$sex), race = unique(pop_race$race), 
                                                    edu = unique(pop_edu$edu), marital = unique(pop_marital$marital), phone=unique(pop_phone$phone), state = unique(loc$state)
                                                    ,year=unique(years)))
  
  joint_complete_phone_st <- joint_complete_phone_st[year %in% years,]
  pop_phone <- pop_phone[year %in% years]
  save(joint_complete_phone_st, pop_marital, pop_edu, pop_race, pop_phone, 
       file=paste0(joint_dir,date_time_stamp,'/pop_data.rdata'))
} else if (withphone==F){
  #to later make all joint distributions square (with NAs) then combine 
  joint_complete_st <- data.table(expand.grid(age = as.factor(unique(pop_edu$age)), sex=unique(pop_edu$sex), race = unique(pop_race$race), 
                                              edu = unique(pop_edu$edu), marital = unique(pop_marital$marital), state = unique(loc$state)
                                              ,year=unique(years)))
  joint_complete_st <- joint_complete_st[year %in% years,]
  
  save(joint_complete_st, pop_marital, pop_edu, pop_race, 
       file=paste0(joint_dir, date_time_stamp,'/pop_data.rdata'))
}

# load(file=paste0(joint_dir, date_time_stamp,'/pop_data.rdata'))

##### IPUMS ###############################################
message('prepping initial joint ditribution without phone')
#Import joint distribution at the national level

if (joint_dist=='IPUMS'){
  joint_ipums_st <- readRDS(ipums_file)
  joint_ipums_st <- setDT(joint_ipums_st)
  joint.initial.st <- joint_ipums_st[joint_ipums_st$AGE>19,]
  joint.initial.st[,age :=cut(AGE, breaks = c(seq(20,85,5),105), labels=seq(20, 85, len=14), right = FALSE)]
  
  joint.initial.st[MARST %in% c(3,4,5), marital := 2]#former
  joint.initial.st[MARST  %in% c(1,2), marital := 1]#current
  joint.initial.st[MARST==6, marital := 3]#never
  
  joint.initial.st[EDUCD > 100, edu := 4]#college grad
  joint.initial.st[EDUCD > 64 & EDUCD < 101, edu := 3]# some college
  joint.initial.st[EDUCD > 61 & EDUCD < 65, edu := 2]#HS grad
  joint.initial.st[EDUCD < 62, edu := 1]#LT HS
  joint.initial.st[EDUCD == 999, edu := NA]
  
  setnames(joint.initial.st, 'STATEFIP', 'state')
  setnames(joint.initial.st, 'SEX','sex')
  setnames(joint.initial.st, 'YEAR','year')
  
  joint.initial.st <- joint.initial.st[, c('race','edu','age','sex','marital','GQ','year','PERWT','state')]
  
  joint.initial.st <- joint.initial.st[, .(pop = sum(PERWT)), .(year,age,sex,race,edu,marital,state)]
  
  joint.initial.st <- merge (joint.initial.st, joint_complete_st, by=c('race','marital','edu','age','sex','year','state'),  all = T)
  
  joint.initial.st[is.na(pop),]$pop <- 0
  
  joint.initial.st[, age := as.numeric(as.character(age))]
  
  joint.initial.st <- joint.initial.st[year %in% years]
  
  save(joint.initial.st, file=paste0(joint_dir, date_time_stamp,'/joint.initial.st.rdata'))
  
  if (joint_pooled==T){
    #' Pool population across time by summing data for all adjacent years.
    #'
    #' @param data data.table with pop data. Must have column pop.
    #' @param pool_size number of years to pool over. Must be odd.
    #' @param id_vars columns in data to group by and sum. E.g. sex and age.
    #'
    #' @return data.table with data pooled across time.
    pool_adj_years <- function(data, pool_size, id_vars, pop_var){
      
      setnames(data, pop_var, 'pop')
      # Empty data.table for binding intermediate pooling steps
      data_pooled_all <- data.table(mid_year = numeric())
      
      # Loop through years creating a unique pooling for each year
      year_range <- sort(unique(data$year))
      for(yr in year_range){
        # Grab years to pool over
        pool_years <- c((yr - floor(pool_size / 2)):(yr + floor(pool_size / 2)))
        
        # If terminal years, use first/last window of size pool_size instead
        # Else create window with current year as center point
        if(any(pool_years < min(year_range))){
          pool_years <- head(year_range, pool_size)
        }else if(any(pool_years > max(year_range))){
          pool_years <- tail(year_range, pool_size)
        }else{
          pool_years <- c((yr - floor(pool_size / 2)):(yr + floor(pool_size / 2)))
        }
        
        # Create midpoint to keep track of previously made pop pools
        mid_pool_year <- pool_years[ceiling(length(pool_years)/2)]
        mid_pool_years_list <- c()
        
        # If pop pool has been made before, grab it and set as new year
        # Else create pooled pop as normal
        if(mid_pool_year %in% mid_pool_years_list){
          data_pooled_yr <- data_pooled_all[mid_year == mid_pool_year]
          data_pooled_yr <- data_pooled_yr[year == unique(data_pooled_yr$year)[1]]
          data_pooled_yr[,year := yr]
          data_pooled_all <- rbind(data_pooled_all, data_pooled_yr)
        }else{
          data_pooled_yr <- data[year %in% pool_years]
          data_pooled_yr <- data_pooled_yr[,.(pop = sum(pop),
                                              year = yr,
                                              mid_year = mid_pool_year),
                                           by = id_vars]
          data_pooled_all <- rbind(data_pooled_yr, data_pooled_all,
                                   fill = TRUE)
          mid_pool_years_list <- c(mid_pool_years_list, mid_pool_year)
        }
      }
      
      # Remove midpoint tracking column
      data_pooled_all$mid_year <- NULL
      setnames(data_pooled_all, 'pop', pop_var)
      
      return(data_pooled_all)
    }
    
    # Pool over 5 years twice to create pyramid weights
    pop5 <- pool_adj_years(joint.initial.st, 5, 
                           c('state','sex','age','race','edu','marital'),
                           'pop')
    
    pop5 <- pool_adj_years(pop5, 5, 
                           c('state','sex','age','race','edu','marital'), 
                           'pop')
    
    save(pop5, file=paste0(joint_dir, date_time_stamp,'/joint.initial.st5.rdata'))
  }
  
  
  
  if (withphone ==T){
    message('prepping initial joint ditribution with phone')
    ##### IPUMS - phone ############################################
    PHONE <- pop_phone[,.(pop = sum(pop)),.(year,phone,state)]
    phone <- dcast(PHONE, state + year ~ phone , value.var = "pop")
    phone.perc <- phone[,3:6]/rowSums(phone[,3:6])
    setnames(phone.perc, c('1','2','3','4'), c('phone1','phone2','phone3','phone4'))
    new.data1 <- cbind(phone, phone.perc)
    
    new.data <- merge(joint.initial.st, new.data1, by=c('year','state'))
    new.data[,popphone1 := round(pop*phone1)]
    new.data[,popphone2 := round(pop*phone2)]
    new.data[,popphone3 := round(pop*phone3)]
    new.data[,popphone4 := round(pop*phone4)]
    
    joint.initial.phone.st <- melt(new.data, id.vars=c("age", "sex","year","race","marital","edu","state"),
                                   measure.vars=c("popphone1", "popphone2", "popphone3", "popphone4" ),
                                   # Name of the destination column that will identify the original
                                   # column that the measurement came from
                                   variable.name="phone",
                                   value.name="pop")
    
    joint.initial.phone.st <- setDT(joint.initial.phone.st)
    
    joint.initial.phone.st[, phone := factor(phone, levels=c("popphone1", "popphone2", "popphone3", "popphone4"), 1:4)]
    joint.initial.phone.st[, age := as.numeric(as.character(age))]
    joint.initial.phone.st[, phone := as.integer(phone)]
    
    joint.initial.phone.st <- joint.initial.phone.st[phone != 4]
    
    joint.initial.phone.st <- joint.initial.phone.st[year %in% years]
    
    save(joint.initial.phone.st, file=paste0(joint_dir, date_time_stamp, '/joint.initial.phone.st.rdata'))
  }
  
} else if (joint_dist == 'BRFSS'){
  brfss <- readRDS("FILEPATH")
  brfss <- brfss[year %in% years]
  brfss[, age := cut(age_continuous, breaks = c(seq(20,80,5),105), labels=seq(20,80,5), right = FALSE)]
  setnames(brfss, c('race77','edu','marital'), c('race_label','edu_label','marital_label'))
  race_map <- list(race = c(1:4,7,9), race_label = c("NH White", "NH Black", "NH AIAN", "NH API", "Hispanic", "all_races"))
  education_map <- data.table(edu = c(1:4,9), edu_label = c("less than HS", "HS grad", "some college", "college grad", "all_edu"))
  marital_map <- list(marital = c(1,2,3,9), marital_label = c("current", "former", "never", "all_marital"))
  brfss <- merge(brfss, race_map, by='race_label', all.x=T)
  brfss <- merge(brfss, education_map, by='edu_label', all.x=T)
  brfss <- merge(brfss, marital_map, by='marital_label', all.x=T)
  
  joint.initial.st <- brfss[!is.na(race) & !is.na(edu) & !is.na(marital) & !is.na(sex) & !is.na(age), .(pop = sum(wt)), .(year,age,sex,race,edu,marital,state)]
  joint.initial.st[, sex := as.integer(sex)]
  joint.initial.st <- merge (joint.initial.st, joint_complete_st, by=c('race','marital','edu','age','sex','year','state'),  all = T)
  
  joint.initial.st[is.na(pop),]$pop <- 0
  
  joint.initial.st[, age := as.numeric(as.character(age))]
  
  joint.initial.st <- joint.initial.st[year %in% years]
  
  save(joint.initial.st, file=paste0(joint_dir, date_time_stamp,'/joint.initial.st.rdata'))
  
}

message('script completed')
