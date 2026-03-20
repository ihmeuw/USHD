### standardize pop margins
### Goal: standardize population marginal and joint distributions
###       and plot checks

###### Set up objects from command args
(non_fatal_repo <- commandArgs(TRUE)[[1]])
(settings_file <- commandArgs(TRUE)[[2]])
(date_time_stamp <- commandArgs(TRUE)[[3]])


## Import settings
funcs <- list.files(paste0(non_fatal_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(non_fatal_repo, "/functions/", func)))
}

settings_path <- "FILEPATH"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc)

message(date_time_stamp)
library(data.table)

joint_dir <- "FILEPATH"

#import mcnty-state reference file ######################
load("FILEPATH")

##### Load marginal pop distribution ######################
load(paste0(joint_dir,date_time_stamp,'/pop_data.rdata'))

# ##### IPUMS ###############################################
#Create IPUMS-based PS frame #####################################################
if (withphone==T){
  load(paste0(joint_dir, date_time_stamp,'/joint.initial.phone.st.rdata'))
} 

if (joint_pooled ==F){
  load(paste0(joint_dir, date_time_stamp,'/joint.initial.st.rdata'))
  popjoint.st <- setDT(joint.initial.st)
  setnames(popjoint.st, 'pop', 'state_value')
} else if (joint_pooled ==T){
  load(paste0(joint_dir, date_time_stamp,'/joint.initial.st5.rdata'))
  popjoint.st <- setDT(pop5)
  setnames(popjoint.st, 'pop', 'state_value')
}
############# mcnty marginal data
# subset to years
popraceagesex_m <- pop_race
popeduagesex_m <- pop_edu
popmaritalagesex_m <- pop_marital

# Check how many 0s in all strata?
if (nrow(popraceagesex_m) != nrow(popraceagesex_m[! (pop %in% 0),])) {
  print(paste0(nrow(popraceagesex_m[(pop %in% 0),]), ' race/age/sex strata  or ',
               round(nrow(popraceagesex_m[(pop %in% 0),])/nrow(popraceagesex_m)*100) ,
               ' % have 0 population in marginal distribution across all years'))
}
if (nrow(popeduagesex_m) != nrow(popeduagesex_m[! (pop %in% 0),])) {
  print(paste0(nrow(popeduagesex_m[(pop %in% 0),]), ' edu/age/sex strata  or ',
               round(nrow(popeduagesex_m[(pop %in% 0),])/nrow(popeduagesex_m)*100) ,
               ' % have 0 population in marginal distribution across all years'))
}
if (nrow(popmaritalagesex_m) != nrow(popmaritalagesex_m[! (pop %in% 0),])) {
  print(paste0(nrow(popmaritalagesex_m[(pop %in% 0),]), ' marital/age/sex strata or ',
               round(nrow(popmaritalagesex_m[(pop %in% 0),])/nrow(popmaritalagesex_m)*100) ,
               ' % have 0 population in marginal distribution across all years'))
}

if (withphone==T){
  if (nrow(popphone_m) != nrow(popphone_m[! (pop %in% 0),])) {
    print(paste0(nrow(popphone_m[(pop %in% 0),]), ' phone strata or ',
                 round(nrow(popphone_m[(pop %in% 0),])/nrow(popphone_m)*100) ,
                 ' % have 0 population in marginal distribution across all years'))
  }
  setnames(popphone_m, 'pop', 'phone_value')
  popphone_m <- popphone_m[phone!=4]
}
setnames(popraceagesex_m, 'pop', 'race_value')
setnames(popeduagesex_m, 'pop', 'edu_value')
setnames(popmaritalagesex_m, 'pop', 'marital_value')

#standardize state-level distribution to match NCHS total by age/sex/race/state/year level
popjoint.st <- merge(popjoint.st, 
                     popraceagesex_m[ ,list(state_age_sex_yr_race=sum(race_value)), 
                                      by=c('state','age','sex','year','race')], 
                     by=c('state','age','sex','year','race'))
popjoint.st <- merge(popjoint.st, 
                     popjoint.st[ ,list(pop_sum_r=sum(state_value)), 
                                  by=c('state','age','sex','year','race')], 
                     by=c('state','age','sex','year','race'))

print(paste('There are', nrow(popjoint.st[pop_sum_r==0 & state_age_sex_yr_race==0])/12, 
            'age/sex/race/year/state strata with 0 pop in both the IPUMS joint',
            'distribution and the pop by race margin --> these will be kept as 0'))

# If pop=0 in both joint distribution and pop by race --> keep 0
# If pop=0 in joint distribution but not in pop by race --> use pop 
if (replace_0_preraking==T){
  popjoint.st[,mismatched := ifelse((pop_sum_r==0 & state_age_sex_yr_race!=0), 1, 0)]
  print(paste('There are', nrow(popjoint.st[mismatched == 1])/12, 
              'age/sex/race/year/state strata with 0 pop in the IPUMS joint distribution',
              'but NOT in the pop by race margin --> these will be replaced by the pop value',
              'from the pop by race file and divided equally across the 12 corresponding edu/marital strata'))
  popjoint.st[, `:=` (state_value2 = ifelse(pop_sum_r==0 & state_age_sex_yr_race==0, 0, 
                                 ifelse(mismatched == 1, state_age_sex_yr_race,
                                        state_value*state_age_sex_yr_race/pop_sum_r)))]
  
  # pool across all years to grab proportions to apply to the pops taken from the joint dist.
  popjoint.st[,state_value_all_years := sum(state_value), by = c('state','age','sex','race','edu','marital')]
  popjoint.st[,pop_sum_r_all_years := sum(state_value), by = c('state','age','sex','race')]
  popjoint.st[,state_prop_all_years := state_value_all_years / pop_sum_r_all_years]
  popjoint.st[is.nan(state_prop_all_years), state_prop_all_years := 0]
  print(paste('There are', nrow(popjoint.st[mismatched == 1 & pop_sum_r_all_years == 0])/12,
              'age/sex/race/state strata with 0 pop still in the IPUMS joint distribution',
              'after all year pooling'))
  
  # pool across all states to grab proportions to apply to the pops taken from the joint dist.
  popjoint.st[,state_value_all_states := sum(state_value), by = c('year','age','sex','race','edu','marital')]
  popjoint.st[,pop_sum_r_all_states := sum(state_value), by = c('year','age','sex','race')]
  popjoint.st[,state_prop_all_states := state_value_all_states / pop_sum_r_all_states]
  popjoint.st[is.nan(state_prop_all_states), state_prop_all_states := 0]
  print(paste('There are', nrow(popjoint.st[mismatched == 1 & pop_sum_r_all_states == 0])/12,
              'age/sex/race/year strata with 0 pop still in the IPUMS joint distribution',
              'after nationally pooling'))
  
  popjoint.st[mismatched == 1, state_value2 := ifelse(pop_sum_r_all_years > 0, (state_prop_all_years * state_value2),
                                                      (state_prop_all_states * state_value2))]
  
  saveRDS(popjoint.st[mismatched == 1], 
          paste0(joint_dir, date_time_stamp,'/mismatching_zeroes_race.rds'))
  
  popjoint.st <- popjoint.st[,-c('mismatched','state_value_all_years','pop_sum_r_all_years',
                                 'state_prop_all_years','state_value_all_states','pop_sum_r_all_states',
                                 'state_prop_all_states')]
  
} else {
  popjoint.st[, state_value2 := ifelse(pop_sum_r==0, 0, state_value*state_age_sex_yr_race/pop_sum_r)]
}

age_sex_year_mcnty_edu <- merge(popeduagesex_m[ ,list(age_sex_year_mcnty_edu=sum(edu_value)), by=c('mcnty','age','sex','year')], 
                               popraceagesex_m[ ,list(age_sex_year_mcnty_race=sum(race_value)), by=c('mcnty','age','sex','year')], 
                               by=c('mcnty','age','sex','year'))

if (nrow(age_sex_year_mcnty_edu[(age_sex_year_mcnty_edu==0 & age_sex_year_mcnty_race!=0) |(age_sex_year_mcnty_edu!=0 & age_sex_year_mcnty_race==0)])>0){
  print(paste0('WARNING: There are ', nrow(age_sex_year_mcnty_edu[(age_sex_year_mcnty_edu==0 & age_sex_year_mcnty_race!=0) |(age_sex_year_mcnty_edu!=0 & age_sex_year_mcnty_race==0)]), ' age/sex/mcnty/year strata with 0 pop in edu marginal pop but not race'))
  saveRDS(age_sex_year_mcnty_edu[(age_sex_year_mcnty_edu==0 & age_sex_year_mcnty_race!=0) |(age_sex_year_mcnty_edu!=0 & age_sex_year_mcnty_race==0)],
    paste0(joint_dir, date_time_stamp,'/mismatching_zeroes_edu.rds'))
}

age_sex_year_mcnty_marital <- merge(popmaritalagesex_m[ ,list(age_sex_year_mcnty_marital=sum(marital_value)), by=c('mcnty','age','sex','year')], 
                                popraceagesex_m[ ,list(age_sex_year_mcnty_race=sum(race_value)), by=c('mcnty','age','sex','year')], 
                                by=c('mcnty','age','sex','year'))

if (nrow(age_sex_year_mcnty_marital[(age_sex_year_mcnty_marital==0 & age_sex_year_mcnty_race!=0) |(age_sex_year_mcnty_marital!=0 & age_sex_year_mcnty_race==0)])>0){
  print(paste0('WARNING: There are ', nrow(age_sex_year_mcnty_marital[(age_sex_year_mcnty_marital==0 & age_sex_year_mcnty_race!=0) |(age_sex_year_mcnty_marital!=0 & age_sex_year_mcnty_race==0)]), ' age/sex/mcnty/year strata with 0 pop in marital marginal pop but not race'))
  saveRDS(age_sex_year_mcnty_marital[(age_sex_year_mcnty_marital==0 & age_sex_year_mcnty_race!=0) |(age_sex_year_mcnty_marital!=0 & age_sex_year_mcnty_race==0)],
    paste0(joint_dir, date_time_stamp,'/mismatching_zeroes_marital.rds'))
}


##########Phone PS frame######################################################################
# create mcnty joint distribution (applying age/sex/year/race/edu/marital joint dist to all mcnties within the same state)
jointipums_m <- vector(mode = "list", length = uniqueN(loc$mcnty))
if (withphone==T){
  for (m in unique(loc$mcnty)){
  print(m)
  joint.initial.phone.st$mcnty <- m
  jointipums_m[[m+1]] <- joint.initial.phone.st[state==unique(loc[mcnty==m]$state) & phone !=4]
  }
} else if (withphone==F){
  joint.initial.st <- copy(popjoint.st)
  for (m in unique(loc$mcnty)){
    print(m)
    joint.initial.st$mcnty <- m
    jointipums_m[[m+1]] <- joint.initial.st[state==unique(loc[mcnty==m]$state)]
  }
}

jointipums_p <- setDT(rbindlist(jointipums_m))
if (withphone==F){
  setnames(jointipums_p, 'state_value2', 'pop')
}
saveRDS(jointipums_p, paste0(joint_dir, date_time_stamp,'/jointipums_p.rds'))

#Calculate margin totals for standardization of joint distributions
state_yr <- popjoint.st[ , list(state_yr=sum(state_value2)), by=c('state','year')]
state_age_sex_yr_edu <- popjoint.st[, list(state_age_sex_yr_edu=sum(state_value2)), by=c('state','age','sex','year','edu')]
state_age_sex_yr_marital <- popjoint.st[, list(state_age_sex_yr_marital=sum(state_value2)), by=c('state','age','sex','year','marital')]


#standardize mcnty distribution by age/sex/edu/year to match state-level total
if (std_to_ipums==T){
  #merge each mcnty distribution with corresponding standardization margin (from state distribution)
  popeduagesex_m <- merge(popeduagesex_m, state_age_sex_yr_edu, by=c('state','age','sex','year','edu'))
  popmaritalagesex_m <- merge(popmaritalagesex_m, state_age_sex_yr_marital, by=c('state','age','sex','year','marital'))
  
  agesexyearstateedu <- popeduagesex_m[ ,list(pop_sum_e=sum(edu_value)), by=c('state','age','sex','year','edu')]
  popeduagesex_m <- merge(popeduagesex_m, agesexyearstateedu, by=c('state','age','sex','year','edu'))
  popeduagesex_m[, edu_value2 := ifelse(pop_sum_e==0, 0, edu_value*state_age_sex_yr_edu/pop_sum_e)]

  #standardize mcnty distribution by age/sex/marital/year to match state-level total
  agesexyearstatemarital <- popmaritalagesex_m[ ,list(pop_sum_m=sum(marital_value)), by=c('state','age','sex','year','marital')]
  popmaritalagesex_m <- merge(popmaritalagesex_m, agesexyearstatemarital, by=c('state','age','sex','year','marital'))
  popmaritalagesex_m[, marital_value2 := ifelse(pop_sum_m==0, 0, marital_value*state_age_sex_yr_marital/pop_sum_m)]
}

if (withphone==T){
    popphone_m <- merge(popphone_m, state_yr, by=c('state','year'))
    yearstate <- popphone_m[ ,list(pop_sum_p=sum(phone_value)), by=c('state','year')]
    popphone_m <- merge(popphone_m, yearstate, by=c('state','year'))
    popphone_m[, `:=` (phone_value2 = ifelse(pop_sum_p==0, 0, phone_value*state_yr/pop_sum_p))]
}
message('merge all marginal distributions')


# for vetting:
popjoint.st_std <- copy(popjoint.st)
save(popmaritalagesex_m, popeduagesex_m, popjoint.st_std, file=paste0(joint_dir, date_time_stamp,'/std_pops.rdata'))

# merge marginal distrobutions into one frame to be used for raking
marginal <- setDT(merge(jointipums_p[,!'state_value'], popraceagesex_m, by=intersect(names(jointipums_p), names(popraceagesex_m))))
marginal <- merge(marginal, popmaritalagesex_m, by=intersect(names(marginal), names(popmaritalagesex_m)))
marginal <- merge(marginal, popeduagesex_m, by=intersect(names(marginal), names(popeduagesex_m)))
if (withphone==T){
  marginal <- merge(marginal, popphone_m[phone != 4], by=intersect(names(marginal), names(popphone_m)))
}

# merge on to mcnty joint state-level distribution margin (by age/sex/race/edu/marital/year)
marginal <- merge(marginal, popjoint.st[, !'state_age_sex_yr_race'], 
                  by=intersect(names(marginal), names(popjoint.st[, !'state_age_sex_yr_race'])), 
                  allow.cartesian = T)

mcnty_age_sex_race_yr <- copy(popraceagesex_m[,!c('race_set','race_label','state','state_name','id_source')])
setnames(mcnty_age_sex_race_yr,'race_value', 'race_value_std')


#standardize mcnty joint distribution (initial frame used in raking) by age/sex/race/edu/marital/mcnty/year to NCHS data by age/sex/race/mcnty/year
marginal <- merge(marginal, mcnty_age_sex_race_yr, by=c('mcnty','age','sex','year','race'))
agesexyearracemcnty <- marginal[ ,list(pop_sum=sum(pop)), by=c('mcnty','age','sex','year','race')]
marginal <- merge(marginal, agesexyearracemcnty, by=c('mcnty','age','sex','year','race'))
marginal[, `:=` (pop2 = ifelse(pop_sum==0, 0, pop*race_value_std/pop_sum))]

if (withphone==T){
  #delete old margins' columns
  marginal <- marginal[,c('age','sex','race','edu','marital','phone','race_label','edu_label','marital_label','phone_label',
                          'state','state_name','year','mcnty','pop2','edu_value2','marital_value2','race_value2','state_value','phone_value2')]
  
  #rename
  setnames(marginal, c('pop2','edu_value2','marital_value2','phone_value2','state_value2'), 
           c('pop','edu_value','marital_value','phone_value','state_value'))
  
  save(marginal, file=paste0(joint_dir,date_time_stamp,'/marginal_dist_phone.rdata'))
} else if (withphone==F){
  #delete old margins' columns
  if (std_to_ipums==T){
  marginal <- marginal[,c('age','sex','race','edu','marital','race_label','edu_label','marital_label',
                          'state','state_name','year','mcnty','pop2','edu_value2','marital_value2','race_value','state_value2')]
  
  #rename
  setnames(marginal, c('pop2','edu_value2','marital_value2','state_value2'), c('pop','edu_value','marital_value','state_value'))
  } else {
    marginal <- marginal[,c('age','sex','race','edu','marital','race_label','edu_label','marital_label','edu_value','marital_value',
                            'state','state_name','year','mcnty','pop2','race_value','state_value2')]
    
    #rename
    setnames(marginal, c('pop2','state_value2'), c('pop','state_value'))
  }
  save(marginal, file=paste0(joint_dir, date_time_stamp,'/marginal_dist_nophone.rdata'))
}

message('script completed')
