####################################################################################################
## Description: Merge BRFSS and SMART BRFSS microdata 
## to run after prep_brfss and prep_brfss_smart
##
####################################################################################################
library(tidyverse)
library(data.table)
library(tidyr)#separate


rm(list=ls())
root <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
main_dir <- paste0(root, "FILEPATH")

#import smart
load(paste0(main_dir, "survey_data/smart_brfss_microdata.rdata"))
datasmart<-datasmart[year %in% 2005:2017,]
table(datasmart$year)
#import all brfss
load(paste0(main_dir, "survey_data/brfss_microdata.rdata"))
databrfss<-databrfss[year %in% 2005:2017,]
#table(databrfss$year)


#create state variable and MSA type in SMART  
smartclean <- separate(datasmart, cbsa_name, c("cbsaname","stateabb"), ", ", extra="merge")
smartclean <- separate(smartclean, stateabb, c("stateabb","MMSA"), " Statistical Area", remove=T, extra="merge")
smartclean <- separate(smartclean, stateabb, c("stateabb","MMSAtype"), " ", remove=T, extra="merge")
smartclean <- separate(smartclean, stateabb, c("stateabb","MMSA"), ",", remove=T, extra="merge")
smart <- smartclean[,-c("MMSA","cbsaname")]
smart <- separate(smart, stateabb, c("stateabb1","stateabb2","stateabb3","stateabb4"), "-", remove=T, extra="merge")
smart$MMSAtype<-recode(smart$MMSAtype, "Metropolitan Division"= "Metropolitan","Metropolitan Statistical"= "Metropolitan",
                       "Metropolitan Statistical Ar"= "Metropolitan","Metropolitan Statistical Are"= "Metropolitan",
                       "Metropolitan Statistical Area" = "Metropolitan", "Micropolitan Statistical Area"="Micropolitan")
setnames(smart, 'id', 'id.smart')
setnames(smart, 'seqno', 'seqno.smart')
rm(smartclean, datasmart)

table(smartclean$MMSAtype, smartclean$stateabb, exclude=NULL)
smartclean[is.na(MMSAtype),]
table(smartclean[is.na(MMSAtype),]$year, exclude=NULL)
table(smartclean[is.na(MMSAtype),]$cbsaname, smartclean[is.na(MMSAtype),]$year, exclude=NULL)
smartclean[cbsaname=='Albuquerque',c('cbsa','MMSAtype')]
table(smartclean[cbsaname=='Albuquerque',]$cbsa, smartclean[cbsaname=='Albuquerque',]$MMSAtype, exclude=NULL)

# remove respondents in MSA from BRFSS and append SMART respondents (in metropolitan areas only)
brfssnomsa<-databrfss[msa.yn %in% c(0, NA),]#5,724,421 obs 60 vars
smartmsa<-smart[year %in% 2005:2017 & MMSAtype == 'Metropolitan',]#2,953,829 obs 45 vars
appendmsa<-rbind(brfssnomsa, smartmsa, fill=TRUE)#5,655,888 obs 69 vars (data brfss has 6,573,761 obs)

# add state fips to SMART data in appended dataset (up to 4 states per CBSA)
states <- read.csv("FILEPATH")
setnames(states, c('STUSAB', 'STATEA'), c('stateabb','state.id'))
states <- states[,c('stateabb','state.id')]
appendmsa1<-merge(appendmsa, states, by.x='stateabb1', by.y='stateabb', all.x = T)
setnames(appendmsa1, 'state.id', 'state.id1')
appendmsa2<-merge(appendmsa1, states, by.x='stateabb2', by.y='stateabb', all.x = T)
setnames(appendmsa2, 'state.id', 'state.id2')
appendmsa3<-merge(appendmsa2, states, by.x='stateabb3', by.y='stateabb', all.x = T)
setnames(appendmsa3, 'state.id', 'state.id3')
appendmsa4<-merge(appendmsa3, states, by.x='stateabb4', by.y='stateabb', all.x = T)
setnames(appendmsa4, 'state.id', 'state.id4')
appendmsa4$cbsa <- as.numeric(appendmsa4$cbsa)

#change order/labels to match pop race/edu/marital categories (phone categories order match)
appendmsa4$race <- factor(appendmsa4$race, levels = c("Hispanic","NH AIAN","NH white","NH black","NH API"),
                    labels = c("Hispanic","NH AIAN","NH White","NH Black","NH API"))
appendmsa4$edu <- factor(appendmsa4$edu, levels = c("less than HS","HS grad","some college","college grad"))
appendmsa4$marital <- factor(appendmsa4$marital, levels = c("current","former","never"))

save(appendmsa4, file=paste0(root, 'FILEPATH'))

#-------------------------------------------------------
library(tidyverse)
library(data.table)
library(tidyr)#separate

load(paste0(root, 'FILEPATH'))

# Import common CBSA/state remainder population weights
load(paste0(main_dir,'populations/cbsa_pop_weight.rdata'))

#merge many to many to match SMART respondents with area.code and population weights where applicable
appendmsa <- appendmsa4 %>% 
  left_join(cbsa.popweight[!is.na(cbsa.popweight$cbsa),c('cbsa','popweight','area.code')], by = "cbsa")#5,524,202 to 6,319,662 obs

uniqueN(cbsa.popweight[!is.na(cbsa.popweight$popweight),]$cbsa)#926 CBSAs in total

uniqueN(appendmsa[is.na(appendmsa$popweight),]$cbsa)#840 -> 106
uniqueN(appendmsa[!is.na(appendmsa$popweight),]$cbsa)#82 cbsas with population weights -> 815

uniqueN(cbsa.popweight[!is.na(cbsa.popweight$popweight) & cbsa.popweight$popweight != 1,]$cbsa)#85 -> 99
uniqueN(appendmsa[!is.na(appendmsa$popweight) & appendmsa$popweight != 1,]$cbsa)#92
uniqueN(cbsa.popweight[,c('cbsa','state')])#1043

#observations not in a cross-state CBSA should have weight=1
appendmsa$popweight <- as.numeric(ifelse(is.na(appendmsa$popweight), 1, appendmsa$popweight))

#manually create area.code for general BRFSS data based on state fips (for merging with CBSA/area.code relationship file to get integer area.code)
appendmsa$area.code <- ifelse(is.na(appendmsa$stateabb1), appendmsa$state, appendmsa$area.code)

#match with area.codeint
#load mcnty/area.code relationship file
load(paste0(main_dir,"locations/area.code_reference_file.rdata"))

# #check that all area.codes have some data
data<-merge(appendmsa, unique(area[,c('area.codeint', 'area.code','common')]), by='area.code', all.y=T, all.x=T)

detach("package:tidyverse", unload=TRUE)
detach("package:tidycensus", unload=TRUE)
detach("package:tidyr", unload=TRUE)
detach("package:dplyr", unload=TRUE)

data[,c('msa.yn','MMSAtype','stateabb1','stateabb2','stateabb3','stateabb4')] <- NULL

#save merged data output 
save(data, file=paste0(main_dir, "survey_data/brfss_smart_merged.rdata"))

#checking population weights
datax <- data
load(paste0(main_dir, "survey_data/brfss_smart_merged.rdata"))
datax$popweight <- as.numeric(datax$popweight)
datax[which(datax$cbsa == 16980 &datax$id.smart==1587009) , c('area.code', 'area.codeint', 'sex','age','marital','race','edu','id.smart','popweight')]
data[which(data$cbsa == 16980 &data$id.smart==1587009) , c('area.code', 'area.codeint', 'sex','age','marital','race','edu','id.smart','popweight')]
sum(data$popweight)
levels(data$marital)


