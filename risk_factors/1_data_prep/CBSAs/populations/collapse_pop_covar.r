library(data.table)
library(zoo)
library(car)

rm(list=ls())

root <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
main_dir <- paste0(root, "FILEPATH")
cnty_dir <- paste0(root, "FILEPATH")
parent_dir <- paste0(root, "FILEPATH")

#load mcnty/area.code relationship file
load(paste0(main_dir,"FILEPATH"))

#subset to variables needed to collapse
area<-area[, c('mcnty','area.codeint')]

#load/collapse covar file
covar_file<-readRDS(paste0(parent_dir, 'FILEPATH'))#121290 obs and 28 vars

covar_file_cbsa<-merge(covar_file, area, by='mcnty') 

#calculate mean pc by CBSA for all variables
covar_file<-covar_file_cbsa[, lapply(.SD,mean), by=.(year,area.codeint)]#4329=111 areas x 39 years
covar_file$mcnty <- NULL
saveRDS(covar_file, file=paste0(main_dir, 'populations/collapsed_covariates.rds'))
#-------------------------------------------------------------------------------------------------------------------