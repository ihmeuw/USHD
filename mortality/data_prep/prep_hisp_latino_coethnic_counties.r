## Purpose: create draws of misclassification ratios for the Hispanic race by coethnic concentration
# Arias et al calculate this by taking the top 50% of counties with the highest number of total deaths from 1999-2011
# So, I will do this for 2000-2017 because that is the data I have, and then I can use their classification ratios

library(R.utils)
library(data.table)
library(TMB)
library(ggplot2)
library(parallel)
library(dplyr)
library(gridExtra)

empir_funcs <- list.files("functions/")
for(func in empir_funcs) {
  source(paste0("functions/", func))
}
source("FILEPATH")

loc <- fread("FILEPATH")

# just choose any directory that has the correct deaths and populations files in the settings csv
dir <- "FILEPATH"

get_settings(dir)

## Load and prep inputs ----------------------------------------------------------------------------
# combine deaths and population
deaths <- readRDS(deaths_file)[cause_id == 294]
setnames(deaths, area_var, "area")
deaths <- deaths[year %in% c(1999:2011) & age %in% ages, list(deaths = sum(deaths)), by = "area,year,sex,race,age"]

pop <- readRDS(pop_file)
setnames(pop, area_var, "area")
if (!by_race) pop[, race := 9]
pop <- pop[year %in% c(1999:2011) & age %in% ages, list(pop = sum(pop)), by = "area,year,sex,race,age"]

# keep this merge because it includes counties that don't have any deaths for a particular race/age/etc
data <- merge(pop, deaths, by = c("area", "year", "sex", "race", "age"), all = T)
data[is.na(deaths), deaths := 0]
rm(pop, deaths); gc()

## aggregate deaths from 2000-2017 across sexes and ages
data_hisp <- data[race == 7]
data_hisp[,total_deaths := sum(deaths), by=c("area")]
data_hisp <- unique(data_hisp[,.(area,total_deaths)])

# Now get the total number of deaths
data_hisp[,total_natl_deaths := sum(total_deaths)]
data_hisp <- merge(data_hisp, loc[,.(mcnty,cnty_name,state_name)],by.x="area",by.y="mcnty")

setorderv(data_hisp,"total_deaths",-1)

# add until we get 50% of deaths
data_hisp[,half_deaths := total_natl_deaths/2]
data_hisp[,cumulative := cumsum(total_deaths)]
data_hisp[,index := .I]
last_dense_index <- data_hisp[cumulative > half_deaths][1,index]

data_hisp[,high_dens := ifelse(index <= last_dense_index, 1, 0)]

write.csv(data_hisp[,.(area,high_dens)], "FILEPATH")


## Look at the overlap in counties
data_hisp <- merge(data_hisp[,.(area,high_dens)], loc[,.(mcnty,cnty_name,state_name)],by.x="area",by.y="mcnty")

unique(data_hisp[high_dens == 1][,.(cnty_name,state_name)])


write.csv(data_hisp[high_dens == 1][,.(cnty_name,state_name)], "FILEPATH")
