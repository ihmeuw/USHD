####################################################################################################
## Description: Prep draws of misclassification ratios from Arias et al 2016. Reads in the ratios
##              and standard errors that have been manually extracted, as well as some other
##              necessary files (e.g. which counties are have "high coethnic concentration").
##
## Requires:    Extracted ratios/standard errors from the paper/
##
## Outputs:     prepped draws for each mcnty/race/sex/age/year
##
####################################################################################################

library(R.utils)
library(data.table)
library(TMB)
library(ggplot2)
library(parallel)
library(dplyr)
library(gridExtra)

load("FILEPATH")

empir_funcs <- list.files("functions/")
for(func in empir_funcs) {
  source(paste0("functions/", func))
}

# just use an example directory to get the ages we want via get_settings()
dir <- "FILEPATH"
get_settings(dir)
n.sims <- 1000
plot <- F # if you want to produce a few plots along the way for vetting purposes


# metadata
race_map <- data.table(race_name = c("nh_white","nh_black","nh_aian","nh_api","hispanic"), 
                       race = c(1,2,3,4,7))
sex_map <- data.table(sex_name = c("male","female","both"), sex = c(1,2,3))


### --- misclassification by age/sex
ratios_age_sex <- fread("FILEPATH") 
ratios_age_sex <- merge(ratios_age_sex, race_map, by="race_name",all.x=T)
if(nrow(ratios_age_sex[is.na(race)]) > 0) stop("Races did not merge on correctly")
ratios_age_sex <- merge(ratios_age_sex,sex_map,by="sex_name",all.x=T)
if(nrow(ratios_age_sex[is.na(sex)]) > 0) stop("Sexes did not merge on correctly")
ratios_age_sex[age_start == max(age_start), age_end := max(ages)]
ratios_age_sex <- ratios_age_sex[!is.na(age_end)]


draws_as <- data.table()
# generate draws
for(line in 1:nrow(ratios_age_sex)){
  r <- ratios_age_sex[line, ratio]
  s <- ratios_age_sex[line, se]
  sample.draws <- rnorm(n.sims, r, s)
  draws_as <- rbind(draws_as,data.table(age_start = ratios_age_sex[line,age_start], age_end = ratios_age_sex[line,age_end],
                                        race = ratios_age_sex[line, race], sex = ratios_age_sex[line, sex],
                                        sim = seq(1,n.sims), ratio = sample.draws))
}

if(plot) {
  ggplot(draws_as[sex == 1], aes(age_start, ratio, group=age_start+race, fill = as.factor(race))) + geom_boxplot() 
}


### --- misclassification by coethnic composition
chsda_areas <- fread("FILEPATH")
chsda_areas$V1 <- NULL
setnames(chsda_areas,"chsda","high_dens")
chsda_areas[,race := 3]
# hispanic
hisp_dens <- fread("FILEPATH")
hisp_dens$V1 <- NULL
hisp_dens[,race := 7]

all_dens <- rbind(chsda_areas,hisp_dens)
to_rep <- setdiff(races,all_dens$race)
for(r in to_rep){
  message(r)
  all_dens <- rbind(all_dens,data.table(area = unique(all_dens$area), high_dens = 1, race = r))
}

class_coethnic <- as.data.table(openxlsx::read.xlsx("FILEPATH"))

class_coethnic <- merge(class_coethnic, race_map, by="race_name",all.x=T)
if(nrow(class_coethnic[is.na(race)]) > 0) stop("Races did not merge on correctly")

class_coethnic$notes <- NULL
setnames(class_coethnic,"coethnic_conce","high_dens")


draws_cc <- data.table()
# generate draws
for(line in 1:nrow(class_coethnic)){
  r <- class_coethnic[line, ratio]
  s <- class_coethnic[line, se]
  sample.draws <- rnorm(n.sims, r, s)
  draws_cc <- rbind(draws_cc,data.table(race = class_coethnic[line, race], high_dens = class_coethnic[line, high_dens],
                                        sim = seq(1,n.sims), ratio = sample.draws))
}

if(plot) {
  ggplot(draws_cc[race %in% c(3,7)], aes(race, ratio, group=race+high_dens, fill = as.factor(high_dens))) + geom_boxplot()
}


## --- misclassification by region
regions <- as.data.table(openxlsx::read.xlsx("FILEPATH"))
regions <- merge(regions,unique(loc[,.(mcnty,state_name)]),by="state_name",all.y=T)
regions[is.na(state_name)]

ratios <- as.data.table(openxlsx::read.xlsx("FILEPATH"))
ratios <- merge(ratios,race_map,by="race_name",all.x=T)
ratios[is.na(race)]

# generate draws
draws_region <- data.table()
for(line in 1:nrow(ratios)){
  r <- ratios[line, ratio]
  s <- ratios[line, se]
  sample.draws <- rnorm(n.sims, r, s)
  draws_region <- rbind(draws_region,data.table(region = ratios[line, region],
                                          race = ratios[line, race],
                                          sim = seq(1,n.sims), ratio = sample.draws))
}

draws_region <- merge(draws_region,regions,by="region",all.x=T,allow.cartesian=T)
draws_region[is.na(mcnty)]
draws_region[,c("state_name") := NULL]

if(plot) {
  ggplot(draws_region[race == 7], aes(mcnty, ratio, group=mcnty)) + geom_boxplot()
  ggplot(draws_region[race == 3], aes(mcnty, ratio, group=mcnty)) + geom_boxplot()
  ggplot(draws_region[race == 4], aes(mcnty, ratio, group=mcnty)) + geom_boxplot()
}

######### ---------- combining draws
setnames(draws_cc,"ratio","ratio_cc")
setnames(draws_as,"ratio","ratio_as")

all_draws <- merge(draws_cc, draws_as, by=c("race","sim"), all = T, allow.cartesian = T)
all_draws <- all_draws[sex < 3] # we will merge the all-age, both-sex draws on later
# merge on counties
all_draws <- merge(all_draws[!((race %in% c(1,2,4) & high_dens == 0))], unique(class_coethnic[,.(sex, age_start, age_end, mcnty, race, high_dens)]), 
                   by = c("sex","age_start","age_end","race","high_dens"), allow.cartesian=T, all=T)
all_draws[is.na(ratio_cc)]
# and then merge on the ratios by region
all_draws <- merge(all_draws,setnames(draws_region,"ratio","ratio_r"), by=c("race","sim","mcnty"),all=T)
all_draws[is.na(ratio_r)]
all_draws[is.na(ratio_cc)]


## bring in the misclassification ratios for both-sex, all-age (by race)
ratios_age_sex_total <- fread("FILEPATH")[sex_name == "both" & age_start == 0 & is.na(age_end)]
ratios_age_sex_total <- merge(ratios_age_sex_total, race_map, by="race_name",all=T)
ratios_age_sex_total <- ratios_age_sex_total[,.(race,ratio,se)]

draws_total <- data.table()
# generate draws
for(line in 1:nrow(ratios_age_sex_total)){
  r <- ratios_age_sex_total[line, ratio]
  s <- ratios_age_sex_total[line, se]
  sample.draws <- rnorm(n.sims, r, s)
  draws_total <- rbind(draws_total,data.table(race = ratios_age_sex_total[line, race],
                                        sim = seq(1,n.sims), ratio = sample.draws))
}

## merge these onto the the other draws:
all_draws <- merge(all_draws, setnames(draws_total, "ratio", "ratio_total"), by=c("race","sim"),all=T)
all_draws[is.na(ratio_total)]
all_draws[is.na(ratio_cc)]

if(plot) {
  ggplot(draws_total,aes(race,ratio_total,group=race)) + geom_boxplot()
}

# the coethnic misclassification does not exist for races except Hispanic and NH AIAN, so just make these equal to the overall MC
# this has the effect of just multiplying everything by 1
all_draws[!(race %in% c(3,7)), ratio_cc := ratio_total]

all_draws[,mc_combined := ratio_total * (ratio_as/ratio_total) * (ratio_cc/ratio_total) * (ratio_r/ratio_total)]

## get ready for being read in within the pipeline
all_draws <- unique(all_draws[,.(race, sim, mcnty, sex, age_start, age_end, mc_combined)])
setnames(all_draws, "mc_combined","ratio")

if(plot) {
  ggplot(all_draws[sex == 1 & age_start == 55 & race == 3],aes(mcnty,ratio,group=mcnty)) + geom_boxplot()
  ggplot(all_draws[sex == 1 & age_start == 55 & race == 7],aes(mcnty,ratio,group=mcnty)) + geom_boxplot()
  ggplot(all_draws[sex == 1 & age_start == 55 & race == 4],aes(mcnty,ratio,group=mcnty)) + geom_boxplot()
}


age_merge <- data.table(age_start = c(rep(0,6),rep(25,4),rep(45,2),rep(55,2),rep(65,2),rep(75,3)), age = c(0,1,seq(5,85,5)))
expanded <- merge(all_draws, age_merge, by="age_start",allow.cartesian=T)
expanded[,c("age_start","age_end") := NULL]

setnames(expanded, "mcnty","area")


saveRDS(expanded, paste0("FILEPATH",n.sims,"_draws.rds"))
