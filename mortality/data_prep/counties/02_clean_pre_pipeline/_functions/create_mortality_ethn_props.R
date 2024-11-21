#################################################################
# Description: Create proportions of hispanic and non hispanic
#              via population files. 
#################################################################

rm(list = ls())
library(data.table)

##########################################################################################
# Grab population ratios
# calculate the proportion of Hispanic vs non-Hispanic
loc <- fread("FILEPATH")

pop_dir <- "FILEPATH"
pop2 <- readRDS("FILEPATH")
pop2 <- pop2[year >= 2000, ]
pop2 <- merge(pop2, loc[, list(fips = cnty, mcnty, state)], by = "fips", all.x = T)
pop2 <- pop2[, list(pop = sum(pop)), by = 'state,mcnty,year,sex,race,hisp']
# calculate the ratio of Hispanic to non-Hispanic for each race/mcnty/sex
pop2_wide <- dcast.data.table(pop2, state + mcnty + year + sex + race ~ hisp, value.var="pop")
## Create various ratios
pop2_wide[,hisp_mcnty_year_sex_race := `1`/(`0`+`1`)] # Hispanic pop divided by all pop
pop2_wide[,total_mcnty_year_sex_race := `0` + `1`]

for(yy in unique(pop2_wide$year)) {
  pop2_wide[year %in% c(yy - 1, yy, yy + 1), `0_pooled_temp` := sum(`0`), by = "mcnty,sex,race"]
  pop2_wide[year %in% c(yy - 1, yy, yy + 1), `1_pooled_temp` := sum(`1`), by = "mcnty,sex,race"]
  
  pop2_wide[year == yy, `0_pooled` := `0_pooled_temp`]
  pop2_wide[year == yy, `1_pooled` := `1_pooled_temp`]
}

pop2_wide[,c("0_pooled_temp","1_pooled_temp") := NULL]

# ratio by mcnty, 3 year pooling, sex, and race
pop2_wide[,hisp_mcnty_pooledyear_sex_race := `1_pooled`/(`0_pooled`+`1_pooled`)]
pop2_wide[,total_mcnty_pooledyear_sex_race := `1_pooled`+`0_pooled`]

# pool by mcnty, 9 year pooling, sex, and race
pop2_wide[,c("0_pooled","1_pooled") := NULL]

for(yy in unique(pop2_wide$year)) {
  pop2_wide[year %in% c((yy - 4):(yy + 4)), `0_pooled_temp` := sum(`0`), by = "mcnty,sex,race"]
  pop2_wide[year %in% c((yy - 4):(yy + 4)), `1_pooled_temp` := sum(`1`), by = "mcnty,sex,race"]
  
  pop2_wide[year == yy, `0_pooled` := `0_pooled_temp`]
  pop2_wide[year == yy, `1_pooled` := `1_pooled_temp`]
}

pop2_wide[,c("0_pooled_temp","1_pooled_temp") := NULL]
pop2_wide[,hisp_mcnty_pooled9year_sex_race := `1_pooled`/(`0_pooled`+`1_pooled`)]
pop2_wide[,total_mcnty_pooled9year_sex_race := `1_pooled`+`0_pooled`]
pop2_wide[,c("0_pooled","1_pooled") := NULL]

## pool by state
pop2_wide[,hisp_state_year_sex_race := sum(`1`)/(sum(`0`)+sum(`1`)), by=c("state","race","sex","year")]
pop2_wide[,total_state_year_sex_race := sum(`0`)+sum(`1`), by=c("state","race","sex","year")]

pop2_wide[, race := as.integer(as.factor(race))]

if(nrow(pop2_wide[is.na(hisp_state_year_sex_race) & total_state_year_sex_race < 200]) > 0) stop("Keep pooling ratios")

pop2_wide[,c("0","1") := NULL]
pop2_long <- melt.data.table(pop2_wide, id.vars = c("state","mcnty","year","sex","race"))
pop2_long[,metric := ifelse(variable %like% "total", "sample_size", "ratio")]
pop2_long$variable <- gsub("total_","",pop2_long$variable)
pop2_long$variable <- gsub("hisp_","",pop2_long$variable)

pop2_final <- dcast.data.table(pop2_long, state + mcnty + year + sex + race + variable ~ metric,
                               value.var = "value")

pop2_final[,final_ratio := -1]
pop2_final[variable == "mcnty_year_sex_race" & sample_size >= 50, final_ratio := ratio]
pop2_final[,final_ratio := max(final_ratio), by=c("mcnty","year","sex","race")]
pop2_final[variable == "mcnty_pooledyear_sex_race" & sample_size >= 50 & final_ratio < 0,
           final_ratio := ratio]
pop2_final[,final_ratio := max(final_ratio), by=c("mcnty","year","sex","race")]
pop2_final[variable == "mcnty_pooled9year_sex_race" & sample_size >= 50 & final_ratio < 0,
           final_ratio := ratio]
pop2_final[,final_ratio := max(final_ratio), by=c("mcnty","year","sex","race")]
pop2_final[variable == "state_year_sex_race" & sample_size >= 50 & final_ratio < 0,
           final_ratio := ratio]
pop2_final[,final_ratio := max(final_ratio), by=c("mcnty","year","sex","race")]

if(nrow(pop2_final[final_ratio < 0]) > 0) stop("Some rows still have a ratio of -1")

#final ratio dt
pop2_final <- unique(pop2_final[ratio == final_ratio,.(mcnty,year,sex,race,ratio)])
pop2_final[,state := NA]

if(!all(loc$mcnty %in% unique(pop2_final$mcnty))) stop("Not all counties are present")


#create pop2 dt for states only and country only and bind on
pop2_state <- copy(pop2)
pop2_state$race <- as.numeric(pop2_state$race)
pop2_state <- pop2_state[,.(pop = sum(pop)), .(state,year,sex,race,hisp)]
pop2_state <- merge(pop2_state[hisp == 0,-'hisp'], pop2_state[hisp == 1,-'hisp'],
                    by = c('state','year', 'sex', 'race'))
pop2_state <- pop2_state[,ratio := pop.y / (pop.x + pop.y)][,-c('pop.x','pop.y')]
pop2_state[,mcnty := NA]

pop2_country <- copy(pop2)
pop2_country$race <- as.numeric(pop2_country$race)
pop2_country <- pop2_country[,.(pop = sum(pop)), .(year,sex,race,hisp)]
pop2_country <- merge(pop2_country[hisp == 0,-'hisp'], pop2_country[hisp == 1,-'hisp'],
                      by = c('year', 'sex', 'race'))
pop2_country <- pop2_country[,ratio := pop.y / (pop.x + pop.y)][,-c('pop.x','pop.y')]
pop2_country <- pop2_country[,mcnty := NA][,state := 0]

cols <- c('state','mcnty','year','sex','race','ratio')
pop2_final <- rbindlist(list(pop2_final[,cols, with = F],
                             pop2_state[,cols, with = F],
                             pop2_country[,cols, with = F]))

write.csv(pop2_final, "FILEPATH",
          row.names = F)
