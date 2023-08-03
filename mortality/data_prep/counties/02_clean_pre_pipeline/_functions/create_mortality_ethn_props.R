#################################################################
# Description: Create proportions of hispanic and non hispanic
#              via population files. 
#################################################################

rm(list = ls())
library(data.table)

##########################################################################################
# Grab population ratios
# calculate the proportion of Hispanic vs non-Hispanic

# location metadata
loc <- fread("FILEPATH")

# population by race/ethn
pop <- readRDS("FILEPATH")
pop <- pop[year >= 2000, ]
pop <- merge(pop, loc[, list(fips = cnty, mcnty, state)], by = "fips", all.x = T)
pop <- pop[, list(pop = sum(pop)), by = 'state,mcnty,year,sex,race,hisp']
# calculate the ratio of Hispanic to non-Hispanic for each race/mcnty/sex
pop_wide <- dcast.data.table(pop, state + mcnty + year + sex + race ~ hisp, value.var="pop")
## Create various ratios
pop_wide[,hisp_mcnty_year_sex_race := `1`/(`0`+`1`)] # Hispanic pop divided by all pop
pop_wide[,total_mcnty_year_sex_race := `0` + `1`]

for(yy in unique(pop_wide$year)) {
  pop_wide[year %in% c(yy - 1, yy, yy + 1), `0_pooled_temp` := sum(`0`), by = "mcnty,sex,race"]
  pop_wide[year %in% c(yy - 1, yy, yy + 1), `1_pooled_temp` := sum(`1`), by = "mcnty,sex,race"]
  
  pop_wide[year == yy, `0_pooled` := `0_pooled_temp`]
  pop_wide[year == yy, `1_pooled` := `1_pooled_temp`]
}

pop_wide[,c("0_pooled_temp","1_pooled_temp") := NULL]

# ratio by mcnty, 3 year pooling, sex, and race
pop_wide[,hisp_mcnty_pooledyear_sex_race := `1_pooled`/(`0_pooled`+`1_pooled`)]
pop_wide[,total_mcnty_pooledyear_sex_race := `1_pooled`+`0_pooled`]

# pool by mcnty, 9 year pooling, sex, and race
pop_wide[,c("0_pooled","1_pooled") := NULL]

for(yy in unique(pop_wide$year)) {
  pop_wide[year %in% c((yy - 4):(yy + 4)), `0_pooled_temp` := sum(`0`), by = "mcnty,sex,race"]
  pop_wide[year %in% c((yy - 4):(yy + 4)), `1_pooled_temp` := sum(`1`), by = "mcnty,sex,race"]
  
  pop_wide[year == yy, `0_pooled` := `0_pooled_temp`]
  pop_wide[year == yy, `1_pooled` := `1_pooled_temp`]
}

pop_wide[,c("0_pooled_temp","1_pooled_temp") := NULL]
pop_wide[,hisp_mcnty_pooled9year_sex_race := `1_pooled`/(`0_pooled`+`1_pooled`)]
pop_wide[,total_mcnty_pooled9year_sex_race := `1_pooled`+`0_pooled`]
pop_wide[,c("0_pooled","1_pooled") := NULL]

# we anticipate that time is important in these ratios, so instead of pooling by all years, we pool by state
pop_wide[,hisp_state_year_sex_race := sum(`1`)/(sum(`0`)+sum(`1`)), by=c("state","race","sex","year")]
pop_wide[,total_state_year_sex_race := sum(`0`)+sum(`1`), by=c("state","race","sex","year")]

pop_wide[, race := as.integer(as.factor(race))]

if(nrow(pop_wide[is.na(hisp_state_year_sex_race) & total_state_year_sex_race < 200]) > 0) stop("Keep pooling ratios")

pop_wide[,c("0","1") := NULL]
pop_long <- melt.data.table(pop_wide, id.vars = c("state","mcnty","year","sex","race"))
pop_long[,metric := ifelse(variable %like% "total", "sample_size", "ratio")]
pop_long$variable <- gsub("total_","",pop_long$variable)
pop_long$variable <- gsub("hisp_","",pop_long$variable)

pop_final <- dcast.data.table(pop_long, state + mcnty + year + sex + race + variable ~ metric,
                               value.var = "value")

pop_final[,final_ratio := -1]
pop_final[variable == "mcnty_year_sex_race" & sample_size >= 50, final_ratio := ratio]
pop_final[,final_ratio := max(final_ratio), by=c("mcnty","year","sex","race")]
pop_final[variable == "mcnty_pooledyear_sex_race" & sample_size >= 50 & final_ratio < 0,
           final_ratio := ratio]
pop_final[,final_ratio := max(final_ratio), by=c("mcnty","year","sex","race")]
pop_final[variable == "mcnty_pooled9year_sex_race" & sample_size >= 50 & final_ratio < 0,
           final_ratio := ratio]
pop_final[,final_ratio := max(final_ratio), by=c("mcnty","year","sex","race")]
pop_final[variable == "state_year_sex_race" & sample_size >= 50 & final_ratio < 0,
           final_ratio := ratio]
pop_final[,final_ratio := max(final_ratio), by=c("mcnty","year","sex","race")]

if(nrow(pop_final[final_ratio < 0]) > 0) stop("Some rows still have a ratio of -1")

#final ratio dt
pop_final <- unique(pop_final[ratio == final_ratio,.(mcnty,year,sex,race,ratio)])
pop_final[,state := NA]

if(!all(loc$mcnty %in% unique(pop_final$mcnty))) stop("Not all counties are present")


#create pop dt for states only and country only and bind on
pop_state <- copy(pop)
pop_state$race <- as.numeric(pop_state$race)
pop_state <- pop_state[,.(pop = sum(pop)), .(state,year,sex,race,hisp)]
pop_state <- merge(pop_state[hisp == 0,-'hisp'], pop_state[hisp == 1,-'hisp'],
                    by = c('state','year', 'sex', 'race'))
pop_state <- pop_state[,ratio := pop.y / (pop.x + pop.y)][,-c('pop.x','pop.y')]
pop_state[,mcnty := NA]

pop_country <- copy(pop)
pop_country$race <- as.numeric(pop_country$race)
pop_country <- pop_country[,.(pop = sum(pop)), .(year,sex,race,hisp)]
pop_country <- merge(pop_country[hisp == 0,-'hisp'], pop_country[hisp == 1,-'hisp'],
                      by = c('year', 'sex', 'race'))
pop_country <- pop_country[,ratio := pop.y / (pop.x + pop.y)][,-c('pop.x','pop.y')]
pop_country <- pop_country[,mcnty := NA][,state := 0]

cols <- c('state','mcnty','year','sex','race','ratio')
pop_final <- rbindlist(list(pop_final[,cols, with = F],
                             pop_state[,cols, with = F],
                             pop_country[,cols, with = F]))

write.csv(pop_final, "FILEPATH",
          row.names = F)
