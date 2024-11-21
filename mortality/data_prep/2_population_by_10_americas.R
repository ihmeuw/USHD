#####################################################################
# Description: Create population numbers by 10 Americas definitions. 
#####################################################################

(list = ls())

library(data.table)

library(lbd.loader, lib.loc = sprintf("FILEPATH/lbd.loader-%s.%s", 
                                      R.version$major, 
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))

ARCHIVE_DIR <- 'FILEPATH'

get_america_pop <- function(yr){
  pop <- get_population_data(population_name = 'pop_by_race_ethn_1977', year = as.list(yr))
  america_locs <- fread('FILEPATH/americas_definitions.csv')

  
  pop <- merge(pop, america_locs, 
               by = c('mcnty','race'),
               all.x = T)
  
  pop <- pop[,.(pop = sum(pop)) ,.(year,america,sex,age)]
  stopifnot(nrow(pop[!complete.cases(pop)]) == 0)
  stopifnot((current_mapping & all(1:10 %in% unique(pop$america))) | (!current_mapping & all(1:8 %in% unique(pop$america))))
  return(pop)
}

for(yr in 1990:2021){
  message(yr)
  pop <- get_america_pop(yr)
  # Use the same archive dir as deaths
  saveRDS(pop, file = paste0(ARCHIVE_DIR, 'pop_', yr, '.RDS'))
}
