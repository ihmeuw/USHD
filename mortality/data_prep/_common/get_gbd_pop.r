####################################################################################################
## Description: Pull GBD population estimates
## Outputs:     A function `get_gbd_pop()` that is used to get GBD population estimates for the
##              specified ages, location ids, years, and sexes. Requires no arguments; optional
##              return_all [boolean] if extra neonatal age detail is desired.
##              
####################################################################################################

library(RMySQL)
library(data.table)

get_gbd_pop <- function(return_all=F){
  process_version_map_id <- 110  # this should be updated as needed to ensure we use the same GBD population in the analysis
  source("FILEPATH/get_population.R")
  source("FILEPATH/get_ids.R")
  
  ## Get locations
  loc_dir <- "FILEPATH"
  loc <- fread(loc_dir)
  state_loc <- loc[fips<60, list(location_id, state=fips)]
  state_loc <- rbind(state_loc, list(102, 0))
  
  ## Get population - 2017 estimates for 1980-2017
  print("before get_populations")
  ages <- c(2, 3, 4, # EN, LN, PN
            seq(5, 20, 1), # 1-4, 5-9, 10-14, ..., 75-79
            30, 31, 32, 235) # 80-84, 85-89, 90-94, 95+
  gbd_pop <- get_population(age_group_id = ages, location_id = unique(state_loc$location_id), year_id = 1980:2017, 
                            sex_id = c(1,2,3), location_set_id = 2, decomp_step = "iterative", gbd_round_id = 5, status = "best")
  print("after get_populations")
  
  ## Get age_group_id and age_group_name
  age_metadata <- get_ids("age_group")
  age_metadata <- age_metadata[age_group_id %in% ages]
  age_metadata[, age:= as.character(suppressWarnings(as.numeric(substr(age_metadata[,age_group_name], 0,2))))]
  age_metadata[age_group_name == "Early Neonatal", age:="EN"]
  age_metadata[age_group_name == "Late Neonatal", age:="LN"]
  age_metadata[age_group_name == "Post Neonatal", age:="PN"]
  gbd_pop <- merge(gbd_pop, age_metadata, by = "age_group_id")
  
  #merge on state names, 
  gbd_pop <- merge(gbd_pop, state_loc, by="location_id", all.x=T)
  gbd_pop <- gbd_pop[,list(state, year=year_id, sex=sex_id, age, location_id, age_group_id, pop=population)]

  if (return_all){ # in this case we want to return the population as is (with neonatal age groups)
    gbd_pop <- gbd_pop[, list(state, year, sex, age, location_id, age_group_id, pop)]
    setkeyv(gbd_pop, c("state", "year", "sex", "age"))
    return(unique(gbd_pop))
  }else { # we can collapse the neonatal age groups to under 1 and return only essential columns
    gbd_pop <- gbd_pop[sex != 3 & location_id != 102]
    gbd_pop[grep("^[ELP]N$", age), age := "0"]
    gbd_pop[, age := as.numeric(age)]
    gbd_pop <- gbd_pop[, list(pop = sum(pop)), keyby=c("state", "year", "sex", "age")]
    return(gbd_pop)
  }
}
