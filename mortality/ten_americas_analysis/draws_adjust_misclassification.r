# Load required libraries
pacman::p_load(data.table) 

###################################################
# Arguments
args <- commandArgs(trailingOnly = TRUE)
print(args)
indir <- args[1]
outdir <- args[2]
america_def_file <- args[3]
this_year <- as.integer(args[4])
resub <- as.logical(args[5]) 


mis_ratio_file <- "[MISCLASSIFICATION_FILENAME]"


##################################################
if (resub | !file.exists(paste0(outdir, "/10_americas_adjusted_deaths_draws_", this_year, ".rds"))) {
  
  message(this_year)
  
  # Load data --------------------------------------------
  # Load county info
  load("[COUNTY_FILENAME]")
  loc <- unique(loc[,.(mcnty)])
  
  # Load 10 Americas definition
  americas <- data.table(read.csv(america_def_file))
  
  # Load misclassification ratios
  mis_ratio <- readRDS(mis_ratio_file)
  mis_ratio <- mis_ratio[,.(mcnty = area, race, sex, age, draw = sim, ratio)]
  
  # Load county-level deaths
  this_death <- readRDS(paste0(indir,"/[DEATH_FILENAME]"))
  this_death <- this_death[, .(mcnty, race, year, sex, age, deaths)]
  
  
  # Calculate deaths adjusted by misclassification -----------------------------------------------

  # Make deaths square to fill in 0s
  setkey(this_death, mcnty, race, year, sex, age)
  this_death <- this_death[CJ(mcnty = loc[,mcnty], race, year, sex, age, unique = T), ]
  this_death[is.na(deaths), deaths := 0]
  
  # Aggregate races
  this_death[, deaths_all_races := sum(deaths), by = 'mcnty,year,sex,age']
  
  # Merge with 10 Americas definition
  this_death <- merge(this_death, americas, by = c("mcnty", "race"), all = T)
  stopifnot(nrow(this_death[is.na(deaths) | is.na(america)]) == 0)
  
  # Merge with misclassification ratios
  this_data <- merge(this_death, mis_ratio, by = c("mcnty", "race", "sex", "age"), all = T, allow.cartesian = T)
  stopifnot(nrow(this_data[is.na(deaths) | is.na(ratio)]) == 0)
  
  # Calculate adjusted deaths
  this_data[, deaths_ad := deaths * ratio]
  
  # Aggregate races
  this_data[, deaths_ad_all_races := sum(deaths_ad), by = 'mcnty,year,sex,age,draw']
  
  # Scale to ensure the all-race deaths is the same
  this_data[, deaths_ad_ad := deaths_all_races/deaths_ad_all_races*deaths_ad]
  this_data[deaths_all_races == 0, deaths_ad_ad := 0]
  stopifnot(nrow(this_data[is.na(deaths_ad_ad)]) == 0)
  
  # Aggregate to america-level
  this_data_america <- this_data[, list(deaths = sum(deaths_ad_ad)), by = 'america,year,sex,age,draw']
  
  # Save
  message(paste0("Saving ", this_year))
  saveRDS(this_data_america, paste0(outdir, "/10_americas_adjusted_deaths_draws_", this_year, ".rds"))
  
}  

