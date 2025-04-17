### Final raking code
### Goal: produce post-stratification frame by age/race/sex/edu/mcnty/year
###       by raking IPUMS and survey-based national joint distribution to mcnty margins

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

library(data.table)
## load database central functions
library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                      R.version$major,
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))

joint_dir <- "FILEPATH"

#import mcnty-state reference file ######################
load("FILEPATH")

##### Raking function #####################################
rake <- function(data, agg_var, constant_vars, replace_value = F) {
  
  # 2. sum over weighted population to get a crude total pop
  data[, sum_weighted_value := sum(value), by = c(constant_vars, agg_var)] #FIXME adopt a similar weighting for geographic aggregation: https://stash.ihme.washington.edu/projects/UC/repos/mortality/browse/sae_models/functions/rake.r#28,31
  
  # 3. generate raking weights by dividing the upper-level (e.g., state) pop over the crude total pop
  data[, raking_weight := get(paste0(agg_var, "_value")) / sum_weighted_value]
  
  # if all county and state level counts are zero (e.g. when you rake by geography for neonatal, age >1),
  # raking_weight will be NA. Convert to zeros.
  data[is.na(raking_weight)|is.infinite(raking_weight), raking_weight := 0]
  
  # 4. generate raked pop estimates by multiplying the original pop by the raking weights
  data[, raked_value :=  value * raking_weight]
  
  if (replace_value) {
    data[, value := raked_value]
    data[, raked_value := NULL]
  }
  
  data[, c("sum_weighted_value", "raking_weight") := NULL]
  
  return(data)
}

# ##### IPUMS ###############################################
#load starting mcnty joint distribution #####################################################
message('load data')

load(paste0(joint_dir, date_time_stamp, '/marginal_dist_nophone.rdata'))

message(paste0('marginal distribution version: ', date_time_stamp))

# Create list by year
jointipums_y <- vector(mode = "list", length = length(years))

print(paste0('Max iterations set to: ', maxit))

for (yr in years){#loop over years because square frame is too large
  print(yr)
  data <- copy(marginal[year==yr])
  data[, value := pop]
  data[, begin_value := value] # save original (beginning) population as begin_pop; this is used in the loop to display incremental changes
  data[, value_change := 1] # initialize pop_change so we can keep track
  data[, max_state := 1]
  data[, max_marital := 1]
  data[, max_edu := 1]
  data[, max_race := 1]
  
  # End conditions:
  # 1) The max change between raking iterations is less than tol.
  # 2) We've hit max iterations and should fail
  iter <- 1
  
  while (unique(data$max_marital) > 10^-5 | unique(data$max_edu) > 10^-5 | unique(data$max_race) > 10^-5 | unique(data$max_state) > 10^-5){ 
    cat(paste("\nRaking, iteration", iter, "\n"))
    
    # # Rake each dimension
    # First, rake across national joint distribution margins
    cat(paste0("Raking across age/sex/race/edu/marital/state"))
    data <- rake(data = data, agg_var = 'state', constant_vars = c('age','sex','year','race','edu','marital'), replace_value = FALSE)

    # Check the max difference between the starting pop and raked pop to determine stability
    data[, value_change := abs(raked_value - begin_value)]
    cat(paste("...max difference:", max(data$value_change, na.rm = T), "\n"))
    data[, value_change_rel := abs((raked_value - begin_value)/begin_value)*100]
    cat(paste("...max RELATIVE difference:", max(data$value_change_rel, na.rm = T), "%\n"))
    setnames(data, 'value', paste0('value_',iter,'_state'))
    data[, c("value", "begin_value") := raked_value]
    data[, raked_value := NULL]
    data$max_state <- max(data$value_change, na.rm = T)

    # Second, across by marital/age/sex/mcnty
    cat(paste0("Raking across marital/age/sex/mcnty"))
    data <- rake(data = data, agg_var = 'marital', constant_vars = c('age','sex','year','mcnty'), replace_value = FALSE)

    # Check the max difference between the starting pop and raked pop to determine stability
    data[, value_change := abs(raked_value - begin_value)]
    cat(paste("...max difference:", max(data$value_change, na.rm = T), "\n"))
    data[, value_change_rel := abs((raked_value - begin_value)/begin_value)*100]
    cat(paste("...max RELATIVE difference:", max(data$value_change_rel, na.rm = T), "%\n"))
    setnames(data, 'value', paste0('value_',iter,'_mar'))
    data[, c("value", "begin_value") := raked_value]
    data[, raked_value := NULL]
    data$max_marital <- max(data$value_change, na.rm = T)

    # Third, across by edu/age/sex/mcnty
    cat(paste0("Raking across edu/age/sex/mcnty"))
    data <- rake(data = data, agg_var = 'edu', constant_vars = c('age','sex','year','mcnty'), replace_value = FALSE)

    # Check the max difference between the starting pop and raked pop to determine stability
    data[, value_change := abs(raked_value - begin_value)]
    cat(paste("...max difference:", max(data$value_change, na.rm = T), "\n"))
    data[, value_change_rel := abs((raked_value - begin_value)/begin_value)*100]
    cat(paste("...max RELATIVE difference:", max(data$value_change_rel, na.rm = T), "%\n"))
    setnames(data, 'value', paste0('value_',iter,'_edu'))
    data[, c("value", "begin_value") := raked_value]
    data[, raked_value := NULL]
    data$max_edu <- max(data$value_change, na.rm = T)

    # Lastly, rake across race/age/sex/mcnty
    cat(paste0("Raking across race/age/sex/mcnty"))
    data <- rake(data = data, agg_var = 'race', constant_vars = c('age','sex','year','mcnty'), replace_value = FALSE)
    
    # Check the max difference between the starting pop and raked pop to determine stability
    data[, value_change := abs(raked_value - begin_value)]
    cat(paste("...max difference:", max(data$value_change, na.rm = T), "\n"))
    data[, value_change_rel := abs((raked_value - begin_value)/begin_value)*100]
    cat(paste("...max RELATIVE difference:", max(data$value_change_rel, na.rm = T), "%\n"))
    setnames(data, 'value', paste0('value_',iter,'_rac'))
    data[, c("value", "begin_value") := raked_value]
    data[, raked_value := NULL]
    data$max_race <- max(data$value_change, na.rm = T)
    
    # update the iteration number; stop after 500 (we can reasonably assume that it's not going to work at that point)
    iter <- iter + 1
    if (iter > maxit) {
      break(print("Raking has iterated %s times. \nCheck to ensure your file is convergence compliant. \nExecution halted."))
    }
  }
  data$iter <- iter
  data$year <- yr  # add year indicator to created joint distribution
  
  # keep last two columns of each margin raking for convergence check 
  setnames(data,c(paste0('value_',iter-1,'_edu'),paste0('value_',iter-2,'_edu'),
                  paste0('value_',iter-1,'_mar'),paste0('value_',iter-2,'_mar'),
                  paste0('value_',iter-1,'_rac'),paste0('value_',iter-2,'_rac'),
                paste0('value_',iter-1,'_state'),paste0('value_',iter-2,'_state')),
           c('value_last_edu','value_prelast_edu','value_last_mar','value_prelast_mar',
             'value_last_rac','value_prelast_rac','value_last_state','value_prelast_state'))#
  
  # subset final dataset 
  jointipums_y[[yr-min(years)+1]] <- data[,c('marital','edu','race','age','sex','year','pop','race_value','edu_value','state_value',
                                             'marital_value','begin_value','value_change',
                                             'max_state','max_marital','max_edu','max_race',
                                             'value_last_edu','value_prelast_edu','value_last_mar','value_prelast_mar',
                                             'value_last_rac','value_prelast_rac','value_last_state','value_prelast_state',
                                             'mcnty','iter','value','state','state_name')]
  
}

# rbind datasets for all years
ps_frame <- setDT(rbindlist(jointipums_y))
ps_frame[is.na(value), value := 0]

saveRDS(ps_frame, file=paste0(joint_dir, date_time_stamp, '/ps_frame_vetting.rds'))

ps_frame_clean <- ps_frame[,c('marital','edu','race','age','sex','year','mcnty','value','state','state_name')]

if (smooth_ps_weights==F){
  saveRDS(ps_frame_clean, file=paste0(joint_dir, date_time_stamp, '/ps_frame.rds'))
} else if (smooth_ps_weights==T){
  saveRDS(ps_frame_clean, file=paste0(joint_dir, date_time_stamp, '/ps_frame_clean.rds'))
}


print(date_time_stamp)
message('raking completed')
