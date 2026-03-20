risk_repo <- paste0("FILEPATH")
library(data.table)
library(fitdistrplus)
library(nloptr)
library(magrittr)
library(GoFKernel, lib = "FILEPATH")
library(dfoptim, lib = "FILEPATH")
source(paste0("FILEPATH/ushd_pdf_families.R")) # updated functions to solve for dist parameters
source(paste0(risk_repo, "FILEPATH/1_parallel_helper_functions.R"))
library(stats)
library(lubridate)
library(spatstat.geom)

nf_repo <- paste0("FILEPATH")
funcs <- list.files(paste0(nf_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(nf_repo, "/functions/", func)))
}

# ----- Set up parameters

debug = 0

if(debug == 1 | interactive()){
  param_map_filepath = "FILEPATH"
  settings_loc <- "FILEPATH"
  task_id = 15
  
  max.time = 168 # set large so that the function doesn't time out during debugging
  # these are settings that can theoretically be changed, but work so much better for one option, we've removed it from the launch script
  
  optim_type = "sbplx" # either sbplx, nmkb, or nlminb
  maxeval = 10 # This can be high. M1 is fast. This is really meant to limit m2
  delete_iteration_csvs = TRUE
} else{
  args <- commandArgs(trailingOnly = TRUE)
  param_map_filepath <- args[1]
  settings_loc <- args[2]
  task_id <- ifelse(Sys.getenv('SLURM_ARRAY_TASK_ID') != '', as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID')), NA)
  
  # these are settings that can theoretically be changed, but work so much better for one option, we've removed it from the launch script
  
  optim_type = "sbplx" # either sbplx, nmkb, or nlminb
  maxeval = 5000 # This can be high. M1 is fast. This is really meant to limit m2
  delete_iteration_csvs = TRUE
  max.time = 168 # set large so that the function doesn't time out 
}

get_settings(settings_loc)

param_map <- fread(param_map_filepath)

initial_condition = param_map[task_id, initial_condition]
strategy = param_map[task_id, strategy]
data_filepath = param_map[task_id, data_filepath]
low_tail = param_map[task_id, low_tail]
middle_tail = param_map[task_id, middle_tail]
high_tail = param_map[task_id, high_tail]
low_weight = param_map[task_id, low_weight]
medium_weight = param_map[task_id, medium_weight]
high_weight = param_map[task_id, high_weight]
launch.date = param_map[task_id, launch.date]
out_dir = paste0(weight_root, param_map[task_id, launch_paths_expanded], "/")

if(!exists("by_demo")) by_demo <- FALSE
if(by_demo){ # from settings: run by separate demographics?
  demo_val_index <- param_map[task_id, demo_val_index]
  demo_vals <- fread(paste0(weight_root, launch.date, "/demo_vals_map.csv"), fill=TRUE) # fill argument needed when only reading in one column
  demo_vals <- demo_vals[demo_val_index]
  print(demo_vals)
}

#getting the time that the entire optimization starts, converting to underscores because of csv's
start.time <<- gsub("-", "_", Sys.time())

# ----- Function being optimized

min_KS_m2 <- function(allData, weights, initial_condition, all_CDF_Data_Params) { 
  allweightfits <- lapply(unique(allData$nid_loc_yr_index), function(nid_loc_yr_i){
    Data = allData[nid_loc_yr_index == nid_loc_yr_i]$data
    
    # Requirement of central code "FILEPATH" that XMAX be set as a global variable
    # e.g. mgumbel$$mv2par only takes two parameters (mn, vr), but is expecting XMAX
    # function(mn, vr) {
    #   list(alpha = XMAX - mn - EULERS_CONSTANT * sqrt(vr) * sqrt(6)/pi, scale = sqrt(vr) * sqrt(6)/pi)
    # }
    # TO-DO: Consider not using central code, or figure out with Bobby why XMAX should be a global variable
    XMAX <<- 50# max(Data, na.rm = T)
    
    cdf.index <- which(unique(allData$nid_loc_yr_index) == nid_loc_yr_i)
    CDF_Data_Params <- all_CDF_Data_Params[[cdf.index]]
  
    # I think this function finds distributions for which the parameters can't be solved
    #   for the provided mean/var, so it sets the weight on this class to zero
    weights <- find_bad_fits(weights, distlist, CDF_Data_Params) 
    # this sometimes prints the message
    # "Error in integrate(xinvweibull, 0, Inf, shape = x[1], scale = x[2], rel.tol = 0.1,  : 
    # the integral is probably divergent"
    # Internally, we set the weight to zero for invweibull when this is the case (so I'm not concerned about this message),
    #  but I can't figure out how to suppress the message b/c is produced internally with the 
    # function distlist$invweibull$mv2par, which has a try statement.
    
    weights <- rescale_to_one(weights)  
    names(weights) <- names(distlist)
    
    den <- suppressMessages({create_pdf(distlist, weights, CDF_Data_Params)})
    
    CDF_Data_Params <- integrate_pdf(den, CDF_Data_Params, scale = T)  
    
    weightfit <- goodness_of_fit_calc(Data = Data, 
                                      allData = allData, 
                                      CDF_Data_Params = CDF_Data_Params, 
                                      weights = weights, 
                                      option = option, 
                                      low_threshold = low_threshold, 
                                      medium_threshold = medium_threshold, 
                                      high_threshold =high_threshold, 
                                      low_tail = low_tail, 
                                      middle_tail = middle_tail, 
                                      high_tail = high_tail, 
                                      low_weight = low_weight, 
                                      medium_weight = medium_weight, 
                                      high_weight = high_weight)
    # creates a CSV saving the score information above for the given iteration -- specific to that nid_loc_yr
    iteration_weights_ks_tracking(out_dir, optim_type, initial_condition, weightfit, nid_loc_yr_i)
    return(weightfit)
  })
  
  KSs <- sapply(allweightfits, function(weightfit) return(weightfit$ks) ) 
  
  # take the sum of the score for each nid_loc_yr
  sum_KSs <- combine_fit(KSs) 
  
  # for every nid_loc_yr in the data, update the files tracking performance with the score summed across all nid_loc_yr (each treated equally)
  global_ks_tracking(allData, sum_KSs)
  
  print(sum_KSs)
  
  #converting that original start time back to the dashes for math purposes
  comp.start.time <- gsub("_", "-", start.time)
  
  #getting current time in optimization
  current.time <- Sys.time()
  
  #converting both to the final math format for dates
  comp.start.time <- ymd_hms(comp.start.time)
  current.time <- ymd_hms(current.time)
  
  runtime <- comp.start.time %--% current.time
  runtime <- as.numeric(as.duration(runtime) /dhours(1))
  
  if (runtime > max.time) {
    message("out of runtime!")
    return()
  }
  return(sum_KSs)
} 

# ----- Set up directories, seed, distribution list, and data

allData = readRDS(data_filepath)
if(by_demo){
  allData <- merge(allData, demo_vals, by = names(demo_vals), all.y = T)
}

## changing data to z score space when needed like envir_lead_bone, also offset by 10
offset = 0 # offset is a vestige of the CGF code -- not needed for USHD

# ultimately, we want the distributions :
# gamma,	invgamma,	llogis,	gumbel,	weibull,	lnorm,	norm,	mgamma,	mgumbel, and betasr
distlist <- c(classA, classM) 
if(exists("test_additional_dist") && test_additional_dist){
  if(!("invweibull" %in% names(distlist))){ # skip if invweibull and beta are already in distlist --- this will happen if we load the USHD PDF families version instead of the one in the ensemble repo
    distlist <- append(distlist,  list(invweibull = invweibullOBJ)) #, glnorm = glnormOBJ))   glnorm does not have mv2par defined  
  }
  if(!"betasr" %in% names(distlist)){
    distlist <- append(distlist,  list(betasr = betasrOBJ))
  }
  
} else{
  if("invweibull" %in% names(distlist)) {
    distlist$invweibull <- NULL # if we load distribution classes from the USHD pdf_families script, we automatically get invweibull
    distlist$betasr <- NULL
  }
}

# remove exp distribution b/c we cannot optimize variance (sd = mean)
distlist$exp <- NULL
# remove glnorm
distlist$glnorm <- NULL

if(me_type %in% c("bw", "ga")){
  distlist$invgamma <- NULL
  distlist$llogis <- NULL
}

set.seed(initial_condition)

write_zero_csv(out_dir, optim_type, initial_condition)
message(paste(initial_condition, me_type, optim_type, strategy, delete_iteration_csvs, out_dir))

allData[, study.index := 1:.N, by = nid_loc_yr_index]
no.of.loc.years <- length(unique(allData$nid_loc_yr_index))
length(unique(allData$nid_index))

# ----- Prepping optimization

weights <- select_initial_weights(distlist, initial_condition)

all_CDF_Data_Params <- lapply(unique(allData$nid_loc_yr_index), function(nid_loc_yr_i) {
  
  Data = allData[nid_loc_yr_index == nid_loc_yr_i]$data
  svy_wt = allData[nid_loc_yr_index == nid_loc_yr_i]$svy_wt
  
  XMAX <<- 50
  
  all.cdfs <- create_CDF(Data, nid_loc_yr_i, svy_wt = svy_wt)
  
  return(all.cdfs)
})

#creates a 1 meta-list with # of component lists = to the number of unique nid loc years
#each of these sub-lists has 8 elements
#total list components = 8 * number of NIDs

# ---- Running Optimization

if(optim_type == "sbplx"){
  
  optim_results <- try(sbplx(x0 = weights, 
                             fn = min_KS_m2, 
                             allData = allData, 
                             initial_condition = initial_condition,
                             all_CDF_Data_Params = all_CDF_Data_Params,
                             lower=rep(0, length(weights)), 
                             upper = rep(1, length(weights)), 
                             control = list(maxeval=maxeval)),
                       silent = F)
  
} else if (optim_type == "nmkb"){
  
  optim_results <- try(nmkb(weights, 
                            min_KS_m2, 
                            allData = allData, 
                            initial_condition = initial_condition,
                            all_CDF_Data_Params = all_CDF_Data_Params,
                            lower=0, 
                            upper=1, 
                            control=list(maxfeval=maxeval)),
                       silent=F)
  
} else if (optim_type == "nlminb"){ 
  optim_results <- try(nlminb(start = weights,
                              objective = min_KS_m2,
                              allData = allData,
                              initial_condition = initial_condition,
                              all_CDF_Data_Params = all_CDF_Data_Params,
                              control = list(iter.max = maxeval),
                              lower = rep(0, length(weights)),
                              upper = rep(1, length(weights))))
} else {
  message("wrong optim type specified:", optim_type)
}

print(optim_results)


save_max_it_data_m2 <- function(out_dir, nid_loc_yr_i_m2, optim_type, initial_condition, option, launch.date) {
  # read files tracking performance for each iteration
  files <- list.files(paste0(out_dir, "/", nid_loc_yr_i_m2, "/", initial_condition))
  max_iteration = max(as.integer(unlist(lapply(files, function(f) { gsub(x = f, pattern = ".csv", replacement = "") } ))))
  
  # read in the file associated with the last iteration, and save it to it's own directory
  # for future use
  max.weights <- fread(paste0(out_dir, "/", nid_loc_yr_i_m2, "/", initial_condition, "/", max_iteration, ".csv"))
  
  dir.create(paste0(out_dir, "/", nid_loc_yr_i_m2, "/", initial_condition, "_records" ), recursive = T)
  
  max.weights[, nid_loc_yr_i := nid_loc_yr_i_m2]
  max.weights[, initial_condition := initial_condition]
  max.weights[, option := option]
  
  write.csv(max.weights, paste0(out_dir, "/", nid_loc_yr_i_m2, "/", "/", initial_condition, "_records/", option,"_weights_", launch.date, ".csv"))
}

for (nid_loc_yr_i_m2 in unique(allData$nid_loc_yr_index)) {
  save_max_it_data_m2(out_dir, nid_loc_yr_i_m2, optim_type, initial_condition, option, launch.date)
}

# ----- Clean up

for(nid_loc_yr_i in unique(allData$nid_loc_yr_index)) {
  # save all of iteration information to one file so that we can delete the separate files
  aggregate_iterations(out_dir, nid_loc_yr_i, optim_type, initial_condition, option, launch.date)
}

if(delete_iteration_csvs == T){
  for(nid_loc_yr_i in unique(allData$nid_loc_yr_index)) {
    unlink(x = paste0(out_dir, "/", nid_loc_yr_i, "/", initial_condition), recursive = T) 
  }
}

#creating the log so we can see how long things run
end.time <- gsub("-", "_", Sys.time())

runtimes.log <- data.table(me.type = me_type, strategy = "m2", opt = option, initial.condition = initial_condition, start_time = start.time, end_time = end.time)
write.csv(runtimes.log, paste0(out_dir, "runtime_records/", option, "_ic_", initial_condition, ".csv"), row.names = F)

# print some settings for vetting in log
print(demo_vals)
print(allData)

