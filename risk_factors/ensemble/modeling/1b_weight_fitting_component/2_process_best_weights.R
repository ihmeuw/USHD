rm(list = ls())

library(dplyr)
library(data.table)
source(file.path("FILEPATH/1_parallel_helper_functions.R"))
source(file.path("FILEPATH/check_implied_prevs.R"))
source(file.path("FILEPATH/plot_edensity_components.R"))
source(file.path("FILEPATH/compare_edensity_ushd.R"))

nf_repo <- paste0("FILEPATH")
funcs <- list.files(paste0(nf_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(nf_repo, "/functions/", func)))
}

if(interactive()){
  launch.date <- "2022_12_16_13_53_32"
} else{
  args <- commandArgs(trailingOnly = TRUE)
  launch.date <- args[1]
}
root <- "FILEPATH"
get_settings(paste0(root, launch.date, "/settings.csv"))

if(!exists("by_demo")) by_demo <- F
if(by_demo){
  # expand the launch date to include all of the demographic-specific directories under the launch date
  launch.date.expanded <- paste(launch.date, 
                        grep(pattern = "runtime", list.dirs(path = paste0(root, launch.date), recursive = F, full.names = F), value = T, invert = T), 
                        sep = "/")
  if(any(grepl("race2", demos))){
      launch.date.expanded <- launch.date.expanded[grep("race2_NA", launch.date.expanded, invert = T)]
  }
  
} else{
  launch.date.expanded <- launch.date
}


if(!exists("test_additional_dist")) test_additional_dist = F
if(run_m2 & !run_m1){
  strategy = "m2"
} else{
  stop("this is only set up to work for m2")
}

# process best weights for each demo, if fit separely by demographic. Otherwise, calc best overall weight
for(ld in launch.date.expanded){
  print(ld)
  # me_type and option are used in call & come from settings
  process_best_weights(me_type = me_type, strategy = strategy, option = option, launch.date = ld, test_additional_dist = test_additional_dist)
}

# if fit by demographic, compile all of the weight sets into a single file
if(by_demo){
  demos <- fread(paste(root, launch.date, "demo_vals_map.csv", sep = "/"), fill=T)
  names <- copy(names(demos))
  
  if("race2" %in% names){
    demos <- demos[race2 != ""]
  }
  
  # make the filepath associated with each demographic combo
  demos[, comb_name := paste(lapply(names, function(dd) tolower(paste(dd, gsub(" ", "_", get(dd)), sep = "_"))), collapse = "_"), by = 1:nrow(demos)]
  
  # read in the weight associated with the demographics and combine with the actual demographic values
  read_wt <- function(dd) rbindlist(lapply(dd, function(d) fread(paste(root, launch.date, d, "best_weights/bestweights.csv", sep = "/"))))
  all_weights <- cbind(demos, demos[, read_wt(comb_name)])
  
  # write out
  fwrite(all_weights, paste0(root, launch.date, "/_weightset.csv"))
} else{ # if not fit by demo, just reformate the output
  all_weights <- fread(paste(root, launch.date, "best_weights/bestweights.csv", sep = "/"))
  all_weights$comb_name = "all_pop"
  fwrite(all_weights, paste0(root, launch.date, "/_weightset.csv"))
}

# Plot the implied distributions and prevs --------------------------------

plots <- check_implied_prevs(launch.date = launch.date,
                    data_version = data_version)

pdf(paste(root, launch.date, "check_implied_prevs.pdf", sep = "/"), width = 17, height = 7)
plots
dev.off()


# Visualize the ensemble distributions ------------------------------------

# define the distlist for plotting
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

weight_list <- lapply(1:nrow(all_weights), function(i) all_weights[i, names(distlist), with = F])
temp_names <- lapply(1:nrow(all_weights), function(i) all_weights[i, comb_name])

density.construction.plots <- plot_edensity(weight_list = weight_list, # a list of weights (at least one set)
                                            distlist = distlist, # object containing the pdf_families -- this get defined in process_best_weights, run above
                                            plot_components = T,  # logical: if true, plots the component distributions and ens dist
                                            CDF_Data_Params = list(mn = 28, # use values around the national mean/var -- use same for each plot for comparison
                                                                   variance = 39,
                                                                   XMIN = 10,
                                                                   XMAX = 70,
                                                                   x = seq(10, 70, length = 1000)),
                                            scale = T, # logical: rescale each CDF so that it is in the range [0,1]
                                            version_titles = temp_names # must be the same length as weight_list
)

distributions <- compare_edensity_ushd(all_weights, distlist)

pdf(paste(root, launch.date, "edensity.pdf", sep = "/"), width = 14, height = 7)
print(distributions)
density.construction.plots
dev.off()

