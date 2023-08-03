
####################################################################################################
## Description: For each of the specified causes (just all-cause for now), copy the file but with a _raked name
##              For mortality, this uses the unraked draws and summaries. But, for YLLs this uses the _prelim_raked
##              files since we want to use the YLLs created from the raked mortality rates.
##
## Args:        dir [character] -- home directory for settings and final output
##              measure [character] -- mx, yll
##              (Note: all other arguments come from the array job specifications)
##
## Outputs:     raked draws and summaries that are equivalent to the unraked values (or, for the case of YLLs,
##              the YLLs generated from the raked mortality rates)
##
####################################################################################################


library(R.utils)
library(data.table)
library(Matrix)
library(splines)
sourceDirectory("functions/", modifiedOnly=F)


## Get settings ------------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]
measure <- args[2]

draw_args <- fread(paste0(dir, "/unraked_save_raked_args.csv"))
task_id <- ifelse(is.na(as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))), 1, as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID")))

#dir <- draw_args[task_id, dir]
sex <- draw_args[task_id, sex]
race <- draw_args[task_id, race]
edu <- draw_args[task_id, edu]
lvl <- draw_args[task_id, lvl]
yr <- draw_args[task_id, yr]


get_settings(dir)

file_draws <- paste0(measure,"_draws_",lvl,"_",yr,"_",sex,"_",race,"_",edu, if(measure == "yll") "_prelim_raked", ".rds")
file_est <- paste0(measure,"_est_",lvl,"_",yr,"_",sex,"_",race,"_",edu, if(measure == "yll") "_prelim_raked", ".rds")

# first, just save the estimates out
file.copy(paste0(dir, "/", file_est), paste0(dir, "/", gsub(paste0(if(measure == "yll") "_prelim_raked", ".rds"), "_raked.rds", file_est)))

# read in the draws in order to split by age
if(measure == "mx") {
  draws <- readRDS(paste0(dir, "/", file_draws))
  
  for(a in ages) {
    
    message(paste0("Saving for ",a))
    
    saveRDS(draws[age == a], paste0(dir,"/",gsub(".rds", paste0("_",a,"_TEMP.rds"), file_draws)))
    
    
  }
}


# copy full draws
file.copy(paste0(dir, "/", file_draws), paste0(dir, "/", gsub(paste0(if(measure == "yll") "_prelim_raked", ".rds"), "_raked.rds", file_draws)))
