####################################################################################################
## Description: Launches save_unraked_as_raked.r
##
## Args:        dir [character] -- home directory for settings and final output
##              resub [boolean] -- if you want to resubmit and thus only launch for the missing files
##              measure [character] -- mx, yll
##
## Outputs:     raked draws and summaries that are equivalent to the unraked values (or, for the case of YLLs,
##              the YLLs generated from the raked mortality rates)
##
####################################################################################################

stopifnot(grepl("mortality/sae_models$", getwd()))

library(R.utils)
library(data.table)
library(Matrix)
library(splines)
sourceDirectory("functions/", modifiedOnly=F)

############# Arguments #############
dir = 'FILEPATH'
resub <- F
measure <- "yll"
####################################

stopifnot(basename(dir) == "_all") ## why are you saving unraked values as raked for anything but all-cause?
get_settings(dir)
combos <- as.data.table(expand.grid(dir = dir, sex = sexes, race = races, edu = 1, lvl = c(area_var,names(geoagg_files)),
                                    yr = years))

if(resub) {
  for(ii in 1:nrow(combos)) {
    sex <- combos[ii, sex]
    race <- combos[ii, race]
    edu <- combos[ii, edu]
    lvl <- combos[ii, lvl]
    yr <- combos[ii, yr]
    
    if(file.exists(paste0(dir, "/", measure, "_draws_",lvl,"_",yr,"_",sex,"_",race,"_",edu,"_raked.rds"))) {
      combos <- combos[ii, dir := NA]
    }
  }
  combos <- combos[!is.na(dir)]
}

fwrite(combos, paste0(dir, "/unraked_save_raked_args.csv"))

sbatch(code = "post_estimation/save_unraked_as_raked.r",
       arguments = c(dir, measure),
       name = "copy_unraked_raked",
       fthread = 2, 
       m_mem_free = "50G",
       h_rt = "02:00:00", 
       archive = F,
       project = "PROJECT", 
       queue = "QUEUE",
       sgeoutput = dir, 
       array = paste0("1-", nrow(combos)), 
       array_throttle = 200)

## Check if there are any missing files by looking at the final files
missing_files <- c()
for(ii in 1:nrow(combos)) {
  
  sex <- combos[ii, sex]
  race <- combos[ii, race]
  edu <- combos[ii, edu]
  lvl <- combos[ii, lvl]
  yr <- combos[ii, yr]
  
  if(!file.exists(paste0(dir, "/", "mx_draws_",lvl,"_",yr,"_",sex,"_",race,"_",edu,"_raked.rds"))) {
    missing_files <- c(missing_files, paste0(dir, "/", "mx_draws_",lvl,"_",yr,"_",sex,"_",race,"_",edu,"_raked.rds"))
  }
}

message(paste(missing_files, collapse="\n"))
