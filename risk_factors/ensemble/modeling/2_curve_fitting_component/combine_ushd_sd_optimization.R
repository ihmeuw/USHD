####################################################################################################
## Description: Checks that all output files exist.
##              Combines all draw files (initial step saves separately by draw)
##              Saves combined draws, julienned draws (i.e., by mcnty), and collapsed ests
##        
## Input: Tasks csv (original, not resub version), which specifies all arrays run originally
##        
## Output:  
##          Combined draws:  "[model_dir]/draws/draws_mcnty_[year]_[sex]_[race]_[edu].rds"
##          Julienned draws: "[model_dir]/draws_mcnty/[MCNTY]/draws_[MCNTY]_[year]_[sex]_[race]_[edu].rds"
##          Ests:            "[model_dir]/est/est_mcnty_[year]_[sex]_[race]_[edu].rds"
##
####################################################################################################

rm(list = ls())
if (!interactive()) {
  args <- commandArgs(TRUE)
} else{
}

message("args")
print(args)

repo <- args[[1]]
settings_loc <- args[[2]]
output_dir <- args[[3]]
tasks_path <- args[[4]]

# Set up ------------------------------------------------------------------

library(data.table)
library(parallel)
source(paste0(repo, "ensemble/modeling/2_curve_fitting_component/sd_opt_utils/get_sd_draw_files.R"))

# read settings and arguments
nf_repo <- paste0("FILEPATH")
risk_repo <- paste0("FILEPATH")
funcs <- list.files(paste0(nf_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(nf_repo, "/functions/", func)))
}

##### Set data location and settings locations, and read in settings
get_settings(settings_loc)
tasks <- fread(tasks_path, stringsAsFactors = F)

# set up output directories for by-mcnty directories ----------------------
if(!dir.exists(paste0(output_dir, "/draws_mcnty"))){
  dir.create(paste0(output_dir, "/draws_mcnty"))
  locs <- fread("FILEPATH")
  sapply(locs[, unique(mcnty)], function(mcnty) dir.create(paste0(paste0(output_dir, "/draws_mcnty/", mcnty))))
}



# Generate list of expected outputs ---------------------------------------

files <- get_sd_draw_files(tasks)
if(files[, sum(failed)] > 0){
  print("The following tasks failed:")
  print(files[failed == 1])
  stop("Cannot combine outputs if some tasks failed.")
} else{
  
  # draws_mcnty_YEAR_SEX_RACE_EDU_imp_all.rds
  
  # process files in terms of chunks that will be saved together
  
  chunks <- unique(files[, .(year, s, r,e)])
  
  mclapply(1:chunks[,.N], function(i){
    cc <- chunks[i]
    print(paste("Saving chunk", i, "out of", chunks[, .N]))
    ff <- files[cc, on = c("year","s","r","e"), file]
    temp <- rbindlist(lapply(ff, readRDS))
    # save combined draws
    saveRDS(temp, 
            file = sprintf("%s/draws/draws_%s_%s_%s_%s_%s.rds", output_dir, area_var, cc$year, cc$s, cc$r, cc$e))
    
    # save combined draws by mcnty
    setkey(temp, area)
    stopifnot(temp[, level][1] == "mcnty")
    for(mcnty in temp[, unique(area)]){
      mcnty_temp <- temp[area == mcnty]
      # remove any columns related to scoring since they interfere with PAF calculator
      rm_col <- c("exp_mean", "score", "implied_under", "implied_over", "implied_obese", "over_prev", "obes_prev", "under_prev")
      rm_col <- rm_col[which(rm_col %in% names(mcnty_temp))]
      mcnty_temp[, ((rm_col)) := NULL]
      saveRDS(mcnty_temp, 
              file = sprintf("%s/draws_mcnty/%s/draws_%s_%s_%s_%s_%s.rds", output_dir, mcnty, mcnty, cc$year, cc$s, cc$r, cc$e))
    }
    
    # save collapsed version
    if(save_score){
      key <- c("level", "area", "year", "sex", "age", "race", "edu")
      score_summary <- sae.shared::collapse_draws(temp,"score", id_vars = key)
      exp_sd_summary <- sae.shared::collapse_draws(temp,"pred", id_vars = key)
      exp_mean_summary <- sae.shared::collapse_draws(temp,"exp_mean", id_vars = key) # remember that exp_mean is modeled, so it is different for each draw
      
      saveRDS(exp_sd_summary[score_summary, on = key][exp_mean_summary, on = key][, `:=`(rei_id = 370, modelable_entity_name = "Adult BMI standard deviation [USHD]", mei = 27161)],
              file = sprintf("%s/est/est_%s_%s_%s_%s_%s.rds", output_dir, area_var, cc$year, cc$s, cc$r, cc$e))
    } else{
      saveRDS(sae.shared::collapse_draws(temp,"pred", id_vars = c("level", "area", "year", "sex", "age", "race", "edu"))[, `:=`(rei_id = 370, modelable_entity_name = "Adult BMI standard deviation [USHD]", mei = 27161)],
              file = sprintf("%s/est/est_%s_%s_%s_%s_%s.rds", output_dir, area_var, cc$year, cc$s, cc$r, cc$e))  
    }
    
    
  })
  
}

