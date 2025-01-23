####################################################################################################
## Description: Reads in the mx draws for a particular year, concatenates them, and saves the draws
##              and point estimates, confidence intervals, and standard errors for all areas/ages/.
##              Deletes the subdraw files once the final files are saved.
##
## Passed args: dir [character] -- home directory for settings and final output
##              sex [integer] -- sex to generate predictions for
##              race [integer] -- race to generate predictions for
##              draw_width [integer] -- width of the subdraw interval (e.g. 1-51 has a draw width of

##                script will not work.
##
## Requires:    mx draws saved by subdraw for the year of interest:
##                [dir]/mx_est_[area_var]_[year]_[sex]_[race]_[max_subdraw].rds
##
## Outputs:     mx draws and estimates, saved in separate files by year:
##                "[dir]/mx_draws_[area_var]_[year]_[sex]_[race].rds"
##                "[dir]/mx_est_[area_var]_[year]_[sex]_[race].rds"
##
####################################################################################################

stopifnot(grepl("mortality/sae_models$", getwd()))

library(R.utils)
library(data.table)
library(Matrix)
library(splines)
library(dplyr)
sourceDirectory("functions/")

set.seed(98121)

## Settings ----------------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]
sex <- as.integer(args[2])
race <- as.integer(args[3])
edu <- as.integer(args[4])
draw_width <- as.integer(args[5])
resub <- as.logical(args[6])


task_id <- ifelse(is.na(as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))), 1, as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID")))
ids <- fread(paste0(dir, "/mx_save_args_", sex, "_", race, "_", edu, ".csv"))
this_year <- ids[task_id, year]

get_settings(dir)

sub_draws <- seq(1, n.sims, draw_width) # has to come after get_settings()

# print out all the variables that control the behavior of this script
# task_id and this_year are not passed in as arguments but do control behavior
# This provides the information that distinguishes each array job in the job-output
cat(
  glue::glue("Settings passed are:",
             "dir = '{dir}'",
             "sex = {sex}",
             "race = {race}",
             "edu = {edu}",
             "draw_width = {draw_width}",
             "task_id = {task_id}",
             "this_year = {this_year}\n",
             .sep = "\n")
)

# other settings
cat(
  glue::glue(
    "Other settings:",
    "race_together = {race_together}",
    "edu_together = {edu_together}",
    "by_race = {by_race}",
    "by_edu = {by_edu}",
    "races =  c({races})",
    "edu_groups = c({paste(edu_groups, collapse = ', ')})\n",
    .sep = "\n"
  )
)
## Function ----------------------------------------------------------------------------------------
# function to save the draws at the end

save_draws <- function(draws, race_together_temp, edu_together_temp, expected_files, y, r=NULL, e=NULL,s=NULL) {
  merge_vars <- c("level", "area", "year", "sex", "race", "edu", "sim")
  if("adjusted" %in% names(draws)){
    merge_vars <- c(merge_vars,"adjusted")
  }
  merge_vars <- c(merge_vars[!(merge_vars == "sim")], "age")
  est <- collapse_draws(draws, "mx", merge_vars)

  if (race_together_temp) {

    saveRDS(draws[race == r, ], file = paste0(dir, "/mx_draws_", area_var, "_", y, "_", s, "_", r, "_", edu, ".rds"))
    saveRDS(est[race == r, ], file = paste0(dir, "/mx_est_", area_var, "_", y, "_", s, "_", r, "_", edu, ".rds"))

  } else if (edu_together_temp) {

    saveRDS(draws[edu == e, ], file = paste0(dir, "/mx_draws_", area_var, "_", y, "_", s, "_", race, "_", e, ".rds"))
    saveRDS(est[edu == e, ], file = paste0(dir, "/mx_est_", area_var, "_", y, "_", s, "_", race, "_", e, ".rds"))

  } else {
    saveRDS(draws, file = paste0(dir, "/mx_draws_", area_var, "_", y, "_", s, "_", race, "_", edu, ".rds"))
    saveRDS(est, file = paste0(dir, "/mx_est_", area_var, "_", y, "_", s, "_", race, "_", edu, ".rds"))

  }

  # delete intermediate files
  sapply(paste0(dir,"/",expected_files), unlink)

}

# Saving  -----------------------------------------------------------------------------------------

if ((!race_together & by_race) | (!edu_together & by_edu)) {
  if (by_race) {
    message(glue::glue("By race model with separate models for each race/ethnicity. Working on race {race}..."))
  }

  if (by_edu) {
    message(glue::glue("By-education model with separate models for each educational attainment group. Working on edu {edu}..."))
  }

  # if resubmitting and the final file is already present, then we don't need to re-compile
  # This section is for race, but file should still say which education it is for (ie all_pop_id), and
  # vice versa
  if(sex_together) {
    sexes_to_model <- sexes
  } else {
    sexes_to_model <- sex
  }
  
  for(s in sexes_to_model) {
    final_file <- paste0("mx_draws_", area_var, "_", this_year, "_", s, "_", race, "_", edu, ".rds")
    
    if(resub & file.exists(paste0(dir,"/",final_file))){
      message("Already have final file")
      next
    }
    
    expected_files <- paste0("mx_draws_", area_var, "_", this_year, "_", s, "_", race, "_", edu, "_",
                             sub_draws+(draw_width-1), ".rds")
    
    # read in the data
    all_draws <- tryCatch({
      lapply(paste0(dir,"/",expected_files), readRDS) %>% rbindlist(use.names=T)
    },error=function(cond){
      message("Files not done saving because could not save")
      message(cond)
      return(NULL)
    })
    
    # saving needs to be different if this is by_race / by_edu. Eg if by_race if F, the if will yield
    # a NULL, which works fine with the save draws function.
    if(!is.null(all_draws)) {
      save_draws(all_draws, race_together, edu_together, expected_files, y=this_year, r = if (by_race) race, e = if (by_edu) edu, s = s)
    }
    
  }
  

} else { 
  if(sex_together) {
    sexes_to_model <- sexes
  } else {
    sexes_to_model <- sex
  }
  
  for(s in sexes_to_model) {
    for (r in races) {
      for (e in edu_groups) {
        
        message(glue::glue("Processing race {r}, edu {e}, and year {this_year}"))
        
        # if resubmitting and the final file is already present, then we don't need to re-compile
        final_file <- paste0("mx_draws_", area_var, "_", this_year, "_", s, "_", r, "_", e, ".rds")
        
        if(resub & file.exists(paste0(dir,"/",final_file))){
          message("Already have final file")
          next
        }
        
        expected_files <- paste0("mx_draws_", area_var, "_", this_year, "_", s, "_", r, "_", e, "_",
                                 sub_draws+(draw_width-1), ".rds")
        
        # read in the data
        all_draws <- tryCatch({
          lapply(paste0(dir,"/",expected_files), readRDS) %>% rbindlist(use.names=T)
        },error=function(cond){
          message("Files not done saving because could not save")
          message(cond)
          return(NULL)
        })
        
        if(!is.null(all_draws)) {
          save_draws(all_draws, race_together, edu_together, expected_files, y=this_year, r=r, e=e, s = s)
        }
        
      } # end e / edu loop
    } # end r / race loop
  }

} # end else: where race_together is F and edu_together is F

message(paste0("Done with ", this_year))
