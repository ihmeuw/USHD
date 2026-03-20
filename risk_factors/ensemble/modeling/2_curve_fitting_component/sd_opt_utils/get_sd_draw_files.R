####################################################################################################
## Description: Function to get the expected paths of exposure SD draws. Useful for 
##                resubmission and combining draws.
##        
## Input: Tasks csv
##        **Should be run after get_settings(settings_loc)**
## 
## Output:  data.table of filepaths based on child_ushd_sd_optimization.R
##
##
####################################################################################################


# set up ------------------------------------------------------------------


library(data.table)
library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                      R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))

sae.shared_repo <- NULL
# If working directly out of cloned repo of sae.shared, pass its path instead (IFF necessary).
# sae.shared_repo <- "~/code/sae.shared"
if (is.null(sae.shared_repo)) {
  # loads the most current library maintained by LSAE. If specific version is required,
  # pass it with "version" argument.
  lbd.loader::load_package("sae.shared")
  lbd.loader::load_package("ushd.dbr")
} else {
  lbd.loader::load_package(path = sae.shared_repo)
}

library(ushd.dbr, lib.loc = "FILEPATH")


# define ------------------------------------------------------------------

parse_setting <- function(i, arg){ # for some columns, can pass a single value or a vector of values as string. Convert to a numeric vector
  eval(parse(text = tasks[task_num==i,arg, with = F]))
}

get_sd_draw_files <- function(tasks # DT of tasks used for array jbo
){
  # some tasks have multiple associated outputs
  expand_task <- function(i){
    # expand all dimensions in settings
    .year_id <- parse_setting(i, "year")
    .sex_id <- parse_setting(i, "sex")
    .draws_keep <- parse_setting(i, "draw")
    .n.imp <- parse_setting(i, "imp")
    .extra_dim_values <- parse_setting(i, extra_dim)
    
    # expected file paths based on the save commands in child_ushd_sd_optimization.R
    
    return(as.data.table(list(
      year = .year_id,
      s = .sex_id,
      r = if (identical(extra_dim, "race")) .extra_dim_values else sae.shared::race_default, 
      e =  if (identical(extra_dim, "edu"))  extra_dim_values else sae.shared::edu_default,
      ii = .n.imp,
      dd = .draws_keep,
      task_num = i
    )))
  }
  
  expected_files <- rbindlist(lapply(tasks[, unique(task_num)], expand_task))
  expected_files[, file := sprintf("%s/draws/intermediate/draws_%s_%s_%s_%s_%s_%s_sim%s.rds", output_dir, area_var, year, s, r, e, ii, dd)]
  expected_files[, failed := as.numeric(!file.exists(file))]
  
  return(expected_files)
}

