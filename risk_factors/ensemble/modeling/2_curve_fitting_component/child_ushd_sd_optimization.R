####################################################################################################
## Description: Run the SD optimization script for BMI for one strata
##                
## Input: Input is specified in settings.csv and pass through array job
## 
## Output: Long-format of draws based on model and input data
##            saved in separate files by year and draw "chunk" (draws get combined in "combine_ushd_sd_optimization.R")
##            
##            "[model_dir]/intermediate/draws_mcnty_[year]_[sex]_[race]_[edu]_[imp]_sim[draw].rds"
##
####################################################################################################

rm(list = ls())
if (!interactive()) {
  args <- commandArgs(TRUE)
  
  i <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  message('print sys.getenv')
  print(i)
} else{
  i = 18
  print(i)
}


message("args")
print(args)

repo <- args[[1]]
settings_loc <- args[[2]]
output_dir <- args[[3]]
tasks_path <- args[[4]]


start <- Sys.time()
  
# Set up ------------------------------------------------------------------

library(data.table)
source(paste0(repo, "ensemble/modeling/2_curve_fitting_component/ushd_exp_sd_bmi.R"))

# read settings and arguments
nf_repo <- paste0("FILEPATH")
funcs <- list.files(paste0(nf_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(nf_repo, "/functions/", func)))
}

##### Set data location and settings locations, and read in settings
get_settings(settings_loc)
tasks <- fread(tasks_path, stringsAsFactors = F)


parse_setting <- function(arg){ # for some columns, can pass a single value or a vector of values as string. Convert to a numeric vector
  eval(parse(text = tasks[i,arg, with = F]))
}

if(tasks[i, locid] == 0) .location_id <-NULL else .location_id <- parse_setting("locid")

.year_id <- parse_setting("year")
.sex_id <- parse_setting("sex")
.draws_keep <- parse_setting("draw")
.imp <- parse_setting("imp")
.extra_dim_values <- parse_setting(extra_dim)

# assign default values to optional settings if not passed through settings

if(!exists("round_digits") || is.null(round_digits)){ # if round_digits not specified in the settings file, use the default
  round_digits <- formals(calc_metab_bmi_adult_exp_sd_ushd)$round_digits
  # formals$arg returns a pairlist, which is bit different than a normal list. 
  # Do some simple formating to get the default values in the expected shape
  round_digits <- as.list(round_digits)
  round_digits <- round_digits[which(names(round_digits) != "")]
  message("using default round_digits")
}

if(!exists(".fx_length") || is.null(.fx_length)){ # if fx_length not specified in settings file, use the default
  .fx_length <- 1000
  message("using default fx_length")
}

if(!exists(".control") || is.null(.control)){ # if control paramaters for nlminb not set, use the defaults from gbd
  .control <- list(iter.max=3, eval.max=3)
  message("using default control params")
}

if(!exists("ens_weight_version_name") & !exists("ensemble_weight_version_ids")){
  # if neither exists, use GBD weights
  ens_weight_version_name <- "GBD2020_weights"
  ensemble_weight_version_ids = NULL
  message("using default ens_weight_version_name ", ens_weight_version_name)
} else if (!exists("ens_weight_version_name")){
  # if version exists but not name, set name as NULL
  ens_weight_version_name = NULL
} else if (!exists("ensemble_weight_version_ids")){
  # if name exists but not version, set version as NULL
  ensemble_weight_version_ids = NULL
} # if both exist, do nothing

message("ens weight version name", ens_weight_version_name)
message("ens weight version ID ", ensemble_weight_version_ids)

if(!exists("threshold_weights")){
  stop("threshold_weights is missing")
}
# perform checks on threshold weights
stopifnot(is.list(threshold_weights))
if(abs(sum(unlist(threshold_weights)) - 1) > 1e-5) stop(sprintf("Sum of threshold weights is not equal to one. Equals %s", sum(unlist(threshold_weights))))
if(is.null(threshold_weights$uw)){
    threshold_weights$uw <- 0
}
if(exists("incl_underweight") && !incl_underweight){
  # the weight assigned to underweight in objective function should
  # be zero if incl_underweight is FALSE
  if(threshold_weights$uw != 0) stop("incl_underweight is FALSE but threshold_weights$uw = ", threshold_weights$uw)
}
stopifnot(identical(sort(names(threshold_weights)), c("ob", "ov", "uw")))
stopifnot(is.numeric(unlist(threshold_weights)))

message("Using threshold_weights ", paste(paste(names(threshold_weights), threshold_weights, sep = ":"), collapse = ", "))
exp_sd <- calc_metab_bmi_adult_exp_sd_ushd(exp_mean_dir = mean_bmi_dir,
                                           location_id = location_id,
                                           year_id = .year_id, 
                                           sex_id = .sex_id,
                                           draws_keep = .draws_keep,
                                           imp = .imp,
                                           extra_dim = extra_dim,
                                           extra_dim_values = .extra_dim_values,
                                           threshold_weights = threshold_weights,
                                           fx_length = .fx_length,
                                           control = .control,
                                           round_digits = round_digits, 
                                           save_score = save_score,
                                           ens_weight_version_name = ens_weight_version_name,
                                           ensemble_weight_version_ids = ensemble_weight_version_ids,
                                           mc_cores = 9,
                                           opt_raked_prev = opt_raked_prev,
                                           opt_raked_exp_mean = opt_raked_exp_mean)

# save outputs

setnames(exp_sd, "draw", "sim") # rename for consistency with fatal/nonfatal

save_cols <- c("area", "level", "year", "sex", extra_dim, "age", "pred", "sim")
if(save_score) save_cols <- c(save_cols, "exp_mean", "score",  "implied_under", "implied_over", "implied_obese", "over_prev", "obes_prev", "under_prev")

# check that all save cols are in exp_sd
save_cols_all <- save_cols
save_cols <- save_cols[which(save_cols %in% names(exp_sd))]
if(length(save_cols_all) != length(save_cols)){
  warning("Some save cols are not found in exp_sd:")
  print(setdiff(save_cols_all, save_cols))
}


for(ii in .imp){
  for(dd in .draws_keep){
    for(this_year in .year_id){
      if(extra_dim == "race"){
        race_dim <- .extra_dim_values
        edu_dim <- 1
      } else if(extra_dim == "edu"){
        edu_dim <- .extra_dim_values
        race_dim <- 1
      } else{
        stop("extra dim ", extra_dim, " not recognized")
      }
      for (r in race_dim) {
        for(e in edu_dim){
          for (s in .sex_id) {
            message(cat(this_year, s, r, e, ii, "sim", dd, sep = "_"))
            if(extra_dim == "race"){
              temp <- exp_sd[race == r & sex == s & year == this_year & imp == ii & sim == dd, ..save_cols][, edu := 1]  
            } else{
              temp <- exp_sd[edu == e & sex == s & year == this_year & imp == ii & sim == dd, ..save_cols][, race := 1] 
            }
            
            saveRDS(temp, 
                    file = sprintf("%s/draws/intermediate/draws_%s_%s_%s_%s_%s_%s_sim%s.rds", output_dir, area_var, this_year, s, r, e, ii, dd))
        }
        
      }
      }
    }  
  }
}


# save runtime information 
end <- Sys.time()
if(save_score) warning("Runtime includes time to score results, which will not be done on the final models")
temp <- tasks[i, ][, runtime_hrs := as.double(end-start, units = "hours")]
fwrite(x = temp, file = paste0(output_dir, "/timelog_child_ushd_sd_optimization.csv"), append = T)
