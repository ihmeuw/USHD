# Reformats the weight set identified by user as "best" and places it in a 
# clean location so that it can be used by exp-sd optimization code
# Uploads to USHD database

library(lbd.loader, lib.loc = sprintf("FILEPATH", 
                                      R.version$major, 
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))

library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
lbd.loader::load_package("sae.shared")
# arguments: --------------------------------------------------------------

version <- "2023_02_21_16_20_42"

# IMPORTANT: Remember to update arguments below for each version
make_best = TRUE
# if make_best is TRUE, need to provide a description of issue with previous version
my_version_name = "2023_02_21_16_20_42_by_age70_sex_race2"
prev_version_issues = "Excluded inverse-Weibull b/c of issue in pdf_families script. Can now include inv-Weibull after making fixes to distribution functions"
version_description = "Ensemble weights stratified by age70 (dichotomous >=70 or younger), sex, and race2 (NHANES race var that separates NH Asian from NH Other). Ens weights fit using edensity over support [10,50]. Remove exponential dist. Version 2023_02_21_16_20_42 Fit using USHD version of pdf_families.R script to addressed some issues with distribution parameter solving. Optimized fit at just overweight and obese thresholds (previously, also included underweight threshold). Found similar performance on left side of dist but superior performance of right side of dist when only optimizes for ow/ob"


# set up ------------------------------------------------------------------

nf_repo <- paste0("FILEPATH")
funcs <- list.files(paste0(nf_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(nf_repo, "/functions/", func)))
}

root <- "FILEPATH"


# Reformat weight set to reflect levels in SAE ests -----------------------

# read in the weight set
temp_wt <- fread(sprintf("%s%s/_weightset.csv", root, version))

# read in the microdata used to produce the weight set:
get_settings(sprintf("%s%s/settings.csv", root, version))
data <- readRDS(sprintf("FILEPATH", data_version))

data[, edu := as.numeric(edu)][, edu := sae.shared::edu_default]

# get all levels of demographic variables in microdata
levels <- na.omit(unique(data[, unique(c(demos, "age", "sex", "race", "edu")), with = F]))

# define race codes used in the BMI SAE models:
# create the key to bridge race with the vars available in NHANES:
if("race2" %in% demos){
  race_key <- as.data.table(list(race_code = c(5,                   4,                6,        7,          2),
                                 # race_code = c(1,                   2,                3,        4,          7),
                                 race_sae =   c("NH White",       "NH Black",     "NH AIAN", "NH API",  "Hispanic"),
                                 race_nhanes = c("NH White Only", "NH Black Only", "other", "NH Asian Only", "Hispanic")))  # THE MATCHING B/W NHANES races and races in the data is not perfect -- i.e., NH Multiracial and and NHOPI are not in the models ests
  
} else if ("race" %in% demos){
  race_key <- as.data.table(list(race_code = c(5, 4, 6, 7, 2),
                                 # race_code = c(1, 2, 3, 4, 7),
                                 race_sae = c("NH White", "NH Black", "NH AIAN", "NH API", "Hispanic"),
                                 race_nhanes = c("NH White Only", "NH Black Only", "other", "other", "Hispanic")))
} else{
  message("race not in demos. Using the 1999-2010 NHANES race key")
  race_key <- as.data.table(list(race_code = c(5, 4, 6, 7, 2),
                                 # race_code = c(1, 2, 3, 4, 7),
                                 race_sae = c("NH White", "NH Black", "NH AIAN", "NH API", "Hispanic"),
                                 race_nhanes = c("NH White Only", "NH Black Only", "other", "other", "Hispanic")))
  
}

print("The following recoding to race is performed: \n")
print(race_key)

# create an age key
age_key <- as.data.table(list(age = 20:85))
age_key[, age_group := 5*floor(age/5)]

levels <- merge(levels, age_key, by = "age", all.x = T)

race_var <- ifelse("race2" %in% demos, "race2", "race")
levels <- merge(levels, race_key, by.x = race_var, by.y = "race_nhanes", all.x = T, allow.cartesian = T)
stopifnot(levels[, .N] == nrow(na.omit(levels)))

# if 85 year age group not in NHANES, make it here (use same weights at 80-year group):

if(levels[age %in% 81:85, .N] == 0){
  temp <- copy(levels[age == 80])
  temp[, `:=`(age = NA, age_group = 85)]
  levels <- rbind(levels, temp)
}

levels <- unique(levels[, age := NULL])

# Expand the weight set to reflect all levels of ests ---------------------

weights <- merge(temp_wt, levels, by = demos, all = T, allow.cartesian = T)

weights[, c("race", "race_sae", "comb_name") := NULL]

# set names to match the columsn in data
setnames(weights, c("race_code", "age_group", "sex", "edu"), c("population_group_id", "age", "sex_id", "educational_attainment_id")) # for now, the population_group_id refers to race

# remove old age groups like age_20, age_70...
rm_old_age <- grep("age_|age70", names(weights), value = T)
if(length(rm_old_age) > 0) weights[, (rm_old_age) := NULL]


# add IDs -----------------------------------------------------------------

age_id_key <- weights[, .(age = sort(unique(age)), age_group_id = sae.shared::translate_ages(sort(unique(age)), "age_group_id"), age_group_name =  sae.shared::translate_ages(sort(unique(age)), "age_group_name"))]
weights[, location_id := 102]
weights <- age_id_key[weights, on = "age"]
# add year ID (same for all years)
weights <- rbindlist(lapply(2000:2019, function(yr) {DT <- copy(weights); DT[, year_id := yr]}))

# get race (population) set
setkeyv(weights, c("year_id", "location_id", "sex_id", "age_group_id", "population_group_id"))

# check column names ------------------------------------------------------
# To avoid NULLs from get_ensemble_weights, fill any of the expected ensemble weight 
# columns with zeros if they're not already present in the weights DT
distributions <- c("exp","gamma","invgamma","llogis","gumbel","invweibull","lnorm","glnorm","betasr","mgamma","mgumbel","weibull","norm")
weights[, setdiff(distributions, names(weights)) := 0]

# check that each row of weights sums to approx 1
stopifnot(all(abs(apply(X = weights[, ..distributions], MARGIN = 1, FUN = sum) -1) < 1e-6))

# save the weight set and info about strata vars --------------------------

dir.create(sprintf("%s/_best/%s", root, version))
fwrite(weights, sprintf("%s/_best/%s/weights.csv", root, version))
saveRDS(demos, sprintf("%s/_best/%s/weight_strat_vars.rds", root, version))

# Upload to database ------------------------------------------------------

ensemble_weight_version_id <- save_ensemble_weights(
  sprintf("%s/_best/%s/weights.csv", root, version),
  version_name = my_version_name,
  version_type = "race",
  is_best = make_best,
  prev_issues = prev_version_issues,
  description = version_description
)
