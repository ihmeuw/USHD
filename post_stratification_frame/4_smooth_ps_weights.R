###### Set up objects from command args
(non_fatal_repo <- commandArgs(TRUE)[[1]])
(settings_file <- commandArgs(TRUE)[[2]])
(date_time_stamp <- commandArgs(TRUE)[[3]])

## Import settings
funcs <- list.files(paste0(non_fatal_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(non_fatal_repo, "/functions/", func)))
}

#' Pool population across ages by summing data for pooled age groups.
#'
#' @param data data.table with pop data. Must have column pop.
#' @param age_pools 
#' @param id_vars columns in data to group by and sum. E.g. sex and age.
#' @param pop_var columns in data which represents the population value.
#'
#' @return data.table with data pooled across age groups.
pool_adj_ages <- function(data, age_pools, id_vars, pop_var){
  
  setnames(data, pop_var, 'pop')
  
  # Empty data.table for binding intermediate pooling steps
  data_pooled_all <- vector(mode = "list", length = length(age_pools))
  
  for (a in 1:length(age_pools)) {
    # Grab ages to pool over for this age group
    pooled_ages <- age_pools[[a]]$ages
    age_group <- as.numeric(substr(names(age_pools)[a], 4, nchar(names(age_pools)[a])))
    
    message(paste0("Processing age group ", age_group, "..."))
    
    # create pooled pops
    data_pooled_all[[a]] <- data[age %in% pooled_ages & age_set == age_pools[[a]]$age_set, .(pop = sum(pop),
                                                                                             age = age_group),
                                 by = id_vars]
  }
  
  data_pooled_all <- rbindlist(data_pooled_all, use.names = TRUE, fill = TRUE)
  setnames(data_pooled_all, 'pop', pop_var)
  
  return(data_pooled_all)
}

settings_path <- "FILEPATH"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc)

joint_dir <- "FILEPATH"
pop_dir <- "FILEPATH"

ps_frame_clean <- readRDS(file=paste0(joint_dir, date_time_stamp,'/ps_frame_clean.rds'))
area_var <- 'mcnty'
strat_vars <- c('edu','marital')

# Get population estimates
pops <- readRDS(paste0(pop_dir, "/pop_by_race_ethn_1977/", pop_race_version, ".rds"))
pops_under_20 <- pops[age <= 25]

# Drop years before 2000
pops_under_20 <- pops_under_20[year %in% 2000:2019]

# Harmonize fields
setnames(pops_under_20, "pop", "value")

# Replicate pops_under_20 by edu and marital
combos <- expand.grid(ind = 1, edu = unique(ps_frame_clean$edu), marital = unique(ps_frame_clean$marital))
pops_under_20$ind <- 1
pops_under_20 <- merge(pops_under_20, combos, by = "ind", all.x = TRUE, allow.cartesian = TRUE)[, -c("ind")]

# Adjust value
pops_under_20[, value := value / nrow(combos)]

# Merge state names
locs <- fread("FILEPATH")
pops_under_20 <- merge(pops_under_20, unique(locs[, c("mcnty", "state_name")]), by = "mcnty", all.x = TRUE)

# #### Set aggregate age groups
age_pools <- list("age0" = list("age_set" = "under20", "ages" = c(0, 1)),
                  "age1" = list("age_set" = "under20", "ages" = c(0, 1, 5)),
                  "age5" = list("age_set" = "under20", "ages" = c(1, 5, 10)),
                  "age10" = list("age_set" = "under20", "ages" = c(5, 10, 15)),
                  "age15" = list("age_set" = "under20", "ages" = c(10, 15, 20)),
                  "age20" = list("age_set" = "under20", "ages" = c(20)),
                  "age20" = list("age_set" = "20+", "ages" = c(20)),
                  "age25" = list("age_set" = "20+", "ages" = c(25)),
                  "age30" = list("age_set" = "20+", "ages" = c(25, 30, 35)),
                  "age35" = list("age_set" = "20+", "ages" = c(30, 35, 40)),
                  "age40" = list("age_set" = "20+", "ages" = c(35, 40, 45)),
                  "age45" = list("age_set" = "20+", "ages" = c(40, 45, 50)),
                  "age50" = list("age_set" = "20+", "ages" = c(45, 50, 55)),
                  "age55" = list("age_set" = "20+", "ages" = c(50, 55, 60)),
                  "age60" = list("age_set" = "20+", "ages" = c(55, 60, 65)),
                  "age65" = list("age_set" = "20+", "ages" = c(60, 65, 70)),
                  "age70" = list("age_set" = "20+", "ages" = c(65, 70, 75)),
                  "age75" = list("age_set" = "20+", "ages" = c(70, 75, 80)),
                  "age80" = list("age_set" = "20+", "ages" = c(75, 80, 85)),
                  "age85" = list("age_set" = "20+", "ages" = c(80, 85)))

# Combine frames
pops_under_20[, age_set := "under20"]
ps_frame_clean[, age_set := "20+"]
ps_frame_clean <- rbindlist(list(pops_under_20, ps_frame_clean), use.names = TRUE, fill = TRUE)

# Copy ps_frame for age pooling
ps_frame_age_pooled <- copy(ps_frame_clean)
setnames(ps_frame_age_pooled, 'value', 'value_age_pooled')

# Pool over ages
ps_frame_age_pooled <- pool_adj_ages(data = ps_frame_age_pooled, 
                                     age_pools = age_pools, 
                                     id_vars = c("year", area_var, "race", "sex", "state", "state_name", "age_set", strat_vars), 
                                     pop_var = 'value_age_pooled')

stopifnot(nrow(ps_frame_clean[!(age == 25 & age_set == "under20")]) == nrow(ps_frame_age_pooled))

# Merge pooled frame onto original PS Frame
ps_frame_clean <- merge(ps_frame_clean[!(age == 25 & age_set == "under20")],
                        ps_frame_age_pooled,
                        by = c("year", area_var, "age", "race", "sex", "state", "state_name", strat_vars, "age_set"),
                        all = T)

#### Set post-stratification weights for strata with zero aggregation weight, using cascade strategy
# Set raw weights
ps_frame_clean[, value_total := sum(value_age_pooled), by = c(area_var, "year", "age", "race", "sex", "state", "state_name", "age_set")]
ps_frame_clean[, c("wt0", "wt") := as.numeric(value_age_pooled / value_total)]

# Third level of the cascade (drop year, and use age-pooled values)
ps_frame_clean[, value3 := sum(value_age_pooled), by = c(area_var, "race", "sex", "state", "state_name", strat_vars, "age", "age_set")]
ps_frame_clean[, value_total3 := sum(value_age_pooled), by = c(area_var, "sex", "race", "state", "state_name", "age", "age_set")]
ps_frame_clean[, wt3 := value3 / value_total3]

# Fourth level of the cascade (drop year and mcnty [but retain state], and use age-pooled values)
ps_frame_clean[, value4 := sum(value_age_pooled), by = c("race", "sex", "state", "state_name", strat_vars, "age", "age_set")]
ps_frame_clean[, value_total4 := sum(value_age_pooled), by = c("race", "sex", "state", "state_name", "age", "age_set")]
ps_frame_clean[, wt4 := value4 / value_total4]

# Fifth level of the cascade (drop year, mcnty and state, and use age-pooled values)
ps_frame_clean[, value5 := sum(value_age_pooled), by = c("race", "sex", strat_vars, "age", "age_set")]
ps_frame_clean[, value_total5 := sum(value_age_pooled), by = c("race", "sex", "age", "age_set")]
ps_frame_clean[, wt5 := value5 / value_total5]

ps_frame_clean[value_total >= 20, use_version := 1]
ps_frame_clean[value_total < 20, use_version := 3]
ps_frame_clean[value_total3 < 20, use_version := 4]
ps_frame_clean[value_total4 < 20, use_version := 5]
ps_frame_clean[, use_version := max(use_version), by = c('marital', 'edu', 'race', 'age', 'sex', 'mcnty')]

ps_frame_clean[, wt := ifelse(use_version == 1, wt,
                              ifelse(use_version == 3, wt3,
                                     ifelse(use_version == 4, wt4,
                                            ifelse(use_version == 5, wt5, NA))))]

saveRDS(ps_frame_clean, file = paste0(joint_dir, date_time_stamp, '/ps_frame_full_age_pooled_except_20_25.rds'))

ps_frame <- ps_frame_clean[, c('marital', 'edu', 'race', 'age', 'sex', 'year', 'mcnty', 'value', 'state', 'state_name', 'wt', "age_set")]
setnames(ps_frame, 'value', 'original_value')

#### Recalculate value
ps_frame[, total_value := sum(original_value), by = c("race", "age", "sex", "year", "mcnty", "age_set")]
ps_frame[total_value == 0, total_value := 1e-12]
ps_frame[, value := total_value * wt]

### Recalculate age-pooled values
ps_frame_age_pooled_recalculated <- pool_adj_ages(data = copy(ps_frame),
                                                  age_pools = age_pools,
                                                  id_vars = c("year", area_var, "race", "sex", "state", "state_name", "age_set", strat_vars),
                                                  pop_var = 'value')

setnames(ps_frame_age_pooled_recalculated, 'value', 'value_age_pooled')

# Merge pooled frame onto original PS Frame
ps_frame <- merge(ps_frame[!(age == 20 & age_set == "under20")],
                  ps_frame_age_pooled_recalculated[!(age == 20 & age_set == "under20")],
                  by = c("year", area_var, "age", "race", "sex", "state", "state_name", strat_vars, "age_set"),
                  all = T)

stopifnot(ps_frame[, sum(wt), by = c("mcnty", "year", "sex", "age", "race")][, max(abs(V1 - 1))] < 1e-10)

### Save final clean PS frames
saveRDS(ps_frame[, -c("total_value", "original_value", "age_set")], 
        file = paste0(joint_dir, date_time_stamp, '/ps_frame_age_pooled.rds'))
