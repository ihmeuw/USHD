###########################################################################################################################################
## Description: Determine state-specific population proportions represented by each merged county.
##
## Input:      Populations by race/ethnicity
## Output:     Aggregation weights for each merged county.
##
###########################################################################################################################################

######## 1. Setup
#### Load needed packages
pacman::p_load(data.table, ggplot2, rgdal, rgeos, raster)

###### Source functions
repo <- paste0("FILEPATH")
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

## load database central functions
library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                      R.version$major,
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
ushd_client$save_covariate_population  # running this line ensures successful database connection and prevents interference from other packages


#### Set output folder
(data_version <- make_time_stamp())
output_dir <- paste0("FILEPATH")

dir.create(output_dir)

#### Read in data
locs <- fread("FILEPATH")
locs[, cnty := sprintf("%05d", cnty)]
ps_path <- "FILEPATH"
ps_path_under20 <- "FILEPATH"
ps_frame <- rbindlist(list(readRDS(ps_path), readRDS(ps_path_under20)), use.names = TRUE, fill = TRUE)

#### Add state names
if (is.null(ps_frame$state)) {
  ps_frame <- merge(ps_frame, unique(locs[, list(mcnty, state)]), by = "mcnty", all.x = TRUE)
}


######## 2. Derive aggregation weights
#### Derive state-level populations by year-sex-age-race-edu-marital
state_pops <- ps_frame[, list(state_pop = sum(value), state_pop_age_pooled = sum(value_age_pooled)), by = list(state, year, sex, age, race, edu, marital)]

#### Merge state pops onto mcnty pops
pops <- merge(ps_frame[, c("marital", "edu", "race", "age", "sex", "year", "mcnty", "value", "value_age_pooled", "state")], state_pops, by = c("state", "year", "sex", "age", "race", "edu", "marital"), all = TRUE)
setnames(pops, c("value", "value_age_pooled"), c("pop", "pop_age_pooled"))

#### Calculate aggregation weights
pops[, agg_wt := pop / state_pop]

#### Set post-stratification weights for strata with zero aggregation weight, using cascade strategy
# Set raw weights
pops[, c("wt0", "wt") := list(agg_wt, agg_wt)]

# Second level of the cascade (drop year)
pops[, pop2 := sum(pop), by = c("mcnty", "state", "age", "race", "sex", "edu", "marital")]
pops[, state_pop2 := sum(pop), by = c("state", "age", "race", "sex", "edu", "marital")]
pops[, wt2 := pop2 / state_pop2]

# Third level of the cascade (drop year, and use pooled age groups)
pops[, pop3 := sum(pop_age_pooled), by = c("mcnty", "state", "race", "sex", "edu", "marital", "age")]
pops[, state_pop3 := sum(pop_age_pooled), by = c("state", "sex", "race", "edu", "marital", "age")]
pops[, wt3 := pop3 / state_pop3]

# Fourth level of the cascade (drop year and race, and use pooled age groups)
pops[, pop4 := sum(pop_age_pooled), by = c("mcnty", "state", "sex", "edu", "marital", "age")]
pops[, state_pop4 := sum(pop_age_pooled), by = c("state", "sex", "edu", "marital", "age")]
pops[, wt4 := pop4 / state_pop4]

# Fifth level of the cascade (drop year, race, and sex)
pops[, pop5 := sum(pop_age_pooled), by = c("mcnty", "state", "age", "edu", "marital")]
pops[, state_pop5 := sum(pop_age_pooled), by = c("state", "age", "edu", "marital")]
pops[, wt5 := pop5 / state_pop5]

# Sixth level of the cascade (drop year, race, sex, and age)
pops[, pop6 := sum(pop_age_pooled), by = c("mcnty", "state", "edu", "marital")]
pops[, state_pop6 := sum(pop_age_pooled), by = c("state", "edu", "marital")]
pops[, wt6 := pop6 / state_pop6]

pops[state_pop >= 20, "use_version" := 1]
pops[state_pop < 20, c("agg_wt", "use_version") := list(wt2, 2)]
pops[state_pop2 < 20, c("agg_wt", "use_version") := list(wt3, 3)]
pops[state_pop3 < 20, c("agg_wt", "use_version") := list(wt4, 4)]
pops[state_pop4 < 20, c("agg_wt", "use_version") := list(wt5, 5)]
pops[state_pop5 < 20, c("agg_wt", "use_version") := list(wt6, 6)]

stopifnot(nrow(pops[is.na(agg_wt)]) == 0)
stopifnot(nrow(pops[state_pop6 < 20]) == 0)
stopifnot(pops[, sum(agg_wt), by = c("state", "year", "sex", "age", "race", "edu", "marital")][, max(abs(V1 - 1))] < 1e-10)

#### Cleanup
pops[, c("use_version", "wt", "wt0", "pop_age_pooled", "state_pop_age_pooled", "pop2", "state_pop2", "wt2", "pop3", "state_pop3", "wt3", "pop4", "state_pop4", "wt4", "pop5", "state_pop5", "wt5", "pop6", "state_pop6", "wt6") := NULL]

#### Set total_pop to tiny value (1e-12) where this is zero
pops[state_pop == 0, state_pop := 1e-12]

#### Set population
pops[, pop := state_pop * agg_wt]

#### Check that weights sum to 1
stopifnot(pops[, sum(agg_wt), by = c("state", "year", "sex", "age", "race", "edu", "marital")][, max(abs(V1 - 1))] < 1e-10)

#### Check for any missingness
stopifnot(nrow(pops) == nrow(pops[complete.cases(pops)]))


######## 3. Save outputs
saveRDS(pops[, -c("state_pop")], paste0(output_dir, "mcnty_state_agg_wts.rds"))

sink(paste0(output_dir, "/input_versions.txt"))
print(paste("PS frame: ", ps_path))
print(paste("PS frame under-20: ", ps_path_under20))
sink()