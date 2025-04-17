###########################################################################################################################################
## Description: Determine national population proportions represented by each merged county.
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


######## 2. Derive aggregation weights
#### Derive national populations by year-sex-age-race-edu-marital
natl_pops <- ps_frame[, list(natl_pop = sum(value), natl_pop_age_pooled = sum(value_age_pooled)), by = list(year, sex, age, race, edu, marital)]

#### Merge natl pops onto mcnty pops
pops <- merge(ps_frame[, c("marital", "edu", "race", "age", "sex", "year", "mcnty", "value", "value_age_pooled")], natl_pops, by = c("year", "sex", "age", "race", "edu", "marital"), all = TRUE)
setnames(pops, c("value", "value_age_pooled"), c("pop", "pop_age_pooled"))

#### Calculate aggregation weights
pops[, agg_wt := pop / natl_pop]


#### Set post-stratification weights for strata with zero aggregation weight, using cascade strategy
# Set raw weights
pops[, c("wt0", "wt") := list(agg_wt, agg_wt)]

# Second level of the cascade (drop year)
pops[, pop2 := sum(pop), by = c("mcnty", "age", "race", "sex", "edu", "marital")]
pops[, natl_pop2 := sum(pop), by = c("age", "race", "sex", "edu", "marital")]
pops[, wt2 := pop2 / natl_pop2]

pops[natl_pop >= 20, "use_version" := 1]
pops[natl_pop < 20, c("agg_wt", "use_version") := list(wt2, 2)]

table(pops$use_version)

stopifnot(nrow(pops[is.na(agg_wt)]) == 0)
stopifnot(nrow(pops[natl_pop2 < 20]) == 0)
stopifnot(pops[, sum(agg_wt), by = c("year", "sex", "age", "race", "edu", "marital")][, max(abs(V1 - 1))] < 1e-10)

#### Cleanup
pops[, c("use_version", "wt", "wt0", "pop_age_pooled", "natl_pop_age_pooled", "pop2", "natl_pop2", "wt2") := NULL]

#### Set total_pop to tiny value (1e-12) where this is zero
pops[natl_pop == 0, natl_pop := 1e-12]

#### Set population
pops[, pop := natl_pop * agg_wt]

#### Check that weights sum to 1
stopifnot(pops[, sum(agg_wt), by = c("year", "sex", "age", "race", "edu", "marital")][, max(abs(V1 - 1))] < 1e-10)

#### Check for any missingness
stopifnot(nrow(pops) == nrow(pops[complete.cases(pops)]))


######## 3. Save outputs
saveRDS(pops, paste0(output_dir, "mcnty_natl_agg_wts.rds"))

sink(paste0(output_dir, "/input_versions.txt"))
print(paste("PS frame: ", ps_path))
print(paste("PS frame under-20: ", ps_path_under20))
sink()
