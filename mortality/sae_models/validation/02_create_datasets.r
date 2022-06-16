####################################################################################################
## Description: Create sampled-down datasets for validating life expectancy models. This is done by
##              sampling from the observed age-sex-race distribution in each county-year to get a
##              similar but smaller population and then sampling from a Poisson distribution using
##              the observed death rate in a given county-age-sex-race-year to get the number of
##              deaths for this smaller population. Two datasets are then saved: one in which the
##              "sampled down" populations and deaths are only used for counties in the validation
##              set (observed population and deaths used everywhere else) and one where "sampled
##              down" populations and deaths are used for all counties. In either case only
##              counties where population is bigger than the sampling down size are sampled down.
##              This procedure is repeated 10 times for each set sampling size.
##
## Requires:    all cause mortality death counts by merged county, age group, sex, and year
##              population counts by merged county, age group, sex, and year
##
## Inputs:      dir [character]         the directory for all models. It pulls deaths_file and
##                                      pop_file from the settings.csv in this dir. It should also
##                                      have validation settings included, such as val_iter.
##              output_dir [character]
##                                      optional argument for a directory to add to the filepath
##                                      of outputs to help distinguish it from other
##                                      validation_sets with different settings. Should just be
##                                      one directory, not the full path.
##
##
## Outputs:     population and death files where only counties in the validation set are sampled
##                down -- ("deaths_[size]_[iter].rds" and "pop_[size]_[iter].rds" in the
##                "sample_down_val_set_only" folder)
##              population and death files where all counties larger than the set sample size are
##                sampled down -- ("deaths_[size]_[iter].rds" and "pop_[size]_[iter].rds in the
##                "sample_down_all" folder)
##              text file with all the settings and inputs that specify the other outputs --
##              02_create_datasets_parameters.md
####################################################################################################

library(R.utils)
library(data.table)
sourceDirectory("functions/")

set.seed(98121)

## External arguements -----------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
dir <- args[1] # modeling directory
output_dir <- args[2] # optional extra directory to differentiate between runs with different settings

## Get settings ------------------------------------------------------------------------------------
get_settings(dir)

## Save message on the passed in parameters/arguments ----------------------------------------------

final_output_base_dir <- paste0("FILEPATH", "FILEPATH", output_dir, "/")

# message template
msg <- "
parameters:

dir <- %s
output_dir <- %s

%s is used to load settings. deaths_file and pop_file are read from there. These files are used to create the datasets

- pop_file: %s
- deaths_file: %s

outputs are saved to %s

"

# update message with values
msg <- sprintf(msg, dir, output_dir, dir, pop_file, deaths_file, final_output_base_dir)

# print / display message
cat(msg)

# save message
cat(msg,
    file =  paste0(final_output_base_dir, "/02_create_datasets_parameters.md"),
    sep="\n")

## Main script  ------------------------------------------------------------------------------------

# identify counties in the validation set
load(paste0(final_output_base_dir, "/validation_set_gs.rdata"))

# inputs holds all the deaths / pop files this script makes
if (!file.exists(paste0(final_output_base_dir, "/inputs/"))) {
  dir.create(paste0(final_output_base_dir, "/inputs/"), recursive = T)
}

val_set_only_dir <- paste0(final_output_base_dir, "/inputs/sampled_down_val_set_only/")
if(!file.exists(val_set_only_dir)) dir.create(val_set_only_dir, recursive = T)

all_dir <- paste0(final_output_base_dir, "/inputs/sampled_down_all/")
if(!file.exists(all_dir)) dir.create(all_dir, recursive = T)

sizes <- c(10, 100, 1000, 3000, 5000, 10000, 25000, 100000)

# load and combine deaths and population
pop <- readRDS(pop_file)
deaths <- readRDS(deaths_file)

# filter to all-cause deaths
if ("acause" %in% names(deaths)) {
  deaths <- deaths[acause == "_all", ]
} else if ("cause_id" %in% names(deaths)) {
  deaths <- deaths[cause_id == 294, ]
}
# else, assume deaths is already all-cause

# filter to years in pop because nulls in data will be assumed to be deaths of zero
pop <- pop[year %in% unique(deaths$year)]

if (!by_race) {
  if (all_pop_id %in% unique(pop$race) | all_pop_id %in% unique(deaths$race)) {
    stop(paste0("Race ",all_pop_id," cannot already be present"))  # avoid duplicating deaths
  }
  pop[, race := all_pop_id]
  deaths[, race := all_pop_id]
}

data <- merge(pop[, list(pop = sum(pop)), by = "mcnty,year,sex,age,race"],
              deaths[, list(deaths = sum(deaths)), by = "mcnty,year,sex,age,race"],
              by = c("mcnty", "year", "sex", "age", "race"), all = T)
data[is.na(deaths), deaths := 0]

data[, mcnty := as.integer(mcnty)]
data[, year := as.integer(year)]
data[, sex := as.integer(sex)]
data[, age := as.integer(age)]
data[, race := as.integer(race)]

# add cause information
data[, cause_id := 294]
data[, acause := "_all"]

# add population if necessary to make sure deaths <= pop
start_pop <- data[, sum(pop)]
data[deaths > pop, pop := deaths]
data[, sum(pop)] - start_pop

gs <- unique(gs_mx[, list(mcnty, race, gs = T)])
data <- merge(data, gs, by = c("mcnty", "race"), all.x = T)
data[is.na(gs), gs := F]
stopifnot(!any(is.na(data))) 

setkeyv(data, c("mcnty", "year", "sex", "age", "race"))
data[, sex_age := factor(paste(sex, age))]

# loop through sample sizes and iterations
for (size in sizes) {

  for (iter in 1:val_iter) {
    cat(paste0("\nSize ", size, ": \n")); flush.console()
    cat(paste0(iter, "\n")); flush.console()

    # pull out counties that are already smaller than "size"
    hold <- copy(data[, sum(pop), by = "race,mcnty,year"][V1 <= size, list(race, mcnty, year)])  # race county year where total pop is less than size
    setkeyv(hold, c("race", "mcnty", "year"))

    hold <- merge(hold, data, by = c("race", "mcnty", "year"), all.x=T)
    hold[, is_in_hold := 1]
    test <- hold[, list(pop = sum(pop)), by = "race,mcnty,year"]
    stopifnot(max(test[, pop]) <= size)  # pop cannot be larger than size

    # only want the race-county-year- that are NOT in hold
    samp <- merge(data, unique(hold[, list(race, mcnty, year, is_in_hold)]),
                  by=intersect(names(hold[, list(race, mcnty, year, is_in_hold)]), names(data)),
                  all.x=T)
    samp[is.na(is_in_hold), is_in_hold := 0]
    stopifnot(!any(is.na(samp)))
    # filter to the  race-county-year- that are NOT in hold
    samp <- samp[is_in_hold == 0, ]

    samp[, is_in_hold := NULL]
    hold[, is_in_hold := NULL]

    stopifnot(nrow(samp) + nrow(hold) == nrow(data))

    # sample down the population and then deaths
    # this line samples pairs of sex and age (the sex_age column) from each
    # mcnty-, year-, race- group with probability determined by the pop column
    # and then it counts the number of times the sex-age pair was sampled with
    # table() and then converts to a number
    samp[, pop1 := as.numeric(table(sample(x = sex_age, size = size, prob = pop, replace = T))), by = "mcnty,year,race"]
    test <- samp[, list(pop1 = sum(pop1)), by = "race,mcnty,year"]
    stopifnot(max(test[, pop1]) <= size)  # pop cannot be larger than size

    samp[pop > 0, death1 := pmin(pop1, rpois(.N, pop1 * deaths / pop))]
    samp[pop == 0, death1 := 0]

    stopifnot("cause_id" %in% names(samp))
    stopifnot("cause_id" %in% names(hold))
    stopifnot("cause_id" %in% names(data))

    stopifnot("acause" %in% names(samp))
    stopifnot("acause" %in% names(hold))
    stopifnot("acause" %in% names(data))

    # save datasets where only the validation set is sampled down
    pop <- rbind(hold[, list(mcnty, year, sex, age, race, pop)],
                 samp[, list(mcnty, year, sex, age, race, pop = ifelse(gs, pop1, pop))])
    setkeyv(pop, c("mcnty", "year", "sex", "age", "race"))

    saveRDS(pop, file = paste0(val_set_only_dir, "/pop_", format(size, scientific = F), "_", iter, ".rds"))

    deaths <- rbind(hold[, list(mcnty, year, sex, age, race, deaths)],
                    samp[, list(mcnty, year, sex, age, race, deaths = ifelse(gs, death1, deaths))])
    setkeyv(deaths, c("mcnty", "year", "sex", "age", "race"))
    saveRDS(deaths, file = paste0(val_set_only_dir, "/deaths_", format(size, scientific = F), "_", iter, ".rds"))

    # save datasets where all counties are sampled down
    pop <- rbind(hold[, list(mcnty, year, sex, age, race, pop)],
                 samp[, list(mcnty, year, sex, age, race, pop = pop1)])
    test <- pop[, list(pop = sum(pop)), by = "race,mcnty,year"]
    stopifnot(max(test[, pop]) <= size)  # pop cannot be larger than size
    setkeyv(pop, c("mcnty", "year", "sex", "age", "race"))
    saveRDS(pop, file = paste0(all_dir, "/pop_", format(size, scientific = F), "_", iter, ".rds"))

    deaths <- rbind(hold[, list(mcnty, year, sex, age, race, deaths)],
                    samp[, list(mcnty, year, sex, age, race, deaths = death1)])
    setkeyv(deaths, c("mcnty", "year", "sex", "age", "race"))
    saveRDS(deaths, file = paste0(all_dir, "/deaths_", format(size, scientific = F), "_", iter, ".rds"))

    rm(hold, samp, pop, deaths); gc()
  }
}
