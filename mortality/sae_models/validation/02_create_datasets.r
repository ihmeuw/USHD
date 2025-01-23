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
##              This procedure is repeated 10 times for each set sampling size. A note: the end
##              is where the validation set is used to choose which population/deaths is saved: the
##              sampled down versions or un-sampled versions.
##
## Requires:    all cause mortality death counts by merged county, age group, sex, and year
##              population counts by merged county, age group, sex, and year
##
## Inputs:      dir [character]         the directory for all models. It pulls deaths_file and
##                                      pop_file from the settings.csv in this dir. It should also
##                                      have validation settings included, such as val_iter.
##              output_dirname [character]
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

stopifnot(grepl("mortality/sae_models$", getwd()))
suppressMessages(suppressWarnings({
  library(R.utils)
  library(data.table)
  library(glue)
  R.utils::sourceDirectory('functions', modifiedOnly = FALSE)
}))

## External arguements -----------------------------------------------------------------------------
if (interactive()) {
  dir <- "FILEPATH"
  output_dirname <- "2024_02_21_validation_no_age_mcnty_race_effect"
} else {
  args <- commandArgs(trailingOnly = TRUE)
  dir <-
    args[1] # "FILEPATH"
  output_dirname <-
    args[2] # optional extra directory to differentiate between runs with different settings
}


## Get settings ------------------------------------------------------------------------------------
get_settings(dir)

if (misclassification_correction) {
  stop("Misclassification correction shouldn't be set to true.")
}

# This is the basis of our output locations. We'll make a number of dirs under this location.
final_output_base_dir <-
  file.path(
    "FILEPATH",
    "FILEPATH",
    output_dirname
  )

## Save message on the passed in parameters/arguments ----------------------------------------------

# message template
msg <- glue(
  "
parameters:

dir <- '{dir}'
output_dirname <- '{output_dirname}'

{dir} is used to load settings. deaths_file and pop_file are read from there. These files are used to create the datasets

- pop_file: {pop_file}
- deaths_file: {deaths_file}
- years: {min(years)}-{max(years)}

outputs are saved to {final_output_base_dir}
"
)

# print / display message
message(msg)

# save message
if (!interactive()) {
  cat(
    msg,
    file =  file.path(
      final_output_base_dir,
      "/02_create_datasets_parameters.md"
    ),
    sep = "\n"
  )
}
## Main script  ------------------------------------------------------------------------------------

# Identify counties in the validation set. We only need one of the validation sets, not both, since
# we're just using the demographic information in them.
gs <- readRDS(file.path(final_output_base_dir, "validation_set", "validation_set_gs_mx.rds"))

## directories  ------------------------------------------------------------------------------------

# Inputs holds all the deaths / pop files this script makes. By inputs we mean inputs to the models,
# not inputs to this script.
inputs_dir <- file.path(final_output_base_dir, "inputs")

if (!dir.exists(inputs_dir)) {
  dir.create(inputs_dir, recursive = T)
}

val_set_only_dir <-
  file.path(final_output_base_dir, "inputs", "sampled_down_val_set_only")

if (!dir.exists(val_set_only_dir)) {
  dir.create(val_set_only_dir, recursive = T)
}

all_dir <-
  file.path(final_output_base_dir, "inputs", "sampled_down_all")

if (!dir.exists(all_dir)) {
  dir.create(all_dir, recursive = T)
}

# prepare population --------------------------------------------------------------------------
pop <- readRDS(gsub(pattern = "FILEPATH" , replacement = "/FILEPATH", x = pop_file))

# filter to years in pop and deaths because nulls in data will be assumed to be deaths of zero

pop <- pop[year %in% years & age %in% ages]

if (!by_race) {
  pop[, race := all_pop_id]
}
if (!by_edu) {
  pop[, edu := all_pop_id]
}

# sometimes, in education modeling, the pop file is squared across all education groups, including
# unknown education. When this happens, we end up with a lot of extra rows of data. We don't need
# the unknown education from the pop side since it is all zeros. That is, there is no unknown
# education in population/births. So, if this is and education modeling run, and if there is unknown
# education in the pop_file, and if all the pop in those rows are zero, completely remove all rows
# where education is unknown. This probably happens because of a squaring step in earlier code. Once
# deaths and pop are merged together (via outer merge) we'll still set rows where pop is missing
# (i.e., rows where deaths have unknown education) to zero.
if (by_edu) {
  if (all(pop[edu == 100, pop] == 0)) {
    pop <-
      pop[edu != 100,] # drop unknown education if there is zero pop in it
  } else {
    stop("data.table 'pop' has non-zero population in education group unknown education.")
  }
}

loc <- fread("FILEPATH/merged_counties.csv")
if (!"state" %in% names(pop)) {
  pop <- merge(pop, unique(loc[, .(mcnty, state)]), by = "mcnty", all.x = T)
}
stopifnot("state" %in% names(pop))


if (!by_race) pop[, race := all_pop_id]
if (!by_edu) pop[, edu := all_pop_id]
pop <- pop[, list(pop = sum(pop)), by = "mcnty,year,sex,age,race,edu,state"]

if (!by_race & by_edu & length(unique(pop$edu)) == 1) {
  stop("by_edu is TRUE, but there is only 1 unique edu value")
}

if (by_race & !by_edu & length(unique(pop$race)) == 1) {
  stop("by_race is TRUE, but there is only 1 unique race value")
}

if (!by_race & !by_edu & (length(unique(pop$race)) > 1 | length(unique(pop$edu)) > 1)) {
  stop("aggregation to all-race/all-edu did not work! there is more than 1 race/edu value present")
}


## Prepare deaths-----------------------------------------------------------------------------------

deaths <- readRDS(deaths_file)

# filter to all-cause deaths
if ("acause" %in% names(deaths)) {
  deaths <- deaths[acause == "_all", ]
} else if ("cause_id" %in% names(deaths)) {
  deaths <- deaths[cause_id == 294, ]
} # else, assume deaths is already all-cause

if ("edu_label" %in% names(deaths)) {
  deaths[, edu_label := NULL]
}

deaths <- deaths[year %in% years & age %in% ages]

gc() # dropping other causes of death removes a lot of rows, forcing gc frees up a ton of memory

if (!("race" %in% names(deaths))) {
  deaths[, race := all_pop_id]
}
if (!("edu" %in% names(deaths))) {
  deaths[, edu := all_pop_id]
}

if (!by_race) deaths[, race := all_pop_id]
if (!by_edu) deaths[, edu := all_pop_id]
deaths <- deaths[, list(deaths = sum(deaths)), by = "mcnty,year,sex,race,edu,age"]

if (!by_race & by_edu & length(unique(deaths$edu)) == 1) {
  stop("by_edu is TRUE, but there is only 1 unique edu value")
}

if (by_race & !by_edu & length(unique(deaths$race)) == 1) {
  stop("by_race is TRUE, but there is only 1 unique race value")
}

if (!by_race & !by_edu & (length(unique(deaths$race)) > 1 | length(unique(deaths$edu)) > 1)) {
  stop("aggregation to all-race/all-edu did not work! there is more than 1 race/edu value present")
}
gc()

# Create data ---------------------------------------------------------------------------------

data <-
  merge(
    pop[, list(pop = sum(pop)), by = "mcnty,year,sex,age,race,edu"],
    deaths[, list(deaths = sum(deaths)), by = "mcnty,year,sex,age,race,edu"],
    by = c("mcnty", "year", "sex", "age", "race", "edu"),
    all = T
  )

# These test the same thing many times just to get the point across.
if (nrow(data[is.na(pop) | is.na(deaths)]) == nrow(data)) {
  stop("Completely mismatched data! All rows have either NA pop or NA deaths.")
}
if (nrow(data[is.na(pop),]) + nrow(data[is.na(deaths),]) == nrow(data)) {
  stop("Completely mismatched data! All rows have either NA pop or NA deaths.")
}
data[is.na(deaths), deaths := 0]
if (any(data[is.na(pop)])) {
  stop("Pop has NA values and that is unusual.")
}
data[is.na(pop), pop := 0]

if (nrow(data[pop == 0 | deaths == 0]) == nrow(data)) {
  stop("Every single row has either a zero in deaths or pop")
}

gc()
# There will be NAs in data at this point, in places where education is unknown.
# if this is by_edu, add an indicator column that will allow state-years that are missing edu
# information to be dropped. This file is generated by calculating the % of deaths with Unknown
# education for the state-year, and then marking any state year with more that 30% Unknown as
# non-reporting

if (by_edu) {
  # Do this separately for infants and adults
  if (setequal(ages, c(0))) {
    # this file separates nyc from the rest of ny in some years. These lines check that it's safe to
    # ignore that distinction. It converts to a table that shows unique reporting status by year for
    # rows mentioning NYC (one reporting status per row). Then it counts the number of unique reporting
    # statuses in each year. If it's not 1, then there are different reporting statuses for NYC / NY
    # excl. NYC., and raises an error
    reporting_status_nyc = infants_non_reporting[state_alpha %like% "NYC", list(unique(reporting_status)), by = "year"]
    stopifnot(all(reporting_status_nyc[, .N, by = "year"][, N] == 1))

    # now it's safe to drop NYC. The above test means that the information in NY is the same as that
    # in NYC
    infants_non_reporting = infants_non_reporting[state_alpha != "NYC"]

    # merge state information onto data
    loc[, state_fips := state]
    setnames(loc, "mcnty", "area")
    loc = unique(loc[, list(area, state_fips)])

    # clean up unneeded columns from infants_non_reporting
    infants_non_reporting = infants_non_reporting[, list(state_fips, year, reporting_status)]

    # add mcnty (area) to infants_non_reporting
    infants_non_reporting <-
      merge(loc,
            infants_non_reporting,
            by = "state_fips",
            allow.cartesian = TRUE)

    # Check that each year has the correct number of counties
    check = infants_non_reporting[, .N, by = "year"]
    stopifnot(unique(check$N) == length(unique(loc$area)))

    # clean up
    infants_non_reporting[, state_fips := NULL]
    setnames(infants_non_reporting,
             "reporting_status",
             "edu_reporting_status")

    before = nrow(data)
    data <-
      merge(data,
            infants_non_reporting,
            by = c("area", "year"),
            all.x = T)
    stopifnot(nrow(data) == before) # check that merge with edu_reporting_status didn't change rows

  } else if (setequal(ages, seq(25, 85, 5))) {
    # For adult mortality, we want to drop the all data for a given state and year if that state
    # didn't report education information on death certificates for that year. This is more
    # complicated than simply dropping the deaths marked as Unknown education. Since we use location
    # of residence, not the location of the occurrence of the death, and since location of occurrence
    # dictates which state's death certificate is used, it is possible to have deaths with known
    # education in a state that did not report education. For this reason we drop all rows of data
    # in state-years that did not report education.

    starting_columns <- names(data)

    # want to excluded data by state and year, so need to get state information. State might
    # already be present due to merging population on, but, since there isn't any unknown edu in pop
    # there will be NA values for state. So, just merge state on.
    if ("state" %in% names(data)) {
      data[, state := NULL]
    }
    mcnty <-
      fread('FILEPATH/merged_counties.csv')
    data <-
      merge(
        data,
        unique(mcnty[, .(mcnty, state)]),
        by = "mcnty",
        all.x = T
      )


    # Calculate the % of deaths marked as unknown. This will allow us to drop whole state years, and
    # also check for unknown edu deaths that we missed.
    reporting_table <-
      data[, list(deaths_num = sum(deaths)), by = "year,state,edu"]
    reporting_table[, deaths_denom := sum(deaths_num), by = "year,state"]
    reporting_table[, prop := deaths_num / deaths_denom]

    if (nrow(reporting_table[edu == 100,]) == 0)
      stop("There is no data with unknown edu for some reason.")

    # filter down to just unknown
    reporting_table <- reporting_table[edu == 100,]

    # at this point, there will only be a handful of states left.

    print(
      glue(
        "The smallest proportion of deaths in unknown-education in any state-year is {round(min(reporting_table$prop), digits = 4)}"
      )
    )

    # Use prop to mark rows to be dropped. the props were calculated by edu, but if the prop is
    # big enough for the Unknown edu, need to mark all edu groups in that state-year to be dropped.
    
    # If imputation didn't work, then after applying the non-reporting condition, there would still
    # be deaths with unknown education. If prop is greater than .9, mark the row as 0, meaning not
    # reported, else, mark as 1, for yes reporting.
    reporting_table[, reporting_status := ifelse(prop > 0.9, 0, 1)]

    # drop edu because we want to repeat the reporting_status value across edu
    reporting_table[, edu := NULL]

    # save out to the data dir
    fwrite(reporting_table,
           file = file.path(final_output_base_dir, "adult_edu_reporting_status.csv"))

    # attach this table back onto data, and fill in anything missing as 1, since if it's missing,
    # the % unknown edu was small enough
    data <-
      merge(data,
            reporting_table[, .(state, year, reporting_status)],
            by = c("state", "year"),
            all.x = T)
    data[is.na(reporting_status), reporting_status := 1]

    # checFILEPATH if the number of unique rows of reporting_status, year, and state, is equal to the
    # number of unique year and state combinations, then this worked: only one value for
    # reporting_status in any state-year pair
    stopifnot(nrow(unique(data[, .(year, state, reporting_status)])) ==
                (length(unique(data$state)) * length(unique(data$year))))

    # add the new column to the starting columns, so that this column is the only one added
    starting_columns <- c(starting_columns, "reporting_status")

    # return data to its original format: drop extra columns (besides reporting_status)
    data <- data[, starting_columns, with = F]

    # change the name
    setnames(data, "reporting_status", "edu_reporting_status")

    rm(reporting_table)

  } else {
    stop(glue("Unhandled case. Check ages. ages is set to c({paste(ages, collapse = ', ')})"))
  }
}
gc()


if (any(is.na(data))) {
  stop("There are NAs in data.")
}


data[, mcnty := as.integer(mcnty)]
data[, year := as.integer(year)]
data[, sex := as.integer(sex)]
data[, age := as.integer(age)]
data[, race := as.integer(race)]
data[, edu := as.integer(edu)]

# add cause information
data[, cause_id := 294]
data[, acause := "_all"]



gs <- unique(gs[, list(mcnty, race, edu, gs = T)])
data <- merge(data, gs, by = c("mcnty", "race", "edu"), all.x = T)
data[is.na(gs), gs := F]
stopifnot(!any(is.na(data)))  # don't expect any nulls

setkeyv(data, c("mcnty", "year", "sex", "race", "edu", "age"))
data[, sex_age := factor(paste(sex, age))]

# loop through sample sizes and iterations
for (size in val_sizes) {
  for (iter in 1:val_iter) {
    message(glue("Size: {size}"))
    message(glue("iteration: {iter}"))

    # pull out counties that are already smaller than "size"
    
    hold <-
      copy(data[, sum(pop), by = "race,edu,mcnty,year"][V1 <= size, list(race, edu, mcnty, year)])  # race county year where total pop is less than size
    setkeyv(hold, c("race", "edu", "mcnty", "year")) 

    # Ensure that 1) everything being withheld is
    # smaller than the population threshold size and 2) that we've correctly filtered out withheld
    # data.
    hold <-
      merge(hold,
            data,
            by = c("race", "edu", "mcnty", "year"),
            all.x = T)
    hold[, is_in_hold := 1]
    test <- hold[, list(pop = sum(pop)), by = "race,edu,mcnty,year"]
    stopifnot(max(test[, pop]) <= size)  # pop cannot be larger than size
    # hold <- copy(data[hold, ])

    
    # only want the race-county-year- that are NOT in hold
    samp <-
      merge(data,
            unique(hold[, list(race, edu, mcnty, year, is_in_hold)]),
            by = c("race", "edu", "mcnty", "year"),
            all.x = T)
    samp[is.na(is_in_hold), is_in_hold := 0] # create indicator column
    stopifnot(!any(is.na(samp)))
    # filter to the  race-county-year- that are NOT in hold
    samp <- samp[is_in_hold == 0, ]

    # no longer need this indicator column
    samp[, is_in_hold := NULL]
    hold[, is_in_hold := NULL]

    stopifnot(nrow(samp) + nrow(hold) == nrow(data)) # assure that rows have been split correctly

    # Sample down the population and then deaths. This line samples pairs of sex and age (the
    # sex_age column) from each mcnty-, year-, race-/edu- group with probability determined by the
    # pop column and then it counts the number of times the sex-age pair was sampled with table()
    # and then converts to a number
    samp[, pop1 := as.numeric(table(sample(
      x = sex_age,
      size = size,
      prob = pop,
      replace = T
    ))), by = "mcnty,year,race,edu"]
    test <- samp[, list(pop1 = sum(pop1)), by = "race,edu,mcnty,year"]
    stopifnot(max(test[, pop1]) <= size)  # pop cannot be larger than size

    samp1 <- data.table::copy(samp)

    # I think that pmin ensures that if the result from rpois is bigger than pop1, then pop1 will be
    # used for deaths. This would keep deaths from being larger than pop.
    samp[pop > 0, death1 := pmin(pop1, rpois(.N, pop1 * deaths / pop))] # compute deaths
    samp[pop == 0, death1 := 0] 

    # This is a test to see that running the sampling of deaths makes new results with each iteration
    samp1[pop > 0, death1 := pmin(pop1, rpois(.N, pop1 * deaths / pop))] # compute deaths
    samp1[pop == 0, death1 := 0]

    all_equal_result <- all.equal(samp, samp1)
    # all.equal returns a string if things are different, so covert that to a FALSE
    all_equal_result <- identical(all_equal_result, TRUE)
    if (all_equal_result) {
      stop("deaths1 doesn't vary between iterations")
    }
    rm(samp1); gc()



    stopifnot("cause_id" %in% names(samp))
    stopifnot("cause_id" %in% names(hold))
    stopifnot("cause_id" %in% names(data))

    stopifnot("acause" %in% names(samp))
    stopifnot("acause" %in% names(hold))
    stopifnot("acause" %in% names(data))

    # Save datasets where only the validation set is sampled down. The if else chooses which
    # population to use. If in the validation set (gold standard, gs), then use the simulated
    # population pop1, else, use the normal population pop
    pop <- rbind(hold[, list(mcnty, year, sex, age, race, edu, pop)],
                 samp[, list(mcnty, year, sex, age, race, edu, pop = ifelse(test = gs, yes = pop1, no = pop))])
    setkeyv(pop, c("mcnty", "year", "sex", "race", "edu", "age"))
    saveRDS(pop, file = file.path(val_set_only_dir, glue("pop_{format(size, scientific = F)}_{iter}.rds")))

    deaths <-
      rbind(hold[, list(mcnty, year, sex, age, race, edu, deaths)],
            samp[, list(mcnty, year, sex, age, race, edu, deaths = ifelse(test = gs, yes = death1, no = deaths))])
    setkeyv(deaths, c("mcnty", "year", "sex", "race", "edu", "age"))

    saveRDS(deaths, file = file.path(val_set_only_dir, glue("deaths_{format(size, scientific = F)}_{iter}.rds")))

    # save datasets where all counties are sampled down. That is, only use the sampled pop and deaths
    pop <- rbind(hold[, list(mcnty, year, sex, age, race, edu, pop)],
                 samp[, list(mcnty, year, sex, age, race, edu, pop = pop1)])
    test <- pop[, list(pop = sum(pop)), by = "race,edu,mcnty,year"]
    stopifnot(max(test[, pop]) <= size)  # pop cannot be larger than size
    setkeyv(pop, c("mcnty", "year", "sex", "race", "edu", "age"))

    saveRDS(pop, file = file.path(all_dir, glue("pop_{format(size, scientific = F)}_{iter}.rds")))

    deaths <-
      rbind(hold[, list(mcnty, year, sex, age, race, edu, deaths)],
            samp[, list(mcnty, year, sex, age, race, edu, deaths = death1)])
    setkeyv(deaths, c("mcnty", "year", "sex", "age", "race", "edu"))

    saveRDS(deaths,
            file = file.path(all_dir, glue("deaths_{format(size, scientific = F)}_{iter}.rds")))

    rm(hold, samp, pop, deaths, test)
    gc()
  }
}
message("DONE.")
