####################################################################################################
## Description: Determine the "validation set" of counties or county-racial/ethnic group. This is
##              done by first pooling data across time with a sliding window, and then dropping all
##              counties that have a zero death count in any age-sex-year and then using simulation
##              on the remaining data to estimate the coefficient of variance for age-specific and
##              age-standardized mortality rates. The validation set is then selected as those
##              counties where the median coefficient of variance is less than a certain threshold
##              for age-specific mortality rates (across all years, sexes, and ages) and less than
##              a separate threshold for all-ages (e.g., crude and standardized) mortality rates.
##              Life tables and associated uncertainty measures are then estimated for these
##              counties as well.
##
##              Conditions:
##                - Condition 1:  After pooling data in a three year moving sum, drop all
##                  county-race/ethnicity groups that have a zero death count in any age-sex-year
##                - Condition 2: The median coefficient of variance is less than 1% for all-ages
##                  (i.e., crude and standardized) mortality rates.
##                - Condition 3: The median coefficient of variance is less than 10% for
##                  age-specific mortality rates (across all years, sexes, and ages)
##
## Requires:    all cause mortality death counts by merged county, age group, sex, and year
##              population counts by merged county, age group, sex, and year
##
## Inputs:      dir [character]         the directory for all models. It pulls deaths_file and
##                                      pop_file from the settings.csv in this dir.
##              n_years [integer]       the number of years to use for year pooling
##              pool_adjacent_counties [bool]
##                                      whether or not to pool adjacent counties
##              output_dirname [character]
##                                      optional argument for a directory to add to the filepath
##                                      of outputs to help distinguish it from other
##                                      validation_sets with different settings. Should just be
##                                      one directory, not the full path.
##              all_age_threshold [numeric]
##                                      threshold for condition 2
##              by_age_threshold [numeric]
##                                      threshold for condition 3
##              skip_min [boolean]      If TRUE will skip condition 1
##              skip_cov [boolean]      If TRUE will skip conditions 2 and 3
##
## Outputs:     a table of the selected counties with names and population counts for the most
##                recent year (validation_set_names.csv)
##              two data.tables of the selected county-years with the "gold standard" life
##                expectancy estimates and mortality rate estimates (validation_set_gs.rdata)
##
####################################################################################################

stopifnot(grepl("mortality/sae_models$", getwd()))
suppressMessages(suppressWarnings({
  library(R.utils)
  library(data.table)
  library(glue)
  R.utils::sourceDirectory('functions', modifiedOnly = FALSE)
}))


set.seed(98121)

## External arguments -----------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
dir <-
  args[1] # "FILEPATH"
n_years <- args[2]  # for year pooling
pool_adjacent_counties <-
  args[3]  # whether or not to pool adjacent counties
output_dirname <-
  args[4] # optional extra directory to differentiate between runs with different settings "by_race_3yr_space_pool"
all_age_threshold <- args[5]  # originally was 0.01
by_age_threshold <- args[6]  # originally was 0.1
skip_min <- args[7]  # If TRUE will not apply first condition
skip_cov <- args[8]  # If TRUE will not apply condtions 2 and 3
pool_prev_yrs <- as.logical(args[9])  # If TRUE pool the last n_years - 1, instead of years before and after each year
allow_mx_0 <- as.logical(args[10])


# default values for optional arguments
if (is.na(all_age_threshold)) {
  all_age_threshold <- 0.01
}
if (is.na(by_age_threshold)) {
  by_age_threshold <- 0.1
}
if (is.na(skip_min)) {
  skip_min <- FALSE
}
if (is.na(skip_cov)) {
  skip_cov <- FALSE
}

if (!is.null(output_dirname)) {
  output_dirname <- as.character(output_dirname)
} else {
  output_dirname <- ""
}

n_years <- as.integer(n_years)
pool_adjacent_counties <- as.logical(pool_adjacent_counties)
all_age_threshold <- as.numeric(all_age_threshold)
by_age_threshold <- as.numeric(by_age_threshold)
skip_min <- as.logical(skip_min)
skip_cov <- as.logical(skip_cov)

bas_dir = file.path(
  "FILEPATH",
  "FILEPATH"
)

final_output_dir <- file.path(base_dir, output_dirname)
FILEPATH_output_dir <- file.path("FILEPATH", output_dirname) # only used to store validation set county-subpop pairs and settings used to make them.

# make the dir if it does not exist (to suppress warning):
if (!dir.exists(final_output_dir)) {
  dir.create(final_output_dir,
             recursive = F)  # don't create full path, in case the filepath gets messed up
}

# make the dir if it does not exist (to suppress warning):
if (!dir.exists(file.path(final_output_dir, "validation_set"))) {
  dir.create(file.path(final_output_dir, "validation_set"),
             recursive = F)  # don't create full path, in case the filepath gets messed up
}

# make the dir if it does not exist (to suppress warning):
if (!dir.exists(file.path(final_output_dir, "intermediate_validation_set"))) {
  dir.create(file.path(final_output_dir, "intermediate_validation_set"),
             recursive = F)  # don't create full path, in case the filepath gets messed up
}

# also make a dir on FILEPATH:
if (!dir.exists(FILEPATH_output_dir)) {
  dir.create(FILEPATH_output_dir,
             recursive = F)
}

loc <- fread("FILEPATH/merged_counties.csv")

## Get settings ------------------------------------------------------------------------------------
get_settings(dir)

### Make sure that the required validation settings are present
# All of the validation settings will be checked before modeling, but some of those are added throughout the course of this script
# So, that is why we do not want to just run check_settings() at this point
if(!("val_types") %in% ls(envir = .GlobalEnv)) stop("'val_types' needs to be in the settings.csv")
if(!("val_iter") %in% ls(envir = .GlobalEnv)) stop("'val_iter' needs to be in the settings.csv")
if(!("val_sizes") %in% ls(envir = .GlobalEnv)) stop("'val_sizes' needs to be in the settings.csv")


## if pop_file is null, we need to add it to the settings
if(!is.null(pop_version)) {

  if(!is.null(pop_file)) stop("Both pop_file and pop_version cannot be specified")

  message(glue::glue("Using pop_version {pop_version} from database"))
  settings <- read.csv(paste0(dir, "/settings.csv"), stringsAsFactors = F, header = F)
  # save original settings
  write.table(
    settings,
    file = file.path(dir, "settings_OG.csv"),
    row.names = F,
    col.names = F,
    sep = ",",
    qmethod = "double"
  )

  pop <- get_population_data(covariate_dataset_id = pop_version)

  if (lt_hc) {
    if (by_race) {
      # only check this for race because for eau we don't have pop data back that far
      if (min(pop$year) > (min(years) - 10)) {
        stop("H-C method specified, but population version does not go back 10 previous years")
      }
    }
  }

  # Format
  pop <- pop[, .(race, mcnty, year, sex, age, state, edu, pop)]
  saveRDS(pop, file.path(
    dir,
    paste0("pop_file_cov_dataset_id_", pop_version, ".rds")
  ))

  # make a new row called pop_file in the settings and remove any old row
  if ("pop_file" %in% settings$V1) {
    message("Removing pop_file from settings.csv in order to replace it")
    settings <- settings[settings$V1 != "pop_file", ]
  }

  if ("pop_growth_file" %in% settings$V1) {
    message("Removing pop_growth_file from settings.csv in order to replace it")
    settings <- settings[settings$V1 != "pop_growth_file", ]
  }

  # Now add on new rows to the csv
  settings <- rbind(settings,
                    data.frame(
                      V1 = c("pop_file", "pop_growth_file"),
                      V2 = c(file.path(
                        dir,
                        paste0("pop_file_cov_dataset_id_", pop_version, ".rds")
                      ),
                      file.path(
                        dir,
                        paste0("pop_file_cov_dataset_id_", pop_version, ".rds")
                      ))
                    ))

  
  if ("pop_version" %in% settings$V1) {
    message("Removing pop_version from settings.csv so that settings won't have both pop_version and pop_file. pop_version will still be in settings_OG.csv.")
    settings <- settings[settings$V1 != "pop_version", ]
  }

  # save updated settings
  write.table(
    settings,
    file = file.path(dir, "settings.csv"),
    row.names = F,
    col.names = F,
    sep = ",",
    qmethod = "double"
  )

  pop_file <- file.path(dir,paste0("pop_file_cov_dataset_id_", pop_version, ".rds"))
  pop_growth_file <- file.path(dir,paste0("pop_file_cov_dataset_id_", pop_version, ".rds"))
  pop_version <- NULL

}

if (lt_hc & !file.exists(file.path(dir, "growth_rates.rds"))) {

  calc_growth_rate(dir, test_by_race = by_race, test_by_edu = by_edu, n_years = ifelse(by_edu, 5, 10))
  message("Done with calculating growth rates!")

}

## Save message on the passed in parameters/arguments ----------------------------------------------
msg <-
  glue::glue(
    "Values that define the validation set and gold standard:

dir <- '{dir}'
n_years <- {n_years}
pool_adjacent_counties <- {pool_adjacent_counties}
output_dirname <- '{output_dirname}'
all_age_threshold <- {all_age_threshold}
by_age_threshold <- {by_age_threshold}
skip_min <- {skip_min}
skip_cov <- {skip_cov}

{dir} is used to load settings. deaths_file, pop_file, and by_race are read from there.

- pop_file: {pop_file}
- deaths_file: {deaths_file}
- by_race: {by_race}
- by_edu: {by_edu}
- years: {min(years)}-{max(years)}

outputs are saved to {final_output_dir}
")

message(msg)

# save message to LU and FILEPATH
cat(
  msg,
  file = paste0(final_output_dir, "/01_define_validation_set_parameters.md"),
  sep = "\n"
)
cat(
  msg,
  file = paste0(FILEPATH_output_dir, "/01_define_validation_set_parameters.md"),
  sep = "\n"
)

## Functions  --------------------------------------------------------------------------------------

#' Makes a long data.table of all counties adjacent to each county. Only used if
#' pool_adjacent_counties is TRUE
#'
#' @param adjmat_file string containing the path to the adjacency matrix file. Should come from
#'        settings.csv
#' @return data.table with two columns: "mcnty" and "adj_mcnty"
get_adjacent_counties <- function(adjmat_file) {
  adjmat <- readRDS(adjmat_file)
  adjmat <- as.data.table(as.matrix(adjmat))

  # set column labels
  setnames(adjmat, names(adjmat), gsub("V", "", names(adjmat)))  # remove "V" from column names
  new_names <-
    as.character(as.integer(names(adjmat)) - 1)  # as numbers, subtract 1
  setnames(adjmat, names(adjmat), new_names)  # set column labels
  adjmat[, mcnty := .I - 1] # area is row number - 1, set row labels
  adjmat_long <-
    melt.data.table(adjmat, id.vars = c("mcnty"))  # reshape so each row is a pair of counties
  adjmat_long <- adjmat_long[value == 1,]  # keep adjacent counties

  # clean
  setnames(adjmat_long, "variable", "adj_mcnty")
  adjmat_long[, value := NULL]
  adjmat_long[, mcnty := as.integer(mcnty)]
  adjmat_long[, adj_mcnty := as.integer(as.character(adj_mcnty))]  # must convert to character first

  # check that encoding is equivalent between mcnty and adj_mcnty
  # off-by-one errors are likely
  setequal(unique(adjmat_long$mcnty), unique(adjmat_long$adj_mcnty))

  # mcnty in data is encoded 0 - 3109
  # check that encoding in adjmat_long for both county columns
  # matches encoding in data
  expected_counties <- seq(0, 3109, 1)
  stopifnot(setequal(union(
    unique(adjmat_long$mcnty),
    unique(adjmat_long$adj_mcnty)
  ),
  expected_counties))

  return(adjmat_long)
}

#' Helper function to pool_adjacent_counties() to test that it works properly.
test_adjacent_counties <- function() {
  test_adj_counties <- data.table(c(1, 1, 2, 2),
                                  c(2, 3, 1, 4))
  names(test_adj_counties) <- c("mcnty", "adj_mcnty")

  test_data <- data.table(c(1, 2, 3, 4),
                          c(10, 5, 2, 3),
                          c(20, 10, 4, 6),
                          c(1, 1, 1, 1),
                          c(1, 1, 1, 1))
  names(test_data) <- c("mcnty", "deaths", "pop", "race", "edu")

  test_results <- pool_adj_counties(data = test_data,
                                    adj_counties = test_adj_counties,
                                    id_vars = "mcnty,race,edu")

  expected_results <- data.table(c(1, 2),
                                 c(1, 1),
                                 c(34, 36),
                                 c(17, 18))
  names(expected_results) <-
    c("mcnty", "race", "edu", "pop", "deaths")

  stopifnot(
    all.equal(
      target = expected_results,
      current = test_results,
      ignore.col.order = T,
      ignore.row.order = T
    )
  )
}

#' Pool population and deaths across space by summing data for all adjacent counties for each county
#'
#' @param data data.frame with pop and deaths data. Must have columns pop and deaths.
#' @param adj_counties data.frame from get_adjacent_county()
#' @param id_vars columns in data to group by and sum. E.g. sex and age.
#'
#' @return data.table with data pooled across space according to adj_counties.
pool_adj_counties <- function(data, adj_counties, id_vars) {
  data <- data.table::copy(data)

  adj_counties <-
    melt(
      adj_counties,
      measure.vars = c("mcnty", "adj_mcnty"),
      id.vars = "mcnty",
      value.name = "mcnty_pool"
    )
  adj_counties[, variable := NULL]
  adj_counties <- unique(adj_counties)

  data <-
    merge(
      adj_counties,
      data,
      by.x = "mcnty_pool",
      by.y = "mcnty",
      allow.cartesian = TRUE
    )

  data <-
    data[, list(pop = sum(pop), deaths = sum(deaths)), by = id_vars]

  return(data)
}

#' helper function that tests the results of pool_data_time()
test_year_pooling <- function(n_years, pool_prev_yrs = F) {
  # get locations
  loc <- fread("FILEPATH/merged_counties.csv")

  # dummy values that will be easy to check for deaths and population
  num_deaths = 10
  num_pop = 1000

  if(!pool_prev_yrs) {
    radius <-
      (n_years - 1) / 2 # should be same as radius used in pool_data_time()

    truncated_years = c((2000 + radius):(2018 - radius))  # should be same year truncation that pool_data_time does()
  } else {

    truncated_years <- c((2000 + n_years - 1):2018)

  }


  # same dimensions as full, real data
  data_large <- CJ(
    mcnty = unique((loc$mcnty)),
    year = c(2000:2018),
    sex = c(1, 2),
    race = all_pop_id,
    edu = all_pop_id,
    age = unique(readRDS(age_std_file)$age),
    deaths = num_deaths,
    pop = num_pop
  )

  # small demonstration data.table that we can eyeball the expected result for
  data_small <- data.table(
    mcnty = 0,
    year = c(2000:2018),
    sex = c(2),
    race = all_pop_id,
    edu = all_pop_id,
    age = 25,
    deaths = num_deaths,
    pop = num_pop
  )

  expected_results <- data.table(
    mcnty = 0,
    year = truncated_years,
    # truncated years on boundaries
    sex = c(2),
    race = all_pop_id,
    edu = all_pop_id,
    age = 25,
    deaths = n_years * num_deaths,
    # n_years of pooling, so we expect n_years * num_deaths pooled deaths
    pop = n_years * num_pop
  )


  data_pooled_large <- pool_data_time(n_years = n_years, data_large, pool_prev_yrs = pool_prev_yrs)

  stopifnot(unique(data_pooled_large$deaths) == (n_years * num_deaths))
  stopifnot(unique(data_pooled_large$pop) == (n_years * num_pop))

  if(n_years == 1) {
    stopifnot(nrow(dplyr::anti_join(data_pooled_large, data_large)) == 0)
  }

  data_pooled_small <- pool_data_time(n_years = n_years, data_small, pool_prev_yrs = pool_prev_yrs)

  stopifnot(
    all.equal(
      target = expected_results,
      current = data_pooled_small,
      ignore.col.order = T,
      ignore.row.order = T
    )
  )
}

#' Pool population and deaths across time with a sliding window that is n-years wide. Sums by
#' mcnty, sex, age, race, and edu. Returns pooled data. For example, if n_years is 3, then for each year,
#' the population and deaths for the previous year, current year, and next year will be summed and
#' placed into the current year.
#'
#' @param n_years integer for width of sliding window. must be odd so that window is symmetrical.
#' @param data data.frame with pop and deaths data. Must have columns pop and deaths.
#'
#' @return data.table with data pooled across time according to n_year.
pool_data_time <- function(n_years, data, pool_prev_yrs = F) {
  if (n_years == 0) {
    return(data)
  }

  
  if (n_years %% 2 == 0 & !pool_prev_yrs) {
    stop("n_years must be odd when not pooling only previous years!")
  }

  
  if (n_years > 7) {
    stop("n_years = ", n_years, " is unreasonably large")
  }

  data <- data.table::copy(data)

  if(!pool_prev_yrs) {
    radius <- (n_years - 1) / 2

    data <-
      lapply((min(data$year) + radius):(max(data$year) - radius), function(yy) {
        temp <- data[year %in% c((yy - radius):(yy + radius)),
                     list(pop = sum(pop), deaths = sum(deaths)),
                     by = "mcnty,sex,age,race,edu"]
        temp[, year := yy]
        temp
      })

  } else {

    min_year <- min(data$year) + n_years - 1

    data <-
      lapply(min_year:max(data$year), function(yy) {
        temp <- data[year %in% c((yy - (n_years-1)):yy),
                     list(pop = sum(pop), deaths = sum(deaths)),
                     by = "mcnty,sex,age,race,edu"]
        temp[, year := yy]
        temp
      })

  }

  data <- rbindlist(data)




  return(data)
}


get_pop <- function(pop_file, loc) {

  pop <- readRDS(pop_file)
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
  # education in population/births. So, if this is and education modeling run, and if there is
  # unknown education in the pop_file, and if all the pop in those rows are zero, completely remove
  # all rows where education is unknown. This probably happens because of a squaring step in earlier
  # code. Once deaths and pop are merged together (via outer merge) we'll still set rows where pop
  # is missing (i.e., rows where deaths have unknown education) to zero.
  if (by_edu) {
    if (all(pop[edu == 100, pop] == 0)) {
      pop <- pop[edu != 100, ] # drop unknown education if there is zero pop in it
    } else {
      stop("data.table 'pop' has non-zero population in education group unknown education.")
    }
  }

  if (!"state" %in% names(pop)) {
    pop <- merge(pop, unique(loc[, .(mcnty, state)]), by = "mcnty", all.x = T)
  }
  stopifnot("state" %in% names(pop))

  return(pop)
}


## Main script  ------------------------------------------------------------------------------------

# load deaths
gc()
deaths <- readRDS(deaths_file)
gc()

## Prepare deaths-----------------------------------------------------------------------------------

# filter to all-cause deaths
if ("acause" %in% names(deaths)) {
  deaths <- deaths[acause == "_all",]
} else if ("cause_id" %in% names(deaths)) {
  deaths <- deaths[cause_id == 294,]
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

# reduce number of rows
deaths <- deaths[, list(deaths = sum(deaths)), by = "mcnty,year,sex,race,edu,age"]
gc()

# Prepare population --------------------------------------------------------------------------
pop <- get_pop(pop_file, loc)

if (!by_race) pop[, race := all_pop_id]
if (!by_edu) pop[, edu := all_pop_id]

# reduce number of rows
pop <- pop[, list(pop = sum(pop)), by = "mcnty,year,sex,race,edu,age"]
gc()

# pop for latest year
latest_year <- max(years)
pop_latest <-
  pop[year == latest_year, list(pop = sum(pop)), by = "mcnty,race,edu"]
setnames(pop_latest, "pop", paste0("pop_", latest_year))


# prepare data --------------------------------------------------------------------------------

data <-
  merge(
    pop[, list(pop = sum(pop)), by = "mcnty,year,sex,age,race,edu"],
    deaths[, list(deaths = sum(deaths)), by = "mcnty,year,sex,age,race,edu"],
    by = c("mcnty", "year", "sex", "age", "race", "edu"),
    all = T
  )

data[is.na(deaths), deaths := 0]

rm(deaths, pop)
gc()


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
      glue::glue(
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
           file = file.path(final_output_dir, "adult_edu_reporting_status.csv"))

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
    stop(glue::glue("Unhandled case. Check ages. ages is set to c({paste(ages, collapse = ', ')})"))
  }
  # DROP ROWS OF DATA WHERE THERE IS NO EDUCATION INFORMATION if relevant, it would have been added
  # in prep_inputs. If this column isn't present, no data will be dropped. This column is expected
  # to exist if by_edu is True
  data <- data[edu_reporting_status == 1, ] # drop non reporting / keep only reporting
  data[, edu_reporting_status := NULL] # remove the reporting column
  # There should not be any deaths marked as unknown
  stopifnot(nrow(data[edu == 100, ]) == 0)
  # There should not be any NA pop
  if (any(is.na(data$pop)))
    stop("There are NA values in the pop column of data")
}
gc()

if (any(data[, edu == 100])) {
  stop("There are rows with unknown education in data.")
}
if (any(is.na(data))) {
  stop("There are NAs in data.")
}

# clean data
data[, mcnty := as.integer(mcnty)]
data[, year := as.integer(year)]
data[, sex := as.integer(sex)]
data[, age := as.integer(age)]
data[, race := as.integer(race)]
data[, edu := as.integer(edu)]

saveRDS(data, file.path(final_output_dir, "data_checkpoint_post_clean.rds"))


## Adjust population accordingly to be consistent with how we adjust population when modeling.
if(pop_add1) {
  warning(paste0("pop_add1 is TRUE, according to settings.csv.\nFor the ",nrow(data[pop == 0 & deaths != 0]),
                 " (",round(nrow(data[pop == 0 & deaths != 0])/nrow(data)*100,3),"%)"," rows where pop= 0 and deaths != 0, we are setting pop to equal 1."))
  data[pop == 0 & deaths != 0, pop := 1]
} else {
  warning(paste0("pop_add1 is FALSE, according to settings.csv.\nFor the ",nrow(data[deaths > pop]),
                 " (",round(nrow(data[deaths > pop])/nrow(data)*100,3),"%)"," rows where deaths > pop, we are setting pop to equal deaths."))
  data[deaths > pop, pop := deaths]
}



# pool adjacent counties
if (pool_adjacent_counties) {
  adj_counties <- get_adjacent_counties(adjmat_file = adjmat_file)
  test_adjacent_counties()
  data <- pool_adj_counties(data = data,
                            adj_counties = adj_counties,
                            id_vars = "age,sex,year,mcnty,race,edu")
}

# create n-year moving sum for all county-years
test_year_pooling(n_years = n_years,
                  pool_prev_yrs = pool_prev_yrs)
## pool over years except 2020 and 2021


# also, this may be very odd to do...
data_copy <- copy(data)
data <- rbind(pool_data_time(n_years = n_years,
                             data = data[year < 2020],
                             pool_prev_yrs = pool_prev_yrs),
              data_copy[year >= 2020])

rm(data_copy)

saveRDS(data, file.path(final_output_dir, "data_checkpoint_post_pooling.rds"))



gc()
# Create a copy of the full data set (all race/ethnicity-counties) for later aggregation
# We'll use this to get an all race gold standard with the same counties as the by race gold standard
data_full <- data.table::copy(data)
saveRDS(data_full, file.path(final_output_dir, "data_full_checkpoint.rds"))

gc()
# Condition 1:
# keep only race/ethnicity-counties with non-zero death counts in all ages for both sexes in all years

if (!skip_min) {

  if(allow_mx_0) {

    # we also know that graduation is not applied to ages < 0, 1-4, 5-9, 80, and 85+. However, it does use data in ages 5-9 and 80.
    # But, if mx = 0 in these ages, then this just means dx = 0, i.e. the slope may be large when doing graduation. But, that might be okay
    # So, we just include strata where there are no 0s in ages 10-79 and 85+

    data[!(age %in% c(0, 1, 5, 80)), min := min(deaths), by = "mcnty,race,edu"]
    data[,min := max(min, na.rm=T), by="mcnty,race,edu"]

  } else {
    data[, min := min(deaths), by = "mcnty,race,edu"]
  }

  # Save min deaths data for later merging
  deaths_min <- unique(data.table::copy(data[, list(mcnty, race, edu, min)]))
  saveRDS(deaths_min, file.path(final_output_dir, "deaths_min_checkpoint.rds"))
  data <- data[min > 0,]
}

if (!nrow(data) > 0) {
  stop("Data has zero rows.")
}

if (min(data$min) <= 0) {
  stop("Min deaths of 0 didn't work")
}

saveRDS(data, file.path(final_output_dir, "data_checkpoint_post_data_min.rds"))

# Get list of remaining counties to derive aggregate all-race / all-edu values This will be used to
# subset the all-race / all-edu version down to the same counties as the by-race version, If ran for
# a by-race validation set.
counties_remaining <- unique(data$mcnty)

if (by_race | by_edu) {
  # Set data to all (individual) races / edus for remaining counties at this point data_full was a
  # copy of data before the first condition was applied. by filtering setting data equal to
  # data_full and filtering data_full down to the counties in data after the first condition was
  # applied, we're basically remaking data with the first condition applied, but with every
  # available race or edu group
  data <- data.table::copy(data_full[mcnty %in% counties_remaining])
}
saveRDS(data, file.path(final_output_dir, "data_checkpoint_post_data_full.rds"))
# simulate from a Poisson distribution to get "draws" of deaths
data <-
  cbind(data, t(sapply(1:nrow(data), function(ii)
    rpois(n = n.sims, lambda = data$deaths[ii]))))
data[, deaths := NULL]
gc()
data <-
  melt(
    data,
    id.vars = c("mcnty", "year", "sex", "age", "race", "edu", "pop"),
    variable.name = "sim",
    value.name = "deaths"
  )
gc() # the melt explodes the data from ~10Gb to ~40Gb
data[, sim := as.integer(sim)]
setkeyv(data, c("mcnty", "year", "sex", "age", "race", "edu", "sim"))

saveRDS(data, file.path(final_output_dir, "data_checkpoint_post_sim.rds"))


## Adjust population accordingly to be consistent with how we adjust population when modeling.
if(pop_add1) {
  
  warning(paste0("pop_add1 is TRUE, according to settings.csv.\nFor the ",nrow(data[pop == 0 & deaths != 0]),
                 " (",round(nrow(data[pop == 0 & deaths != 0])/nrow(data)*100,3),"%)"," rows where pop= 0 and deaths != 0, we are setting pop to equal 1."))
  data[pop == 0 & deaths != 0, pop := 1]
} else {
  # add population if necessary to make sure deaths <= pop
  # this is necessary when keeping all counties
  warning(paste0("pop_add1 is FALSE, according to settings.csv.\nFor the ",nrow(data[deaths > pop]),
                 " (",round(nrow(data[deaths > pop])/nrow(data)*100,3),"%)"," rows where deaths > pop, we are setting pop to equal deaths."))
  data[deaths > pop, pop := deaths]
}

gc()
# Create aggregated all races (race "all_pop_id") if this is a by-race or by-edu run
if (by_race | by_edu) {
  data_allpop <-
    data[, list(
      pop = sum(pop, na.rm = TRUE),
      deaths = sum(deaths, na.rm = TRUE),
      race = all_pop_id,
      edu = all_pop_id
    ), by = "mcnty,year,sex,age,sim"]
  gc()
  saveRDS(data_allpop, file.path(final_output_dir, "data_checkpoint_post_allpop.rds"))
  # Now drop race/ethnicity-counties / edu-counties that failed criterion 1
  # This is important, we kept the county-races/edus that failed the first criteria, and then we
  # brought back the races that failed the criterion with data_full, and now we're re-applying
  # criterion 1 and dropping the race/ethnicities / edus that failed.
  # Note the implicit inner merge.
  data <-
    merge(data, deaths_min[min > 0], by = c("mcnty", "race", "edu"))
  # this hasn't been saved yet
  saveRDS(data, file.path(final_output_dir, "data_checkpoint_post_re_apply_min.rds"))
  data[, min := NULL]

  # Append "all_pop_id" values if this is a by_race / by_edu run
  data <- rbindlist(list(data, data_allpop), use.names = TRUE)
  rm(data_allpop, deaths_min); gc()
}

# calculate all-ages standardized and crude death rates and combine with ASMRs
data[, mx := deaths / pop]
data[deaths == 0 & pop == 0, mx := 0]
data[, deaths := NULL]

# load the age standard
std_wt <- readRDS(age_std_file)[, list(age, wt = wt / sum(wt))]

gc()
data[, level := "mcnty"]
data <- setnames(x = data, old = "mcnty", new = "area")
gc()
data <-
  calc_all_ages(data, std_wt, "mx", allow_missing_ages = by_edu)  # This drops pop column
data <- setnames(x = data, new = "mcnty", old = "area")
data[, level := NULL]
gc()

# add population column back

pop <- get_pop(pop_file, loc)

pop <- pop[, list(pop = sum(pop)), by = "mcnty,year,sex,age,race,edu"]

# Calculate all-race (race "all_pop_id") total population and append to pop
if (by_race) {
  pop_allpop <-
    pop[, list(pop = sum(pop, na.rm = TRUE), race = all_pop_id), by = "mcnty,year,sex,age,edu"]
  pop <- rbindlist(list(pop, pop_allpop), use.names = TRUE)
  rm(pop_allpop); gc()
}
if (by_edu) {
  pop_allpop <-
    pop[, list(pop = sum(pop, na.rm = TRUE), edu = all_pop_id), by = "mcnty,year,sex,age,race"]
  pop <- rbindlist(list(pop, pop_allpop), use.names = TRUE)
  rm(pop_allpop); gc()
}
# second merge of pop
data <-
  merge(
    data,
    pop,
    by = c("mcnty", "year", "sex", "age", "race", "edu"),
    all.x = T
  )
rm(pop)
gc()

# pop can be null for these ages
if (any(is.na(data[!(age %in% c(98, 99)), pop]))) {
  stop("Pop column has nulls")
}

# collapse data to get point estimates and standard errors of mortality rates

mx <- data[, list(
  mx_mean = mean(mx),
  mx_se = sd(mx),
  mx_var = var(mx),
  mx_lb = quantile(mx, 0.025),
  mx_ub = quantile(mx, 0.975)
),
keyby = "mcnty,year,sex,age,race,edu"]

# select counties where the median coefficient of variance across all years and both sexes for the
# overall mortality rate is less than 0.01 and for age-specific mortality rates is less than 0.1
# to serve as the validation set
# Conditions 2 and 3:
if (!skip_cov) {
  mx[, cov := mx_se / mx_mean]
  mx[is.na(cov), cov := max(mx$cov, na.rm = T)]

  # If this is a by-race model run, don't use race "all_pop_id" to determine val_set
  if (by_race) {
    val_set <-
      mx[race != all_pop_id, list(all = median(cov[age > 90]), byage = median(cov[age < 90])), by = "mcnty,race,edu"]
    val_set <-
      val_set[all < all_age_threshold &
                byage < by_age_threshold, list(mcnty, race, edu)]
    # Retrieve and append race "all_pop_id" for remaining counties
    val_set_allpop <-
      mx[race == all_pop_id &
           mcnty %in% unique(val_set$mcnty), list(all = median(cov[age > 90]), byage = median(cov[age < 90])), by = "mcnty,race,edu"]
    val_set <-
      rbindlist(list(val_set, val_set_allpop[, list(mcnty, race, edu)]), use.names = TRUE)
  } else if (by_edu) {
    val_set <-
      mx[edu != all_pop_id, list(all = median(cov[age > 90]), byage = median(cov[age < 90])), by = "mcnty,race,edu"]
    val_set <-
      val_set[all < all_age_threshold &
                byage < by_age_threshold, list(mcnty, race, edu)]
    # Retrieve and append race "all_pop_id" for remaining counties
    val_set_allpop <-
      mx[edu == all_pop_id &
           mcnty %in% unique(val_set$mcnty), list(all = median(cov[age > 90]), byage = median(cov[age < 90])), by = "mcnty,race,edu"]
    val_set <-
      rbindlist(list(val_set, val_set_allpop[, list(mcnty, race, edu)]), use.names = TRUE)
  } else {
    val_set <-
      mx[, list(all = median(cov[age > 90]), byage = median(cov[age < 90])), by = "mcnty,race,edu"]
    val_set <-
      val_set[all < all_age_threshold &
                byage < by_age_threshold, list(mcnty, race, edu)]
  }
  mx[, cov := NULL]
} else {
  val_set <- unique(mx[, list(mcnty, race, edu)])
}

setkeyv(data, c("mcnty", "race", "edu"))
setkeyv(val_set, c("mcnty", "race", "edu"))
setkeyv(mx, c("mcnty", "race", "edu"))


# filter data and mx down to the validation set county-race/ethnicity groups
if (!skip_cov) {
  data <- data[J(val_set),]
  mx <- mx[J(val_set),]
}
gc()

# Save non-lifetable results ------------------------------------------------------------------

dir.create(file.path(final_output_dir, "intermediate_validation_set", "ex_input"), recursive = T)
dir.create(file.path(final_output_dir, "intermediate_validation_set", "ex_output"), recursive = T)

message("SAVING TEMP FILES...")
saveRDS(data, file = file.path(final_output_dir, "intermediate_validation_set", glue("data_{format(Sys.time(), '%Y_%m_%d_%H_%M_%S')}.rds")))
message("Done saving temp files...")

# prepare the list of counties in the validation set
loc <- fread("FILEPATH/merged_counties.csv")
# collapse county names into merge-counties
loc[, cnty_name := gsub(" County| Parish", "", cnty_name)]
loc <- loc[, list(cnty_name = paste0(paste(cnty_name, collapse = "/"), ", ", state_name[1])),
           by = "mcnty"]
setkey(loc, "mcnty")
loc <- loc[J(val_set),]
loc <- merge(loc,
             pop_latest,
             by = c("mcnty", "race", "edu"),
             all.x = T)
# save out list of counties in validation set
write.csv(
  loc,
  file = file.path(final_output_dir, "validation_set", "validation_set_names.csv"),
  row.names = F
)

# also save out to FILEPATH. It's important that both are saved. First remove pop column.
write.csv(
  loc[,.(mcnty,race,edu,cnty_name)],
  file = file.path(FILEPATH_output_dir, "validation_set_names.csv"),
  row.names = F
)

# save mortality gold standard
saveRDS(mx, file.path(final_output_dir, "validation_set", "validation_set_gs_mx.rds"))


############ Add this gold standard dataset to the settings ############
message("Adding gold standard mx file name to settings.csv")
settings <- read.csv(paste0(dir, "/settings.csv"), stringsAsFactors = F, header = F)

if ("gs_file" %in% settings$V1) {
  message("Removing gs_file from settings.csv in order to replace it")
  settings <- settings[settings$V1 != "gs_file", ]
}

# And while we are add it, let's add val_dir
inputs_dir <- file.path(final_output_dir, "inputs")

if ("val_dir" %in% settings$V1) {
  message("Removing val_dir from settings.csv in order to replace it")
  settings <- settings[settings$V1 != "val_dir", ]
}

# Now add on new rows to the csv
settings <- rbind(settings,
                  data.frame(
                    V1 = c("gs_file", "val_dir"),
                    V2 = c(file.path(final_output_dir, "validation_set", "validation_set_gs_mx.rds"),
                           file.path(final_output_dir, "inputs"))
                  ))

# save updated settings
write.table(
  settings,
  file = file.path(dir, "settings.csv"),
  row.names = F,
  col.names = F,
  sep = ",",
  qmethod = "double"
)

########################################################################

# Lifetables ----------------------------------------------------------------------------------

data <- data[age < 90]
gc()

# save temp files for ex workers to run on
for (this_race in unique(data$race)) {
  for (this_edu in unique(data$edu)) {
    for (this_sex in unique(data$sex)) {
      for (this_year in unique(data$year)) {
        message(glue("Saving data for {this_year}, sex {this_sex} , race {this_race}, edu {this_edu}..."))
        temp_ex <-
          data[sex == this_sex &
                 race == this_race & edu == this_edu & year == this_year,]
        saveRDS(temp_ex,
                file = file.path(
                  final_output_dir,
                  "intermediate_validation_set",
                  "ex_input",
                  glue(
                    "ex_input_data_{this_year}_{this_sex}_{this_race}_{this_edu}.rds"
                  )
                ))
        rm(temp_ex)
        gc()
      }
    }
  }
}

# create a table for holding job ids
jids <- CJ(year = unique(data$year),
           sex = c(1,2),
           edu = unique(data$edu),
           race = unique(data$race))

# save jids to use as a set of expected dimensions in the final step.
saveRDS(jids,
        file = file.path(
          final_output_dir,
          "intermediate_validation_set",
          glue("expected_ex_dimensions.rds")
        ))

rm(data); gc()


jids[, pred_lt := sbatch(
  code = "FILEPATH/01b_create_validation_set_lifetables.r",
  name = glue("val_set_lt_{year}_{sex}_{edu}_{race}"),
  arguments = c(
    dir,
    output_dirname,
    sex,
    race,
    edu,
    year,
    pop_growth_file,
    allow_mx_0
  ),
  queue = "QUEUE",
  fthread = 2,
  m_mem_free = "50G",
  h_rt = "12:00:00",
  project = "PROJECT",
  sgeoutput = file.path(final_output_dir)
),
by = "year,sex,race,edu"]

# now collect results
jids[, collect_lt := sbatch(
  code = "FILEPATH/01c_finish_validation_set.r",
  name = "finish_validation_set",
  hold = unique(pred_lt),
  arguments = c(dir,
                output_dirname),
  queue = "QUEUE",
  fthread = 2,
  m_mem_free = "40G",
  project = "PROJECT",
  sgeoutput = file.path(final_output_dir)
)]

message("DONE.")
