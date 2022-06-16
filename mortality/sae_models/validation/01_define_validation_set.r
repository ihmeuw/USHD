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
##              output_dir [character]
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

library(R.utils)
library(data.table)
sourceDirectory("functions/")

set.seed(98121)

## Arguements -----------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
dir <- args[1] # model directory
n_years <- args[2]  # for year pooling
pool_adjacent_counties <- args[3]  # whether or not to pool adjacent counties
output_dir <- args[4] # optional extra directory to differentiate between runs with different settings "by_race_3yr_space_pool"
all_age_threshold <- args[5]  # originally was 0.01
by_age_threshold <- args[6]  # originally was 0.1
skip_min <- args[7]  # If TRUE will not apply first condition
skip_cov <- args[8]  # If TRUE will not apply condtions 2 and 3


# default values for optional arguments
if (is.na(all_age_threshold)) all_age_threshold <- 0.01
if (is.na(by_age_threshold)) by_age_threshold <- 0.1
if(is.na(skip_min)) skip_min <- FALSE
if(is.na(skip_cov)) skip_cov <- FALSE

if (!is.null(output_dir)) {
  output_dir <- as.character(output_dir)
} else {
  output_dir <- ""
}

n_years <- as.integer(n_years)
pool_adjacent_counties <- as.logical(pool_adjacent_counties)
all_age_threshold <- as.numeric(all_age_threshold)
by_age_threshold <- as.numeric(by_age_threshold)
skip_min <- as.logical(skip_min)
skip_cov <- as.logical(skip_cov)

limited_use_base_dir = paste0("FILEPATH", "FILEPATH")

final_output_dir <- paste0(limited_use_base_dir, "/", output_dir)

if (!dir.exists(final_output_dir)) {
  dir.create(final_output_dir,
    recursive = F)  # don't create full path, in case the filepath gets messed up
}

## Get settings ------------------------------------------------------------------------------------
get_settings(dir)

## Save message on the passed in parameters/arguments ----------------------------------------------

# message template
msg <- "
Values that define the validation set and gold standard:

dir <- %s
n_years <- %i
pool_adjacent_counties <- %s
output_dir: %s
all_age_threshold <- %f
by_age_threshold <- %f
skip_min <- %s
skip_cov <- %s

%s is used to load settings. deaths_file, pop_file, and by_race are read from there.

- pop_file: %s
- deaths_file: %s
- by_race: %s

outputs are saved to %s
"
# update message with values
msg <- sprintf(msg, dir, n_years, pool_adjacent_counties, output_dir, all_age_threshold, by_age_threshold, skip_min, skip_cov,
               dir, pop_file, deaths_file, by_race,
               final_output_dir)

# print / display message
cat(msg)

# save message
cat(msg,
    file=paste0(final_output_dir, "/01_define_validation_set_parameters.md"),
    sep="\n")

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
  setnames(adjmat, names(adjmat), gsub("V","", names(adjmat)))  # remove "V" from column names
  new_names <- as.character(as.integer(names(adjmat))-1)  # as numbers, subtract 1
  setnames(adjmat, names(adjmat), new_names)  # set column labels
  adjmat[, mcnty := .I - 1] # area is row number - 1, set row labels
  adjmat_long <- melt.data.table(adjmat, id.vars = c("mcnty"))  # reshape so each row is a pair of counties
  adjmat_long <- adjmat_long[value == 1, ]  # keep adjacent counties

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
  stopifnot(setequal(union(unique(adjmat_long$mcnty),
                 unique(adjmat_long$adj_mcnty)),
           expected_counties))

  return(adjmat_long)
}

#' Helper function to pool_adjacent_counties() to test that it works properly.
test_adjacent_counties <- function() {

  test_adj_counties <- data.table(
    c(1, 1, 2, 2),
    c(2, 3, 1, 4)
    )
  names(test_adj_counties) <- c("mcnty", "adj_mcnty")

  test_data <- data.table(
    c(1, 2, 3, 4),
    c(10, 5, 2, 3),
    c(20, 10, 4, 6),
    c(1, 1, 1, 1)
    )
  names(test_data) <- c("mcnty", "deaths", "pop", "race")

  test_results <- pool_adj_counties(data = test_data,
                                    adj_counties = test_adj_counties,
                                    id_vars = "mcnty,race")

  expected_results <- data.table(
    c(1, 2),
    c(1, 1),
    c(34, 36),
    c(17, 18)
    )
  names(expected_results) <- c("mcnty", "race", "pop", "deaths")

  stopifnot(
    all.equal(target = expected_results,
              current = test_results,
              ignore.col.order = T,
              ignore.row.order = T))
}

#' Pool population and deaths across space by summing data for all adjacent counties for each county
#'
#' @param data data.frame with pop and deaths data. Must have columns pop and deaths.
#' @param adj_counties data.frame from get_adjacent_county()
#' @param id_vars columns in data to group by and sum. E.g. sex and age.
#'
#' @return data.table with data pooled across space according to adj_counties.
pool_adj_counties <- function(data, adj_counties, id_vars) {

  data <- copy(data)

  adj_counties <- melt(adj_counties, measure.vars = c("mcnty", "adj_mcnty"),
                      id.vars = "mcnty",
                      value.name = "mcnty_pool")
  adj_counties[, variable := NULL]
  adj_counties <- unique(adj_counties)

  data <- merge(adj_counties, data, by.x="mcnty_pool", by.y="mcnty", allow.cartesian=TRUE)

  data <- data[, list(pop = sum(pop), deaths=sum(deaths)), by=id_vars]

  return(data)
}

#' helper function that tests the results of pool_data_time()
test_year_pooling <- function(n_years) {

  # get locations
  load("FILEPATH")

  # dummy values that will be easy to check for deaths and population
  num_deaths = 10
  num_pop = 1000

  radius <- (n_years - 1) / 2 # should be same as radius used in pool_data_time()

  truncated_years = c((2000 + radius):(2018 - radius))  # should be same year truncation that pool_data_time does()

  # same dimensions as full, real data
  data_large <- CJ(mcnty = unique((loc$mcnty)),
                   year = c(2000:2018),
                   sex = c(1, 2),
                   race = all_pop_id,
                   age = unique(readRDS(age_std_file)$age),
                   deaths = num_deaths,
                   pop = num_pop
  )

  # small demonstration data.table that we can eyeball the expected result for
  data_small <- data.table(mcnty = 0,
                           year = c(2000:2018),
                           sex = c(2),
                           race = all_pop_id,
                           age = 25,
                           deaths = num_deaths,
                           pop = num_pop)

  expected_results <- data.table(mcnty = 0,
                                 year = truncated_years,  # truncated years on boundaries
                                 sex = c(2),
                                 race = all_pop_id,
                                 age = 25,
                                 deaths = n_years * num_deaths,  # n_years of pooling, so we expect n_years * num_deaths pooled deaths
                                 pop = n_years * num_pop)


  data_pooled_large <- pool_data_time(n_years = n_years, data_large)

  stopifnot(unique(data_pooled_large$deaths) == (n_years * num_deaths))
  stopifnot(unique(data_pooled_large$pop) == (n_years * num_pop))

  data_pooled_small<- pool_data_time(n_years = n_years, data_small)

  stopifnot(
    all.equal(target = expected_results, current = data_pooled_small,
              ignore.col.order = T, ignore.row.order = T
    )
  )
}

#' Pool population and deaths across time with a sliding window that is n-years wide. Sums by
#' mcnty, sex, age, race. Returns pooled data. For example, if n_years is 3, then for each year,
#' the population and deaths for the previous year, current year, and next year will be summed and
#' placed into the current year.
#'
#' @param n_years integer for width of sliding window. must be odd so that window is symmetrical.
#' @param data data.frame with pop and deaths data. Must have columns pop and deaths.
#'
#' @return data.table with data pooled across time according to n_year.
pool_data_time <- function(n_years, data) {

  if (n_years == 0) {
    return(data)
  }

  # n_years better be odd!
  if (n_years %% 2 == 0) {
    stop("n_years must be odd!")
  }

  # Test that window isn't too large
  if (n_years > 7) {
    stop("n_years = ", n_years, " is unreasonably large")
  }

  data <- copy(data)

  radius <- (n_years - 1) / 2

  data <- lapply((min(data$year) + radius):(max(data$year) - radius), function(yy) {
    temp <- data[year %in% c((yy - radius) : (yy + radius)),
                 list(pop = sum(pop), deaths = sum(deaths)),
                 by = "mcnty,sex,age,race"]
    temp[, year := yy]
    temp
  })

  data <- rbindlist(data)

  return(data)
}

#' try-catch function for lifetables
#' if there is an error, it will return a data.table that looks like what
#' would have been returned if lifetable() had succeeded, but with NA in every value column
trycatch_lifetable <- function(mx, ax = NULL, sex = NULL, graduation = T, extrap = T, gr=NA) {
  out <- tryCatch(
    {
      
      lifetable(mx = value, sex = sex[1], graduation = T, extrap = F,
                lt_hc = lt_hc, gr = mean_gr)[, list(age, mx, ax, qx, ex)]
      
      
    },
    error=function(cond) {

      message(paste0(cond, "\n")) 
      return(
        data.table(age = ages, n = NA, mx = NA, ax = NA, qx = NA, lx = NA, dx = NA, Lx = NA, Tx = NA, ex = NA)
      )
    }
  )
  return(out)
}


## Main script  ------------------------------------------------------------------------------------

# load and combine deaths and population
pop <- readRDS(pop_file)
deaths <- readRDS(deaths_file)

# filter to all-cause deaths
if ("acause" %in% names(deaths)) {
  deaths <- deaths[acause == "_all", ]
} else if ("cause_id" %in% names(deaths)) {
  deaths <- deaths[cause_id == 294, ]
} # else, assume deaths is already all-cause

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

# pop for latest year
latest_year <- max(data$year)
pop_latest <- pop[year == latest_year, list(pop = sum(pop)), by = "mcnty,race"]
setnames(pop_latest, "pop", paste0("pop_", latest_year))
rm(deaths)

# clean data
data[, mcnty := as.integer(mcnty)]
data[, year := as.integer(year)]
data[, sex := as.integer(sex)]
data[, age := as.integer(age)]
data[, race := as.integer(race)]

# add population if necessary to make sure deaths <= pop
data[deaths > pop, pop := deaths]

# pool adjacent counties
if (pool_adjacent_counties) {
  adj_counties <- get_adjacent_counties(adjmat_file = adjmat_file)
  test_adjacent_counties()
  data <- pool_adj_counties(data = data,
    adj_counties = adj_counties,
    id_vars = "age,sex,year,mcnty,race")
}

# create n-year moving sum for all county-years
test_year_pooling(n_years = n_years)
data <- pool_data_time(n_years = n_years, data = data)

# Create a copy of the full data set (all race/ethnicity-counties) for later aggregation
# We'll use this to get an all race gold standard with the same counties as the by race gold standard
data_full <- copy(data)

# Condition 1:
# keep only race/ethnicity-counties with non-zero death counts in all ages for both sexes in all years
# NOTE: this is by race and county
if (!skip_min) {
  data[, min := min(deaths), by = "mcnty,race"]

  # Save min deaths data for later merging
  deaths_min <- unique(copy(data[, list(mcnty, race, min)]))
  data <- data[min > 0, ]
  data[, min := NULL]
}

if (!nrow(data) > 0) {
  stop("Data has zero rows.")
}

# Get list of remaining counties to derive aggregate all-race (race "all_pop_id") values
# This will be used to subset the all-race version down to the same counties as the by-race version,
# If ran for a by-race validation set.
counties_remaining <- unique(data$mcnty)

if (by_race) {
  data <- copy(data_full[mcnty %in% counties_remaining])
}

# simulate from a Poisson distribution to get "draws" of deaths
data <- cbind(data, t(sapply(1:nrow(data), function(ii) rpois(n = n.sims, lambda = data$deaths[ii]))))
data[, deaths := NULL]
data <- melt(data, id.vars = c("mcnty", "year", "sex", "age", "race", "pop"),
             variable.name = "sim", value.name = "deaths")
data[, sim := as.integer(sim)]
setkeyv(data, c("mcnty", "year", "sex", "age","race", "sim"))

# add population if necessary to make sure deaths <= pop
# this is necessary when keeping all counties
data[deaths > pop, pop := deaths]

if (by_race) {
  # Create aggregated all races (race "all_pop_id") if this is a by-race run
  data_allrace <- data[, list(pop = sum(pop, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE), race = all_pop_id), by = "mcnty,year,sex,age,sim"]

  # Now drop race/ethnicity-counties that failed criterion 1
  # This is important, we kept the county-races that failed the first criteria, and then we
  # brought back the races that failed the criterion with data_full, and now we're re-applying
  # criterion 1 and dropping the race/ethnicities that failed.
  data <- merge(data, deaths_min[min > 0], by = c("mcnty", "race"))
  data[, min := NULL]

  # Append race "all_pop_id" values if this is a by-race run
  data <- rbindlist(list(data, data_allrace), use.names = TRUE)
}

# calculate all-ages standardized and crude death rates and combine with ASMRs
data[, mx := deaths / pop]
data[deaths == 0 & pop == 0, mx := 0]
data[, deaths := NULL]

# load the age standard
std_wt <- readRDS(age_std_file)[, list(age, wt = wt / sum(wt))]

data[, level := "mcnty"]
data <- setnames(x = data, old = "mcnty", new = "area")
data <- calc_all_ages(data, std_wt, "mx")  # This drops pop column
data <- setnames(x = data, new = "mcnty", old = "area")
data[, level := NULL]

# add population column back
pop <- readRDS(pop_file)
if (!by_race) {
  pop[, race := all_pop_id]
}

pop <- pop[, list(pop = sum(pop)), by = "mcnty,year,sex,age,race"]

# Calculate all-race (race "all_pop_id") total population and append to pop
if (by_race) {
  pop_allrace <- pop[, list(pop = sum(pop, na.rm = TRUE), race = all_pop_id), by = "mcnty,year,sex,age"]
  pop <- rbindlist(list(pop, pop_allrace), use.names = TRUE)
}

data <- merge(data, pop, by = c("mcnty", "year", "sex", "age", "race"), all.x = T)

# pop can be null for these ages
if (any(is.na(data[!(age %in% c(98, 99)), pop]))) {
  stop("Pop column has nulls")
}

# collapse data to get point estimates and standard errors of mortality rates
mx <- data[, list(mx_mean = mean(mx),
                  mx_se = sd(mx), mx_var = var(mx),
                  mx_lb = quantile(mx, 0.025), mx_ub = quantile(mx, 0.975)),
           keyby = "mcnty,year,sex,age,race"]

# select counties where the median coefficient of variance across all years and both sexes for the
# overall mortality rate is less than 0.01 and for age-specific mortality rates is less than 0.1
# to serve as the validation set
# Conditions 2 and 3:
if (!skip_cov) {
  mx[, cov := mx_se / mx_mean]
  mx[is.na(cov), cov := max(mx$cov, na.rm = T)]

  # If this is a by-race model run, don't use race "all_pop_id" to determine val_set
  if (by_race) {
    val_set <- mx[race != all_pop_id, list(all = median(cov[age > 90]), byage = median(cov[age < 90])), by = "mcnty,race"]
    val_set <- val_set[all < all_age_threshold & byage < by_age_threshold, list(mcnty, race)]
    # Retrieve and append race "all_pop_id" for remaining counties
    val_set_allrace <- mx[race == all_pop_id & mcnty %in% unique(val_set$mcnty), list(all = median(cov[age > 90]), byage = median(cov[age < 90])), by = "mcnty,race"]
    val_set <- rbindlist(list(val_set, val_set_allrace[, list(mcnty, race)]), use.names = TRUE)
  } else {
    val_set <- mx[, list(all = median(cov[age > 90]), byage = median(cov[age < 90])), by = "mcnty,race"]
    val_set <- val_set[all < all_age_threshold & byage < by_age_threshold, list(mcnty, race)]
  }
  mx[, cov := NULL]
} else {
  val_set <- unique(mx[, list(mcnty, race)])
}

setkeyv(data, c("mcnty", "race"))
setkeyv(val_set, c("mcnty", "race"))
setkeyv(mx, c("mcnty", "race"))

# filter data and mx down to the validation set county-race/ethnicity groups
if (!skip_cov) {
  data <- data[J(val_set), ]
  mx <- mx[J(val_set), ]
}


# calculate life expectancy for each draw
setnames(ex, area_var, "area")
ex[,level := area_var]

## Merge on growth rates necessary for the life table methods
ex <- add_growth_rates(dir = dir, data = ex,
                       idvars = c("year","sex","area", "level", "race"),
                       lt_hc = lt_hc)

setnames(ex, "area", area_var)
ex[,level := NULL]


ex <- ex[, c(list(pop = sum(pop)),
             trycatch_lifetable(mx = mx, sex = sex[1], graduation = T, extrap = F, gr = mean_gr)[, list(age, mx, ax, qx, ex)]),
           by = "mcnty,race,year,sex,sim"]

setnames(ex, area_var, "area")
ex[, level := area_var]

if("mean_gr" %in% names(ex)) ex[,mean_gr := NULL]


# ex of inf should be dropped!
# ex of NA should be dropped!
ex[, drop := any(is.na(ex)), by = "mcnty,race,year,sex"]
ex[, drop := any(is.infinite(ex)), by = "mcnty,race,year,sex"]
# drop will have value of TRUE if it should be dropped, so keep the rows where drop is not TRUE
ex <- ex[drop != TRUE, ]
ex[, drop := NULL]

# collapse life table draws to get point estimates and CI
ex <- ex[, list(total_pop = pop[1], mx = mean(mx), ax = mean(ax),
                ex_se = sd(ex), ex_var = var(ex),
                ex_lb = quantile(ex, 0.025), ex_ub = quantile(ex, 0.975)),
         keyby = "mcnty,race,year,sex,age"]

# merge on the growth rates again
ex <- merge(ex, all_gr, by=c("mcnty","year","sex","race"), all.x=T)
stopifnot(nrow(ex[is.na(mean_gr)]) == 0)

ex[, c("ex_mean") := lifetable_horiuchi(mx = mx, ax = ax, graduation = F, extrap = F, gr = mean_gr)[, list(ex)], by = "mcnty,year,sex,race"]
ex <- ex[, list(mcnty, year, sex, age, race, ex_mean, ex_se, ex_var, ex_lb, ex_ub)]
setkeyv(ex, c("mcnty", "year", "sex", "race", "age"))

# save the gold standards for the validation set
gs_mx <- mx
gs_ex <- ex
save(gs_ex, gs_mx,
     file = paste0(final_output_dir, "/validation_set_gs.rdata"))

# save the list of counties in the validation set
loc <- fread("FILEPATH")
loc[, cnty_name := gsub(" County| Parish", "", cnty_name)]
loc <- loc[, list(cnty_name = paste0(paste(cnty_name, collapse = "/"), ", ", state_name[1])), by = "mcnty"]
setkey(loc, "mcnty")
loc <- loc[J(val_set), ]
loc <- merge(loc, pop_latest, by = c("mcnty","race"), all.x = T)
write.csv(loc,
          file = paste0(final_output_dir, "/validation_set_names.csv"),
          row.names = F)
