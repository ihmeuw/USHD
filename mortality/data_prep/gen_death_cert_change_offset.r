# Purpose: pull out the year in which each state changed death certificate version, by looking at
# the education revision "edu_flag". Note that some states switched mid year. Also note that before
# (not including) 2003, states either didn't say anything about education, or used the 1989 revision
# | Edu_flag | Meaning           |
# |----------|-------------------|
# | 0        | 1989 revision     |
# | 1        | 2003 revision     |
# | 2        | No education item |


library(data.table)
library(ggplot2)
library(glue)


# data prep -----------------------------------------------------------------------------------

get_data_files <- function() {
  dir <- "FILEPATH"

  years <- c(2000:2019)

  
  # Unknown edu only remains in states that didn't report education
  data_files <- file.path(dir, paste0("data_", years, "_cleaned.csv"))
  data_files <- data_files[file.exists(data_files)]
  stopifnot(length(data_files) == length(years))
  return(data_files)
}

process_one_year <- function(full_filepath) {

  dt <- fread(full_filepath)
  if (!("edu_flag" %in% names(dt))) stop(glue("edu_flag doesn't exist in file full_filepath"))
  dt <- dt[, list(deaths = sum(deaths)), by = "year,state_res_numeric,state_res_alpha,state_occ_numeric,state_occ_alpha,edu_flag,education"]
  return(dt)
}

get_edu_flag_deaths_data <- function(data_files) {

  edu_flag_deaths <- rbindlist(lapply(data_files, process_one_year), use.names = T)

  edu_flag_deaths[, edu_revision := dplyr::case_when(edu_flag == 0 ~ "1989 revision", edu_flag == 1 ~ "2003 revision",
                                                     edu_flag == 2 ~ "no education item")]

  return(edu_flag_deaths)
}


# table of edu flag death percents by RESIDENCE --------------------------------------------------
gen_edu_flag_res_perc <- function(edu_flag_deaths) {
  dt_res <- data.table::copy(edu_flag_deaths) # COPY this for safe keeping

  # keep only 50 states + DC in state of RESIDENCE
  dt_res <- dt_res[(state_res_numeric %in% 1:56)]

  # drop state of OCCURRENCE
  dt_res[, state_occ_numeric := NULL]
  dt_res[, state_occ_alpha := NULL]

  # collapse to state of RESIDENCE
  dt_res <- dt_res[, list(deaths = sum(deaths)), by = "year,state_res_numeric,state_res_alpha,edu_flag,edu_revision"]

  # make percents
  dt_res <- dt_res[, list(edu_flag, edu_revision, deaths, percent = 100 * deaths / sum(deaths)), by = "year,state_res_alpha,state_res_numeric"]

  # This is the table that has the percents of deaths in each edu_flag, useful for plotting
  setkeyv(dt_res, c("state_res_alpha", "year"))

  return(dt_res)
}


# table of edu flag death percents by OCCURRENCE --------------------------------------------------
gen_edu_flag_occ_perc <- function(edu_flag_deaths) {
  dt_occ <- data.table::copy(edu_flag_deaths) # COPY this for safe keeping

  # keep only 50 states + DC in state of OCCURRENCE
  dt_occ <- dt_occ[(state_occ_numeric %in% 1:56)]

  # drop state of RESIDENCE
  dt_occ[, state_res_numeric := NULL]
  dt_occ[, state_res_alpha := NULL]

  # collapse to state of OCCURRENCE
  dt_occ <- dt_occ[, list(deaths = sum(deaths)), by = "year,state_occ_numeric,state_occ_alpha,edu_flag,edu_revision"]

  # make percents
  dt_occ <- dt_occ[, list(edu_flag, edu_revision, deaths, percent = 100 * deaths / sum(deaths)), by = "year,state_occ_alpha,state_occ_numeric"]

  # This is the table that has the percents of deaths in each edu_flag, useful for plotting
  setkeyv(dt_occ, c("state_occ_alpha", "year"))

  return(dt_occ)
}


# table of majority edu flag ------------------------------------------------------------------

# this is to get the year that a state changed versions of the death certificate
# we want to use occurrence because the state where the death happens determines which death certificate is used.
gen_death_cert_table <- function(dt_occ) {
  # use OCCURENCE for the year
  death_cert_change_table <- data.table::copy(dt_occ) # COPY this for safe keeping


  # using the same table, create largest percentage within a state-year group
  death_cert_change_table <- death_cert_change_table[, list(edu_revision, edu_flag, deaths, max_percent = max(percent), percent), by = "year,state_occ_alpha,state_occ_numeric"]

  # select the maximum percentage within a state-year group, also drop the max_percent column / clean up
  death_cert_change_table <- death_cert_change_table[percent == max_percent, .(edu_revision,edu_flag,year,state_occ_alpha,state_occ_numeric)]

  # check that the filtering worked by asserting that the table is unique by state and year
  stopifnot(nrow(death_cert_change_table) == nrow(unique(death_cert_change_table[,.(year, state_occ_alpha)])))

  # save
  setkeyv(death_cert_change_table, c("state_occ_alpha", "year"))

  return(death_cert_change_table)
}


# switch year ---------------------------------------------------------------------------------
get_switch_year_single_state <- function(death_cert_change_table, state) {
  s <- copy(state) # avoid name collision
  # get the year in the table where the edu_flag changes, ignoring the change from no edu to some edu
  temp_table <- data.table::copy(death_cert_change_table[state_occ_numeric == s & edu_flag != 2])
  if (length(unique(temp_table$edu_flag)) == 1) warning(glue("Only one edu revision in state {s}"))
  if (nrow(temp_table) == 0) stop("No rows in temp_table")

  # this gets the index / row where the value changes
  row_index <- which(c(FALSE, tail(temp_table$edu_flag,-1) != head(temp_table$edu_flag,-1)))

  # grab the whole row
  row <- temp_table[row_index]

  # get the year from the row
  switch_year <-  row$year

  if (s == 46) {
    switch_year = 2004
  }

  return(switch_year)
}


gen_year_switch_table <- function(states) {
  datalist = list()

  # apply the function to ever state
  for (i in c(1:length(states))) {
    s <- states[i]
    print(s)
    switch_year <- get_switch_year_single_state(death_cert_change_table, s)
    temp <- data.table("state_occ_numeric" = s, "switch_year" = switch_year)
    datalist[[i]] <- temp
  }

  year_switch_table <- rbindlist(datalist, use.names = T)

  return(year_switch_table)
}


# proportion of deaths in old format (covariate) ----------------------------------------------

# the proportion of deaths coded to the “old” death certificate, by age, sex, year and state of
# residence (not occurrence)
# INTERNAL_COMMENT

# Unknown edu only remains in states that didn't report education

# uses state of residence
process_one_year_full_demog <- function(full_filepath, id_cols, state) {

  stopifnot((state == "res" | state == "occ"))
  message(glue("state is set to {state}"))

  dt <- fread(full_filepath)

  if (!("edu_flag" %in% names(dt))) stop(glue("edu_flag doesn't exist in file full_filepath"))

  # filter year
  dt <- dt[year <= 2019]

  # filter to adult ages
  dt <-
    dt[!(
      age %in% c(
        "1-5mo",
        "10",
        "2-4",
        "7d",
        "6-11mo",
        "0d",
        "12-23mo",
        "5",
        "999i",
        "999",
        "15",
        "20"
      )
    ),]
  # prop <- prop[age %in% c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)]
  dt[, age := as.integer(age)]
  dt[age %in% c(90, 95), age := 85]

  # set state_res or state_occ to state
  setnames(dt, glue("state_{state}_numeric"), "state_numeric", skip_absent = F)
  setnames(dt, glue("state_{state}_alpha"), "state_alpha", skip_absent = F)
  message(glue("state_{state}_numeric has been renamed to state_numeric"))
  message(glue("state_{state}_alpha has been renamed to state_alpha"))

  # rename education
  setnames(dt, "education", "edu")

  dt <- dt[, list(deaths = sum(deaths)), by = id_cols]
  return(dt)
}

get_edu_flag_deaths_data_demog <- function(data_files, id_cols, state) {
  
  edu_flag_deaths_demog <-
    rbindlist(lapply(data_files, process_one_year_full_demog, id_cols = id_cols, state = state), use.names = T)

  edu_flag_deaths_demog[, edu_revision := dplyr::case_when(
    edu_flag == 0 ~ "1989 revision",
    edu_flag == 1 ~ "2003 revision",
    edu_flag == 2 ~ "no education item"
  )]

  # keep only 50 states + DC
  edu_flag_deaths_demog <- edu_flag_deaths_demog[state_numeric %in% 1:56, ]
  stopifnot(length(unique(edu_flag_deaths_demog$state_alpha)) == 51) # check that all states are present

  return(edu_flag_deaths_demog)
}

gen_prop_1989_revision <- function(edu_flag_deaths_demog, id_cols) {

  prop <- data.table::copy(edu_flag_deaths_demog) # COPY this for safe keeping

  # collapse deaths
  prop <-
    prop[, list(deaths = sum(deaths)),
         by = id_cols]

  
  square <-
    as.data.table(expand.grid(
      year = unique(prop$year),
      state_alpha = unique(prop$state_alpha),
      sex = c(1, 2),
      edu_flag = c(0, 1, 2),
      edu = c(100, 101, 102, 103, 104),
      age = c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)
    ))
  drop_these_cols <- names(square)[!(names(square) %in% id_cols)]
  for (var in drop_these_cols) {
    square[, eval(var) := NULL]
  }
  if (any(drop_these_cols %in% names(square))) stop()

  square <- unique(square)

  # add edu_revision text
  square[, edu_revision := dplyr::case_when(
    edu_flag == 0 ~ "1989 revision",
    edu_flag == 1 ~ "2003 revision",
    edu_flag == 2 ~ "no education item"
  )]
  # add state_numeric
  square <-
    merge(square, unique(prop[, .(state_alpha, state_numeric)], by = "state_alpha", all.x = T))

  prop <-
    merge(
      square,
      prop,
      by = id_cols,
      all.x = T
    )
  prop[is.na(deaths), deaths := 0]
  if (any(is.na(prop))) stop("NAs present")

  # make proportions
  denominator_cols <- id_cols[id_cols != "edu_flag"]
  denominator_cols <- denominator_cols[denominator_cols != "edu_revision"]
  prop <- prop[, list(edu_flag, edu_revision, deaths, deaths_denom = sum(deaths)),
                by = denominator_cols]
  prop[, prop := deaths / deaths_denom]
  prop[deaths_denom == 0, prop := 0] # fix zero divided by zero, premised on deaths_denom being correct

  # check that props sum to 1 for a year, age, state, and education
  prop[, check_sum := sum(prop), by = denominator_cols]
  # check_sum can be 0 if there isn't a prop, when deaths_denom is 0
  stopifnot(all.equal(current = prop[deaths_denom != 0, check_sum],
                      target = rep(1, nrow(prop[deaths_denom != 0, ])))) # if this fails it means that a prop didn't sum to 1.

  # select the 1989 revision
  prop <- prop[edu_revision == "1989 revision", ]

  # clean up
  keep_cols <- c(id_cols, "edu_revision", "prop")
  drop_these_cols <- names(prop)[!(names(prop) %in% keep_cols)]
  for (var in drop_these_cols) {
    prop[, eval(var) := NULL]
  }
  if (any(drop_these_cols %in% names(prop))) stop()
  stopifnot("prop" %in% names(prop))

  return(prop)
}

# repeat values across mcnty ------------------------------------------------------------------

add_mcnty_to_props <- function(prop, id_cols) {

  mcnty <- fread('FILEPATH')

  prop[, state := state_numeric]

  square <-
    as.data.table(expand.grid(
      year = unique(prop$year),
      mcnty = unique(mcnty[, mcnty]),
      sex = c(1, 2),
      edu = c(100, 101, 102, 103, 104),
      age = c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)
    ))

  # drop any columns in square that are not in id_cols, except for mcnty, which we want to add
  # this is so that we can create a fully square data.table flexibly, depending on the values of
  # id cols.
  drop_these_cols <- names(square)[!(names(square) %in% id_cols)]
  drop_these_cols <- drop_these_cols[drop_these_cols != "mcnty"]
  for (var in drop_these_cols) {
    square[, eval(var) := NULL]
  }
  if (any(drop_these_cols %in% names(square))) stop()

  square <- unique(square)

  # add state (numeric) info
  square <- merge(square, unique(mcnty[, .(mcnty, state)]), by = "mcnty", all.x = T)
  stopifnot("state" %in% names(square))
  if (any(is.na(square))) stop("NAs in square")
  setnames(square, "state", "state_numeric")

  # add state alpha
  square <- merge(square, unique(prop[, .(state_numeric, state_alpha)]), by = "state_numeric", all.x = T)
  stopifnot("state_alpha" %in% names(square))
  if (any(is.na(square))) stop("NAs in square")

  # add edu_flag
  square[, edu_flag := 0]

  prop <-
    merge(
      square,
      prop,
      by = id_cols,
      all.x = T
    )
  if (any(is.na(prop$mcnty))) stop("mcnty column of prop has NAs values")
  if (any(is.na(prop))) stop("prop has NAs values")

  # clean up some more
  prop[, state_numeric := NULL]
  prop[, state_alpha := NULL]
  prop[, state := NULL]

  check_cols <- c(id_cols, "mcnty")
  check_cols  <- check_cols[check_cols != "state_alpha"]
  check_cols  <- check_cols[check_cols != "state_numeric"]


  # some checks
  if (!(nrow(unique(prop[, check_cols, with = F])) == nrow(prop))) {
    stop("Rows of prop are not unique.")
  }
  if (any(is.na(prop$mcnty))) stop("NAs in mcnty column of prop")
  if (any(is.na(prop))) stop("prop has NAs in some column(s)")

  stopifnot("prop" %in% names(prop))

  return(prop)
}


# make a covariate file -----------------------------------------------------------------------

add_1989_revision_props_to_covars <- function(prop, id_cols) {

  covar <- data.table::copy(prop)

  covar[, edu_flag := NULL]
  covar[, edu_revision := NULL]

  setnames(covar, "prop", "prop_1989_revision")
  
  return(covar)
}


# run -----------------------------------------------------------------------------------------

out_dir <-
  "FILEPATH"

data_files <- get_data_files()

id_cols_agg <-   c("year",
                   "state_numeric",
                   "state_alpha",
                   "edu_flag")
edu_flag_deaths_demog <- get_edu_flag_deaths_data_demog(data_files, id_cols_agg, state = "res")


plotting_data <- edu_flag_deaths_demog[state_alpha == "GA", ]
plotting_data <- plotting_data[, list(deaths = sum(deaths)), by = "year,state_numeric,state_alpha,edu_flag,edu_revision"]

plotting_data <- plotting_data[, list(edu_flag, edu_revision, deaths, deaths_denom = sum(deaths)), by = "year,state_numeric,state_alpha"]
plotting_data[, prop := deaths / deaths_denom]

p <- ggplot2::ggplot(
  data = plotting_data,
  aes(x = year, y = prop, by = edu_revision, color = edu_revision)
) +
  geom_line() +
  geom_point() +
  labs(title = "Georgia: proportion of deaths in each education format (death certificate revision)") +
  theme_minimal()
ggsave(filename = file.path(out_dir, "ga_all_revision_props.png"),plot = p, device = "png")
plot(p)


# double check that only the 50 states + DC are present
stopifnot(length(unique(edu_flag_deaths_demog$state_alpha)) == 51) # check that all states are present
# compute proportions of deaths in using 1989 death certificate revision
prop_agg <- gen_prop_1989_revision(edu_flag_deaths_demog, id_cols_agg)

# add in reporting status column
prop_agg <- add_mcnty_to_props(prop_agg, id_cols = id_cols_agg)

# attach proportions to subpop covariates
covar <- add_1989_revision_props_to_covars(prop_agg, id_cols = id_cols_agg)


saveRDS(covar,
        "FILEPATH")


# altered prop for non-reporting ---------------------------------------------------------------

# want to set the prop to zero in the state-years that don't report

# get reporting info, add mcnty to it
reporting <- fread("FILEPATH")
mcnty <- fread('FILEPATH')

reporting <- merge(unique(mcnty[, .(mcnty, state)]), reporting, by = "state", all.y = T)


# just read it back in for dev speed
covar <- readRDS("FILEPATH")
start_nrow <- nrow(covar)
# merge reporting status onto covar
covar <- merge(covar, reporting[, .(mcnty, year, reporting_status)], by = c("year", "mcnty"), all.x = T)
if (start_nrow != nrow(covar)) stop("Merge of covar with reporting changed the number of rows.")

# fill in the reporting status: if it is absent, then it did report:
covar[is.na(reporting_status), reporting_status := 1]
if (any(is.na(covar))) stop("NAs after merging on reporting status")

# eliminate the props in the state years where edu was not reported
covar[reporting_status == 0, prop_1989_revision := 0]

# save
saveRDS(covar,
        "FILEPATH")
