####################################################################################################
## Description: Create proportions to split unknown education in states Georgia, Rhode Island, and
## South Dakota, in the years 2000-2002. It's called backcasting because it is taking information
## from after 2002 and applying it backwards in time. The proportions attempt to be specific as
## possible, but are more broad if the denominator is not large enough. Our threshold for that is
## 25 births. This script is ran after imputation and interpolation has been ran.
##
## The years 2003-2005 are used to create proportions. Data is pooled across these three years for
## all proportions. These years are selected because they a) start right after the time period we
## apply the proportions to, b) because we cannot pool data in years that contain a change-over from
## one birth certificate format / education encoding to another, since that can lead to changes in
## relative size of each education group, and pooling those changes would make bad proportions, and
## c) it is helpful to keep the year pooling consistent for every state. South Dakota changes birth
## certificate in 2006.
####################################################################################################

library(data.table)
library(R.utils)
library(stringr)

# reads in births from FILEPATH
# after imputation has been applied
get_pre_backcasting_data <- function() {

  file_years = c(2003:2005)

  files = paste0("FILEPATH", file_years, "_interpolated.csv")
  stopifnot(length(files) == length(file_years))
  DT <- rbindlist(lapply(files, fread), use.names = T)
  stopifnot(all(unique(DT$year) == file_years))

  setnames(DT, "full_fips_res_numeric", "cnty") # must use residence, not occurrence

  loc <- fread('FILEPATH/merged_counties.csv')

  DT <- merge(DT, unique(loc[, .(cnty, mcnty, state_name)]), by = "cnty")
  if (any(is.na(DT$mcnty))) stop("Missing mcnty in some rows.")

  # subset to states
  DT <- DT[state_name %in% c("Georgia", "South Dakota", "Rhode Island")]
  stopifnot(length(unique(DT$state_name)) == 3)

  # rename and map edu
  edu_map = data.table(edu = c(101,102,103,104,100),
                       edu_label = c('Less than HS',
                                     'HS graduate',
                                     'Some college',
                                     'College graduate',
                                     'Unknown'))

  DT <- merge(DT, edu_map, by = "edu")
  if (any(is.na(DT$edu))) stop("missing education")

  DT <- merge(DT, data.table(mother_age = c(10,15,20,25,30,35,40,45,50),
                             age_group = c(1,2,3,4,5,5,6,6,6)),
              by = 'mother_age')

  DT <- DT[, list(births = sum(births)), by = "mcnty,year,sex,mother_age,age_group,age,edu,edu_label"]

  # square it
  square <- as.data.table(expand.grid(year = unique(DT$year), mcnty = unique(DT$mcnty), sex = c(1,2), edu = unique(DT$edu), mother_age = unique(DT$mother_age), age = c(0)))

  # add edu_labels to square data
  square <- merge(square, unique(edu_map[, .(edu, edu_label)]), by = "edu", all.x = T)

  DT = merge(square, DT, by = c("year", "mcnty", "sex", "edu", "edu_label", "mother_age", "age"), all.x = TRUE)

  # age_group is null on the imputed rows, so drop it and re-do it
  DT[, age_group := NULL]
  DT <- merge(DT, data.table(mother_age = c(10,15,20,25,30,35,40,45,50),
                             age_group = c(1,2,3,4,5,5,6,6,6)),
              by = 'mother_age')

  DT[is.na(births), births := 0]
  if(any(is.na(DT))) stop("NAs in DT")

  DT <- DT[, list(births = sum(births)), by = "mcnty,year,sex,age_group,mother_age,age,edu,edu_label"]
  stopifnot("year" %in% names(DT))
  return(DT)
}

# filter to the states we want
prep_births_prop <- function(data) {
  DT <- data.table::copy(data)

  # subset years, we only need to use data after 2003 and we don't need data after 2005
  DT <- DT[year %in% c(2003:2005), ]

  # get location info
  loc <- fread('FILEPATH/merged_counties.csv')
  setnames(loc, "state", "state_fips")

  # add location state name
  DT <- merge(DT, unique(loc[, list(mcnty, state_fips, state_name)]), all.x = T)
  if (any(is.na(data$state_name))) stop()

  # subset state
  DT <- DT[state_name %in% c("Georgia", "Rhode Island", "South Dakota"), ]

  # first collapse births to our most detailed level, that we can use for our numerator of the first prop (across sex)
  DT <- DT[, list(births = sum(births)), by  = "year,mcnty,age,age_group,mother_age,edu,edu_label,state_name,state_fips"]

  # drop unknown education. Check that we aren't dropping any births. This enforces the assumption
  # that imputation has been ran.
  stopifnot(DT[edu_label == "Unknown", sum(births)] == 0)
  DT <- DT[edu_label != "Unknown"]

  return(DT)
}

# Proportion 1: proportions are made across time, and are specific to a mcnty, age_group, and edu
make_prop_1 <- function(data, years) {

  # cannot include 2006
  if (any(years > 2005)) stop("years cannot be larger than 2005, because of when SD changed birth certificate formats")

  # function to pool variable sets of years, and make proportions from pooled data
  # subset to years we want to pool
  DT <- data.table::copy(data[year %in% years]) # filter

  DT <- DT[, list(births = sum(births)), by = "state_fips,mcnty,age,age_group,mother_age,edu,edu_label,state_name"]

  # make numerator
  DT[, births_1_num := sum(births), by = "mcnty,age_group,edu"]

  # check for NA
  if (any(is.na(DT$births_1_num))) stop("NAs present in births_1_num, this would cause downstream issues in all proprotions.")

  # and denominator
  # it's ok to sum across years because we already subsetted to the years we want to pool
  DT[, births_1_denom := sum(births), by = "mcnty,age_group"] # removed year

  # proportion
  DT[, prop_1 := births_1_num / births_1_denom]
  
  DT[births_1_num == 0 & births_1_denom == 0, prop_1 := 0] # avoid NaN from divide by 0

  # values are now repeated across year
  DT[, births := NULL] # avoid merge issues, ie births.x, births.y

  # check that values sum to 1, summing across the columns that represent the denominator
  # do this before renaming the prop column for easier code
  # do this when denominator is not zero, because anything divided by zero is junk
  test = unique(DT[births_1_denom != 0, list(mcnty, age_group, edu, prop_1)])
  test = test[, list(total_prop = sum(prop_1)), by = "mcnty,age_group"]
  stopifnot(all.equal(current = test[, total_prop], target = rep(1, nrow(test))))

  return(DT)
}

# Proportion 2: pool across counties in a metro/non-metro area. proportions are specific to an edu,
# age_group, and metro/non-metro area. denominator is the age_group, and metro/non-metro area.
# proportions sum to 1 across the metro/non-metro area, age_group.
make_prop_2 <- function(data) {
  DT <- data.table::copy(data)

  # grab metro and non metro mcntys
  # code below grabs the most recent version based on date/time of creation
  files <- file.info(list.files(paste0('FILEPATH',
                                       'raw/usda_urban_rural_codes'), full.names = TRUE))
  file <- rownames(files)[which.max(files$mtime)]
  urban_cov <- data.table(readRDS(file)); rm(file)

  # get loc
  loc = fread('FILEPATH/merged_counties.csv')
  urban_cov <- merge(urban_cov, unique(loc[, c('mcnty','cnty','state','state_name')]),
                     by.x = 'fips', by.y = 'cnty', all.x = T)

  # subset to our states of interest: this helps with checking
  urban_cov = urban_cov[state_name %in% c("Georgia", "South Dakota", "Rhode Island")]
  # subset to the years that matter: 2003
  # There are three year points in this: 1993, 2003, and 2013
  urban_cov = urban_cov[year == 2003, ]

  # set metro status based on urban/rural status
  urban_cov[, metro := ifelse(rur_urb_code %in% 1:3, 1, 0)]

  # only keep mcnty and metro, and make unique
  urban_cov <- unique(urban_cov[,c('mcnty','metro')])

  # label metro area
  start_rows = nrow(DT)
  DT <- merge(DT, urban_cov, by = "mcnty", all.x = T)
  stopifnot(!any(is.na(DT$metro)))
  stopifnot(start_rows == nrow(DT))

  # numerator
  DT[, births_2_num := sum(births_1_num, na.rm = T), by = "state_fips,metro,age_group,edu"]

  # denominator
  DT[, births_2_denom := sum(births_1_num, na.rm = T), by = "state_fips,metro,age_group"]

  # proportion
  DT[, prop_2 := births_2_num / births_2_denom]

  # check sum to 1: have to make unique, since there are multiple rows for a singe state_fips,metro,age_group,edu, because there are multiple counties props repeat across mcnty, for each state_fips,metro,age_group,edu
  test = unique(DT[, list(state_fips,metro,age_group,edu,prop_2)])
  test[, total_prop := sum(prop_2), by = "state_fips,metro,age_group"]
  stopifnot(all.equal(current = test[, total_prop], target = rep(1, nrow(test))))

  return(DT)
}

# Proportion 3: pool across the counties in the state. Proportions are specific to a state, edu, and
# mother_edu. Denominator is state, age_group, and year. proportions sum to 1 across the state,
# age_group, and year
make_prop_3 <- function(data) {

  # prop 3: same as prop 2, but across the whole state
  # similar to prop_2, need to preserve the year pooling
  DT <- data.table::copy(data)

  # numerator
  DT[, births_3_num := sum(births_1_num),  by = "age_group,state_fips,edu"]

  # denominator
  DT[, births_3_denom := sum(births_1_num), by = "age_group,state_fips"]

  # proportion
  DT[, prop_3 := births_3_num / births_3_denom]
  DT[births_3_num == 0 & births_3_denom == 0, prop_3 := 0] # avoid NaN from divide by 0

  # check sum to 1
  test = unique(DT[, list(state_fips,age_group,edu,prop_3)])
  test[, total_prop := sum(prop_3), by = "age_group,state_fips"]
  stopifnot(all.equal(current = test[, total_prop], target = rep(1, nrow(test))))

  return(DT)
}

# Proportion 4: Proportions are specific to a state and edu. These proportions pool data across
# age_group. Proportions sum to 1 across the state.
make_prop_4 <- function(data) {
  # prop 4: same as prop 4, but across the whole state
  # similar to previous props, need to preserve the year pooling
  DT <- data.table::copy(data)

  # numerator
  DT[, births_4_num := sum(births_1_num),  by = "state_fips,edu"]

  # denominator
  DT[, births_4_denom := sum(births_1_num), by = "state_fips"]

  # proportion
  DT[, prop_4 := births_4_num / births_4_denom]

  DT[births_4_num == 0 & births_4_denom == 0, prop_4 := 0] # avoid NaN from divide by 0

  # check sum to 1
  test = unique(DT[births_4_denom != 0, list(state_fips,edu,prop_4)])
  test[, total_prop := sum(prop_4), by = "state_fips"]
  stopifnot(all.equal(current = test[, total_prop], target = rep(1, nrow(test))))

  return(DT)
}

# Fill in proportions, depending on if the denominator is larger than min_sample_size.Ffill in the
# prop, numerator, and denominator, and note which level was used
fill_props <- function(data, min_sample_size) {

  DT = data.table::copy(data)

  # fill in zeros, because comparison to NA is NA, e.g.: ifelse(NA > min_sample_size, 0, 300) is NA
  DT[is.na(births_1_denom), births_1_denom := 0]
  DT[is.na(births_2_denom), births_2_denom := 0]
  DT[is.na(births_3_denom), births_3_denom := 0]
  DT[is.na(births_4_denom), births_4_denom := 0]

  # fill in prop
  DT[, prop := ifelse(births_1_denom > min_sample_size, prop_1,
                      ifelse(births_2_denom > min_sample_size, prop_2,
                             ifelse(births_3_denom > min_sample_size, prop_3, prop_4)))]

  # fill in numerator
  DT[, births_num := ifelse(births_1_denom > min_sample_size, births_1_num,
                            ifelse(births_2_denom > min_sample_size, births_2_num,
                                   ifelse(births_3_denom > min_sample_size, births_3_num, births_4_num)))]
  # fill in denominator
  DT[, births_denom := ifelse(births_1_denom > min_sample_size, births_1_denom,
                              ifelse(births_2_denom > min_sample_size, births_2_denom,
                                     ifelse(births_3_denom > min_sample_size, births_3_denom, births_4_denom)))]
  # note which level was used
  DT[, prop_level := ifelse(births_1_denom > min_sample_size, "prop_1",
                            ifelse(births_2_denom > min_sample_size, "prop_2",
                                   ifelse(births_3_denom > min_sample_size, "prop_3", "prop_4")))]

  # re-apply threshold, just to be sure
  DT[births_denom < min_sample_size, prop := NA]
  DT[births_denom < min_sample_size, prop_level := NA]

  return(DT)
}

# Assumption:  restrict education for mothers aged 10-14 to "Less than HS". There actually is a
# small amount of births to moms aged 10-14 with a HS degree or more. If in the youngest age group,
# and edu isn't in unknown or "Less than HS", and births is non-zero, change edu to "Less than HS"
fix_age_group_1 <- function(data) {
  DT <- data.table::copy(data)

  # detect columns that have props
  prop_cols = names(DT)[names(DT) %like% "prop_"]

  DT[age_group == 1, (prop_cols) := ifelse(edu_label == "Less than HS", 1, 0)]

  return(DT)
}

clean_births_prop <- function(data) {
  DT <- data.table::copy(data)
  DT = DT[, list(
    state_fips,
    state_name,
    mcnty,
    age,
    age_group,
    mother_age,
    edu,
    edu_label,
    births_num,
    births_denom,
    prop,
    prop_level
  )]
  return(DT)
}

# Check that the proportions pass a series of tests.
check_props <- function(data, min_sample_size) {

  births_prop_test = data.table::copy(data)

  # are there any missing things?
  if(any(is.na(births_prop_test$prop))) stop("Some prop are missing")
  if(any(is.na(births_prop_test$prop_level))) stop("Some prop_level are missing")

  # is denom big enough?
  if(!min(births_prop_test$births_denom) >= min_sample_size) stop()

  # is num / denom the same as prop?
  # need to exclude the rows that we set the prop to 1 or 0 due to age_group 1 in the fix_age_group_1
  # function
  filter = births_prop_test[age_group == 1 & (prop == 1 | prop == 0), .I]
  births_prop_test[, test_prop := births_num / births_denom]
  stopifnot((nrow(births_prop_test[prop != test_prop & (!filter)]) == 0))

  # Check that the props sum to one for their group, but first, check that the columns needed for
  # this test are present
  stopifnot("prop" %in% names(births_prop_test))
  stopifnot("mcnty" %in% names(births_prop_test))
  stopifnot("age_group" %in% names(births_prop_test))
  stopifnot("mother_age" %in% names(births_prop_test))
  # unique gets rid of repeats in mother_age across age_group
  test <- unique(births_prop_test[, list(edu, mcnty, age_group, prop)])
  test <- test[, total_prop := sum(prop), by = "mcnty,age_group"] # sum the proportions
  stopifnot(all.equal(target = rep(1, nrow(test)), current = test[, total_prop])) # test closeness to 1

}

# main function to create proportions
make_births_education_props <- function(min_sample_size = 25, clean = T) {
  births_data <- get_pre_backcasting_data()
  births_data <- prep_births_prop(data = births_data)

  births_prop <- make_prop_1(data = births_data, years = c(2003:2005)) 
  births_prop <- make_prop_2(data = births_prop)
  births_prop <- make_prop_3(data = births_prop)
  births_prop <- make_prop_4(data = births_prop)

  # set props for age_group zero in less than HS to be 1, and other edu groups to 0
  
  births_prop <- fix_age_group_1(data = births_prop)

  births_prop <- fill_props(data = births_prop, min_sample_size = min_sample_size)
  if (clean) {
    births_prop <- clean_births_prop(births_prop)
  }

  check_props(data = births_prop, min_sample_size = min_sample_size)
  return(births_prop)
}

save_props <- function(props, filpath) {
  saveRDS(filepath)
}

# main function. This saves births data for 2000-2002 that has been backcasted to remove unknown
# education.
apply_backcasting_props <- function(archive_date) {

  # read in the data that will be backcasted.
  births <- rbindlist(lapply(2000:2002, function(yr) {
    fread(paste0("FILEPATH", yr, "_interpolated.csv"))
  }), use.names = T)

  births <- births[state_res_numeric %in% 1:56]  # keep only 50 states + DC

  # after filtering to 50 states + DC, can now save the total number of births
  total_births <- sum(births$births)

  mcnty <- fread("FILEPATH/merged_counties.csv")
  births[mcnty, on = c("full_fips_res_numeric" = "cnty"), mcnty := i.mcnty]  # add mcntys
  if(any(is.na(births$mcnty))) stop("There are NAs in mcnty")

  # split data into things we are changing and things we are not. Also, take note of certain aspects
  # of the data so that we can ensure they stay the same after we make our changes.
  start_rows = nrow(births)
  dont_touch <- births[!(state_res_alpha %in% c("GA", "SD", "RI")), ]
  births <- births[state_res_alpha %in% c("GA", "SD", "RI"), ]
  stopifnot(length(unique(births$state_res_alpha)) == 3)
  stopifnot(start_rows == nrow(births) + nrow(dont_touch))
  stopifnot(sum(births$births) + sum(dont_touch$births) == total_births)

  # now that we've separated out the data we will modify, save some info about the total number
  # of births in the states that are being modified.
  total_births_backcasting_states <- sum(births$births)

  # Add age_group to births. The proportions have these age groups
  births <- merge(births, data.table(mother_age = c(10,15,20,25,30,35,40,45,50),
                                     age_group = c(1,2,3,4,5,5,6,6,6)),
                  by = 'mother_age')

  # clean up
  
  births[, c("full_fips_res_numeric",
             "education_orig", "edu_flag", # we must collapse across education in order to apply proportions and split into edu groups, and don't need this education metadata
             "rec_weight", # this is always 1
             "state_occ_numeric", "state_occ_alpha", "county_occ", "full_fips_occ_numeric" # don't need occurrence metadata
  ) := NULL]

  # Collapse education: this necessary in order to apply the proportions and split all edu births
  # into our edu groups. Also collapse across geographic info about occurrence, but preserving geo
  # info about residence

  
  # the mcnty level, can repeat across county
  births <- births[, list(births = sum(births)), by = "mother_age,year,state_res_numeric,state_res_alpha,sex,age,nid,mcnty,age_group,county_res"]

  # rename births to distinguish it from our upcoming split births
  setnames(births, "births", "births_orig")

  # get proportions
  births_prop_25 = make_births_education_props(min_sample_size = 25, clean = T)
  births_prop_25[, c("state_name", "prop_level", "births_num", "births_denom") := NULL]

  # Merge the proportions on. Proportions don't have sex or year: need to repeat
  births <- merge(births, births_prop_25, by = c("age_group", "mother_age", "age", "mcnty"), allow.cartesian = T)
  if(any(is.na(births$prop))) stop("Not all rows have proportions!")

  # This is backcasting
  # apply the proportions
  births[, births := births_orig * prop]

  # testing
  stopifnot(total_births_backcasting_states == sum(births$births))

  # clean up births
  births[, births_orig := NULL]
  births[, prop := NULL]
  births[, age_group := NULL]
  births[, state_fips := NULL]

  # clean up the data that we DIDN'T modify
  # add edu labels
  edu_map = data.table(edu = c(101,102,103,104,100),
                       edu_label = c('Less than HS',
                                     'HS graduate',
                                     'Some college',
                                     'College graduate',
                                     'Unknown'))

  dont_touch <- merge(dont_touch, edu_map, by = "edu", all.x = T)
  if(any(is.na(dont_touch$edu))) stop("NAs in edu")

  # re-append the other births that we didn't modify
  stopifnot(length(setdiff(names(births), names(dont_touch))) == 0) # assert that dont_touch has everything that births has
  not_in_births_cols = setdiff(names(dont_touch), names(births)) # make list of columns that are in dont_touch but not in births

  # drop the columns of dont_touch that are not in births, to avoid NAs when we append the two data.tables
  dont_touch <- dont_touch[, !not_in_births_cols, with = F]

  final <- rbindlist(
    list(
      dont_touch,
      births
    ), use.names = TRUE
  )
  if (any(is.na(final))) stop("There are NAs")

  # check that total number of births in all states did not change
  stopifnot(sum(final$births) == total_births)

  # check for county_res, need it for database.
  stopifnot("county_res" %in% names(final))

  # drop mcnty
  final[, mcnty := NULL]

  # final collapse, this saves space by collapsing the stuff that wasn't modified.
  final = final[, list(births = sum(births)), by = "year,state_res_numeric,state_res_alpha,county_res,sex,mother_age,age,nid,edu,edu_label"]

  # Save backcasted data for years that had backcasting happen
  
  dir.create(glue::glue("FILEPATH"))
  for (this_year in c(2000:2002)) {
    temp <- final[year == this_year, ]
    this_filename = glue::glue("FILEPATH")
    message(glue::glue("Saving backcasted data for year {this_year} in file {this_filename}"))
    fwrite(x = temp, file = this_filename)
    archive_filename = glue::glue("FILEPATH")
    fwrite(x = temp, file = archive_filename)
    rm(this_filename, temp, this_year)
  }

  # Modify the year files that we didn't apply backcasting to, so that they have the same format as
  # the backcasted files. This means mapping the "edu" to "edu_label", mapping to mcnty, dropping
  # out non-US locations, and dropping columns.
  for (this_year in c(2003:2020)) { # this year selection is critical, must not include 2000-2002
    this_filepath = glue::glue("FILEPATH")
    message(glue::glue("Working on year {this_year} and file {this_filepath}..."))
    DT <- fread(this_filepath)

    # filter to US states
    DT <- DT[state_res_numeric %in% 1:56]
    start_births = sum(DT$births)

    DT[mcnty, on = c("full_fips_res_numeric" = "cnty"), mcnty := i.mcnty]  # add mcntys
    if (any(is.na(DT$mcnty))) stop("NAs in mcnty")
    stopifnot(sum(DT$births) == start_births)

    # rename and map edu
    edu_map = data.table(edu = c(101,102,103,104,100),
                         edu_label = c('Less than HS',
                                       'HS graduate',
                                       'Some college',
                                       'College graduate',
                                       'Unknown'))

    DT <- merge(DT, edu_map, by = "edu")
    if (any(is.na(DT$edu))) stop("There are missing edu values")
    stopifnot(sum(DT$births) == start_births)

    DT <- DT[, list(births = sum(births)), by = "year,state_res_numeric,state_res_alpha,sex,mother_age,age,nid,edu,edu_label,mcnty,county_res"]
    stopifnot(sum(DT$births) == start_births)

    end_births = sum(DT$births)
    stopifnot(start_births == end_births)

    
    stopifnot("county_res" %in% names(DT))

    # drop mcnty
    DT[, mcnty := NULL]

    # save
    fwrite(DT, glue::glue("FILEPATH"))
    archive_filename = glue::glue("FILEPATH")
    fwrite(x = DT, file = archive_filename)
    rm(start_births, end_births, this_filepath, this_year)
  }

  # check that a file exists for all years:
  for (this_year in c(2000:2020)) {
    this_filename = glue::glue("FILEPATH")
    stopifnot(file.exists(this_filename))
  }

  # check that the files for the years that were backcasted have the same format as the files for
  # the years that were not backcasted. ie, check that the columns in 2002 match 2003. any two years
  # from the two groups will do
  message("Checking that formats match...")
  test_2002 = fread("FILEPATH/births_2002_cleaned.csv")
  test_2003 = fread("FILEPATH/births_2003_cleaned.csv")
  stopifnot(setequal(names(test_2002), names(test_2003)))

  message("Done.")
}

get_post_backcasting_data <- function() {
  births <- rbindlist(lapply(2000:2002, function(yr) {
    fread(paste0("FILEPATH", yr, "_cleaned.csv"))
  }), use.names = T)

  return(births)
}

## Run ---------------------------------------------------------------------------------------------

if (getOption("run.main", default = TRUE)) {
  apply_backcasting_props()
}
