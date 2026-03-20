###################################################################################################
## Description: We need to make Census vintage 2021 PEP estimates more comparable with vintage
##              2020. We need intercensal population estimates for 2010-2020, since
##              those would be informed by the 2020 census in addition to the 2010 census instead
##              of only the 2010 census. The Census plans to release these in Fall 2024 (about 1
##              year from creation of this script), but we need them earlier so we will attempt to
##              estimate them ourselves. We can relicate the method the census bureau used to
##              produce 2000-2010 intercensal estimates. This involves modifying the equations in
##              the linked documentation to use July 1 (instead of April 1) population values as
##              well as postcensal estimates for 2020 based on the 2020 census instead of April 1,
##              2020 census estimates. Also, since there is an extra leap day between July 1, 2010 
##              and July 1, 2020 compared with April 1, 2000 and April 1, 2010, the max value of t
##              increases to 3653 from 3652 for our calculations.
##
##              See the link below for methodology documentation:
##              https://www2.census.gov/programs-surveys/popest/technical-documentation/methodology/intercensal/2000-2010-intercensal-estimates-methodology.pdf 
## 
## Input: 2010-2020 postcensal (2010) estimates and 2020 postcensal (2020) estimates
## Output: 2010-2020 intercensal estimates
## 
###################################################################################################

# Helper functions based on Census documentation (link above) -------------------------------------
# Das Gupta method for most cases
calc_das_gupta <- function(t,  # time (days) since July 1, 2010
                           q_t,  # postcensal (2010 census) pop at time t
                           p_3653,  # postcensal (2020 census) pop on July 1, 2020
                           q_3653) {  # postcensal (2010 census) pop on July 1, 2020
  
  p_t <- q_t*((p_3653/q_3653)^(t/3653))
  return(p_t)
}

# Linear interpolation method for the following edge cases:
# q_t = 0 OR
# q_3653 = 0 or 1 OR
# p_3653 = 0 or 1 OR
# q_3653 < p_3653/2
calc_linear_interp <- function(t,  # time (days) since July 1, 2020
                               p_3653,  # postcensal (2020 census) pop on July 1, 2020
                               p_0) {  # postcensal (2010 census) pop on July 1, 2010

  p_t <- (p_3653*(t/3653)) + (p_0*((3653-t)/3653))
  return(p_t)
}

# Main function to return estimates ---------------------------------------------------------------
create_intercensal <- function(pc10,  # post-2010 census estimates, data years 2010-2020
                               pc20_2020) {  # post-2020 census estimates, data year 2020
  # avoid editing of input datasets by making copies
  pc10 <- copy(pc10)
  pc20_2020 <- copy(pc20_2020)
  
  # clear any key variables
  setkeyv(pc10, NULL)
  setkeyv(pc20_2020, NULL)
  
  # remove year from 2020 postcensal to avoid merging on it
  pc20_2020[, year := NULL]
  
  # calculate variables for estimation
  setnames(pc20_2020, "pop", "p_3653")
  pc10_2020 <- copy(pc10[year == 2020])
  pc10_2010 <- copy(pc10[year == 2010])
  setnames(pc10_2020, "pop", "q_3653")
  setnames(pc10_2010, "pop", "p_0")
  pc10 <- merge.data.table(pc10, pc10_2020[, -"year"], all.x = T, allow.cartesian = T)
  stopifnot(nrow(pc10[is.na(q_3653)]) == 0)
  pc10 <- merge.data.table(pc10, pc10_2010[, -"year"], all.x = T, allow.cartesian = T)
  stopifnot(nrow(pc10[is.na(p_0)]) == 0)
  rm(pc10_2020, pc10_2010)
  pc10[, t := case_when(year < 2012 ~ (year - 2010)*365,
                        year %in% 2012:2015 ~ (year - 2010)*365 + 1,  # count 1 leap day in Feb 2012
                        year %in% 2016:2019 ~ (year - 2010)*365 + 2,  # add 2nd leap day from Feb 2016
                        year == 2020 ~ (year - 2010)*365 + 3,  # add 3rd leap day from Feb 2020
                        TRUE ~ NA_real_)]
  stopifnot(nrow(pc10[is.na(t)]) == 0)  # make sure there is a valid t for all rows
  stopifnot(nrow(pc10[t < 0]) == 0)  # make sure t is never negative
  stopifnot(nrow(pc10[t > 3653]) == 0)  # make sure t is never more than 10 years plus 3 leap days
  
  # merge datasets and conditionally calculate intercensal estimates
  temp <- merge.data.table(pc10, pc20_2020, all = T)
  temp[, pop_ic := ifelse(pop == 0 | q_3653 %in% c(0, 1) | p_3653 %in% c(0, 1) | q_3653 < p_3653/2,  # if one of these conditions is true,
                          calc_linear_interp(t, p_3653, p_0),  # use the linear interpolation method
                          calc_das_gupta(t, pop, p_3653, q_3653))]  # otherwise, use the default (Das Gupta) method
  stopifnot(nrow(temp[is.na(pop_ic)]) == 0)
  stopifnot(nrow(temp[pop_ic < 0]) == 0)
  
  # filter to columns of interest and return
  dt <- copy(temp[, list(fips, year, sex, age_start, race, pop = pop_ic)])
  stopifnot(nrow(dt) == nrow(pc10))  # make sure we have the same number of observations as we started with
  return(dt)
}
