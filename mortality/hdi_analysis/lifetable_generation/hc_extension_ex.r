#####################################################################################
## Description: calculate full life tables
##
## Reads in arguments from an array job
#####################################################################################

# source the life table specific functions
library(ltcore, lib.loc = "[PATH_TO_LIBRARY]")
library(mortcore, lib.loc = "[PATH_TO_LIBRARY]")
library(mortdb, lib.loc = "[PATH_TO_LIBRARY]")

# import process specific functions
code_dir <- "[PATH_TO_LIBRARY]"
empir_lt_func_dir <- paste0(code_dir, "[PATH_TO_LIBRARY]")
empir_funcs <- list.files(empir_lt_func_dir)
for(func in empir_funcs) {
  source(paste0(empir_lt_func_dir, "/", func))
}

hdi_proj_dir <- "[PROJECT_DIRECTORY]"

kannisto_model <- function(C, b, x, a) {
  
  m_x <- (C*exp(b*(x-a+1)))/(1 + C*exp(b*(x-a+1)))

  return(m_x)
}

library(data.table)
library(ggplot2)
library(assertable)
library(haven)
library(doParallel)

####### Get submission arguments
# Array job arguments
if(interactive()) {
  task_id <- 1
} else {
  task_id <- ifelse(is.na(as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))), 1, as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID")))
}

ids <- fread(file.path(hdi_proj_dir, "full_lt_calc_list.csv"))
geo <- ids[task_id, geography] # geographic granularity
rr <- ids[task_id, race]
sx <- ids[task_id, sex]
yr <- ids[task_id, year]
model_dir <- ids[task_id, ushd_dir]
by_puma <- as.logical(ids[task_id, by_puma])

# We cannot crosswalk to the PUMA level unless we start at the mcnty level
if(geo != "mcnty" & by_puma) {
  by_puma <- F
  message(paste0("Geography is ", geo,", but by_puma is TRUE. Converting by_puma to FALSE."))
}

older_age_groups <- seq(85, 110, 5)

## Mcnty to PUMA crosswalk information
# this is the proportion of each PUMA that is composed of each mcnty (based on population)
# to get mortality at the PUMA level, we want to calculate a population-weighted average of the mcnty-level mortality rates that make up that PUMA
# thus, if mcnty 1 makes up 10% of the PUMA population, and mcnty 2 makes up 90% of the PUMA population, then the aggregated mortality should be
# mcnty1*0.1 + mcnty2*0.9
county_puma_xw <- readRDS(file.path(hdi_proj_dir,"count_to_puma_xw.rds"))
county_puma_xw[,pop := NULL]

# The following is then used to rescale the populations so that they reflect the age distribution of our population
puma_pop_in_each_county <- readRDS(file.path(hdi_proj_dir,"puma_pop_within_county.rds")) # proportion of puma-level populations within each county
puma_pop_in_each_county[,pop := NULL]
setnames(puma_pop_in_each_county, "mcnty","area")

lt_dir <- file.path(hdi_proj_dir,"full_lifetables", basename(dirname(model_dir)), "kannisto_hc_method")
dir.create(lt_dir, recursive = T)

# Read in our life expectancy estimates
races <- c(2,4,5,6,7)
race_labels <- c("Hispanic","NH Black", "NH White", "NH AIAN","NH API")
by_vars <- c("level","area","year","sex","race")

#### Read in raked life table estimates
filename <- file.path(model_dir,paste0("lt_est_",geo,"_",yr,"_",sx,"_",rr,"_1_raked.rds"))
ex <- readRDS(filename)

# Simplify columns
ex <- ex[,list(mx = mx_mean,
               ax = ax_mean,
               qx = qx_mean,
               ex = ex_mean), by='level,area,year,sex,race,age']

## Crosswalk to the PUMA level, if specified
if(by_puma) {
  # Get the population used for modeling so that we can rescale the crosswalking weights to better reflect the population distribution of our population data
  # get population and aggregate to national level in order to weight
  pop_file <- as.data.table(read.csv(file.path(model_dir, "settings.csv"), header=F))
  pop_file <- as.character(pop_file[V1 == "pop_file", V2])
  pop_full <- readRDS(pop_file)
  pop_full <- pop_full[year == yr]
  setnames(pop_full, "mcnty", "area")

  # Merge on population
  ex <- merge(ex, pop_full, by=c("area","year","sex","race","age"), all.x=T)
  stopifnot(nrow(ex[is.na(pop)]) == 0)

  pop_by_race_year <- ex[,list(pop = sum(pop)), by='year,race']
  setkeyv(pop_by_race_year, c("year","race", "pop"))

  ex[,version := ifelse(year < 2012, 2000, ifelse(year >= 2012 & year <= 2021, 2012, 2020))]

  ## merge on county-PUMA crosswalk and calculate PUMA-level values
  ex <- merge(ex, county_puma_xw[version %in% unique(ex$version)], by.x=c("area","version"), 
              by.y=c("mcnty","version"), all=T, allow.cartesian=T)
  stopifnot(nrow(ex[is.na(ex)]) == 0)
  stopifnot(nrow(ex[is.na(prop_by_puma)]) == 0)

  # Now merge on the proportions of county-level population within each PUMA
  ex <- merge(ex, puma_pop_in_each_county[version %in% unique(ex$version)], by=c("area","puma","version"), all.x=T)
  stopifnot(nrow(ex[is.na(prop_within_county)]) == 0) 

  ## Get the implied population at each mcnty-puma intersection, we need to multiply prop_within_county by population
  ex[,pop2 := pop*prop_within_county] # this gives us the implied population between counties and PUMAs, using our population data
  ex[, prop_by_puma_old := prop_by_puma]
  ex[, prop_by_puma := pop2 / sum(pop2), by=c("puma","version","year","sex","race","age")] 
  # there will be some places with 0 population, and thus these weights will be undefined. For these, the weight should be 0 since the population is 0
  ex[is.na(prop_by_puma), prop_by_puma := 0]
  stopifnot(nrow(ex[is.na(prop_by_puma)]) == 0)
  stopifnot(nrow(ex[prop_by_puma == "Inf"]) == 0)

  # But, in some cases we get a proportion of 0 within a year/sex/race/puma/age
  # in this case, we would want to use some sort of back up weight
  ex[,all_zero := as.integer(max(prop_by_puma) == 0), by=c("year","sex","race","puma","age")]
  # backup weights
  ex[,pop2_across_age := sum(pop2), by=c("area","puma","version","year","sex","race")]
  # now get the denominator: puma-level population summed across ages
  ex[,pop2_puma_across_age := sum(pop2), by=c("puma","version","year","sex","race")]
  ex[all_zero == 1, prop_by_puma := pop2_across_age/pop2_puma_across_age]

  # now recalculate this
  ex[, all_zero := NULL]
  ex[,all_zero := as.integer(max(prop_by_puma) == 0), by=c("year","sex","race","puma","age")]

  stopifnot(nrow(ex[all_zero == 1]) == 0)

  # tabulate again to make sure nothing got dropped
  pop_by_race_year_xw <- ex[,list(pop = sum(pop2)), by='year,race']
  setkeyv(pop_by_race_year_xw, c("year","race", "pop"))
  stopifnot(all.equal(pop_by_race_year, pop_by_race_year_xw)) # nothing was dropped, yay!
  rm(pop_by_race_year, pop_by_race_year_xw); gc()
  
  # Aggregate mx and ax 
  # Also aggregate ex so that we can get a starting value of ex to compare to later
  ex <- ex[, list(mx = sum(mx * prop_by_puma),
                  ax = sum(ax * (mx * prop_by_puma)/sum(mx * prop_by_puma)),
                  ex = sum(ex * prop_by_puma),
                  level = "puma"),
           by='puma,year,sex,race,age']

  # make sure there are no NAs
  stopifnot(nrow(ex[is.na(ex)]) == 0)
  # make sure none are 0
  stopifnot(nrow(ex[ex == 0]) == 0)
  setnames(ex, "puma", "area")

  # make sure there are no duplicates
  ex[,count := .N, by=c("level","area","year","sex","race","age")]
  stopifnot(nrow(ex[count > 1]) == 0)
  ex[,count := NULL]

  setkeyv(ex, c(by_vars, "age"))
  ex[,age_length := ifelse(age == 0, 1, ifelse(age == 1, 4, ifelse(age < 85, 5, NA)))]
  ex[, qx := mx_ax_to_qx(m = mx, a = ax, t = age_length)]
  ex[age == 85, qx := 1] # fill in terminal qx
  stopifnot(nrow(ex[is.na(qx)]) == 0)

  ex[,c("age_length") := NULL]

  geo <- "puma"
}

# Life table calculations
ex_original <- copy(ex)
setkeyv(ex_original, c(by_vars, "age"))
qx_to_lx(ex_original, assert_na = T, terminal_age = 85)
ex_original[,version := "1 - original"]

terminal <- ex[age == 85, .(terminal_age_start = age, ex_term = ex, level, area, year, sex, race)] # get the terminal mortality value
mx_80 <- ex[age == 80, .(mx_penultimate = mx, level, area, year, sex, race)]

##### We want to fit the Kannisto model so that the results best match the terminal age (85+) life expectancy
# C = mortality in age 80-84
# a: 85
# x: each older age: 85, 90, 85, 100, 105, 110

# Merge on terminal life expectancy and mortality in the penultimate age group
mx_80 <- merge(mx_80, terminal, by=c("level","area","year","sex","race"), all=T)
stopifnot(nrow(mx_80[is.na(ex_term)]) == 0)
stopifnot(nrow(mx_80[is.na(mx_penultimate)]) == 0)

ex <- merge(ex[age <= 80], mx_80, by=c("area","year","sex","race","level"), all=T)
stopifnot(nrow(ex[is.na(ex_term)]) == 0)
stopifnot(nrow(ex[is.na(age)]) == 0)

# All we want left is mx and ax
ex[,c("ex","qx") := NA]

setkeyv(ex, c("level","area","year","sex","race","age"))

# For each row, we want to optimize to find the best value of b that translates our mortality rates into the closest e85 value
lifetable_fxn <- function(dt) {
  setkeyv(dt, c("level","area","year","sex","race","age"))

  # now calculate life expectancy
  # assume age-specific death rate is constant within each age group 
  dt[,age_length := ifelse(age == 0, 1, ifelse(age == 1, 4, ifelse(age < max(age), 5, NA)))]
  dt[is.na(ax), ax := ltcore::mx_to_ax(m = mx, t = age_length)]

  # get qx
  dt[is.na(qx), qx := ifelse(age == max(age), 1, mx_ax_to_qx(m = mx, a = ax, t = age_length))]
  stopifnot(nrow(dt[qx == 1 & age < max(age)]) == 0) # make sure only the terminal age group has qx = 1

  # get lx
  qx_to_lx(dt, terminal_age = 110)
  lx_to_dx(dt, terminal_age = 110)
  gen_nLx(dt, terminal_age = 110)
  gen_Tx(dt, id_vars = c("level","area","year","sex","race","age"))

  dt[,ex := Tx/lx] # NOTE: this recalculates ex in all ages
  dt[age == max(age), ax := ex]

  return(dt)
}

# Calculates life expectancy for a given b parameter, and saves the data table of differences in e85 (compared to the original)
calc_e85 <- function(dt, return_diff = T, opt = T, b = NULL) {

  # extrapolate mortality for every age
  for(ag in older_age_groups) {
    tmp <- copy(dt[age == 80][,c("mx","ax","ex",'qx') := NULL])[, mx := kannisto_model(C = (mx_penultimate)/(1-mx_penultimate), 
                                                                                       a = terminal_age_start, x = ag, b = b)]
    tmp[,age := ag]
    dt <- rbind(dt, tmp, fill=T)
  }
  dt <- lifetable_fxn(dt)

  # now test how similar life expectancy at 85 is to the original values
  if(return_diff) {
    return_dt <- dt[age == 85, .(change_85 = ex - ex_term, area, year, sex, race, level, age, b = b)]

    if(opt) {
      return_dt <- abs(return_dt$change_85)
    }

  } else {
    return_dt <- dt[,.(area, year, sex, race, level, age, mx, ax, ex, qx, lx, dx)]
  }

  return(return_dt)
}

# optimize
# for each area, year, sex, race, we want to apply the Kannisto method and then compare to original e85
registerDoParallel(cl <- makeCluster(4))
differences <- foreach(ar = unique(ex$area), .packages = c("data.table")) %dopar% {
  library(ltcore, lib.loc = "[LIBRARY_LOCATION]")
  opt <- optimize(f = function(b) calc_e85(ex[area == ar], return_diff = T, opt = T, b),
                  interval = c(0,3))
  data.table(b = opt$minimum,
             abs_diff = opt$objective,
             area = ar)
}
stopCluster(cl)
differences <- rbindlist(differences)

# Merge ex on so that we get the rest of the demographic information
differences <- merge(differences, ex[age == 80,.(area, year, sex, race, level)], by=c("area"))
stopifnot(nrow(differences) == length(unique(ex$area)))

## save this out
dir.create(file.path(lt_dir,"ex85_differences"), recursive = T)
saveRDS(differences, file.path(lt_dir,"ex85_differences",paste0("diffs_",geo,"_",yr,"_sex_",sx,"_race_",rr,".rds")))


### Now, for each of the best values of b, let's calculate the rest of the life table
ex <- merge(ex, differences[,.(area, year, sex, race, level, b)], by=c("area","year","sex","race","level"), all=T)
stopifnot(nrow(ex[is.na(ex_term)]) == 0)
stopifnot(nrow(ex[is.na(b)]) == 0)

ex <- calc_e85(dt = ex, return_diff = F, opt = F)

saveRDS(ex, file.path(lt_dir, paste0("abridged_lt_",geo,"_",yr,"_sex_",sx,"_race_",rr,".rds")))

# make a copy for comparison
ex_abridged <- copy(ex)[,version := "2 - post extension and scaling"]


# now calculate qx assuming mortality is constant within each age range
age_map <- data.table(age_specific = seq(0,110,1))
age_map[,age := floor(age_specific/5)*5]
age_map[age_specific %in% c(1:4), age := 1]

ex_full <- merge(ex[,.(area, year, sex, race, level, age, qx)], age_map, by="age", all=T, allow.cartesian = T)
stopifnot(nrow(ex_full[is.na(age_specific)]) == 0)
stopifnot(nrow(ex_full[is.na(qx)]) == 0)
ex_full[,age_length := ifelse(age == 0, 1, ifelse(age == 1, 4, ifelse(age < max(age), 5, NA)))]

ex_full[,age := NULL]
setnames(ex_full, "age_specific", "age")
ex_full[!(age %in% c(min(age), max(age))), qx := 1 - (1-qx)^(1/age_length)]
ex_full[,age_length := NULL]


saveRDS(ex_full, file.path(lt_dir, paste0("full_lt_",geo,"_",yr,"_sex_",sx,"_race_",rr,".rds")))

# make a comparison and save
ex_compare <- rbind(ex_original,
                    ex_abridged,
                    copy(ex_full)[,version := "3 - post-qx splitting"],
                    fill=T)

dir.create(file.path(lt_dir,"comparison_data"), recursive = T)
saveRDS(ex_compare, file.path(lt_dir,"comparison_data", paste0("comparison_lt_",geo,"_",yr,"_sex_",sx,"_race_",rr,".rds")))
