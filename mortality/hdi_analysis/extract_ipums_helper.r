#####################################################################################
## Extract the IPUMS data and format for HDI analysis
#####################################################################################

empir_funcs <- list.files("functions/")
for(func in empir_funcs) {
  source(paste0("functions/", func))
}

library(readstata13)
library(haven)
library(survey)
library(ggplot2)
library(RColorBrewer)
library(stats)
library(ggpubr)
library(sf)
library(gridExtra)
library(data.table)
library(mgsub)
library(dplyr)

library(ltcore, lib.loc = "[LIBRARY_PATH]")

user <- Sys.getenv("USER")

if (interactive()) {
  proj_dir <- "[PROJECT_DIRECTORY]"
  model_dir <- "[MODEL_DIRECTORY]"
  outdir_name <- "[OUTPUT_DIRECTORY]"
  max_age <- 110
  rep_id <- "repwtp1"
  date_time_stamp <- "[DATE_TIME_STAMP]"
} else {
  args <- commandArgs(trailingOnly = TRUE)
  proj_dir <- args[1]
  model_dir <- args[2]
  outdir_name <- args[3]
  max_age <- args[4]
  rep_id <- args[5]
  date_time_stamp <- args[6]
}

# Define the folder
in_dir <- file.path(proj_dir, "acs_microdata")
plot_dir <- file.path(proj_dir,"plots")
out_dir <- file.path(proj_dir, outdir_name, date_time_stamp)

lt_dir <- file.path(proj_dir, "full_lifetables", basename(dirname(model_dir)), "kannisto_hc_method")

# Read in regional price parity adjustments
rpp <- readRDS(file.path(proj_dir,"income_adjustments.rds"))

locations <- fread("[MCNTY_LOCATION_INFORMATION]")

# race info
races <- c(2,4,5,6,7)
race_labels <- c("Latino", "Black", "White", "AIAN", "API")

# define age groups within which we want to calculate HDI
age_group_labels <- c("25-44", "45-64", "65-84", "85+")

years <- 2008:2021

# additional adjustment factor for converting 1999 dollars to 2021 dollars, from this table: https://cps.ipums.org/cps/cpi99.shtml
inc_1999_to_2021 <- 1.626

# Load and prep ACS data ------------------------------------------------------------
base_cols <- c("year", "statefip", "puma", "age", "sex", "educd", "hhincome", "cpi99", "numprec","rachsing",
               "sample", "serial", "pernum")
# note: https://usa.ipums.org/usa-action/variables/SERIAL#description_section says that sample, serial, and pernum uniquely describe each entry
inc_vars <- c("inctot")
size_vars <- c("gq") 
race_vars <- c(paste0("pred",c("ai","api","blk","wht","hisp")), "race")

cols <- c(base_cols,
          inc_vars,
          size_vars,
          race_vars,
          rep_id) # and include the replicate weight (this could also be perwt)

dt_full <- as.data.table(read_dta(file.path(in_dir, "usa_00003.dta"), col_select = all_of(cols)))
dt_full <- dt_full[year %in% years]
gc()

if(rep_id != "perwt") {
  message(paste0("setting ",rep_id," to perwt"))
  setnames(dt_full, rep_id, "perwt")
}

# calculate the total population, so we can see if this is the same between files
total_pop <- dt_full[,list(perwt = sum(perwt)), by='year']
saveRDS(total_pop, paste0(out_dir,"/total_population_", rep_id, ".rds"))

extract_data <- function(yr) {
  cat(paste0(yr, "\n")); flush.console()

  dt <- copy(dt_full[year == yr])
  dt[, puma := 100000*as.integer(as.character(statefip)) + as.integer(as.character(puma))]

  # Merge on price parity information
  dt <- merge(dt, rpp[year == yr], by=c("puma","year"), all=T)
  stopifnot(nrow(dt[is.na(price_adj)]) == 0)
  stopifnot(nrow(dt[is.na(sex)]) == 0)

  dt[, agegp := cut(age, breaks = c(seq(25,110,5)), right = FALSE)]

  # drop children and adolescents
  dt <- dt[!is.na(agegp)]
  dt[,age_specific := age]
  dt[,age := NULL]

  dt[,age := tstrsplit(agegp,",")[[1]]]
  dt$age <- gsub("\\[","",dt$age)
  dt[,age := as.numeric(age)]
  dt[,agegp := NULL]

  # Melt to get the bridging proportions
  setnames(dt,"race","single_race")
  total_pop <- sum(dt$perwt)
  dt <- melt.data.table(dt, measure.vars=c("predai", "predapi", "predblk","predwht","predhisp"))

  dt[,perwt := perwt*value]
  stopifnot(abs(total_pop == sum(dt$perwt)) < 0.000001) # make sure the total population didn't change
  rm(total_pop)

  dt <- dt[value != 0]
  dt[,race := ifelse(variable == "predwht", "White",
                     ifelse(variable == "predblk", "Black",
                            ifelse(variable == "predapi", "API",
                                   ifelse(variable == "predhisp", "Latino",
                                          ifelse(variable == "predai", "AIAN", NA)))))]
  stopifnot(nrow(dt[is.na(race)]) == 0)

  dt[,rachsing := factor(rachsing, c(1:5), c("White","Black","AIAN","API","Latino"))]
  stopifnot(nrow(dt[rachsing != race & single_race < 7]) == 0) # for all non-hispanic single race rows, rachsing and race should be the same

  if(rep_id == "perwt") {
    dt[, sex := as.numeric(sex)]
    dt[,numprec := as.numeric(as.character(numprec))]

    # Map on education values 
    educd_to_eduyrs <- data.table(educd = c(1,2, 11,12, 14:17, 22, 23, 25, 26, 30, 40, 50, 61, 63, 64, 65, 71, 81, 101, 114, 115, 116),
                                  eduyrs = c(0, 0, 0.5, 1, 2:12, 12.9, 13, 13, 13.5, 15, 15, 17, 19, 21, 21))
    dt <- merge(dt, educd_to_eduyrs, by="educd", all.x=T)
    stopifnot(nrow(dt[is.na(eduyrs)]) == 0)

    ### Now adjust household income using the inflation factor to get to 1999 dollars, and then 2021 dollars
    dt[hhincome == 9999999, hhincome := NA]
    stopifnot(nrow(dt[is.na(hhincome) & !(gq %in% c(3,4))]) == 0)
    stopifnot(nrow(dt[!is.na(hhincome) & gq %in% c(3,4)]) == 0)
    stopifnot(nrow(dt[gq %in% c(3,4) & numprec != 1]) == 0) # make sure there is a household size of 1 for group quarters

    dt[inctot %in% c(9999999, 9999998), inctot := NA]
    dt[is.na(hhincome) & gq %in% c(3,4), hhincome := inctot] # assign personal income to HH income for group quarters
    stopifnot(nrow(dt[is.na(hhincome)]) == 0)

    # we also divide by RPP
    dt[,hhincome_adjusted := hhincome*cpi99*inc_1999_to_2021/price_adj]
    stopifnot(nrow(dt[numprec == 0]) == 0)
    dt[,hh_consumption := hhincome_adjusted/sqrt(numprec)]

    # remove unnecessary columns before merging on the PUMA-level life expectancy results
    dt[,c("cpi99","numprec","hhincome","educd","hhincome_adjusted","price_adj","inctot",
          "rachsing","single_race","variable","value","gq") := NULL]
    dt[,year := as.numeric(as.character(year))]

    ## Get life expectancy
    lt_files <- c()
    for(rr in races) {
      for(sx in c(1,2)) {
        lt_files <- c(lt_files, file.path(lt_dir, paste0("full_lt_puma_",yr,"_sex_",sx,"_race_",rr,".rds")))
      }
    }

    extended_lt <- rbindlist(lapply(lt_files, readRDS), use.names=T)[level == "puma"][,.(puma = area, year, sex, race, age, qx)]

    # now read in the abridged life table in order to get ax for the oldest and youngest ages
    lt_files_abridged <- c()
    for(rr in races) {
      for(sx in c(1,2)) {
        lt_files_abridged <- c(lt_files_abridged, file.path(lt_dir, paste0("abridged_lt_puma_",yr,"_sex_",sx,"_race_",rr,".rds")))
      }
    }

    abridged_lt <- rbindlist(lapply(lt_files_abridged, readRDS), use.names=T)[level == "puma"][,.(puma = area, year, sex, race, age, ax, mx, ex)]

    # merge on ax values and mx for terminal age groups
    extended_lt <- merge(extended_lt, abridged_lt[,.(puma,year,sex,race,age,ax,mx)],
                         by=c("puma","year","sex","race","age"), all=T)
    stopifnot(nrow(extended_lt[is.na(qx)]) == 0)
    extended_lt[!(age %in% c(min(age),max(age))), ax := 0.5]
    stopifnot(nrow(extended_lt[is.na(ax)]) == 0)
    extended_lt[age < max(age), mx := NA] # we don't want to use mx except for in the terminal age group

    extended_lt[,age_length := ifelse(age < max(age), 1, NA)]
    setkeyv(extended_lt, c("puma","year","sex","race","age"))

    qx_to_lx(extended_lt, terminal_age = max_age)
    lx_to_dx(extended_lt, terminal_age = max_age)
    gen_nLx(extended_lt, terminal_age = max_age)
    gen_Tx(extended_lt, id_vars = c("puma","year","sex","race","age"))

    extended_lt[,ex := Tx/lx]

    # compare ex in terminal age group to the abridged life table values
    message("Testing that the terminal life expectancy is the same post-interpolation")
    new_terminal_ex <- extended_lt[age == max(age), .(puma, year, sex, race, age, ex_new = ex)]
    original_terminal_ex <- abridged_lt[age == max(age), .(puma, year, sex, race, age, ex_old = ex)]
    terminal_ex_test <- merge(new_terminal_ex, original_terminal_ex, by=c("puma","year","sex","race","age"), all=T)
    stopifnot(nrow(terminal_ex_test[is.na(ex_new)]) == 0)
    stopifnot(nrow(terminal_ex_test[is.na(ex_old)]) == 0)
    stopifnot(max(terminal_ex_test[,abs(ex_new - ex_old)]) < 0.00001)

    rm(new_terminal_ex, original_terminal_ex, terminal_ex_test)
    gc()

    extended_lt[,ex_mean := ex + age] # adds life expectancy to specific age now
    extended_lt[,race := factor(race, races, race_labels)]

    dt <- merge(dt, extended_lt[,.(puma, year, sex, race, age_specific = age, ex_mean)],
                by=c("puma","year","sex","race","age_specific"), all.x=T)

    stopifnot(nrow(dt[is.na(ex_mean)]) == 0)
    stopifnot(nrow(dt[age_specific > ex_mean]) == 0)

  } else {
    dt <- dt[,.(sample, serial, pernum, race, perwt)]
    setnames(dt, "perwt", rep_id)
  }

  return(dt)
}

extractions <- lapply(years, extract_data)
gc()
data <- rbindlist(extractions, use.names = T) 

# Save output -----------------------------------------------------------------------
saveRDS(data, paste0(out_dir,"/prepped_for_hdi_", rep_id, ".rds"))
