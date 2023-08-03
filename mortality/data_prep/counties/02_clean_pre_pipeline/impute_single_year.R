#################################################################
# Description: Apply imputation proportions to cleaned dataset
#              Single year launched by 01_impute_data.R
#################################################################

rm(list = ls())

args <- commandArgs(trailingOnly = TRUE)
year <- args[1]

library(data.table)

ushd_dir <- 'FILEPATH'
data_out_dir <- 'FILEPATH'
data_in_dir <- 'FILEPATH'
props_dir <- 'FILEPATH'
source(paste0(functions_dir, 'prep_functions.R'))

adjust_unknown_ethn <- function(dt){
  deaths_before <- sum(dt$deaths)
  origi_cols <- names(copy(dt))
  
  props <- get_proportion('mortality_ethn_props')
  # grab and add merged counties metadata
  mcntys <- fread('FILEPATH')[,c('cnty','mcnty')]
  dt <- merge(dt, unique(mcntys[,.(cnty, mcnty)]),
              by.x="full_fips_res_numeric",
              by.y = 'cnty',
              allow.cartesian = T,
              all.x = T)
  
  dt[,race_temp := ifelse(race %in% c(68,78,5,58,48,6,38,18,28,8,7,0),
                          4, race)]
  dt[,hisp_missing := ifelse(hisp_recode == 9, "missing", "not_missing")]
  
  dt_adjust <- dt[hisp_missing == 'missing' & state_res_numeric <= 56]
  dt <- dt[hisp_missing != 'missing' | state_res_numeric > 56]
  
  dt_adjust[,state := ifelse(is.na(mcnty), state_res_numeric, NA)]
  dt_adjust <- merge(dt_adjust, props,
                     by.x = c("state", "mcnty", "year", "sex", "race_temp"),
                     by.y = c("state", "mcnty", "year", "sex", "race"),
                     all.x = T)
  
  dt_adjust_hisp <- copy(dt_adjust)
  dt_adjust_nonhisp <- copy(dt_adjust)
  
  dt_adjust_hisp <- dt_adjust_hisp[,deaths := deaths * ratio]
  #set "new" hispanic races to hispanic
  dt_adjust_hisp[,c('race_label_1997', 'race_label_1977') := 'Hispanic']
  
  dt_adjust_nonhisp <- dt_adjust_nonhisp[,deaths := deaths * (1 - ratio)]
  
  dt_adjust <- rbind(dt_adjust_hisp[,origi_cols, with = FALSE], 
                     dt_adjust_nonhisp[,origi_cols, with = FALSE])
  dt <- rbind(dt[,origi_cols, with = FALSE], 
              dt_adjust)
  
  
  dt <- dt[, list(deaths=sum(deaths)), by=setdiff(names(dt), "deaths")]
  stopifnot(round(deaths_before - sum(dt$deaths)) == 0)
  
  return(dt)
}

file_name <- list.files(data_in_dir, as.character(year))
dt <- fread(paste0(data_in_dir, file_name))

dt <- adjust_unknown_ethn(dt)
fwrite(dt, 'FILEPATH', row.names = FALSE)