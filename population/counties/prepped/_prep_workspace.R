####################################################################################################
## Description: Load basic settings (should be the same for all populations) for US counties. This
##              is intended to be sourced at the top of all prep scripts.
##
####################################################################################################

library(data.table)
library(ggplot2)
library(sp)
library(mgsub)
library(R.utils)
library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                      R.version$major,
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
ushd_client$save_covariate_population  # running this line ensures successful database connection and prevents interference from other packages

## Load functions
if (!(grepl("population$", getwd()))) setwd("FILEPATH")
sourceDirectory("_functions/", modifiedOnly = F)

## Set directories
root <- ifelse(Sys.info()[1] == "Windows", "FILEPATH", "FILEPATH")
pop_dir <- paste0(root, "FILEPATH")
observed_year_dir <- paste0(pop_dir, "FILEPATH")  # for keeping record of which years are interpolated
if (!dir.exists(observed_year_dir)) dir.create(observed_year_dir, showWarnings = F)
rlibs_loc <- paste0(root, "FILEPATH")

## Set current range of data years
interp_years <- 1980:2020

## Load merged counties
loc <- fread(paste0(root, "FILEPATH"))
setnames(loc, "cnty", "fips")