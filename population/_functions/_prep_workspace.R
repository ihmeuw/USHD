####################################################################################################
## Description: Load basic settings (should be the same for all populations) for US counties. This
##              intended to be sourced at the top of all prep scripts.
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
pop_dir <- "FILEPATH"
observed_year_dir <- "FILEPATH"
if (!dir.exists(observed_year_dir)) dir.create(observed_year_dir, showWarnings = F)
rlibs_loc <- "FILEPATH"

## Set current range of data years
interp_years <- 1980:2021

## Load merged counties
loc <- fread("FILEPATH")
setnames(loc, "cnty", "fips")
