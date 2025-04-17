####################################################################################################
## Description: Load basic settings (should be the same for all covariates) for US counties. This
##              intended to be sourced at the top of all prep scripts.
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

## Make sure working directory is covariates/
stopifnot(grepl("covariates$", getwd()))
sourceDirectory(paste0(getwd(), "/_functions/"), modifiedOnly = F)

## Set directories
cov_dir <- "FILEPATH"
pop_dir <- "FILEPATH"
observed_year_dir <- "FILEPATH"  # for keeping record of which years are interpolated
if (!dir.exists(observed_year_dir)) dir.create(observed_year_dir, showWarnings = F)
rversion <- gsub("\\.", "", paste0(R.version[["major"]], R.version[["minor"]]))  # get R version and set rlibs dir accordingly
rlibs_loc <- "FILEPATH"
if (!dir.exists(rlibs_loc)) {  # create folder for R version if doesn't already exist and auto-install INLA
  dir.create(rlibs_loc)
  install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE,
                   lib = rlibs_loc)
}  

## Set years
interp_years <- 2000:2022

## Load merged counties
loc <- fread("FILEPATH")
setnames(loc, "cnty", "fips")

## Load shape files
state_map <- readRDS("FILEPATH")
mcnty_map <- readRDS("FILEPATH")
