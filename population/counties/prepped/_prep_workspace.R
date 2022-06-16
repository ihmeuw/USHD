####################################################################################################
## Description: Load basic settings (should be the same for all covariates) for US counties. This
##              intended to be sourced at the top of all prep scripts.
####################################################################################################

library(R.utils)
library(data.table)
library(ggplot2)
library(plyr)
library(sp)
library(caTools)
library(mgsub)
library(DBI)
library(odbc)
library(ini)
library(RMySQL)

rm(list = ls())

## Load functions
stopifnot(grepl("population$", getwd()))
sourceDirectory("_functions/", modifiedOnly = F)

## Set directories
root <- ifelse(Sys.info()[1] == "Windows", "[FILEPATH]", "[FILEPATH]")
pop_dir <- paste0(root, "[FILEPATH]")
observed_year_dir <- paste0(pop_dir, "[FILEPATH]")  # for keeping record of which years are interpolated
if (!dir.exists(observed_year_dir)) dir.create(observed_year_dir, showWarnings = F)
rlibs_loc <- paste0(root, "[FILEPATH]")

## Set years
interp_years <- 1980:2019

db_query <- function(DSN, query_string){
  odbc <- read.ini(paste0("[FILEPATH]", Sys.info()["user"], "[FILEPATH]"))[[DSN]]
  con <- dbConnect("[ARGS]")
  query_results <- dbGetQuery(con, query_string)
  dbDisconnect(con)
  return(query_results)
}
query <- 'SELECT covariate_id FROM shared.project_covariate WHERE project_id = 2'  # project ID for USHD is 2
cov_ids <- as.vector(db_query(DSN = "engine", query_string = query)$covariate_id)  # get list of unique USHD covariate IDs
query <- paste('SELECT covariate_id, covariate_name_short',
               'FROM shared.covariate',
               'WHERE covariate_id IN',
               paste0("(", paste(as.character(cov_ids), collapse = ", "), ")"))
cov_ids <- as.data.table(db_query(DSN = "engine", query_string = query))  # get USHD covariate IDs and their short names

## Load merged counties
loc <- fread(paste0(root, "[FILEPATH]"))
setnames(loc, "cnty", "fips")

## Load shape files
state_map <- readRDS(paste0(root,"[FILEPATH]"))
mcnty_map <- readRDS(paste0(root,"[FILEPATH]"))
