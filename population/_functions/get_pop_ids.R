####################################################################################################
## Description: Connect to shared.covariate database, pull table of USHD populations and covariates,
##              and filter to populations. Source this script to load the table and use as a reference
##              for short names (e.g., "pop_by_age_sex") and IDs.
####################################################################################################

library(data.table)
library(DBI)
library(odbc)
library(ini)
library(RMySQL)

## Load shared database (project_covariate and covariate tables) to get USHD covariate IDs and their corresponding short names
# see script description above for how to add shared DB credentials to your ODBC, if not already done
db_query <- function(DSN, query_string){
  odbc <- read.ini("FILEPATH")[[DSN]]
  con <- dbConnect(RMySQL::MySQL(),
                   user = odbc$USER,
                   password = odbc$PASSWORD,
                   dbname = odbc$DATABASE,
                   host = odbc$SERVER)
  query_results <- dbGetQuery(con, query_string)
  dbDisconnect(con)
  return(query_results)
}
query <- 'SELECT covariate_id FROM DATABASETABLE WHERE project_id = 2'  # project ID for USHD is 2
pop_ids <- as.vector(db_query(DSN = "DATABASE", query_string = query)$covariate_id)  # get list of unique USHD covariate IDs
query <- paste('SELECT covariate_id, covariate_name_short',
               'FROM DATABASETABLE',
               'WHERE covariate_id IN',
               paste0("(", paste(as.character(pop_ids), collapse = ", "), ")"))
pop_ids <- as.data.table(db_query(DSN = "DATABASE", query_string = query))  # get USHD covariate IDs and their short names
pop_ids <- pop_ids[grepl("pop_by", covariate_name_short)]  # filter to populations only

# also grab IDs for different population group sets
pop_group_sets <- query_database('DATABASE','SELECT * FROM DATABASETABLE')

rm(query, db_query)
