####################################################################################################
## Description: Pull non-best versions from get_covariate_estimates()
## Outputs:     A function `get_covariates()` that can be used to get covariate data. Requires...
##                  - covariate_id
##                  - model_version_id
##                  - location_ids
##                  - year_ids
##              A function `format_covariates()` that standardizes gbd output to us_counties standards. Requires...
##                  - data (output from get_covariates)
##                  - value_name (optional name for value, like "gbd_fr") used to label value columns
##                  - collapse_neonatal (default T) collapses neonatal age groups to 0-1 age group
####################################################################################################

library(data.table)
library(RMySQL)

dbQuery <- function(dbconn, query_string) {
  print(dbconn)
  conn <- dbConnect(RMySQL::MySQL(),
                    host = "HOST",
                    username = "USERNAME",
                    password = "PASSWORD",
                    db = dbconn)
  print("connection established")
  df <- dbGetQuery(conn, query_string)
  print("query obtained")
  dbDisconnect(conn)
  return(data.table(df))
}

# Adapted from shared function get_covariate_estimates()
get_covariates <- function(covariate_id,
                           model_version_id,
                           location_ids=c(102, 523:573),
                           year_ids=1980:2015){
  data <- dbQuery("DB_NAME", sprintf("QUERY",
    covariate_id, model_version_id, paste(location_ids, collapse=","), paste(year_ids, collapse=",")))
  return(data)
}


format_covariates <- function(data, value_name=NULL, collapse_neonatal=T) {
  if (is.null(value_name)) value_name <- unique(data$covariate_name_short)

  source("FILEPATH/get_ids.R")

  print("formatting age groups")
  age_metadata <- get_ids("age_group")
  age_metadata <- age_metadata[age_group_id %in% unique(data$age_group_id)]
  age_metadata[, age:= as.character(suppressWarnings(as.numeric(substr(age_metadata[,age_group_name], 0,2))))]
  age_metadata[age_group_name == "Early Neonatal", age:="EN"]
  age_metadata[age_group_name == "Late Neonatal", age:="LN"]
  age_metadata[age_group_name == "Post Neonatal", age:="PN"]
  data <- merge(data, age_metadata, by=c("age_group_id", "age_group_name"), all.x=T)

  print("formatting locations")
  loc <- fread("FILEPATH")
  loc <- loc[location_parent_id == 102, list(location_id, area=fips, level="state")]
  loc <- rbind(loc, data.table(location_id=102, area=0, level="natl"))
  data <- merge(data, loc, by="location_id", all.x=T)

  data <- data[, list(level, area, year=year_id, sex=sex_id, age, mean_value, lower_value, upper_value)]
  if (collapse_neonatal) {
    print("collapsing neonatal age groups")
    data[grep("^[ELP]N$", age), age := "0"]
    data[, age := as.numeric(age)]
    data <- data[, list(mean_value = sum(mean_value), lower_value=sum(lower_value), upper_value=sum(upper_value)),
                 by=c("level", "area", "year", "sex", "age")]
  }
  setnames(data, c("mean_value", "lower_value", "upper_value"),
           c(paste0(value_name, "_mean"), paste0(value_name, "_lb"), paste0(value_name, "_ub")))
  setkeyv(data, c("level", "area", "year", "sex", "age"))
  print("formatted!")
  data
}
