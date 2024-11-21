####################################################################################################
## Description: Extract the list of GBD 2015 computation causes (excluding YLD only causes) along
##              with metadata relating to age- and sex-restrictions.
##
## Outputs:     'FILEPATH' -- a data.table
##              with one line per GBD computation cause and columns for cause_id, level, parent_id,
##              cause_outline, acause, cause_name, secret_cause, female, male, yll_age_start,
##              yll_age_end.
##
##  Inputs: cause_version: "reporting" for reporting dataset
##                          "estimation" for estimation dataset
##
####################################################################################################

library(RODBC)
library(data.table)

rm(list=ls())

get_cause_list <- function(cause_version="reporting", this_gbd_round_id=4){
  # connect to shared database
  db <- odbcConnect(dsn = "shared")
  fname <- ifelse(cause_version=="reporting", "", "estimation_")
  this_cause_set_id <- ifelse(cause_version == "reporting", 3, 4)

  # get active cause set version id
  set_versions_active <- data.table(sqlQuery(db, "QUERY", stringsAsFactors=F))
  cause_set_version_id <- set_versions_active[gbd_round_id == this_gbd_round_id & cause_set_id == this_cause_set_id, cause_set_version_id]
  rm(set_versions_active)

  # get cause list and hierarchy; use hierarchy table to subset to causes in the GBD estimation cause list
  cause_list_all <- data.table(sqlQuery(db, "QUERY"), key="cause_id")
  hierarchy <- data.table(sqlQuery(db, paste("QUERY")), key="cause_id")
  cause_list <- cause_list_all[hierarchy,]

  rm(hierarchy, cause_list_all)

  # get relevant metadata
  metadata_type <- data.table(sqlQuery(db, "QUERY"), key="cause_metadata_type_id")
  metadata_type <- metadata_type[cause_metadata_type %in% c("yll_only", "yld_only", "yll_age_start", "yll_age_end", "male", "female", "secret_cause")]

  metadata <- data.table(sqlQuery(db, "QUERY"), key="cause_metadata_type_id")
  metadata <- metadata[metadata_type,]
  metadata[, cause_metadata_type_id := NULL]
  metadata <- dcast.data.table(metadata, cause_id ~ cause_metadata_type, value.var="cause_metadata_value")
  metadata <- metadata[, lapply(.SD, function(x) as.numeric(as.character(x)))]
  setkey(metadata, cause_id)

  # merge metadata with cause list, append secret cause list
  cause_list <- metadata[cause_list,]
  rm(metadata, metadata_type)

  # drop causes where secret_cause and age/sex restrictions are missing
  cause_list <- cause_list[!is.na(secret_cause) & !is.na(female),]

  # format and save
  cause_list <- cause_list[, list(cause_id, level, parent_id, path_to_top_parent, cause_outline, acause,
                                  cause_name, secret_cause, female, male, yll_age_start, yll_age_end,
                                  yld_only, sort_order)]
  setkey(cause_list, cause_id)
  write.csv(cause_list, file="FILEPATH", row.names=F)
  odbcCloseAll()

  # generate a dataset with a column for every level. A cause will have its ancestors entered into every level above it,
  # and NAs at every level below it, so you can collapse up to any level by simply summing over the relevant column.
  hierarchy <- cause_list[, list(cause_id, level, parent_id, path_to_top_parent, acause, yld_only, secret_cause)]
  hierarchy[, path_to_top_parent:= as.character(path_to_top_parent)]
  hierarchy <- hierarchy[order(level, cause_id)]

  # make a new column for every value in path_to_top_parent
  newcols <- data.table(sapply(1:6, function(int){sapply(strsplit(hierarchy$path_to_top_parent, ","), function(x){as.numeric(x[int])})}))
  setnames(newcols, c("level_0", "level_1", "level_2", "level_3", "level_4", "level_5"))
  hierarchy <- cbind(hierarchy, newcols)

  # save
  write.csv(hierarchy, file="FILEPATH", row.names=F)
}
