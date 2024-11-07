##############################################################################################################################
## Description: Adds up total deaths for each parent cause and its children to see how they compare. Discrepancies are
##              recorded and saved to file.
## Input:
##      --data: clean post-pipeline mortality data.table
## Output:
##      --parents_vs_children_deaths_{year}.csv: a CSV file with parent vs child death discrepancies recorded for that year
##############################################################################################################################

compare_parents_children_deaths <- function(data, by_race = F) {
  
  library(data.table)
  
  # Directory to save output file
  main_dir <- "FILEPATH"
  
  # Get cause information
  source(paste0("FILEPATH", "get_cause_metadata.R"))
  causes <- get_cause_metadata(cause_set_id = 4, gbd_round_id = 6)
  
  # Merge parent causes onto deaths data
  data <- data[, list(total_deaths = sum(deaths)), keyby = 'year_id,level,cause_id,acause']
  data[causes, on = 'cause_id', parent_cause_id := i.parent_id]
  data[cause_id == 294, parent_cause_id := NA]   # all-cause is listed as its own parent in cause metadata
  data[, total_children_deaths := sum(total_deaths), by = 'parent_cause_id']
  
  parent_data <- data[, list(parent_cause_id, total_children_deaths)]
  parent_data <- unique(parent_data)
  data[, c('total_children_deaths') := NULL]
  data[parent_data, on = c('cause_id' = 'parent_cause_id'), total_children_deaths := i.total_children_deaths]
  data[, children_parent_ratio := total_children_deaths/total_deaths]
  rm(parent_data)
  
  setcolorder(data, c("year_id", "cause_id", "acause", "level", "parent_cause_id", "total_deaths", 
                    "total_children_deaths", "children_parent_ratio"))
  fwrite(data, "FILEPATH")

}
