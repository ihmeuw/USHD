####################################################################################################
## Description: Define a function for validating race codes in a data.table
##
## Arguments:   DT -- data.table with race codes you'd like to validate
##              race_set_id (default 1 = OMB 1977) -- corresponds to population_group_set table in ushd_shared database
##              silent (default TRUE) -- doesn't return any message if passes check
##              includes_all_race (default TRUE) -- check for all race
##              expect_missing_race (default NULL) -- vector of race codes that are expected to be missing from a input
##
## Output:      
####################################################################################################

validate_race_codes <- function(
    DT = NULL,
    race_set_id = 1,
    silent = TRUE,
    includes_all_race = TRUE,
    expect_missing_race = NULL){
  require(data.table)
  
  # load reference for races
  mapping <- fread("FILEPATH")
  if(is.na(race_set_id)){
    warning("race_set_id is NA -- interpreting this as pre-database race/ethnicity groups (OMB 1977 standards)")
    assert <- na.omit(mapping[is.na(population_group_set_id), sort(unique(race))])
  } else{
    assert <- mapping[population_group_set_id == race_set_id, sort(unique(race))]  
  }
  
  
  if(!is.null(expect_missing_race)){
    # remove race groups from assert that are expected to be missing
    if(!all(expect_missing_race %in% assert)) stop("Not all values passed to expect_missing_race were found in the refernce race codes. Check arguments.")
    assert <- setdiff(assert, expect_missing_race)
  }
  
  if(!includes_all_race){
    # remove All-race group
    if(is.na(race_set_id)){
      all_race <- mapping[is.na(population_group_set_id) & race_label %like% "All", sort(unique(race))]
    } else{
      all_race <- mapping[population_group_set_id == race_set_id & race_label %like% "All", sort(unique(race))]  
    }
    
    if(length(all_race) != 1) stop(sprintf("Identified %s race codes that appear to be all race (expect 1). This is a problem with validate_race_codes function", length(all_race)))
    assert <- setdiff(assert, all_race)
  }
  
  races <- DT[, sort(unique(race))]
  
  if(dplyr::setequal(assert, races)){
    if(!silent){
      if(is.na(race_set_id)){
        tb <- mapping[is.na(population_group_set_id) & race %in% races, .(population_group_set_id, race, race_label, population_group_set_name)]
      } else{
        tb <- mapping[population_group_set_id == race_set_id & race %in% races, .(population_group_set_id, race, race_label, population_group_set_name)]  
      }
      message("Found expected race codes:")
      message(paste0(capture.output(tb), collapse = "\n"))
    }
  } else{
    stop(sprintf("Found race codes %s in DT but expected %s", paste(races, collapse = ","), paste(assert, collapse = ",")))
  }
  
}
